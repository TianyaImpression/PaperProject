import os
import sys

# Fix missing CuPy compilation path in Anaconda environments
if "CONDA_PREFIX" not in os.environ:
    os.environ["CONDA_PREFIX"] = sys.prefix

import glob
import numpy as np
import cupy as cp
import rasterio
from rasterio.io import MemoryFile
from rasterio.vrt import WarpedVRT
from rasterio.enums import Resampling
from rasterio.transform import Affine
from rasterio.crs import CRS
from concurrent.futures import ThreadPoolExecutor, as_completed
from tqdm import tqdm

# ================= Paths =================
CRU_NC_PATH = "G:/Geodata/Weather/Raster/CRUTS4.09/cru_ts4.09.1901.2024.tmp.dat.nc"
WC_DIR = "G:/Geodata/Weather/Raster/WorldCLIMB/wc2.1_30s_tavg"
OUT_DIR = "D:/WorldDeltaClim/tmp/"

# ================= Parallel settings =================
# Use CPU core count, capped between 8 and 12 to avoid disk bottleneck
MAX_WORKERS = min(48, os.cpu_count() or 48)

os.makedirs(OUT_DIR, exist_ok=True)

# Get and sort WorldClim average temperature files
wc_files = sorted(glob.glob(os.path.join(WC_DIR, "*.tif")))
if len(wc_files) != 12:
    raise ValueError(f"Expected 12 monthly WorldClim files, found {len(wc_files)}")


def process_single_month(task_info):
    """
    Worker function: handles one month of interpolation, GPU addition, and writing.
    """
    anom_2d, wc_path, cru_meta, out_filename = task_info
    tmp_filename = out_filename + ".tmp"  # Temporary file for safe write

    with rasterio.open(wc_path) as wc_src:
        out_meta = wc_src.meta.copy()
        out_meta.update(
            dtype=rasterio.float32,
            compress='lzw',
            tiled=True,
            nodata=-9999.0
        )

        # Write low-resolution anomaly to an in-memory file
        cru_meta.update(driver='GTiff', count=1, dtype=rasterio.float32)
        with MemoryFile() as memfile:
            with memfile.open(**cru_meta) as mem_src:
                mem_src.write(anom_2d, 1)

            vrt_options = {
                'crs': wc_src.crs,
                'transform': wc_src.transform,
                'height': wc_src.height,
                'width': wc_src.width,
                'resampling': Resampling.bilinear
            }

            # Virtual resampling pipeline (bilinear)
            with memfile.open() as mem_src, WarpedVRT(mem_src, **vrt_options) as vrt:
                with rasterio.open(tmp_filename, 'w', **out_meta) as dst:
                    for ji, window in wc_src.block_windows(1):
                        wc_chunk = wc_src.read(1, window=window).astype(np.float32)

                        valid_mask = (wc_chunk != wc_src.nodata)
                        if not np.any(valid_mask):
                            empty_chunk = np.full_like(wc_chunk, -9999.0)
                            dst.write(empty_chunk, 1, window=window)
                            continue

                        anom_chunk = vrt.read(1, window=window)

                        # --- GPU computation ---
                        wc_gpu = cp.asarray(wc_chunk)
                        anom_gpu = cp.asarray(anom_chunk)

                        # Core algorithm: temperature downscaling uses addition
                        # TMP_downscaled = WorldClim_TMP + Anomaly_TMP
                        out_gpu = wc_gpu + anom_gpu

                        valid_mask_gpu = cp.asarray(valid_mask)
                        out_gpu = cp.where(valid_mask_gpu, out_gpu, -9999.0)

                        out_chunk = cp.asnumpy(out_gpu)
                        dst.write(out_chunk, 1, window=window)

                        del wc_gpu, anom_gpu, out_gpu, valid_mask_gpu

    # Rename temporary file after successful write (atomic save)
    if os.path.exists(tmp_filename):
        os.replace(tmp_filename, out_filename)

    # Free GPU memory buffers accumulated in this thread
    cp.get_default_memory_pool().free_all_blocks()
    return out_filename


def main():
    print("1. Reading CRU mean temperature (tmp) data...")
    with rasterio.open(CRU_NC_PATH) as root_src:
        subdatasets = root_src.subdatasets
        # Automatically locate the :tmp subdataset path
        tmp_sds_path = next((s for s in subdatasets if s.endswith(':tmp')),
                            f'netcdf:{CRU_NC_PATH}:tmp')

    cru_src = rasterio.open(tmp_sds_path)
    cru_meta = cru_src.meta.copy()

    if cru_src.crs is None or cru_src.transform.is_identity:
        cru_transform = Affine(0.5, 0.0, -180.0, 0.0, -0.5, 90.0)
        cru_meta.update({'crs': CRS.from_epsg(4326), 'transform': cru_transform})

    # Rasterio band indices are 1-based
    # 1970-2000 (31 years): bands 829 to 1200
    # 1982-2024 (43 years): bands 973 to 1488
    baseline_indices = list(range(829, 1201))
    target_indices = list(range(973, 1489))

    # ---------------- CPU vectorized precomputation ----------------
    print("2. Extracting baseline and target years, running full-matrix precomputation...")
    baseline_data = cru_src.read(baseline_indices)
    clim = np.zeros((12, cru_src.height, cru_src.width), dtype=np.float32)
    for m in range(12):
        clim[m] = np.mean(baseline_data[m::12], axis=0)
    del baseline_data

    target_data = cru_src.read(target_indices).astype(np.float32)
    clim_tiled = np.tile(clim, (len(target_indices) // 12, 1, 1))

    # Core algorithm: temperature anomaly uses difference
    # An_TMP = TMP_original - CRUClim_TMP
    # Subtraction avoids division-by-zero issues
    all_anomalies = target_data - clim_tiled

    del target_data, clim_tiled
    print("   -> Anomaly precomputation complete. Ready to dispatch GPU tasks.")
    # ----------------------------------------------------------------

    # Prepare task queue and perform checkpoint scan
    tasks = []
    start_year = 1982
    start_month = 1

    print("\n3. Scanning output directory for existing files...")
    for idx in range(len(target_indices)):
        current_year = start_year + (start_month + idx - 1) // 12
        current_month = (start_month + idx - 1) % 12 + 1
        out_filename = os.path.join(OUT_DIR, f"{current_year}-{current_month:02d}-01.tif")

        if os.path.exists(out_filename):
            continue  # Skip already completed files

        m = idx % 12
        wc_path = wc_files[m]
        anom_2d = all_anomalies[idx]

        tasks.append((anom_2d, wc_path, cru_meta, out_filename))

    skipped = len(target_indices) - len(tasks)
    if skipped > 0:
        print(f"   -> Found {skipped} completed file(s), skipped. {len(tasks)} task(s) remaining.")

    if len(tasks) == 0:
        print("\nAll tasks already completed. Nothing to compute!")
        return

    # ---------------- Multi-threaded GPU scheduling ----------------
    print(f"\n4. Starting multi-threaded processing (Max workers: {MAX_WORKERS}) ...")

    with ThreadPoolExecutor(max_workers=MAX_WORKERS) as executor:
        futures = [executor.submit(process_single_month, task) for task in tasks]

        for future in tqdm(as_completed(futures), total=len(futures),
                           desc="Temperature downscaling progress"):
            try:
                future.result()
            except Exception as e:
                print(f"\nTask failed: {e}")

    print("\nTemperature downscaling completed successfully.")


if __name__ == "__main__":
    main()