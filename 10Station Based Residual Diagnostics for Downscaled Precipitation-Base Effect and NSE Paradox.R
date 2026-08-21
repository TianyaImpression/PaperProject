# Load required packages
library(tidyverse)
library(broom)

# ==============================================================================
# Data preparation (assuming pre_station_diag has been created previously)
# The snippet below computes station-level diagnostics:
# n_records, mean precipitation, variance, RMSE, and NSE, then filters valid stations
# ==============================================================================

# (The following code is part of a dplyr pipeline that creates pre_station_diag.
#  It is kept as a reference and assumes the input data frame is available.)
# pre_station_diag <- ... |>
#   summarise(
#     n_records = n(),
#     Mean_Precip = mean(Obs, na.rm = TRUE),
#     Var_Precip  = var(Obs, na.rm = TRUE),
#     RMSE        = sqrt(mean((`0.5'` - Obs)^2, na.rm = TRUE)),
#     NSE         = 1 - (sum((`0.5'` - Obs)^2) / sum((Obs - mean(Obs))^2)),
#     .groups     = "drop"
#   ) |>
#   filter(
#     is.finite(NSE),
#     n_records >= 12,
#     Mean_Precip > 1.0,
#     Var_Precip  > 0.1
#   )

# ------------------------------------------------------------------------------
# Figure 4a: Base effect on precipitation error
# ------------------------------------------------------------------------------
x_anchor_4a <- min(pre_station_diag$Mean_Precip, na.rm = TRUE)
y_anchor_4a <- max(pre_station_diag$RMSE, na.rm = TRUE)
r_value <- round(cor(log10(pre_station_diag$RMSE),
                     log10(pre_station_diag$Mean_Precip),
                     use = "complete.obs"), 3)

p4a <- ggplot(pre_station_diag, aes(x = Mean_Precip, y = RMSE)) +
  geom_point(alpha = 0.25, size = 1, shape = 16, color = "#2c7bb6") +
  geom_smooth(method = "loess", formula = y ~ x,
              color = "#d7191c", fill = "#fdae61", linewidth = 1, se = TRUE) +
  scale_x_log10(breaks = c(1, 10, 100, 1000),
                labels = scales::label_number(accuracy = 1)) +
  scale_y_log10(breaks = c(0.1, 1, 10, 100, 1000),
                labels = scales::label_number(accuracy = 0.1)) +
  annotation_logticks() +
  annotate("text", x = x_anchor_4a, y = y_anchor_4a,
           label = paste0("r = ", r_value, "\nn = ", nrow(pre_station_diag)),
           hjust = 0, vjust = 1, size = 4, color = "gray20", fontface = "bold") +
  labs(
    title = "(a) Base Effect on Precipitation Error",
    x = "Mean Monthly Precipitation (mm, log scale)",
    y = "RMSE (mm, log scale)"
  ) +
  theme_bw(base_size = 11) +
  theme(panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold"))

# ------------------------------------------------------------------------------
# Figure 4b: NSE paradox across observed variance
# ------------------------------------------------------------------------------
neg5_data <- pre_station_diag |> filter(NSE < -5)
n_neg5 <- nrow(neg5_data)
min_nse <- if (n_neg5 > 0) min(neg5_data$NSE, na.rm = TRUE) else NA_real_

pre_station_diag <- pre_station_diag |>
  mutate(NSE_class = case_when(
    NSE > 0 ~ "Positive",
    NSE <= 0 ~ "Non-positive"
  ))

label_pos <- paste0("Positive NSE: ",
                    round(mean(pre_station_diag$NSE > 0, na.rm = TRUE) * 100, 1), "%")
label_neg <- if (n_neg5 > 0) {
  paste0("NSE < -5: ", n_neg5, " stations\nMin NSE: ", round(min_nse, 1))
} else {
  "No NSE < -5"
}

x_anchor_4b <- min(pre_station_diag$Var_Precip, na.rm = TRUE)

p4b <- ggplot(pre_station_diag, aes(x = Var_Precip, y = NSE)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "#e6550d", linewidth = 0.8) +
  geom_point(aes(color = NSE_class), alpha = 0.25, size = 1, shape = 16) +
  scale_color_manual(values = c("Positive" = "#1a9850",
                                "Non-positive" = "#d73027"),
                     guide = "none") +
  geom_smooth(data = filter(pre_station_diag, NSE >= -5),
              method = "loess", formula = y ~ x,
              color = "black", fill = "gray80", linewidth = 1, se = TRUE) +
  scale_x_log10(
    breaks = c(1, 10, 100, 1000, 10000, 100000),
    labels = scales::label_number(accuracy = 1),
    expand = expansion(mult = c(0.05, 0.08))
  ) +
  coord_cartesian(ylim = c(-5, 1), clip = "on") +
  scale_y_continuous(breaks = seq(-5, 1, by = 1)) +
  annotate("text", x = x_anchor_4b, y = 0.8,
           label = label_pos, hjust = 0, vjust = 1, size = 4,
           color = "#1a9850", fontface = "bold") +
  annotate("text", x = x_anchor_4b, y = -4.8,
           label = label_neg, hjust = 0, vjust = 0, size = 3.5,
           color = "#d73027", fontface = "italic") +
  labs(
    title = "(b) NSE Paradox across Observed Variance",
    x = "Variance of Observed Precipitation (log scale)",
    y = "Nash-Sutcliffe Efficiency"
  ) +
  theme_bw(base_size = 11) +
  theme(panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold"))

# Combine panels and save
fig4_pdf <- p4a + p4b + plot_layout(widths = c(1, 1))
ggsave("D:/R/DeltaClim/Figure_4_Residual_Diagnosis.pdf",
       plot = fig4_pdf, width = 11, height = 5, device = "pdf")

# ==============================================================================
# Supplementary statistics for Figure 4
# ==============================================================================

# 1. Correlation coefficient and significance for Figure 4a
cor_test <- cor.test(log10(pre_station_diag$RMSE),
                     log10(pre_station_diag$Mean_Precip),
                     method = "pearson")
r_val <- round(cor_test$estimate, 3)
p_val <- cor_test$p.value
p_label <- ifelse(p_val < 0.001, "< 0.001", round(p_val, 4))

cat("Figure 4a statistics:\n")
cat(sprintf("   r = %.3f, p %s, n = %d\n", r_val, p_label, nrow(pre_station_diag)))

# 2. NSE distribution quantiles
nse_quantiles <- quantile(pre_station_diag$NSE,
                          probs = c(0, 0.25, 0.5, 0.75, 1),
                          na.rm = TRUE)
cat("\nNSE quantiles:\n")
print(round(nse_quantiles, 4))

# 3. NSE classification (consistent with text)
nse_class <- pre_station_diag %>%
  mutate(
    class = case_when(
      NSE < -5               ~ "Poor_Negative",
      NSE >= -5 & NSE < 0    ~ "Moderate_Neg",
      NSE >= 0 & NSE < 0.5   ~ "Acceptable_Pos",
      NSE >= 0.5             ~ "Excellent_Pos"
    )
  ) %>%
  group_by(class) %>%
  summarise(n = n()) %>%
  mutate(percent = n / nrow(pre_station_diag) * 100)

cat("\nNSE classification percentages:\n")
print(nse_class)

# Extract key proportions for the manuscript
positive_pct <- nse_class %>%
  filter(class %in% c("Acceptable_Pos", "Excellent_Pos")) %>%
  pull(percent) %>%
  sum()
excellent_pct <- nse_class %>% filter(class == "Excellent_Pos") %>% pull(percent)
acceptable_pct <- nse_class %>% filter(class == "Acceptable_Pos") %>% pull(percent)
poor_neg_pct <- nse_class %>% filter(class == "Poor_Negative") %>% pull(percent)

cat(sprintf("\nPositive NSE total: %.1f%%\n", positive_pct))
cat(sprintf("   Excellent (>=0.5): %.1f%%\n", excellent_pct))
cat(sprintf("   Acceptable (0-0.5): %.1f%%\n", acceptable_pct))
cat(sprintf("   Poor (< -5): %.1f%%\n", poor_neg_pct))

# 4. Extreme negative NSE details
neg5 <- pre_station_diag %>% filter(NSE < -5)
n_neg5 <- nrow(neg5)
min_nse <- if (n_neg5 > 0) min(neg5$NSE, na.rm = TRUE) else NA_real_

cat("\nExtreme negative NSE statistics:\n")
cat(sprintf("   NSE < -5 stations: %d (%.2f%%)\n",
            n_neg5, n_neg5 / nrow(pre_station_diag) * 100))
cat(sprintf("   Minimum NSE: %.1f\n", min_nse))

# 5. Confidence interval for correlation (optional)
ci <- cor_test$conf.int
cat(sprintf("   95%% CI for r: [%.3f, %.3f]\n", ci[1], ci[2]))