# Code 1: Explorer missing values ----

## Settings ----
source("Code/0.1 Settings.R")
source("Code/0.2 Functions.R")
source("Code/0.3 Packages.R")

# Data path 
data_inp <- "Data/Input/"
data_out <- "Data/Output/"

## Data ---- 
data <- rio::import(paste0(data_out, "series_pm25_o3_kriging_idw", ".RData")) 
glimpse(data)

## Scatter-plots ---- 

make_cor_label <- function(x, y, data) {
  test <- cor.test(data[[x]], data[[y]], use = "complete.obs")
  r <- round(test$estimate, 2)
  p <- format.pval(test$p.value, digits = 3, eps = .001)
  paste0("r = ", r, ", p = ", p)
}

plot_corr_panel <- function(data, x, y, xlab, ylab) {
  label_txt <- make_cor_label(x, y, data)
  
  ggplot(data, aes_string(x = x, y = y)) +
    stat_bin2d(bins = 100) +
    scale_fill_gradient(low = "#fee8c8", high = "#e34a33", name = "Count") +
    geom_smooth(method = "lm", formula = y ~ x, color = "#08519c", alpha = 0.3, linewidth = 1) +
    annotate("text", x = Inf, y = Inf, label = label_txt, hjust = 1.1, vjust = 1.5, size = 4) +
    labs(x = xlab, y = ylab) +
    theme_light() +
    theme(
      panel.grid = element_blank(),
      plot.title = element_text(size = 11, hjust = 0)
    )
}

g1 <- plot_corr_panel(
  data = data,
  x    = "pm25_krg",
  y    = "pm25_idw",
  xlab = "PM2.5 Kriging",
  ylab = "PM2.5 IDW"
) +
  scale_x_continuous(limits =c(0, 100)) +
  scale_y_continuous(limits =c(0, 100)) +
  labs(title = "A. PM2.5: Kriging vs IDW") +
  theme(legend.position = "right")

g2 <- plot_corr_panel(
  data = data,
  x    = "o3_krg",
  y    = "o3_idw",
  xlab = "O3 Kriging",
  ylab = "O3 IDW"
) +
  scale_x_continuous(limits =c(0, 35)) +
  scale_y_continuous(limits =c(0, 35)) +
  labs(title = "B. O3: (Kriging) vs IDW") +
  theme(legend.position = "right")

ggarrange(g1, g2, ncol = 2, common.legend = TRUE, legend = "right")

ggsave(
  filename = "Output/idw_vs_kriging/Correlation_krg_idw_pm_o3.png",
  #plot     = last_plot(),
  res      = 300,
  width    = 20,
  height   = 10,
  units    = 'cm',
  scaling  = 0.9,
  device   = ragg::agg_png
)

## Distribution plots ----

# PM2.5
data_pm <- data |>
  select(pm25_krg, pm25_idw) |>
  pivot_longer(cols = everything(), names_to = "method", values_to = "value") |>
  mutate(method = recode(method,
                         "pm25_krg" = "Kriging",
                         "pm25_idw" = "IDW"),
         contaminant = "PM2.5")

# O3
data_o3 <- data |>
  select(o3_krg, o3_idw) |>
  pivot_longer(cols = everything(), names_to = "method", values_to = "value") |>
  mutate(method = recode(method,
                         "o3_krg" = "Kriging",
                         "o3_idw" = "IDW"),
         contaminant = "O3")

# Combinar
data_dens <- bind_rows(data_pm, data_o3)

plot_density_panel <- function(data, contaminant, xlim_range = NULL) {
  data_sub <- filter(data, contaminant == contaminant)
  
  ggplot(data_sub, aes(x = value, fill = method, color = method)) +
    geom_density(alpha = 0.3, linewidth = 0.8) +
    scale_fill_manual(values = c("Kriging" = "#1f77b4", "IDW" = "#ff7f0e")) +
    scale_color_manual(values = c("Kriging" = "#1f77b4", "IDW" = "#ff7f0e")) +
    labs(x = contaminant, y = "Density") +
    theme_light() +
    theme(
      panel.grid = element_blank(),
      legend.position = "top",
      legend.title = element_blank(),
      plot.title = element_text(size = 11, hjust = 0)
    ) +
    coord_cartesian(xlim = xlim_range)
}

g3 <- plot_density_panel(data_dens, contaminant = "PM2.5", xlim_range = c(0, 100)) +
  labs(title = "A. PM2.5: Distribution by Method")

g4 <- plot_density_panel(data_dens, contaminant = "O3", xlim_range = c(0, 35)) +
  labs(title = "B. O3: Distribution by Method")


ggarrange(g3, g4, ncol = 2, common.legend = TRUE)

ggsave(
  filename = "Output/idw_vs_kriging/Distribution_krg_idw_pm.png",
  #plot     = last_plot(),
  res      = 300,
  width    = 20,
  height   = 10,
  units    = 'cm',
  scaling  = 0.9,
  device   = ragg::agg_png
)
