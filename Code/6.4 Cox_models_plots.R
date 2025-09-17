# Code 6: Survival models preliminar ----

rm(list=ls())
## Settings ----
source("Code/0.1 Settings.R")
source("Code/0.2 Functions.R")
source("Code/0.3 Packages.R")

# Data path
data_out <- "Data/Output/"

## Plot Data ---- 

results_cox <- rio::import(paste0("Output/", "Models/", "Cox_models_contamination", ".xlsx")) |> 
  mutate(data="Overall")
results_cox_pm <- rio::import(paste0("Output/", "Models/", "Cox_models_contamination_pm25_winter", ".xlsx")) |> 
  mutate(data="Winter", 
         conf.high = as.numeric(conf.high))
results_cox_o3 <- rio::import(paste0("Output/", "Models/", "Cox_models_contamination_ozone_summer", ".xlsx")) |> 
  mutate(data="Summer")  

plot_data <- bind_rows(results_cox, results_cox_pm, results_cox_o3)  |> 
  filter(str_detect(term, "^(pm25|o3)")) |> 
  filter(str_detect(term, "iqr")) |> 
  mutate(
      exposure = case_when(
      grepl("full", term) ~ "Overall",
      grepl("t1", term)   ~ "Trimester 1",
      grepl("t2", term)   ~ "Trimester 2",
      grepl("t3", term)   ~ "Trimester 3",
      grepl("30", term)   ~ "30 Days",
      grepl("4", term)    ~ "4 Days",
      TRUE ~ NA_character_
    )) |> 
  mutate(
    unit = case_when(
      grepl("iqr", term) ~ "IQR",
      grepl("10", term)   ~ "10",
      TRUE ~ "1"
    )) |> 
  mutate(
    pollutant = if_else(str_detect(term, "^pm25"), "PM[2.5]", "O[3]"),
  ) |> 
  mutate(
    exposure = factor(exposure,
                      levels = c("Overall", "Trimester 1", "Trimester 2",
                                 "Trimester 3", "30 Days", "4 Days"))
  ) |> 
  mutate(adjustment = factor(adjustment, levels = c("Unadjusted", "Adjusted"))) |> 
  mutate(group = case_when(
         data == "Overall" & pollutant == "PM[2.5]" ~ "A*'.'~Overall~-~PM[2.5]",
         data == "Overall" & pollutant == "O[3]"    ~ "B*'.'~Overall~-~O[3]",
         data == "Winter"  & pollutant == "PM[2.5]" ~ "C*'.'~Winter~-~PM[2.5]",
         data == "Summer"  & pollutant == "O[3]"    ~ "D*'.'~Summer~-~O[3]"
        )) |> 
  mutate(method = case_when(grepl("krg", term) ~ "Kriging", TRUE ~ "IDW"))


## Plot effects paper PTB ----

g1 <- plot_data |> 
  filter(method == "Kriging") |> 
  filter(group %in% c("A*'.'~Overall~-~PM[2.5]", "B*'.'~Overall~-~O[3]")) |> 
  filter(dependent_var == "birth_preterm") |> 
  ggplot(aes(y = estimate, x = exposure, color = adjustment, shape = adjustment)) +
    geom_hline(yintercept = 1, linetype = "dashed", color = "grey50") +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high), 
                  width = 0.3,
                  position = position_dodge2(width = 0.8, preserve = "single") 
                ) +
    geom_point(size = 1.5, 
               position = position_dodge2(width = 0.3, preserve = "single") 
              ) +
    scale_color_manual(values = c("Unadjusted" = "grey50", "Adjusted" = "black")) +
    scale_shape_manual(values = c("Unadjusted" = 16, "Adjusted" = 15)) +
    scale_y_continuous(limits = c(0.7, 1.3), n.breaks = 6) +
    labs(
      y        = "HR (95% CI)",
      x        = NULL
    ) +
    facet_wrap(~group, nrow = 1, scales = "free_x", 
               labeller = label_parsed
              ) +
    theme_light(base_size = 10) +
    theme(
      legend.position     = "top",
      legend.title = element_blank(),
      legend.text = element_text(size = 10),
      strip.background = element_rect(color = "white", fill = "white"),
      strip.text = element_text(size = 10, color = "black", face = "bold", hjust = 0),
      panel.grid          = element_blank(),
      axis.text.y         = element_text(size = 10),
      axis.text.x         = element_text(size = 10),
      axis.ticks.y        = element_line(),
      plot.margin         = margin(2, 2, 2, 2, "pt")
    )

g1

g2 <- plot_data |> 
  filter(method == "Kriging") |> 
  filter(!group %in% c("A*'.'~Overall~-~PM[2.5]", "B*'.'~Overall~-~O[3]")) |> 
  filter(dependent_var == "birth_preterm") |> 
  ggplot(aes(y = estimate, x = exposure, color = adjustment, shape = adjustment)) +
    geom_hline(yintercept = 1, linetype = "dashed", color = "grey50") +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high), 
                  width = 0.15,
                  position = position_dodge2(width = 0.8, preserve = "single") 
                ) +
    geom_point(size = 1.5, 
               position = position_dodge2(width = 0.15, preserve = "single") 
              ) +
    scale_color_manual(values = c("Unadjusted" = "grey50", "Adjusted" = "black")) +
    scale_shape_manual(values = c("Unadjusted" = 16, "Adjusted" = 15)) +
    scale_y_continuous(limits = c(0.7, 1.3), n.breaks = 6) +
    labs(
      y        = "HR (95% CI)",
      x        = NULL
    ) +
    facet_wrap(~group, nrow = 1, scales = "free_x", 
               labeller = label_parsed
              ) +
    theme_light(base_size = 10) +
    theme(
      legend.position     = "top",
      legend.title = element_blank(),
      legend.text = element_text(size = 10),
      strip.background = element_rect(color = "white", fill = "white"),
      strip.text = element_text(size = 10, color = "black", face = "bold", hjust = 0),
      panel.grid          = element_blank(),
      axis.text.y         = element_text(size = 10),
      axis.text.x         = element_text(size = 10),
      axis.ticks.y        = element_line(),
      plot.margin         = margin(2, 2, 2, 2, "pt")
    )

g2

ggarrange(g1, g2, nrow = 2, common.legend = TRUE)

ggsave("Output/Models/HR_PTB_COX_panel.png",
  #plot     = last_plot(),
  res      = 300,
  width    = 22.5,
  height   = 17,
  units    = 'cm',
  scaling  = 0.9,
  device   = ragg::agg_png
)


## Plot effects paper other outcomes ----

dep_vars <- unique(plot_data$dependent_var)[-1]

for (dv in dep_vars) {

  g1 <- plot_data |>
    filter(method == "Kriging",
           group %in% c("A*'.'~Overall~-~PM[2.5]", "B*'.'~Overall~-~O[3]"),
           dependent_var == dv) |>
    ggplot(aes(y = estimate, x = exposure, color = adjustment, shape = adjustment)) +
    geom_hline(yintercept = 1, linetype = "dashed", color = "grey50") +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                  width = 0.3,
                  position = position_dodge2(width = 0.8, preserve = "single")) +
    geom_point(size = 1.5,
               position = position_dodge2(width = 0.3, preserve = "single")) +
    scale_color_manual(values = c("Unadjusted" = "grey50", "Adjusted" = "black")) +
    scale_shape_manual(values = c("Unadjusted" = 16, "Adjusted" = 15)) +
    scale_y_continuous(limits = c(0.5, 1.5), n.breaks = 6) +
    labs(y = "HR (95% CI)", x = NULL) +
    facet_wrap(~group, nrow = 1, scales = "free_x", labeller = label_parsed) +
    theme_light(base_size = 10) +
    theme(
      legend.position = "top",
      legend.title = element_blank(),
      legend.text = element_text(size = 10),
      strip.background = element_rect(color = "white", fill = "white"),
      strip.text = element_text(size = 10, color = "black", face = "bold", hjust = 0),
      panel.grid = element_blank(),
      axis.text.y = element_text(size = 10),
      axis.text.x = element_text(size = 10),
      axis.ticks.y = element_line(),
      plot.margin = margin(2, 2, 2, 2, "pt")
    )

  g2 <- plot_data |>
    filter(method == "Kriging",
           !group %in% c("A*'.'~Overall~-~PM[2.5]", "B*'.'~Overall~-~O[3]"),
           dependent_var == dv) |>
    ggplot(aes(y = estimate, x = exposure, color = adjustment, shape = adjustment)) +
    geom_hline(yintercept = 1, linetype = "dashed", color = "grey50") +
    geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                  width = 0.15,
                  position = position_dodge2(width = 0.8, preserve = "single")) +
    geom_point(size = 1.5,
               position = position_dodge2(width = 0.15, preserve = "single")) +
    scale_color_manual(values = c("Unadjusted" = "grey50", "Adjusted" = "black")) +
    scale_shape_manual(values = c("Unadjusted" = 16, "Adjusted" = 15)) +
    scale_y_continuous(limits = c(0.5, 1.5), n.breaks = 6) +
    labs(y = "HR (95% CI)", x = NULL) +
    facet_wrap(~group, nrow = 1, scales = "free_x", labeller = label_parsed) +
    theme_light(base_size = 10) +
    theme(
      legend.position = "top",
      legend.title = element_blank(),
      legend.text = element_text(size = 10),
      strip.background = element_rect(color = "white", fill = "white"),
      strip.text = element_text(size = 10, color = "black", face = "bold", hjust = 0),
      panel.grid = element_blank(),
      axis.text.y = element_text(size = 10),
      axis.text.x = element_text(size = 10),
      axis.ticks.y = element_line(),
      plot.margin = margin(2, 2, 2, 2, "pt")
    )

  panel <- ggarrange(g1, g2, nrow = 2, common.legend = TRUE, legend = "top")

  outfile <- sprintf("Output/Models/HR_%s_COX_panel.png", dv)
  ggsave(outfile,
         plot = panel,
         res = 300, width = 22.5, height = 17, units = "cm",
         scaling = 0.9, device = ragg::agg_png)
  message("Saved: ", outfile)
}
