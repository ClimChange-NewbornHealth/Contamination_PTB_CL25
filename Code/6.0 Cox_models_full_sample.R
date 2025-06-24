# Code 6: Survival models preliminar ----

rm(list=ls())
## Settings ----
source("Code/0.1 Settings.R")
source("Code/0.2 Functions.R")
source("Code/0.3 Packages.R")

# Data path
data_out <- "Data/Output/"

## Data ---- 

exp_data <- rio::import(paste0(data_out, "series_births_exposition_pm25_o3_kriging_idw", ".RData")) |> drop_na()
summary(exp_data)
glimpse(exp_data) # 713.918

exp_vars <- exp_data |> select(c(
  "pm25_krg_full_10":"o3_idw_full_10",  
  "pm25_krg_30_10":"o3_idw_30_10",
  "pm25_krg_4_10":"o3_idw_4_10",
  "pm25_krg_t1_10":"o3_idw_t3_10",
  "pm25_krg_full_iqr":"o3_idw_full_iqr",
  "pm25_krg_30_iqr":"o3_idw_30_iqr",
  "pm25_krg_4_iqr":"o3_idw_4_iqr",
  "pm25_krg_t1_iqr":"o3_idw_t3_iqr" 
)) |> colnames()
exp_vars

dependent_vars <- c("birth_preterm", "birth_very_preterm", "birth_moderately_preterm", 
                    "birth_late_preterm") # , "birth_term", "birth_posterm"

control_vars <- c("weeks", "sex", 
    "age_group_mom", "educ_group_mom", "job_group_mom",
    "age_group_dad", "educ_group_dad", "job_group_dad",
    "month_week1", "year_week1", "vulnerability")

exp_data <- exp_data |> 
  dplyr::select(all_of(c("id",  dependent_vars, control_vars, exp_vars
  )))

## HR COX Models ---- 
fit_cox_model <- function(dependent, predictor, data) {

  formula <- as.formula(paste("Surv(weeks, ", dependent, ") ~ ", predictor, 
                              "+ sex + age_group_mom + educ_group_mom + job_group_mom +",
                              "age_group_dad + educ_group_dad + job_group_dad +",
                              "factor(month_week1) + factor(year_week1) + vulnerability"))
  
  # Ajuste del modelo de Cox usando el argumento `data`
  model_fit <- coxph(formula, data = data)
  
  # Extraer resultados con tidy
  results <- broom::tidy(model_fit, exponentiate = TRUE, conf.int = TRUE, conf.level = 0.95) %>%
    select(term, estimate, std.error, statistic, p.value, conf.low, conf.high) %>%
    mutate(dependent_var = dependent, predictor = predictor)  # Añadir columnas de identificación
  return(results)

  rm(model_fit); gc()
}

# Iterar sobre las combinaciones de dependientes y predictores
combinations <- expand.grid(dependent_vars, exp_vars, stringsAsFactors = FALSE)

plan(multisession, workers = parallel::detectCores() - 4)
options(future.globals.maxSize = 1.5 * 1024^3)  # 1.5 GB

tic()
results_list <- future_lapply(seq_len(nrow(combinations)), function(i) {
  dep_var <- combinations[i, 1]
  exp_var <- combinations[i, 2]
  fit_cox_model(dep_var, exp_var, data=exp_data)
})
toc() # time: 
beepr::beep(8)
plan(sequential)

# Extract results
results_cox <- bind_rows(results_list)

writexl::write_xlsx(results_cox, path =  paste0("Output/", "Models/", "Cox_models_exp", ".xlsx"))

results_cox <- rio::import(paste0("Output/", "Models/", "Cox_models_exp", ".xlsx"))

## Plots with Exposure Effects COX Models ---- 

results_filtered <- results_cox %>%
  filter(term %in% c(exp_vars))

results_filtered <- results_filtered |> 
  mutate(estimate = round(estimate, 4), 
           std.error = round(std.error, 3),
           statistic = round(statistic, 3),
           p.value = round(p.value, 3),
           conf.low = round(conf.low, 4),
           conf.high = round(conf.high, 4)) 

exp_vars <- unique(results_filtered$term)

plot_data <- results_filtered %>%
  filter(term %in% exp_vars) %>%
  mutate(
    # Kriging vs IDW
    method = case_when(
      str_detect(term, "_krg_") ~ "Kriging",
      str_detect(term, "_idw_") ~ "IDW"
    ) %>% factor(levels = c("Kriging", "IDW")),
    # PM2.5 vs Ozone
    pollutant = case_when(
      str_detect(term, "^pm25") ~ "PM2.5",
      str_detect(term, "^o3")   ~ "Ozone"
    ),
    # Ventana / trimestre
    period = case_when(
      str_detect(term, "_4_")      ~ "4-day",
      str_detect(term, "_30")      ~ "30-day",
      str_detect(term, "_t1_")     ~ "T1",
      str_detect(term, "_t2_")     ~ "T2",
      str_detect(term, "_t3_")     ~ "T3",
      str_detect(term, "_full")    ~ "Full"
    ) %>% factor(levels = c("4-day","30-day","T1","T2","T3","Full")),
    # Media vs IQR
    is_iqr = str_detect(term, "_iqr"),
    # Etiqueta Y y orden estricto
    metric = paste0(period, if_else(is_iqr, " IQR", "")) %>%
      factor(levels = c(
        # medias
        "4-day","30-day","T1","T2","T3","Full",
        # IQR  
        "4-day IQR","30-day IQR","T1 IQR","T2 IQR","T3 IQR","Full IQR"
      ))
  )

# Design the plot

make_hr_plot <- function(df, poll, outc) {
  ggplot(df, aes(x = estimate, y = metric)) +
    geom_point(shape = 15, size = 1.5, colour = "black") +
    geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                   height = 0.2, colour = "black") +
    geom_vline(xintercept = 1, linetype = "dashed", colour = "red", alpha = 0.5) +
    geom_text(
      aes(
        x     = 1.9,
        label = sprintf("%.2f (%.2f–%.2f)", estimate, conf.low, conf.high)
      ),
      hjust = 1.02, size = 3, colour = "black"
    ) +
    facet_wrap(~ method,
               ncol = 2,
               labeller = labeller(method = c(Kriging = "Kriging",
                                              IDW     = "IDW")),
               strip.position = "top") +
    scale_x_continuous(limits = c(0, 2), expand = c(0, 0)) +
    labs(
      title = paste0("HR for ", outc, " — ", poll),
      x     = "HR (95% CI)",
      y     = NULL
    ) +
    theme_light() +
    theme(
      panel.grid       = element_blank(),
      strip.background = element_rect(fill = "white"),
      strip.text       = element_text(size = 11, color = "black", hjust=0),
      axis.text.y      = element_text(size = 9),
      axis.ticks.y     = element_blank(),
      legend.position  = "none"
    )
}

plots_list <- vector("list", length = length(unique(plot_data$dependent_var)) * 2)

names(plots_list) <- expand.grid(
  outcome   = unique(plot_data$dependent_var),
  pollutant = c("PM2.5","Ozone"),
  stringsAsFactors = FALSE
) %>%
  transmute(name = paste0(outcome, "_", pollutant)) %>%
  pull(name)

i <- 1
for (outcome in unique(plot_data$dependent_var)) {
  for (poll in c("PM2.5","Ozone")) {
    df_sub <- filter(plot_data, dependent_var == outcome, pollutant == poll)
    key    <- paste0(outcome, "_", poll)
    plots_list[[key]] <- make_hr_plot(df_sub, poll, outcome)
    i <- i + 1
  }
}

# Save plot 
plots_save <- ggarrange(
  plots_list$birth_preterm_PM2.5 + labs(title = "HR Preterm vs PM2.5"), 
  plots_list$birth_preterm_Ozone + labs(title = "HR Preterm vs Ozone"),
  ncol=2, nrow=1, 
  common.legend = TRUE
)

plots_save

ggsave(plots_save,
  filename = paste0("Output/", "Models/", "PTB_COX_PM25_O3", ".png"), 
  res = 300,
  width = 40,
  height = 20,
  units = 'cm',
  scaling = 1.2,
  device = ragg::agg_png)

ggsave(plots_list$birth_preterm_Ozone + labs(title = "HR Preterm vs Ozone"),, 
  filename = paste0("Output/", "Models/", "PTB_COX_O3", ".png"), 
  res = 300,
  width = 40,
  height = 20,
  units = 'cm',
  scaling = 1.2,
  device = ragg::agg_png)

ggsave(plots_list$birth_preterm_PM2.5 + labs(title = "HR Preterm vs PM2.5"), 
  filename = paste0("Output/", "Models/", "PTB_COX_PM25", ".png"), 
  res = 300,
  width = 40,
  height = 20,
  units = 'cm',
  scaling = 1.2,
  device = ragg::agg_png)


