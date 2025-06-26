# Code 6: Survival models preliminar ----

rm(list=ls())
## Settings ----
source("Code/0.1 Settings.R")
source("Code/0.2 Functions.R")
source("Code/0.3 Packages.R")

# Data path
data_out <- "Data/Output/"

## Data ---- 

exp_data <- rio::import(paste0(data_out, "series_births_exposition_pm25_o3_kriging_idw_ozone_summer", ".RData")) |> drop_na()
summary(exp_data)
glimpse(exp_data) # 252498

exp_vars <- exp_data |>
  select(starts_with("pm25_krg"), starts_with("pm25_idw"),
         starts_with("o3_krg"),   starts_with("o3_idw")) |>
  names()

t1_10   <- grep("_t1_10$",  exp_vars, value = TRUE)
groups10 <- lapply(t1_10, function(v) {
  base <- sub("_t1_10$", "", v)
  paste0(base, c("_t1_10","_t2_10","_t3_10"))
})

t1_iqr  <- grep("_t1_iqr$", exp_vars, value = TRUE)
groups_iqr <- lapply(t1_iqr, function(v) {
  base <- sub("_t1_iqr$", "", v)
  paste0(base, c("_t1_iqr","_t2_iqr","_t3_iqr"))
})

# Vector con todas las variables de trimestres (10 + iqr)
trimestre_vars <- c(unlist(groups10, use.names = FALSE),
                    unlist(groups_iqr, use.names = FALSE))

# Variables de exposición “individuales”
single_vars <- setdiff(exp_vars, trimestre_vars)

grouped10   <- vapply(groups10,   paste, collapse = " + ", FUN.VALUE = "")
grouped_iqr <- vapply(groups_iqr, paste, collapse = " + ", FUN.VALUE = "")

exp_vars_models <- c(single_vars, grouped10, grouped_iqr)

dependent_vars <- c("birth_preterm", "birth_very_preterm", "birth_moderately_preterm", 
                    "birth_late_preterm") # , "birth_term", "birth_posterm"

control_vars <- c("weeks", "sex", 
    "age_group_mom", "educ_group_mom", "job_group_mom",
    "age_group_dad", "educ_group_dad", "job_group_dad",
    "month_week1", "year_week1", "vulnerability")

exp_data <- exp_data |> 
  dplyr::select(all_of(c("id",  dependent_vars, control_vars, exp_vars, trimestre_vars 
  )))

glimpse(exp_data)

# All models execution
combinations <- expand.grid(
  dependent  = dependent_vars,
  predictor  = exp_vars_models,
  stringsAsFactors = FALSE
)
combinations

## HR COX Models ---- 
fit_cox_model <- function(dependent, predictor, data) {

  formula <- as.formula(paste("Surv(weeks, ", dependent, ") ~ ", predictor, 
                              "+ sex + age_group_mom + educ_group_mom + job_group_mom +",
                              "age_group_dad + educ_group_dad + job_group_dad +",
                              "factor(month_week1) + factor(year_week1) + vulnerability"))
  
  # Ajuste del modelo de Cox usando el argumento `data`
  model_fit <- coxph(formula, data = data)
  
  # Extraer resultados con tidy
  results <- broom::tidy(model_fit, exponentiate = TRUE, conf.int = TRUE, conf.level = 0.95) |>
    select(term, estimate, std.error, statistic, p.value, conf.low, conf.high) |>
    mutate(dependent_var = dependent, predictor = predictor)  # Añadir columnas de identificación
  return(results)

  rm(model_fit); gc()
}

## Parallel models -----

plan(multisession, workers = parallel::detectCores() - 1)
options(future.globals.maxSize = 1.5 * 1024^3)
tic()
results_list <- future_lapply(seq_len(nrow(combinations)), function(i) {
  dep <- combinations$dependent[i]
  pred <- combinations$predictor[i]
  
  # Tu función original, que arma la fórmula y corre coxph:
  fit_cox_model(dep, pred, data = exp_data)
})
toc() # 196,962 sec elapsed 
plan(sequential)

# Save models results
saveRDS(results_list, file = "Output/Models/Contamination_models_Ozone_summer.rds")

results_cox <- bind_rows(results_list)

writexl::write_xlsx(results_cox, path =  paste0("Output/", "Models/", "Cox_models_contamination_Ozone_summer", ".xlsx"))

results_cox <- rio::import(paste0("Output/", "Models/", "Cox_models_contamination_Ozone_summer", ".xlsx"))

## Plots with Exposure Effects COX Models ---- 

exp_vars <- str_subset(exp_vars, pattern = "o3")

results_filtered <- results_cox |>
  filter(term %in% c(exp_vars))

results_filtered <- results_filtered |> 
  mutate(estimate = round(estimate, 4), 
           std.error = round(std.error, 3),
           statistic = round(statistic, 3),
           p.value = round(p.value, 3),
           conf.low = round(conf.low, 4),
           conf.high = round(as.numeric(conf.high), 4)) 

exp_vars <- str_subset(unique(results_filtered$term), "_(10|iqr)$")

plot_data <- results_filtered |>
  filter(term %in% exp_vars) |>
  mutate(
    # Kriging vs IDW
    method = case_when(
      str_detect(term, "_krg_") ~ "Kriging",
      str_detect(term, "_idw_") ~ "IDW"
    ) |> factor(levels = c("Kriging", "IDW")),
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
    ) |> factor(levels = c("4-day","30-day","T1","T2","T3","Full")),
    # Media vs IQR
    is_iqr = str_detect(term, "_iqr"),
    # Etiqueta Y y orden estricto
    metric = paste0(period, if_else(is_iqr, " IQR", "")) |>
      factor(levels = c(
        # medias
        "4-day","30-day","T1","T2","T3","Full",
        # IQR  
        "4-day IQR","30-day IQR","T1 IQR","T2 IQR","T3 IQR","Full IQR"
      ))
  )

# Design the plot

make_pair <- function(df, method_name, pollutant_name, tag10, tagIQR, li, lr, scale) {
  df_base <- df |>
    filter(method   == method_name,
           pollutant == pollutant_name) |> 
    filter(dependent_var == "birth_preterm")

  # -- /10 measures
  df10 <- df_base |> filter(!is_iqr)
  p10 <- ggplot(df10, aes(x = estimate, y = period)) +
    geom_vline(xintercept = 1, linetype = "dashed", color = "grey50") +
    geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                   height = 0.2) +
    geom_point(aes(color = pollutant), size = 2) +
    scale_x_continuous(limits = c(li, lr)) +
    scale_color_manual(values = if (pollutant_name=="PM2.5") "darkred" else "darkblue") +
    labs(subtitle = paste0(tag10, " ", pollutant_name, " [", scale," - X/10]"),
         x = "HR (95% CI)", y = NULL) +
    theme_light() +
    theme(
      legend.position = "none",
      plot.subtitle    = element_text(face = "bold"),
      panel.grid       = element_blank(),
      axis.text.y      = element_text(size = 8),
      axis.ticks.y     = element_blank()
    )

  # -- IQR measures
  dfI <- df_base |> filter(is_iqr)
  pI <- ggplot(dfI, aes(x = estimate, y = period)) +
    geom_vline(xintercept = 1, linetype = "dashed", color = "grey50") +
    geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                   height = 0.2) +
    geom_point(aes(color = pollutant), size = 2) +
    scale_x_continuous(limits = c(li, lr)) +
    scale_color_manual(values = if (pollutant_name=="PM2.5") "darkred" else "darkblue") +
    labs(subtitle = paste0(tagIQR, " ", pollutant_name, " [", scale," - X/IQR]"),
         x = "HR (95% CI)", y = NULL) +
    theme_light() +
    theme(
      legend.position = "none",
      plot.subtitle    = element_text(face = "bold"),
      panel.grid       = element_blank(),
      axis.text.y      = element_text(size = 8),
      axis.ticks.y     = element_blank()
    )

  # devolverlos lado a lado
  p10 + pI + plot_layout(ncol = 2)
}

pA <- make_pair(plot_data, "Kriging", "Ozone", tag10 = "A.-KRG", tagIQR = "B-KRG", li=0, lr=10, scale="ppb")
pB <- make_pair(plot_data, "IDW",     "Ozone", tag10 = "C-IDW.", tagIQR = "D-IDW.", li=0, lr=10, scale="ppb")

final_plot <- (pA / pB) 
print(final_plot)
ggsave("Output/Models/HR_Cox_panel_O3_summer.png",
  #plot     = last_plot(),
  res      = 300,
  width    = 25,
  height   = 20,
  units    = 'cm',
  scaling  = 0.9,
  device   = ragg::agg_png
)

