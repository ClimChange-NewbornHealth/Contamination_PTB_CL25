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

t1 <- grep("_t1$",  exp_vars, value = TRUE)
groups1 <- lapply(t1, function(v) {
  base <- sub("_t1$", "", v)
  paste0(base, c("_t1","_t2","_t3"))
})

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
trimestre_vars <- c(unlist(groups1, use.names = FALSE),
                    unlist(groups10, use.names = FALSE),
                    unlist(groups_iqr, use.names = FALSE))

# Variables de exposición “individuales”
single_vars <- setdiff(exp_vars, trimestre_vars)

grouped1   <- vapply(groups1,   paste, collapse = " + ", FUN.VALUE = "")
grouped10   <- vapply(groups10,   paste, collapse = " + ", FUN.VALUE = "")
grouped_iqr <- vapply(groups_iqr, paste, collapse = " + ", FUN.VALUE = "")

exp_vars_models <- c(single_vars, grouped1, grouped10, grouped_iqr)

exp_vars_models <- exp_vars_models[!str_detect(exp_vars_models, "pm")]

exp_vars_models

dependent_vars <- c("birth_preterm", # "lbw", "tlbw", "sga",
                    "birth_very_preterm", "birth_moderately_preterm", 
                    "birth_late_preterm") # , "birth_term", "birth_posterm"

control_vars <- c("weeks", "sex", 
    "age_group_mom", "educ_group_mom", "job_group_mom",
    "age_group_dad", "educ_group_dad", "job_group_dad",
    "month_week1", "year_week1", "covid", "vulnerability")

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

writexl::write_xlsx(combinations, path =  paste0("Output/", "Models/", "List_models_contamination_Ozone_Summer", ".xlsx"))

## HR COX Models ---- 
fit_cox_model <- function(dependent, predictor, data) {

  formula <- as.formula(paste("Surv(weeks, ", dependent, ") ~ ", predictor, 
                              "+ sex + age_group_mom + educ_group_mom + job_group_mom +",
                              "age_group_dad + educ_group_dad + job_group_dad +",
                              "factor(month_week1) + factor(year_week1) + factor(covid) + vulnerability"))
  
  # Ajuste del modelo de Cox usando el argumento `data`
  model_fit <- coxph(formula, data = data)
  
  # Extraer resultados con tidy
  results <- broom::tidy(model_fit, exponentiate = TRUE, conf.int = TRUE, conf.level = 0.95) |>
    select(term, estimate, std.error, statistic, p.value, conf.low, conf.high) |>
    mutate(dependent_var = dependent, predictor = predictor)  # Añadir columnas de identificación
  return(results)

  rm(model_fit); gc()
}

fit_logit_model <- function(dependent, predictor, data) {
  
  formula <- as.formula(paste(dependent, " ~ ", predictor, 
                              "+ sex + age_group_mom + educ_group_mom + job_group_mom +",
                              "age_group_dad + educ_group_dad + job_group_dad +",
                              "factor(month_week1) + factor(year_week1) + factor(covid) + vulnerability"))
  
  # Ajuste del modelo logístico
  model_fit <- glm(formula, data = data, family = binomial(link = "logit"))
  
  # Extraer resultados con tidy
  results <- broom::tidy(model_fit, exponentiate = TRUE, conf.int = TRUE, conf.level = 0.95) |>
    select(term, estimate, std.error, statistic, p.value, conf.low, conf.high) |>
    mutate(dependent_var = dependent, predictor = predictor)
  
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
  
  # Si el dependent es lbw, tlbw o sga → usa logit, si no → usa cox
  if (dep %in% c("lbw", "tlbw", "sga")) {
    fit_logit_model(dep, pred, data = exp_data)
  } else {
    fit_cox_model(dep, pred, data = exp_data)
  }
})
toc() 
plan(sequential)

# Save models results
saveRDS(results_list, file = "Output/Models/Contamination_models_Ozone_summer.rds")

results_cox <- bind_rows(results_list)

writexl::write_xlsx(results_cox, path =  paste0("Output/", "Models/", "Cox_models_contamination_Ozone_summer", ".xlsx"))

results_cox <- rio::import(paste0("Output/", "Models/", "Cox_models_contamination_Ozone_summer", ".xlsx"))

