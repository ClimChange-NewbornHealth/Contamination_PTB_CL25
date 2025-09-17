# Code 6: Survival models preliminar ----

rm(list=ls())
## Settings ----
source("Code/0.1 Settings.R")
source("Code/0.2 Functions.R")
source("Code/0.3 Packages.R")

# Data path
data_out <- "Data/Output/"

## Data ---- 

bw <- rio::import(paste0(data_out, "series_births_exposition_pm25_o3_kriging_idw_long_obs", ".RData")) |> 
  drop_na() 

glimpse(bw) #  12.099.995 obs

exposure_vars <- c("pm25_week", "o3_week", 
                   "pm25_week_10", "o3_week_10", 
                   "pm25_week_iqr", "o3_week_iqr")

bw <- bw |> 
  group_by(id) |> 
  summarise(
    com = first(com),
    name_com = first(name_com),
    reg = first(reg),
    name_reg = first(name_reg),
    weeks = first(weeks),
    date_nac = first(date_nac),
    day_nac = first(day_nac),
    month_nac = first(month_nac),
    year_nac = first(year_nac),
    month_week1 = first(month_week1), 
    year_week1 = first(year_week1), 
    sex = first(sex),
    tbw = first(tbw),
    size = first(size),
    age_group_mom = first(age_group_mom),
    educ_group_mom = first(educ_group_mom),
    job_group_mom = first(job_group_mom),
    age_group_dad = first(age_group_dad),
    educ_group_dad = first(educ_group_dad),
    job_group_dad = first(job_group_dad),
    covid = first(covid),
    sovi = first(sovi),
    vulnerability = first(vulnerability),
    birth_preterm = first(birth_preterm),
    lbw = first(lbw),
    tlbw = first(tlbw),
    sga = first(sga),
    birth_very_preterm = first(birth_very_preterm),
    birth_moderately_preterm = first(birth_moderately_preterm),
    birth_late_preterm = first(birth_late_preterm),
    birth_term = first(birth_term),
    birth_posterm = first(birth_posterm),
    across(all_of(exposure_vars), ~ mean(.x[week_gest_num %in% 1:12], na.rm = TRUE), .names = "{.col}_t1"),
    across(all_of(exposure_vars), ~ mean(.x[week_gest_num %in% 13:24], na.rm = TRUE), .names = "{.col}_t2"),
    across(all_of(exposure_vars), ~ mean(.x[week_gest_num >= 25], na.rm = TRUE), .names = "{.col}_t3"),
    across(all_of(exposure_vars), ~ mean(.x, na.rm = TRUE), .names = "{.col}"),
    across(all_of(exposure_vars), ~ mean(tail(.x, 4), na.rm = TRUE), .names = "{.col}_30"),
    across(all_of(exposure_vars), ~ mean(tail(.x, 1), na.rm = TRUE), .names = "{.col}_4")
  ) |> 
  ungroup()

glimpse(bw) #  314.087 obs 44% original data

exp_vars <- bw |>
  select(starts_with("pm25"),
         starts_with("o3")) |>
  names()

t1 <- grep("_t1$",  exp_vars, value = TRUE)
groups1 <- lapply(t1, function(v) {
  base <- sub("_t1$", "", v)
  paste0(base, c("_t1","_t2","_t3"))
})

t1_10   <- grep("_10_t1$",  exp_vars, value = TRUE)
groups10 <- lapply(t1_10, function(v) {
  base <- sub("_10_t1$", "", v)
  paste0(base, c("_10_t1","_10_t2","_10_t3"))
})

t1_iqr  <- grep("_iqr_t1$", exp_vars, value = TRUE)
groups_iqr <- lapply(t1_iqr, function(v) {
  base <- sub("_iqr_t1$", "", v)
  paste0(base, c("_iqr_t1","_iqr_t2","_iqr_t3"))
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

#exp_vars_models <- exp_vars_models[!str_detect(exp_vars_models, "o3")]

dependent_vars <- c("birth_preterm", "lbw", "tlbw", "sga",
                    "birth_very_preterm", "birth_moderately_preterm", 
                    "birth_late_preterm") # , "birth_term", "birth_posterm"

control_vars <- c("weeks", "sex", 
    "age_group_mom", "educ_group_mom", "job_group_mom",
    "age_group_dad", "educ_group_dad", "job_group_dad",
    "month_week1", "year_week1", "covid", "vulnerability")

exp_data <- bw |> 
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

writexl::write_xlsx(combinations, path =  paste0("Output/", "Models/", "List_models_contamination_obs", ".xlsx"))

## HR COX Models ---- 
fit_cox_model <- function(dependent, predictor, data) {

  formula <- as.formula(paste("Surv(weeks, ", dependent, ") ~ ", predictor, 
                              "+ sex + age_group_mom + educ_group_mom + job_group_mom +",
                              "age_group_dad + educ_group_dad + job_group_dad +",
                              "factor(month_week1) + factor(year_week1) + factor(covid) + vulnerability"))
  
  # Ajuste del modelo de Cox usando el argumento `data`
  model_fit <- coxph(formula, data = data, ties = "efron")
  
  # Extraer resultados con tidy
  results <- broom::tidy(model_fit, exponentiate = TRUE, conf.int = TRUE, conf.level = 0.95) |>
    select(term, estimate, std.error, statistic, p.value, conf.low, conf.high) |>
    mutate(dependent_var = dependent, predictor = predictor)  # Añadir columnas de identificación
  return(results)

  rm(model_fit); gc()
}

fit_logit_model <- function(dependent, predictor, data, conf.level = 0.95) {
  
  # 1) Construir fórmula
  fml <- as.formula(
    paste0(dependent, " ~ ", predictor,
           " + sex + age_group_mom + educ_group_mom + job_group_mom +",
           " age_group_dad + educ_group_dad + job_group_dad +",
           " factor(month_week1) + factor(year_week1) + factor(covid) + vulnerability")
  )
  
  # 2) Ajustar modelo logístico en escala logit
  model_fit <- glm(fml, data = data, family = binomial(link = "logit"))
  
  # 3) Extraer tabla básica (coeficientes en log-odds)
  tbl <- broom::tidy(model_fit, conf.int = FALSE, exponentiate = FALSE)
  
  # 4) Parámetro z para el nivel de confianza deseado
  z <- qnorm(1 - (1 - 0.95)/2)
  #z <- abs(stats::qnorm((1 - conf.level) / 2))
  
  # 5) Calcular OR y sus IC de Wald
  tbl <- tbl |> 
    mutate(
      or = exp(estimate), 
      conf.low = exp(estimate - z * std.error),
      conf.high = exp(estimate + z * std.error)
    ) |> 
    mutate(estimate = or)
  
  # 6) Reordenar y renombrar columnas
  results <- tbl |> 
    select(term, estimate, std.error, statistic, p.value, conf.low, conf.high) |> 
    mutate(dependent_var = dependent, predictor = predictor)

  return(results)

  rm(model_fit); gc()
}

## Parallel models -----

plan(multisession, workers = parallel::detectCores() - 4)
options(future.globals.maxSize = 1.5 * 1024^3)
tic()
results_list <- future_lapply(seq_len(nrow(combinations)), function(i) {
  message("Iteración ", i, " en PID ", Sys.getpid())
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
saveRDS(results_list, file = "Output/Models/Contamination_models_obs.rds")

results_cox <- bind_rows(results_list)

writexl::write_xlsx(results_cox, path =  paste0("Output/", "Models/", "Cox_models_contamination_obs", ".xlsx"))

results_cox <- rio::import(paste0("Output/", "Models/", "Cox_models_contamination_obs", ".xlsx"))

