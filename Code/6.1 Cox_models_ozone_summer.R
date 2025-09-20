# Code 6.1: Survival models preliminar ----
# Ozone in Summer 

rm(list=ls())
## Settings ----
source("Code/0.1 Settings.R")
source("Code/0.2 Packages.R")
source("Code/0.3 Functions.R")

# Data path
data_out <- "Data/Output/"

## Data ---- 

exp_data <- rio::import(paste0(data_out, "series_births_exposition_pm25_o3_kriging_idw_ozone_summer", ".RData")) |> drop_na()
summary(exp_data)
glimpse(exp_data) # 252498

exp_vars <- exp_data |>
  select(starts_with("o3_krg"),  starts_with("o3_idw")) |>
  select(contains("30"), contains("4")) |> 
  names()

dependent_vars <- c("birth_preterm", "lbw", "tlbw", "sga",
                    "birth_very_preterm", "birth_moderately_preterm", 
                    "birth_late_preterm") # , "birth_term", "birth_posterm"

control_vars <- c("weeks", "sex", 
    "age_group_mom", "educ_group_mom", "job_group_mom",
    "age_group_dad", "educ_group_dad", "job_group_dad",
    "month_week1", "year_week1", "covid", "vulnerability")

exp_data <- exp_data |> 
  dplyr::select(all_of(c("id",  dependent_vars, control_vars, exp_vars
  )))

glimpse(exp_data)
summary(exp_data)

# All models execution
combinations <- expand.grid(
  dependent  = dependent_vars,
  predictor  = exp_vars,
  adjustment = c("Adjusted", "Unadjusted"),
  stringsAsFactors = FALSE
)
combinations

writexl::write_xlsx(combinations, path =  paste0("Output/", "Models/", "List_models_contamination_summer_ozone", ".xlsx"))

## HR COX/LOGIT Models ---- 

fit_cox_model <- function(dependent, predictor, data, adjustment = "Adjusted") {

  rhs <- if (identical(adjustment, "Adjusted")) {
    paste(
      predictor,
      "+ sex + age_group_mom + educ_group_mom + job_group_mom +",
      "age_group_dad + educ_group_dad + job_group_dad +",
      "factor(year_week1) + vulnerability"
    )
  } else {
    predictor
  }

  form <- as.formula(paste("Surv(weeks, ", dependent, ") ~ ", rhs))
  model_fit <- coxph(form, data = data, ties = "efron")

  results <- broom::tidy(model_fit, exponentiate = TRUE, conf.int = TRUE, conf.level = 0.95) |>
    dplyr::select(term, estimate, std.error, statistic, p.value, conf.low, conf.high) |>
    dplyr::mutate(dependent_var = dependent, predictor = predictor, adjustment = adjustment)

  return(results)

  rm(model_fit); gc()
}

fit_logit_model <- function(dependent, predictor, data, conf.level = 0.95, adjustment = "Adjusted") {

  rhs <- if (identical(adjustment, "Adjusted")) {
    paste(
      predictor,
      " + sex + age_group_mom + educ_group_mom + job_group_mom +",
      " age_group_dad + educ_group_dad + job_group_dad +",
      " factor(year_week1) + vulnerability"
    )
  } else {
    predictor
  }

  fml <- as.formula(paste0(dependent, " ~ ", rhs))
  model_fit <- glm(fml, data = data, family = binomial(link = "logit"))

  tbl <- broom::tidy(model_fit, conf.int = FALSE, exponentiate = FALSE)
  z   <- qnorm(1 - (1 - conf.level) / 2)

  tbl <- tbl |>
    dplyr::mutate(
      or        = exp(estimate),
      conf.low  = exp(estimate - z * std.error),
      conf.high = exp(estimate + z * std.error),
      estimate  = or
    )

  results <- tbl |>
    dplyr::select(term, estimate, std.error, statistic, p.value, conf.low, conf.high) |>
    dplyr::mutate(dependent_var = dependent, predictor = predictor, adjustment = adjustment)

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
  adj  <- combinations$adjustment[i]   # <- NUEVO

  # Si el dependent es lbw, tlbw o sga → usa logit, si no → usa cox
  if (dep %in% c("lbw", "tlbw", "sga")) {
    fit_logit_model(dep, pred, data = exp_data, adjustment = adj)  # <- pasa adj
  } else {
    fit_cox_model(dep, pred, data = exp_data, adjustment = adj)    # <- pasa adj
  }
})
toc()
plan(sequential)
beepr::beep(8)

# Save models results
saveRDS(results_list, file = "Output/Models/Contamination_models_ozone_summer.rds")

results_cox <- bind_rows(results_list)

writexl::write_xlsx(results_cox, path =  paste0("Output/", "Models/", "Cox_models_contamination_ozone_summer", ".xlsx"))

results_cox <- rio::import(paste0("Output/", "Models/", "Cox_models_contamination_ozone_summer", ".xlsx"))

## Tables with Exposure Effects COX Models ---- 

tbl_export <- results_cox |>
  filter(term %in% c(exp_vars)) |> 
  mutate(across(where(is.numeric), ~ formatC(., format = "f", digits = 4, decimal.mark = "."))) |> 
  mutate(
      exposure = case_when(
      grepl("30", term)   ~ "30 Days",
      grepl("4", term)    ~ "4 Days",
      TRUE ~ NA_character_
    ),
    unit = case_when(
      grepl("iqr", term) ~ "IQR",
      grepl("10", term)   ~ "10",
      TRUE ~ "1"
    ),
    method = case_when(
      grepl("krg", term) ~ "Kriging",
      TRUE ~ "IDW"
    ),
    pollutant = if_else(grepl("^pm25", term), "PM2.5", "O3"),
    hr = paste0(estimate, " (", conf.low, " - ", conf.high, ")")
  ) |>
  filter(unit != "10") |> 
  mutate(
    exposure = factor(exposure,
                      levels = c("30 Days", "4 Days"))
  ) |>
  mutate(
    unit = factor(unit,
                      levels = c("1", "IQR"))
  ) |>
  arrange(adjustment, dependent_var, pollutant, method, unit, exposure, term) |>
  select(term, dependent_var, pollutant, method, unit, adjustment, hr)

writexl::write_xlsx(tbl_export, path =  paste0("Output/", "Models/", "Table_cox_effects_contamination_ozone_summer", ".xlsx"))
