# Code 6.5: Attributable fraction population ----

rm(list=ls())
## Settings ----
source("Code/0.1 Settings.R")
source("Code/0.2 Packages.R")
source("Code/0.3 Functions.R")

## Data  ----

data_out <- "Data/Output/"

data <- rio::import(paste0(data_out, "series_births_exposition_pm25_o3_kriging_idw", ".RData")) |> 
  drop_na() |>
  mutate(
    month_week1 = factor(month_week1),
    year_week1  = factor(year_week1),
    covid       = factor(covid),
    vulnerability = factor(vulnerability)
  )

data_w <- rio::import(paste0(data_out, "series_births_exposition_pm25_o3_kriging_idw_pm25_winter", ".RData")) |> 
  drop_na() |>
  mutate(
    month_week1 = factor(month_week1),
    year_week1  = factor(year_week1),
    covid       = factor(covid),
    vulnerability = factor(vulnerability)
  )

data_s <- rio::import(paste0(data_out, "series_births_exposition_pm25_o3_kriging_idw_ozone_summer", ".RData")) |> 
  drop_na() |> 
  mutate(
    month_week1 = factor(month_week1),
    year_week1  = factor(year_week1),
    covid       = factor(covid),
    vulnerability = factor(vulnerability)
  )

glimpse(data) 
glimpse(data_w)
glimpse(data_s)

## Variables ----
dependent    <- "birth_preterm"
time_var     <- "weeks"

control_vars <- c(
  "sex",
  "age_group_mom", "educ_group_mom", "job_group_mom",
  "age_group_dad", "educ_group_dad", "job_group_dad",
  "month_week1", "year_week1", "covid", "vulnerability"
)

exposures_krg <- c(
  "pm25_krg_full","pm25_krg_30","pm25_krg_4",
  "o3_krg_full","o3_krg_30","o3_krg_4",
  "pm25_krg_t1","pm25_krg_t2","pm25_krg_t3",
  "o3_krg_t1","o3_krg_t2","o3_krg_t3"
)

exposures_krg <- paste0(exposures_krg, "_iqr")
exposures_idw <- str_replace_all(exposures_krg, "_krg_", "_idw_")

## Models ----

fit_cox_exposure <- function(df, exposure_var, controls, time_var, event_var, ties = "breslow") {
  rhs <- paste(c(exposure_var, controls), collapse = " + ")
  form <- as.formula(paste0("Surv(", time_var, ", ", event_var, ") ~ ", rhs))
  coxph(form, data = df, ties = ties)
}

tic()
fits_iqr <- map(setNames(exposures_krg, exposures_krg),
                ~ fit_cox_exposure(data, .x, control_vars, time_var, dependent))
toc()

summary(fits_iqr$pm25_krg_full_iqr)
summary(fits_iqr$pm25_krg_t1_iqr)
summary(fits_iqr$pm25_krg_t2_iqr)
summary(fits_iqr$pm25_krg_t3_iqr)
summary(fits_iqr$pm25_krg_30_iqr)
summary(fits_iqr$pm25_krg_4_iqr)

broom::tidy(fits_iqr$pm25_krg_full_iqr, exponentiate = TRUE, conf.int = TRUE, conf.level = 0.95)
broom::tidy(fits_iqr$pm25_krg_t1_iqr, exponentiate = TRUE, conf.int = TRUE, conf.level = 0.95)

AFcoxph(fits_iqr$pm25_krg_full_iqr, data = data, exposure = "pm25_krg_full", times = time_var)
