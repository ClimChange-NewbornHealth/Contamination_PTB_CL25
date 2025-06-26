# 6.2 Check models -----

rm(list=ls())
## Settings ----
source("Code/0.1 Settings.R")
source("Code/0.2 Functions.R")
source("Code/0.3 Packages.R")

## Data models -----
m_full <- readRDS("Output/Models/Contamination_models.rds")
m_summer <- readRDS("Output/Models/Contamination_models_Ozone_summer.rds")
exp_data <- rio::import(paste0("Data/Output/series_births_exposition_pm25_o3_kriging_idw", ".RData")) |> drop_na()

## PM25 models -----

m_full[[2]][,c(1:2, 6:7)]


m1 <- as.formula(paste("Surv(weeks, ", "birth_preterm", ") ~ ", "pm25_krg_full_10", 
                              "+ sex + age_group_mom + educ_group_mom + job_group_mom +",
                              "age_group_dad + educ_group_dad + job_group_dad +",
                              "factor(month_week1) + factor(year_week1) + vulnerability"))

m2 <- as.formula(paste("Surv(weeks, ", "birth_preterm", ") ~ ", "pm25_idw_full_10", 
                              "+ sex + age_group_mom + educ_group_mom + job_group_mom +",
                              "age_group_dad + educ_group_dad + job_group_dad +",
                              "factor(month_week1) + factor(year_week1) + vulnerability"))

m3 <- as.formula(paste("Surv(weeks, ", "birth_preterm", ") ~ ", "pm25_krg_t1_10 + pm25_krg_t2_10 + pm25_krg_t3_10", 
                              "+ sex + age_group_mom + educ_group_mom + job_group_mom +",
                              "age_group_dad + educ_group_dad + job_group_dad +",
                              "factor(month_week1) + factor(year_week1) + vulnerability"))

coxph(m1, data = exp_data) |> 
  broom::tidy(exponentiate = TRUE, conf.int = TRUE, conf.level = 0.95) |>
  select(term, estimate, conf.low, conf.high) 
  
m_full[[25]][,c(1:2, 6:8)]

coxph(m2, data = exp_data) |> 
  broom::tidy(exponentiate = TRUE, conf.int = TRUE, conf.level = 0.95) |>
  select(term, estimate, conf.low, conf.high) 

m_full[[73]][,c(1:2, 6:8)]

coxph(m3, data = exp_data) |> 
  broom::tidy(exponentiate = TRUE, conf.int = TRUE, conf.level = 0.95) |>
  select(term, estimate, conf.low, conf.high) 

m_full[[193]][,c(1:2, 6:8)]



## Ozone models -----
