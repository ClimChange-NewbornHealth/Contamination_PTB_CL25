# Code 5: Descriptives ----

rm(list=ls())
## Settings ----
source("Code/0.1 Settings.R")
source("Code/0.2 Functions.R")
source("Code/0.3 Packages.R")

# Data path
data_out <- "Data/Output/"

## Data ---- 

exp_data <- rio::import(paste0(data_out, "series_births_exposition_pm25_o3_kriging_idw", ".RData")) %>% drop_na()
glimpse(exp_data)

date_ref <- as.Date("2020-12-31")
date_fcb <- date_ref - weeks(42) # Maybe max(exp_data$weeks)

exp_data_filter <- exp_data |> filter(date_nac <= date_fcb) 
nrow(exp_data) - nrow(exp_data_filter)
summary(exp_data_filter$date_ends_week_gest)
summary(exp_data_filter$date_nac)

exposure_vars <- c(
  "pm25_krg_full", "pm25_krg_30", "pm25_krg_4",
  "o3_krg_full", "o3_krg_30", "o3_krg_4",
  "pm25_idw_full", "pm25_idw_30", "pm25_idw_4",
  "o3_idw_full",   "o3_idw_30",   "o3_idw_4"
)

exp_data <- exp_data |> 
  mutate(across(all_of(exposure_vars), ~ .x / 10, .names = "{.col}_10"))

tab1 <-  exp_data %>% 
   select(
          birth_preterm,
          birth_very_preterm,      
          birth_moderately_preterm,
          birth_late_preterm,      
          birth_term, 
          #birth_posterm, 
          tbw, weeks, sex,  
          age_group_mom, educ_group_mom, job_group_mom,
          age_group_dad, educ_group_dad, job_group_dad, 
          year_nac, month_nac, 
          sovi, vulnerability,
          o3_krg_full:o3_idw_4_10
          
   ) %>% 
   mutate(
    birth_preterm=factor(birth_preterm),
    birth_very_preterm=factor(birth_very_preterm),      
    birth_moderately_preterm=factor(birth_moderately_preterm),
    birth_late_preterm=factor(birth_late_preterm),      
    birth_term=factor(birth_term), 
   ) %>% 
   st(,
   digits = 1, 
   out="return", 
   add.median = TRUE,
   fixed.digits = TRUE, 
   simple.kable = FALSE,
   title="",
   numformat = NA) %>% 
   data.frame() 

  writexl::write_xlsx(tab1, path =  paste0("Output/", "Descriptives/",  "Descriptives", ".xlsx"))
