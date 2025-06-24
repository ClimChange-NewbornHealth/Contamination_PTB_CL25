## Exposition Data ---- 

exp_data <- rio::import(paste0(data_out, "series_births_exposition_pm25_o3_kriging_idw", ".RData")) %>% drop_na()
glimpse(exp_data)

exposure_vars <- c(
  "pm25_krg_full_10", "pm25_krg_30_10", "pm25_krg_4_10",
  "o3_krg_full_10", "o3_krg_30_10", "o3_krg_4_10",
  "pm25_idw_full_10", "pm25_idw_30_10", "pm25_idw_4_10",
  "o3_idw_full_10",   "o3_idw_30_10",   "o3_idw_4_10"
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
