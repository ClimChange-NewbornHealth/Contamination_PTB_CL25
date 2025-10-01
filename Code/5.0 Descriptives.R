# Code 5: Descriptives ----

rm(list=ls())
## Settings ----
source("Code/0.1 Settings.R")
source("Code/0.2 Packages.R")
source("Code/0.3 Functions.R")

# Data path
data_out <- "Data/Output/"

## Data ---- 

exp_data <- rio::import(paste0(data_out, "series_births_exposition_pm25_o3_kriging_idw", ".RData")) %>% drop_na()
exp_data_o3 <- rio::import(paste0(data_out, "series_births_exposition_pm25_o3_kriging_idw_ozone_summer", ".RData")) |> drop_na()
exp_data_pm <- rio::import(paste0(data_out, "series_births_exposition_pm25_o3_kriging_idw_pm25_winter", ".RData")) |> drop_na()

glimpse(exp_data)
glimpse(exp_data_o3)
glimpse(exp_data_pm)

# Full Sample
tab1 <-  exp_data %>% 
   select(
          birth_preterm,
          birth_very_preterm,      
          birth_moderately_preterm,
          birth_late_preterm,      
          birth_term, 
          #birth_posterm, 
          lbw, 
          tlbw, 
          sga,
          tbw, weeks, sex,  
          age_group_mom, educ_group_mom, job_group_mom,
          age_group_dad, educ_group_dad, job_group_dad, 
          year_nac, month_nac, 
          sovi, vulnerability
          
   ) %>% 
   mutate(
    birth_preterm=factor(birth_preterm),
    birth_very_preterm=factor(birth_very_preterm),      
    birth_moderately_preterm=factor(birth_moderately_preterm),
    birth_late_preterm=factor(birth_late_preterm),      
    birth_term=factor(birth_term), 
    lbw=factor(lbw),
    tlbw=factor(tlbw),
    sga=factor(sga),
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

# Summer Sample
tab2 <-  exp_data_o3 %>% 
   select(
          birth_preterm,
          birth_very_preterm,      
          birth_moderately_preterm,
          birth_late_preterm,      
          birth_term, 
          #birth_posterm, 
          lbw, 
          tlbw, 
          sga,
          tbw, weeks, sex,  
          age_group_mom, educ_group_mom, job_group_mom,
          age_group_dad, educ_group_dad, job_group_dad, 
          year_nac, month_nac, 
          sovi, vulnerability
          
   ) %>% 
   mutate(
    birth_preterm=factor(birth_preterm),
    birth_very_preterm=factor(birth_very_preterm),      
    birth_moderately_preterm=factor(birth_moderately_preterm),
    birth_late_preterm=factor(birth_late_preterm),      
    birth_term=factor(birth_term), 
    lbw=factor(lbw),
    tlbw=factor(tlbw),
    sga=factor(sga),
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

# Winter Sample
tab3 <-  exp_data_pm %>% 
   select(
          birth_preterm,
          birth_very_preterm,      
          birth_moderately_preterm,
          birth_late_preterm,      
          birth_term, 
          #birth_posterm, 
          lbw, 
          tlbw, 
          sga,
          tbw, weeks, sex,  
          age_group_mom, educ_group_mom, job_group_mom,
          age_group_dad, educ_group_dad, job_group_dad, 
          year_nac, month_nac, 
          sovi, vulnerability
          
   ) %>% 
   mutate(
    birth_preterm=factor(birth_preterm),
    birth_very_preterm=factor(birth_very_preterm),      
    birth_moderately_preterm=factor(birth_moderately_preterm),
    birth_late_preterm=factor(birth_late_preterm),      
    birth_term=factor(birth_term), 
    lbw=factor(lbw),
    tlbw=factor(tlbw),
    sga=factor(sga),
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



tab <- tab1 |> cbind(tab2) |> cbind(tab3) 
glimpse(tab)

writexl::write_xlsx(tab, path =  paste0("Output/", "Descriptives/",  "Descriptives", ".xlsx"))
