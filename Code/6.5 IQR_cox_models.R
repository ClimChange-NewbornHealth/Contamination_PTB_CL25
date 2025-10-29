# Code 6: Survival models preliminar ----
# 6.5 IQR by models 
 
rm(list=ls())
## Settings ----
source("Code/0.1 Settings.R")
source("Code/0.2 Packages.R")
source("Code/0.3 Functions.R")

# Data path
data_out <- "Data/Output/"

iqr_ov <- rio::import(paste0(data_out, "Data_IQR_ref_values.xlsx")) |> 
  pivot_longer(cols = everything(), 
               names_to = "metric", 
               values_to = "Full")

iqr_win <- rio::import(paste0(data_out, "Data_IQR_ref_values_winter.xlsx")) |> 
    pivot_longer(cols = everything(), 
               names_to = "metric", 
               values_to = "Winter")

iqr_sum <- rio::import(paste0(data_out, "Data_IQR_ref_values_summer.xlsx")) |> 
      pivot_longer(cols = everything(), 
               names_to = "metric", 
               values_to = "Summer")

iqrs <- iqr_ov |> 
  left_join(iqr_win, by = "metric") |> 
  left_join(iqr_sum, by = "metric") |> 
  mutate(across(where(is.numeric), ~ formatC(., format = "f", digits = 2, decimal.mark = ".")))

iqrs

# Save results 
writexl::write_xlsx(iqrs, path =  paste0("Output/", "Models/", "Table_IQRs", ".xlsx"))
