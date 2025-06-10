# Code 4: Find and join Data ----

rm(list=ls())
## Settings ----
source("Code/0.1 Settings.R")
source("Code/0.2 Functions.R")
source("Code/0.3 Packages.R")

# Data path 
data_inp <- "Data/Input/"
data_out <- "Data/Output/"
data_sovi <- "Data/Input/SOVI/"

## Data ---- 

# Exposure  
exp <- rio::import(paste0(data_out, "series_exposition_pm25_o3_kriging_idw", ".RData"))
glimpse(exp)
length(unique(exp$com))

# BW
bw_data <- rio::import(paste0(data_out, "births_2010_2020", ".RData"))
glimpse(bw_data)
length(unique(bw_data$com))

setdiff(unique(exp$name_com), unique(bw_data$name_com))
setdiff(unique(bw_data$name_com), unique(exp$name_com))

# Filter data only municipality in exposure
bw_data <- bw_data %>% filter(com %in% unique(exp$com)) # 776299

# Add vulnerability data 
sovi <- rio::import(paste0(data_sovi, "sovi_datasets", ".RData")) %>% 
  select(-name_comuna)  %>% 
  rename(vulnerability=vulnerablidad) %>% 
    mutate(vulnerability = fct_recode(vulnerability,
      "Low" = "Baja",
      "Medium-low" = "Medio-baja",
      "Medium-high" = "Medio-alta"))

sovi <- sovi |> 
  rename(com = cod_com)

## Join Data ---- 
glimpse(bw_data)
glimpse(exp)
glimpse(sovi)

exp_j <- exp |> 
  select(id, o3_krg_full:o3_idw_4)

glimpse(exp_j)

bw_data_join <- bw_data |> 
  left_join(exp_j, by="id") |> 
  left_join(sovi, by="com") |> 
  relocate(c("sovi", "vulnerability"), .before = "birth_preterm")

glimpse(bw_data_join)

## Save Data ----
save(bw_data_join, file=paste0(data_out, "series_births_exposition_pm25_o3_kriging_idw", ".RData"))
