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
exp2 <- rio::import(paste0(data_out, "series_exposition_pm25_o3_kriging_idw_old", ".RData"))

glimpse(exp)
glimpse(exp2)

summary(exp)
summary(exp2)

length(unique(exp$com))

# BW
bw_data <- rio::import(paste0(data_out, "births_2010_2020", ".RData"))
glimpse(bw_data)
length(unique(bw_data$com))

setdiff(unique(exp$name_com), unique(bw_data$name_com))
setdiff(unique(bw_data$name_com), unique(exp$name_com))

# Filter data only municipality in exposure
bw_data <- bw_data %>% filter(com %in% unique(exp$com)) # 713918

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
  select(id, pm25_krg_full:o3_idw_t3)

glimpse(exp_j)

bw_data_join <- bw_data |> 
  left_join(exp_j, by="id") |> 
  left_join(sovi, by="com") |> 
  relocate(c("sovi", "vulnerability"), .before = "birth_preterm")

glimpse(bw_data_join)

bw_data_join$vulnerability <- droplevels(
  bw_data_join$vulnerability[bw_data_join$vulnerability != "Alta"]
)

## Exposure vars ----

exposure_vars <- colnames(bw_data_join)[38:61]
exposure_vars

# exposure_vars/10
bw_data_join <- bw_data_join |> 
  mutate(across(all_of(exposure_vars), ~ .x / 10, .names = "{.col}_10")) 

# exposure_vars/IQR(exposure_vars)
iqr_vals <- bw_data_join |>
  summarise(across(all_of(exposure_vars), ~ IQR(.x, na.rm = TRUE))) |>
  as.list()
iqr_vals # Calculate IQR per variable 

writexl::write_xlsx(iqr_vals |> data.frame(), paste0(data_out, "Data_IQR_ref_values.xlsx"))

bw_data_join <- bw_data_join |> 
  mutate(across(all_of(exposure_vars), ~ .x / iqr_vals[[cur_column()]], .names = "{.col}_iqr"))

glimpse(bw_data_join)

## Save Data Full ----
save(bw_data_join, file=paste0(data_out, "series_births_exposition_pm25_o3_kriging_idw", ".RData"))

## Save Data Ozone (Summer) ----

bw_data_ozone <- bw_data_join |>
  rowwise() |>
  mutate(
    last30_start = date_nac - days(29),
    last30_months = list(unique(month(seq(last30_start, date_nac, by = "1 day")))),
    exposed_last30_summer = all(last30_months %in% c(11, 12, 1, 2, 3))
  ) |>
  ungroup() |>
  filter(exposed_last30_summer) |>
  select(-last30_start, -last30_months, -exposed_last30_summer) |> 
  select(id:o3_idw_t3)

glimpse(bw_data_ozone)

# exposure_vars/10
bw_data_ozone <- bw_data_ozone |> 
  mutate(across(all_of(exposure_vars), ~ .x / 10, .names = "{.col}_10")) 

# exposure_vars/IQR(exposure_vars)
iqr_vals <- bw_data_ozone |>
  summarise(across(all_of(exposure_vars), ~ IQR(.x, na.rm = TRUE))) |>
  as.list()
iqr_vals # Calculate IQR per variable 

writexl::write_xlsx(iqr_vals |> data.frame(), paste0(data_out, "Data_IQR_ref_values_summer.xlsx"))

bw_data_ozone <- bw_data_ozone |> 
  mutate(across(all_of(exposure_vars), ~ .x / iqr_vals[[cur_column()]], .names = "{.col}_iqr"))

glimpse(bw_data_ozone)

save(bw_data_ozone, file=paste0(data_out, "series_births_exposition_pm25_o3_kriging_idw_ozone_summer", ".RData"))
