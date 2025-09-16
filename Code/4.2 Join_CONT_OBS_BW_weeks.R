# Code 4: Find and join Data ----

rm(list=ls())
## Settings ----
source("Code/0.1 Settings.R")
source("Code/0.2 Functions.R")
source("Code/0.2.1 Expositions_functions.R")
source("Code/0.3 Packages.R")

# Data path 
data_inp <- "Data/Input/"
data_out <- "Data/Output/"
data_sovi <- "Data/Input/SOVI/"

## Data ---- 

# Exposure  
exp <- rio::import(paste0(data_inp, "Contant_series/","series_imputated_missforest_2010_2023", ".RData"))
unique(exp$station) # 11 stations
# Exclude talagante 
exp <- exp |> 
  mutate(name_com = station) |> 
  mutate(name_com = if_else(name_com == "Parque O'Higgins", "Santiago", name_com)) 

# BW
bw_data <- rio::import(paste0(data_out, "births_2010_2020_weeks_long", ".RData"))
glimpse(bw_data)
length(unique(bw_data$com))

setdiff(unique(exp$name_com), unique(bw_data$name_com))
setdiff(unique(bw_data$name_com), unique(exp$name_com))

# Filter data only municipality in exposure
bw_data <- bw_data %>% filter(name_com %in% unique(exp$name_com)) # 314087

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
glimpse(sovi)

bw_data_join <- bw_data |> 
  left_join(sovi, by="com") |> 
  relocate(c("sovi", "vulnerability"), .before = "birth_preterm")

glimpse(bw_data_join)

bw_data_join$vulnerability <- droplevels(
  bw_data_join$vulnerability[bw_data_join$vulnerability != "Alta"]
)

bw_data <- bw_data_join

## Exposure Data ----

source("Code/0.2.1 Expositions_functions.R")

# Data in parts 
# Dado que la carga computacional es muy grande, optaremos por dividir los datos en 50 trozos q
# Guardaremos estos trozo y los procesaremos uno a uno. 
bw_data_join <- bw_data_join |> 
  select(id, name_com, week_gest_num, date_start_week, date_end_week)

tic()
parts(data=bw_data_join, 
      path="Data/Output/",
      folder="Parts_Data_Births_obs",
      num_parts = 50)
toc()

## Clean memory use
rm(list = ls()[!ls() %in%  c("exp", 
                             "calculate_cont_stats",
                             "process_files_parallel",
                             "load_and_extract_df",
                             "bw_data",
                             "data_out"
                             
)])

gc() # Explicit clean memory

# Process each part 
tic()
process_files_parallel(
  input_directory  = "Data/Output/Parts_Data_Births_obs",
  output_directory = "Data/Output/Parts_Data_Contamination_obs",
  cont_data        = exp,                  # tu tabla completa de contaminación
  calc_func        = calculate_cont_stats, # tu función de cálculo
  workers          = parallel::detectCores() - 4
)
toc() 

# Time execution: 33,44322 min 

# Check
load("Data/Output/Parts_Data_Contamination_obs/part_01_results_processed.RData")
test <- exp[
  date >= as.Date("2010-03-29") & date < as.Date("2010-04-05") & name_com == "El Bosque",
  .(daily_pm25)
]
mean(test$daily_pm25)

# Append results
file_list <- list.files(path = "Data/Output/Parts_Data_Contamination_obs", pattern = "*.RData", full.names = TRUE)
births_weeks_temp <- map_df(file_list, load_and_extract_df)

## Adjust Exposure vars ----
exposure_vars <- c("pm25_week", "o3_week")
exposure_vars

# exposure_vars/10
births_weeks_temp <- births_weeks_temp |> 
  mutate(across(all_of(exposure_vars), ~ .x / 10, .names = "{.col}_10")) 

# exposure_vars/IQR(exposure_vars)
iqr_vals <- births_weeks_temp |>
  summarise(across(all_of(exposure_vars), ~ IQR(.x, na.rm = TRUE))) |>
  as.list()
iqr_vals # Calculate IQR per variable 

writexl::write_xlsx(iqr_vals |> data.frame(), paste0(data_out, "Data_IQR_ref_values_weeks_obs.xlsx"))

births_weeks_temp <- births_weeks_temp |> 
  mutate(across(all_of(exposure_vars), ~ .x / iqr_vals[[cur_column()]], .names = "{.col}_iqr"))

glimpse(births_weeks_temp)

## Save Data Full ----

# Calculations
save(births_weeks_temp, file=paste0(data_out, "series_exposition_pm25_o3_kriging_idw_long_obs", ".RData"))

# Complete data
bw_data_full <- bw_data |> 
  left_join(births_weeks_temp, by=c("id", "name_com", "week_gest_num", "date_start_week", "date_end_week")) |> 
  select(-I)

glimpse(bw_data_full)

save(bw_data_full, file=paste0(data_out, "series_births_exposition_pm25_o3_kriging_idw_long_obs", ".RData"))

