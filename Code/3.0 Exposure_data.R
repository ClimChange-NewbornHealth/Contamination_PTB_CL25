# Code 1: Explorer missing values ----

## Settings ----
source("Code/0.1 Settings.R")
source("Code/0.2 Functions.R")
source("Code/0.3 Packages.R")

# Data path 
data_inp <- "Data/Input/Exposure/"
data_out <- "Data/Output/"

## IDW ---- 

pm25_idw <- rio::import(paste0(data_inp, "births_PM25_idw_exposure.csv")) |> clean_names()
glimpse(pm25_idw)
summary(pm25_idw)
length(unique(pm25_idw$id))

pm25_idw <- pm25_idw |> 
  rename(
    pm25_idw_full = full_window,
    pm25_idw_30 =  x30_days_window,
    pm25_idw_4 = x4_days_window,
  )

o3_idw <- rio::import(paste0(data_inp, "births_O3_idw_exposure.csv")) |> clean_names()
glimpse(o3_idw)
summary(o3_idw)
length(unique(o3_idw$id))

o3_idw <- o3_idw |> 
  rename(
    o3_idw_full = full_window,
    o3_idw_30 =  x30_days_window,
    o3_idw_4 = x4_days_window,
  ) |> 
  select(id, com, o3_idw_full, o3_idw_30, o3_idw_4)

exp_idw <- pm25_idw |> 
  left_join(o3_idw, by=c("id", "com"))

glimpse(exp_idw)

## Kriging ---- 

exp_krg <- rio::import(paste0(data_inp, "utm_exposure.csv")) |> clean_names()
glimpse(exp_krg)

exp_krg  <- exp_krg  |> 
  rename(
    pm25_krg_full = pm25_gest,
    pm25_krg_30 =  pm25_mes,
    pm25_krg_4 = pm25_4d,
    o3_krg_full = o3_gest,
    o3_krg_30 =  o3_mes,
    o3_krg_4 = o3_4d,
  ) 

## Check complete data ---- 

glimpse(exp_krg)
glimpse(exp_idw)

setdiff(unique(exp_krg$id), unique(exp_idw$id))
setdiff(unique(exp_idw$id), unique(exp_krg$id))

# Join
exp <- exp_krg |> 
  left_join(select(exp_idw, c("id", "com", 
                               "pm25_idw_full", "pm25_idw_30", "pm25_idw_4",
                                "o3_idw_full", "o3_idw_30", "o3_idw_4",
                              )), 
                               
                               by = c("id", "com"))

glimpse(exp)
summary(exp)

# Save data ---------
save(exp, file=paste0(data_out, "series_exposition_pm25_o3_kriging_idw", ".RData"))
