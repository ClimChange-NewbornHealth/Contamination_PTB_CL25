# Code 1: Explorer missing values ----

## Settings ----
source("Code/0.1 Settings.R")
source("Code/0.2 Functions.R")
source("Code/0.3 Packages.R")

# Data path 
data_inp <- "Data/Input/Contant_series/"
data_out <- "Data/Output/"

## IDW ---- 

pm_idw <- rio::import(paste0(data_inp, "idw_pm25.csv")) |> 
  #rename(fecha=`...1`) |> 
  pivot_longer(!fecha, names_to = "municipio", values_to = "pm25_idw")

o3_idw <- rio::import(paste0(data_inp, "idw_o3.csv")) |> 
  #rename(fecha=`...1`) |> 
  pivot_longer(!fecha, names_to = "municipio", values_to = "o3_idw")

glimpse(pm_idw)
glimpse(o3_idw)

data_idw <- pm_idw |> 
  left_join(o3_idw, by=c("municipio", "fecha"))

glimpse(data_idw)
summary(data_idw)

## Kriging ---- 

pm_krg <- rio::import(paste0(data_inp, "utm_interpol_pm25.csv")) |> 
  drop_na() |> 
  select(-var1.var) |> 
  rename(pm25_krg = var1.pred)

glimpse(pm_krg)

o3_krg  <- rio::import(paste0(data_inp, "utm_interpol_o3.csv")) |> 
  drop_na() |> 
  select(-var1.var) |> 
  rename(o3_krg = var1.pred)

glimpse(o3_krg)

data_krg <- pm_krg |> 
  left_join(o3_krg, by=c("utm_x", "utm_y", "municipio", "date")) |> 
  relocate(pm25_krg, .before = o3_krg)

glimpse(data_krg)
summary(data_krg)

## Check complete data ---- 

unique(data_idw$municipio)
unique(data_krg$municipio)

setdiff(unique(data_idw$municipio), unique(data_krg$municipio))
setdiff(unique(data_krg$municipio), unique(data_idw$municipio))

# Adjust dates
data_krg <- data_krg |> 
  mutate(date = as.Date(date))  # de IDate a Date

data_idw <- data_idw |> 
  rename(date = fecha) |> 
  mutate(date = as.Date(date))  # de chr a Date

# Luego puedes hacer el left_join
data <- data_krg |> 
  left_join(data_idw, by = c("date", "municipio"))

glimpse(data)
summary(data)

# Save data ---------
save(data, file=paste0(data_out, "series_pm25_o3_kriging_idw", ".RData"))
