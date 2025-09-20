# Code 1: Explorer missing values ----

## Settings ----
source("Code/0.1 Settings.R")
source("Code/0.2 Packages.R")
source("Code/0.3 Functions.R")

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

# Descriptive analysis (vs real station values) ---------
glimpse(data)
load(file=paste0(data_inp, "analytical_series_full_2000_2023", ".RData"))
glimpse(series)

g0 <- ggplot(series, aes(x = pm25)) +
    geom_density(alpha = 0.3, linewidth = 0.8, fill = "darkgreen", color = "darkgreen") +
    #scale_fill_manual(values = c("Kriging" = "#1f77b4", "IDW" = "#ff7f0e")) +
    #scale_color_manual(values = c("Kriging" = "#1f77b4", "IDW" = "#ff7f0e")) +
    labs(x = "PM2.5", y = "Density", title = "PM2.5") +
    theme_light() +
    theme(
      panel.grid = element_blank(),
      legend.position = "top",
      legend.title = element_blank(),
      plot.title = element_text(size = 11, hjust = 0)
    ) +
    coord_cartesian(xlim = c(0, 100))

g0

g1 <- ggplot(data, aes(x = pm25_krg)) +
    geom_density(alpha = 0.3, linewidth = 0.8, fill = "red", color = "red") +
    #scale_fill_manual(values = c("Kriging" = "#1f77b4", "IDW" = "#ff7f0e")) +
    #scale_color_manual(values = c("Kriging" = "#1f77b4", "IDW" = "#ff7f0e")) +
    labs(x = "PM2.5", y = "Density", title = "PM2.5 Kriging") +
    theme_light() +
    theme(
      panel.grid = element_blank(),
      legend.position = "top",
      legend.title = element_blank(),
      plot.title = element_text(size = 11, hjust = 0)
    ) +
    coord_cartesian(xlim = c(0, 100))

g1


g2 <- ggplot(data, aes(x = pm25_idw)) +
    geom_density(alpha = 0.3, linewidth = 0.8, fill = "blue", color = "blue") +
    #scale_fill_manual(values = c("Kriging" = "#1f77b4", "IDW" = "#ff7f0e")) +
    #scale_color_manual(values = c("Kriging" = "#1f77b4", "IDW" = "#ff7f0e")) +
    labs(x = "PM2.5", y = "Density", title = "PM2.5 IDW") +
    theme_light() +
    theme(
      panel.grid = element_blank(),
      legend.position = "top",
      legend.title = element_blank(),
      plot.title = element_text(size = 11, hjust = 0)
    ) +
    coord_cartesian(xlim = c(0, 100))

g2

g0b <- ggplot(series, aes(x = o3)) +
    geom_density(alpha = 0.3, linewidth = 0.8, fill = "darkgreen", color = "darkgreen") +
    #scale_fill_manual(values = c("Kriging" = "#1f77b4", "IDW" = "#ff7f0e")) +
    #scale_color_manual(values = c("Kriging" = "#1f77b4", "IDW" = "#ff7f0e")) +
    labs(x = "PM2.5", y = "Density", title = "Ozone") +
    theme_light() +
    theme(
      panel.grid = element_blank(),
      legend.position = "top",
      legend.title = element_blank(),
      plot.title = element_text(size = 11, hjust = 0)
    ) +
    coord_cartesian(xlim = c(0, 70))

g0b

g1b <- ggplot(data, aes(x = o3_krg)) +
    geom_density(alpha = 0.3, linewidth = 0.8, fill = "red", color = "red") +
    #scale_fill_manual(values = c("Kriging" = "#1f77b4", "IDW" = "#ff7f0e")) +
    #scale_color_manual(values = c("Kriging" = "#1f77b4", "IDW" = "#ff7f0e")) +
    labs(x = "PM2.5", y = "Density", title = "Ozone Kriging") +
    theme_light() +
    theme(
      panel.grid = element_blank(),
      legend.position = "top",
      legend.title = element_blank(),
      plot.title = element_text(size = 11, hjust = 0)
    ) +
    coord_cartesian(xlim = c(0, 70))

g1b


g2b <- ggplot(data, aes(x = o3_idw)) +
    geom_density(alpha = 0.3, linewidth = 0.8, fill = "blue", color = "blue") +
    #scale_fill_manual(values = c("Kriging" = "#1f77b4", "IDW" = "#ff7f0e")) +
    #scale_color_manual(values = c("Kriging" = "#1f77b4", "IDW" = "#ff7f0e")) +
    labs(x = "PM2.5", y = "Density", title = "Ozone IDW") +
    theme_light() +
    theme(
      panel.grid = element_blank(),
      legend.position = "top",
      legend.title = element_blank(),
      plot.title = element_text(size = 11, hjust = 0)
    ) +
    coord_cartesian(xlim = c(0, 70))

g2b


ggarrange(g0, g1, g2, 
          g0b, g1b, g2b, 
          nrow = 2, ncol = 3)

ggsave(
  filename = "Output/Descriptives/Distribution_contamination.png",
  #plot     = last_plot(),
  res      = 300,
  width    = 30,
  height   = 20,
  units    = 'cm',
  scaling  = 0.9,
  device   = ragg::agg_png
)
