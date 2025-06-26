# Code 3.0: Generate exposure data ----

## Settings ----
source("Code/0.1 Settings.R")
source("Code/0.2 Functions.R")
source("Code/0.3 Packages.R")

# Data path 
data_inp <- "Data/Input/Exposure/"
data_out <- "Data/Output/"

## Births data ---- 

bw_data <- rio::import(paste0(data_out, "births_2010_2020", ".RData")) |>
  select(id, com, name_com, date_nac, weeks, date_start_week_gest, date_ends_week_gest)

glimpse(bw_data)

## Contamination data ---- 

cont_data <- rio::import(paste0(data_out, "series_pm25_o3_kriging_idw", ".RData")) 
glimpse(cont_data)
summary(cont_data)

edit_com <- c(
  "Conchalí" = "Conchali",
  "Estación Central" = "Estacion Central",
  "Maipú" = "Maipu",
  "Ñuñoa" = "Nunoa",
  "Peñalolén" = "Penalolen",
  "San Joaquín" = "San Joaquin",
  "San Ramón" = "San Ramon"
)

cont_data <- cont_data |> 
  mutate(
    name_com = recode(municipio, !!!edit_com, .default = municipio)
  )

setdiff(unique(bw_data$name_com), unique(cont_data$name_com))
setdiff(unique(cont_data$name_com), unique(bw_data$name_com))

## Exposure data ---- 

calc_exposure_periods <- function(start_date, end_date, cont_data) {
  cont_data |>
    filter(date >= start_date, date <= end_date) |>
    dplyr::summarise(
      #pm25_krg_iqr = IQR(.data$pm25_krg, na.rm = TRUE),
      #o3_krg_iqr   = IQR(.data$o3_krg,   na.rm = TRUE),
      #pm25_idw_iqr = IQR(.data$pm25_idw, na.rm = TRUE),
      #o3_idw_iqr   = IQR(.data$o3_idw,   na.rm = TRUE),

      pm25_krg    = mean(pm25_krg,    na.rm = TRUE),
      o3_krg      = mean(o3_krg,      na.rm = TRUE),
      pm25_idw    = mean(pm25_idw,    na.rm = TRUE),
      o3_idw      = mean(o3_idw,      na.rm = TRUE),
    )
}

calc_all_exposures <- function(start_date, end_date, cont_data) {
  # Full / 30 / 4
  full <- calc_exposure_periods(start_date,           end_date, cont_data)
  d30  <- calc_exposure_periods(end_date - days(30),  end_date, cont_data)
  d4   <- calc_exposure_periods(end_date - days(4),   end_date, cont_data)
  
  # Trimester
  t1_end <- min(start_date + weeks(13), end_date)
  t2_end <- min(start_date + weeks(26), end_date)
  
  tri1 <- calc_exposure_periods(start_date,       t1_end, cont_data)
  tri2 <- calc_exposure_periods(t1_end + days(1), t2_end, cont_data)
  tri3 <- calc_exposure_periods(t2_end + days(1), end_date, cont_data)
  
  # combinar todo en un tibble de salida
  tibble(
    # full
    pm25_krg_full    = full$pm25_krg,
    o3_krg_full      = full$o3_krg,
    pm25_idw_full    = full$pm25_idw,
    o3_idw_full      = full$o3_idw,
    #pm25_krg_full_iqr = full$pm25_krg_iqr,
    #o3_krg_full_iqr   = full$o3_krg_iqr,
    #pm25_idw_full_iqr = full$pm25_idw_iqr,
    #o3_idw_full_iqr   = full$o3_idw_iqr,
    
    # 30 días
    pm25_krg_30    = d30$pm25_krg,
    o3_krg_30      = d30$o3_krg,
    pm25_idw_30    = d30$pm25_idw,
    o3_idw_30      = d30$o3_idw,
    #pm25_krg_30_iqr = d30$pm25_krg_iqr,
    #o3_krg_30_iqr   = d30$o3_krg_iqr,
    #pm25_idw_30_iqr = d30$pm25_idw_iqr,
    #o3_idw_30_iqr   = d30$o3_idw_iqr,
    
    # 4 días
    pm25_krg_4    = d4$pm25_krg,
    o3_krg_4      = d4$o3_krg,
    pm25_idw_4    = d4$pm25_idw,
    o3_idw_4      = d4$o3_idw,
    #pm25_krg_4_iqr = d4$pm25_krg_iqr,
    #o3_krg_4_iqr   = d4$o3_krg_iqr,
    #pm25_idw_4_iqr = d4$pm25_idw_iqr,
    #o3_idw_4_iqr   = d4$o3_idw_iqr,
    
    # Trimester (mean)
    pm25_krg_t1 = tri1$pm25_krg,
    pm25_krg_t2 = tri2$pm25_krg,
    pm25_krg_t3 = tri3$pm25_krg,

    o3_krg_t1   = tri1$o3_krg,
    o3_krg_t2   = tri2$o3_krg,
    o3_krg_t3   = tri3$o3_krg,

    pm25_idw_t1 = tri1$pm25_idw,
    pm25_idw_t2 = tri2$pm25_idw,
    pm25_idw_t3 = tri3$pm25_idw,

    o3_idw_t1   = tri1$o3_idw,
    o3_idw_t2   = tri2$o3_idw,
    o3_idw_t3   = tri3$o3_idw
    
    # Trimester (IQR)
    # pm25_krg_t1_iqr = tri1$pm25_krg_iqr,
    # pm25_krg_t2_iqr = tri2$pm25_krg_iqr,
    # pm25_krg_t3_iqr = tri3$pm25_krg_iqr,

    # o3_krg_t1_iqr   = tri1$o3_krg_iqr,
    # o3_krg_t2_iqr   = tri2$o3_krg_iqr,
    # o3_krg_t3_iqr   = tri3$o3_krg_iqr,

    # pm25_idw_t1_iqr = tri1$pm25_idw_iqr,
    # pm25_idw_t2_iqr = tri2$pm25_idw_iqr,
    # pm25_idw_t3_iqr = tri3$pm25_idw_iqr,

    # o3_idw_t1_iqr   = tri1$o3_idw_iqr,
    # o3_idw_t2_iqr   = tri2$o3_idw_iqr,
    # o3_idw_t3_iqr   = tri3$o3_idw_iqr
  )
}

#bw_data2 <- bw_data |> sample_n(100)

parallel::detectCores() 
plan(multisession, workers = parallel::detectCores() - 4)

tic()
results <- future_pmap_dfr(
  list(
    start_date = bw_data$date_start_week_gest,
    end_date   = bw_data$date_ends_week_gest
  ),
  function(start_date, end_date) {
    calc_all_exposures(start_date, end_date, cont_data)
  },
  .options = furrr_options(seed = TRUE)
)

bw_data_expo <- bind_cols(bw_data, results)
toc()
beepr::beep(8)

glimpse(bw_data_expo)
summary(bw_data_expo)
plan(sequential)

## Check results ----

check <- bw_data_expo |> 
  select(id, name_com, date_start_week_gest, date_ends_week_gest,
         pm25_krg_full, 
         pm25_krg_30,  
         pm25_krg_4) %>%  # aquí solo pm25; repite para o3/idw si quieres
  slice_sample(n = 3)

mun <- check$name_com[2]
s <- as.Date(check$date_start_week_gest[2])
e <- as.Date(check$date_ends_week_gest[2])
e30 <- e - days(30) 

cont_data |> filter(# municipio==mun & 
  (date >= s & date <= e)
) |> 
  select(date, pm25_krg) |> 
  summarise(mean = mean(pm25_krg))

cont_data |> filter( # municipio==mun & 
  (date >= e30 & date <= e)
) |> 
  select(date, pm25_krg) |> 
  summarise(mean = mean(pm25_krg))

# Results ok

## Save results ----

save(bw_data_expo, file=paste0(data_out, "series_exposition_pm25_o3_kriging_idw", ".RData"))