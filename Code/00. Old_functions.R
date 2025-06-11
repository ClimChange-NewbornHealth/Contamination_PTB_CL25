# Old functions

# Function calculate exposure data
calc_exposure_periods <- function(start_date, end_date, comuna, cont_data) {
  cont_data |>
    filter(
      name_com == comuna,
      date >= start_date,
      date <= end_date
    ) |>
    summarise(
      pm25_krg = mean(pm25_krg, na.rm = TRUE),
      o3_krg   = mean(o3_krg, na.rm = TRUE),
      pm25_idw = mean(pm25_idw, na.rm = TRUE),
      o3_idw   = mean(o3_idw, na.rm = TRUE)
    )
}


tic()
bw_data_expo <- bw_data |>
  rowwise() |>
  mutate(
    expos_full = list(calc_exposure_periods(date_start_week_gest, date_ends_week_gest, name_com, cont_data)),
    expos_30   = list(calc_exposure_periods(date_ends_week_gest - 30, date_ends_week_gest, name_com, cont_data)),
    expos_4    = list(calc_exposure_periods(date_ends_week_gest - 4, date_ends_week_gest, name_com, cont_data))
  ) |>
  unnest_wider(expos_full) |>
  rename(
    pm25_krg_full = pm25_krg,
    o3_krg_full   = o3_krg,
    pm25_idw_full = pm25_idw,
    o3_idw_full   = o3_idw
  ) |>
  unnest_wider(expos_30) |>
  rename(
    pm25_krg_30 = pm25_krg,
    o3_krg_30   = o3_krg,
    pm25_idw_30 = pm25_idw,
    o3_idw_30   = o3_idw
  ) |>
  unnest_wider(expos_4) |>
  rename(
    pm25_krg_4 = pm25_krg,
    o3_krg_4   = o3_krg,
    pm25_idw_4 = pm25_idw,
    o3_idw_4   = o3_idw
  ) |>
  ungroup()
toc()
beepr::beep(8)