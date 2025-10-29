# Code 7.0 DLM PretermBirth ----
# Replication code with Tarik

## Settings ----
source("Code/0.1 Settings.R")
source("Code/0.2 Packages.R")
source("Code/0.3 Functions.R")

# Data path
data_out <- "Data/Output/"

## Open Data ---- 

bw <- rio::import(paste0(data_out, "series_births_exposition_pm25_o3_kriging_idw_long", ".RData")) |> 
  drop_na() 

#ids <- bw |>
#  slice_sample(n=5000) |>
#  pull(id)

#bw <- bw |> 
  #filter(id %in% ids) |> 
  #filter(weeks <= 37)

glimpse(bw) #  27.509.363 obs
setDT(bw)

bw_pm25_wide <- bw |> 
  select(id:birth_posterm, week_gest_num, pm25_krg_week_iqr) |> 
  rename(week = week_gest_num) |> 
  pivot_wider(
    names_from = week, 
    values_from = "pm25_krg_week_iqr", 
    names_prefix = "pm25_krg_iqr_week_")

glimpse(bw_pm25_wide)

bw_o3_wide <- bw |> 
  select(id:birth_posterm, week_gest_num, o3_krg_week_iqr) |> 
  rename(week = week_gest_num) |> 
  pivot_wider(
    names_from = week, 
    values_from = "o3_krg_week_iqr", 
    names_prefix = "o3_krg_iqr_week_")

glimpse(bw_o3_wide)


## Specification data ---- 

# rs
bw_pm25_long <- bw |> 
  select(id:birth_posterm, week_gest_num, pm25_krg_week_iqr) |> 
  mutate(week = week_gest_num) |> 
  arrange(id, week) |> 
  group_by(id) |> 
  dplyr::mutate(pm25_krg_week_iqr_lagged = purrr::map_dbl(dplyr::row_number(), function(i) {
    if (week[i] == 0) return(NA_real_)
    past_rows <- which(week < week[i])
    weights <- 1 / (week[i] - week[past_rows])
    exposures <- pm25_krg_week_iqr[past_rows]
    sum(weights * exposures, na.rm = TRUE)
  }))


setDT(bw_pm25_long)

bw_pm25_wide_lagged <- bw_pm25_long |> 
  select(id, week, pm25_krg_week_iqr_lagged) %>%
  pivot_wider(
    names_from = week,
    values_from = pm25_krg_week_iqr_lagged,
    names_prefix = "pm25_krg_week_iqr_lagged_week_"
  ) |> 
  ungroup()

bw_o3_long <- bw |> 
  select(id:birth_posterm, week_gest_num, o3_krg_week_iqr) |> 
  mutate(week = week_gest_num) |> 
  arrange(id, week) |> 
  group_by(id) |> 
  dplyr::mutate(o3_krg_week_iqr_lagged = purrr::map_dbl(dplyr::row_number(), function(i) {
    if (week[i] == 0) return(NA_real_)
    past_rows <- which(week < week[i])
    weights <- 1 / (week[i] - week[past_rows])
    exposures <- o3_krg_week_iqr[past_rows]
    sum(weights * exposures, na.rm = TRUE)
  }))

setDT(bw_o3_long)

bw_o3_wide_lagged <- bw_o3_long |> 
  select(id, week, o3_krg_week_iqr_lagged) %>%
  pivot_wider(
    names_from = week,
    values_from = o3_krg_week_iqr_lagged,
    names_prefix = "o3_krg_week_iqr_lagged_week_"
  ) |> 
  ungroup()

data_pm25 <- left_join(bw_pm25_wide, 
  bw_pm25_wide_lagged,
  by = "id") |> 
  arrange(id)

setDT(data_pm25)

data_o3 <- left_join(bw_o3_wide, 
  bw_o3_wide_lagged,
  by = "id") |> 
  arrange(id)

setDT(data_o3)

## Define the model PM25 ----

# Exposition variables 
expo <- grep("^pm25_krg_iqr_week_\\d+$", names(data_pm25), value = TRUE)
expo_lag <- grep("^pm25_krg_week_iqr_lagged_week_\\d+$", names(data_pm25), value = TRUE)

# Extract week numbers
expo_weeks <- 1:36
expo_lag_weeks <- 1:36

# Week 1 to 36 (where lagged variables exist)
common_weeks <- intersect(expo_weeks, expo_lag_weeks)
common_weeks <- common_weeks[common_weeks >= 1 & common_weeks <= 36]

# Variables
expo_vars <- paste0("pm25_krg_iqr_week_", common_weeks)
expo_vars_lag <- paste0("pm25_krg_week_iqr_lagged_week_", common_weeks)

# Save results
mod_pm25 <- data.table()

cores <- max(1L, future::availableCores() - 4L)
plan(multisession, workers = cores)

run_cox <- function(i, expo_vars, expo_vars_lag, common_weeks, data) {
  exp <- expo_vars[i]
  lag <- expo_vars_lag[i]

  formula_str <- paste0(
    "Surv(weeks, birth_preterm) ~ ",
    exp, " + ", lag,
    " + sex + age_group_mom + educ_group_mom + job_group_mom +",
    " age_group_dad + educ_group_dad + job_group_dad +",
    " factor(month_week1) + factor(year_week1) + vulnerability + factor(com)"
  )

  fml <- as.formula(formula_str)
  mod <- coxph(fml, data = data, ties = "efron")

  beta  <- unname(mod$coefficients[1])
  se    <- sqrt(mod$var[1, 1])
  lower <- beta - qnorm(0.975) * se
  upper <- beta + qnorm(0.975) * se

  data.frame(
    Week      = common_weeks[i],
    Exposure  = exp,
    Lagged    = lag,
    `No Obs`  = mod$n,
    beta      = beta,
    se        = se,
    AIC       = AIC(mod),
    BIC       = BIC(mod),
    Lower     = lower,
    Upper     = upper,
    beta_exp  = exp(beta),      # HR por 1 unidad (ajusta si quieres por 5 o 10)
    Lower_exp = exp(lower),
    Upper_exp = exp(upper),
    stringsAsFactors = FALSE
  )
}

idx <- seq_along(expo_vars)

tic()
res_list <- furrr::future_map(
  idx,
  ~ run_cox(.x, expo_vars, expo_vars_lag, common_weeks, data_pm25),
  .progress = TRUE
)
toc()

res_combo <- data.table::rbindlist(res_list, use.names = TRUE, fill = TRUE)

g1 <- ggplot(res_combo, aes(x = Week, y = beta_exp)) +
  geom_point() +
  geom_errorbar(aes(ymin = Lower_exp, ymax = Upper_exp), width = 0.2) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "gray40") +
  scale_y_continuous(limits = c(0.9, 1.1)) +
  scale_x_continuous(breaks= seq(0,36, by=3)) +
  labs(
    title = expression(bold("A. Overall - PM" [2.5])),
    x = "Gestational Weeks",
    y = "HR (95% CI)"
  ) +
  theme_bw() +
  theme(panel.grid = element_blank())
g1
ggsave("Output/DLM/Contamination_models_pm25.png",
       res = 300, width = 15, height = 10, units = "cm",
       scaling = 0.9, device = ragg::agg_png)

saveRDS(res_combo, file = "Output/DLM/Contamination_models_pm25.rds")

t1 <- res_combo |> 
  mutate(across(where(is.numeric), ~ formatC(., format = "f", digits = 4, decimal.mark = ".")))

## Define the model O3 ----

# Exposition variables 
expo <- grep("^o3_krg_iqr_week_\\d+$", names(data_o3), value = TRUE)
expo_lag <- grep("^o3_krg_week_iqr_lagged_week_\\d+$", names(data_o3), value = TRUE)

# Extract week numbers
expo_weeks <- 1:36
expo_lag_weeks <- 1:36

expo_weeks <- as.numeric(gsub("o3_krg_iqr_week_", "", expo))
expo_lag_weeks <- as.numeric(gsub("o3_krg_week_iqr_lagged_week_", "", expo_lag))

# Week 1 to 36 (where lagged variables exist)
common_weeks <- intersect(expo_weeks, expo_lag_weeks)
common_weeks <- common_weeks[common_weeks >= 1 & common_weeks <= 36]


# Variables
expo_vars <- paste0("o3_krg_iqr_week_", common_weeks)
expo_vars_lag <- paste0("o3_krg_week_iqr_lagged_week_", common_weeks)

# Save results
mod_o3 <- data.table()

cores <- max(1L, future::availableCores() - 4L)
plan(multisession, workers = cores)

run_cox <- function(i, expo_vars, expo_vars_lag, common_weeks, data) {
  exp <- expo_vars[i]
  lag <- expo_vars_lag[i]

  formula_str <- paste0(
    "Surv(weeks, birth_preterm) ~ ",
    exp, " + ", lag,
    " + sex + age_group_mom + educ_group_mom + job_group_mom +",
    " age_group_dad + educ_group_dad + job_group_dad +",
    " factor(month_week1) + factor(year_week1) + vulnerability + factor(com)"
  )

  fml <- as.formula(formula_str)
  mod <- coxph(fml, data = data, ties = "efron")
  #l[i] <- mod

  beta  <- unname(mod$coefficients[1])
  se    <- sqrt(mod$var[1, 1])
  lower <- beta - qnorm(0.975) * se
  upper <- beta + qnorm(0.975) * se

  data.frame(
    Week      = common_weeks[i],
    Exposure  = exp,
    Lagged    = lag,
    `No Obs`  = mod$n,
    beta      = beta,
    se        = se,
    AIC       = AIC(mod),
    BIC       = BIC(mod),
    Lower     = lower,
    Upper     = upper,
    beta_exp  = exp(beta),      # HR por 1 unidad (ajusta si quieres por 5 o 10)
    Lower_exp = exp(lower),
    Upper_exp = exp(upper),
    stringsAsFactors = FALSE
  )
}

idx <- seq_along(expo_vars)

tic()
res_list <- furrr::future_map(
  idx,
  ~ run_cox(.x, expo_vars, expo_vars_lag, common_weeks, data_o3),
  .progress = TRUE
)
toc()

res_combo <- data.table::rbindlist(res_list, use.names = TRUE, fill = TRUE)

g2 <- ggplot(res_combo, aes(x = Week, y = beta_exp)) +
  geom_point() +
  geom_errorbar(aes(ymin = Lower_exp, ymax = Upper_exp), width = 0.2) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "gray40") +
  scale_y_continuous(limits = c(0.9, 1.2)) +
  scale_x_continuous(breaks= seq(0,36, by=3)) +
  labs(
    title = expression(bold("B. Overall - O" [3])),
    x = "Gestational Weeks",
    y = "HR (95% CI)"
  ) +
  theme_bw() +
  theme(panel.grid = element_blank())

g2

ggsave("Output/DLM/Contamination_models_o3.png",
       res = 300, width = 15, height = 10, units = "cm",
       scaling = 0.9, device = ragg::agg_png)

saveRDS(res_combo, file = "Output/DLM/Contamination_models_o3.rds")

t2 <- res_combo |> 
  mutate(across(where(is.numeric), ~ formatC(., format = "f", digits = 4, decimal.mark = ".")))

ggarrange(g1, g2, ncol = 2)

ggsave("Output/DLM/Contamination_models_pm25_o3.png",
       res = 300, width = 25, height = 10, units = "cm",
       scaling = 0.9, device = ragg::agg_png)

## Table with results ----

l <- list(pm25 = t1, 
          o3 = t2)

writexl::write_xlsx(l, "Output/DLM/Contamination_models_pm25_o3.xlsx")
