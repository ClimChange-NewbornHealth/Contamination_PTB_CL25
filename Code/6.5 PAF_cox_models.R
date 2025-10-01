# Code 6.5: Attributable fraction population ----

rm(list=ls())
## Settings ----
source("Code/0.1 Settings.R")
source("Code/0.2 Packages.R")
source("Code/0.3 Functions.R")

# Data 

data_out <- "Data/Output/"
#data <- rio::import(paste0(data_out, "series_births_exposition_pm25_o3_kriging_idw", ".RData")) |> drop_na()
data <- rio::import(paste0(data_out, "series_births_exposition_pm25_o3_kriging_idw_pm25_winter", ".RData")) |> drop_na()
#data <- rio::import(paste0(data_out, "series_births_exposition_pm25_o3_kriging_idw_ozone_summer", ".RData")) |> drop_na()
glimpse(data) 

# Specific model test 
form <- as.formula(paste("Surv(weeks, ", "birth_preterm", ") ~ ", 
                  paste("pm25_krg_4_iqr",
                  "+ sex + age_group_mom + educ_group_mom + job_group_mom +",
      "age_group_dad + educ_group_dad + job_group_dad +",
      "factor(year_week1) + vulnerability"
                  )
                  ))

model_fit <- coxph(form, data = data, ties = "breslow",
                     x    = TRUE, y = TRUE, model = TRUE
                    
)

summary(model_fit)

vars_needed <- all.vars(form)      
data <- data |> select(vars_needed)

## Global PAF ----
# Levin Form
paf_levin <- function(Pe, RR, RR_l = NA, RR_u = NA) {
  PAF <- (Pe * (RR - 1)) / (1 + Pe * (RR - 1))
  LCL <- UCL <- NA_real_
  if (is.finite(RR_l) && is.finite(RR_u)) {
    LCL <- (Pe * (RR_l - 1)) / (1 + Pe * (RR_l - 1))
    UCL <- (Pe * (RR_u - 1)) / (1 + Pe * (RR_u - 1))
  }
  data.frame(Pe = Pe, RR = RR, PAF = PAF, LCL = LCL, UCL = UCL)
}

paf_levin(Pe = mean(data$birth_preterm), RR = exp(coef(model_fit)[1]))

## PAF COX models ----
# Source: https://cran.r-project.org/web/packages/AF/index.html
# Chen, L., Lin, D. Y., and Zeng, D. (2010). Attributable fraction functions for censored event times. Biometrika 97, 713-726.
# Sjölander, A. and Vansteelandt, S. (2014). Doubly robust estimation of attributable fractions in survival analysis. Statistical Methods in Medical Research. doi: 10.1177/0962280214564003.
# https://github.com/johnfergusonNUIG/graphPAF/blob/master/R/PAF_calc_continuous.R
# 

# Intervención: truncar al percentil 50 de la propia muestra
cf_p <- function(df) {
  thr <- quantile(df$pm25_krg_4_iqr, 0.1, na.rm = TRUE)
  df$pm25_krg_4_iqr <- pmin(df$pm25_krg_4_iqr, thr)
  df
}

t_vec <- seq(28, 37, by = 1)

# Resultado puntual
res_pt <- paf_cox_manual(
  fit    = model_fit,
  data   = data,
  expose = "pm25_krg_4_iqr",
  t_vec  = t_vec,
  cf_fun = cf_p
)
print(res_pt)

tic()
ci_ind <- paf_cox_boot(
  fit    = model_fit,
  data   = data,
  expose = "pm25_krg_4_iqr",
  t_vec  = t_vec,
  cf_fun = cf_p,
  B      = 2000
)
toc() # 30 minutes
print(ci_ind)

#     t        PAF          LCL        UCL
# 1  28 0,01592033 -0,006133958 0,03705880
# 2  29 0,01590946 -0,006129520 0,03703570
# 3  30 0,01589477 -0,006123124 0,03700248
# 4  31 0,01587749 -0,006115993 0,03696627
# 5  32 0,01584815 -0,006104638 0,03690173
# 6  33 0,01580968 -0,006088565 0,03681432
# 7  34 0,01574127 -0,006060181 0,03666417
# 8  35 0,01562190 -0,006010809 0,03640552
# 9  36 0,01538871 -0,005914930 0,03590590
# 10 37 0,01538871 -0,005914930 0,03590590


paf <- AFcoxph(
        model_fit, 
        data = data, 
        exposure = data$pm25_krg_full_iqr
        #times = seq(28, 44, by = 1)
      )

summary(paf)
plot(paf, CI = TRUE)

## Classic PAF ----

PAF_calc_continuous(
  model= model_fit,
  riskfactor_vec = "pm25_krg_full_iqr", 
  q_vec = c("0.01"),
  data = data,
  calculation_method = "D", 
  ci = TRUE,
  boot_rep = 50, 
  ci_type = "norm",
  #t_vector = seq(28, 44, by = 2), 
  verbose=TRUE
)
