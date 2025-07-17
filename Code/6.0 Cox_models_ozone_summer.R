# Code 6: Survival models preliminar ----

rm(list=ls())
## Settings ----
source("Code/0.1 Settings.R")
source("Code/0.2 Functions.R")
source("Code/0.3 Packages.R")

# Data path
data_out <- "Data/Output/"

## Data ---- 

exp_data <- rio::import(paste0(data_out, "series_births_exposition_pm25_o3_kriging_idw_ozone_summer", ".RData")) |> drop_na()
summary(exp_data)
glimpse(exp_data) # 252498

exp_vars <- exp_data |>
  select(starts_with("o3_krg"),  starts_with("o3_idw")) |>
  select(contains("30"), contains("4")) |> 
  names()

dependent_vars <- c("birth_preterm", "lbw", "tlbw", "sga",
                    "birth_very_preterm", "birth_moderately_preterm", 
                    "birth_late_preterm") # , "birth_term", "birth_posterm"

control_vars <- c("weeks", "sex", 
    "age_group_mom", "educ_group_mom", "job_group_mom",
    "age_group_dad", "educ_group_dad", "job_group_dad",
    "month_week1", "year_week1", "covid", "vulnerability")

exp_data <- exp_data |> 
  dplyr::select(all_of(c("id",  dependent_vars, control_vars, exp_vars
  )))

glimpse(exp_data)

# All models execution
combinations <- expand.grid(
  dependent  = dependent_vars,
  predictor  = exp_vars,
  stringsAsFactors = FALSE
)
combinations

writexl::write_xlsx(combinations, path =  paste0("Output/", "Models/", "List_models_contamination_summer_ozone", ".xlsx"))

## HR COX Models ---- 
fit_cox_model <- function(dependent, predictor, data) {

  formula <- as.formula(paste("Surv(weeks, ", dependent, ") ~ ", predictor, 
                              "+ sex + age_group_mom + educ_group_mom + job_group_mom +",
                              "age_group_dad + educ_group_dad + job_group_dad +",
                              "factor(year_week1) + factor(covid) + vulnerability")) # factor(month_week1) + 
  
  # Ajuste del modelo de Cox usando el argumento `data`
  model_fit <- coxph(formula, data = data)
  
  # Extraer resultados con tidy
  results <- broom::tidy(model_fit, exponentiate = TRUE, conf.int = TRUE, conf.level = 0.95) |>
    select(term, estimate, std.error, statistic, p.value, conf.low, conf.high) |>
    mutate(dependent_var = dependent, predictor = predictor)  # Añadir columnas de identificación
  return(results)

  rm(model_fit); gc()
}

fit_logit_model <- function(dependent, predictor, data, conf.level = 0.95) {
  
  # 1) Construir fórmula
  fml <- as.formula(
    paste0(dependent, " ~ ", predictor,
           " + sex + age_group_mom + educ_group_mom + job_group_mom +",
           " age_group_dad + educ_group_dad + job_group_dad +",
           " factor(year_week1) + factor(covid) + vulnerability") # factor(month_week1) + 
  )
  
  # 2) Ajustar modelo logístico en escala logit
  model_fit <- glm(fml, data = data, family = binomial(link = "logit"))
  
  # 3) Extraer tabla básica (coeficientes en log-odds)
  tbl <- broom::tidy(model_fit, conf.int = FALSE, exponentiate = FALSE)
  
  # 4) Parámetro z para el nivel de confianza deseado
  z <- qnorm(1 - (1 - 0.95)/2)
  #z <- abs(stats::qnorm((1 - conf.level) / 2))
  
  # 5) Calcular OR y sus IC de Wald
  tbl <- tbl |> 
    mutate(
      or = exp(estimate), 
      conf.low = exp(estimate - z * std.error),
      conf.high = exp(estimate + z * std.error)
    ) |> 
    mutate(estimate = or)
  
  # 6) Reordenar y renombrar columnas
  results <- tbl |> 
    select(term, estimate, std.error, statistic, p.value, conf.low, conf.high) |> 
    mutate(dependent_var = dependent, predictor = predictor)

  return(results)

  rm(model_fit); gc()
}

## Parallel models -----

plan(multisession, workers = parallel::detectCores() - 4)
options(future.globals.maxSize = 1.5 * 1024^3)
tic()
results_list <- future_lapply(seq_len(nrow(combinations)), function(i) {
  message("Iteración ", i, " en PID ", Sys.getpid())
  dep <- combinations$dependent[i]
  pred <- combinations$predictor[i]
  
  # Si el dependent es lbw, tlbw o sga → usa logit, si no → usa cox
  if (dep %in% c("lbw", "tlbw", "sga")) {
    fit_logit_model(dep, pred, data = exp_data)
  } else {
    fit_cox_model(dep, pred, data = exp_data)
  }
})
toc() 
plan(sequential)

# Save models results
saveRDS(results_list, file = "Output/Models/Contamination_models_Ozone_summer.rds")

results_cox <- bind_rows(results_list)

writexl::write_xlsx(results_cox, path =  paste0("Output/", "Models/", "Cox_models_contamination_Ozone_summer", ".xlsx"))

results_cox <- rio::import(paste0("Output/", "Models/", "Cox_models_contamination_Ozone_summer", ".xlsx"))

## Plots with Exposure Effects COX Models ---- 

#exp_vars <- str_subset(unique(exp_vars), "^o3.*(_10)$", negate = TRUE) # Test moment

results_filtered <- results_cox |>
  filter(term %in% c(exp_vars))

results_filtered <- results_filtered |> 
  mutate(estimate = round(estimate, 4), 
           std.error = round(std.error, 3),
           statistic = round(statistic, 3),
           p.value = round(p.value, 3),
           conf.low = round(conf.low, 4),
           conf.high = round(conf.high, 4)) 

#exp_vars <- str_subset(unique(results_filtered$term), "_(10|iqr)$")

# 1) Prepara tus datos, SIN filtrar ninguno de los sufijos
plot_data <- results_filtered %>%
  filter(str_detect(term, "_krg_") | str_detect(term, "_idw_")) %>%
  mutate(
    method = if_else(str_detect(term, "_krg_"), "Kriging", "IDW") %>%
             factor(levels = c("Kriging","IDW")),
    pollutant = if_else(str_detect(term, "^pm25"), "PM2.5", "Ozone") %>%
                factor(levels = c("PM2.5","Ozone")),
    period = case_when(
      str_detect(term, "_4($|_)")    ~ "4-day",
      str_detect(term, "_30($|_)")   ~ "30-day",
      str_detect(term, "_t1($|_)")   ~ "T1",
      str_detect(term, "_t2($|_)")   ~ "T2",
      str_detect(term, "_t3($|_)")   ~ "T3",
      str_detect(term, "_full($|_)") ~ "Full"
    ) %>% factor(levels = c("4-day","30-day","T1","T2","T3","Full")),
    suffix = case_when(
      str_detect(term, "_iqr$") ~ "IQR",
      str_detect(term, "_10$")  ~ "X/10",
      TRUE                      ~ "Raw"
    ) %>% factor(levels = c("Raw","X/10","IQR")),
    metric = if_else(
      suffix == "Raw",
      as.character(period),
      paste0(period, " ", suffix)
    ) %>% factor(levels = c(
      # Raw
      "4-day","30-day","T1","T2","T3","Full",
      # X/10
      "4-day X/10","30-day X/10","T1 X/10","T2 X/10","T3 X/10","Full X/10",
      # IQR
      "4-day IQR","30-day IQR","T1 IQR","T2 IQR","T3 IQR","Full IQR"
    ))
  )

outcomes <- c("birth_preterm", "lbw", "tlbw", "sga")

# 2) Función para cada celda, con droplevels() para quedarnos sólo con las 6 filas necesarias
make_cell <- function(data, meth, suff, out, x_lim, show_y) {
  df_cell <- data %>%
    filter(
      method        == meth,
      suffix        == suff,
      dependent_var == out
    ) %>%
    droplevels()  # elimina los niveles de `metric` que no estén en df_cell
  
  ggplot(df_cell, aes(x = estimate, y = metric)) +
    geom_vline(xintercept = 1, linetype = "dashed", color = "grey50") +
    geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
    geom_point(size = 1) +
    scale_x_continuous(limits = x_lim) +
    labs(
      subtitle = paste0(meth, " ", suff),
      x        = "HR (95% CI)",
      y        = NULL
    ) +
    theme_light(base_size = 10) +
    theme(
      legend.position     = "none",
      plot.subtitle       = element_text(face = "bold", size = 10),
      panel.grid          = element_blank(),
      axis.text.y         = if (show_y) element_text(size = 8) else element_blank(),
      axis.ticks.y        = if (show_y) element_line() else element_blank(),
      plot.margin         = margin(2, 2, 2, 2, "pt")
    )
}

# 3) Función para ensamblar todo en un solo wrap_plots()
make_panel <- function(data, x_lim) {
  combos <- expand.grid(
    method = c("Kriging","IDW"),
    suffix = c("Raw","X/10","IQR"),
    stringsAsFactors = FALSE
  )
  
  # 3.1) Títulos de columna
  title_plots <- map(outcomes, ~
    ggplot() +
      labs(title = .x) +
      theme_void() +
      theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 11))
  )
  
  # 3.2) Celdas (fila × columna)
  cell_plots <- vector("list", length(outcomes) * nrow(combos))
  idx <- 1
  for(i in seq_len(nrow(combos))) {
    for(j in seq_along(outcomes)) {
      cell_plots[[idx]] <- make_cell(
        data   = data,
        meth   = combos$method[i],
        suff   = combos$suffix[i],
        out    = outcomes[j],
        x_lim  = x_lim,
        show_y = (j == 1)    # sólo primera columna muestra eje Y
      )
      idx <- idx + 1
    }
  }
  
  # 3.3) Combinar TODO en un solo wrap_plots()
  wrap_plots(
    plots   = c(title_plots, cell_plots),
    ncol    = length(outcomes),
    widths  = rep(1, length(outcomes)),
    heights = c(0.1, rep(1, nrow(combos))),
    align   = "hv"
  )
}

# 4) Genera y muestra los paneles
panel_o3 <- plot_data %>%
  filter(pollutant == "Ozone") %>%
  make_panel(x_lim = c(0.5, 1.5))

panel_o3
ggsave("Output/Models/HR_Cox_panel_Ozone_summer.png",
  #plot     = last_plot(),
  res      = 300,
  width    = 30,
  height   = 30,
  units    = 'cm',
  scaling  = 0.9,
  device   = ragg::agg_png
)

writexl::write_xlsx(plot_data, path =  paste0("Output/", "Models/", "Cox_models_contamination_subset_Ozone_summer", ".xlsx"))
