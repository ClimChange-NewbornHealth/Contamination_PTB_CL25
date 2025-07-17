# Code 6: Survival models preliminar ----

rm(list=ls())
## Settings ----
source("Code/0.1 Settings.R")
source("Code/0.2 Functions.R")
source("Code/0.3 Packages.R")

# Data path
data_out <- "Data/Output/"

## Data ---- 

exp_data <- rio::import(paste0(data_out, "series_births_exposition_pm25_o3_kriging_idw", ".RData")) |> drop_na()
summary(exp_data)
glimpse(exp_data) # 713.918

exp_vars <- exp_data |>
  select(starts_with("pm25_krg"), starts_with("pm25_idw"),
         starts_with("o3_krg"),   starts_with("o3_idw")) |>
  names()

t1 <- grep("_t1$",  exp_vars, value = TRUE)
groups1 <- lapply(t1, function(v) {
  base <- sub("_t1$", "", v)
  paste0(base, c("_t1","_t2","_t3"))
})

t1_10   <- grep("_t1_10$",  exp_vars, value = TRUE)
groups10 <- lapply(t1_10, function(v) {
  base <- sub("_t1_10$", "", v)
  paste0(base, c("_t1_10","_t2_10","_t3_10"))
})

t1_iqr  <- grep("_t1_iqr$", exp_vars, value = TRUE)
groups_iqr <- lapply(t1_iqr, function(v) {
  base <- sub("_t1_iqr$", "", v)
  paste0(base, c("_t1_iqr","_t2_iqr","_t3_iqr"))
})

# Vector con todas las variables de trimestres (10 + iqr)
trimestre_vars <- c(unlist(groups1, use.names = FALSE),
                    unlist(groups10, use.names = FALSE),
                    unlist(groups_iqr, use.names = FALSE))

# Variables de exposición “individuales”
single_vars <- setdiff(exp_vars, trimestre_vars)

grouped1   <- vapply(groups1,   paste, collapse = " + ", FUN.VALUE = "")
grouped10   <- vapply(groups10,   paste, collapse = " + ", FUN.VALUE = "")
grouped_iqr <- vapply(groups_iqr, paste, collapse = " + ", FUN.VALUE = "")

exp_vars_models <- c(single_vars, grouped1, grouped10, grouped_iqr)

#exp_vars_models <- exp_vars_models[!str_detect(exp_vars_models, "o3")]

dependent_vars <- c("birth_preterm", "lbw", "tlbw", "sga",
                    "birth_very_preterm", "birth_moderately_preterm", 
                    "birth_late_preterm") # , "birth_term", "birth_posterm"

control_vars <- c("weeks", "sex", 
    "age_group_mom", "educ_group_mom", "job_group_mom",
    "age_group_dad", "educ_group_dad", "job_group_dad",
    "month_week1", "year_week1", "covid", "vulnerability")

exp_data <- exp_data |> 
  dplyr::select(all_of(c("id",  dependent_vars, control_vars, exp_vars, trimestre_vars 
  )))

glimpse(exp_data)

# All models execution
combinations <- expand.grid(
  dependent  = dependent_vars,
  predictor  = exp_vars_models,
  stringsAsFactors = FALSE
)
combinations

writexl::write_xlsx(combinations, path =  paste0("Output/", "Models/", "List_models_contamination", ".xlsx"))

## HR COX Models ---- 
fit_cox_model <- function(dependent, predictor, data) {

  formula <- as.formula(paste("Surv(weeks, ", dependent, ") ~ ", predictor, 
                              "+ sex + age_group_mom + educ_group_mom + job_group_mom +",
                              "age_group_dad + educ_group_dad + job_group_dad +",
                              "factor(month_week1) + factor(year_week1) + factor(covid) + vulnerability"))
  
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
           " factor(month_week1) + factor(year_week1) + factor(covid) + vulnerability")
  )
  
  # 2) Ajustar modelo logístico en escala logit
  model_fit <- glm(fml, data = data, family = binomial(link = "logit"))
  
  # 3) Extraer tabla básica (coeficientes en log-odds)
  tbl <- broom::tidy(model_fit, conf.int = FALSE, exponentiate = FALSE)
  
  # 4) Parámetro z para el nivel de confianza deseado
  z <- abs(stats::qnorm((1 - conf.level) / 2))
  
  # 5) Calcular OR y sus IC de Wald
  tbl <- tbl |> 
    mutate(
      estimate = exp(estimate), 
      ci.low = exp(estimate - z * std.error),
      ci.high = exp(estimate + z * std.error)
    )
  
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
saveRDS(results_list, file = "Output/Models/Contamination_models.rds")

results_cox <- bind_rows(results_list)

writexl::write_xlsx(results_cox, path =  paste0("Output/", "Models/", "Cox_models_contamination", ".xlsx"))

results_cox <- rio::import(paste0("Output/", "Models/", "Cox_models_contamination", ".xlsx"))

## Plots with Exposure Effects COX Models ---- 

exp_vars <- str_subset(unique(exp_vars), "^o3.*(_10)$", negate = TRUE) # Test

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

plot_data <- results_filtered |>
  filter(term %in% exp_vars) |>
  mutate(
    # Kriging vs IDW
    method = case_when(
      str_detect(term, "_krg_") ~ "Kriging",
      str_detect(term, "_idw_") ~ "IDW"
    ) |> factor(levels = c("Kriging", "IDW")),
    # PM2.5 vs Ozone
    pollutant = case_when(
      str_detect(term, "^pm25") ~ "PM2.5",
      str_detect(term, "^o3")   ~ "Ozone"
    ),
    # Ventana / trimestre
    period = case_when(
      str_detect(term, "_4")      ~ "4-day",
      str_detect(term, "_30")      ~ "30-day",
      str_detect(term, "_t1")     ~ "T1",
      str_detect(term, "_t2")     ~ "T2",
      str_detect(term, "_t3")     ~ "T3",
      str_detect(term, "_full")    ~ "Full"
    ) |> factor(levels = c("4-day","30-day","T1","T2","T3","Full")),
    # Media vs IQR
    is_iqr = str_detect(term, "_iqr"),
    # Etiqueta Y y orden estricto
    metric = paste0(period, if_else(is_iqr, "", "")) |>
      factor(levels = c(
        # medias
        "4-day","30-day","T1","T2","T3","Full"
        # IQR  
        #"4-day IQR","30-day IQR","T1 IQR","T2 IQR","T3 IQR","Full IQR"
      ))
  )

outcomes <- c("birth_preterm", "lbw", "tlbw", "sga")

make_cell <- function(df, method, is_iqr, outcome, x_limits) {
  df_cell <- df %>% 
    filter(method == method,
           dependent_var == outcome,
           is_iqr == is_iqr)
  
  ggplot(df_cell, aes(x = estimate, y = period)) +
    geom_vline(xintercept = 1, linetype = "dashed", color = "grey50") +
    geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                   height = 0.2) +
    geom_point(size = 2) +
    scale_x_continuous(limits = x_limits) +
    labs(
      # solo el método en el subtítulo de la fila
      subtitle = paste0(method, if_else(is_iqr, " IQR", " X/10")),
      x        = "HR (95% CI)",
      y        = NULL
    ) +
    theme_light() +
    theme(
      legend.position     = "none",
      plot.subtitle       = element_text(face = "bold"),
      panel.grid          = element_blank(),
      axis.text.y         = element_text(size = 10)
    )
}

# construye el panel para un data.frame de entrada
make_panel <- function(df, x_limits) {
  combos <- tribble(
    ~method,   ~is_iqr,
    "Kriging", FALSE,
    "Kriging", TRUE,
    "IDW",     FALSE,
    "IDW",     TRUE
  )
  
  # 1) fila de títulos de columna
  col_titles <- outcomes %>% 
    map(~ ggplot() + 
          labs(title = .x) +
          theme_void() +
          theme(
            plot.title = element_text(face="bold", hjust=0.5)
          )
    )
  title_row <- wrap_plots(col_titles, ncol = 4)
  
  # 2) para cada combo generar su fila de 4 gráficas
  data_rows <- combos %>% 
    pmap(function(method, is_iqr) {
      cells <- outcomes %>% 
        map(~ make_cell(df, method, is_iqr, .x, x_limits))
      wrap_plots(cells, ncol = 4)
    })
  
  # 3) ensamblar todo
  title_row / wrap_plots(data_rows, ncol = 1) +
    plot_layout(heights = c(0.01, 1))
}

# los outcomes que quieres en columnas
outcomes <- c("birth_preterm", "lbw", "tlbw", "sga")

# filtrar & mutar tu objeto original
plot_data <- results_filtered %>%
  filter(str_detect(term, "_(10|iqr)$")) %>%
  mutate(
    method    = case_when(
      str_detect(term, "_krg_") ~ "Kriging",
      str_detect(term, "_idw_") ~ "IDW"
    ) %>% factor(levels = c("Kriging","IDW")),
    pollutant = case_when(
      str_detect(term, "^pm25") ~ "PM2.5",
      str_detect(term, "^o3")   ~ "Ozone"
    ),
    period = case_when(
      str_detect(term, "_4_")   ~ "4-day",
      str_detect(term, "_30_")  ~ "30-day",
      str_detect(term, "_t1")   ~ "T1",
      str_detect(term, "_t2")   ~ "T2",
      str_detect(term, "_t3")   ~ "T3",
      str_detect(term, "_full") ~ "Full"
    ) %>% factor(levels = c("4-day","30-day","T1","T2","T3","Full")),
    is_iqr = str_detect(term, "_iqr")
  )

# Generar paneles
panel_pm25 <- plot_data %>%
  filter(pollutant == "PM2.5") %>%
  make_panel(x_limits = c(0.5, 1.75)) 

panel_o3 <- plot_data %>%
  filter(pollutant == "Ozone") %>%
  make_panel(x_limits = c(0.5, 1.5)) 

# Para visualizar
panel_pm25
ggsave("Output/Models/HR_Cox_panel_test.png",
  #plot     = last_plot(),
  res      = 300,
  width    = 30,
  height   = 30,
  units    = 'cm',
  scaling  = 0.9,
  device   = ragg::agg_png
)


panel_o3
ggsave("Output/Models/HR_Cox_panel_test2.png",
  #plot     = last_plot(),
  res      = 300,
  width    = 30,
  height   = 30,
  units    = 'cm',
  scaling  = 0.9,
  device   = ragg::agg_png
)

