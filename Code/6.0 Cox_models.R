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

exp_vars <- exp_data |> select(pm25_krg_full:o3_idw_4_10) |> colnames()
exp_vars

dependent_vars <- c("birth_preterm", "birth_very_preterm", "birth_moderately_preterm", 
                    "birth_late_preterm") # , "birth_term", "birth_posterm"

control_vars <- c("weeks", "sex", 
    "age_group_mom", "educ_group_mom", "job_group_mom",
    "age_group_dad", "educ_group_dad", "job_group_dad",
    "month_week1", "year_week1", "vulnerability")

exp_data <- exp_data |> 
  dplyr::select(all_of(c("id", dependent_vars, control_vars, exposure_vars
  )))

## HR COX Models ---- 
fit_cox_model <- function(dependent, predictor, data) {
  formula <- as.formula(paste("Surv(weeks, ", dependent, ") ~ ", predictor, 
                              "+ sex + age_group_mom + educ_group_mom + job_group_mom +",
                              "age_group_dad + educ_group_dad + job_group_dad +",
                              "factor(month_week1) + factor(year_week1) + vulnerability"))
  
  # Ajuste del modelo de Cox usando el argumento `data`
  model_fit <- coxph(formula, data = data)
  
  # Extraer resultados con tidy
  results <- broom::tidy(model_fit, exponentiate = TRUE, conf.int = TRUE, conf.level = 0.95) %>%
    mutate(estimate = round(estimate, 3), 
           std.error = round(std.error, 3),
           statistic = round(statistic, 3),
           p.value = round(p.value, 3),
           conf.low = round(conf.low, 3),
           conf.high = round(conf.high, 3)) %>%
    select(term, estimate, std.error, statistic, p.value, conf.low, conf.high) %>%
    mutate(dependent_var = dependent, predictor = predictor)  # Añadir columnas de identificación
  return(results)

  rm(model_fit); gc()
}

# Iterar sobre las combinaciones de dependientes y predictores
combinations <- expand.grid(dependent_vars, exp_vars, stringsAsFactors = FALSE)

plan(multisession, workers = parallel::detectCores() - 4)
options(future.globals.maxSize = 1.5 * 1024^3)  # 1.5 GB

tic()
results_list <- future_lapply(seq_len(nrow(combinations)), function(i) {
  dep_var <- combinations[i, 1]
  exp_var <- combinations[i, 2]
  fit_cox_model(dep_var, exp_var, data=exp_data)
})
toc() # time: 1333.067 sec elapsed, 22.21778 min 
beepr::beep(8)
plan(sequential)

# Extract results
results_cox <- bind_rows(results_list)

writexl::write_xlsx(results_cox, path =  paste0("Output/", "Models/", "Cox_models_exp", ".xlsx"))

results_cox <- rio::import(paste0("Output/", "Models/", "Cox_models_exp", ".xlsx"))

## Plots with Exposure Effects COX Models ---- 

results_filtered <- results_cox %>%
  filter(term %in% c(exp_vars), 
    dependent_var %in% dependent_vars)

plots <- list()

for (dep_var in dependent_vars) {
  # 1. Filtrar resultados para esta variable dependiente
  data_subset <- results_filtered %>%
    filter(dependent_var == dep_var) %>%
    # 2. Extraer duración y crear etiqueta si tu dataframe tiene un término similar
    #    (si no necesitas duration_label, puedes omitir todo este bloque)
    mutate(
      duration = str_extract(term, "\\d+d"),
      duration_label = case_when(
        duration == "2d" ~ "2 days",
        duration == "3d" ~ "3 days",
        duration == "4d" ~ "4 or more days",
        TRUE ~ NA_character_
      ),
      duration_label = factor(duration_label,
                              levels = c("2 days", "3 days", "4 or more days"))
    ) %>%
    # 3. Filtrar solo los términos que estén en exp_vars
    filter(term %in% exp_vars) %>%
    # 4. Factorizar term para ordenarlas y asignarles etiquetas más legibles
    mutate(
      term = factor(term, levels = exp_vars, labels = {
        # Función auxiliar inline para transformar cada nombre en algo legible:
        make_label <- function(x) {
          x %>%
            str_replace_all("^pm25", "PM25") %>%
            str_replace_all("^o3",   "O3") %>%
            str_replace_all("_krg_", " Kriging ") %>%
            str_replace_all("_idw_", " IDW ") %>%
            str_replace_all("_full", " (full)") %>%
            str_replace_all("_30",   " (30-day)") %>%
            str_replace_all("_4(?=[_10]|$)", " (4-day)") %>% 
            str_replace_all("_10",   " x 10u")
        }
        sapply(exp_vars, make_label)
      })
    )

  # 5. Definir posiciones y límites en x según dep_var (ajústalos si cambian)
  text_x_position <- 1.3
  x_limits <- c(0.5, 1.4)

  # 6. Construir el gráfico con ggplot2
  p <- ggplot(data_subset, aes(x = estimate, y = term, color = duration_label)) +
    geom_point(size = 3, shape = 15) +
    geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
    # Líneas horizontales como separadores visuales (opcional)
    geom_hline(yintercept = seq_len(length(exp_vars)) - 0.5, color = "gray", size = 0.2) +
    geom_vline(xintercept = 1, linetype = "dashed", color = "red", alpha = 0.5) +
    scale_colour_manual(
      name = "Duration HW:",
      values = c("#e59866", "#d35400", "#873600"),
      na.translate = FALSE
    ) +
    scale_x_continuous(limits = x_limits) +
    geom_text(
      aes(
        x = text_x_position,
        label = paste0(
          format(round(estimate, 3), nsmall = 2), " (",
          format(round(conf.low, 3), nsmall = 2), " - ",
          format(round(conf.high, 3), nsmall = 2), ")"
        )
      ),
      position = position_dodge(width = 0.75),
      size = 3,
      show.legend = FALSE
    ) +
    labs(
      title = paste0("Hazard Ratios para ", dep_var),
      #subtitle = "Variables de exposición (por método)",
      x = "HR (95% CI)",
      y = "Métrica de exposición"
    ) +
    theme_light() +
    theme(
      panel.grid = element_blank(),
      legend.position = "top",
      legend.text = element_text(size = 11),
      axis.text.y = element_text(size = 9)
    )

  plots[[dep_var]] <- p
}

# Save plots

#plots$birth_preterm + labs(title = "A. Preterm (<37 weeks) last week")
#plots$birth_late_preterm + labs(title = "B. Late Preterm (34-37 weeks)")
#plots$birth_moderately_preterm + labs(title = "C. Moderately Preterm (32-33 weeks)")
#plots$birth_very_preterm + labs(title = "D. Very Preterm (28-32 weeks)")

ggsave(plots$birth_preterm + labs(title = "A. Preterm (<37 weeks)"),
       filename = paste0("Output/", "Models/", "PTB_COX", ".png"), # "Preterm_trendsrm1991"
       res = 300,
       width = 20,
       height = 15,
       units = 'cm',
       scaling = 0.90,
       device = ragg::agg_png)

ggsave(plots$birth_late_preterm + labs(title = "B. Late Preterm (34-37 weeks)"),
       filename = paste0("Output/", "Models/", "PTB_late_COX", ".png"), # "Preterm_trendsrm1991"
       res = 300,
       width = 15,
       height = 15,
       units = 'cm',
       scaling = 1,
       device = ragg::agg_png)

ggsave(plots$birth_moderately_preterm + labs(title = "C. Moderately Preterm (32-33 weeks)"),
  filename = paste0("Output/", "Models/", "PTB_moderate_COX", ".png"), # "Preterm_trendsrm1991"
  res = 300,
  width = 20,
  height = 15,
  units = 'cm',
  scaling = 0.90,
  device = ragg::agg_png)

ggsave(plots$birth_very_preterm + labs(title = "D. Very Preterm (28-32 weeks)"),
  filename = paste0("Output/", "Models/", "PTB_very_COX", ".png"), # "Preterm_trendsrm1991"
  res = 300,
  width = 20,
  height = 15,
  units = 'cm',
  scaling = 0.90,
  device = ragg::agg_png)


#  Save all plots

plots_save <- ggarrange(
  plots$birth_preterm + labs(title = "A. Preterm (<37 weeks)"),
  plots$birth_late_preterm + labs(title = "B. Late Preterm (34-37 weeks)"),
  plots$birth_moderately_preterm + labs(title = "C. Moderately Preterm (32-33 weeks)"),
  plots$birth_very_preterm + labs(title = "D. Very Preterm (28-32 weeks)"),
  ncol=2, nrow=2, 
  common.legend = TRUE
)

ggsave(plots_save,
  filename = paste0("Output/", "Models/", "PTB_COX_SUP", ".png"), 
  res = 300,
  width = 40,
  height = 35,
  units = 'cm',
  scaling = 1.2,
  device = ragg::agg_png)

# Table with effects

table_models <- results_filtered %>% 
  mutate(HR=paste0(round(estimate, 3), " (", 
                   round(conf.low, 3), "; ",
                   round(conf.high, 3), ")" 
                  )) %>% 
  select(dependent_var, term, HR) %>% 
  pivot_wider(names_from = dependent_var, 
              values_from = HR) %>% 
  mutate(term = factor(term,
                        levels = c("HW_30C_2d_bin", "HW_30C_3d_bin", "HW_30C_4d_bin", 
                                    "HW_p90_2d_bin", "HW_p90_3d_bin", "HW_p90_4d_bin", 
                                    "HW_p95_2d_bin", "HW_p95_3d_bin", "HW_p95_4d_bin",
                                    "HW_p99_2d_bin", "HW_p99_3d_bin", "HW_p99_4d_bin",
                                    "HW_EHF_TAD_2d_bin", "HW_EHF_TAD_3d_bin", "HW_EHF_TAD_4d_bin"
                                  ),
                         labels = c("HW-30ºC 2D", "HW-30ºC 3D", "HW-30ºC 4D",
                                    "HW-P90 2D", "HW-P90 3D", "HW-P90 4D",
                                    "HW-P95 2D", "HW-P95 3D", "HW-P95 4D",
                                    "HW-P99 2D", "HW-P99 3D", "HW-P99 4D",
                                    "HW-EHF 2D", "HW-EHF 3D", "HW-EHF 4D")))

colnames(table_models) <- c("HR Definition", 
                            "Preterm (<37)", 
                            "Very Preterm (28-32)", 
                            "Moderate Preterm (32-33)",
                            "Late Preterm (34-37)")

writexl::write_xlsx(table_models, path =  paste0("Output/", "Models/", "Table_COX_lw", ".xlsx"))

