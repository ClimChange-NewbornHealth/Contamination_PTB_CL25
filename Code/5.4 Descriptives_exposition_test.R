## Settings ----
source("Code/0.1 Settings.R")
source("Code/0.2 Functions.R")
source("Code/0.3 Packages.R")

# Data path 
data_inp <- "Data/Input/"
data_out <- "Data/Output/"

## Exposition Data ---- 

exp_data <- rio::import(paste0(data_out, "series_births_exposition_pm25_o3_kriging_idw", ".RData")) %>% drop_na()
glimpse(exp_data)

exp_data <- exp_data |> 
  filter(birth_preterm==1)

# Variables mean (/10)
mean_vars <- grep("(_full_10$|_30_10$|_4_10$|_t[123]_10$)", names(exp_data), value = TRUE) %>%
  setdiff(grep("_iqr_10$", names(exp_data), value = TRUE))

# Variables IQR
iqr_vars  <- grep("_iqr_10$", names(exp_data), value = TRUE)

# Función que pivotea y etiqueta
make_long <- function(vars, is_iqr) {
  exp_data %>%
    select(all_of(vars)) %>%
    pivot_longer(everything(), names_to="var", values_to="exposure") %>%
    mutate(
      metric    = if (is_iqr) "IQR" else "/10",
      pollutant = case_when(
        str_starts(var, "pm25") ~ "PM2.5",
        str_starts(var, "o3")   ~ "Ozone"
      ),
      method = if_else(str_detect(var, "_krg_"), "Kriging", "IDW"),
      window = case_when(
        str_detect(var, "_full")  ~ "Full",
        str_detect(var, "_30")    ~ "30-day",
        str_detect(var, "_4")     ~ "4-day",
        str_detect(var, "_t[123]_10$") ~ "Trimesters",
        str_detect(var, "_t[123]_iqr_10$") ~ "Trimesters",
      ) %>% factor(levels=c("4-day","30-day","Trimesters","Full")),
      trimester = case_when(
        str_detect(var, "_t1_10$") ~ "T1",
        str_detect(var, "_t2_10$") ~ "T2",
        str_detect(var, "_t3_10$") ~ "T3",
        str_detect(var, "_t1_iqr_10$") ~ "T1",
        str_detect(var, "_t2_iqr_10$") ~ "T2",
        str_detect(var, "_t3_iqr_10$") ~ "T3",
        TRUE                    ~ NA_character_
      )
    )
}

exp_mean  <- make_long(mean_vars, FALSE)
exp_iqr   <- make_long(iqr_vars,  TRUE)

# 2) Función que crea un solo panel de densidades para un df filtrado
# -------------------------------------------------------------------

make_panel <- function(df, pollutant_name, metric_label) {
  dfp <- df %>% filter(pollutant == pollutant_name)
  
  ggplot() +
    # Curvas para 4-day, 30-day, Full (rellenas)
    geom_density(
      data = dfp %>% filter(window %in% c("4-day","30-day","Full")),
      aes(x=exposure),
      fill = if (pollutant_name=="PM2.5") "#D95F02" else "#1B9E77",
      alpha = 0.4
    ) +
    # Curvas Trimesters (T1,T2,T3) superpuestas
    geom_density(
      data = dfp %>% filter(window=="Trimesters"),
      aes(x=exposure, color=trimester, fill=trimester),
      alpha = 0.3, size = 0.5
    ) +
    facet_grid(window ~ method, scales="free") +
    scale_color_brewer("Trimester", palette="Set1") +
    scale_fill_brewer("Trimester",  palette="Set1") +
    labs(
      title = paste0(pollutant_name, " ", metric_label),
      x     = expression("Exposure (10"*mu*"g/"*m^3*")"),
      y     = "Density"
    ) +
    theme_light() +
    theme(
      strip.background = element_rect(fill="white", color="grey80"),
      strip.text       = element_text(face="bold", color = "black"),
      strip.text.y = element_text(angle = 0),
      panel.grid       = element_blank(),
      legend.position  = if (pollutant_name=="PM2.5") "top" else "top"
    )
}

# 3) Construir los 4 paneles
# --------------------------
p1 <- make_panel(exp_mean, "PM2.5", "[Mean]")   # Plot 1
p2 <- make_panel(exp_mean, "Ozone", "[Mean]")   # Plot 2

p3 <- make_panel(exp_iqr,  "PM2.5", "[IQR]")          # Plot 3
p4 <- make_panel(exp_iqr,  "Ozone", "[IQR]")          # Plot 4


ggsave(
  filename = "Output/Descriptives/Dist_exp_pm25_pt.png",
  plot     = p1,
  res      = 300,
  width    = 25,
  height   = 20,
  units    = 'cm',
  scaling  = 0.9,
  device   = ragg::agg_png
)

ggsave(
  filename = "Output/Descriptives/Dist_exp_o3_pt.png",
  plot     = p2,
  res      = 300,
  width    = 25,
  height   = 20,
  units    = 'cm',
  scaling  = 0.9,
  device   = ragg::agg_png
)

ggsave(
  filename = "Output/Descriptives/Dist_exp_pm25_iqr_pt.png",
  plot     = p3,
  res      = 300,
  width    = 25,
  height   = 20,
  units    = 'cm',
  scaling  = 0.9,
  device   = ragg::agg_png
)

ggsave(
  filename = "Output/Descriptives/Dist_exp_o3_iqr_pt.png",
  plot     = p4,
  res      = 300,
  width    = 25,
  height   = 20,
  units    = 'cm',
  scaling  = 0.9,
  device   = ragg::agg_png
)
