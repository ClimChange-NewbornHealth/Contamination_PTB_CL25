# Code 5: Descriptives ----

rm(list=ls())
## Settings ----
source("Code/0.1 Settings.R")
source("Code/0.2 Packages.R")
source("Code/0.3 Functions.R")

# Data path
data_out <- "Data/Output/"

## Contamination Data ---- 

cont_data <- rio::import(paste0(data_out, "series_pm25_o3_kriging_idw", ".RData")) 

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

com <- chilemapas::codigos_territoriales |> 
  mutate(nombre_comuna=stringr::str_to_title(nombre_comuna)) |> 
  filter(codigo_region==13) |>
  #select(1:2) |> 
  mutate(codigo_comuna=as.numeric(codigo_comuna)) |> 
  mutate(codigo_provincia=as.numeric(codigo_provincia)) |> 
  mutate(codigo_region=as.numeric(codigo_region)) |> 
  rename(name_com="nombre_comuna")

# 33 COM
com_suburb <- c(unique(com$codigo_comuna[com$nombre_provincia=="Santiago"]), 13201) # +13201
com <- com |> filter(codigo_comuna %in% c(com_suburb))
setdiff(unique(cont_data$name_com), unique(com$name_com))
setdiff(unique(com$name_com), unique(cont_data$name_com))

cont_data <- cont_data |> 
  left_join(com, by="name_com")

glimpse(cont_data)

cont_vars <- c("pm25_krg", "o3_krg", "pm25_idw", "o3_idw")

cont_data <- cont_data |> 
  filter(date <= as.Date("2020-12-31"))

### Mean contamination -----

tab1 <- cont_data |> 
  group_by(codigo_comuna, name_com) |> 
  summarise(
    mean_pm25_k = round(mean(pm25_krg),2),
    min_pm25_k = round(min(pm25_krg),2),
    max_pm25_k = round(max(pm25_krg),2),
    
    mean_o3_k = round(mean(o3_krg),2),
    min_o3_k = round(min(o3_krg),2),
    max_o3_k = round(max(o3_krg),2),

    mean_pm25_i = round(mean(pm25_idw),2),
    min_pm25_i = round(min(pm25_idw),2),
    max_pm25_i = round(max(pm25_idw),2),

    mean_o3_i = round(mean(o3_idw),2),
    min_o3_i = round(min(o3_idw),2),
    max_o3_i = round(max(o3_idw),2)
  ) |> 
  mutate(across(where(is.numeric), ~ format(., decimal.mark = ".", scientific = FALSE)))


tab1
writexl::write_xlsx(tab1, path =  paste0("Output/", "Descriptives/",  "Descriptives_pm25_o3_mun", ".xlsx"))


### Mean contamination per time -----

cont_data <- cont_data |>
  mutate(
    season = case_when(
      (month(date) == 12 & day(date) >= 21) | (month(date) %in% c(1, 2)) | (month(date) == 3 & day(date) <= 20) ~ "Summer",
      (month(date) == 3 & day(date) >= 21) | (month(date) %in% c(4, 5)) | (month(date) == 6 & day(date) <= 20) ~ "Autumn",
      (month(date) == 6 & day(date) >= 21) | (month(date) %in% c(7, 8)) | (month(date) == 9 & day(date) <= 20) ~ "Winter",
      (month(date) == 9 & day(date) >= 21) | (month(date) %in% c(10, 11)) | (month(date) == 12 & day(date) <= 20) ~ "Spring"
    ),
    season = factor(season, levels = c("Summer", "Autumn", "Winter", "Spring"))
  ) |> 
  mutate(year = year(date))

tab2 <- cont_data |> 
  group_by(codigo_comuna, name_com, season, year) |> 
  summarise(
    mean_pm25_k = round(mean(pm25_krg),2),
    min_pm25_k = round(min(pm25_krg),2),
    max_pm25_k = round(max(pm25_krg),2),
    
    mean_o3_k = round(mean(o3_krg),2),
    min_o3_k = round(min(o3_krg),2),
    max_o3_k = round(max(o3_krg),2),

    mean_pm25_i = round(mean(pm25_idw),2),
    min_pm25_i = round(min(pm25_idw),2),
    max_pm25_i = round(max(pm25_idw),2),

    mean_o3_i = round(mean(o3_idw),2),
    min_o3_i = round(min(o3_idw),2),
    max_o3_i = round(max(o3_idw),2)
  ) |> 
  mutate(across(where(is.numeric), ~ format(., decimal.mark = ".", scientific = FALSE)))


tab2
writexl::write_xlsx(tab2, path =  paste0("Output/", "Descriptives/",  "Trend_cont_mun_season_year", ".xlsx"))


tab3 <- cont_data |> 
  group_by(year, season) |> 
  summarise(
    mean_pm25_k = round(mean(pm25_krg),2),
    min_pm25_k = round(min(pm25_krg),2),
    max_pm25_k = round(max(pm25_krg),2),
    
    mean_o3_k = round(mean(o3_krg),2),
    min_o3_k = round(min(o3_krg),2),
    max_o3_k = round(max(o3_krg),2),

    mean_pm25_i = round(mean(pm25_idw),2),
    min_pm25_i = round(min(pm25_idw),2),
    max_pm25_i = round(max(pm25_idw),2),

    mean_o3_i = round(mean(o3_idw),2),
    min_o3_i = round(min(o3_idw),2),
    max_o3_i = round(max(o3_idw),2)
  ) |> 
  mutate(across(where(is.numeric), ~ format(., decimal.mark = ".", scientific = FALSE))) 


tab3
writexl::write_xlsx(tab3, path =  paste0("Output/", "Descriptives/",  "Trend_cont_year_season", ".xlsx"))


### Distribution contamination ----

# PM2.5
data_pm <- cont_data |>
  select(pm25_krg, pm25_idw) |>
  pivot_longer(cols = everything(), names_to = "method", values_to = "value") |>
  mutate(method = recode(method,
                         "pm25_krg" = "Kriging",
                         "pm25_idw" = "IDW"),
         contaminant = "PM2.5")

# O3
data_o3 <- cont_data |>
  select(o3_krg, o3_idw) |>
  pivot_longer(cols = everything(), names_to = "method", values_to = "value") |>
  mutate(method = recode(method,
                         "o3_krg" = "Kriging",
                         "o3_idw" = "IDW"),
         contaminant = "O3")

# Combinar
data_dens <- bind_rows(data_pm, data_o3)

plot_density_panel <- function(data, contaminant, xlim_range = NULL) {
  data_sub <- filter(data, contaminant == contaminant)
  
  ggplot(data_sub, aes(x = value, fill = method, color = method)) +
    geom_density(alpha = 0.3, linewidth = 0.8) +
    scale_fill_manual(values = c("Kriging" = "#1f77b4", "IDW" = "#ff7f0e")) +
    scale_color_manual(values = c("Kriging" = "#1f77b4", "IDW" = "#ff7f0e")) +
    labs(x = contaminant, y = "Density") +
    theme_light() +
    theme(
      panel.grid = element_blank(),
      legend.position = "top",
      legend.title = element_blank(),
      plot.title = element_text(size = 11, hjust = 0)
    ) +
    coord_cartesian(xlim = xlim_range)
}

g1 <- plot_density_panel(data_dens, contaminant = "PM2.5", xlim_range = c(0, 100)) +
  labs(title = "A. PM2.5")

g2 <- plot_density_panel(data_dens, contaminant = "O3", xlim_range = c(0, 100)) +
  labs(title = "B. O3")


plt1 <- ggarrange(g1, g2, ncol = 2, common.legend = TRUE)
plt1

ggsave(
  filename = "Output/Descriptives/Distribution_krg_idw_pm.png",
  #plot     = last_plot(),
  res      = 300,
  width    = 20,
  height   = 10,
  units    = 'cm',
  scaling  = 0.9,
  device   = ragg::agg_png
)

## Scatter-plots ---- 

make_cor_label <- function(x, y, data) {
  test <- cor.test(data[[x]], data[[y]], use = "complete.obs")
  r <- round(test$estimate, 2)
  p <- format.pval(test$p.value, digits = 3, eps = .001)
  paste0("r = ", r, ", p = ", p)
}

plot_corr_panel <- function(data, x, y, xlab, ylab) {
  label_txt <- make_cor_label(x, y, data)
  
  ggplot(data, aes_string(x = x, y = y)) +
    stat_bin2d(bins = 100) +
    scale_fill_gradient(low = "#fee8c8", high = "#e34a33", name = "Count") +
    geom_smooth(method = "lm", formula = y ~ x, color = "#08519c", alpha = 0.3, linewidth = 1) +
    annotate("text", x = Inf, y = Inf, label = label_txt, hjust = 1.1, vjust = 1.5, size = 4) +
    labs(x = xlab, y = ylab) +
    theme_light() +
    theme(
      panel.grid = element_blank(),
      plot.title = element_text(size = 11, hjust = 0)
    )
}

g3 <- plot_corr_panel(
  data = cont_data,
  x    = "pm25_krg",
  y    = "pm25_idw",
  xlab = "PM2.5 Kriging",
  ylab = "PM2.5 IDW"
) +
  scale_x_continuous(limits =c(0, 100)) +
  scale_y_continuous(limits =c(0, 100)) +
  labs(title = "A. PM2.5: Kriging vs IDW") +
  theme(legend.position = "right")

g4 <- plot_corr_panel(
  data = cont_data,
  x    = "o3_krg",
  y    = "o3_idw",
  xlab = "O3 Kriging",
  ylab = "O3 IDW"
) +
  scale_x_continuous(limits =c(0, 35)) +
  scale_y_continuous(limits =c(0, 35)) +
  labs(title = "B. O3: (Kriging) vs IDW") +
  theme(legend.position = "right")

plt2 <- ggarrange(g3, g4, ncol = 2, common.legend = TRUE, legend = "top")
plt2 

ggsave(
  filename = "Output/Descriptives/Correlation_krg_idw_pm_o3.png",
  #plot     = last_plot(),
  res      = 300,
  width    = 20,
  height   = 10,
  units    = 'cm',
  scaling  = 0.9,
  device   = ragg::agg_png
)

### Complete plot -----

ggarrange(plt1, plt2, ncol = 1)
 
ggsave(
  filename = "Output/Descriptives/Descriptive_krg_idw_pm_o3.png",
  #plot     = last_plot(),
  res      = 300,
  width    = 20,
  height   = 20,
  units    = 'cm',
  scaling  = 0.9,
  device   = ragg::agg_png
)

### Time plots -----

cont_data_mean <- cont_data |>
  group_by(date) |>
  summarise(
    pm25_krg  = mean(pm25_krg, na.rm = TRUE),
    pm25_idw  = mean(pm25_idw, na.rm = TRUE),
    o3_krg    = mean(o3_krg, na.rm = TRUE),
    o3_idw    = mean(o3_idw, na.rm = TRUE),
  ) |> 
  ungroup()

# Lista de contaminantes y etiquetas
gvars <- list(
  list(var = "pm25_krg", title = "A. PM2.5 Kriging"),
  list(var = "pm25_idw", title = "B. PM2.5 IDW"),
  list(var = "o3_krg",   title = "C. O3 Kriging"),
  list(var = "o3_idw",   title = "D. O3 IDW")
)

# Lista para guardar los gráficos
plots <- list()

# Loop para crear cada gráfico
for (v in gvars) {
  p <- ggplot(cont_data_mean, aes_string(x = "date", y = v$var)) +
    geom_point(size = 0.5, alpha = 0.1) +
    geom_smooth(method = "loess", span=0.05, se = TRUE, linewidth = 0.6) +
    labs(
      title = v$title,
      x = NULL, 
      y = "Concentration (10μg/m3)"
    ) +
    scale_x_date(
      date_breaks = "1 year",
      date_labels = "%Y"
    ) +
    theme_light() +
    theme(
      plot.title      = element_text(),
      panel.grid      = element_blank(),
      strip.background = element_rect(fill = "white"),
      strip.text      = element_text(size = 11, color = "black", hjust = 0),
      axis.text.y     = element_text(size = 9),
      axis.ticks.y    = element_blank(),
      axis.text.x     = element_text(angle = 45, hjust = 1, size = 8)
    )
  
  plots[[v$title]] <- p
}

# Unir con ggarrange (2x2)
fig_final <- ggarrange(
  plots[[1]], plots[[2]], plots[[3]], plots[[4]],
  ncol = 2, nrow = 2,
  align = "hv"
)

ggsave(
  fig_final, 
  filename = "Output/Descriptives/Time_distribution_pm25_o3.png",
  #plot     = last_plot(),
  res      = 300,
  width    = 20,
  height   = 15,
  units    = 'cm',
  scaling  = 0.9,
  device   = ragg::agg_png
)

### Space time plots -----

comunas_sf <- chilemapas::mapa_comunas |> 
  mutate(codigo_comuna = as.numeric(codigo_comuna)) |> 
  filter(codigo_comuna %in% c(com_suburb)) |> 
  mutate(id = 1:n()) %>% 
  mutate(id_mun = str_pad(as.integer(factor(id)), width = 2, pad = "0")) |> 
  select(codigo_comuna, geometry, id_mun) 

cont_map <- cont_data |>
  filter(season %in% c("Winter", "Summer")) |> # year %in% c(2010, 2015, 2020), 
  group_by(name_com, codigo_comuna, year, season) |>
  summarise(
    pm25_krg = mean(pm25_krg, na.rm = TRUE),
    pm25_idw = mean(pm25_idw, na.rm = TRUE),
    o3_krg   = mean(o3_krg, na.rm = TRUE),
    o3_idw   = mean(o3_idw, na.rm = TRUE),
    .groups = "drop"
  ) |> 
  left_join(comunas_sf, by="codigo_comuna")

glimpse(cont_map)

plot_pollutant_map <- function(data, var, title, i, e) {
  g1 <- data |> 
  mutate(
    year_group = case_when(
      year <= 2015 ~ "2010-2015",
      year > 2015 ~ "2016-2020"),
    year = as.factor(year) 
  ) |> 
  filter(year %in% c(i:e)) |> 
  ggplot( ) +
    geom_sf(aes(fill = .data[[var]], geometry = geometry), color = "white", size = 0.1) +
    #geom_sf_text(aes(label = id_mun, geometry = geometry), size = 2, fontface = "bold", color = "black", stat = "sf_coordinates") +
    facet_grid(season ~ year) +
    #ggh4x::facet_nested(
    #  cols = vars(year_group, year),
    #  rows = vars(season),
    #  switch = "y") +
    scale_fill_fermenter(
      palette = "YlOrRd", direction = 1, 
      n.breaks = 8, limits = c(0, 50),
      name = expression(paste(PM[2.5], " (10", mu, "g/", m^3, ")")),
      guide = guide_colorbar(
        barwidth = 10, barheight = 0.5, 
        direction = "horizontal", 
        title.position = "left",
        reverse = FALSE,
        label.theme = element_text(angle = 0)
      )
    ) +
    labs(title = title, x = NULL, y = NULL) +
    theme_light() +
    theme(
      legend.position = "top",
      legend.key.size = unit(0.6, "cm"), 
      legend.text = element_text(size = 8), 
      legend.title = element_text(size = 8, face = "bold"), 
      plot.title = element_text(size = 10, hjust = 0),
      strip.background = element_rect(fill = NA, color = "black"),
      strip.text = element_text(color = "black"),
      strip.text.y = element_text(angle = 0),
      panel.grid = element_blank()
    )
}

m1 <- plot_pollutant_map(cont_map, "pm25_idw", NULL, i=2010, e=2020)

m1

ggsave(
  filename = "Output/Descriptives/Space_PM25_IDW.png",
  #plot     = last_plot(),
  res      = 300,
  width    = 50,
  height   = 30,
  units    = 'cm',
  scaling  = 0.9,
  device   = ragg::agg_png
)


m2 <- plot_pollutant_map(cont_map, "pm25_krg", NULL, i=2010, e=2020)

m2


ggsave(
  filename = "Output/Descriptives/Space_PM25_KRG.png",
  #plot     = last_plot(),
  res      = 300,
  width    = 50,
  height   = 30,
  units    = 'cm',
  scaling  = 0.9,
  device   = ragg::agg_png
)


plot_pollutant_map_o3 <- function(data, var, title) {
  ggplot(data) +
    geom_sf(aes(fill = .data[[var]], geometry=geometry), color = "white", size = 0.1) +
    #geom_sf_text(aes(label = id_mun, geometry = geometry), size = 2, fontface = "bold", color = "black", stat = "sf_coordinates") +
    facet_grid(season ~ year) +
    scale_fill_fermenter(
      palette = "YlOrRd", direction = 1, 
      n.breaks = 8, limits = c(0, 20),
      name = expression(paste(O[3], "(ppb)")),
      guide = guide_colorbar(expression(paste(O[3], " (10", mu, "g/", m^3, ")")),
                           barwidth = 10, barheight = 0.5, 
                           direction = "horizontal", 
                           title.position = "left",
                           reverse = FALSE,
                           label.theme = element_text(angle = 0)) 
    ) +
    labs(title = title, x=NULL, y=NULL) +
    theme_light() +
    theme(
      legend.position = "top",
      legend.key.size = unit(0.6, "cm"), 
      legend.text = element_text(size = 8), 
      legend.title = element_text(size = 8, face = "bold"), 
      plot.title = element_text(size = 10, hjust = 0),
      strip.background = element_rect(fill = NA, color = "black"),
      strip.text = element_text(color = "black"),
      strip.text.y = element_text(angle = 0),
      panel.grid = element_blank()
    )
}


m3 <- plot_pollutant_map_o3(cont_map, "o3_krg", NULL)

m3

ggsave(
  filename = "Output/Descriptives/Space_O3_KRG.png",
  #plot     = last_plot(),
  res      = 300,
  width    = 50,
  height   = 30,
  units    = 'cm',
  scaling  = 0.9,
  device   = ragg::agg_png
)

m4 <- plot_pollutant_map_o3(cont_map, "o3_idw", NULL)

m4

ggsave(
  filename = "Output/Descriptives/Space_O3_IDW.png",
  #plot     = last_plot(),
  res      = 300,
  width    = 50,
  height   = 30,
  units    = 'cm',
  scaling  = 0.9,
  device   = ragg::agg_png
)

# Tab map id 

id_mun <- cont_map |>
  group_by(id_mun, name_com) |> 
  summarise(n=n()) |> 
  dplyr::select(-n)

id_mun_text <- c(paste(id_mun$id_mun, "-", id_mun$name_com), "", "", "")
id_mun_matrix <- matrix(id_mun_text, ncol = 4, byrow = FALSE)

text_mun <- tableGrob(id_mun_matrix, theme = ttheme_minimal(
  core = list(fg_params = list(cex = 0.5, hjust = 0, x = 0),
              bg_params = list(fill = "white", col = "white") 
) ,
  padding = unit(c(0.2, 0.2, 0.2, 0.2), "cm") 
))

text_municipality <- textGrob("Municipality:\n", gp = gpar(fontsize = 8, fontface="bold"), hjust = -0.4, x = 0, y = -6)

tab <- ggarrange(
  text_municipality, text_mun,  
  ncol = 1, heights = c(0.02, 1) 
)

tab

ggsave(
  filename = paste0("Output/Descriptives/Stgo_tab.png"), 
  res = 300,
  width = 10,
  height = 6 ,
  units = 'cm',
  scaling = 1,
  device = ragg::agg_png) 


## Map Complete manuscript

cont_map <- cont_data |>
  filter(season %in% c("Winter", "Summer")) |>
  group_by(name_com, codigo_comuna, season) |>
  summarise(
    pm25_krg = mean(pm25_krg, na.rm = TRUE),
    pm25_idw = mean(pm25_idw, na.rm = TRUE),
    o3_krg   = mean(o3_krg, na.rm = TRUE),
    o3_idw   = mean(o3_idw, na.rm = TRUE),
    .groups = "drop"
  ) |> 
  left_join(comunas_sf, by="codigo_comuna")

glimpse(cont_map)

plot_pollutant_map_pm25 <- function(data, var, title) {
  ggplot(data) +
    geom_sf(aes(fill = .data[[var]], geometry = geometry), color = "white", size = 0.1) +
    geom_sf_text(aes(label = id_mun, geometry = geometry), size = 2, fontface = "bold", color = "black", stat = "sf_coordinates") +
    facet_wrap(~ season, ncol = 2) +
    scale_fill_fermenter(
      palette = "YlOrRd", direction = 1, 
      n.breaks = 8, limits = c(0, 4),
      name = expression(paste(PM[2.5], " (10", mu, "g/", m^3, ")")),
      guide = guide_colorbar(
        barwidth = 10, barheight = 0.5, 
        direction = "horizontal", title.position = "left",
        label.theme = element_text(angle = 0)
      )
    ) +
    labs(title = title, x = NULL, y = NULL) +
    theme_light() +
    theme(
      legend.position = "top",
      legend.key.size = unit(0.6, "cm"),
      legend.text = element_text(size = 8),
      legend.title = element_text(size = 8, face = "bold"),
      plot.title = element_text(size = 10, hjust = 0),
      strip.background = element_rect(fill = NA, color = "black"),
      strip.text = element_text(color = "black"),
      panel.grid = element_blank()
    )
}

plot_pollutant_map_o3 <- function(data, var, title) {
  ggplot(data) +
    geom_sf(aes(fill = .data[[var]], geometry = geometry), color = "white", size = 0.1) +
    geom_sf_text(aes(label = id_mun, geometry = geometry), size = 2, fontface = "bold", color = "black", stat = "sf_coordinates") +
    facet_wrap(~ season, ncol = 2) +
    scale_fill_fermenter(
      palette = "PuBuGn", direction = 1, 
      n.breaks = 8, limits = c(0, 2),
      name = expression(paste(O[3], " (10", mu, "g/", m^3, ")")),
      guide = guide_colorbar(
        barwidth = 10, barheight = 0.5, 
        direction = "horizontal", title.position = "left",
        label.theme = element_text(angle = 0)
      )
    ) +
    labs(title = title, x = NULL, y = NULL) +
    theme_light() +
    theme(
      legend.position = "top",
      legend.key.size = unit(0.6, "cm"),
      legend.text = element_text(size = 8),
      legend.title = element_text(size = 8, face = "bold"),
      plot.title = element_text(size = 10, hjust = 0),
      strip.background = element_rect(fill = NA, color = "black"),
      strip.text = element_text(color = "black"),
      panel.grid = element_blank()
    )
}


pm25_map <- plot_pollutant_map_pm25(cont_map, "pm25_krg", "A. PM2.5 - Kriging")
pm25_map_idw <- plot_pollutant_map_pm25(cont_map, "pm25_idw", "B. PM2.5 - IDW")

o3_map <- plot_pollutant_map_o3(cont_map, "o3_krg", "C. O₃ - Kriging")
o3_map_idw <- plot_pollutant_map_o3(cont_map, "o3_idw", "D. O₃ - IDW")

(pm25_map | pm25_map_idw) /
(o3_map | o3_map_idw)


ggarrange(pm25_map, pm25_map_idw,
          o3_map, o3_map_idw, 
          ncol = 2, nrow = 2, common.legend = FALSE)

ggsave(
  filename = paste0("Output/Descriptives/PM25_O3_SW.png"), 
  res = 300,
  width = 25,
  height = 20 ,
  units = 'cm',
  scaling = 1,
  device = ragg::agg_png) 

# Other formato plot

plots_list <- list()

for (s in c("Summer", "Winter")) {
  for (y in c(2010, 2015, 2020)) {
    df_sub <- cont_map |> filter(season == s, year == y)
    
    p <- ggplot(df_sub) +
      geom_sf(aes(fill = pm25_krg, geometry=geometry), color = "white", size = 0.1) +
      scale_fill_distiller(palette = "plasma", direction = 1,
                           name = expression(paste(PM[2.5], " (10", mu, "g/", m^3, ")"))) +
      labs(title = paste0(s, " ", y)) +
      theme_light() +
      theme(
      legend.position = "top",
      legend.key.size = unit(0.6, "cm"), 
      legend.text = element_text(size = 6), 
      legend.title = element_text(size = 8, face = "bold"), 
      plot.title = element_text(size = 10, hjust = 0),
      strip.background = element_rect(fill = NA, color = "black"),
      strip.text = element_text(color = "black"),
      panel.grid = element_blank()
    )
    
    
    plots_list[[paste0(s, "_", y)]] <- p
  }
}

# Unir todos los mapas (6)
ggarrange(plotlist = plots_list, nrow = 2, ncol = 3)

ggsave(
  filename = "Output/Descriptives/Space_PM25_test.png",
  #plot     = last_plot(),
  res      = 300,
  width    = 25,
  height   = 20,
  units    = 'cm',
  scaling  = 0.9,
  device   = ragg::agg_png
)

