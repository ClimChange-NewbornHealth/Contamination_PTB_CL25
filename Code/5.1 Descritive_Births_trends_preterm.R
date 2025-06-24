# Code 1.1: Birth exploratorion and preparation ----
rm(list=ls())
## Settings ----
source("Code/0.1 Settings.R")
source("Code/0.2 Functions.R")
source("Code/0.3 Packages.R")

# Data path 
data_inp <- "Data/Input/Nacimientos/"
data_out <- "Data/Output/"

## Birth data ---- 

# ID file load
file <- "series_births_exposition_pm25_o3_kriging_idw.RData"

# Open data in R
load(paste0(data_out, file)) 

glimpse(bw_data_join) # 713918

births <- bw_data_join
rm(bw_data_join)

### 1. Fixed cohort bias  -----

weeks <- births %>% 
  group_by(date_start_week_gest, date_ends_week_gest, weeks) %>% 
  summarise(n_gestantes=n(),
            min_semana_gestacion=min(weeks),
            max_semana_gestacion=max(weeks), 
            ultimo_nacimiento=max(date_nac) 
          )

write.xlsx(weeks, "Output/Descriptives/Start_ends_gestational_weeks.xlsx")

# Figure trends preterms

table <- births %>% 
  group_by(year_nac) %>% 
  summarise(
    tasa_vpt=mean(birth_very_preterm, na.rm=TRUE)*1000,
    tasa_mpt=mean(birth_moderately_preterm, na.rm=TRUE)*1000,
    tasa_lpt=mean(birth_late_preterm, na.rm=TRUE)*1000,
    tasa_pt=mean(birth_preterm, na.rm=TRUE)*1000,
    tasa_t=mean(birth_term, na.rm=TRUE)*1000,
    tasa_post=mean(birth_posterm, na.rm=TRUE)*1000,
  ) %>% 
  pivot_longer(
    cols=!year_nac, 
    names_to="preterm",
    values_to="prev"
  ) %>% 
  mutate(preterm=case_when(
    preterm=="tasa_vpt" ~ "Very Preterm",
    preterm=="tasa_mpt" ~ "Moderately Preterm",
    preterm=="tasa_lpt" ~ "Late Preterm",
    preterm=="tasa_pt" ~ "Preterm",
    preterm=="tasa_t" ~  "Term",
    preterm=="tasa_post" ~ "Post-term"
  )) %>% 
  mutate(preterm=factor(preterm, levels=c(
    "Very Preterm",
    "Moderately Preterm",
    "Late Preterm",
    "Preterm",
    "Term",
    "Post-term"
  )))

table %>% 
  ggplot(aes(y=prev, x=year_nac)) +
  geom_line(color="#08519c") +
  geom_point(color="#08519c") +
  facet_wrap(~preterm, ncol = 2, scales = "free") +
  scale_x_continuous(breaks = seq(2010, 2020, by=1)) +
  labs(y ="Prevalence (per 1.000)", x=NULL) +
  theme_light() +
  theme(
    plot.title = element_text(hjust = 0.5),
    panel.grid       = element_blank(),
    strip.background = element_rect(fill = "white"),
    strip.text       = element_text(size = 11, color = "black", hjust=0),
    axis.text.y      = element_text(size = 9),
    axis.ticks.y     = element_blank()
  )

ggsave(filename = paste0("Output/", "Descriptives/", "Preterm_trends_2010_2020", ".png"), # "Preterm_trendsrm1991"
       res = 300,
       width = 20,
       height = 12,
       units = 'cm',
       scaling = 0.90,
       device = ragg::agg_png)


