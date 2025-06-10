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
file <- "births_1992_2020.RData"

# Open data in R
load(paste0(data_out, file)) 

glimpse(births)

### 1. Fixed cohort bias  -----

# Floor 
table(births$year_week1) 

# births_rm1991 <- births %>% 
#   filter(year_week1>=1992) # Loss 77980 Introduce bias 

length(unique(births$id)) # 2,988,646,213
# Ceiling date_ends_week_gest 2020-12-31 

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
  scale_x_continuous(breaks = seq(1992, 2020, by=4)) +
  labs(y ="Prevalence (per 1.000)", x=NULL) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))

ggsave(filename = paste0("Output/", "Descriptives/", "Preterm_trends", ".png"), # "Preterm_trendsrm1991"
       res = 300,
       width = 20,
       height = 12,
       units = 'cm',
       scaling = 0.90,
       device = ragg::agg_png)

# Podrían haber nacido en el úlimo mes 30/04, pero no última semana -> fecha de cohorte 07/04. Todo el resto fuera. 
#rm(births_rm1991)

### Save new births data ----
glimpse(births)

births_1020 <- births |> 
  filter(
    date_start_week_gest >= as.Date("2010-01-01") &
    date_ends_week_gest   <= as.Date("2020-12-31")
  )

glimpse(births_1020)
summary(births_1020$date_start_week_gest)
summary(births_1020$date_ends_week_gest)

save(births_1020, file=paste0(data_out, "births_2010_2020", ".RData"))
