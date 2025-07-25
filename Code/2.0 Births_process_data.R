# Code 1: Birth data preparation ----
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
file <- "data_1992_2020.RData"
file2 <- "nacim2019.xlsx"
file3 <- "nacim2020.xlsx"

# Open data in R
load(paste0(data_inp, file)) 

births <- data_1992_2020 |> janitor::clean_names()

rm(data_1992_2020)

births19 <- rio::import(paste0(data_inp, file2), sheet="R") |> janitor::clean_names() 
births20 <- rio::import(paste0(data_inp, file3), sheet="R") |> janitor::clean_names()

# Explorer data 
glimpse(births) # 7,084,698 obs 
glimpse(births19) # 194,978 obs 
glimpse(births20) # 210,188 obs 

# nrow(births) + nrow(births19) + nrow(births20) # 7,489,864

# Prepare data
### 1. Date 1992-2020 -----
table(births$ano_nac)
table(births$region)
length(unique(births$comuna[births$region==13]))

table(births19$ano_nac)
table(births19$region)
table(births19$comuna)
length(unique(births19$comuna[births19$comuna>=13000 & births19$comuna<14000]))

table(births20$ano_nac)
table(births20$region)
table(births20$comuna)
length(unique(births20$comuna[births20$comuna>=13000 & births20$comuna<14000]))

nrow(filter(births, ano_nac %in% 2010:2018)) + nrow(births19) + nrow(births20) # 2557140
  
births <- births |> filter(comuna>=13000 & comuna<14000) # 2,724,086
births19 <- births19 |> filter(comuna>=13000 & comuna<14000) |> mutate(region=13) # 89,770
births20 <- births20 |> filter(comuna>=13000 & comuna<14000) |> mutate(region=13) # 81,351

length(unique(births$comuna))
length(unique(births19$comuna))
length(unique(births20$comuna))

births <- births |> filter(!ano_nac %in% c(2019, 2020))

nrow(births) + nrow(births19) + nrow(births20) # 2,895,207

births <- births |>
  bind_rows(births19) |> 
    bind_rows(births20)

table(births$ano_nac)
glimpse(births)
rm(births19)
rm(births20)

### 2. Construction births date ----

births <- births |> 
  mutate(date_nac = make_date(year = ano_nac, month = mes_nac, day = dia_nac))  # Year, Month, Date 

glimpse(births)
table(births$region, births$ano_nac, useNA = "ifany")
summary(births$date_nac)

# Filter time 2010 - 2020
unique(births$ano_nac)

births <- births |> 
  filter(ano_nac %in% c(2010:2020)) # 1060476

nrow(births)

# Adjust BW with urban Santiago
# Adjust BW with urban Santiago
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

births <- births |> filter(comuna %in% com_suburb) # 859626
births <- births |> 
  left_join(com, by=c("comuna"="codigo_comuna"))


### 3. Create week_gest obs ----

start_count <- nrow(births)

births <- births |>
  mutate(tbw=if_else(peso==9999, NA_real_, peso),
        weeks=if_else(semanas==99, NA_real_, semanas)) |> 
  drop_na(date_nac) %>% 
  {
    cat("Observaciones después de drop_na(date_nac):", nrow(.), "\nMissing:", start_count - nrow(.), "\n") # NA: 0
    start_count <<- nrow(.)  # Actualiza el contador de filas
    .  # Retorna el dataset para la siguiente operación
  } 

births <- births |>     
  drop_na(weeks)  %>% 
  {
    cat("Observaciones después de drop_na(semanas):", nrow(.), "\nMissing:", start_count - nrow(.), "\n") # NA 688
    start_count <<- nrow(.)  # Actualiza el contador de filas
    .  # Retorna el dataset para la siguiente operación
  } 

births <- births |>
  drop_na(comuna)  %>%  
  {
    cat("Observaciones después de drop_na(comuna):", nrow(.), "\nMissing:", start_count - nrow(.), "\n") # NA 0
    start_count <<- nrow(.)  # Actualiza el contador de filas
    .  # Retorna el dataset para la siguiente operación
  } 

nrow(births) # 858938

births <- births |>     
  mutate(id=1:n()) |> 
  mutate(date_start = date_nac - weeks(semanas-1),
  date_end = date_nac) 
  
# Check results

t1 <- births |>
  group_by(weeks) |> 
  summarise(min_tbw=min(tbw, na.rm = TRUE), 
            mean_tbw=mean(tbw, na.rm = TRUE), 
            median_tbw=median(tbw, na.rm = TRUE), 
            max_tbw=max(tbw, na.rm = TRUE),  
            n_births=n())

write.xlsx(t1, "Data/Output/Data_weeks_tbw.xlsx")

### 4. Prepare births variables ----

# 858,938
# Adjust missing values variables 
births <- births |>
  mutate(
    size=if_else(talla==99, NA_real_, talla),
    age_mom=if_else(edad_madre==99, NA_real_, edad_madre),
    educ_mom=if_else(nivel_madre==9, NA_real_, nivel_madre),
    job_mom=if_else(activ_madre %in% c(9), NA_real_, activ_madre+1),
    age_dad=if_else(edad_padre==99, NA_real_, edad_padre),
    educ_dad=if_else(nivel_padre==9, NA_real_, nivel_padre),
    job_dad=if_else(activ_padre %in% c(3,9), NA_real_, activ_padre+1)
    )

summary(births)

# Apply filters
start_count <- nrow(births)

births <- births |>
  filter(age_mom>=12 & age_mom<=50)  %>% 
      {
        cat("Observaciones después de filter:", nrow(.), "\nMissing:", start_count - nrow(.), "\n") # NA: 403
        start_count <<- nrow(.)  # Actualiza el contador de filas
        .  # Retorna el dataset para la siguiente operación
      } 

births <- births |>
  filter(weeks >= 28)  %>% 
    {
      cat("Observaciones después de filter:", nrow(.), "\nMissing:", start_count - nrow(.), "\n") # NA: 4127
      start_count <<- nrow(.)  # Actualiza el contador de filas
      .  # Retorna el dataset para la siguiente operación
    } 

births <- births |>
  filter(tipo_parto==1)  %>%  
    {
      cat("Observaciones después de filter", nrow(.), "\nMissing:", start_count - nrow(.), "\n") # NA: 19321
      start_count <<- nrow(.)  # Actualiza el contador de filas
      .  # Retorna el dataset para la siguiente operación
    } 

# Edit covariates
births <- births |>
  mutate(sex=factor(sexo, levels=c(1,2), labels=c("Boy", "Girl"))) |>
  mutate(date_week1 = date_start, 
         year_week1 = year(date_start), 
         month_week1 = month(date_start),
         ym_week1 = as.yearmon(date_start, "%M-%Y")) |>
  rename(day_nac=dia_nac,
         month_nac=mes_nac,
         year_nac=ano_nac,
         com=comuna, 
         #name_com=nombre_comuna, 
         reg=codigo_region,
         name_reg=nombre_region, 
         date_start_week_gest=date_start,
         date_ends_week_gest=date_end
         ) |> 
  mutate(
    age_group_mom=case_when(
      age_mom <= 20 ~ 1, 
      age_mom > 20 & age_mom <= 29 ~ 2,
      age_mom >= 30 & age_mom <= 39 ~ 3,
      age_mom >= 40 ~ 4, 
      TRUE ~ NA
    ),
    age_group_mom=factor(age_group_mom, 
                         levels=c(1:4), 
                         labels=c("<=20", "20-29", "30-39", ">=40")),
    educ_group_mom = case_when(
      educ_mom == 1 ~ 3, # College
      educ_mom == 2 ~ 2, # Secondary
      educ_mom == 3 ~ 2, # Secondary
      educ_mom == 4 ~ 1, # Primary
      educ_mom == 5 ~ 1, # No educaction 
      TRUE ~ NA_real_, #Unknow
    ), 
    educ_group_mom = factor(educ_group_mom, 
                            levels = c(1:3), 
                            labels = c("Primary", "Secondary", "College")),
    #job_group_mom = if_else(is.na(job_mom), 4, job_mom), 
    job_group_mom = if_else(job_mom==3, 1, job_mom),
    job_group_mom = factor(job_group_mom, levels = c(1,2), labels=c("Not working", "Employed"))
  ) |>
  relocate(age_group_mom, educ_group_mom, job_group_mom, .after=job_mom) |>
  mutate(
    age_group_dad=case_when(
      age_dad <= 20 ~ 1, 
      age_dad > 20 & age_dad <= 29 ~ 2,
      age_dad >= 30 & age_dad <= 39 ~ 3,
      age_dad >= 40 ~ 4,
      TRUE ~ 5
    ),
    age_group_dad=factor(age_group_dad, 
                         levels=c(1:5), 
                         labels=c("<=20", "20-29", "30-39", ">=40", "Unknown")),
    educ_group_dad = case_when(
      educ_dad == 1 ~ 3, # College
      educ_dad == 2 ~ 2, # Secondary
      educ_dad == 3 ~ 2, # Secondary
      educ_dad == 4 ~ 1, # Primary
      educ_dad == 5 ~ 1, # No educaction 
      TRUE ~ 4, #Unknow
    ), 
    educ_group_dad = factor(educ_group_dad, 
                            levels = c(1:4), 
                            labels = c("Primary", "Secondary", "College", "Unknown")),
    job_group_dad = if_else(job_dad==3, 1, job_dad),
    job_group_dad = if_else(is.na(job_group_dad), 3, job_group_dad), 
    job_group_dad = factor(job_group_dad, levels = c(1,2,3), labels=c("Not working", "Employed", "Unknown"))
  ) |>
  relocate(age_group_dad, educ_group_dad, job_group_dad, .after=job_dad) |> 
  select(id, 
         com, name_com, reg, name_reg, 
         weeks,
         # Other variables 
         date_nac, day_nac, month_nac, year_nac, 
         date_week1, year_week1, month_week1, ym_week1,
         date_start_week_gest, date_ends_week_gest,
         sex, tbw, size, 
         age_group_mom,educ_group_mom,job_group_mom,
         age_group_dad,educ_group_dad,job_group_dad,
) |> 
drop_na()

glimpse(births) # 818336

### 6. Exclusion criteria  -----

# USA criteria: Alexander G, Himes J, Kaufaman R, Mor J, Kogan M. A United States national reference for fetal growth. Obstet Gynecol. 1996;87(2). 

missing <- births |>
  mutate(
    test = case_when(
      weeks == 28 ~ tbw >= 250 & tbw <= 2500,
      weeks == 29 ~ tbw >= 250 & tbw <= 2750,
      weeks == 30 ~ tbw >= 375 & tbw <= 3000,
      weeks == 31 ~ tbw >= 375 & tbw <= 3250,
      weeks == 32 ~ tbw >= 500 & tbw <= 3500,
      weeks == 33 ~ tbw >= 500 & tbw <= 3750,
      weeks == 34 ~ tbw >= 500 & tbw <= 4000,
      weeks == 35 ~ tbw >= 750 & tbw <= 4500,
      weeks == 36 ~ tbw >= 750 & tbw <= 5000,
      weeks == 37 ~ tbw >= 750 & tbw <= 5500,
      weeks >= 38 ~ tbw >= 1000 & tbw <= 6000,
      TRUE ~ FALSE  
    )
  ) |>
  filter(!test) |>
  group_by(weeks) |>
  summarise(loss_data = n()) |> 
  ungroup()

# Total missing: 
sum(missing$loss_data) # 146

write.xlsx(missing, "Data/Output/Data_exclussion_weeks_tbw-USA.xlsx")

# Chile criteria: remove 0.1% extreme values 
# Save data for sensivity analysis 
missing2 <- births |>
  group_by(year_week1, weeks) |>
  mutate(
    P2.5 = quantile(tbw, probs = 0.005, na.rm = TRUE),
    P97.5 = quantile(tbw, probs = 0.995, na.rm = TRUE),
    test = tbw >= P2.5 & tbw <= P97.5
  ) |>
  ungroup() |>
  filter(!test) |>
  group_by(weeks) |>
  summarise(loss_data = n(), .groups = 'drop') |> 
  ungroup()

# Total missing: 
sum(missing2$loss_data) # 7151

write.xlsx(missing2, "Data/Output/Data_exclussion_weeks_tbw-1percent.xlsx")

# Apply USA criteria 
# Init sample 835087
births <- births |>
  filter(
    case_when(
      weeks == 28 ~ tbw >= 250 & tbw <= 2500,
      weeks == 29 ~ tbw >= 250 & tbw <= 2750,
      weeks == 30 ~ tbw >= 375 & tbw <= 3000,
      weeks == 31 ~ tbw >= 375 & tbw <= 3250,
      weeks == 32 ~ tbw >= 500 & tbw <= 3500,
      weeks == 33 ~ tbw >= 500 & tbw <= 3750,
      weeks == 34 ~ tbw >= 500 & tbw <= 4000,
      weeks == 35 ~ tbw >= 750 & tbw <= 4500,
      weeks == 36 ~ tbw >= 750 & tbw <= 5000,
      weeks == 37 ~ tbw >= 750 & tbw <= 5500,
      weeks >= 38 ~ tbw >= 1000 & tbw <= 6000,
      TRUE ~ FALSE  
    )
  )
# End sample 834917
nrow(births)

### 7. Outcome: preterm (834917) ---- 
# Refs p10 weeks 28-42 (Alarcón & Pittaluga)
ref_p10 <- tribble(
  ~weeks, ~p10,
    28,  945.7,
    29, 1092.1,
    30, 1258.2,
    31, 1439.2,
    32, 1630.8,
    33, 1828.7,
    34, 2028.6,
    35, 2226.0,
    36, 2416.7,
    37, 2562.2,
    38, 2760.2,
    39, 2904.2,
    40, 3024.1,
    41, 3115.3,
    42, 3173.5,
    43,    NA_real_,  # dejamos NA para calcularlo
    44,    NA_real_
)

births <- births |> 
  left_join(ref_p10, by = "weeks") |> 
  mutate(p10 = if_else(is.na(p10), quantile(tbw, probs = 0.1, na.rm = TRUE), p10)) |> 
  mutate(birth_preterm = if_else(weeks < 37, 1, 0)) |>
  mutate(lbw = if_else(tbw < 2500, 1, 0)) |> 
  mutate(tlbw = if_else(tbw < 2500 & weeks >= 37, 1, 0)) |> 
  mutate(sga = if_else(tbw < p10, 1, 0)) |> 
  select(-p10) |> 
  #mutate(birth_extremely_preterm = if_else(weeks < 28, 1, 0)) |> 
  mutate(birth_very_preterm = if_else(weeks >= 28 & weeks <32, 1, 0)) |> 
  mutate(birth_moderately_preterm = if_else(weeks >= 32 & weeks <33, 1, 0)) |> 
  mutate(birth_late_preterm = if_else(weeks >= 34 & weeks <37, 1, 0)) |> 
  mutate(birth_term = if_else(weeks >= 37 & weeks <42, 1, 0)) |> 
  mutate(birth_posterm = if_else(weeks >= 42, 1, 0)) 

### 8.  Fixed cohort Bias ----
# N=834917

date_last_week <- as.Date("2020-12-31") - weeks(42) # 42 weeks
date_last_week

births <- births |> 
  filter(date_start_week_gest >= as.Date("2010-01-01"))

# N=776299

births <- births |> 
  filter(date_ends_week_gest <= date_last_week)

# N=713918
nrow(births)
summary(births)

# COVID variable
births <- births |> 
  mutate(covid = if_else(date_ends_week_gest > as.Date("2020-03-01"), 1, 0)) |> 
  relocate(covid, .before = birth_preterm)

### 7.  Save new births data ----
glimpse(births)
save(births, file=paste0(data_out, "births_2010_2020", ".RData"))



