# Code 1: Birth data preparation ----
rm(list=ls())
## Settings ----
source("Code/0.1 Settings.R")
source("Code/0.2 Packages.R")
source("Code/0.3 Functions.R")

# Data path 
data_inp <- "Data/Input/Nacimientos/"
data_out <- "Data/Output/"

## 1. Birth data ---- 

file <- paste0("births_2010_2020", ".RData")

# Open data in R
load(paste0(data_out, file)) 
glimpse(births)
summary(births)

## 2. Gestational long weeks ---- 

births_weeks <- births |> 
  rowwise() |> 
  mutate(week_gest = list(seq.Date(date_start_week_gest, date_ends_week_gest, by = "week"))) |>
  unnest(week_gest) |>
  group_by(id) |>
  mutate(week_gest_num = paste0(abs(weeks - row_number())),  
         week_gest_num = (weeks) - as.numeric(week_gest_num), 
         date_start_week = (week_gest - (7 * abs(week_gest_num - row_number()))) - weeks(1), #(abs(week_gest_num - row_number())),
         date_end_week = week_gest - (7 * abs(week_gest_num - row_number()))
         ) |> # ,(abs(week_gest_num - row_number())
  group_by(id) |> 
  distinct(week_gest_num, .keep_all = TRUE) |> 
  arrange(id, week_gest_num) |> 
  ungroup()

glimpse(births_weeks)

# Check results 
t1 <- births_weeks %>%
  group_by(id) %>% 
  summarise(min=min(week_gest_num), 
            max=max(week_gest_num), 
            n=n(), 
            test=if_else(n==max, 1, 0))

table(t1$test)

## 3.  Save new births data ----
save(births_weeks, file=paste0(data_out, "births_2010_2020_weeks_long", ".RData"))



