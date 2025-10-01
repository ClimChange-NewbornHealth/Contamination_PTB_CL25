# Settings ---- 

options(scipen=999)
options(max.print = 99999999)
options(knitr.kable.NA = '', OutDec = ",") 
knitr::opts_chunk$set("ft.shadow" = FALSE)
rm(list=(ls()))

# Local figures text
#Sys.setlocale(category = "LC_ALL", "es_ES.UTF-8") #LAT
Sys.setlocale(category = "LC_ALL", "en_US.UTF-8") #USA
Sys.setlocale("LC_NUMERIC", "C")
