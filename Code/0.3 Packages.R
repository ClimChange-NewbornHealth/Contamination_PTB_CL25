# Packages ---- 

# Function install/load packages
install_load <- function(packages){
  for (i in packages) {
    if (i %in% rownames(installed.packages())) {
      library(i, character.only=TRUE)
    } else {
      install.packages(i)
      library(i, character.only = TRUE)
    }
  }
}

# Apply function
install_load(c("rio", 
               "janitor", 
               "tidyverse", 
               "openxlsx",
               "chilemapas", 
               "patchwork",
               "sf", 
               "vtable",
               "naniar", 
               "visdat", 
               "parallel", 
               "profvis", 
               "htmlwidgets",
               "future", 
               "purrr", 
               "furrr",
               "future.apply", 
               "zoo",
               "splines",      
               "magrittr",
               "plotly",
               "grid",
               "gridExtra",      
               "nlme",
               "ggstatsplot",
               "tidymodels",
               "knitr", 
               "kableExtra",
               "writexl",
               "RColorBrewer",
               "ComplexUpset",
               "ggpubr",
               "GGally",
               "rnaturalearth",
               "yardstick",
               "furrr", # parallel computation
               "doParallel", 
               "beepr",
               "tictoc",
               "paletteer",
               "texreg",
               "tidymodels", 
               "broom",
               "survival",
               "flexsurv",
               "survminer",
               "ggsurvfit",
               "DT",
               "data.table",
               "dlnm", 
               "splines", 
               "mgcv",
               "tsModel"
               ))


