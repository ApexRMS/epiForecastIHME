# Environment -------------------------------------------------------------

# Load libraries
library(rsyncrosim)
library(dplyr)
library(tidyr)
library(readr)
library(lubridate) 
library(httr)
library(stringr)
library(rvest)

# Load environment --------------------------------------------------------

LIB <- ssimLibrary()
SCE <- scenario()

# Variables ---------------------------------------------------------------

TRANSFORMER_NAME <- "Download IHME forecasts"

IHME_URL <- "http://www.healthdata.org/covid/data-downloads"

VARS <- 
  c("Hospitalizations - Daily", "Hospitalizations - Daily Min", "Hospitalizations - Daily Max", 
    "Deaths - Daily", "Deaths - Daily Min", "Deaths - Daily Max", 
    "Deaths - Cumulative", "Deaths - Cumulative Min", "Deaths - Cumulative Max", 
    "Infections - Daily", "Infections - Daily Min", "Infections - Daily Max", 
    "Infections - Cumulative", "Infections - Cumulative Min", "Infections - Cumulative Max")

RAWVARS <- 
  c("admis_mean", "admis_lower", "admis_upper",
    "deaths_mean_smoothed", "deaths_lower_smoothed", "deaths_upper_smoothed", 
    "totdea_mean_smoothed", "totdea_lower_smoothed", "totdea_upper_smoothed", 
    "est_infections_mean", "est_infections_lower", "est_infections_upper", 
    "inf_cuml_mean", "inf_cuml_lower", "inf_cuml_upper")

VARS_LOOKUP <- data.frame(VARS = VARS,
                          RAWVARS = RAWVARS)

JURIS_LOOkUP <- read_csv(file.path(E$PackageDirectory, "data", 
                                   "juris_lookup.csv"), col_types = cols(
                                     administrative_area_level_1 = col_character(),
                                     administrative_area_level_2 = col_character(),
                                     administrative_area_level_3 = col_character()
                                   ))
