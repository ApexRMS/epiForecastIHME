# Environment -------------------------------------------------------------

# Load libraries
library(dplyr)
library(tidyr)
library(readr)
library(lubridate) 

# Load environment

LIB <- ssimLibrary()
SCE <- scenario()

# TRANSFER_DIR <- e$TransferDirectory

VARS <- c("Cases - Cumulative", "Cases - Daily", 
          "Recovered - Cumulative", "Recovered - Daily", 
          "Tested - Cumulative", "Tested - Daily",
          "Deaths - Cumulative", "Deaths - Daily", 
          "Vaccines - Cumulative", "Vaccines - Daily")
RAWVARS <- c("confirmed", "dailyconfirmed", 
             "recovered", "dailyrecovered", 
             "tested", "dailytested", 
             "deaths", "dailydeaths", 
             "vaccines", "dailyvaccines")
LOOKUP <- data.frame(VARS = VARS,
                     RAWVARS = RAWVARS)
