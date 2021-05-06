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

IHME_URL <- "http://www.healthdata.org/covid/data-downloads"

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
