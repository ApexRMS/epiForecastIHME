# IHME TRANSFORMER

library(rsyncrosim)
E <- ssimEnvironment()

# Source accessory scripts ------------------------------------------------

source(file.path(E$PackageDirectory, "0_epiForecastIHME_env.R"))
source(file.path(E$PackageDirectory, "0_epiForecastIHME_functions.R"))

# 1. Load data

forecast <- load_forecast(mySce = SCE, backend = "IHME", E = E)

# 2. Save to epi

save_to_epi(dataSubset = forecast$forecast, mySce = SCE, vars = VARS)

# 3. Process data and save it

saveDatasheet(SCE, as.data.frame(forecast$forecast) %>% 
                mutate(TransformerID = TRANSFORMER_NAME), 
              "epi_DataSummary", append = TRUE)

# 4. Save outpout

filePath <- forecast$inputs$ScenarioFile
save_output_info(mySce = SCE, inputs = forecast$inputs, 
                 backend = "IHME", filePath = filePath)
