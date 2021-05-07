# IHME TRANSFORMER
# Source accessory scripts ------------------------------------------------

source(file.path(E$PackageDirectory, "0_epiDataForecastIHME_env.R"))
source(file.path(E$PackageDirectory, "0_epiDataForecastIHME_functions.R"))

# 1. Load data

forecast <- load_forecast(mySce = SCE, backend = "IHME", E = E)

# 2. Save to epi

save_to_epi(dataSubset = inputs$IHMEData, mySce = SCE, vars = VARS)

# 3. Process data and save it

forecasts <- process_data(
    data = inputs$IHMEData,
    backend = "IHME", lookup = LOOKUP) %>%
    mutate(TransformerID = TRANSFORMER_NAME)

saveDatasheet(SCE, covidDataFinal, "epi_DataSummary", append = TRUE)

# 4. Write out data

fileName <- make_filename(inputs_vars = inputs$input_vars)
filePath <- file.path(E$TransferDirectory, fileName)

write.csv(forecasts, filePath, row.names = FALSE)

# 5. Save outpout

save_output_info(mySce = SCE, input_vars = inputs$input_vars, backend = "HUB", 
                 filePath = filePath)