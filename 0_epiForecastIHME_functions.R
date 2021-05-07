# Package Helpers ---------------------------------------------------------

# Adapted Code ------------------------------------------------------------

# Adapted from
# Get all dates and urls, returns dataframe with all the information
# https://github.com/ApexRMS/covid19sim/blob/master/setup.R
get_all_dates <- function(base_url = IHME_URL){
  # Scrape the list of IHME model archives using `rvest`
  dates_url <- data.frame(
    url = base_url %>%
      read_html %>%                        # Pull html for the downloads page
      html_nodes(css = "a") %>%            # Extract hyperlink tags
      html_attr(name = "href") %>%         # Extract the urls
      str_subset("202.*zip")) %>%     
    mutate(date = ymd(str_extract(url, "202\\d-\\d\\d-\\d\\d"))) %>% 
    arrange(date)
  return(dates_url)
}

# Adapted from
# Download data and return file names
# https://github.com/ApexRMS/covid19sim/blob/master/headers/ihme.R
download_IHME <- function(url, tempdir = E$TempDirectory){
  
  ihmeZip <- file.path(tempdir, "temp.zip")
  
  # Download, unzip, and remove zip file
  # - also store the original extracted folder name for renaming
  download.file(url, ihmeZip, quiet = TRUE)
  unzip(ihmeZip, exdir = tempdir)
  folderName <- basename(dirname(url)) 
  file.remove(ihmeZip)
  
  # Return the list of file names 
  all_files <- list.files(file.path(tempdir, folderName), full.names = TRUE)
  
  return(all_files)
}

# Package functions -------------------------------------------------------

load_forecast <- function(mySce, backend="IHME", E = E){
  
  inputs <- load_inputs(backend, mySce)
  
  closest_date <- get_closest_date(inputs$inputs$ForecastDate)
  
  all_files <- download_IHME(closest_date$url, E$TempDirectory)
  
  scenario_file <- match_scenario(all_files, inputs$inputs$ForecastScenario)
  
  forecast_data <- read_csv(scenario_file)
  
  forecast_data_clean <- process_data(forecast_data)
  
  forecast_data_clean_filtered <- filter_data(forecast_data_clean, inputs$input_vars)
  
  return(forecast_data_clean)
  
}

# Load and process the inputs parameters
load_inputs <- function(backend, mySce){
  
  if(backend == "IHME"){
    
    inputsheet <- "epiForecastIHME_InputsIHME"
    
  } else {
    
    stop("Only IHME is supported as backend in this package.")
    
  }
  
  inputs <- datasheet(mySce, inputsheet, lookupsAsFactors = FALSE) %>% 
    mutate(ForecastDate = ymd(ForecastDate))
  input_vars <- check_inputs(inputs)
  
  outList <- list(inputs = inputs, 
                  input_vars = input_vars)
  
  return(outList)
  
}

# Check inputs
check_inputs <- function(inputs){
  
  # Check jurisdiction
  if (length(inputs$Country) != 0){
    
    juris_input <- inputs$Country
    
    if (juris_input == "[All Countries]"){
      
      juris_covid <- NULL
      
    } else {
      
      juris_covid <- juris_input
      
    }
    
  } else {
    
    stop("No Country provided")
    
  }
  
  # Check level
  if(length(inputs$Level) != 0){
    
    level_input <- inputs$Level
    
    level_covid <- lookup_level(inputs$Level)
    
  } else {
    
    message("No level provided, default to level 1 (Country)")
    
    level_input <- "(1) Country"
    level_covid <- 1
    
  }
  
  return(list(juris_input = juris_input, 
              juris_covid = juris_covid, 
              level_input = level_input, 
              level_covid = level_covid))
}

# Replace level string
lookup_level <- function(level){
  
  if(is.character(level)){
    
    if(grepl("Country", level,  fixed = TRUE)){
      level_int = 1
    } else if (grepl("State", level,  fixed = TRUE)){
      level_int = 2
    } else if (grepl("Lower", level,  fixed = TRUE)){
      level_int = 3
    }
    
  } else {
    
    stop("Level should be of type character")
    
  }
  
  return(level_int)
  
}

# Get the closest date to the list of possible dates
get_closest_date <- function(input_date){
  
  all_dates <- get_all_dates()
  
  if(is.null(input_date)){
    # Get the first one
    query_date <- slice_tail(all_dates, n = 1)
  } else {
    distance_from_input <- abs(all_dates$date - input_date)
    query_date_id <- which(distance_from_input == min(distance_from_input))[1]
    query_date <- slice(all_dates, query_date_id)
  }
  
  return(query_date)
  
}

# Match filenames with scenarios
match_scenario <- function(files, scenarioname){
  
  if(is.null(scenarioname)){
    scenarioname <- "Base"
  }
  
  if(scenarioname == "Base"){
    return_scenario <- 
      names(which(sapply(files, grepl, pattern = "reference", fixed = TRUE)))
  } else if(scenarioname == "Worse"){
    return_scenario <- 
      names(which(sapply(files, grepl, pattern = "worse", fixed = TRUE)))
  } else if(scenarioname == "Masks"){
    return_scenario <- 
      names(which(sapply(files, grepl, pattern = "best", fixed = TRUE)))
  }
  
  return(return_scenario)
  
}

# Process data
process_data <- function(forecast_data, lookup = VARS_LOOKUP){
  
  forecast_data_selected <- forecast_data %>%
    select(-contains(c("type", "rate", "confirmed", "bed", "Ven",
                       "ICU", "mobility", "tests", "seroprev", "pop"))) %>% 
    select(-c("deaths_upper", "deaths_lower", "deaths_mean")) %>% 
    select(-c("totdea_upper", "totdea_lower", "totdea_mean")) %>% 
    select(-c("V1", "location_id"))
  
  forecast_data_clean <- forecast_data_selected %>% 
    rename(Timestep = date, Jurisdiction = location_name) %>% 
    pivot_longer(3:last_col(), names_to = "RAWVARS", values_to = "Value") %>% 
    left_join(lookup, by = "RAWVARS") %>% 
    select(-"RAWVARS") %>% rename(Variable = VARS) %>% 
    relocate(Variable, .after = "Jurisdiction") %>% 
    filter(!is.na(Variable))
  
  # TODO calculadte cumulative hospitalizations

  return(forecast_data_clean)
}

# Filter based on jurisdictions
filter_data <- function(forecast_data, input_vars, lookup = JURIS_LOOkUP){
  
  browser()
  
  # Filter lookup
  lookup_sub <- lookup %>% 
    select(1:(input_vars$level_covid)) %>% 
    unique() %>% 
    rename(Jurisdiction = .data[[2]])
  
  if(is.null(input_vars$juris_covid)){
    filtered_data <- filtered_data %>% 
      filter(Jurisdiction == "Global") %>% 
      mutate()
  }
  
  
  
  return(filtered_data)
  
}

# Save jurisdictions to the epi package datasheets
save_to_epi <- function(dataSubset, mySce, vars){
  
  # Get the vector of jurisdictions
  allJuris <- unique(dataSubset$Jurisdiction)
  vars <- unique(vars)
  
  # Add the required variables and jurisdictions to the SyncroSim project
  saveDatasheet(mySce, 
                data.frame(Name = allJuris), "epi_Jurisdiction")
  saveDatasheet(mySce, 
                data.frame(Name = vars), "epi_Variable")
  
}

# Make file name
make_filename <- function(inputs_vars){
  
  # TODO
  
  return(fileName)
}

# Save output info
save_output_info <- function(mySce, input_vars, backend, filePath){
  
  if(backend == "IHME"){
    
    outputsheet <- "epiForecastIHME_OutputsIHME"
    sourceID <- "IHME"
    
  } else {
    
    stop("Only IHME is supported as backend in this package.")
    
  }
  
  # TODO UPDATE THIS
  
  download_time <- as.character(Sys.time())
  
  output <- datasheet(mySce, outputsheet) %>% add_row()
  
  output$Jurisdiction = input_vars$juris_input
  output$DataSourceID = sourceID
  output$Level = input_vars$level_input
  output$DownloadFile = filePath
  output$DownloadDateTime = download_time
  
  saveDatasheet(mySce, output, outputsheet)
}