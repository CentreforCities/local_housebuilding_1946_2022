library(tidyverse)
library(readxl)
library(dplyr)
library(openxlsx)
library(ggplot2)
library(tidyr)
library(purrr)

### EXPORT net completed & population stats to excel
# Export data as an excel workbook with multiple sheets:
export_to_excel <- function(data_frames, file_path) {
  # Create a New Excel workbook
  workbook <- createWorkbook()
  # Loop through the data frames
  for (i in seq_along(data_frames)) {
    # Get the data frame and sheet name
    df <- data_frames[[i]]
    sheet_name <- names(data_frames)[i]
    # Add the data frame to the workbook as a New sheet
    addWorksheet(workbook, sheetName = sheet_name)
    writeData(workbook, sheet = sheet_name, x = df)
  }
  # Save the Excel file
  saveWorkbook(workbook, file_path, overwrite = TRUE)
}

#get data!
#-----------------------------------------------------------------------------------------
# Get today's date in the format "YYYY-MM-DD"
today <- format(Sys.Date(), "%Y-%m-%d")

#bring in flagging data
path <- paste0(dirname("~"),"/Centre for Cities/Centre for Cities POC - Documents/Research/Housing/History of Planning 2/Data/Input/", collapse = NULL)
setwd(path)

flags_2023 <- read.xlsx("2024-07-22_GB_Lookups.xlsx", sheet = "lad_23")
colnames(flags_2023)[1] <- "LA.Code_2023"

flags_1981 <- read.xlsx("2024-07-22_GB_Lookups.xlsx", sheet = "cty_81")
colnames(flags_1981)[1] <- "cty_81"

path = paste0(dirname("~"),"/Centre for Cities/Centre for Cities POC - Documents/Research/Housing/History of Planning 2/Data/Output/From R scripts/Housebuilding/", collapse = NULL)
setwd(path) 

# Construct the file name dynamically
file_name <- paste0(today, "FINAL_74_22_summarised_data_output.xlsx")

#bring in 1974-2022 data at 1981 counties
population_74_22 <- read.xlsx(file_name, sheet = "pop_cty_81")
stock_74_22 <- read.xlsx(file_name, sheet = "stock_cty_81")
gross_all_building_74_22 <- read.xlsx(file_name, sheet = "built_cty_81")
gross_LA_building_74_22 <- read.xlsx(file_name, sheet = "LAbuilt_cty_81")
gross_HA_building_74_22 <- read.xlsx(file_name, sheet = "HAbuilt_cty_81")
gross_private_building_74_22 <- read.xlsx(file_name, sheet = "privatebuilt_cty_81")
gross_public_building_74_22 <- read.xlsx(file_name, sheet = "publicbuilt_cty_81")
gross_affordable_not_social_91_22 <- read.xlsx(file_name, sheet = "affordable_not_social_cty_81")
gross_social_91_22 <- read.xlsx(file_name, sheet = "socialbuilt_cty_81")
building_rate_74_22 <- read.xlsx(file_name, sheet = "total_building_rate_cty")
private_building_rate_74_22 <- read.xlsx(file_name, sheet = "private_building_rate_cty")
public_building_rate_74_22 <- read.xlsx(file_name, sheet = "public_building_rate_cty")
LA_building_rate_74_22 <- read.xlsx(file_name, sheet = "LA_building_rate_cty")
HA_building_rate_74_22 <- read.xlsx(file_name, sheet = "HA_building_rate_cty")
affordable_not_social_BR_91_22 <- read.xlsx(file_name, sheet = "affordable_not_social_BR_cty")
social_building_rate_91_22 <- read.xlsx(file_name, sheet = "social_building_rate_cty")

file_name <- paste0(today, "FINAL_45_73_summarised_data_output.xlsx")

#bring in 1946-1973 data at 1981 counties 
population_46_73 <- read.xlsx(file_name,  sheet = "population_cty_1981")
stock_45_73 <- read.xlsx(file_name,  sheet = "stocks_cty_1981")
gross_total_building_46_73 <- read.xlsx(file_name,  sheet = "gross_all_building_cty_1981")
gross_LA_building_46_73 <- read.xlsx(file_name,  sheet = "gross_LA_building_cty_1981")
gross_HA_building_46_73 <- read.xlsx(file_name,  sheet = "gross_HA_building_cty_1981")
gross_private_building_46_73 <- read.xlsx(file_name,  sheet = "gross_private_building_cty_1981")
gross_public_building_46_73 <- read.xlsx(file_name,  sheet = "gross_public_building_cty_1981")
building_rate_46_73 <- read.xlsx(file_name,  sheet = "total_building_rate_cty_1981")
private_building_rate_46_73 <- read.xlsx(file_name,  sheet = "private_building_rate_cty_1981")
public_building_rate_46_73 <- read.xlsx(file_name,  sheet = "public_building_rate_cty_1981")
LA_building_rate_46_73 <- read.xlsx(file_name,  sheet = "LA_building_rate_cty_1981")
HA_building_rate_46_73 <- read.xlsx(file_name,  sheet = "HA_building_rate_cty_1981")

#remove 1973 from pre-74 data so no duplicate 1973 
stock_45_73 <- stock_45_73 %>% 
  select(-adj_dec_73)

#join pre- and post-74 data by 1981 county geography and add 1981 flags and green belt information
#----------------------------------------------------------------------------------------

population46_22_1981geog <- population_46_73 %>% 
  left_join(population_74_22, by = "cty_81") %>%
  select(-matches("_ha|lad_23|contains|gb"))

population46_22_1981geog <- population46_22_1981geog %>% 
  left_join(flags_1981, by = "cty_81")   %>%
  rename_with(~ gsub("_POP", "", .), everything())


stock46_22_1981geog <- stock_45_73 %>% 
  left_join(stock_74_22, by = "cty_81") %>%
  select(-matches("_ha|lad_23|contains|gb"))

stock46_22_1981geog <- stock46_22_1981geog %>%
  left_join(flags_1981, by = "cty_81") %>%
  rename_with(~ gsub("adj_dec_", "", .), everything()) %>%
  rename_with(~ gsub("dec_", "", .), everything()) %>%
  rename_with(~ gsub("apr_", "", .), everything()) %>%
  rename_with(~ ifelse(grepl("^[0-9]{2}$", .), paste0("19", .), .), everything())


gross_total_building46_22_1981geog <- gross_total_building_46_73 %>% 
  left_join(gross_all_building_74_22, by = "cty_81") %>%
  select(-matches("_ha|lad_23|contains|gb"))

gross_total_building46_22_1981geog <- gross_total_building46_22_1981geog %>% 
  left_join(flags_1981, by = "cty_81") %>%
  rename_with(~ gsub("_COMPLETED_ALL", "", .), everything())


gross_LA_building46_22_1981geog <- gross_LA_building_46_73 %>% 
  left_join(gross_LA_building_74_22, by = "cty_81") %>%
  select(-matches("_ha|lad_23|contains|gb"))

gross_LA_building46_22_1981geog <- gross_LA_building46_22_1981geog %>% 
  left_join(flags_1981, by = "cty_81") %>%
  rename_with(~ gsub("_COMPLETED_LOCAL", "", .), everything())


gross_HA_building46_22_1981geog <- gross_HA_building_46_73 %>% 
  left_join(gross_HA_building_74_22, by = "cty_81") %>%
  select(-matches("_ha|lad_23|contains|gb"))

gross_HA_building46_22_1981geog <- gross_HA_building46_22_1981geog %>% 
  left_join(flags_1981, by = "cty_81") %>%
  rename_with(~ gsub("_COMPLETED_HA", "", .), everything())


gross_private_building46_22_1981geog <- gross_private_building_46_73 %>% 
  left_join(gross_private_building_74_22, by = "cty_81") %>%
  select(-matches("_ha|lad_23|contains|gb")) 

gross_private_building46_22_1981geog <- gross_private_building46_22_1981geog %>% 
  left_join(flags_1981, by = "cty_81") %>%
  rename_with(~ gsub("_COMPLETED_PRIVATE", "", .), everything())


gross_public_building46_22_1981geog <- gross_public_building_46_73 %>% 
  left_join(gross_public_building_74_22, by = "cty_81") %>%
  select(-matches("_ha|lad_23|contains|gb"))

gross_public_building46_22_1981geog <- gross_public_building46_22_1981geog %>% 
left_join(flags_1981, by = "cty_81") %>%
  rename_with(~ gsub("_COMPLETED_PUBLIC", "", .), everything()) 

building_rate_46_22 <- building_rate_46_73 %>% 
  left_join(building_rate_74_22, by = "cty_81")

private_building_rate_46_22 <- private_building_rate_46_73 %>% 
  left_join(private_building_rate_74_22, by = "cty_81")

public_building_rate_46_22 <- public_building_rate_46_73 %>% 
  left_join(public_building_rate_74_22, by = "cty_81")

LA_building_rate_46_22 <- LA_building_rate_46_73 %>% 
  left_join(LA_building_rate_74_22, by = "cty_81")

HA_building_rate_46_22 <- HA_building_rate_46_73 %>% 
  left_join(HA_building_rate_74_22, by = "cty_81")

#affordable not social and social don't have pre1973 equivalents, so just joining to flags
gross_affordable_not_social_building91_22_1981geog <- gross_affordable_not_social_91_22 %>% 
  left_join(flags_1981, by = "cty_81")

gross_social_91_22_1981geog <- gross_social_91_22 %>% 
  left_join(flags_1981, by = "cty_81") 

#Export data 
#-------------------------------------------------------------------------------------
#1981 counties summarised data output 
date <- format(Sys.Date(), "%Y-%m-%d")
file_name_geog <- paste0(date, "FINAL_45_22_counties_flagged_data_output", ".xlsx")
dir_path_geog = paste0(dirname("~"),"/Centre for Cities/Centre For Cities POC - Documents/Research/Housing/History of Planning 2/Data/Output/From R scripts/Housebuilding/")
file_path_geog <- file.path(dir_path_geog, file_name_geog)

# Create a list of data frames
df_list_geog <- list(population46_22_1981geog, stock46_22_1981geog , gross_total_building46_22_1981geog, gross_private_building46_22_1981geog, 
                     gross_public_building46_22_1981geog, gross_LA_building46_22_1981geog, gross_HA_building46_22_1981geog,
                     gross_affordable_not_social_building91_22_1981geog, gross_social_91_22_1981geog,
                     building_rate_46_22, private_building_rate_46_22, 
                     public_building_rate_46_22, LA_building_rate_46_22, HA_building_rate_46_22, 
                     affordable_not_social_BR_91_22, social_building_rate_91_22)
names(df_list_geog) <- c("population", "stock", "total_built", "private_built", "public_built", "local_built", "HA_built", 
                         "affordable_not_social_built", "social_built", 
                         "total_BR", "private_BR", 
                         "public_BR", "LA_BR", "HA_BR", "affordable_not_social_BR", "social_BR")                         

# Export data frames to Excel
export_to_excel(df_list_geog, file_path_geog)

 

###ENGLAND total summaries 
#get data!
#-----------------------------------------------------------------------------------------
# Get today's date in the format "YYYY-MM-DD"
today <- format(Sys.Date(), "%Y-%m-%d")

path = paste0(dirname("~"),"/Centre for Cities/Centre for Cities POC - Documents/Research/Housing/History of Planning 2/Data/Output/From R scripts/Housebuilding/", collapse = NULL)
setwd(path) 

# Construct the file name dynamically
file_name <- paste0(today, "FINAL_74_22_summarised_data_output.xlsx")

#bring in 1974-2022 data at 1981 counties
population_74_22 <- read.xlsx(file_name, sheet = "population_ENG")
stock_74_22 <- read.xlsx(file_name, sheet = "stocks_ENG")
gross_all_building_74_22 <- read.xlsx(file_name, sheet = "gross_total_building_ENG")
gross_private_building_74_22 <- read.xlsx(file_name, sheet = "gross_private_building_ENG")
gross_public_building_74_22 <- read.xlsx(file_name, sheet = "gross_public_building_ENG")

file_name <- paste0(today, "FINAL_45_73_summarised_data_output.xlsx")

#bring in 1946-1973 data at 1981 counties 
population_46_73 <- read.xlsx(file_name,  sheet = "population_ENG")
stock_45_73 <- read.xlsx(file_name,  sheet = "stocks_ENG")
gross_total_building_46_73 <- read.xlsx(file_name,  sheet = "gross_total_building_ENG")
gross_private_building_46_73 <- read.xlsx(file_name,  sheet = "gross_private_building_ENG")
gross_public_building_46_73 <- read.xlsx(file_name,  sheet = "gross_public_building_ENG")

# Function to add 'Country' column at the beginning
add_country_column <- function(df) {
  df <- cbind(Country = 'ENGLAND', df)
  return(df)
}

#Add 'Country' column to each dataframe
population_74_22 <- add_country_column(population_74_22)
stock_74_22 <- add_country_column(stock_74_22)
gross_all_building_74_22 <- add_country_column(gross_all_building_74_22)
gross_private_building_74_22 <- add_country_column(gross_private_building_74_22)
gross_public_building_74_22 <- add_country_column(gross_public_building_74_22)

population_46_73 <- add_country_column(population_46_73)
stock_45_73 <- add_country_column(stock_45_73)
gross_total_building_46_73 <- add_country_column(gross_total_building_46_73)
gross_private_building_46_73 <- add_country_column(gross_private_building_46_73)
gross_public_building_46_73 <- add_country_column(gross_public_building_46_73)


#join them! 
population <- left_join(population_46_73, population_74_22, by = "Country") %>%
  rename_with(~ gsub("_POP", "", .), everything())

stock <- left_join(stock_45_73, stock_74_22, by = "Country") 

# Function to extract year from column names
extract_year <- function(name) {
  if (grepl("\\d{4}", name)) {
    return(sub(".*(\\d{4}).*", "\\1", name))
  }
  return(name)
}

# Apply the function to column names from the second column onward
colnames(stock)[2:ncol(stock)] <- sapply(colnames(stock)[2:ncol(stock)], extract_year)

stock <- stock[ , !colnames(stock) %in% '1973.1']
stock <- stock[ , !colnames(stock) %in% '1973.1']

gross_total_built <- left_join(gross_total_building_46_73, gross_all_building_74_22, by = "Country") %>%
  rename_with(~ gsub("_COMPLETED_ALL", "", .), everything())

gross_private_built <- left_join(gross_private_building_46_73, gross_private_building_74_22, by = "Country") %>%
  rename_with(~ gsub("_COMPLETED_PRIVATE", "", .), everything())

gross_public_built <- left_join(gross_public_building_46_73, gross_public_building_74_22, by = "Country") %>%
  rename_with(~ gsub("_COMPLETED_PUBLIC", "", .), everything())


#make long so join by years is easy 
population_long <- population %>%
  pivot_longer(cols = -Country,          
               names_to = "YEAR",  
               values_to = "population")  

stock_long <- stock %>%
  pivot_longer(cols = -Country,          
               names_to = "YEAR",  
               values_to = "stock")  

gross_total_built_long <- gross_total_built %>%
  pivot_longer(cols = -Country,          
               names_to = "YEAR",  
               values_to = "gross_total_built")  

gross_private_built_long <- gross_private_built %>%
  pivot_longer(cols = -Country,          
               names_to = "YEAR",  
               values_to = "gross_private_built")  

gross_public_built_long <- gross_public_built %>%
  pivot_longer(cols = -Country,          
               names_to = "YEAR",  
               values_to = "gross_public_built")  

#join them into one dataframe 
ENG_data <- left_join(stock_long, population_long, by = "YEAR")
ENG_data <- left_join(ENG_data, gross_total_built_long, by = "YEAR")
ENG_data <- left_join(ENG_data, gross_private_built_long, by = "YEAR")
ENG_data <- left_join(ENG_data, gross_public_built_long, by = "YEAR")

ENG_data <- ENG_data[ , !grepl("Country", names(ENG_data))]

#export
#-------------------------------------------------------------------------
#England summarised data output 
date <- format(Sys.Date(), "%Y-%m-%d")
file_name_geog <- paste0(date, "FINAL_45_22_England_data_output", ".xlsx")
dir_path_geog = paste0(dirname("~"),"/Centre for Cities/Centre For Cities POC - Documents/Research/Housing/History of Planning 2/Data/Output/From R scripts/Housebuilding/")
file_path_geog <- file.path(dir_path_geog, file_name_geog)

#####ALERT - need to add gross_public_building46_22_1981geog,
# Create a list of data frames
df_list_geog <- list(ENG_data, population, stock, gross_total_built, gross_private_built, gross_public_built)
names(df_list_geog) <- c("England_data_table", "population", "stock", "gross_total_built", "gross_private_built", "gross_public_built")                         

# Export data frames to Excel
export_to_excel(df_list_geog, file_path_geog)



