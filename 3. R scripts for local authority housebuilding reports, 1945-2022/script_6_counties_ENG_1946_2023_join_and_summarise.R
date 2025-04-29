library(tidyverse)
library(readxl)
library(dplyr)
library(openxlsx)
library(ggplot2)
library(tidyr)
library(purrr)

#set directory workaround
dirname <- sub("^(([^/]+/){2}[^/]+).*", "\\1", dirname("~"))

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
path <- paste0(dirname, "/centre for Cities/Centre for Cities POC - Documents/Research/Housing/History of Planning 2/Data/Input/", collapse = NULL)
setwd(path)

flags_2023 <- read.xlsx("2024-07-22_GB_Lookups.xlsx", sheet = "lad_23")
colnames(flags_2023)[1] <- "LA.Code_2023"

flags_1981 <- read.xlsx("2024-07-22_GB_Lookups.xlsx", sheet = "cty_81")
colnames(flags_1981)[1] <- "cty_81"

path = paste0(dirname, "/centre for Cities/Centre for Cities POC - Documents/Research/Housing/History of Planning 2/Data/Output/From R scripts/Housebuilding/", collapse = NULL)
setwd(path) 

# Construct the file name dynamically
file_name <- paste0(today, "FINAL_74_23_summarised_data_output.xlsx")

#bring in 1974-2022 data at 1981 counties
population_74_22 <- read.xlsx(file_name, sheet = "pop_cty_81")
stock_74_23 <- read.xlsx(file_name, sheet = "stock_cty_81")
gross_all_building_74_23 <- read.xlsx(file_name, sheet = "built_cty_81")
gross_LA_building_74_23 <- read.xlsx(file_name, sheet = "LAbuilt_cty_81")
gross_HA_building_74_23 <- read.xlsx(file_name, sheet = "HAbuilt_cty_81")
gross_market_building_74_23 <- read.xlsx(file_name, sheet = "marketbuilt_cty_81")
gross_private_delivered_building_74_23 <- read.xlsx(file_name, sheet = "private_delivered_cty_81")
gross_public_building_74_23 <- read.xlsx(file_name, sheet = "publicbuilt_cty_81")
building_rate_74_23 <- read.xlsx(file_name, sheet = "total_building_rate_cty")
market_building_rate_74_23 <- read.xlsx(file_name, sheet = "market_building_rate_cty")
private_delivered_rate_74_23 <- read.xlsx(file_name, sheet = "private_delivered_rate_cty")
public_building_rate_74_23 <- read.xlsx(file_name, sheet = "public_building_rate_cty")
LA_building_rate_74_23 <- read.xlsx(file_name, sheet = "LA_building_rate_cty")
HA_building_rate_74_23 <- read.xlsx(file_name, sheet = "HA_building_rate_cty")

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

#remove flags/categories data from 1946-73 dataframes (as joined later on)
population_46_73 <- population_46_73[, 1:29]
stock_45_73 <- stock_45_73[, 1:29] # note this removes 1973 too because can import 1973 from the second dataset 
gross_total_building_46_73 <- gross_total_building_46_73[, 1:29]
gross_LA_building_46_73 <- gross_LA_building_46_73[, 1:29]
gross_HA_building_46_73 <- gross_HA_building_46_73[, 1:13]
gross_private_building_46_73 <- gross_private_building_46_73[, 1:29]
gross_public_building_46_73 <- gross_public_building_46_73[, 1:29]


#join pre- and post-74 data by 1981 county geography and add 1981 flags and green belt information
#----------------------------------------------------------------------------------------

population46_22_1981geog <- population_46_73 %>% 
  left_join(population_74_22, by = "cty_81") %>%
  select(-matches("_ha|lad_23|contains|gb"))

population46_22_1981geog <- population46_22_1981geog %>% 
  left_join(flags_1981, by = "cty_81")   %>%
  rename_with(~ gsub("_POP", "", .), everything())


stock46_23_1981geog <- stock_45_73 %>% 
  left_join(stock_74_23, by = "cty_81") %>%
  select(-matches("_ha|lad_23|contains|gb"))

stock46_23_1981geog <- stock46_23_1981geog %>%
  left_join(flags_1981, by = "cty_81") %>%
  rename_with(~ gsub("adj_dec_", "", .), everything()) %>%
  rename_with(~ gsub("dec_", "", .), everything()) %>%
  rename_with(~ gsub("apr_", "", .), everything()) %>%
  rename_with(~ ifelse(grepl("^[0-9]{2}$", .), paste0("19", .), .), everything())


gross_total_building46_23_1981geog <- gross_total_building_46_73 %>% 
  left_join(gross_all_building_74_23, by = "cty_81") %>%
  select(-matches("_ha|lad_23|contains|gb"))

gross_total_building46_23_1981geog <- gross_total_building46_23_1981geog %>% 
  left_join(flags_1981, by = "cty_81") %>%
  rename_with(~ gsub("_COMPLETED_ALL", "", .), everything())


gross_LA_building46_23_1981geog <- gross_LA_building_46_73 %>% 
  left_join(gross_LA_building_74_23, by = "cty_81") %>%
  select(-matches("_ha|lad_23|contains|gb"))

gross_LA_building46_23_1981geog <- gross_LA_building46_23_1981geog %>% 
  left_join(flags_1981, by = "cty_81") %>%
  rename_with(~ gsub("_COMPLETED_LOCAL", "", .), everything())


gross_HA_building46_23_1981geog <- gross_HA_building_46_73 %>% 
  left_join(gross_HA_building_74_23, by = "cty_81") %>%
  select(-matches("lad_23|contains|gb"))

gross_HA_building46_23_1981geog <- gross_HA_building46_23_1981geog %>% 
  left_join(flags_1981, by = "cty_81") %>%
  rename_with(~ gsub("_COMPLETED_HA", "", .), everything())


gross_market_building46_23_1981geog <- gross_private_building_46_73 %>% 
  left_join(gross_market_building_74_23, by = "cty_81") %>%
  select(-matches("_ha|lad_23|contains|gb")) 

gross_market_building46_23_1981geog <- gross_market_building46_23_1981geog %>% 
  left_join(flags_1981, by = "cty_81") %>%
  rename_with(~ gsub("_COMPLETED_market", "", .), everything())


gross_private_delivered46_23_1981geog <- gross_private_building_46_73 %>% 
  left_join(gross_private_delivered_building_74_23, by = "cty_81") %>%
  select(-matches("_ha|lad_23|contains|gb")) 

gross_private_delivered46_23_1981geog <- gross_private_delivered46_23_1981geog %>% 
  left_join(flags_1981, by = "cty_81") %>%
  rename_with(~ gsub("_COMPLETED_market", "", .), everything())


gross_public_building46_23_1981geog <- gross_public_building_46_73 %>% 
  left_join(gross_public_building_74_23, by = "cty_81") %>%
  select(-matches("_ha|lad_23|contains|gb"))

gross_public_building46_23_1981geog <- gross_public_building46_23_1981geog %>% 
left_join(flags_1981, by = "cty_81") %>%
  rename_with(~ gsub("_COMPLETED_PUBLIC", "", .), everything())

building_rate_46_23 <- building_rate_46_73 %>% 
  left_join(building_rate_74_23, by = "cty_81")

market_building_rate_46_23 <- private_building_rate_46_73 %>% 
  left_join(market_building_rate_74_23, by = "cty_81")

private_delivered_rate_46_23 <- private_building_rate_46_73 %>% 
  left_join(private_delivered_rate_74_23, by = "cty_81")

public_building_rate_46_23 <- public_building_rate_46_73 %>% 
  left_join(public_building_rate_74_23, by = "cty_81")

LA_building_rate_46_23 <- LA_building_rate_46_73 %>% 
  left_join(LA_building_rate_74_23, by = "cty_81")

HA_building_rate_46_23 <- HA_building_rate_46_73 %>% 
  left_join(HA_building_rate_74_23, by = "cty_81")


#Export data 
#-------------------------------------------------------------------------------------
#1981 counties summarised data output 
date <- format(Sys.Date(), "%Y-%m-%d")
file_name_geog <- paste0(date, "FINAL_45_23_counties_flagged_data_output", ".xlsx")
dir_path_geog = paste0(dirname, "/centre for Cities/Centre for Cities POC - Documents/Research/Housing/History of Planning 2/Data/Output/From R scripts/Housebuilding/")
file_path_geog <- file.path(dir_path_geog, file_name_geog)

#####ALERT - need to add gross_public_building46_22_1981geog,
# Create a list of data frames
df_list_geog <- list(population46_22_1981geog, stock46_23_1981geog , gross_total_building46_23_1981geog, gross_market_building46_23_1981geog, gross_private_delivered46_23_1981geog,
                     gross_public_building46_23_1981geog, gross_LA_building46_23_1981geog, gross_HA_building46_23_1981geog, 
                     building_rate_46_23, market_building_rate_46_23, private_delivered_rate_46_23,
                     public_building_rate_46_23, LA_building_rate_46_23, HA_building_rate_46_23)
names(df_list_geog) <- c("population", "stock", "total_built", "market_built", "private_delivered", 
                         "public_built", "LA_built", "HA_built", 
                         "total_BR", "market_BR", "private_delivered_BR",
                         "public_BR", "LA_BR", "HA_BR")                         

# Export data frames to Excel
export_to_excel(df_list_geog, file_path_geog)



###ENGLAND total summaries 
#get data!
#-----------------------------------------------------------------------------------------
# Get today's date in the format "YYYY-MM-DD"
today <- format(Sys.Date(), "%Y-%m-%d")

# a small proportion of affordable housebuilding data from T1011 doesn't have geography - so not possible to include in LA or County-level data. 
# But bring in here from affordable housebuilding summary so no missing data
# note, this means that England AH totals won't match sum of smaller geographies

path = paste0(dirname, "/centre for Cities/Centre for Cities POC - Documents/Research/Housing/History of Planning 2/Data/Output/From R scripts/Affordable housebuilding/", collapse = NULL)
setwd(path) 

file_name <- paste0(today, "affordable_housing_summaries_lad_2023.xlsx")

#bring in LA level AH data 
gross_public_building_91_23 <- read.xlsx(file_name, sheet = "any_tenure_any_provider")
gross_LA_building_91_23 <- read.xlsx(file_name, sheet = "LA")
gross_HA_building_91_23 <- read.xlsx(file_name, sheet = "HA")
gross_S106nilgrant_building_91_23 <- read.xlsx(file_name, sheet = "s106_nogrant")
gross_grant_building_91_23 <- read.xlsx(file_name, sheet = "grant_any")

#sum all rows for each AH dataset
gross_public_building_91_23 <- gross_public_building_91_23 %>%
  select(where(is.numeric)) %>%                                # Keep numeric columns
  select(matches("^\\d{4}$")) %>%                              # Keep only 4-digit column names
  summarise(across(everything(), sum, na.rm = TRUE))           # Sum each column
gross_LA_building_91_23 <- gross_LA_building_91_23 %>%
  select(where(is.numeric)) %>%                                # Keep numeric columns
  select(matches("^\\d{4}$")) %>%                              # Keep only 4-digit column names
  summarise(across(everything(), sum, na.rm = TRUE))           # Sum each column
gross_HA_building_91_23 <- gross_HA_building_91_23 %>%
  select(where(is.numeric)) %>%                                # Keep numeric columns
  select(matches("^\\d{4}$")) %>%                              # Keep only 4-digit column names
  summarise(across(everything(), sum, na.rm = TRUE))           # Sum each column
gross_S106nilgrant_building_91_23 <- gross_S106nilgrant_building_91_23%>%
  select(where(is.numeric)) %>%                                # Keep numeric columns
  select(matches("^\\d{4}$")) %>%                              # Keep only 4-digit column names
  summarise(across(everything(), sum, na.rm = TRUE))           # Sum each column
gross_grant_building_91_23 <- gross_grant_building_91_23 %>%
  select(where(is.numeric)) %>%                                # Keep numeric columns
  select(matches("^\\d{4}$")) %>%                              # Keep only 4-digit column names
  summarise(across(everything(), sum, na.rm = TRUE))           # Sum each column

path = paste0(dirname, "/centre for Cities/Centre for Cities POC - Documents/Research/Housing/History of Planning 2/Data/Output/From R scripts/Housebuilding/", collapse = NULL)
setwd(path) 

# Construct the file name dynamically
file_name <- paste0(today, "FINAL_74_23_summarised_data_output.xlsx")

#bring in 1974-2023 data 
population_74_23 <- read.xlsx(file_name, sheet = "population_ENG")
stock_74_23 <- read.xlsx(file_name, sheet = "stocks_ENG")
gross_all_building_74_23 <- read.xlsx(file_name, sheet = "total_building_ENG")
gross_market_building_74_23 <- read.xlsx(file_name, sheet = "market_building_ENG")
gross_private_delivered_74_23 <- read.xlsx(file_name, sheet = "private_delivered_ENG")
gross_public_building_74_23 <- read.xlsx(file_name, sheet = "public_building_ENG")
gross_LA_building_74_23 <- read.xlsx(file_name, sheet = "LA_building_ENG")
gross_HA_building_74_23 <- read.xlsx(file_name, sheet = "HA_building_ENG")

#replace 91 data onward with T1011 data which includes rows without geography 
common_years <- intersect(
  names(gross_public_building_74_23),
  names(gross_public_building_91_23)
)
common_years <- common_years[grepl("^\\d{4}$", common_years)]

gross_public_building_74_23[common_years] <- gross_public_building_91_23[common_years]
gross_LA_building_74_23[common_years] <- gross_LA_building_91_23[common_years]
gross_HA_building_74_23[common_years] <- gross_HA_building_91_23[common_years]

file_name <- paste0(today, "FINAL_45_73_summarised_data_output.xlsx")

#bring in 1946-1973 data  
population_46_73 <- read.xlsx(file_name,  sheet = "population_ENG")
stock_45_73 <- read.xlsx(file_name,  sheet = "stocks_ENG")
gross_total_building_46_73 <- read.xlsx(file_name,  sheet = "gross_total_building_ENG")
gross_private_building_46_73 <- read.xlsx(file_name,  sheet = "gross_private_building_ENG")
gross_public_building_46_73 <- read.xlsx(file_name,  sheet = "gross_public_building_ENG")
gross_LA_building_46_73 <- read.xlsx(file_name, sheet = "gross_LA_building_ENG")
gross_HA_building_46_73 <- read.xlsx(file_name, sheet = "gross_HA_building_ENG")

# Function to add 'Country' column at the beginning
add_country_column <- function(df) {
  df <- cbind(Country = 'ENGLAND', df)
  return(df)
}

#Add 'Country' column to each dataframe
population_74_23 <- add_country_column(population_74_23)
stock_74_23 <- add_country_column(stock_74_23)
gross_all_building_74_23 <- add_country_column(gross_all_building_74_23)
gross_market_building_74_23 <- add_country_column(gross_market_building_74_23)
gross_private_delivered_74_23 <- add_country_column(gross_private_delivered_74_23)
gross_public_building_74_23 <- add_country_column(gross_public_building_74_23)
gross_LA_building_74_23 <- add_country_column(gross_LA_building_74_23)
gross_HA_building_74_23 <- add_country_column(gross_HA_building_74_23)
gross_S106nilgrant_building_91_23 <- add_country_column(gross_S106nilgrant_building_91_23)
gross_grant_building_91_23 <- add_country_column(gross_grant_building_91_23)

population_46_73 <- add_country_column(population_46_73)
stock_45_73 <- add_country_column(stock_45_73)
gross_total_building_46_73 <- add_country_column(gross_total_building_46_73)
gross_private_building_46_73 <- add_country_column(gross_private_building_46_73)
gross_public_building_46_73 <- add_country_column(gross_public_building_46_73)
gross_LA_building_46_73 <- add_country_column(gross_LA_building_46_73)
gross_HA_building_46_73 <- add_country_column(gross_HA_building_46_73)

#join them! 
population <- left_join(population_46_73, population_74_23, by = "Country") %>%
  rename_with(~ gsub("_POP", "", .), everything())

stock <- left_join(stock_45_73, stock_74_23, by = "Country") 

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

gross_total_built <- left_join(gross_total_building_46_73, gross_all_building_74_23, by = "Country") %>%
  rename_with(~ gsub("_COMPLETED_ALL", "", .), everything())

gross_market_built <- left_join(gross_private_building_46_73, gross_market_building_74_23, by = "Country") %>%
  rename_with(~ gsub("_COMPLETED_PRIVATE", "", .), everything())

gross_private_delivered <- left_join(gross_private_building_46_73, gross_private_delivered_74_23, by = "Country") %>%
  rename_with(~ gsub("_COMPLETED_PRIVATE", "", .), everything())

gross_public_built <- left_join(gross_public_building_46_73, gross_public_building_74_23, by = "Country") %>%
  rename_with(~ gsub("_COMPLETED_PUBLIC", "", .), everything())

gross_LA_built <- left_join(gross_LA_building_46_73, gross_LA_building_74_23, by = "Country") %>%
  rename_with(~ gsub("_COMPLETED_LOCAL", "", .), everything())

gross_HA_built <- left_join(gross_HA_building_46_73, gross_HA_building_74_23, by = "Country") %>%
  rename_with(~ gsub("_COMPLETED_HA", "", .), everything())

##### NOTE should add HA and LA to complete this

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

gross_market_built_long <- gross_market_built %>%
  pivot_longer(cols = -Country,          
               names_to = "YEAR",  
               values_to = "gross_market_built")  

gross_private_delivered_long <- gross_private_delivered %>%
  pivot_longer(cols = -Country,          
               names_to = "YEAR",  
               values_to = "gross_private_delivered") 

gross_public_built_long <- gross_public_built %>%
  pivot_longer(cols = -Country,          
               names_to = "YEAR",  
               values_to = "gross_public_built")  

gross_LA_built_long <- gross_LA_built %>%
  pivot_longer(cols = -Country,          
               names_to = "YEAR",  
               values_to = "gross_LA_built") 

gross_HA_built_long <- gross_HA_built %>%
  pivot_longer(cols = -Country,          
               names_to = "YEAR",  
               values_to = "gross_HA_built")  

gross_S106nilgrant_built_long <- gross_S106nilgrant_building_91_23 %>%
  pivot_longer(cols = -Country,          
               names_to = "YEAR",  
               values_to = "gross_S106nilgrant_building_91_23") 

gross_grant_built_long <- gross_grant_building_91_23 %>%
  pivot_longer(cols = -Country,          
               names_to = "YEAR",  
               values_to = "gross_grant_building_91_23") 

#join them into one dataframe 
ENG_data <- left_join(stock_long, population_long, by = "YEAR")
ENG_data <- left_join(ENG_data, gross_total_built_long, by = "YEAR")
ENG_data <- left_join(ENG_data, gross_market_built_long, by = "YEAR")
ENG_data <- left_join(ENG_data, gross_private_delivered_long, by = "YEAR")
ENG_data <- left_join(ENG_data, gross_public_built_long, by = "YEAR")
ENG_data <- left_join(ENG_data, gross_LA_built_long, by = "YEAR")
ENG_data <- left_join(ENG_data, gross_HA_built_long, by = "YEAR")
ENG_data <- left_join(ENG_data, gross_S106nilgrant_built_long, by = "YEAR")
ENG_data <- left_join(ENG_data, gross_grant_built_long, by = "YEAR")

ENG_data <- ENG_data[ , !grepl("Country", names(ENG_data))]

#export
#-------------------------------------------------------------------------
#England summarised data output 
date <- format(Sys.Date(), "%Y-%m-%d")
file_name_geog <- paste0(date, "FINAL_45_23_England_data_output", ".xlsx")
dir_path_geog = paste0(dirname, "/centre for Cities/Centre for Cities POC - Documents/Research/Housing/History of Planning 2/Data/Output/From R scripts/Housebuilding/")
file_path_geog <- file.path(dir_path_geog, file_name_geog)

# Create a list of data frames
df_list_geog <- list(ENG_data, population, stock, gross_total_built, gross_market_built, gross_private_delivered, gross_public_built, gross_LA_built, gross_HA_built, 
                     gross_S106nilgrant_building_91_23, gross_grant_building_91_23)
names(df_list_geog) <- c("England_data_table", "population", "stock", "gross_total_built", "gross_market_built", "gross_private_delivered", "gross_public_built", "gross_LA_built", "gross_HA_built", 
                         "gross_S106nilgrant_built", "gross_grant_built")                         

# Export data frames to Excel
export_to_excel(df_list_geog, file_path_geog)



