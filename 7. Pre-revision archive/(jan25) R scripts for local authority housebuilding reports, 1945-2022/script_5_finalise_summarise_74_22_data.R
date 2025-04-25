library(tidyverse)
library(readxl)
library(dplyr)
library(openxlsx) 
library(ggplot2)
library(tidyr)
library(purrr)


#excel write function 
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

### Getting the data 
#-----------------------------------------------------------------------------------------
#bring in flagging data
path <- paste0(dirname("~"),"/Centre for Cities/Centre for Cities POC - Documents/Research/Housing/History of Planning 2/Data/Input/", collapse = NULL)
setwd(path)

flags_2023 <- read.xlsx("2024-07-22_GB_Lookups.xlsx", sheet = "lad_23")
colnames(flags_2023)[1] <- "LA.Code_2023"


# Get today's date in the format "YYYY-MM-DD"
today <- format(Sys.Date(), "%Y-%m-%d")

path = paste0(dirname("~"),"/Centre for Cities/Centre for Cities POC - Documents/Research/Housing/History of Planning 2/Data/Output/From R scripts/Housebuilding/", collapse = NULL)
setwd(path) 

# Construct the file name dynamically
file_name <- paste0(today, "population_stock_housebuilding_1974_2022_2023geographies.xlsx")

population <- read.xlsx(file_name, sheet = "population_74_22")
population <- population[, !grepl("cty", colnames(population), ignore.case = TRUE)]
stock <- read.xlsx(file_name, sheet = "net_stock_74_22")
stock <- stock[, !grepl("cty", colnames(stock), ignore.case = TRUE)]
gross_all_building <- read.xlsx(file_name, sheet = "gross_all_74_22")
gross_all_building <- gross_all_building[, !grepl("cty", colnames(gross_all_building), ignore.case = TRUE)]
gross_LA_building <- read.xlsx(file_name, sheet = "gross_LA_74_22")
gross_LA_building <- gross_LA_building[, !grepl("cty", colnames(gross_LA_building), ignore.case = TRUE)]
gross_HA_building <- read.xlsx(file_name, sheet = "gross_HA_74_22")
gross_HA_building <- gross_HA_building[, !grepl("cty", colnames(gross_HA_building), ignore.case = TRUE)]
gross_private_building <- read.xlsx(file_name, sheet = "gross_private_74_22")
gross_private_building <- gross_private_building[, !grepl("cty", colnames(gross_private_building), ignore.case = TRUE)]
gross_public_building <- read.xlsx(file_name, sheet = "gross_public_74_22") 
gross_public_building <- gross_public_building[, !grepl("cty", colnames(gross_public_building), ignore.case = TRUE)]
total_demolished <- read.xlsx(file_name, sheet = "total_demolished_74_79") 
gross_affordable_not_social <- read.xlsx(file_name, sheet = "affordable_not_social_91_22") 
colnames(gross_affordable_not_social)[1] <- "LA.Code_2023"
gross_social <- read.xlsx(file_name, sheet = "social_91_22")

#adjust the 1970s stock so it is continuous with pre74 data
#-------------------------------------------------------------------------------------------
#bring in the 1970s adjustment factor 
# Read the value from the RDS file
file_name <- paste0(today, "stock_70s_adj_factor.rds")
adj_factor_70s <- readRDS(file_name)

#adjust the 1970s stock figures by the adjustment figure. This will smooth so it is exactly continuous with the pre1974 data in terms of total stock 
stock$`1973` <- stock$`1973`*((adj_factor_70s*8)+1)
stock$`1974` <- stock$`1974`*((adj_factor_70s*7)+1)
stock$`1975` <- stock$`1975`*((adj_factor_70s*6)+1)
stock$`1976` <- stock$`1976`*((adj_factor_70s*5)+1)
stock$`1977` <- stock$`1977`*((adj_factor_70s*4)+1)
stock$`1978` <- stock$`1978`*((adj_factor_70s*3)+1)
stock$`1979` <- stock$`1979`*((adj_factor_70s*2)+1)
stock$`1980` <- stock$`1980`*((adj_factor_70s*1)+1)


#join to flags 
#--------------------------------------------------------------------------------
population_joined <- population %>% 
  left_join(flags_2023, by = "LA.Code_2023") 

stock_joined <- stock %>% 
  left_join(flags_2023, by = "LA.Code_2023") 

gross_all_building_joined <- gross_all_building %>% 
  left_join(flags_2023, by = "LA.Code_2023") 

gross_private_building_joined <- gross_private_building %>% 
  left_join(flags_2023, by = "LA.Code_2023") 

gross_public_building_joined <- gross_public_building %>% 
  left_join(flags_2023, by = "LA.Code_2023") 

gross_LA_building_joined <- gross_LA_building %>% 
  left_join(flags_2023, by = "LA.Code_2023") 

gross_HA_building_joined <- gross_HA_building %>% 
  left_join(flags_2023, by = "LA.Code_2023")  

total_demolished_joined <- total_demolished %>% 
  left_join(flags_2023, by = "LA.Code_2023")  

gross_affordable_not_social_joined <- gross_affordable_not_social %>% 
  left_join(flags_2023, by = "LA.Code_2023") 

gross_social_joined <- gross_social %>% 
  left_join(flags_2023, by = "LA.Code_2023") 
  
#calculate housebuilding rates at LA level
#----------------------------------------------------------------------------  
#total
housebuilding_rate_LA <- stock_joined %>% 
  left_join(gross_all_building_joined, by = "LA.Code_2023") 

for (year in 1974:2022) {
  col_index <- year - 1910  # Calculate the column index dynamically (1974 corresponds to 64)
  ref_index <- year - 1972  # Calculate the reference index dynamically (1974 corresponds to 2)
  housebuilding_rate_LA[[paste0("GR", year)]] <- housebuilding_rate_LA[[col_index]] / housebuilding_rate_LA[[ref_index]]
}

housebuilding_rate_LA <- housebuilding_rate_LA[, c(1, grep("GR", colnames(housebuilding_rate_LA)))] 

#private
#total
private_housebuilding_rate_LA <- stock_joined %>% 
  left_join(gross_private_building_joined, by = "LA.Code_2023") 

# Loop from 1974 to 2022
for (year in 1974:2022) {
  col_index <- year - 1910  # Calculate the column index dynamically (1974 corresponds to 64)
  ref_index <- year - 1972  # Calculate the reference index dynamically (1974 corresponds to 2)
  private_housebuilding_rate_LA[[paste0("GR", year)]] <- private_housebuilding_rate_LA[[col_index]] / private_housebuilding_rate_LA[[ref_index]]
}

private_housebuilding_rate_LA <- private_housebuilding_rate_LA[, c(1, grep("GR", colnames(private_housebuilding_rate_LA)))] 

#public
public_housebuilding_rate_LA <- stock_joined %>% 
  left_join(gross_public_building_joined, by = "LA.Code_2023") 

# Loop from 1974 to 2022
for (year in 1974:2022) {
  col_index <- year - 1910  # Calculate the column index dynamically (1974 corresponds to 64)
  ref_index <- year - 1972  # Calculate the reference index dynamically (1974 corresponds to 2)
  public_housebuilding_rate_LA[[paste0("GR", year)]] <- public_housebuilding_rate_LA[[col_index]] / public_housebuilding_rate_LA[[ref_index]]
}

public_housebuilding_rate_LA <- public_housebuilding_rate_LA[, c(1, grep("GR", colnames(public_housebuilding_rate_LA)))] 

#local authority
LA_housebuilding_rate_LA <- stock_joined %>% 
  left_join(gross_LA_building_joined, by = "LA.Code_2023") 

# Loop from 1974 to 2022
for (year in 1974:2022) {
  col_index <- year - 1910  # Calculate the column index dynamically (1974 corresponds to 64)
  ref_index <- year - 1972  # Calculate the reference index dynamically (1974 corresponds to 2)
  LA_housebuilding_rate_LA[[paste0("GR", year)]] <- LA_housebuilding_rate_LA[[col_index]] / LA_housebuilding_rate_LA[[ref_index]]
}

LA_housebuilding_rate_LA <- LA_housebuilding_rate_LA[, c(1, grep("GR", colnames(LA_housebuilding_rate_LA)))] 

#housing association
HA_housebuilding_rate_LA <- stock_joined %>% 
  left_join(gross_HA_building_joined, by = "LA.Code_2023") 

# Loop from 1974 to 2022
for (year in 1974:2022) {
  col_index <- year - 1910  # Calculate the column index dynamically (1974 corresponds to 64)
  ref_index <- year - 1972  # Calculate the reference index dynamically (1974 corresponds to 2)
  HA_housebuilding_rate_LA[[paste0("GR", year)]] <- HA_housebuilding_rate_LA[[col_index]] / HA_housebuilding_rate_LA[[ref_index]]
}

HA_housebuilding_rate_LA <- HA_housebuilding_rate_LA[, c(1, grep("GR", colnames(HA_housebuilding_rate_LA)))] 

#demolition rates 
demolition_rate_LA <- stock_joined %>% 
  left_join(total_demolished_joined, by = "LA.Code_2023") 

# Loop from 1974 to 2022
for (year in 1974:1979) {
  col_index <- year - 1910  # Calculate the column index dynamically (1974 corresponds to 64)
  ref_index <- year - 1972  # Calculate the reference index dynamically (1974 corresponds to 2)
  demolition_rate_LA[[paste0("DR", year)]] <- demolition_rate_LA[[col_index]] / demolition_rate_LA[[ref_index]]
}

demolition_rate_LA <- demolition_rate_LA[, c(1, grep("DR", colnames(demolition_rate_LA)))] 

#affordable not social
affordable_not_social_housebuilding_rate_LA <- stock_joined %>% 
  left_join(gross_affordable_not_social_joined, by = "LA.Code_2023")  

# Create new columns GR1991 to GR2022
for (year in 1991:2022) {
  numerator_col <- 64 + (year - 1991) # Column indices for numerator (64 to 95)
  denominator_col <- 19 + (year - 1991) # Column indices for denominator (19 to 50)
  new_col_name <- paste0("GR", year) # New column name (GR1991 to GR2022)
  
  # Calculate the new column
  affordable_not_social_housebuilding_rate_LA[[new_col_name]] <- 
    affordable_not_social_housebuilding_rate_LA[[numerator_col]] / affordable_not_social_housebuilding_rate_LA[[denominator_col]]
}

affordable_not_social_housebuilding_rate_LA <- affordable_not_social_housebuilding_rate_LA[, c(1, grep("GR", colnames(affordable_not_social_housebuilding_rate_LA)))] 

#social rent 
social_housebuilding_rate_LA <- stock_joined %>% 
  left_join(gross_social_joined, by = "LA.Code_2023")  

# Create new columns GR1991 to GR2022
for (year in 1991:2022) {
  numerator_col <- 64 + (year - 1991) # Column indices for numerator (64 to 95)
  denominator_col <- 19 + (year - 1991) # Column indices for denominator (19 to 50)
  new_col_name <- paste0("GR", year) # New column name (GR1991 to GR2022)
  
  # Calculate the new column
  social_housebuilding_rate_LA[[new_col_name]] <- 
    social_housebuilding_rate_LA[[numerator_col]] / social_housebuilding_rate_LA[[denominator_col]]
}

social_housebuilding_rate_LA <- social_housebuilding_rate_LA[, c(1, grep("GR", colnames(social_housebuilding_rate_LA)))] 



#export a final version of the 1974-2022 data at district level 
# Generate file path for Excel workbook with date 01--------------------------------
date <- format(Sys.Date(), "%Y-%m-%d")
file_name_geog <- paste0(date, "FINAL_74_22_lad23_data_output", ".xlsx")
dir_path_geog = paste0(dirname("~"),"/Centre for Cities/Centre for Cities POC - Documents/Research/Housing/History of Planning 2/Data/Output/From R scripts/Housebuilding/")
file_path_geog <- file.path(dir_path_geog, file_name_geog)

# Create a list of data frames
df_list_geog <- list(population_joined, stock_joined, gross_all_building_joined, gross_private_building_joined, gross_public_building_joined, 
                     gross_LA_building_joined, gross_HA_building_joined, total_demolished_joined, gross_affordable_not_social_joined, gross_affordable_not_social_joined,
                     housebuilding_rate_LA, private_housebuilding_rate_LA, public_housebuilding_rate_LA, 
                     LA_housebuilding_rate_LA, HA_housebuilding_rate_LA, demolition_rate_LA, affordable_not_social_housebuilding_rate_LA, social_housebuilding_rate_LA)
names(df_list_geog) <- c("population", "stocks", "gross_all_building", "gross_private_building", "gross_public_building", 
                         "gross_LA_building", "gross_HA_building", "total_demolished", "affordable_not_social_building", "social_building",
                         "total_building_rate", "private_buliding_rate", "public_building_rate", 
                         "LA_building_rate", "HA_building_rate", "demolition_rate_74_79", "affordable_not_social_BR", "social_building_rate")   

# Export data frames to Excel
export_to_excel(df_list_geog, file_path_geog)



#summarise to higher geographies, including 1981 counties
#-----------------------------------------------------------------------------------
numeric_columns <- names(population_joined)[sapply(names(population_joined), function(x) grepl("^[0-9]+$", x))]
pop_cty_81 <- population_joined %>%
  group_by(cty_81) %>%
  summarize(across(all_of(numeric_columns), sum, na.rm = TRUE))  

pop_pua <- population_joined %>%
  group_by(pua) %>%
  summarize(across(where(is.numeric), sum, na.rm = TRUE))
pop_pua$GBshare <- pop_pua$gb_area_ha / pop_pua$area_ha 

numeric_columns <- names(stock_joined)[sapply(names(stock_joined), function(x) grepl("^[0-9]+$", x))]
stock_cty_81 <- stock_joined %>%
  group_by(cty_81) %>%
  summarize(across(all_of(numeric_columns), sum, na.rm = TRUE))

stock_pua <- stock_joined %>%
  group_by(pua) %>%
  summarize(across(where(is.numeric), sum, na.rm = TRUE))
stock_pua$GBshare <- stock_pua$gb_area_ha / stock_pua$area_ha 

numeric_columns <- names(gross_all_building_joined)[sapply(names(gross_all_building_joined), function(x) grepl("^[0-9]+$", x))]
built_cty_81 <- gross_all_building_joined %>%
  group_by(cty_81) %>%
  summarize(across(all_of(numeric_columns), sum, na.rm = TRUE))

built_pua <- gross_all_building_joined %>%
  group_by(pua) %>%
  summarize(across(where(is.numeric), sum, na.rm = TRUE))
built_pua$GBshare <- built_pua$gb_area_ha / built_pua$area_ha 

numeric_columns <- names(gross_LA_building_joined)[sapply(names(gross_LA_building_joined), function(x) grepl("^[0-9]+$", x))]
LAbuilt_cty_81 <- gross_LA_building_joined %>%
  group_by(cty_81) %>%
  summarize(across(all_of(numeric_columns), sum, na.rm = TRUE))

LAbuilt_pua <- gross_LA_building_joined %>%
  group_by(pua) %>%
  summarize(across(where(is.numeric), sum, na.rm = TRUE))
LAbuilt_pua$GBshare <- LAbuilt_pua$gb_area_ha / LAbuilt_pua$area_ha 

numeric_columns <- names(gross_HA_building_joined)[sapply(names(gross_HA_building_joined), function(x) grepl("^[0-9]+$", x))]
HAbuilt_cty_81 <- gross_HA_building_joined %>%
  group_by(cty_81) %>%
  summarize(across(all_of(numeric_columns), sum, na.rm = TRUE))

HAbuilt_pua <- gross_HA_building_joined %>%
  group_by(pua) %>%
  summarize(across(where(is.numeric), sum, na.rm = TRUE))
HAbuilt_pua$GBshare <- HAbuilt_pua$gb_area_ha / HAbuilt_pua$area_ha 

numeric_columns <- names(gross_private_building_joined)[sapply(names(gross_private_building_joined), function(x) grepl("^[0-9]+$", x))]
privatebuilt_cty_81 <- gross_private_building_joined %>%
  group_by(cty_81) %>%
  summarize(across(all_of(numeric_columns), sum, na.rm = TRUE))

privatebuilt_pua <- gross_private_building_joined %>%
  group_by(pua) %>%
  summarize(across(where(is.numeric), sum, na.rm = TRUE))
privatebuilt_pua$GBshare <- privatebuilt_pua$gb_area_ha / privatebuilt_pua$area_ha 

numeric_columns <- names(gross_public_building_joined)[sapply(names(gross_public_building_joined), function(x) grepl("^[0-9]+$", x))]
publicbuilt_cty_81 <- gross_public_building_joined %>%
  group_by(cty_81) %>%
  summarize(across(all_of(numeric_columns), sum, na.rm = TRUE))

publicbuilt_pua <- gross_public_building_joined %>%
  group_by(pua) %>%
  summarize(across(where(is.numeric), sum, na.rm = TRUE))
publicbuilt_pua$GBshare <- publicbuilt_pua$gb_area_ha / publicbuilt_pua$area_ha 

numeric_columns <- names(gross_affordable_not_social_joined)[sapply(names(gross_affordable_not_social_joined), function(x) grepl("^[0-9]+$", x))]
affordable_not_socialbuilt_cty_81 <- gross_affordable_not_social_joined %>%
  group_by(cty_81) %>%
  summarize(across(all_of(numeric_columns), sum, na.rm = TRUE))

affordable_not_socialbuilt_pua <- gross_affordable_not_social_joined %>%
  group_by(pua) %>%
  summarize(across(where(is.numeric), sum, na.rm = TRUE))
affordable_not_socialbuilt_pua$GBshare <- affordable_not_socialbuilt_pua$gb_area_ha / affordable_not_socialbuilt_pua$area_ha 

numeric_columns <- names(gross_social_joined)[sapply(names(gross_social_joined), function(x) grepl("^[0-9]+$", x))]
socialbuilt_cty_81 <- gross_social_joined %>%
  group_by(cty_81) %>%
  summarize(across(all_of(numeric_columns), sum, na.rm = TRUE))

socialbuilt_pua <- gross_social_joined %>%
  group_by(pua) %>%
  summarize(across(where(is.numeric), sum, na.rm = TRUE))
socialbuilt_pua$GBshare <- socialbuilt_pua$gb_area_ha / socialbuilt_pua$area_ha 

#summarise to England total 
#make whole country totals for each tenure by summing all rows 
population_ENG <- pop_cty_81 %>% 
  select(-cty_81) %>% 
  summarise(across(everything(), sum, na.rm = TRUE)) 

stocks_ENG <- stock_cty_81 %>% 
  select(-cty_81) %>% 
  summarise(across(everything(), sum, na.rm = TRUE)) 

gross_total_building_ENG <- built_cty_81 %>% 
  select(-cty_81) %>% 
  summarise(across(everything(), sum, na.rm = TRUE))

gross_public_building_ENG <- publicbuilt_cty_81 %>% 
  select(-cty_81) %>% 
  summarise(across(everything(), sum, na.rm = TRUE))

gross_private_building_ENG <- privatebuilt_cty_81 %>% 
  select(-cty_81) %>% 
  summarise(across(everything(), sum, na.rm = TRUE))


#get county-level building rates
#-------------------------------------------------------------
#total
housebuilding_rate_cty <- stock_cty_81 %>% 
  left_join(built_cty_81, by = "cty_81")

for (year in 1974:2022) {
  col_index <- year - 1923  # Calculate the column index dynamically (1974 corresponds to 52)
  ref_index <- year - 1972  # Calculate the reference index dynamically (1974 corresponds to 2)
  housebuilding_rate_cty[[paste0("GR", year)]] <- housebuilding_rate_cty[[col_index]] / housebuilding_rate_cty[[ref_index]]
}

housebuilding_rate_cty <- housebuilding_rate_cty[, c(1, grep("GR", colnames(housebuilding_rate_cty)))] 

#private
private_housebuilding_rate_cty <- stock_cty_81 %>% 
  left_join(privatebuilt_cty_81, by = "cty_81")

for (year in 1974:2022) {
  col_index <- year - 1923  # Calculate the column index dynamically (1974 corresponds to 52)
  ref_index <- year - 1972  # Calculate the reference index dynamically (1974 corresponds to 2)
  private_housebuilding_rate_cty[[paste0("GR", year)]] <- private_housebuilding_rate_cty[[col_index]] / private_housebuilding_rate_cty[[ref_index]]
}

private_housebuilding_rate_cty <- private_housebuilding_rate_cty[, c(1, grep("GR", colnames(private_housebuilding_rate_cty)))] 

#public
public_housebuilding_rate_cty <- stock_cty_81 %>% 
  left_join(publicbuilt_cty_81, by = "cty_81")

for (year in 1974:2022) {
  col_index <- year - 1923  # Calculate the column index dynamically (1974 corresponds to 52)
  ref_index <- year - 1972  # Calculate the reference index dynamically (1974 corresponds to 2)
  public_housebuilding_rate_cty[[paste0("GR", year)]] <- public_housebuilding_rate_cty[[col_index]] / public_housebuilding_rate_cty[[ref_index]]
}

public_housebuilding_rate_cty <- public_housebuilding_rate_cty[, c(1, grep("GR", colnames(public_housebuilding_rate_cty)))] 

#local authority
LA_housebuilding_rate_cty <- stock_cty_81 %>% 
  left_join(LAbuilt_cty_81, by = "cty_81")

for (year in 1974:2022) {
  col_index <- year - 1923  # Calculate the column index dynamically (1974 corresponds to 52)
  ref_index <- year - 1972  # Calculate the reference index dynamically (1974 corresponds to 2)
  LA_housebuilding_rate_cty[[paste0("GR", year)]] <- LA_housebuilding_rate_cty[[col_index]] / LA_housebuilding_rate_cty[[ref_index]]
}

LA_housebuilding_rate_cty <- LA_housebuilding_rate_cty[, c(1, grep("GR", colnames(LA_housebuilding_rate_cty)))] 

#housing association
HA_housebuilding_rate_cty <- stock_cty_81 %>% 
  left_join(HAbuilt_cty_81, by = "cty_81")

for (year in 1974:2022) {
  col_index <- year - 1923  # Calculate the column index dynamically (1974 corresponds to 52)
  ref_index <- year - 1972  # Calculate the reference index dynamically (1974 corresponds to 2)
  HA_housebuilding_rate_cty[[paste0("GR", year)]] <- HA_housebuilding_rate_cty[[col_index]] / HA_housebuilding_rate_cty[[ref_index]]
}

HA_housebuilding_rate_cty <- HA_housebuilding_rate_cty[, c(1, grep("GR", colnames(HA_housebuilding_rate_cty)))] 

#affordable_not_social
affordable_not_social_building_rate_cty <- stock_cty_81 %>% 
  left_join(affordable_not_socialbuilt_cty_81, by = "cty_81")

# Loop through years 1991 to 2022
for (year in 1991:2022) {
  numerator_col <- 51 + (year - 1991) # Column indices for numerator (51 to 82)
  denominator_col <- 19 + (year - 1991) # Column indices for denominator (19 to 50)
  new_col_name <- paste0("GR", year) # New column name (GR1991 to GR2022)
  
  # Calculate the new column
  affordable_not_social_building_rate_cty[[new_col_name]] <- 
    affordable_not_social_building_rate_cty[[numerator_col]] / 
    affordable_not_social_building_rate_cty[[denominator_col]]
}

affordable_not_social_building_rate_cty <- affordable_not_social_building_rate_cty[, c(1, grep("GR", colnames(affordable_not_social_building_rate_cty)))] 

#social rented
social_building_rate_cty <- stock_cty_81 %>% 
  left_join(socialbuilt_cty_81, by = "cty_81")

# Loop through years 1991 to 2022
for (year in 1991:2022) {
  numerator_col <- 51 + (year - 1991) # Column indices for numerator (51 to 82)
  denominator_col <- 19 + (year - 1991) # Column indices for denominator (19 to 50)
  new_col_name <- paste0("GR", year) # New column name (GR1991 to GR2022)
  
  # Calculate the new column
  social_building_rate_cty[[new_col_name]] <- 
    social_building_rate_cty[[numerator_col]] / 
    social_building_rate_cty[[denominator_col]]
}

social_building_rate_cty <- social_building_rate_cty[, c(1, grep("GR", colnames(social_building_rate_cty)))] 


#export a final version of the 1974-2022 data at county and PUA level
# Generate file path for Excel workbook with date 01--------------------------------
date <- format(Sys.Date(), "%Y-%m-%d")
file_name_geog <- paste0(date, "FINAL_74_22_summarised_data_output", ".xlsx")
dir_path_geog = paste0(dirname("~"),"/Centre for Cities/Centre for Cities POC - Documents/Research/Housing/History of Planning 2/Data/Output/From R scripts/Housebuilding/")
file_path_geog <- file.path(dir_path_geog, file_name_geog)

# Create a list of data frames
df_list_geog <- list(pop_cty_81, stock_cty_81, built_cty_81, privatebuilt_cty_81, publicbuilt_cty_81, LAbuilt_cty_81, HAbuilt_cty_81,  
                     affordable_not_socialbuilt_cty_81, socialbuilt_cty_81, 
                     housebuilding_rate_cty, private_housebuilding_rate_cty, public_housebuilding_rate_cty, LA_housebuilding_rate_cty, HA_housebuilding_rate_cty,
                     affordable_not_social_building_rate_cty, social_building_rate_cty,
                     pop_pua, stock_pua, built_pua, privatebuilt_pua, publicbuilt_pua, LAbuilt_pua, HAbuilt_pua, affordable_not_socialbuilt_pua, socialbuilt_pua,
                     population_ENG, stocks_ENG, gross_total_building_ENG, gross_private_building_ENG, gross_public_building_ENG)
names(df_list_geog) <- c("pop_cty_81", "stock_cty_81", "built_cty_81", "privatebuilt_cty_81", "publicbuilt_cty_81", "LAbuilt_cty_81", "HAbuilt_cty_81",  
                         "affordable_not_social_cty_81", "socialbuilt_cty_81", 
                         "total_building_rate_cty", "private_building_rate_cty", "public_building_rate_cty", "LA_building_rate_cty", "HA_building_rate_cty",
                         "affordable_not_social_BR_cty", "social_building_rate_cty",
                         "pop_pua", "stock_pua", "built_pua", "privatebuilt_pua", "publicbuilt_pua", "LAbuilt_pua", "HAbuilt_pua", "affordable_not_social_pua", "socialbuilt_pua",
                         "population_ENG", "stocks_ENG", "gross_total_building_ENG", "gross_private_building_ENG", "gross_public_building_ENG")   

# Export data frames to Excel
export_to_excel(df_list_geog, file_path_geog)









