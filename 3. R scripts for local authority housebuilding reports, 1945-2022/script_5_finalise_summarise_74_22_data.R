library(tidyverse)
library(readxl)
library(dplyr)
library(openxlsx) 
library(ggplot2)
library(tidyr)
library(purrr)

#set directory workaround
dirname <- sub("^(([^/]+/){2}[^/]+).*", "\\1", dirname("~"))

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
path <- paste0(dirname, "/centre for Cities/Centre for Cities POC - Documents/Research/Housing/History of Planning 2/Data/Input/", collapse = NULL)
setwd(path)

flags_2023 <- read.xlsx("2024-07-22_GB_Lookups.xlsx", sheet = "lad_23")
colnames(flags_2023)[1] <- "LA.Code_2023"


# Get today's date in the format "YYYY-MM-DD"
today <- format(Sys.Date(), "%Y-%m-%d")

path = paste0(dirname, "/centre for Cities/Centre for Cities POC - Documents/Research/Housing/History of Planning 2/Data/Output/From R scripts/Housebuilding/", collapse = NULL)
setwd(path) 

# Construct the file name dynamically
file_name <- paste0(today, "population_stock_housebuilding_1974_2023_2023geographies.xlsx")

population <- read.xlsx(file_name, sheet = "population_74_22")
stock <- read.xlsx(file_name, sheet = "net_stock_74_23")
all_building <- read.xlsx(file_name, sheet = "gross_all_74_23")
LA_building <- read.xlsx(file_name, sheet = "gross_LA_74_23")
HA_building <- read.xlsx(file_name, sheet = "gross_HA_74_23")
market_building <- read.xlsx(file_name, sheet = "gross_market_74_23")
private_delivered <- read.xlsx(file_name, sheet = "gross_private_delivered_74_23")
public_building <- read.xlsx(file_name, sheet = "gross_public_74_23") 
S106_building <- read.xlsx(file_name, sheet = "S106_00_23") 
affordable_not_social_building <- read.xlsx(file_name, sheet = "affordable_not_social_91_23") 
social_building <- read.xlsx(file_name, sheet = "social_91_23") 
total_demolished <- read.xlsx(file_name, sheet = "total_demolished_74_79") 

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

all_building_joined <- all_building %>% 
  left_join(flags_2023, by = "LA.Code_2023") 

market_building_joined <- market_building %>% 
  left_join(flags_2023, by = "LA.Code_2023") 

private_delivered_joined <- private_delivered %>% 
  left_join(flags_2023, by = "LA.Code_2023") 

public_building_joined <- public_building %>% 
  left_join(flags_2023, by = "LA.Code_2023") 

LA_building_joined <- LA_building %>% 
  left_join(flags_2023, by = "LA.Code_2023") 

HA_building_joined <- HA_building %>% 
  left_join(flags_2023, by = "LA.Code_2023")  

S106_building_joined <- S106_building %>% 
  left_join(flags_2023, by = "LA.Code_2023")  

affordable_not_social_building_joined <- affordable_not_social_building  %>% 
  left_join(flags_2023, by = "LA.Code_2023")  

social_building_joined <- social_building %>% 
  left_join(flags_2023, by = "LA.Code_2023")  

total_demolished_joined <- total_demolished %>% 
  left_join(flags_2023, by = "LA.Code_2023") 
  
#calculate housebuilding rates at LA level
#----------------------------------------------------------------------------  
#total
total_housebuilding_rate <- stock_joined %>% 
  left_join(all_building_joined, by = "LA.Code_2023") 

for (year in 1974:2023) {
  col_index <- year - 1908  # Calculate the column index dynamically (1974 corresponds to 64)
  ref_index <- year - 1972  # Calculate the reference index dynamically (1974 corresponds to 2)
  total_housebuilding_rate[[paste0("GR", year)]] <- total_housebuilding_rate[[col_index]] / total_housebuilding_rate[[ref_index]]
}

total_housebuilding_rate <- total_housebuilding_rate[, c(1, grep("GR", colnames(total_housebuilding_rate)))] 

#market
market_housebuilding_rate <- stock_joined %>% 
  left_join(market_building_joined, by = "LA.Code_2023") 

# Loop from 1974 to 2022
for (year in 1974:2023) {
  col_index <- year - 1908  # Calculate the column index dynamically (1974 corresponds to 64)
  ref_index <- year - 1972  # Calculate the reference index dynamically (1974 corresponds to 2)
  market_housebuilding_rate[[paste0("GR", year)]] <- market_housebuilding_rate[[col_index]] / market_housebuilding_rate[[ref_index]]
}

market_housebuilding_rate <- market_housebuilding_rate[, c(1, grep("GR", colnames(market_housebuilding_rate)))] 

#private delivery
private_delivered_housebuilding_rate <- stock_joined %>%
  left_join(private_delivered_joined, by = "LA.Code_2023") 

# Loop from 1974 to 2022
for (year in 1974:2023) {
  col_index <- year - 1908  # Calculate the column index dynamically (1974 corresponds to 64)
  ref_index <- year - 1972  # Calculate the reference index dynamically (1974 corresponds to 2)
  private_delivered_housebuilding_rate[[paste0("GR", year)]] <- private_delivered_housebuilding_rate[[col_index]] / private_delivered_housebuilding_rate[[ref_index]]
}

private_delivered_housebuilding_rate <- private_delivered_housebuilding_rate[, c(1, grep("GR", colnames(private_delivered_housebuilding_rate)))] 

#public
public_housebuilding_rate <- stock_joined %>% 
  left_join(public_building_joined, by = "LA.Code_2023") 

# Loop from 1974 to 2022
for (year in 1974:2023) {
  col_index <- year - 1908  # Calculate the column index dynamically (1974 corresponds to 64)
  ref_index <- year - 1972  # Calculate the reference index dynamically (1974 corresponds to 2)
  public_housebuilding_rate[[paste0("GR", year)]] <- public_housebuilding_rate[[col_index]] / public_housebuilding_rate[[ref_index]]
}

public_housebuilding_rate <- public_housebuilding_rate[, c(1, grep("GR", colnames(public_housebuilding_rate)))] 

#local authority
LA_housebuilding_rate <- stock_joined %>% 
  left_join(LA_building_joined, by = "LA.Code_2023") 

# Loop from 1974 to 2022
for (year in 1974:2023) {
  col_index <- year - 1908  # Calculate the column index dynamically (1974 corresponds to 64)
  ref_index <- year - 1972  # Calculate the reference index dynamically (1974 corresponds to 2)
  LA_housebuilding_rate[[paste0("GR", year)]] <- LA_housebuilding_rate[[col_index]] / LA_housebuilding_rate[[ref_index]]
}

LA_housebuilding_rate <- LA_housebuilding_rate[, c(1, grep("GR", colnames(LA_housebuilding_rate)))] 

#housing association
HA_housebuilding_rate <- stock_joined %>% 
  left_join(HA_building_joined, by = "LA.Code_2023") 

# Loop from 1974 to 2022
for (year in 1974:2023) {
  col_index <- year - 1908  # Calculate the column index dynamically (1974 corresponds to 64)
  ref_index <- year - 1972  # Calculate the reference index dynamically (1974 corresponds to 2)
  HA_housebuilding_rate[[paste0("GR", year)]] <- HA_housebuilding_rate[[col_index]] / HA_housebuilding_rate[[ref_index]]
}

HA_housebuilding_rate <- HA_housebuilding_rate[, c(1, grep("GR", colnames(HA_housebuilding_rate)))] 

#S106
S106_housebuilding_rate <- stock_joined %>% 
  left_join(S106_building_joined, by = "LA.Code_2023") 

# Loop from 1974 to 2022
for (year in 2000:2023) {
  col_index <- year - 1934  # Calculate the column index dynamically (1974 corresponds to 64)
  ref_index <- year - 1998  # Calculate the reference index dynamically (1974 corresponds to 2)
  S106_housebuilding_rate[[paste0("GR", year)]] <- S106_housebuilding_rate[[col_index]] / S106_housebuilding_rate[[ref_index]]
}

S106_housebuilding_rate <- S106_housebuilding_rate[, c(1, grep("GR", colnames(S106_housebuilding_rate)))] 

#affordable_not_social
affordable_not_social_housebuilding_rate <- stock_joined %>% 
  left_join(affordable_not_social_building_joined, by = "LA.Code_2023") 

# Loop from 1974 to 2022
for (year in 1991:2023) {
  col_index <- year - 1925  # Calculate the column index dynamically (1974 corresponds to 64)
  ref_index <- year - 1989  # Calculate the reference index dynamically (1974 corresponds to 2)
  affordable_not_social_housebuilding_rate[[paste0("GR", year)]] <- affordable_not_social_housebuilding_rate[[col_index]] / affordable_not_social_housebuilding_rate[[ref_index]]
}

affordable_not_social_housebuilding_rate <- affordable_not_social_housebuilding_rate[, c(1, grep("GR", colnames(affordable_not_social_housebuilding_rate)))] 

#social
social_housebuilding_rate <- stock_joined %>% 
  left_join(social_building_joined, by = "LA.Code_2023") 

# Loop from 1974 to 2022
for (year in 1991:2023) {
  col_index <- year - 1925  # Calculate the column index dynamically (1974 corresponds to 64)
  ref_index <- year - 1989  # Calculate the reference index dynamically (1974 corresponds to 2)
  social_housebuilding_rate[[paste0("GR", year)]] <- social_housebuilding_rate[[col_index]] / social_housebuilding_rate[[ref_index]]
}

social_housebuilding_rate <- social_housebuilding_rate[, c(1, grep("GR", colnames(social_housebuilding_rate)))] 

#demolition rates 
demolition_rate <- stock_joined %>% 
  left_join(total_demolished_joined, by = "LA.Code_2023") 

# Loop from 1974 to 2022
for (year in 1974:1979) {
  col_index <- year - 1908  # Calculate the column index dynamically (1974 corresponds to 64)
  ref_index <- year - 1972  # Calculate the reference index dynamically (1974 corresponds to 2)
  demolition_rate[[paste0("DR", year)]] <- demolition_rate[[col_index]] / demolition_rate[[ref_index]]
}

demolition_rate <- demolition_rate[, c(1, grep("DR", colnames(demolition_rate)))] 


#export a final version of the 1974-2022 data at district level 
# Generate file path for Excel workbook with date 01--------------------------------
date <- format(Sys.Date(), "%Y-%m-%d")
file_name_geog <- paste0(date, "FINAL_74_23_lad23_data_output", ".xlsx")
dir_path_geog = paste0(dirname, "/centre for Cities/Centre for Cities POC - Documents/Research/Housing/History of Planning 2/Data/Output/From R scripts/Housebuilding/")
file_path_geog <- file.path(dir_path_geog, file_name_geog)

# Create a list of data frames
df_list_geog <- list(population_joined, stock_joined, all_building_joined, market_building_joined, public_building_joined, 
                     LA_building_joined, HA_building_joined, private_delivered_joined, S106_building_joined, affordable_not_social_building_joined, social_building_joined, 
                     total_demolished_joined, 
                     total_housebuilding_rate, market_housebuilding_rate, public_housebuilding_rate, 
                     LA_housebuilding_rate, HA_housebuilding_rate, private_delivered_housebuilding_rate, S106_housebuilding_rate, affordable_not_social_housebuilding_rate, social_housebuilding_rate,
                     demolition_rate)
names(df_list_geog) <- c("population", "stocks", "all_new_building", "market_rate_building", "public_building", 
                         "LA_building", "HA_building", "private_delivered", "S106_delivered", "affordable_not_social", "social_rented", 
                         "total_demolished", 
                         "total_building_rate", "market_buliding_rate", "public_building_rate", 
                         "LA_building_rate", "HA_building_rate", "private_delivered_building_rate", "S106_building_rate", "affordable_NS_building_rate", "social_building_rate",
                         "demolition_rate_74_79")   

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


numeric_columns <- names(all_building_joined)[sapply(names(all_building_joined), function(x) grepl("^[0-9]+$", x))]
built_cty_81 <- all_building_joined %>%
  group_by(cty_81) %>%
  summarize(across(all_of(numeric_columns), sum, na.rm = TRUE))

built_pua <- all_building_joined %>%
  group_by(pua) %>%
  summarize(across(where(is.numeric), sum, na.rm = TRUE))
built_pua$GBshare <- built_pua$gb_area_ha / built_pua$area_ha 


numeric_columns <- names(LA_building_joined)[sapply(names(LA_building_joined), function(x) grepl("^[0-9]+$", x))]
LAbuilt_cty_81 <- LA_building_joined %>%
  group_by(cty_81) %>%
  summarize(across(all_of(numeric_columns), sum, na.rm = TRUE))

LAbuilt_pua <- LA_building_joined %>%
  group_by(pua) %>%
  summarize(across(where(is.numeric), sum, na.rm = TRUE))
LAbuilt_pua$GBshare <- LAbuilt_pua$gb_area_ha / LAbuilt_pua$area_ha 


numeric_columns <- names(HA_building_joined)[sapply(names(HA_building_joined), function(x) grepl("^[0-9]+$", x))]
HAbuilt_cty_81 <- HA_building_joined %>%
  group_by(cty_81) %>%
  summarize(across(all_of(numeric_columns), sum, na.rm = TRUE))

HAbuilt_pua <- HA_building_joined %>%
  group_by(pua) %>%
  summarize(across(where(is.numeric), sum, na.rm = TRUE))
HAbuilt_pua$GBshare <- HAbuilt_pua$gb_area_ha / HAbuilt_pua$area_ha 


numeric_columns <- names(market_building_joined)[sapply(names(market_building_joined), function(x) grepl("^[0-9]+$", x))]
marketbuilt_cty_81 <- market_building_joined %>%
  group_by(cty_81) %>%
  summarize(across(all_of(numeric_columns), sum, na.rm = TRUE))

marketbuilt_pua <- market_building_joined %>%
  group_by(pua) %>%
  summarize(across(where(is.numeric), sum, na.rm = TRUE))
marketbuilt_pua$GBshare <- marketbuilt_pua$gb_area_ha / marketbuilt_pua$area_ha 


numeric_columns <- names(private_delivered_joined)[sapply(names(private_delivered_joined), function(x) grepl("^[0-9]+$", x))]
private_delivered_cty_81 <- private_delivered_joined %>%
  group_by(cty_81) %>%
  summarize(across(all_of(numeric_columns), sum, na.rm = TRUE))

private_delivered_pua <- private_delivered_joined %>%
  group_by(pua) %>%
  summarize(across(where(is.numeric), sum, na.rm = TRUE))
private_delivered_pua$GBshare <- private_delivered_pua$gb_area_ha / private_delivered_pua$area_ha 


numeric_columns <- names(public_building_joined)[sapply(names(public_building_joined), function(x) grepl("^[0-9]+$", x))]
publicbuilt_cty_81 <- public_building_joined %>%
  group_by(cty_81) %>%
  summarize(across(all_of(numeric_columns), sum, na.rm = TRUE))

publicbuilt_pua <- public_building_joined %>%
  group_by(pua) %>%
  summarize(across(where(is.numeric), sum, na.rm = TRUE))
publicbuilt_pua$GBshare <- publicbuilt_pua$gb_area_ha / publicbuilt_pua$area_ha 


#summarise to England total 
#make whole country totals for each tenure by summing all rows 
population_ENG <- pop_cty_81 %>% 
  select(-cty_81) %>% 
  summarise(across(everything(), sum, na.rm = TRUE)) 

stocks_ENG <- stock_cty_81 %>% 
  select(-cty_81) %>% 
  summarise(across(everything(), sum, na.rm = TRUE)) 

total_building_ENG <- built_cty_81 %>% 
  select(-cty_81) %>% 
  summarise(across(everything(), sum, na.rm = TRUE))

public_building_ENG <- publicbuilt_cty_81 %>% 
  select(-cty_81) %>% 
  summarise(across(everything(), sum, na.rm = TRUE))

LA_building_ENG <- LAbuilt_cty_81 %>% 
  select(-cty_81) %>% 
  summarise(across(everything(), sum, na.rm = TRUE))

HA_building_ENG <- HAbuilt_cty_81 %>% 
  select(-cty_81) %>% 
  summarise(across(everything(), sum, na.rm = TRUE))

market_building_ENG <-  marketbuilt_cty_81 %>% 
  select(-cty_81) %>% 
  summarise(across(everything(), sum, na.rm = TRUE))

private_delivered_building_ENG <-  private_delivered_cty_81 %>% 
  select(-cty_81) %>% 
  summarise(across(everything(), sum, na.rm = TRUE))


#get county-level building rates
#-------------------------------------------------------------
#total
housebuilding_rate_cty <- stock_cty_81 %>% 
  left_join(built_cty_81, by = "cty_81")

for (year in 1974:2023) {
  col_index <- year - 1921  # Calculate the column index dynamically (1974 corresponds to 52)
  ref_index <- year - 1972  # Calculate the reference index dynamically (1974 corresponds to 2)
  housebuilding_rate_cty[[paste0("GR", year)]] <- housebuilding_rate_cty[[col_index]] / housebuilding_rate_cty[[ref_index]]
}

housebuilding_rate_cty <- housebuilding_rate_cty[, c(1, grep("GR", colnames(housebuilding_rate_cty)))] 

#market
market_housebuilding_rate_cty <- stock_cty_81 %>% 
  left_join(marketbuilt_cty_81, by = "cty_81")

for (year in 1974:2023) {
  col_index <- year - 1921  # Calculate the column index dynamically (1974 corresponds to 52)
  ref_index <- year - 1972  # Calculate the reference index dynamically (1974 corresponds to 2)
  market_housebuilding_rate_cty[[paste0("GR", year)]] <- market_housebuilding_rate_cty[[col_index]] / market_housebuilding_rate_cty[[ref_index]]
}

market_housebuilding_rate_cty <- market_housebuilding_rate_cty[, c(1, grep("GR", colnames(market_housebuilding_rate_cty)))] 

#private delivered 
private_delivered_rate_cty <- stock_cty_81 %>% 
  left_join(private_delivered_cty_81, by = "cty_81")

for (year in 1974:2023) {
  col_index <- year - 1921  # Calculate the column index dynamically (1974 corresponds to 52)
  ref_index <- year - 1972  # Calculate the reference index dynamically (1974 corresponds to 2)
  private_delivered_rate_cty[[paste0("GR", year)]] <- private_delivered_rate_cty[[col_index]] / private_delivered_rate_cty[[ref_index]]
}

private_delivered_rate_cty <- private_delivered_rate_cty[, c(1, grep("GR", colnames(private_delivered_rate_cty)))]

#public
public_housebuilding_rate_cty <- stock_cty_81 %>% 
  left_join(publicbuilt_cty_81, by = "cty_81")

for (year in 1974:2023) {
  col_index <- year - 1921  # Calculate the column index dynamically (1974 corresponds to 52)
  ref_index <- year - 1972  # Calculate the reference index dynamically (1974 corresponds to 2)
  public_housebuilding_rate_cty[[paste0("GR", year)]] <- public_housebuilding_rate_cty[[col_index]] / public_housebuilding_rate_cty[[ref_index]]
}

public_housebuilding_rate_cty <- public_housebuilding_rate_cty[, c(1, grep("GR", colnames(public_housebuilding_rate_cty)))] 

#local authority
LA_housebuilding_rate_cty <- stock_cty_81 %>% 
  left_join(LAbuilt_cty_81, by = "cty_81")

for (year in 1974:2023) {
  col_index <- year - 1921  # Calculate the column index dynamically (1974 corresponds to 52)
  ref_index <- year - 1972  # Calculate the reference index dynamically (1974 corresponds to 2)
  LA_housebuilding_rate_cty[[paste0("GR", year)]] <- LA_housebuilding_rate_cty[[col_index]] / LA_housebuilding_rate_cty[[ref_index]]
}

LA_housebuilding_rate_cty <- LA_housebuilding_rate_cty[, c(1, grep("GR", colnames(LA_housebuilding_rate_cty)))] 

#housing association
HA_housebuilding_rate_cty <- stock_cty_81 %>% 
  left_join(HAbuilt_cty_81, by = "cty_81")

for (year in 1974:2023) {
  col_index <- year - 1921  # Calculate the column index dynamically (1974 corresponds to 52)
  ref_index <- year - 1972  # Calculate the reference index dynamically (1974 corresponds to 2)
  HA_housebuilding_rate_cty[[paste0("GR", year)]] <- HA_housebuilding_rate_cty[[col_index]] / HA_housebuilding_rate_cty[[ref_index]]
}

HA_housebuilding_rate_cty <- HA_housebuilding_rate_cty[, c(1, grep("GR", colnames(HA_housebuilding_rate_cty)))] 


#export a final version of the 1974-2022 data at county and PUA level
# Generate file path for Excel workbook with date 01--------------------------------
date <- format(Sys.Date(), "%Y-%m-%d")
file_name_geog <- paste0(date, "FINAL_74_23_summarised_data_output", ".xlsx")
dir_path_geog = paste0(dirname, "/centre for Cities/Centre for Cities POC - Documents/Research/Housing/History of Planning 2/Data/Output/From R scripts/Housebuilding/")
file_path_geog <- file.path(dir_path_geog, file_name_geog)

# Create a list of data frames
df_list_geog <- list(pop_cty_81, stock_cty_81, built_cty_81, marketbuilt_cty_81, private_delivered_cty_81, publicbuilt_cty_81, LAbuilt_cty_81, HAbuilt_cty_81, 
                     housebuilding_rate_cty, market_housebuilding_rate_cty, private_delivered_rate_cty, public_housebuilding_rate_cty,
                     LA_housebuilding_rate_cty, HA_housebuilding_rate_cty,
                     pop_pua, stock_pua, built_pua, marketbuilt_pua, private_delivered_pua, publicbuilt_pua, LAbuilt_pua, HAbuilt_pua,
                     population_ENG, stocks_ENG, total_building_ENG, market_building_ENG, private_delivered_building_ENG, public_building_ENG, LA_building_ENG, HA_building_ENG)
names(df_list_geog) <- c("pop_cty_81", "stock_cty_81", "built_cty_81", "marketbuilt_cty_81", "private_delivered_cty_81", "publicbuilt_cty_81", "LAbuilt_cty_81", "HAbuilt_cty_81", 
                         "total_building_rate_cty", "market_building_rate_cty", "private_delivered_rate_cty", "public_building_rate_cty", 
                         "LA_building_rate_cty", "HA_building_rate_cty",
                         "pop_pua", "stock_pua", "built_pua", "marketbuilt_pua", "private_delivered_pua", "publicbuilt_pua", "LAbuilt_pua", "HAbuilt_pua",
                         "population_ENG", "stocks_ENG", "total_building_ENG", "market_building_ENG", "private_delivered_ENG", "public_building_ENG", "LA_building_ENG", "HA_building_ENG")   

# Export data frames to Excel
export_to_excel(df_list_geog, file_path_geog)









