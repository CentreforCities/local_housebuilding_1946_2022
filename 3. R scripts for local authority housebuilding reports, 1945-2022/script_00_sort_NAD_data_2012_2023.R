#Source1: Table 123: https://www.gov.uk/government/statistical-data-sets/live-tables-on-net-supply-of-housing

# Load packages -----------------------------------------------------------

#install.packages(-name-) if these aren't already installed
library(tidyverse)
library(readxl)
library(haven)
library(dplyr)
library(openxlsx)
library(ggplot2)

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

#set directory workaround
dirname <- sub("^(([^/]+/){2}[^/]+).*", "\\1", dirname("~"))

#bring in geographies 
path <- paste0(dirname, "/centre for Cities/Centre for Cities POC - Documents/Research/Core Data/Lookups/PUA and Small Geographies/", collapse = NULL)
setwd(path) 

geography <- read_xlsx(paste0(path, "2024-06_all_LAs_into_LA23_into_CfC_geogs.xlsx"), sheet = "LA") |> 
  rename("LA Code"="LA Code (original)")

#bring in net additional dwellings housing data
path <- paste0(dirname, "/centre for Cities/Centre for Cities POC - Documents/Research/Housing/History of Planning 2/Data/Input/Housebuilding Tables/", collapse = NULL)
setwd(path) 

#pull in raw sheets 
raw2012_13 <- read_xlsx(paste0(path, "LT_123_NADs_LA_components.xlsx"), sheet = "2012-13")
raw2013_14 <- read_xlsx(paste0(path, "LT_123_NADs_LA_components.xlsx"), sheet = "2013-14")
raw2014_15 <- read_xlsx(paste0(path, "LT_123_NADs_LA_components.xlsx"), sheet = "2014-15")
raw2015_16 <- read_xlsx(paste0(path, "LT_123_NADs_LA_components.xlsx"), sheet = "2015-16")
raw2016_17 <- read_xlsx(paste0(path, "LT_123_NADs_LA_components.xlsx"), sheet = "2016-17")
raw2017_18 <- read_xlsx(paste0(path, "LT_123_NADs_LA_components.xlsx"), sheet = "2017-18")
raw2018_19 <- read_xlsx(paste0(path, "LT_123_NADs_LA_components.xlsx"), sheet = "2018-19")
raw2019_20 <- read_xlsx(paste0(path, "LT_123_NADs_LA_components.xlsx"), sheet = "2019-20")
raw2020_21 <- read_xlsx(paste0(path, "LT_123_NADs_LA_components.xlsx"), sheet = "2020-21")
raw2021_22 <- read_xlsx(paste0(path, "LT_123_NADs_LA_components.xlsx"), sheet = "2021-22")
raw2022_23 <- read_xlsx(paste0(path, "LT_123_NADs_LA_components.xlsx"), sheet = "2022-23")
raw2023_24 <- read_xlsx(paste0(path, "LT_123_NADs_LA_components.xlsx"), sheet = "2023-24")

#process to select only new builds 
# Create a named list of your dataframes
dfs <- list(
  raw2012_13 = raw2012_13,
  raw2013_14 = raw2013_14,
  raw2014_15 = raw2014_15, 
  raw2015_16 = raw2015_16, 
  raw2016_17 = raw2016_17,
  raw2017_18 = raw2017_18,
  raw2018_19 = raw2018_19,
  raw2019_20 = raw2019_20,
  raw2020_21 = raw2020_21,
  raw2021_22 = raw2021_22,
  raw2022_23 = raw2022_23, 
  raw2023_24 = raw2023_24
)

# Apply transformations to each dataframe and rename them
for (name in names(dfs)) {
  df <- dfs[[name]]
  
  # Perform the transformations
  colnames(df) <- df[2, ]          # Make row 2 the column names
  df <- df[-c(1:2), ]              # Delete rows 1 to 2
  df <- df[, 3:5, drop = FALSE]    # Keep only columns 3 to 5
  rownames(df) <- NULL             # Reset row names
  
  # Rename the first column to "LA Code"
  colnames(df)[1] <- "LA Code"
  
  # Extract the suffix from the dataframe name (e.g., "2012_13" from "raw2012_13")
  suffix <- sub("^raw", "", name)
  
  # Rename the third column with the suffix
  colnames(df)[3] <- suffix
  
  # Create new variable names by replacing "raw" with "newbuild"
  new_name <- sub("^raw", "newbuild", name)
  
  # Assign the transformed dataframe to the new variable name
  assign(new_name, df)
}

#join all the newbuild data to geography, so can then collapse by 2023 LA Code. 
newbuild_dfs <- list(
  newbuild2012_13 = newbuild2012_13,
  newbuild2013_14 = newbuild2013_14,
  newbuild2014_15 = newbuild2014_15,
  newbuild2015_16 = newbuild2015_16, 
  newbuild2016_17 = newbuild2016_17,
  newbuild2017_18 = newbuild2017_18,
  newbuild2018_19 = newbuild2018_19,
  newbuild2019_20 = newbuild2019_20,
  newbuild2020_21 = newbuild2020_21,
  newbuild2021_22 = newbuild2021_22,
  newbuild2022_23 = newbuild2022_23, 
  newbuild2023_24 = newbuild2023_24
)

# Start with the geography dataframe
newbuild_geography_joined <- geography

# Loop through each "newbuild" dataframe and perform a left join
for (name in names(newbuild_dfs)) {
  df <- newbuild_dfs[[name]]
  
  # Extract the third column name dynamically
  third_col_name <- colnames(df)[3]
  
  # Select only "LA Code" and the third column for the join
  df_to_join <- df %>%
    select(`LA Code`, all_of(third_col_name))
  
  # Perform the left join
  newbuild_geography_joined  <- newbuild_geography_joined  %>%
    left_join(df_to_join, by = "LA Code")
}

#collapse by LA Code 2023 and keep only English data
newbuild_LAs2023 <- newbuild_geography_joined %>%
  filter(grepl("^E", `LA Code 2023`)) %>%  # Keep only rows where "LA Code 2023" starts with "E"
  group_by(`LA Code 2023`) %>%
  summarise(across(`2012_13`:`2023_24`, ~ sum(as.numeric(.), na.rm = TRUE))) %>%
  ungroup()

#convert to calendar years 
newbuild_calendar_LAs2023 <- newbuild_LAs2023
newbuild_calendar_LAs2023$`2012` <- newbuild_calendar_LAs2023$`2012_13`
newbuild_calendar_LAs2023$`2013` <- newbuild_calendar_LAs2023$`2012_13` * 0.25 + newbuild_calendar_LAs2023$`2013_14` * 0.75
newbuild_calendar_LAs2023$`2014` <- newbuild_calendar_LAs2023$`2013_14` * 0.25 + newbuild_calendar_LAs2023$`2014_15` * 0.75
newbuild_calendar_LAs2023$`2015` <- newbuild_calendar_LAs2023$`2014_15` * 0.25 + newbuild_calendar_LAs2023$`2015_16` * 0.75
newbuild_calendar_LAs2023$`2016` <- newbuild_calendar_LAs2023$`2015_16` * 0.25 + newbuild_calendar_LAs2023$`2016_17` * 0.75
newbuild_calendar_LAs2023$`2017` <- newbuild_calendar_LAs2023$`2016_17` * 0.25 + newbuild_calendar_LAs2023$`2017_18` * 0.75
newbuild_calendar_LAs2023$`2018` <- newbuild_calendar_LAs2023$`2017_18` * 0.25 + newbuild_calendar_LAs2023$`2018_19` * 0.75
newbuild_calendar_LAs2023$`2019` <- newbuild_calendar_LAs2023$`2018_19` * 0.25 + newbuild_calendar_LAs2023$`2019_20` * 0.75
newbuild_calendar_LAs2023$`2020` <- newbuild_calendar_LAs2023$`2019_20` * 0.25 + newbuild_calendar_LAs2023$`2020_21` * 0.75
newbuild_calendar_LAs2023$`2021` <- newbuild_calendar_LAs2023$`2020_21` * 0.25 + newbuild_calendar_LAs2023$`2021_22` * 0.75
newbuild_calendar_LAs2023$`2022` <- newbuild_calendar_LAs2023$`2021_22` * 0.25 + newbuild_calendar_LAs2023$`2022_23` * 0.75
newbuild_calendar_LAs2023$`2023` <- newbuild_calendar_LAs2023$`2022_23` * 0.25 + newbuild_calendar_LAs2023$`2023_24` * 0.75

newbuild_calendar_LAs2023 <- newbuild_calendar_LAs2023[, c(1,14:ncol(newbuild_calendar_LAs2023)) ,]   


### EXPORT to excel--------------------------------
# Generate file path for Excel workbook with date 02--------------------------------
date <- format(Sys.Date(), "%Y-%m-%d")
file_name_geog <- paste0(date, "newbuild_NAD_lad_2023", ".xlsx")
dir_path_geog = paste0(dirname,"/Centre for Cities/Centre for Cities POC - Documents/Research/Housing/History of Planning 2/Data/Output/From R scripts/Housebuilding/")
file_path_geog <- file.path(dir_path_geog, file_name_geog)

# Create a list of data frames
df_list_geog <- list(newbuild_calendar_LAs2023)
names(df_list_geog) <- c("newbuild_NAD")                             

# Export data frames to Excel
export_to_excel(df_list_geog, file_path_geog) 














