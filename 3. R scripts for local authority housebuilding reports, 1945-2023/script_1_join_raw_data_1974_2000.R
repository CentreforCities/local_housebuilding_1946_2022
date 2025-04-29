library(tidyverse)
library(readxl)
library(dplyr)
library(openxlsx) 
library(ggplot2)
library(tidyr)
library(purrr)

#set directory workaround
dirname <- sub("^(([^/]+/){2}[^/]+).*", "\\1", dirname("~"))

#house price data source data path
path = paste0(dirname, "/centre for Cities/Centre for Cities POC - Documents/Research/Housing/History of Planning 2/Data/Input/Housing Returns/Phase 2 - geography joined/", collapse = NULL)
setwd(path) 

#import basic geography lookup for 1974 to 2001  
geog_names <- read.xlsx("T125_2001_names_lookup.xlsx") 
colnames(geog_names) <- c("Area_2001", "Area_name")

#Importing and knitting the data sets
#---------------------------------------------------------------------
#1980 onwards (when all NT figures are additional to LA figures - so can just sum; note NT figures don't include private building on 
#private land inside NT areas - that building is automatically in LA building figures)
# list of years - write out all those you want to import and join
years <- c(1980, 1981, 1982, 1983, 1984, 1985, 1986, 1987, 1988, 1989, 1990, 1991, 
           1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999, 2000)

import_and_clean_data <- function(years) {
  all_data <- list()
  
  for (year in years) {
    # Import raw data for the specified year
    raw_data <- read.xlsx(paste0(year, " - phase 2 joined.xlsx"))
    
    # Add year in front of column titles
    colnames(raw_data)[9:ncol(raw_data)] <- paste0(year, "_", colnames(raw_data)[9:ncol(raw_data)])
    
    # Remove rows without 2001 Area Codes
    clean_data <- raw_data[!is.na(raw_data$Area_2001) & raw_data$Area_2001 != 0, ] 
    
    # Convert columns 9 onward to numeric
    clean_data <- clean_data %>%
      mutate(across(9:ncol(clean_data), ~ as.numeric(as.character(.))))
    
    # Summarize data by Area_2001
    collapsed_LA <- clean_data %>%
      group_by(Area_2001) %>%
      summarise(across(
        grep(paste0("^", year), colnames(clean_data), value = TRUE), 
        sum, na.rm = TRUE)
      )
    
    # Filter data for "New Town" construction
    new_towns_data <- clean_data %>%
      filter(Labelled.New.Town.Construction == "New Town") 
    
    # Store cleaned and summarized data for the year
    all_data[[paste0("data_", year)]] <- list(collapsed_LA = collapsed_LA, new_towns_data = new_towns_data)
  }
  
  return(all_data)
}

# Import and clean data for all years in the list
all_years_data <- import_and_clean_data(years)

# Access cleaned and summarized data for each year
for (year in years) {
  # Access data for the current year
  current_data <- all_years_data[[paste0("data_", year)]]
  
  # Extract cleaned and summarized data
  assign(paste0("collapsed_LA_", year), current_data$collapsed_LA)
  assign(paste0("new_towns_", year), current_data$new_towns_data)
}


#for 1974-1979 (when public housebuilding in NTs needs to be added to LA, but private housebuilding in NTs is already 
#in LA figures) 
# Define a vector of years
years70s <- c(1974, 1975, 1976, 1977, 1978, 1979)  # Add any additional years you want to process

# Create lists to store dataframes for each year
collapsed_LA_list <- list()
new_towns_data_list <- list()

# Loop over each year
for (year in years70s) {
  # Import raw data for the specified year
  raw_data <- read.xlsx(paste0(year, " - phase 2 joined.xlsx"))
  
  # Add year in front of column titles
  colnames(raw_data)[9:ncol(raw_data)] <- paste0(year, "_", colnames(raw_data)[9:ncol(raw_data)])
  
  # Remove rows without 2001 Area Codes
  clean_data <- raw_data[!is.na(raw_data$Area_2001) & raw_data$Area_2001 != 0, ] 
  
  # Convert columns 9 onward to numeric
  clean_data <- clean_data %>%
    mutate(across(9:ncol(clean_data), ~ as.numeric(as.character(.))))
  
  clean_data <- clean_data %>%
    select(-contains("APPROVED"))
  
  # Replace NA values in the column Labelled.New.Town.Construction with 'LA'
  clean_data$Labelled.New.Town.Construction[is.na(clean_data$Labelled.New.Town.Construction)] <- 'LA'
  
  # Replace all NA values with 0 so things add up properly 
  clean_data <- clean_data %>%
    mutate(across(where(is.numeric), ~replace_na(., 0))) %>%
    mutate(across(where(is.character), ~replace_na(., "NA")))
  
  # Identify 'New Town' rows
  new_towns_data <- clean_data %>% filter(Labelled.New.Town.Construction == 'New Town')
  
  # Identify columns that end in '_LOCAL'
  local_columns <- grep("_LOCAL$", colnames(clean_data), value = TRUE) 
  
  # Identify columns that end in '_ALL'
  all_columns <- grep("_ALL$", colnames(clean_data), value = TRUE) 
  
  # Create a copy of the dataframe to hold the updated values
  publicNT_added <- clean_data
  
  # Iterate over each 'New Town' row and update corresponding non-'New Town' rows
  for (i in 1:nrow(new_towns_data)) {
    new_town_row <- new_towns_data[i, ]
    area <- new_town_row$Area_2001
    
    # Find matching rows based on 'Area_2001' value
    matching_rows_index <- which(publicNT_added$Area_2001 == area & publicNT_added$Labelled.New.Town.Construction != 'New Town')
    
    # Add the '_LOCAL' values from the 'New Town' row to the next '_ALL' and '_LOCAL' values in the matching rows
    for (j in 1:length(local_columns)) {
      local_col <- local_columns[j]
      all_col <- all_columns[j]
      publicNT_added[matching_rows_index, all_col] <- publicNT_added[matching_rows_index, all_col] + new_town_row[[local_col]]
      publicNT_added[matching_rows_index, local_col] <- publicNT_added[matching_rows_index, local_col] + new_town_row[[local_col]]
    }
  } 
  
  # Add a new column that sums values from columns containing 'DEMOLISHED'
  demolished_cols <- grep("DEMOLISHED", colnames(publicNT_added), value = TRUE)
  col_name_total_demolished <- paste0(year, "_Total_demolished")
  publicNT_added[[col_name_total_demolished]] <- rowSums(publicNT_added[, demolished_cols], na.rm = TRUE)
  
  # Add a new column 'Net_completed' which is COMPLETED_ALL - Total_Demolished
  completed_cols <- grep("COMPLETED_ALL$", colnames(publicNT_added), value = TRUE)
  if (length(completed_cols) > 0) {
    col_name_net_completed <- paste0(year, "_Net_completed")
    publicNT_added[[col_name_net_completed]] <- rowSums(publicNT_added[, completed_cols, drop = FALSE], na.rm = TRUE) - publicNT_added[[col_name_total_demolished]]
  } else {
    col_name_net_completed <- paste0(year, "_Net_completed")
    publicNT_added[[col_name_net_completed]] <- rep(NA, nrow(publicNT_added))  # If no COMPLETED_ALL columns found
  }
  
  merged_LA <- publicNT_added %>%
    filter(Labelled.New.Town.Construction != "New Town")
  
  collapsed_LA <- merged_LA %>%
    group_by(Area_2001) %>%
    summarise_if(is.numeric, sum)
  
  # Assign dataframes to variables in the global environment
  assign(paste0("collapsed_LA_", year), collapsed_LA)
  assign(paste0("new_towns_", year), new_towns_data)
}


#Joining the data sets 
#------------------------------------------------------------------------

#re-set which years you want to draw into the mastersheet

years <- c(1974, 1975, 1976, 1977, 1978, 1979, 1980, 1981, 1982, 1983, 1984, 1985,
           1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994, 1995, 1996, 1997, 
           1998, 1999, 2000)

#join data from all years to create local authority mastersheet
# Filter the collapsed_clean data frames for the years you care about
collapsed_clean_years <- mget(paste0("collapsed_LA_", years))

# Join all collapsed_clean data frames by 'Area_2001'
mastersheet <- reduce(collapsed_clean_years, left_join, by = 'Area_2001') 

# Join geog_names to mastersheet by 'Area_2001'
mastersheet <- left_join(mastersheet, geog_names, by = "Area_2001")

#make financial year adjustment for all data (no need for new towns because data stops before 1996 switch to FYs)
mastersheet$`1996_COMPLETED_ALL_CY` <- (mastersheet$`1995_COMPLETED_ALL` * 0.25) + (mastersheet$`1996_COMPLETED_ALL` * 0.75)
mastersheet$`1997_COMPLETED_ALL_CY` <- (mastersheet$`1996_COMPLETED_ALL` * 0.25) + (mastersheet$`1997_COMPLETED_ALL` * 0.75)
mastersheet$`1998_COMPLETED_ALL_CY` <- (mastersheet$`1997_COMPLETED_ALL` * 0.25) + (mastersheet$`1998_COMPLETED_ALL` * 0.75)
mastersheet$`1999_COMPLETED_ALL_CY` <- (mastersheet$`1998_COMPLETED_ALL` * 0.25) + (mastersheet$`1999_COMPLETED_ALL` * 0.75)
mastersheet$`2000_COMPLETED_ALL_CY` <- (mastersheet$`1999_COMPLETED_ALL` * 0.25) + (mastersheet$`2000_COMPLETED_ALL` * 0.75) 

mastersheet$`1996_COMPLETED_ALL` <- mastersheet$`1996_COMPLETED_ALL_CY` 
mastersheet$`1997_COMPLETED_ALL` <- mastersheet$`1997_COMPLETED_ALL_CY` 
mastersheet$`1998_COMPLETED_ALL` <- mastersheet$`1998_COMPLETED_ALL_CY` 
mastersheet$`1999_COMPLETED_ALL` <- mastersheet$`1999_COMPLETED_ALL_CY` 
mastersheet$`2000_COMPLETED_ALL` <- mastersheet$`2000_COMPLETED_ALL_CY` 

mastersheet$`1996_COMPLETED_PRIVATE_CY` <- (mastersheet$`1995_COMPLETED_PRIVATE` * 0.25) + (mastersheet$`1996_COMPLETED_PRIVATE` * 0.75)
mastersheet$`1997_COMPLETED_PRIVATE_CY` <- (mastersheet$`1996_COMPLETED_PRIVATE` * 0.25) + (mastersheet$`1997_COMPLETED_PRIVATE` * 0.75)
mastersheet$`1998_COMPLETED_PRIVATE_CY` <- (mastersheet$`1997_COMPLETED_PRIVATE` * 0.25) + (mastersheet$`1998_COMPLETED_PRIVATE` * 0.75)
mastersheet$`1999_COMPLETED_PRIVATE_CY` <- (mastersheet$`1998_COMPLETED_PRIVATE` * 0.25) + (mastersheet$`1999_COMPLETED_PRIVATE` * 0.75)
mastersheet$`2000_COMPLETED_PRIVATE_CY` <- (mastersheet$`1999_COMPLETED_PRIVATE` * 0.25) + (mastersheet$`2000_COMPLETED_PRIVATE` * 0.75)

mastersheet$`1996_COMPLETED_PRIVATE` <- mastersheet$`1996_COMPLETED_PRIVATE_CY` 
mastersheet$`1997_COMPLETED_PRIVATE` <- mastersheet$`1997_COMPLETED_PRIVATE_CY` 
mastersheet$`1998_COMPLETED_PRIVATE` <- mastersheet$`1998_COMPLETED_PRIVATE_CY` 
mastersheet$`1999_COMPLETED_PRIVATE` <- mastersheet$`1999_COMPLETED_PRIVATE_CY` 
mastersheet$`2000_COMPLETED_PRIVATE` <- mastersheet$`2000_COMPLETED_PRIVATE_CY`

mastersheet$`1996_COMPLETED_LOCAL_CY` <- (mastersheet$`1995_COMPLETED_LOCAL` * 0.25) + (mastersheet$`1996_COMPLETED_LOCAL` * 0.75)
mastersheet$`1997_COMPLETED_LOCAL_CY` <- (mastersheet$`1996_COMPLETED_LOCAL` * 0.25) + (mastersheet$`1997_COMPLETED_LOCAL` * 0.75)
mastersheet$`1998_COMPLETED_LOCAL_CY` <- (mastersheet$`1997_COMPLETED_LOCAL` * 0.25) + (mastersheet$`1998_COMPLETED_LOCAL` * 0.75)
mastersheet$`1999_COMPLETED_LOCAL_CY` <- (mastersheet$`1998_COMPLETED_LOCAL` * 0.25) + (mastersheet$`1999_COMPLETED_LOCAL` * 0.75)
mastersheet$`2000_COMPLETED_LOCAL_CY` <- (mastersheet$`1999_COMPLETED_LOCAL` * 0.25) + (mastersheet$`2000_COMPLETED_LOCAL` * 0.75)

mastersheet$`1996_COMPLETED_LOCAL` <- mastersheet$`1996_COMPLETED_LOCAL_CY` 
mastersheet$`1997_COMPLETED_LOCAL` <- mastersheet$`1997_COMPLETED_LOCAL_CY` 
mastersheet$`1998_COMPLETED_LOCAL` <- mastersheet$`1998_COMPLETED_LOCAL_CY` 
mastersheet$`1999_COMPLETED_LOCAL` <- mastersheet$`1999_COMPLETED_LOCAL_CY` 
mastersheet$`2000_COMPLETED_LOCAL` <- mastersheet$`2000_COMPLETED_LOCAL_CY`

mastersheet$`1996_COMPLETED_HA_CY` <- (mastersheet$`1995_COMPLETED_HA` * 0.25) + (mastersheet$`1996_COMPLETED_HA` * 0.75)
mastersheet$`1997_COMPLETED_HA_CY` <- (mastersheet$`1996_COMPLETED_HA` * 0.25) + (mastersheet$`1997_COMPLETED_HA` * 0.75)
mastersheet$`1998_COMPLETED_HA_CY` <- (mastersheet$`1997_COMPLETED_HA` * 0.25) + (mastersheet$`1998_COMPLETED_HA` * 0.75)
mastersheet$`1999_COMPLETED_HA_CY` <- (mastersheet$`1998_COMPLETED_HA` * 0.25) + (mastersheet$`1999_COMPLETED_HA` * 0.75)
mastersheet$`2000_COMPLETED_HA_CY` <- (mastersheet$`1999_COMPLETED_HA` * 0.25) + (mastersheet$`2000_COMPLETED_HA` * 0.75)

mastersheet$`1996_COMPLETED_HA` <- mastersheet$`1996_COMPLETED_HA_CY` 
mastersheet$`1997_COMPLETED_HA` <- mastersheet$`1997_COMPLETED_HA_CY` 
mastersheet$`1998_COMPLETED_HA` <- mastersheet$`1998_COMPLETED_HA_CY` 
mastersheet$`1999_COMPLETED_HA` <- mastersheet$`1999_COMPLETED_HA_CY` 
mastersheet$`2000_COMPLETED_HA` <- mastersheet$`2000_COMPLETED_HA_CY`

mastersheet <- mastersheet %>%
  select(-matches("CY"))

# make New Towns data mastersheet 
# Filter the collapsed_clean data frames for the years you care about
new_towns_all_data <- mget(paste0("new_towns_", years))

# Join all collapsed_clean data frames by 'Area_2001'
new_towns_mastersheet <- reduce(new_towns_all_data, left_join, by = 'raw.order') 



###select data for different purposes
#-------------------------------------------------------------------------

#summarised data (including new towns)

#select data on total completions per year only 
# List of columns containing 'COMPLETED_ALL' in their names
completed_all_cols <- grep("COMPLETED_ALL", names(mastersheet), value = TRUE)

# Select 'Area_2001' and columns containing 'COMPLETED_ALL'
completed_all <- mastersheet %>%
  select(Area_2001, Area_name, all_of(completed_all_cols))

#supplementary data before 2000-------------------------------------------------
#path
path = paste0(dirname, "/centre for Cities/Centre for Cities POC - Documents/Research/Housing/History of Planning 2/Data/Input/Supplementary data/", collapse = NULL)

#import data
# Define the path to your Excel file
file_path <- paste0(path, "LiveTable253.xlsx")

# Get all sheet names
sheet_names <- getSheetNames(file_path)

# Define the list of consecutive zeros
values_to_keep <- c("E06000006", "E06000013", "E06000015", "E06000023", "E06000024", "E06000037", 
                    "E06000038", "E07000001", "E07000007", "E07000009", "E07000011", "E07000016", 
                    "E07000024", "E07000032", "E07000041", "E07000043", "E07000061", "E07000069", 
                    "E07000074", "E07000084", "E07000085", "E07000102", "E07000105", "E07000118", 
                    "E07000133", "E07000140", "E07000159", "E07000188", "E07000192", "E07000193", 
                    "E07000226", "E07000227", "E07000240", "E08000001", "E08000011", "E08000012", 
                    "E08000013", "E08000014", "E09000001", "E09000002", "E09000005", "E09000006", 
                    "E09000007", "E09000012", "E09000013", "E09000017", "E09000019", "E09000022", 
                    "E09000023", "E09000024", "E09000025", "E09000027", "E09000028", "E09000030", 
                    "E10000012", "E10000017", "E10000021", "E11000001", "E11000002", "E11000003", 
                    "E11000005", "E11000006", "E12000007")

# Loop through each sheet and assign the data to a variable with the same name as the sheet
for (i in seq_along(sheet_names)) {
  sheet_name <- sheet_names[i]
  print(paste("Reading sheet:", sheet_name))
  
  # Determine the number of rows to skip
  if (i <= 31) {
    start_row <- 5  # Skip the first 4 rows (start from row 5)
  } else {
    start_row <- 4  # Skip the first 3 rows (start from row 4)
  }
  
  # Read the data
  data <- read.xlsx(file_path, sheet = sheet_name, startRow = start_row) |> 
    select(3,5,10:13)
  
  # Filter rows based on the first column values- consecutive zeros
  data <- data[data[[1]] %in% values_to_keep, ]
  
  # Create a variable name in the format F1981, F1982, etc.
  variable_name <- paste0("F", 1979 + i)
  
  # Add suffix to column names for columns 3 to 6
  prefix <- paste0(1979 + i, "_")
  colnames(data)[3:6] <- paste0(prefix, colnames(data)[3:6])
  
  # Assign the data to the newly created variable name
  assign(variable_name, data)
}


# Generate new datasets for 1981 to 1999
#Merge them into one sheet
path = paste0(dirname, "/centre for Cities/Centre for Cities POC - Documents/Research/Housing/History of Planning 2/Data/Input/Supplementary data/", collapse = NULL)

lookup <- read.xlsx(paste0(path, "consecutivezeros.xlsx"), sheet = "lookup")%>%
  rename(Current.ONS.code = Area_2001)

# Loop through each year and merge datasets with lookup
merged_data <-lookup

for (year in 1980:2000) {
  # Construct the variable name as a string
  data_name <- paste0('F', year)
  
  # Retrieve the actual data frame using get()
  data <- get(data_name)
  
  #drop the name column
  data <- data[, -2]
  
  # Merge with lookup dataset
  merged_data <- left_join(merged_data, data, by = "Current.ONS.code")
  
}

# Rename the dataset
FinancialYear <- merged_data


#Translate into Calender year
# Create a copy of FinancialYear to modify
CalendarYear <- FinancialYear

# Function to convert columns to numeric and handle non-numeric values
convert_to_numeric <- function(df, cols) {
  for (col in cols) {
    # Convert to numeric, turning non-numeric values into NA
    df[[col]] <- as.numeric(as.character(df[[col]]))
  }
  return(df)
}

# Columns to check for conversion (from 2rd column onwards and column + 4)
cols_to_check <- 2:(ncol(FinancialYear) - 4)
CalendarYear <- convert_to_numeric(CalendarYear, cols_to_check)
CalendarYear <- convert_to_numeric(CalendarYear, cols_to_check + 4)

# Loop through columns starting from the 3rd to calculate new values
for (col in cols_to_check) {
  # Define the current column index
  current_col <- col
  # Define the column to use for the weighted average (current + 4)
  next_col <- col + 4
  
  # Compute the new values, setting result to NA if either value is NA
  CalendarYear[[current_col]] <- ifelse(
    is.na(CalendarYear[[current_col]]) | is.na(CalendarYear[[next_col]]),
    NA,
    1/4 * CalendarYear[[current_col]] + 3/4 * CalendarYear[[next_col]]
  )
}

# Get column names from the 6th column onward in FinancialYear
new_col_names <- colnames(FinancialYear)[6:ncol(FinancialYear)]

# Rename the columns in CalendarYear starting from the 2rd column
colnames(CalendarYear)[2:(ncol(FinancialYear) - 4)] <- new_col_names

#Drop unnecessary financial year columns.
CalendarYear <- CalendarYear %>% select(-c((ncol(CalendarYear)-3):ncol(CalendarYear)))


#Separate the columns based on suffix and create the datasets---------------
#Rename the first column
colnames(CalendarYear)[1] <- "Area_2001"

# Helper function to rename columns based on suffix and create dataset
separate_and_rename <- function(df, suffix, new_suffix) {
  df %>%
    select(Area_2001, ends_with(suffix)) %>%
    rename_with(~ sub(suffix, new_suffix, .), ends_with(suffix))
}

# Dataset for Local Authority
sulocal <- separate_and_rename(CalendarYear, "_Local.Authority", "_COMPLETED_LOCAL")

# Dataset for Housing Associations
suHA <- separate_and_rename(CalendarYear, "_Housing.Associations", "_COMPLETED_HA")

# Dataset for Private Enterprise
supri <- separate_and_rename(CalendarYear, "_Private.Enterprise", "_COMPLETED_PRIVATE")

# Dataset for All
suall <- separate_and_rename(CalendarYear, "_All", "_COMPLETED_ALL")



#Replace zeros with supplementary data, ALL--------------------------------------
# Step 1: Filter rows in completed_all where Area_2001 is in values_to_keep
filtered_built <- completed_all %>%
  filter(Area_2001 %in% values_to_keep)

# Step 2: Merge filtered_built with suall to get the replacement values
# Assuming suall has columns named the same as filtered_built starting from the 3rd column
merged_data <- filtered_built %>%
  left_join(suall, by = "Area_2001", suffix = c("", ".suall"))

# Step 3: Replace zero values in columns starting from the 3rd column
for (col in 3:ncol(filtered_built)) {
  column_name <- colnames(filtered_built)[col]
  suall_column_name <- paste0(column_name, ".suall")
  
  if (column_name %in% colnames(suall)) {
    # Replace zero values in the current column
    merged_data <- merged_data %>%
      mutate(
        !!sym(column_name) := ifelse(
          get(column_name) == 0,
          coalesce(get(suall_column_name), get(column_name)),
          get(column_name)
        )
      )
  }
}

# Drop the suall columns if no longer needed
result_data <- merged_data %>%
  select(-ends_with(".suall"))

#The data for E09000028-Southwark is not continuous between the two data sources. 
# Let's replace all the data for Southwark 1982-1999 with the replacement data.
# Step 1: Find the row index in result_data where Area_2001 is "E09000028"
row_index <- which(result_data$Area_2001 == "E09000028")

# Step 2: Identify the columns starting with "1982_" and all the subsequent columns
start_col_index <- grep("^1982_", names(result_data))
end_col_index <- grep("^1998_", names(result_data))
sustart_col_index <- grep("^1982_", names(suall))
suend_col_index <- grep("^1998_", names(suall))

# Step 3: Extract the matching row from suall
suall_row <- suall[suall$Area_2001 == "E09000028", ]

# Step 4: Replace the values in result_data
result_data[row_index, start_col_index:end_col_index] <- suall_row[, sustart_col_index:suend_col_index]


# Combine the updated rows back into the original dataset
completed_all <- completed_all %>% 
  filter(!Area_2001 %in% values_to_keep)

completed_all <- rbind(completed_all, result_data)

#Calculate net with updated value------------------------------------------------
demolished_all_cols <- grep("Total_demolished", names(mastersheet), value = TRUE) 
demolished_all <- mastersheet %>% 
  select(Area_2001, all_of(demolished_all_cols))

completed_and_net_1974_2000 <- merge(completed_all, demolished_all, by = "Area_2001")

#Calculate net 1974-79 (demolished data is available for these years)
# Step 1: Define the years for which we want to calculate net values
years <- 1974:1979

# Step 2: Loop through each year, calculate the net, and create a new column
for (year in years) {
  completed_col <- paste0(year, "_COMPLETED_ALL")
  demolished_col <- paste0(year, "_Total_demolished")
  net_col <- paste0(year, "_Net_completed")
  
  # Calculate the net value and create the new column
  completed_and_net_1974_2000[[net_col]] <- completed_and_net_1974_2000[[completed_col]] - completed_and_net_1974_2000[[demolished_col]]
}


#select data on local authority completions per year only ------------------------------------
completed_local_cols <- grep("COMPLETED_LOCAL", names(mastersheet), value = TRUE)

completed_local <- mastersheet %>%
  select(Area_2001, Area_name, all_of(completed_local_cols))

#Replace zeros with supplementary data, local--------
# Step 1: Filter rows in completed_local where Area_2001 is in values_to_keep
filtered_built <- completed_local %>%
  filter(Area_2001 %in% values_to_keep)

# Step 2: Merge filtered_built with sulocal to get the replacement values
# Assuming sulocal has columns named the same as filtered_built starting from the 3rd column
merged_data <- filtered_built %>%
  left_join(sulocal, by = "Area_2001", suffix = c("", ".sulocal"))

# Step 3: Replace zero values in columns starting from the 3rd column
for (col in 3:ncol(filtered_built)) {
  column_name <- colnames(filtered_built)[col]
  sulocal_column_name <- paste0(column_name, ".sulocal")
  
  if (column_name %in% colnames(sulocal)) {
    # Replace zero values in the current column
    merged_data <- merged_data %>%
      mutate(
        !!sym(column_name) := ifelse(
          get(column_name) == 0,
          coalesce(get(sulocal_column_name), get(column_name)),
          get(column_name)
        )
      )
  }
}

# Drop the sulocal columns if no longer needed
result_data <- merged_data %>%
  select(-ends_with(".sulocal"))

#The data for E09000028-Southwark is not continuous between the two data sources. 
# Let's replace all the data for Southwark 1982-1999 with the replacement data.
# Step 1: Find the row index in result_data where Area_2001 is "E09000028"
row_index <- which(result_data$Area_2001 == "E09000028")

# Step 2: Identify the columns starting with "1982_" and all the subsequent columns
start_col_index <- grep("^1982_", names(result_data))
end_col_index <- grep("^1998_", names(result_data))
sustart_col_index <- grep("^1982_", names(sulocal))
suend_col_index <- grep("^1998_", names(sulocal))

# Step 3: Extract the matching row from sulocal
sulocal_row <- sulocal[sulocal$Area_2001 == "E09000028", ]

# Step 4: Replace the values in result_data
result_data[row_index, start_col_index:end_col_index] <- sulocal_row[, sustart_col_index:suend_col_index]


# Combine the updated rows back into the original dataset
completed_local <- completed_local %>% 
  filter(!Area_2001 %in% values_to_keep)

completed_local <- rbind(completed_local, result_data)


#select data on housing association completions per year only-----------------------------------
completed_HA_cols <- grep("COMPLETED_HA", names(mastersheet), value = TRUE)

completed_HA <- mastersheet %>%
  select(Area_2001, Area_name, all_of(completed_HA_cols))

#Replace zeros with supplementary data, HA--------
# Step 1: Filter rows in completed_HA where Area_2001 is in values_to_keep
filtered_built <- completed_HA %>%
  filter(Area_2001 %in% values_to_keep)

# Step 2: Merge filtered_built with suHA to get the replacement values
# Assuming suHA has columns named the same as filtered_built starting from the 3rd column
merged_data <- filtered_built %>%
  left_join(suHA, by = "Area_2001", suffix = c("", ".suHA"))

# Step 3: Replace zero values in columns starting from the 3rd column
for (col in 3:ncol(filtered_built)) {
  column_name <- colnames(filtered_built)[col]
  suHA_column_name <- paste0(column_name, ".suHA")
  
  if (column_name %in% colnames(suHA)) {
    # Replace zero values in the current column
    merged_data <- merged_data %>%
      mutate(
        !!sym(column_name) := ifelse(
          get(column_name) == 0,
          coalesce(get(suHA_column_name), get(column_name)),
          get(column_name)
        )
      )
  }
}

# Drop the suHA columns if no longer needed
result_data <- merged_data %>%
  select(-ends_with(".suHA"))

#The data for E09000028-Southwark is not continuous between the two data sources. 
# Let's replace all the data for Southwark 1982-1999 with the replacement data.
# Step 1: Find the row index in result_data where Area_2001 is "E09000028"
row_index <- which(result_data$Area_2001 == "E09000028")

# Step 2: Identify the columns starting with "1982_" and all the subsequent columns
start_col_index <- grep("^1982_", names(result_data))
end_col_index <- grep("^1998_", names(result_data))
sustart_col_index <- grep("^1982_", names(suHA))
suend_col_index <- grep("^1998_", names(suHA))

# Step 3: Extract the matching row from suHA
suHA_row <- suHA[suHA$Area_2001 == "E09000028", ]

# Step 4: Replace the values in result_data
result_data[row_index, start_col_index:end_col_index] <- suHA_row[, sustart_col_index:suend_col_index]


# Combine the updated rows back into the original dataset
completed_HA <- completed_HA %>% 
  filter(!Area_2001 %in% values_to_keep)

completed_HA <- rbind(completed_HA, result_data)


#select data on private completions per year only-------------------------------------------
completed_private_cols <- grep("COMPLETED_PRIVATE", names(mastersheet), value = TRUE)

completed_private <- mastersheet %>%
  select(Area_2001, Area_name, all_of(completed_private_cols))

#Replace zeros with supplementary data, private--------
# Step 1: Filter rows in completed_private where Area_2001 is in values_to_keep
filtered_built <- completed_private %>%
  filter(Area_2001 %in% values_to_keep)

# Step 2: Merge filtered_built with supri to get the replacement values
# Assuming supri has columns named the same as filtered_built starting from the 3rd column
merged_data <- filtered_built %>%
  left_join(supri, by = "Area_2001", suffix = c("", ".supri"))

# Step 3: Replace zero values in columns starting from the 3rd column
for (col in 3:ncol(filtered_built)) {
  column_name <- colnames(filtered_built)[col]
  supri_column_name <- paste0(column_name, ".supri")
  
  if (column_name %in% colnames(supri)) {
    # Replace zero values in the current column
    merged_data <- merged_data %>%
      mutate(
        !!sym(column_name) := ifelse(
          get(column_name) == 0,
          coalesce(get(supri_column_name), get(column_name)),
          get(column_name)
        )
      )
  }
}

# Drop the supri columns if no longer needed
result_data <- merged_data %>%
  select(-ends_with(".supri"))

#The data for E09000028-Southwark is not continuous between the two data sources. 
# Let's replace all the data for Southwark 1982-1999 with the replacement data.
# Step 1: Find the row index in result_data where Area_2001 is "E09000028"
row_index <- which(result_data$Area_2001 == "E09000028")

# Step 2: Identify the columns starting with "1982_" and all the subsequent columns
start_col_index <- grep("^1982_", names(result_data))
end_col_index <- grep("^1998_", names(result_data))
sustart_col_index <- grep("^1982_", names(supri))
suend_col_index <- grep("^1998_", names(supri))

# Step 3: Extract the matching row from supri
supri_row <- supri[supri$Area_2001 == "E09000028", ]

# Step 4: Replace the values in result_data
result_data[row_index, start_col_index:end_col_index] <- supri_row[, sustart_col_index:suend_col_index]


# Combine the updated rows back into the original dataset
completed_private <- completed_private %>% 
  filter(!Area_2001 %in% values_to_keep)

completed_private <- rbind(completed_private, result_data)


#select data on population in each year----------------------------------------------
pop_all_cols <- grep("POP", names(mastersheet), value = TRUE)

population_all <- mastersheet %>%
  select(Area_2001, Area_name, all_of(pop_all_cols)) 


#new towns data 
#-----------------------------------------------------------------------------
# List of columns containing 'COMPLETED_ALL' in their names
NTcompleted_all_cols <- grep("COMPLETED_ALL", names(new_towns_mastersheet), value = TRUE)

# Select 'Area_2001' and columns containing 'COMPLETED_ALL'
NTcompleted_all <- new_towns_mastersheet %>%
  select(raw.order, Area_2001.x, all_of(NTcompleted_all_cols))

names(NTcompleted_all)[2] <- 'Area_2001'

#select data on local authority completions per year only 
NTcompleted_local_cols <- grep("COMPLETED_LOCAL", names(new_towns_mastersheet), value = TRUE)

NTcompleted_local <- new_towns_mastersheet %>%
  select(raw.order, Area_2001.x, all_of(NTcompleted_local_cols))

names(NTcompleted_local)[2] <- 'Area_2001'

#select data on housing association completions per year only 
NTcompleted_HA_cols <- grep("COMPLETED_HA", names(new_towns_mastersheet), value = TRUE)

NTcompleted_HA <- new_towns_mastersheet %>%
  select(raw.order, Area_2001.x, all_of(NTcompleted_HA_cols))

names(NTcompleted_HA)[2] <- 'Area_2001'

#select data on private completions per year only 
NTcompleted_private_cols <- grep("COMPLETED_PRIVATE", names(new_towns_mastersheet), value = TRUE)

NTcompleted_private <- new_towns_mastersheet %>%
  select(raw.order, Area_2001.x, all_of(NTcompleted_private_cols))

names(NTcompleted_private)[2] <- 'Area_2001'


#create sheet to look at percentage contribution of specific NT building to building in the LA they are in
NTcontributions_all <- left_join(NTcompleted_all, completed_all, by = "Area_2001")
NTcontributions_LA <- left_join(NTcompleted_local, completed_local, by = "Area_2001")
NTcontributions_HA <- left_join(NTcompleted_HA, completed_HA, by = "Area_2001")
NTcontributions_private <- left_join(NTcompleted_private, completed_private, by = "Area_2001")

# Get the column names
years <- 1974:2000

# Loop through each year and create new columns
for (year in years) {
  x_col <- paste0(year, "_COMPLETED_ALL.x")
  y_col <- paste0(year, "_COMPLETED_ALL.y")
  new_col <- paste0(year, "_COMPLETED_ALL_RATIO")
  
  NTcontributions_all[[new_col]] <- NTcontributions_all[[x_col]] / NTcontributions_all[[y_col]]
}

NTcontributions_all <- NTcontributions_all %>%
  mutate_all(~ ifelse(is.nan(.), 0, .))

NTcontributions_all <- left_join(NTcontributions_all, geog_names, by = "Area_2001")

column_names <- colnames(NTcontributions_all)
last_col_name <- tail(column_names, 1)
new_order <- c(column_names[1], last_col_name, column_names[2:(length(column_names) - 1)])
NTcontributions_all <- NTcontributions_all[, new_order]

# Loop through each year and create new columns
for (year in years) {
  x_col <- paste0(year, "_COMPLETED_LOCAL.x")
  y_col <- paste0(year, "_COMPLETED_LOCAL.y")
  new_col <- paste0(year, "_COMPLETED_LOCAL_RATIO")
  
  NTcontributions_LA[[new_col]] <- NTcontributions_LA[[x_col]] / NTcontributions_LA[[y_col]]
}

NTcontributions_LA <- NTcontributions_LA %>%
  mutate_all(~ ifelse(is.nan(.), 0, .))

NTcontributions_LA <- left_join(NTcontributions_LA, geog_names, by = "Area_2001")

column_names <- colnames(NTcontributions_LA)
last_col_name <- tail(column_names, 1)
new_order <- c(column_names[1], last_col_name, column_names[2:(length(column_names) - 1)])
NTcontributions_LA <- NTcontributions_LA[, new_order]

# Loop through each year and create new columns
for (year in years) {
  x_col <- paste0(year, "_COMPLETED_HA.x")
  y_col <- paste0(year, "_COMPLETED_HA.y")
  new_col <- paste0(year, "_COMPLETED_HA_RATIO")
  
  NTcontributions_HA[[new_col]] <- NTcontributions_HA[[x_col]] / NTcontributions_HA[[y_col]]
}

NTcontributions_HA <- NTcontributions_HA %>%
  mutate_all(~ ifelse(is.nan(.), 0, .))

NTcontributions_HA <- left_join(NTcontributions_HA, geog_names, by = "Area_2001")

column_names <- colnames(NTcontributions_HA)
last_col_name <- tail(column_names, 1)
new_order <- c(column_names[1], last_col_name, column_names[2:(length(column_names) - 1)])
NTcontributions_HA <- NTcontributions_HA[, new_order]

# Loop through each year and create new columns
for (year in years) {
  x_col <- paste0(year, "_COMPLETED_PRIVATE.x")
  y_col <- paste0(year, "_COMPLETED_PRIVATE.y")
  new_col <- paste0(year, "_COMPLETED_PRIVATE_RATIO")
  
  NTcontributions_private[[new_col]] <- NTcontributions_private[[x_col]] / NTcontributions_private[[y_col]]
}

NTcontributions_private <- NTcontributions_private %>%
  mutate_all(~ ifelse(is.nan(.), 0, .))

NTcontributions_private <- left_join(NTcontributions_private, geog_names, by = "Area_2001")

column_names <- colnames(NTcontributions_private)
last_col_name <- tail(column_names, 1)
new_order <- c(column_names[1], last_col_name, column_names[2:(length(column_names) - 1)])
NTcontributions_private <- NTcontributions_private[, new_order]

#create sheet to look at percentage contribution of all NT building to building in the LA that they are in 
#NOTE - the only difference between this and the above is for one LA, which contains two new towns - Welwyn and Hatfield 
NT_all_summarised <- NTcompleted_all %>%
  group_by(Area_2001) %>%
  summarize(across(where(is.numeric), mean, na.rm = TRUE))

NT_all_summarised <- NT_all_summarised %>%
  mutate_all(~ ifelse(is.nan(.), 0, .))

NT_local_summarised <- NTcompleted_local %>%
  group_by(Area_2001) %>%
  summarize(across(where(is.numeric), mean, na.rm = TRUE))

NT_local_summarised <- NT_local_summarised %>%
  mutate_all(~ ifelse(is.nan(.), 0, .))

NT_HA_summarised <- NTcompleted_HA %>%
  group_by(Area_2001) %>%
  summarize(across(where(is.numeric), mean, na.rm = TRUE))

NT_HA_summarised <- NT_HA_summarised %>%
  mutate_all(~ ifelse(is.nan(.), 0, .))

NT_private_summarised <- NTcompleted_private %>%
  group_by(Area_2001) %>%
  summarize(across(where(is.numeric), mean, na.rm = TRUE))

NT_private_summarised <- NT_private_summarised %>%
  mutate_all(~ ifelse(is.nan(.), 0, .))

# Get the column names
years <- 1974:2000

#work out the percentage contributions of NT building in their respective LAs - all housebuilding
NTpercentage_all <- left_join(NT_all_summarised, completed_all, by = "Area_2001")

# Loop through each year and create new columns
for (year in years) {
  x_col <- paste0(year, "_COMPLETED_ALL.x")
  y_col <- paste0(year, "_COMPLETED_ALL.y")
  new_col <- paste0(year, "_COMPLETED_ALL_RATIO")
  
  NTpercentage_all[[new_col]] <- NTpercentage_all[[x_col]] / NTpercentage_all[[y_col]]
}

NTpercentage_all <- NTpercentage_all %>%
  mutate_all(~ ifelse(is.nan(.), 0, .))

NTpercentage_all <- left_join(NTpercentage_all, geog_names, by = "Area_2001")

column_names <- colnames(NTpercentage_all)
last_col_name <- tail(column_names, 1)
new_order <- c(column_names[1], last_col_name, column_names[2:(length(column_names) - 1)])
NTpercentage_all <- NTpercentage_all[, new_order]

#work out the percentage contributions of NT building in their respective LAs - local authority housebuilding
NTpercentage_LA <- left_join(NT_local_summarised, completed_local, by = "Area_2001")

# Loop through each year and create new columns
for (year in years) {
  x_col <- paste0(year, "_COMPLETED_LOCAL.x")
  y_col <- paste0(year, "_COMPLETED_LOCAL.y")
  new_col <- paste0(year, "_COMPLETED_LOCAL_RATIO")
  
  NTpercentage_LA[[new_col]] <- NTpercentage_LA[[x_col]] / NTpercentage_LA[[y_col]]
}

NTpercentage_LA <- NTpercentage_LA %>%
  mutate_all(~ ifelse(is.nan(.), 0, .))

NTpercentage_LA <- left_join(NTpercentage_LA, geog_names, by = "Area_2001")

column_names <- colnames(NTpercentage_LA)
last_col_name <- tail(column_names, 1)
new_order <- c(column_names[1], last_col_name, column_names[2:(length(column_names) - 1)])
NTpercentage_LA <- NTpercentage_LA[, new_order]

#work out the percentage contributions of NT building in their respective LAs - houseing association housebuilding
NTpercentage_HA <- left_join(NT_HA_summarised, completed_HA, by = "Area_2001")

# Loop through each year and create new columns
for (year in years) {
  x_col <- paste0(year, "_COMPLETED_HA.x")
  y_col <- paste0(year, "_COMPLETED_HA.y")
  new_col <- paste0(year, "_COMPLETED_HA_RATIO")
  
  NTpercentage_HA[[new_col]] <- NTpercentage_HA[[x_col]] / NTpercentage_HA[[y_col]]
}

NTpercentage_HA <- NTpercentage_HA %>%
  mutate_all(~ ifelse(is.nan(.), 0, .))

NTpercentage_HA <- left_join(NTpercentage_HA, geog_names, by = "Area_2001")

column_names <- colnames(NTpercentage_HA)
last_col_name <- tail(column_names, 1)
new_order <- c(column_names[1], last_col_name, column_names[2:(length(column_names) - 1)])
NTpercentage_HA <- NTpercentage_HA[, new_order]

#work out the percentage contributions of NT building in their respective LAs - private housebuilding
NTpercentage_private <- left_join(NT_private_summarised, completed_private, by = "Area_2001")

# Loop through each year and create new columns
for (year in years) {
  x_col <- paste0(year, "_COMPLETED_PRIVATE.x")
  y_col <- paste0(year, "_COMPLETED_PRIVATE.y")
  new_col <- paste0(year, "_COMPLETED_PRIVATE_RATIO")
  
  NTpercentage_private[[new_col]] <- NTpercentage_private[[x_col]] / NTpercentage_private[[y_col]]
}

NTpercentage_private <- NTpercentage_private %>%
  mutate_all(~ ifelse(is.nan(.), 0, .))

NTpercentage_private <- left_join(NTpercentage_private, geog_names, by = "Area_2001")

column_names <- colnames(NTpercentage_private)
last_col_name <- tail(column_names, 1)
new_order <- c(column_names[1], last_col_name, column_names[2:(length(column_names) - 1)])
NTpercentage_private <- NTpercentage_private[, new_order]


#export to excel 
#---------------------------------------------

### EXPORT completed & population stats to excel
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

# Generate file path for Excel workbook with date
date <- format(Sys.Date(), "%Y-%m-%d")
file_name_geog <- paste0(date, "built_and_population_1974_2000", ".xlsx")
dir_path_geog = paste0(dirname, "/centre for Cities/Centre for Cities POC - Documents/Research/Housing/History of Planning 2/Data/Output/From R scripts/Housebuilding/")
file_path_geog <- file.path(dir_path_geog, file_name_geog)

# Create a list of data frames
df_list_geog <- list(completed_and_net_1974_2000, completed_all, completed_private, completed_local, completed_HA, population_all)
names(df_list_geog) <- c("net built", "total built", "private built", "LA built", "HA built", "population")                         

# Export data frames to Excel
export_to_excel(df_list_geog, file_path_geog)



# Generate file path for Excel workbook with date
date <- format(Sys.Date(), "%Y-%m-%d")
file_name_geog <- paste0(date, "new_towns_1974_2000", ".xlsx")
dir_path_geog = paste0(dirname, "/centre for Cities/Centre for Cities POC - Documents/Research/Housing/History of Planning 2/Data/Output/From R scripts/Housebuilding/")
file_path_geog <- file.path(dir_path_geog, file_name_geog)

# Create a list of data frames
df_list_geog <- list(NTcontributions_all, NTcontributions_LA, NTcontributions_private, NTcontributions_HA,
                     NTpercentage_all, NTpercentage_LA, NTpercentage_private, NTpercentage_HA)
names(df_list_geog) <- c("IndividualNTs_total", "IndividualNTs_local", "IndividualNTs_private", "IndividualNTs_HA", 
                         "NT_by_LA_total", "NT_by_LA_local", "NT_by_LA_private", "NT_HA")                         

# Export data frames to Excel
export_to_excel(df_list_geog, file_path_geog)







