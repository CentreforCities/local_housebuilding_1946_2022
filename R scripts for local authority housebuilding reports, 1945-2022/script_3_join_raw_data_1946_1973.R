library(tidyverse)
library(readxl)
library(dplyr)
library(openxlsx) 
library(ggplot2)
library(tidyr)
library(purrr)

#geography lookup path----------------------------------------------------------
path = paste0(dirname("~"),"/Centre for Cities/Centre for Cities POC - Documents/Research/Housing/History of Planning 2/Data/Input/", collapse = NULL)
setwd(path) 

#Create a lookup without NT
# equiv_1971_cd has duplicates from the lookup data, but only for new towns and the areas that include them, as well as Whiston and Kirkby.
# Import basic geography lookup for 1946 to 1973, drop new town
geog_names <- read.xlsx("Lookup_for_pre1974.xlsx") |> 
  select(1:8, 11,14) |> 
  filter(`GEOGRAPHY.TYPE` != "NT")

# Drop the first entry where equiv_1971_cd equals "e200698" to keep only Whiston for lookup (Kirkby merged with Whiston for continuity)
geog_names <- geog_names |> 
  filter(!(equiv_1971_cd == "e200698" & row_number() == which(equiv_1971_cd == "e200698")[1]))

# Remove rows with NA values in equiv_1971_cd
geog_names <- geog_names[complete.cases(geog_names$equiv_1971_cd), ]

#remove duplicate 1971 code lines
geog_names <- geog_names %>%
  distinct(equiv_1971_cd, .keep_all = TRUE)

# Find duplicated equiv_1971_cd values (should be 0 now)
duplicates <- geog_names[duplicated(geog_names$equiv_1971_cd) | duplicated(geog_names$equiv_1971_cd, fromLast = TRUE), ]
print(duplicates)


#bring in flagging data
path <- paste0(dirname("~"),"/Centre for Cities/Centre for Cities POC - Documents/Research/Housing/History of Planning 2/Data/Input/", collapse = NULL)
setwd(path)

lad71_cty81 <- read.xlsx("2024-07-22_GB_Lookups.xlsx", sheet = "lad_71")
colnames(lad71_cty81)[1] <- "equiv_1971_cd"
lad71_cty81 <- lad71_cty81[, c("equiv_1971_cd", "cty_81")]

# Define a vector of years------------------------------------------------------
years <- c(1946,1950,1951, 1953:1973) # Add any additional years you want to process
new_towns <- list()


#house price data source data path----------------------------------------------
path = paste0(dirname("~"),"/Centre for Cities/Centre for Cities POC - Documents/Research/Housing/History of Planning 2/Data/Input/Housing Returns/Phase 2 - geography joined/", collapse = NULL)
setwd(path) 

#Import housing data for 1946-1973 
#---------------------------------------------------------------------
for (year in years) {
  
  file_path <- paste0(year, " - phase 2 joined.xlsx")
  sheet_name <- paste0(year, "_cleaned")
  
  # Import raw data for the specified year
  raw_data <- read_excel(file_path, sheet = sheet_name)
  
  # Add year in front of column titles
  colnames(raw_data)[16:ncol(raw_data)] <- paste0(year, "_", colnames(raw_data)[16:ncol(raw_data)])
  
  # Remove rows without 1971 Area Codes (equiv_1971_cd)
  clean_data <- raw_data[!is.na(raw_data$equiv_1971_cd) & raw_data$equiv_1971_cd != 0, ] 
  
  # Convert columns 16 onward to numeric
  clean_data <- clean_data %>%
    mutate(across(16:ncol(clean_data), ~ as.numeric(as.character(.))))
  
  # Replace all NA values with 0 so things add up properly
  clean_data <- clean_data %>%
    mutate(across(where(is.numeric), ~replace_na(., 0))) %>%
    mutate(across(where(is.character), ~replace_na(., "NA")))
  
  # Remove columns containing "APPROVED", "SITE", "TEMP","STARTED"
  clean_data <- clean_data %>%
    # “TEMP” and “WAR” are taken off because they are temporary and do not contribute to the lasting housing stock.
    select(-contains("APPROVED"), -contains("SITE_"), -contains("TEMP"), -contains("STARTED"), -contains("INCONSTRUCT"), -contains("WAR"))

  
  #Create Sum Columns only for years before 1966--------------------------------
  if (year < 1966) {
    # Filter the dataframe for the current year less than 1966
    filtered_data <- clean_data %>% filter(year == year)
    
    # Find columns that contain "COMPLETED" in their names
    completed_cols <- grep("COMPLETED", colnames(filtered_data), value = TRUE)
    
    # Create new column names for the totals
    col_name_total_completed <- paste0(year, "_COMPLETED_ALL")
    
    # Calculate sum columns
    filtered_data[[col_name_total_completed]] <- rowSums(filtered_data[, completed_cols], na.rm = TRUE)
    } else {
    filtered_data <- clean_data
  }
  
  # Add a new column that sums values from columns containing 'DEMOLISHED' (DEMOLISHED_INCLEAR, DEMOLISHED_ELSE only available since 1956)---------------
  demolished_cols <- grep("DEMOLISHED", colnames(filtered_data), value = TRUE)
  ## There're DEMOLISHED_ORDERS column in 1966, 1967,1968!
  demolished_cols <- demolished_cols[!grepl("ORDER", demolished_cols)]
  
  if (length(demolished_cols) > 0) {
    col_name_total_demolished <- paste0(year, "_Total_demolished")
    filtered_data[[col_name_total_demolished]] <- rowSums(filtered_data[, demolished_cols], na.rm = TRUE)
  } else {
    col_name_total_demolished <- paste0(year, "_Total_demolished")
    filtered_data[[col_name_total_demolished]] <- rep(NA, nrow(filtered_data))  # If no DEMOLISHED columns found
  }
 
  
  # Creating a combined public housebuilding (HA + LA) column _HA only available since 1962)-------------------
  if (year < 1962) {
    filtered_data <- filtered_data %>% filter(year == year)
    
    # Find _LOCAL columns
    completed_la_cols <- grep("COMPLETED_LOCAL", colnames(filtered_data), value = TRUE)
    
    # Create new column names for the totals
    col_name_completed_combined <- paste0(year, "_COMPLETED_PUBLIC")
    
    # Copy COMPLETED_LA columns over to COMPLETED_COMBINED
    filtered_data[[col_name_completed_combined]] <- rowSums(filtered_data[, completed_la_cols], na.rm = TRUE)
  }
  
  if (year > 1961) {
    filtered_data <- filtered_data %>% filter(year == year)
    
    # Find "_LOCAL" and "_HA"
    completed_la_cols <- grep("COMPLETED_LOCAL", colnames(filtered_data), value = TRUE)
    completed_ha_cols <- grep("COMPLETED_HA", colnames(filtered_data), value = TRUE)
    

    # Create new column names for the totals
    col_name_completed_combined <- paste0(year, "_COMPLETED_PUBLIC")
    
    # Calculate total public housebuilding by summing
    filtered_data[[col_name_completed_combined]] <- rowSums(filtered_data[, c(completed_la_cols, completed_ha_cols)], na.rm = TRUE)
    
  }
  
  
  #for 1946-1973 (when public housebuilding in NTs needs to be added to LA, but private housebuilding in NTs is already in LA figures) -------------
  # Because 1946 doesn't have new town, skip it.
  if (year > 1946) {
  
  # Filter data for "New Town" construction  
  new_towns_data <- filtered_data %>% filter(`GEOGRAPHY TYPE` == "NT")
  
  # Create a copy of the dataframe to hold the updated values
  publicNT_added <- filtered_data %>% filter(`GEOGRAPHY TYPE` != "NT")
  
  #in some years equiv_1971_cd is not identical (e.g. e120323), collapse it here
  publicNT_added <- publicNT_added %>%
    group_by(equiv_1971_cd) %>%
    summarise(
      across(
        grep(paste0("^", year, "_"), colnames(.), value = TRUE),
        ~ sum(., na.rm = TRUE)
      )
    )
  
  # Identify columns that end in ...
  local_columns <- grep("_COMPLETED_LOCAL$", colnames(new_towns_data), value = TRUE)
  ha_columns <- grep("_COMPLETED_HA$", colnames(new_towns_data), value = TRUE)
  public_columns <- grep("_COMPLETED_PUBLIC$", colnames(new_towns_data), value = TRUE)
  all_columns <- grep("_COMPLETED_ALL$", colnames(new_towns_data), value = TRUE)
  
  # Ensure that the lengths of column vectors are equal
    
    # Iterate over each 'New Town' row and update corresponding non-'New Town' rows
    for (i in 1:nrow(new_towns_data)) {
      new_town_row <- new_towns_data[i, ]
      area <- new_town_row$equiv_1971_cd
      
      # Find matching rows based on 'equiv_1971_cd' value
      matching_rows_index <- which(publicNT_added$equiv_1971_cd == area)
      
      # Check if there are any matching rows
      if (length(matching_rows_index) > 0) {
        # Add the '_PUBLIC', '_LOCAL', and '_HA' values from the 'New Town' row to the corresponding values in the matching rows
        for (j in 1:length(local_columns)) {
          local_col <- local_columns[j]
          ha_col <- ha_columns[j]
          public_col <- public_columns[j]
          all_col <- all_columns[j]
          
          # Ensure that the columns are valid before updating
          if (all_col %in% colnames(publicNT_added) && public_col %in% colnames(new_town_row)) {
            print(paste("Adding to", all_col, "the value of", public_col, "which is", new_town_row[[public_col]]))
            publicNT_added[matching_rows_index, all_col] <- publicNT_added[matching_rows_index, all_col] + new_town_row[[public_col]]
            print(paste("Updated value of", all_col, "is", publicNT_added[matching_rows_index, all_col]))
          }
          
          if (local_col %in% colnames(publicNT_added) && local_col %in% colnames(new_town_row)) {
            publicNT_added[matching_rows_index, local_col] <- publicNT_added[matching_rows_index, local_col] + new_town_row[[local_col]]
          }
          
          if (ha_col %in% colnames(publicNT_added) && ha_col %in% colnames(new_town_row)) {
            publicNT_added[matching_rows_index, ha_col] <- publicNT_added[matching_rows_index, ha_col] + new_town_row[[ha_col]]
          }
          
          if (public_col %in% colnames(publicNT_added) && public_col %in% colnames(new_town_row)) {
            publicNT_added[matching_rows_index, public_col] <- publicNT_added[matching_rows_index, public_col] + new_town_row[[public_col]]
          }
        }
      }
    }
  } else {
    new_towns_data <- filtered_data %>% filter(`GEOGRAPHY TYPE` == "NT")
    publicNT_added <- filtered_data %>% filter(`GEOGRAPHY TYPE` != "NT")
  }
  

  #equiv_1971_cd is not identical (only because Whiston and Kirkby), collapse it here------------------
  collapsed_LA <- publicNT_added %>%
    group_by(equiv_1971_cd) %>%
    summarise(
      across(
        grep(paste0("^", year, "_"), colnames(.), value = TRUE),
        ~ sum(., na.rm = TRUE)
      )
    )
  
  #collapse makes the '_demolished' column becomes 0 when data is unavailable. We need to change it back to NA.
  if (all(collapsed_LA[[col_name_total_demolished]] == 0, na.rm = TRUE) &&
      any(!is.na(collapsed_LA[[col_name_total_demolished]]))) {
    collapsed_LA[[col_name_total_demolished]] <- ifelse(collapsed_LA[[col_name_total_demolished]] == 0, NA, collapsed_LA[[col_name_total_demolished]])
  }
  
 
  #clean out put (otherwise there would be columns named like "1946...33" generated - reason unknown!)
  collapsed_LA <- collapsed_LA %>% select(-contains("..."))
  new_towns_data <- new_towns_data %>% select(-contains("..."))
 
  # Assign dataframes to variables in the global environment
  assign(paste0("collapsed_LA_", year), collapsed_LA)
  assign(paste0("new_towns_", year), new_towns_data)
  
  # Store the generated new towns data in a list
  new_towns[[paste0("new_towns_", year)]] <- new_towns_data

}


#1969 data is for first 9 months only, so we need to divide by 0.75 to get the full year estimate 
collapsed_LA_1969 <- collapsed_LA_1969 %>%
  mutate(across(where(is.numeric) & !matches('1969_POP'), ~ . / 0.75))

new_towns_1969 <- new_towns_1969 %>%
  mutate(across(where(is.numeric) & !matches('1969_POP'), ~ . / 0.75))
 
#1966,1967 demolition data is for first 9 months only, so we need to divide by 0.75 to get the full year estimate
# For 1966 data 
collapsed_LA_1966 <- collapsed_LA_1966 %>% 
  mutate(across(matches('1966_Total_demolished'), ~ . / 0.75)) 
new_towns_1966 <- new_towns_1966 %>% 
  mutate(across(matches('1966_Total_demolished'), ~ . / 0.75))
# For 1967 data 
collapsed_LA_1967 <- collapsed_LA_1967 %>% 
  mutate(across(matches('1967_Total_demolished'), ~ . / 0.75)) 
new_towns_1967 <- new_towns_1967 %>% 
  mutate(across(matches('1967_Total_demolished'), ~ . / 0.75))

#1946 data is for 1yr 9 months from April 1945 to Dec 1946. Dividing by 1.75 assumes unrealistic flat building rate (and also makes our totals 48% lower
#than UK national stats). To assume increasing building rate through the period but not put all in 1946, I divide by 1.25 
collapsed_LA_1946 <- collapsed_LA_1946 %>%
  mutate(across(where(is.numeric) & !matches('1946_population'), ~ . / 1.25)) 


#Joining the data sets 
#------------------------------------------------------------------------

#re-set which years you want to draw into the mastersheet
years <- c(1946,1950,1951, 1953:1973)   # Add any additional years you want to process

#join data from all years to create local authority mastersheet
# Filter the collapsed_clean data frames for the years you care about
collapsed_clean_years <- mget(paste0("collapsed_LA_", years))

# Join all collapsed_clean data frames by 'equiv_1971_cd'
mastersheet <- reduce(collapsed_clean_years, left_join, by = 'equiv_1971_cd') 

# Join geog_names to mastersheet by 'equiv_1971_cd'
mastersheet <- left_join(mastersheet, geog_names, by = "equiv_1971_cd")

# Multiply the population data by 1000 to match the post-1973 data
mastersheet <- mastersheet %>%
  mutate(across(contains(c("_POP", "_population")), ~ . * 1000))

## Keep only the first occurrence of each duplicate and remove the rest (APPEAR SINCE 8/23/2024)
mastersheet <- mastersheet[!duplicated(mastersheet[[1]]), ]

# Notes: The Mastersheet comes from collapsed_LA, which in turn comes from publicNT_added. And publicNT_added doesn't include new town data.

geog_names_reduced <- geog_names %>%
  select(equiv_1971_cd, equiv_1971_nm)

## Keep only the first occurrence of each duplicate and remove the rest (APPEAR SINCE 8/23/2024)
geog_names_reduced <- geog_names_reduced[!duplicated(geog_names_reduced[[1]]), ]

#output raw versions of the data for checking purposes, before we convert the cumulative columns to annual below 
master_local <- mastersheet %>%
  select(equiv_1971_cd, contains("COMPLETED_LOCAL"))
master_local <- master_local %>%
  left_join(geog_names_reduced, by = "equiv_1971_cd")

master_HA <- mastersheet %>%
  select(equiv_1971_cd, contains("COMPLETED_HA"))
master_HA <- master_HA %>%
  left_join(geog_names_reduced, by = "equiv_1971_cd")

master_private <- mastersheet %>%
  select(equiv_1971_cd, contains("COMPLETED_PRIVATE"))
master_private <- master_private %>%
  left_join(geog_names_reduced, by = "equiv_1971_cd")

master_all <- mastersheet %>%
  select(equiv_1971_cd, contains("COMPLETED_ALL"))
master_all <- master_all %>%
  left_join(geog_names_reduced, by = "equiv_1971_cd")

master_demos <- mastersheet %>%
  select(equiv_1971_cd, contains("Total_demo"))
master_demos <- master_demos %>%
  left_join(geog_names_reduced, by = "equiv_1971_cd")

#Because the completions are cumulative (from 1st April 1945) up to 1965. Since 1966, they become annual.For now, let's do annual.
#------------------------------------------------------------------------ 
#percentage of national total housebuilding done in years 1947-1950, and 1952-53 - so we can apportion our missing years. 
#1947    1948      1949      1950        1952    1953
#18.89%   30.25%   25.33%    25.53%      42.75%   57.25%

# total built
# For 1954-1965, Loop through each column and update values
year_columns <- paste0(1953:1965, "_COMPLETED_ALL")

for (i in length(year_columns):2) {
  mastersheet[[year_columns[i]]] <- mastersheet[[year_columns[i]]] - mastersheet[[year_columns[i - 1]]]
}

# Because we don't have data for 1947-1949 and 1952, we need to calculate the figures for 1950 and 1953 manually by proportion.
# For 1951, 1951 ANNUAL = 1951-1950
mastersheet[["1953_COMPLETED_ALL"]] <- (mastersheet[["1953_COMPLETED_ALL"]] - mastersheet[["1951_COMPLETED_ALL"]])*0.5725
mastersheet[["1952_COMPLETED_ALL"]] <- mastersheet[["1953_COMPLETED_ALL"]] * (0.4275/0.5725)
mastersheet[["1951_COMPLETED_ALL"]] <- mastersheet[["1951_COMPLETED_ALL"]] - mastersheet[["1950_COMPLETED_ALL"]]
mastersheet[["1950_COMPLETED_ALL"]] <- (mastersheet[["1950_COMPLETED_ALL"]] - mastersheet[["1946_COMPLETED_ALL"]])*0.2553
mastersheet[["1949_COMPLETED_ALL"]] <- mastersheet[["1950_COMPLETED_ALL"]] * (0.2533/0.2553)
mastersheet[["1948_COMPLETED_ALL"]] <- mastersheet[["1950_COMPLETED_ALL"]] * (0.3025/0.2553)
mastersheet[["1947_COMPLETED_ALL"]] <- mastersheet[["1950_COMPLETED_ALL"]] * (0.1889/0.2553)


# Private built
#percentage of national private housebuilding done in years 1947-1950, and 1952-53 - so we can apportion our missing years. 
#1947       1948       1949       1950           1952       1953
#32.71%    25.71%     20.15%     21.43%          34.36%     65.64%

# For 1954-1965, loop through each column and update values
year_columns <- paste0(1953:1965, "_COMPLETED_PRIVATE")

for (i in length(year_columns):2) {
  mastersheet[[year_columns[i]]] <- mastersheet[[year_columns[i]]] - mastersheet[[year_columns[i - 1]]]
}

# Because we don't have data for 1947-1949 and 1952, we need to calculate the figures for 1950 and 1953 manually by proportion.
mastersheet[["1953_COMPLETED_PRIVATE"]] <- (mastersheet[["1953_COMPLETED_PRIVATE"]] - mastersheet[["1951_COMPLETED_PRIVATE"]])*0.6564
mastersheet[["1952_COMPLETED_PRIVATE"]] <- mastersheet[["1953_COMPLETED_PRIVATE"]] * (0.3436/0.6564)
mastersheet[["1951_COMPLETED_PRIVATE"]] <- mastersheet[["1951_COMPLETED_PRIVATE"]] - mastersheet[["1950_COMPLETED_PRIVATE"]]
mastersheet[["1950_COMPLETED_PRIVATE"]] <- (mastersheet[["1950_COMPLETED_PRIVATE"]] - mastersheet[["1946_COMPLETED_PRIVATE"]])*0.2143
mastersheet[["1949_COMPLETED_PRIVATE"]] <- mastersheet[["1950_COMPLETED_PRIVATE"]] * (0.2015/0.2143)
mastersheet[["1948_COMPLETED_PRIVATE"]] <- mastersheet[["1950_COMPLETED_PRIVATE"]] * (0.2571/0.2143)
mastersheet[["1947_COMPLETED_PRIVATE"]] <- mastersheet[["1950_COMPLETED_PRIVATE"]] * (0.3271/0.2143) 

#re-apportion the worst of the 1955 spikes 
#proportion of the difference our total national private built (1946-1954) after above apportionment and national statistics that is due to difference in each year
#- to use to re-apportion the spike backwards from 1955 
#1946     1947    1948    1949    1950    1951    1952    1953    1954
#18.6%    23.3%   18.3%   14.4%   15.3%   4.1%    1.7%    3.2%    1.1%

#work out the average housebuilding in 1954, 1956-58 for each place 
mastersheet$ave54_58_COMPLETED_PRIVATE <- (mastersheet[["1954_COMPLETED_PRIVATE"]] + mastersheet[["1956_COMPLETED_PRIVATE"]] + mastersheet[["1957_COMPLETED_PRIVATE"]]
                                           + mastersheet[["1958_COMPLETED_PRIVATE"]])/4 

#calculate ratio of 1955 to the average 1954-1958 housebuilding 
mastersheet$ratio55_5458 <- mastersheet[["1955_COMPLETED_PRIVATE"]] / mastersheet[["ave54_58_COMPLETED_PRIVATE"]]   

#replace any NaNs with ratio 1 
mastersheet$ratio55_5458[is.nan(mastersheet$ratio55_5458)] <- 1

#create new column with total N houses to backward apportion from 1955
mastersheet <- mastersheet %>%
  mutate(to_apportion = if_else(
    ratio55_5458 > 1.5,                           # Condition to check
    0.8 * (`1955_COMPLETED_PRIVATE` - `ave54_58_COMPLETED_PRIVATE`),  # Value if condition is TRUE
    0                                             # Value if condition is FALSE
  ))

mastersheet[["1954_COMPLETED_PRIVATE"]] <- mastersheet[["1954_COMPLETED_PRIVATE"]] + (mastersheet[["to_apportion"]] * 0.011)
mastersheet[["1953_COMPLETED_PRIVATE"]] <- mastersheet[["1953_COMPLETED_PRIVATE"]] + (mastersheet[["to_apportion"]] * 0.032)
mastersheet[["1952_COMPLETED_PRIVATE"]] <- mastersheet[["1952_COMPLETED_PRIVATE"]] + (mastersheet[["to_apportion"]] * 0.017)
mastersheet[["1951_COMPLETED_PRIVATE"]] <- mastersheet[["1951_COMPLETED_PRIVATE"]] + (mastersheet[["to_apportion"]] * 0.041)
mastersheet[["1950_COMPLETED_PRIVATE"]] <- mastersheet[["1950_COMPLETED_PRIVATE"]] + (mastersheet[["to_apportion"]] * 0.153)
mastersheet[["1949_COMPLETED_PRIVATE"]] <- mastersheet[["1949_COMPLETED_PRIVATE"]] + (mastersheet[["to_apportion"]] * 0.144)
mastersheet[["1948_COMPLETED_PRIVATE"]] <- mastersheet[["1948_COMPLETED_PRIVATE"]] + (mastersheet[["to_apportion"]] * 0.183)
mastersheet[["1947_COMPLETED_PRIVATE"]] <- mastersheet[["1947_COMPLETED_PRIVATE"]] + (mastersheet[["to_apportion"]] * 0.233)
mastersheet[["1946_COMPLETED_PRIVATE"]] <- mastersheet[["1946_COMPLETED_PRIVATE"]] + (mastersheet[["to_apportion"]] * 0.186) 

#take the number off 1955 to kill the spike 
mastersheet[["1955_COMPLETED_PRIVATE"]] <- mastersheet[["1955_COMPLETED_PRIVATE"]] - (mastersheet[["to_apportion"]])

#we also need to adjust our 'completed all' totals so they also take into account the pre-apportioned private  
mastersheet[["1954_COMPLETED_ALL"]] <- mastersheet[["1954_COMPLETED_ALL"]] + (mastersheet[["to_apportion"]] * 0.011)
mastersheet[["1953_COMPLETED_ALL"]] <- mastersheet[["1953_COMPLETED_ALL"]] + (mastersheet[["to_apportion"]] * 0.032)
mastersheet[["1952_COMPLETED_ALL"]] <- mastersheet[["1952_COMPLETED_ALL"]] + (mastersheet[["to_apportion"]] * 0.017)
mastersheet[["1951_COMPLETED_ALL"]] <- mastersheet[["1951_COMPLETED_ALL"]] + (mastersheet[["to_apportion"]] * 0.041)
mastersheet[["1950_COMPLETED_ALL"]] <- mastersheet[["1950_COMPLETED_ALL"]] + (mastersheet[["to_apportion"]] * 0.153)
mastersheet[["1949_COMPLETED_ALL"]] <- mastersheet[["1949_COMPLETED_ALL"]] + (mastersheet[["to_apportion"]] * 0.144)
mastersheet[["1948_COMPLETED_ALL"]] <- mastersheet[["1948_COMPLETED_ALL"]] + (mastersheet[["to_apportion"]] * 0.183)
mastersheet[["1947_COMPLETED_ALL"]] <- mastersheet[["1947_COMPLETED_ALL"]] + (mastersheet[["to_apportion"]] * 0.233)
mastersheet[["1946_COMPLETED_ALL"]] <- mastersheet[["1946_COMPLETED_ALL"]] + (mastersheet[["to_apportion"]] * 0.186) 

#take the number off 1955 to kill the spike 
mastersheet[["1955_COMPLETED_ALL"]] <- mastersheet[["1955_COMPLETED_ALL"]] - (mastersheet[["to_apportion"]])


# LA built
#percentage of national local housebuilding done in years 1947-1950, and 1952-53 - so we can apportion our missing years. 
#1947    1948    1949    1950        1952    1953
#15.76%     31.28%     26.51%      26.45%        44.76%     55.24%

# For 1954-1965, loop through each column and update values
year_columns <- paste0(1953:1965, "_COMPLETED_LOCAL")

for (i in length(year_columns):2) {
  mastersheet[[year_columns[i]]] <- mastersheet[[year_columns[i]]] - mastersheet[[year_columns[i - 1]]]
}

# Because we don't have data for 1947-1949 and 1952, we need to calculate the figures for 1950 and 1953 manually by proportion.
mastersheet[["1953_COMPLETED_LOCAL"]] <- (mastersheet[["1953_COMPLETED_LOCAL"]] - mastersheet[["1951_COMPLETED_LOCAL"]])*0.5524
mastersheet[["1952_COMPLETED_LOCAL"]] <- mastersheet[["1953_COMPLETED_LOCAL"]] * (0.4476/0.5524)
mastersheet[["1951_COMPLETED_LOCAL"]] <- mastersheet[["1951_COMPLETED_LOCAL"]] - mastersheet[["1950_COMPLETED_LOCAL"]]
mastersheet[["1950_COMPLETED_LOCAL"]] <- (mastersheet[["1950_COMPLETED_LOCAL"]] - mastersheet[["1946_COMPLETED_LOCAL"]])*0.2645
mastersheet[["1949_COMPLETED_LOCAL"]] <- mastersheet[["1950_COMPLETED_LOCAL"]] * (0.2651/0.2645)
mastersheet[["1948_COMPLETED_LOCAL"]] <- mastersheet[["1950_COMPLETED_LOCAL"]] * (0.3128/0.2645)
mastersheet[["1947_COMPLETED_LOCAL"]] <- mastersheet[["1950_COMPLETED_LOCAL"]] * (0.1576/0.2645) 


# HA built (column _HA only available since 1962)
# For 1963-1965, loop through each column and update values
year_columns <- paste0(1962:1965, "_COMPLETED_HA")

for (i in length(year_columns):2) {
  mastersheet[[year_columns[i]]] <- mastersheet[[year_columns[i]]] - mastersheet[[year_columns[i - 1]]]
}


# Public built
#percentage of national public housebuilding done in years 1947-1950, and 1952-53 - so we can apportion our missing years. 
#1947    1948    1949    1950        1952    1953


# For 1954-1965, loop through each column and update values
year_columns <- paste0(1953:1965, "_COMPLETED_PUBLIC")

for (i in length(year_columns):2) {
  mastersheet[[year_columns[i]]] <- mastersheet[[year_columns[i]]] - mastersheet[[year_columns[i - 1]]]
}

# Because we don't have data for 1947-1949 and 1952, we need to calculate the figures for 1950 and 1953 manually by proportion.
mastersheet[["1953_COMPLETED_PUBLIC"]] <- (mastersheet[["1953_COMPLETED_PUBLIC"]] - mastersheet[["1951_COMPLETED_PUBLIC"]])*0.5524
mastersheet[["1952_COMPLETED_PUBLIC"]] <- mastersheet[["1953_COMPLETED_PUBLIC"]] * (0.4576/0.5524)
mastersheet[["1951_COMPLETED_PUBLIC"]] <- mastersheet[["1951_COMPLETED_PUBLIC"]] - mastersheet[["1950_COMPLETED_PUBLIC"]]
mastersheet[["1950_COMPLETED_PUBLIC"]] <- (mastersheet[["1950_COMPLETED_PUBLIC"]] - mastersheet[["1946_COMPLETED_PUBLIC"]])*0.2645
mastersheet[["1949_COMPLETED_PUBLIC"]] <- mastersheet[["1950_COMPLETED_PUBLIC"]] * (0.2651/0.2645)
mastersheet[["1948_COMPLETED_PUBLIC"]] <- mastersheet[["1950_COMPLETED_PUBLIC"]] * (0.3128/0.2645)
mastersheet[["1947_COMPLETED_PUBLIC"]] <- mastersheet[["1950_COMPLETED_PUBLIC"]] * (0.1576/0.2645) 


# Demolished
# For 1957-1965, loop through each column and update values
year_columns <- paste0(1956:1965, "_Total_demolished")

for (i in length(year_columns):2) {
  mastersheet[[year_columns[i]]] <- mastersheet[[year_columns[i]]] - mastersheet[[year_columns[i - 1]]]
}

# Because we don't have data for 1947-1949 and 1952, we need to calculate the figures for 1950 and 1953 manually by proportion.
mastersheet[["1953_Total_demolished"]] <- (mastersheet[["1953_Total_demolished"]] - mastersheet[["1951_Total_demolished"]]) / 2
mastersheet[["1952_Total_demolished"]] <- mastersheet[["1953_Total_demolished"]] 
mastersheet[["1951_Total_demolished"]] <- mastersheet[["1951_Total_demolished"]] - mastersheet[["1950_Total_demolished"]]
mastersheet[["1950_Total_demolished"]] <- (mastersheet[["1950_Total_demolished"]] - mastersheet[["1946_Total_demolished"]]) / 4
mastersheet[["1949_Total_demolished"]] <- mastersheet[["1950_Total_demolished"]] 
mastersheet[["1948_Total_demolished"]] <- mastersheet[["1950_Total_demolished"]] 
mastersheet[["1947_Total_demolished"]] <- mastersheet[["1950_Total_demolished"]] 


# Add a new column 'Net_completed' which is COMPLETED_ALL - Total_Demolished-----------------------------------------------------------------------
years <- c(1946:1973) 

for (year in years) {
  completed_cols <- grep("COMPLETED_ALL$", colnames(mastersheet), value = TRUE)
  if (length(completed_cols) > 0) {
    
    col_name_completed <- paste0(year, "_COMPLETED_ALL")
    col_name_net_completed <- paste0(year, "_Net_completed")
    col_name_total_demolished <- paste0(year, "_Total_demolished")
    
    mastersheet[[col_name_net_completed]] <- mastersheet[[col_name_completed]] - mastersheet[[col_name_total_demolished]]
  } else {
    col_name_net_completed <- paste0(year, "_Net_completed")
    mastersheet[[col_name_net_completed]] <- rep(NA, nrow(mastersheet))  # If no COMPLETED_ALL columns found
  }
}



###select data for different purposes
#-------------------------------------------------------------------------

#select data on total completions per year only ----------------
# List of columns containing 'COMPLETED_ALL' in their names
completed_all_cols <- grep("COMPLETED_ALL", names(mastersheet), value = TRUE)

# Select 'equiv_1971_cd' and columns containing 'COMPLETED_ALL'
completed_all <- mastersheet %>%
  select(equiv_1971_cd, equiv_1971_nm, all_of(completed_all_cols))

# Extract the column names starting from the 3rd column
year_columns <- names(completed_all)[3:ncol(completed_all)]

# Extract the year part from the column names and convert to numeric for sorting
sorted_year_columns <- year_columns[order(as.numeric(sub("_.*", "", year_columns)))]

# Reorder the dataframe columns
completed_all <- completed_all %>%
  select(1:2, all_of(sorted_year_columns))


# Select data on public housebuilding-------------------
completed_public <- grep("COMPLETED_PUBLIC", names(mastersheet), value = TRUE)

completed_public <- mastersheet %>%
  select(equiv_1971_cd, equiv_1971_nm, all_of(completed_public))

# Extract the column names starting from the 3rd column
year_columns <- names(completed_public)[3:ncol(completed_public)]

# Extract the year part from the column names and convert to numeric for sorting
sorted_year_columns <- year_columns[order(as.numeric(sub("_.*", "", year_columns)))]

# Reorder the dataframe columns
completed_public <- completed_public %>%
  select(1:2, all_of(sorted_year_columns))


#select data on net_completions 1946-79, and then total completions 1980-1973 ------------------
net_completed_all_cols <- grep("Net_completed", names(mastersheet), value = TRUE) 

net_completed_all <- mastersheet %>% 
  select(equiv_1971_cd, all_of(net_completed_all_cols))

# Extract the column names starting from the 3rd column
year_columns <- names(net_completed_all)[3:ncol(net_completed_all)]

# Extract the year part from the column names and convert to numeric for sorting
sorted_year_columns <- year_columns[order(as.numeric(sub("_.*", "", year_columns)))]

# Reorder the dataframe columns
net_completed_all <- net_completed_all %>%
  select(1:2, all_of(sorted_year_columns))



#select data on local authority completions per year only ------------------
completed_local_cols <- grep("_COMPLETED_LOCAL", names(mastersheet), value = TRUE)

completed_local <- mastersheet %>%
  select(equiv_1971_cd, equiv_1971_nm, all_of(completed_local_cols)) %>% select(-contains("WAR"))

# Extract the column names starting from the 3rd column
year_columns <- names(completed_local)[3:ncol(completed_local)]

# Extract the year part from the column names and convert to numeric for sorting
sorted_year_columns <- year_columns[order(as.numeric(sub("_.*", "", year_columns)))]

# Reorder the dataframe columns
completed_local <- completed_local %>%
  select(1:2, all_of(sorted_year_columns))


#select data on housing association completions per year only -----------------
completed_HA_cols <- grep("COMPLETED_HA", names(mastersheet), value = TRUE)

completed_HA <- mastersheet %>%
  select(equiv_1971_cd, equiv_1971_nm, all_of(completed_HA_cols))

# Extract the column names starting from the 3rd column
year_columns <- names(completed_HA)[3:ncol(completed_HA)]

# Extract the year part from the column names and convert to numeric for sorting
sorted_year_columns <- year_columns[order(as.numeric(sub("_.*", "", year_columns)))]

# Reorder the dataframe columns
completed_HA <- completed_HA %>%
  select(1:2, all_of(sorted_year_columns))


#select data on private completions per year only -----------------
completed_private_cols <- grep("COMPLETED_PRIVATE", names(mastersheet), value = TRUE)

completed_private <- mastersheet %>%
  select(equiv_1971_cd, equiv_1971_nm, all_of(completed_private_cols)) %>% select(-contains("WAR"))

#select data on demolished per year only -----------------
completed_demolished_cols <- grep("Total_demolished", names(mastersheet), value = TRUE)

completed_demolished <- mastersheet %>%
  select(equiv_1971_cd, equiv_1971_nm, all_of(completed_demolished_cols))

# Extract the column names starting from the 3rd column
year_columns <- names(completed_private)[3:ncol(completed_private)]

# Extract the year part from the column names and convert to numeric for sorting
sorted_year_columns <- year_columns[order(as.numeric(sub("_.*", "", year_columns)))]

# Reorder the dataframe columns
completed_private <- completed_private %>%
  select(1:2, all_of(sorted_year_columns))


#select data on population in each year------------------------------
pop_all_cols <- grep("pop", names(mastersheet), value = TRUE, ignore.case = TRUE)

population_all <- mastersheet %>%
  select(equiv_1971_cd, equiv_1971_nm, all_of(pop_all_cols)) 

# Create a sequence of new names from "1946_POP" to "1973_POP"
years <- c(1946,1950,1951,1953:1973) 
new_names <- paste0(years, "_POP")
names(population_all)[names(population_all) %in% pop_all_cols] <- new_names[1:length(pop_all_cols)]

population_all$`1952_POP` <-  population_all$`1951_POP` + (population_all$`1953_POP` - population_all$`1951_POP`)/2 
population_all$`1949_POP` <-  population_all$`1946_POP` + (population_all$`1950_POP` - population_all$`1946_POP`)*0.75 
population_all$`1948_POP` <-  population_all$`1946_POP` + (population_all$`1950_POP` - population_all$`1946_POP`)*0.5 
population_all$`1947_POP` <-  population_all$`1946_POP` + (population_all$`1950_POP` - population_all$`1946_POP`)*0.25

# Extract the column names starting from the 3rd column
year_columns <- names(population_all)[3:ncol(population_all)]

# Extract the year part from the column names and convert to numeric for sorting
sorted_year_columns <- year_columns[order(as.numeric(sub("_.*", "", year_columns)))]

# Reorder the dataframe columns
population_all <- population_all %>%
  select(1:2, all_of(sorted_year_columns))


#Export New town--------------------------------------------------------------------------------------------------------------------
new_towns_all <- new_towns_1973 %>% select(matches("polygon_code"))

for (i in 2:24) {
  temp <- new_towns[[i]]
  
  # Find the index of the column that contains "pop"
  pop_col_index <- which(grepl("pop", colnames(new_towns[[i]]), ignore.case = TRUE))[1] # Assuming first match
  
  # Select "polygon_code" and all columns after the "pop" column
  temp <- new_towns[[i]] %>%
    select(polygon_code, (pop_col_index + 1):ncol(new_towns[[i]]))

  new_towns_all <- full_join(new_towns_all, temp, by = "polygon_code") %>%
    mutate(across(everything(), ~ replace_na(., 0)))  # Replace all NA values with 0
}


#Because the completions are cumulative (from 1st April 1945) up to 1965. Since 1966, they become annual.For now, let's do annual.
#------------------------------------------------------------------------  

#note: I spread 1953-1951 difference evenly over 1952 and 1953, rather than according to national trend as I do above, because unclear that what was happening in the
#first round of NTs would have followed the national building trends - so easier to just assume even split. Doesn't impact phase 2 or 3 NTs. 

# total built
# For 1954-1965, Loop through each column and update values
year_columns <- paste0(1953:1965, "_COMPLETED_ALL")

for (i in length(year_columns):2) {
  new_towns_all[[year_columns[i]]] <- new_towns_all[[year_columns[i]]] - new_towns_all[[year_columns[i - 1]]]
}

new_towns_all[["1953_COMPLETED_ALL"]] <- (new_towns_all[["1953_COMPLETED_ALL"]] - new_towns_all[["1951_COMPLETED_ALL"]])/2
new_towns_all[["1952_COMPLETED_ALL"]] <- new_towns_all[["1953_COMPLETED_ALL"]]
new_towns_all[["1951_COMPLETED_ALL"]] <- new_towns_all[["1951_COMPLETED_ALL"]] - new_towns_all[["1950_COMPLETED_ALL"]]

# Private built
# For 1954-1965, loop through each column and update values
year_columns <- paste0(1953:1965, "_COMPLETED_PRIVATE")

for (i in length(year_columns):2) {
  new_towns_all[[year_columns[i]]] <- new_towns_all[[year_columns[i]]] - new_towns_all[[year_columns[i - 1]]]
}

new_towns_all[["1953_COMPLETED_PRIVATE"]] <- (new_towns_all[["1953_COMPLETED_PRIVATE"]] - new_towns_all[["1951_COMPLETED_PRIVATE"]]) / 2
new_towns_all[["1952_COMPLETED_PRIVATE"]] <- new_towns_all[["1953_COMPLETED_PRIVATE"]]
new_towns_all[["1951_COMPLETED_PRIVATE"]] <- new_towns_all[["1951_COMPLETED_PRIVATE"]] - new_towns_all[["1950_COMPLETED_PRIVATE"]]


# LA built
# For 1954-1965, loop through each column and update values
year_columns <- paste0(1953:1965, "_COMPLETED_LOCAL")

for (i in length(year_columns):2) {
  new_towns_all[[year_columns[i]]] <- new_towns_all[[year_columns[i]]] - new_towns_all[[year_columns[i - 1]]]
}

new_towns_all[["1953_COMPLETED_LOCAL"]] <- (new_towns_all[["1953_COMPLETED_LOCAL"]] - new_towns_all[["1951_COMPLETED_LOCAL"]]) / 2
new_towns_all[["1952_COMPLETED_LOCAL"]] <- new_towns_all[["1953_COMPLETED_LOCAL"]]
new_towns_all[["1951_COMPLETED_LOCAL"]] <- new_towns_all[["1951_COMPLETED_LOCAL"]] - new_towns_all[["1950_COMPLETED_LOCAL"]]


# HA built (column _HA only available since 1962)
# For 1963-1965, loop through each column and update values
year_columns <- paste0(1962:1965, "_COMPLETED_HA")

for (i in length(year_columns):2) {
  new_towns_all[[year_columns[i]]] <- new_towns_all[[year_columns[i]]] - new_towns_all[[year_columns[i - 1]]]
}


# Public built
# For 1954-1965, loop through each column and update values
year_columns <- paste0(1953:1965, "_COMPLETED_PUBLIC")

for (i in length(year_columns):2) {
  new_towns_all[[year_columns[i]]] <- new_towns_all[[year_columns[i]]] - new_towns_all[[year_columns[i - 1]]]
}

new_towns_all[["1953_COMPLETED_PUBLIC"]] <- (new_towns_all[["1953_COMPLETED_PUBLIC"]] - new_towns_all[["1951_COMPLETED_PUBLIC"]]) / 2
new_towns_all[["1952_COMPLETED_PUBLIC"]] <- new_towns_all[["1953_COMPLETED_PUBLIC"]]
new_towns_all[["1951_COMPLETED_PUBLIC"]] <- new_towns_all[["1951_COMPLETED_PUBLIC"]] - new_towns_all[["1950_COMPLETED_PUBLIC"]]


# Demolished
# For 1954-1965, loop through each column and update values
year_columns <- paste0(1953:1965, "_Total_demolished")

for (i in length(year_columns):2) {
  new_towns_all[[year_columns[i]]] <- new_towns_all[[year_columns[i]]] - new_towns_all[[year_columns[i - 1]]]
}

new_towns_all[["1953_Total_demolished"]] <- (new_towns_all[["1953_Total_demolished"]] - new_towns_all[["1951_Total_demolished"]]) / 2
new_towns_all[["1952_Total_demolished"]] <- new_towns_all[["1953_Total_demolished"]]
new_towns_all[["1951_Total_demolished"]] <- new_towns_all[["1951_Total_demolished"]] - new_towns_all[["1950_Total_demolished"]]


# Net built
#Add a new column 'Net_completed' which is COMPLETED_ALL - Total_Demolished
years <- c(1950:1973) 

for (year in years) {
  completed_cols <- grep("COMPLETED_ALL$", colnames(new_towns_all), value = TRUE)
  if (length(completed_cols) > 0) {
    
    col_name_completed <- paste0(year, "_COMPLETED_ALL")
    col_name_net_completed <- paste0(year, "_Net_completed")
    col_name_total_demolished <- paste0(year, "_Total_demolished")
    
    new_towns_all[[col_name_net_completed]] <- new_towns_all[[col_name_completed]] - new_towns_all[[col_name_total_demolished]]
  } else {
    col_name_net_completed <- paste0(year, "_Net_completed")
    new_towns_all[[col_name_net_completed]] <- rep(NA, nrow(new_towns_all))  # If no COMPLETED_ALL columns found
  }
}



###select data for different purposes

#select data on total completions per year only
#-------------------------------------------------------------------------
# List of columns containing 'COMPLETED_ALL' in their names
NT_completed_cols <- grep("COMPLETED_ALL", names(new_towns_all), value = TRUE) 

# Select 'equiv_1971_cd' and columns containing 'COMPLETED_ALL'
NT_completed <- new_towns_all %>%
  select(polygon_code, all_of(NT_completed_cols))%>%
  left_join(geog_names, by = "polygon_code") 

# Extract the column names starting from the 3rd column
year_columns <- names(NT_completed)[3:ncol(NT_completed)]

# Extract the year part from the column names and convert to numeric for sorting
sorted_year_columns <- year_columns[order(as.numeric(sub("_.*", "", year_columns)))]

# Reorder the dataframe columns
NT_completed <- NT_completed %>%
  select(1:2, all_of(sorted_year_columns))


#add county-level "COMPLETED_ALL" data 
cty_completed <- left_join(completed_all, geog_names, by = "equiv_1971_cd") %>%
  group_by(cty_1981) %>%
  summarize(across(where(is.numeric), sum, na.rm = TRUE))

NTcty_completed <- cty_completed %>%
  filter(cty_1981 %in% NT_completed$cty_1981)|> 
  rename_with(~ paste0(., "_cty"), .cols = contains("COMPLETED_ALL"))

#See what percentage of the county total built comes from each new town.-------------------------
NT_cty_completed <-left_join(NT_completed, NTcty_completed, by = "cty_1981") %>%
  mutate(
    across(
      all_of(NT_completed_cols),
      ~ {
        cty_col <- get(paste0(cur_column(), "_cty"))
        case_when(
          is.na(.) ~ NA_real_,  # Check if the value in columns_to_mutate is NA
          is.na(cty_col) ~ NA_real_,     # Check if the cty column is NA
          cty_col == 0 ~ NA_real_,       # Avoid division by zero
          TRUE ~ . / cty_col * 100
        )
      },
      .names = "{str_sub(.col, 1, 4)}"
    )
  )

#See what percentage of the county total built comes from new towns.-------------------------
NTs_completed <- NT_completed %>%
  group_by(cty_1981) %>%
  summarize(across(where(is.numeric), sum, na.rm = TRUE))

NTs_cty_completed <-left_join(NTs_completed, NTcty_completed, by = "cty_1981") %>%
  mutate(
    across(
      all_of(NT_completed_cols),
      ~ {
        cty_col <- get(paste0(cur_column(), "_cty"))
        case_when(
          is.na(.) ~ NA_real_,  # Check if the value in columns_to_mutate is NA
          is.na(cty_col) ~ NA_real_,     # Check if the cty column is NA
          cty_col == 0 ~ NA_real_,       # Avoid division by zero
          TRUE ~ . / cty_col * 100
        )
      },
      .names = "{str_sub(.col, 1, 4)}"
    )
  )

#Add back county lookup
geog_cty<-geog_names %>% group_by(cty_1981)%>% summarize(across(everything(), ~ first(.), .names = "{.col}")) |> select(1,4:5)

NTs_cty_completed <- left_join(NTs_cty_completed, geog_cty, by = "cty_1981")




#See what percentage of the area (equiv_1971_cd) total built comes from new towns.-------------------------
#add area-level "COMPLETED_ALL" data 
area_completed <- left_join(completed_all, geog_names, by = "equiv_1971_cd") %>%
  group_by(equiv_1971_cd) %>%
  summarize(across(where(is.numeric), sum, na.rm = TRUE))

NTarea_completed <- completed_all %>%
  filter(equiv_1971_cd %in% NT_completed$equiv_1971_cd)|> 
  rename_with(~ paste0(., "_area"), .cols = contains("COMPLETED_ALL"))

#Sum new towns data
NTs_completed <- NT_completed %>%
  group_by(equiv_1971_cd) %>%
  summarize(across(where(is.numeric), sum, na.rm = TRUE))

NTs_area_completed <-left_join(NTs_completed, NTarea_completed, by = "equiv_1971_cd") %>%
  mutate(
    across(
      all_of(NT_completed_cols),
      ~ {
        area_col <- get(paste0(cur_column(), "_area"))
        case_when(
          is.na(.) ~ NA_real_,  # Check if the value in columns_to_mutate is NA
          is.na(area_col) ~ NA_real_,     # Check if the cty column is NA
          area_col == 0 ~ NA_real_,       # Avoid division by zero
          TRUE ~ . / area_col * 100
        )
      },
      .names = "{str_sub(.col, 1, 4)}"
    )
  )

#Add back county lookup
geog_area<-geog_names %>% group_by(equiv_1971_cd)%>% summarize(across(everything(), ~ first(.), .names = "{.col}")) |> select(1:9)

NTs_area_completed <- left_join(NTs_area_completed, geog_area, by = "equiv_1971_cd")





#select data on local authority completions per year only 
#-------------------------------------------------------------------------
NT_completed_local_cols <- grep("COMPLETED_LOCAL", names(new_towns_all), value = TRUE)

NT_completed_local <- new_towns_all %>%
  select(polygon_code,  all_of(NT_completed_local_cols))%>%
  left_join(geog_names, by = "polygon_code") 

#because 1946-1954 have extra WAR columns, so sum them into the according local column first
completed_local <- completed_local %>%
  mutate(across(ends_with("_WAR"), 
                .fns = ~ . + get(gsub("_WAR$", "", cur_column())), 
                .names = "{gsub('_WAR$', '', .col)}")) %>%
  select(-ends_with("_WAR"))

NT_completed_local <- NT_completed_local %>%
  mutate(across(ends_with("_WAR"), 
                .fns = ~ . + get(gsub("_WAR$", "", cur_column())), 
                .names = "{gsub('_WAR$', '', .col)}")) %>%
  select(-ends_with("_WAR"))

NT_completed_local_cols <- grep("COMPLETED_LOCAL", names(NT_completed_local), value = TRUE)


# Extract the column names starting from the 3rd column
year_columns <- names(NT_completed_local)[3:ncol(NT_completed_local)]

# Extract the year part from the column names and convert to numeric for sorting
sorted_year_columns <- year_columns[order(as.numeric(sub("_.*", "", year_columns)))]

# Reorder the dataframe columns
NT_completed_local <- NT_completed_local %>%
  select(1:2, all_of(sorted_year_columns))


#add county-level "COMPLETED_LOCAL" data 
cty_completed_local <- left_join(completed_local, geog_names, by = "equiv_1971_cd") %>%
  group_by(cty_1981) %>%
  summarize(across(where(is.numeric), sum, na.rm = TRUE))

NTcty_completed_local <- cty_completed_local %>%
  filter(cty_1981 %in% NT_completed_local$cty_1981)|> 
  rename_with(~ paste0(., "_cty"), .cols = contains("COMPLETED_LOCAL"))

#See what percentage of the county total built comes from each new town.-------------------------
NT_cty_completed_local <-left_join(NT_completed_local, NTcty_completed_local, by = "cty_1981") %>%
  mutate(
    across(
      all_of(NT_completed_local_cols),
      ~ {
        cty_col <- get(paste0(cur_column(), "_cty"))
        case_when(
          is.na(.) ~ NA_real_,  # Check if the value in columns_to_mutate is NA
          is.na(cty_col) ~ NA_real_,     # Check if the cty column is NA
          cty_col == 0 ~ NA_real_,       # Avoid division by zero
          TRUE ~ . / cty_col * 100
        )
      },
      .names = "{str_sub(.col, 1, 4)}"
    )
  )

#See what percentage of the county total built comes from new towns.-------------------------
NTs_completed_local <- NT_completed_local %>%
  group_by(cty_1981) %>%
  summarize(across(where(is.numeric), sum, na.rm = TRUE))

NTs_cty_completed_local <-left_join(NTs_completed_local, NTcty_completed_local, by = "cty_1981") %>%
  mutate(
    across(
      all_of(NT_completed_local_cols),
      ~ {
        cty_col <- get(paste0(cur_column(), "_cty"))
        case_when(
          is.na(.) ~ NA_real_,  # Check if the value in columns_to_mutate is NA
          is.na(cty_col) ~ NA_real_,     # Check if the cty column is NA
          cty_col == 0 ~ NA_real_,       # Avoid division by zero
          TRUE ~ . / cty_col * 100
        )
      },
      .names = "{str_sub(.col, 1, 4)}"
    )
  )

#Add back county lookup
geog_cty<-geog_names %>% group_by(cty_1981)%>% summarize(across(everything(), ~ first(.), .names = "{.col}")) |> select(1:5)

NTs_cty_completed_local <- left_join(NTs_cty_completed_local, geog_cty, by = "cty_1981")


#See what percentage of the area (equiv_1971_cd) local built comes from new towns.-------------------------
#add area-level "COMPLETED_LOCAL" data 
area_completed_local <- left_join(completed_local, geog_names, by = "equiv_1971_cd") %>%
  group_by(equiv_1971_cd) %>%
  summarize(across(where(is.numeric), sum, na.rm = TRUE))

NTarea_completed_local <- completed_local %>%
  filter(equiv_1971_cd %in% NT_completed_local$equiv_1971_cd)|> 
  rename_with(~ paste0(., "_area"), .cols = contains("COMPLETED_LOCAL"))

#Sum new towns data
NTs_completed_local <- NT_completed_local %>%
  group_by(equiv_1971_cd) %>%
  summarize(across(where(is.numeric), sum, na.rm = TRUE))

NTs_area_completed_local <-left_join(NTs_completed_local, NTarea_completed_local, by = "equiv_1971_cd") %>%
  mutate(
    across(
      all_of(NT_completed_local_cols),
      ~ {
        area_col <- get(paste0(cur_column(), "_area"))
        case_when(
          is.na(.) ~ NA_real_,  # Check if the value in columns_to_mutate is NA
          is.na(area_col) ~ NA_real_,     # Check if the cty column is NA
          area_col == 0 ~ NA_real_,       # Avoid division by zero
          TRUE ~ . / area_col * 100
        )
      },
      .names = "{str_sub(.col, 1, 4)}"
    )
  )

#Add back county lookup
geog_area<-geog_names %>% group_by(equiv_1971_cd)%>% summarize(across(everything(), ~ first(.), .names = "{.col}")) |> select(1:9)

NTs_area_completed_local <- left_join(NTs_area_completed_local, geog_area, by = "equiv_1971_cd")



#select data on housing association completions per year only 
#-------------------------------------------------------------------------
NT_completed_HA_cols <- grep("COMPLETED_HA", names(new_towns_all), value = TRUE)

NT_completed_HA <- new_towns_all %>%
  select(polygon_code,  all_of(NT_completed_HA_cols))%>%
  left_join(geog_names, by = "polygon_code") 


#because 1946-1954 have extra WAR columns, so sum them into the according HA column first
completed_HA <- completed_HA %>%
  mutate(across(ends_with("_WAR"), 
                .fns = ~ . + get(gsub("_WAR$", "", cur_column())), 
                .names = "{gsub('_WAR$', '', .col)}")) %>%
  select(-ends_with("_WAR"))

NT_completed_HA <- NT_completed_HA %>%
  mutate(across(ends_with("_WAR"), 
                .fns = ~ . + get(gsub("_WAR$", "", cur_column())), 
                .names = "{gsub('_WAR$', '', .col)}")) %>%
  select(-ends_with("_WAR"))

NT_completed_HA_cols <- grep("COMPLETED_HA", names(NT_completed_HA), value = TRUE)


# Extract the column names starting from the 3rd column
year_columns <- names(NT_completed_HA)[3:ncol(NT_completed_HA)]

# Extract the year part from the column names and convert to numeric for sorting
sorted_year_columns <- year_columns[order(as.numeric(sub("_.*", "", year_columns)))]

# Reorder the dataframe columns
NT_completed_HA <- NT_completed_HA %>%
  select(1:2, all_of(sorted_year_columns))

#add county-level "COMPLETED_HA" data 
cty_completed_HA <- left_join(completed_HA, geog_names, by = "equiv_1971_cd") %>%
  group_by(cty_1981) %>%
  summarize(across(where(is.numeric), sum, na.rm = TRUE))

NTcty_completed_HA <- cty_completed_HA %>%
  filter(cty_1981 %in% NT_completed_HA$cty_1981)|> 
  rename_with(~ paste0(., "_cty"), .cols = contains("COMPLETED_HA"))

#See what percentage of the county total built comes from each new town.-------------------------
NT_cty_completed_HA <-left_join(NT_completed_HA, NTcty_completed_HA, by = "cty_1981") %>%
  mutate(
    across(
      all_of(NT_completed_HA_cols),
      ~ {
        cty_col <- get(paste0(cur_column(), "_cty"))
        case_when(
          is.na(.) ~ NA_real_,  # Check if the value in columns_to_mutate is NA
          is.na(cty_col) ~ NA_real_,     # Check if the cty column is NA
          cty_col == 0 ~ NA_real_,       # Avoid division by zero
          TRUE ~ . / cty_col * 100
        )
      },
      .names = "{str_sub(.col, 1, 4)}"
    )
  )

#See what percentage of the county total built comes from new towns.-------------------------
NTs_completed_HA <- NT_completed_HA %>%
  group_by(cty_1981) %>%
  summarize(across(where(is.numeric), sum, na.rm = TRUE))

NTs_cty_completed_HA <-left_join(NTs_completed_HA, NTcty_completed_HA, by = "cty_1981") %>%
  mutate(
    across(
      all_of(NT_completed_HA_cols),
      ~ {
        cty_col <- get(paste0(cur_column(), "_cty"))
        case_when(
          is.na(.) ~ NA_real_,  # Check if the value in columns_to_mutate is NA
          is.na(cty_col) ~ NA_real_,     # Check if the cty column is NA
          cty_col == 0 ~ NA_real_,       # Avoid division by zero
          TRUE ~ . / cty_col * 100
        )
      },
      .names = "{str_sub(.col, 1, 4)}"
    )
  )

#Add back county lookup
geog_cty<-geog_names %>% group_by(cty_1981)%>% summarize(across(everything(), ~ first(.), .names = "{.col}")) |> select(1:5)

NTs_cty_completed_HA <- left_join(NTs_cty_completed_HA, geog_cty, by = "cty_1981")


#See what percentage of the area (equiv_1971_cd) HA built comes from new towns.-------------------------
#add area-level "COMPLETED_HA" data 
area_completed_HA<- left_join(completed_HA, geog_names, by = "equiv_1971_cd") %>%
  group_by(equiv_1971_cd) %>%
  summarize(across(where(is.numeric), sum, na.rm = TRUE))

NTarea_completed_HA<- completed_HA %>%
  filter(equiv_1971_cd %in% NT_completed_HA$equiv_1971_cd)|> 
  rename_with(~ paste0(., "_area"), .cols = contains("COMPLETED_HA"))

#Sum new towns data
NTs_completed_HA<- NT_completed_HA%>%
  group_by(equiv_1971_cd) %>%
  summarize(across(where(is.numeric), sum, na.rm = TRUE))

NTs_area_completed_HA<-left_join(NTs_completed_HA, NTarea_completed_HA, by = "equiv_1971_cd") %>%
  mutate(
    across(
      all_of(NT_completed_HA_cols),
      ~ {
        area_col <- get(paste0(cur_column(), "_area"))
        case_when(
          is.na(.) ~ NA_real_,  # Check if the value in columns_to_mutate is NA
          is.na(area_col) ~ NA_real_,     # Check if the population column is NA
          area_col == 0 ~ NA_real_,       # Avoid division by zero
          TRUE ~ . / area_col * 100
        )
      },
      .names = "{str_sub(.col, 1, 4)}"
    )
  )

#Add back county lookup
geog_area<-geog_names %>% group_by(equiv_1971_cd)%>% summarize(across(everything(), ~ first(.), .names = "{.col}")) |> select(1:9)

NTs_area_completed_HA<- left_join(NTs_area_completed_HA, geog_area, by = "equiv_1971_cd")



#select data on private completions per year only 
#-------------------------------------------------------------------------
NT_completed_private_cols <- grep("COMPLETED_PRIVATE", names(new_towns_all), value = TRUE)

NT_completed_private <- new_towns_all %>%
  select(polygon_code,  all_of(NT_completed_private_cols))%>%
  left_join(geog_names, by = "polygon_code") 

#because 1946-1954 have extra WAR columns, so sum them into the according private column first
completed_private <- completed_private %>%
  mutate(across(ends_with("_WAR"), 
                .fns = ~ . + get(gsub("_WAR$", "", cur_column())), 
                .names = "{gsub('_WAR$', '', .col)}")) %>%
  select(-ends_with("_WAR"))

NT_completed_private <- NT_completed_private %>%
  mutate(across(ends_with("_WAR"), 
                .fns = ~ . + get(gsub("_WAR$", "", cur_column())), 
                .names = "{gsub('_WAR$', '', .col)}")) %>%
  select(-ends_with("_WAR"))

NT_completed_private_cols <- grep("COMPLETED_PRIVATE", names(NT_completed_private), value = TRUE)


# Extract the column names starting from the 3rd column
year_columns <- names(NT_completed_private)[3:ncol(NT_completed_private)]

# Extract the year part from the column names and convert to numeric for sorting
sorted_year_columns <- year_columns[order(as.numeric(sub("_.*", "", year_columns)))]

# Reorder the dataframe columns
NT_completed_private <- NT_completed_private %>%
  select(1:2, all_of(sorted_year_columns))

#add county-level "COMPLETED_private" data 
cty_completed_private <- left_join(completed_private, geog_names, by = "equiv_1971_cd") %>%
  group_by(cty_1981) %>%
  summarize(across(where(is.numeric), sum, na.rm = TRUE))

NTcty_completed_private <- cty_completed_private %>%
  filter(cty_1981 %in% NT_completed_private$cty_1981)|> 
  rename_with(~ paste0(., "_cty"), .cols = contains("COMPLETED_private"))

#See what percentage of the county total built comes from each new town.-------------------------
NT_cty_completed_private <-left_join(NT_completed_private, NTcty_completed_private, by = "cty_1981") %>%
  mutate(
    across(
      all_of(NT_completed_private_cols),
      ~ {
        cty_col <- get(paste0(cur_column(), "_cty"))
        case_when(
          is.na(.) ~ NA_real_,  # Check if the value in columns_to_mutate is NA
          is.na(cty_col) ~ NA_real_,     # Check if the population column is NA
          cty_col == 0 ~ NA_real_,       # Avoid division by zero
          TRUE ~ . / cty_col * 100
        )
      },
      .names = "{str_sub(.col, 1, 4)}"
    )
  )

#See what percentage of the county total built comes from new towns.-------------------------
NTs_completed_private <- NT_completed_private %>%
  group_by(cty_1981) %>%
  summarize(across(where(is.numeric), sum, na.rm = TRUE))

NTs_cty_completed_private <-left_join(NTs_completed_private, NTcty_completed_private, by = "cty_1981") %>%
  mutate(
    across(
      all_of(NT_completed_private_cols),
      ~ {
        cty_col <- get(paste0(cur_column(), "_cty"))
        case_when(
          is.na(.) ~ NA_real_,  # Check if the value in columns_to_mutate is NA
          is.na(cty_col) ~ NA_real_,     # Check if the population column is NA
          cty_col == 0 ~ NA_real_,       # Avoid division by zero
          TRUE ~ . / cty_col * 100
        )
      },
      .names = "{str_sub(.col, 1, 4)}"
    )
  )

#Add back county lookup
geog_cty<-geog_names %>% group_by(cty_1981)%>% summarize(across(everything(), ~ first(.), .names = "{.col}")) |> select(1:5)

NTs_cty_completed_private <- left_join(NTs_cty_completed_private, geog_cty, by = "cty_1981")


#See what percentage of the area (equiv_1971_cd) local built comes from new towns.-------------------------
#add area-level "COMPLETED_PRIVATE" data 
area_completed_private <- left_join(completed_private, geog_names, by = "equiv_1971_cd") %>%
  group_by(equiv_1971_cd) %>%
  summarize(across(where(is.numeric), sum, na.rm = TRUE))

NTarea_completed_private <- completed_private %>%
  filter(equiv_1971_cd %in% NT_completed_private$equiv_1971_cd)|> 
  rename_with(~ paste0(., "_area"), .cols = contains("COMPLETED_PRIVATE"))

#Sum new towns data
NTs_completed_private <- NT_completed_private %>%
  group_by(equiv_1971_cd) %>%
  summarize(across(where(is.numeric), sum, na.rm = TRUE))

NTs_area_completed_private <-left_join(NTs_completed_private, NTarea_completed_private, by = "equiv_1971_cd") %>%
  mutate(
    across(
      all_of(NT_completed_private_cols),
      ~ {
        area_col <- get(paste0(cur_column(), "_area"))
        case_when(
          is.na(.) ~ NA_real_,  # Check if the value in columns_to_mutate is NA
          is.na(area_col) ~ NA_real_,     # Check if the population column is NA
          area_col == 0 ~ NA_real_,       # Avoid division by zero
          TRUE ~ . / area_col * 100
        )
      },
      .names = "{str_sub(.col, 1, 4)}"
    )
  )

#Add back county lookup
geog_area<-geog_names %>% group_by(equiv_1971_cd)%>% summarize(across(everything(), ~ first(.), .names = "{.col}")) |> select(1:9)

NTs_area_completed_private <- left_join(NTs_area_completed_private, geog_area, by = "equiv_1971_cd")


#export to excel 
#---------------------------------------------------------------------------------------------------------------------------------------

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

# Generate file path for Excel workbook with date ----------------------
date <- format(Sys.Date(), "%Y-%m-%d")
file_name_geog <- paste0(date, "mastersheet_1946_1973", ".xlsx")
dir_path_geog = paste0(dirname("~"),"/Centre for Cities/Centre for Cities POC - Documents/Research/Housing/History of Planning 2/Data/Output/From R scripts/Housebuilding/")
file_path_geog <- file.path(dir_path_geog, file_name_geog)

# Create a list of data frames
df_list_geog <- list(mastersheet, new_towns_all, master_all, master_local, master_HA, master_private, master_demos)
names(df_list_geog) <- c("non_NT", "NT", "master_all", "master_local", "master_HA", "master_private", "master_demos")                         

# Export data frames to Excel
export_to_excel(df_list_geog, file_path_geog)


# Generate file path for Excel workbook with date ----------------------
date <- format(Sys.Date(), "%Y-%m-%d")
file_name_geog <- paste0(date, "built_and_population_1946_1973", ".xlsx")
dir_path_geog = paste0(dirname("~"),"/Centre for Cities/Centre for Cities POC - Documents/Research/Housing/History of Planning 2/Data/Output/From R scripts/Housebuilding/")
file_path_geog <- file.path(dir_path_geog, file_name_geog)

# Create a list of data frames
df_list_geog <- list(net_completed_all, completed_all, completed_private, completed_local, completed_HA, completed_public, population_all, completed_demolished)
names(df_list_geog) <- c("net built", "total built", "private built", "LA built", "HA built", "public built", "population","total demolished")                         

# Export data frames to Excel
export_to_excel(df_list_geog, file_path_geog)


# Generate file path for Excel workbook with New Town date ----------------------
date <- format(Sys.Date(), "%Y-%m-%d")
file_name_geog <- paste0(date, "new_town_1946_1973", ".xlsx")
dir_path_geog = paste0(dirname("~"),"/Centre for Cities/Centre for Cities POC - Documents/Research/Housing/History of Planning 2/Data/Output/From R scripts/Housebuilding/")
file_path_geog <- file.path(dir_path_geog, file_name_geog)

# Create a list of data frames
df_list_geog <- list(NT_cty_completed, NTs_cty_completed, NT_cty_completed_local, NTs_cty_completed_local, NT_cty_completed_HA, NTs_cty_completed_HA, NT_cty_completed_private, NTs_cty_completed_private,
                     NTs_area_completed, NTs_area_completed_local, NTs_area_completed_HA, NTs_area_completed_private)
names(df_list_geog) <- c("NT_cty_completed", "NTs_cty_completed", "NT_cty_completed_local", "NTs_cty_completed_local", "NT_cty_completed_HA", "NTs_cty_completed_HA", "NT_cty_completed_private", "NTs_cty_completed_private",
                         "NTs_area_completed", "NTs_area_completed_local", "NTs_area_completed_HA", "NTs_area_completed_private")                        

# Export data frames to Excel
export_to_excel(df_list_geog, file_path_geog) 



#create summary sheet 
#------------------------------------------------------------------------------------------------------
#summarising statistics by cty_1981 (county 1981), 
#can add to this code easily - especially once we've added tags to the geography lookup 
#population
population_joined <- population_all %>%
  left_join(lad71_cty81, by = "equiv_1971_cd")  

population_cty_1981 <- population_joined %>%
  group_by(cty_81) %>%
  summarize(across(where(is.numeric), sum, na.rm = TRUE))   

#completed minus demolitions 
built_minus_demos <- net_completed_all %>% 
  left_join(lad71_cty81, by = "equiv_1971_cd")  

built_minus_demos_cty_1981 <- built_minus_demos %>% 
  group_by(cty_81) %>%
  summarize(across(where(is.numeric), sum, na.rm = TRUE))  

#gross_all
gross_all_building_joined <- completed_all %>%
  left_join(lad71_cty81, by = "equiv_1971_cd") 

gross_all_building_cty_1981 <- gross_all_building_joined %>%
  group_by(cty_81) %>%
  summarize(across(where(is.numeric), sum, na.rm = TRUE)) 

#gross_LA 
gross_LA_building_joined <- completed_local %>%
  left_join(lad71_cty81, by = "equiv_1971_cd") 

gross_LA_building_cty_1981 <- gross_LA_building_joined %>%
  group_by(cty_81) %>%
  summarize(across(where(is.numeric), sum, na.rm = TRUE))  

#gross_HA
gross_HA_building_joined <- completed_HA %>%
  left_join(lad71_cty81, by = "equiv_1971_cd") 

gross_HA_building_cty_1981 <- gross_HA_building_joined %>%
  group_by(cty_81) %>%
  summarize(across(where(is.numeric), sum, na.rm = TRUE))  

#gross_private
gross_private_building_joined <- completed_private %>%
  left_join(lad71_cty81, by = "equiv_1971_cd") 

gross_private_building_cty_1981 <- gross_private_building_joined %>%
  group_by(cty_81) %>%
  summarize(across(where(is.numeric), sum, na.rm = TRUE)) 

#gross_public
gross_public_building_joined <- completed_private %>%
  left_join(lad71_cty81, by = "equiv_1971_cd") 

gross_public_building_cty_1981 <- gross_public_building_joined %>%
  group_by(cty_81) %>%
  summarize(across(where(is.numeric), sum, na.rm = TRUE)) 



### EXPORT net completed & population stats to excel--------------------------------
# Export data frames to Excel
export_to_excel(df_list_geog, file_path_geog) 

# Generate file path for Excel workbook with date 02--------------------------------
date <- format(Sys.Date(), "%Y-%m-%d")
file_name_geog <- paste0(date, "population_stock_housebuilding_46_73_1981geographies", ".xlsx")
dir_path_geog = paste0(dirname("~"),"/Centre for Cities/Centre for Cities POC - Documents/Research/Housing/History of Planning 2/Data/Output/From R scripts/Housebuilding/")
file_path_geog <- file.path(dir_path_geog, file_name_geog)

# Create a list of data frames
df_list_geog <- list(population_cty_1981, built_minus_demos_cty_1981, gross_all_building_cty_1981, gross_LA_building_cty_1981, gross_HA_building_cty_1981,
                     gross_private_building_cty_1981, gross_public_building_cty_1981)
names(df_list_geog) <- c("population_cty_1981", "built_minus_demos_cty_1981", "gross_all_building_cty_1981", "gross_LA_building_cty_1981",
                         "gross_HA_building_cty_1981", "gross_private_building_cty_1981", "gross_public_building_cty_1981")                             

# Export data frames to Excel
export_to_excel(df_list_geog, file_path_geog) 

