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

#set directory workaround
dirname <- sub("^(([^/]+/){2}[^/]+).*", "\\1", dirname("~"))

#bring in affordable housing data
path <- paste0(dirname, "/centre for Cities/Centre for Cities POC - Documents/Research/Housing/History of Planning 2/Data/Input/Affordable Housing Tables/", collapse = NULL)
setwd(path) 

affordable_raw <- read.csv("AHS_199192_to_202223_open_data.csv") 

#make it only new builds and completions
affordable_new_builds <- affordable_raw[affordable_raw$Type == "NB", ] 
affordable_new_builds <- affordable_new_builds[affordable_new_builds$Completions == "Completion", ] 

#select different types of delivery
affordable_HAgrant <- affordable_new_builds[affordable_new_builds$LT1000 == "Private Registered Provider HE/GLA funded",] 
affordable_HAother <- affordable_new_builds[affordable_new_builds$LT1000 == "Private Registered Provider other funding",] 
affordable_HAany <- affordable_new_builds[affordable_new_builds$LT1000 %in% c("Private Registered Provider HE/GLA funded", "Private Registered Provider other funding"), ]
affordable_LAgrant <- affordable_new_builds[affordable_new_builds$LT1000 == "Local Authority HE/GLA funded",] 
affordable_LAother <- affordable_new_builds[affordable_new_builds$LT1000 == "Local Authority other funding",]  
affordable_LAany <- affordable_new_builds[affordable_new_builds$LT1000 %in% c("Local Authority HE/GLA funded", "Local Authority other funding"), ]
affordable_S106_nogrant <- affordable_new_builds[affordable_new_builds$LT1000 == "s106 nil grant",]
affordable_S106_partgrant <- affordable_new_builds[affordable_new_builds$LT1000 == "s106 part grant",]
affordable_s106_any <- affordable_new_builds[affordable_new_builds$LT1000 %in% c("s106 nil grant", "s106 part grant"), ]
excluded_values <- c("Private Registered Provider HE/GLA funded", 
                     "Private Registered Provider other funding", 
                     "Local Authority HE/GLA funded", 
                     "Local Authority other funding", 
                     "s106 nil grant", 
                     "s106 part grant")
affordable_other <- affordable_new_builds[!affordable_new_builds$LT1000 %in% excluded_values, ]
 
#select different types of affordable housing
social_rent <- affordable_new_builds[affordable_new_builds$Tenure == "Social Rent",]  
LDN_affordable_rent <- affordable_new_builds[affordable_new_builds$Tenure == "London Affordable Rent",] 
affordable_rent <- affordable_new_builds[affordable_new_builds$Tenure == "Affordable Rent",]  
intermediate_rent <- affordable_new_builds[affordable_new_builds$Tenure == "Intermediate Rent",]  
affordable_ownership <- affordable_new_builds[affordable_new_builds$Tenure == "Affordable Home Ownership",] 
shared_ownership <- affordable_new_builds[affordable_new_builds$Tenure == "Shared Ownership",]  
excluded_values <- c("Social Rent", 
                     "London Affordable Rent", 
                     "Affordable Rent", 
                     "Intermediate Rent", 
                     "Affordable Home Ownership", 
                     "Shared Ownership")
other_affordable <- affordable_new_builds[!affordable_new_builds$Tenure %in% excluded_values, ] 

#select mixed combinations of interest 
#genuinely affordable rent categories 
genuinely_affordable <- affordable_new_builds[affordable_new_builds$Tenure %in% c("Social Rent", "London Affordable Rent"), ]

#any provision by RPs or LAs not through grant, and social or genuinely affordable rented
LA_HA_non_grant_social <- affordable_new_builds %>%
  filter(LT1000 %in% c("Private Registered Provider other funding", "Local Authority other funding") &
           Tenure %in% c("Social Rent", "London Affordable Rent")) 

#s106 delivering genuinely affordable rent
s106_social <- affordable_new_builds %>%
  filter(LT1000 %in% c("s106 nil grant") &
           Tenure %in% c("Social Rent", "London Affordable Rent"))  

#not really that affordable or questionable categories 
questionably_affordable <- affordable_new_builds %>%
  filter(Tenure %in% c("Intermediate Rent", "First Homes", "Shared Ownership"))  

#summarise the data by appropriate geographies 
#bring in flagging data
path <- paste0(dirname,"/Centre for Cities/Centre for Cities POC - Documents/Research/Core Data/Lookups/PUA and Small Geographies/", collapse = NULL)
setwd(path)

geog_lad_lookup <- read.xlsx("2024-06_all_LAs_into_LA23_into_CfC_geogs.xlsx", sheet = "LA")

geog_lad_lookup <- geog_lad_lookup[, c(1, 4)]

geog_lad_lookup <- geog_lad_lookup %>%
  rename(LA.code.202223 = `LA.Code.(original)`) 

#join flags to all the dataframes
# Define a function to join flag data
join_geogs <- function(df) {
  df %>%
    left_join(geog_lad_lookup, by = "LA.code.202223")
}

# Apply the function to each summarized dataframe
affordable_new_builds <- join_geogs(affordable_new_builds)
affordable_HAgrant <- join_geogs(affordable_HAgrant)
affordable_HAother <- join_geogs(affordable_HAother)
affordable_HAany <- join_geogs(affordable_HAany)
affordable_LAgrant <- join_geogs(affordable_LAgrant)
affordable_LAother <- join_geogs(affordable_LAother)
affordable_LAany <- join_geogs(affordable_LAany)
affordable_S106_nogrant <- join_geogs(affordable_S106_nogrant)
affordable_S106_partgrant <- join_geogs(affordable_S106_partgrant)
affordable_s106_any <- join_geogs(affordable_s106_any)
affordable_other <- join_geogs(affordable_other)
social_rent <- join_geogs(social_rent)
LDN_affordable_rent <- join_geogs(LDN_affordable_rent)
affordable_rent <- join_geogs(affordable_rent)
intermediate_rent <- join_geogs(intermediate_rent)
affordable_ownership <- join_geogs(affordable_ownership)
shared_ownership <- join_geogs(shared_ownership)
other_affordable <- join_geogs(other_affordable)
questionably_affordable <- join_geogs(questionably_affordable)
s106_social <- join_geogs(s106_social)
LA_HA_non_grant_social <- join_geogs(LA_HA_non_grant_social)
genuinely_affordable <- join_geogs(genuinely_affordable)

# Define a function to summarize by LA.code.202223 and year, and sum the 'units' column
summarize_units_by_LA_and_year <- function(data) {
  data %>%
    mutate(units = as.numeric(as.character(Units))) %>%  # Convert units to numeric
    group_by(LA.Code.2023, Year) %>%  # Group by LA and year
    summarise(units = sum(units, na.rm = TRUE)) %>%  # Summarize units
    pivot_wider(names_from = Year, values_from = units, values_fill = 0)  # Convert to wide format
}

# Apply the function to each dataframe 
sum_affordable_any <- summarize_units_by_LA_and_year(affordable_new_builds)

sum_affordable_HAgrant <- summarize_units_by_LA_and_year(affordable_HAgrant)
sum_affordable_HAother <- summarize_units_by_LA_and_year(affordable_HAother)
sum_affordable_HAany <- summarize_units_by_LA_and_year(affordable_HAany)
sum_affordable_LAgrant <- summarize_units_by_LA_and_year(affordable_LAgrant)
sum_affordable_LAother <- summarize_units_by_LA_and_year(affordable_LAother)
sum_affordable_LAany <- summarize_units_by_LA_and_year(affordable_LAany)
sum_affordable_S106_nogrant <- summarize_units_by_LA_and_year(affordable_S106_nogrant)
sum_affordable_S106_partgrant <- summarize_units_by_LA_and_year(affordable_S106_partgrant)
sum_affordable_s106_any <- summarize_units_by_LA_and_year(affordable_s106_any)
sum_affordable_other_provider <- summarize_units_by_LA_and_year(affordable_other)

sum_social_rent <- summarize_units_by_LA_and_year(social_rent)
sum_LDN_affordable_rent <- summarize_units_by_LA_and_year(LDN_affordable_rent)
sum_affordable_rent <- summarize_units_by_LA_and_year(affordable_rent)
sum_intermediate_rent <- summarize_units_by_LA_and_year(intermediate_rent)
sum_affordable_ownership <- summarize_units_by_LA_and_year(affordable_ownership)
sum_shared_ownership <- summarize_units_by_LA_and_year(shared_ownership)
sum_other_affordable_tenure <- summarize_units_by_LA_and_year(other_affordable) 

sum_questionably_affordable <- summarize_units_by_LA_and_year(questionably_affordable)
sum_s106_social <- summarize_units_by_LA_and_year(s106_social)
sum_LA_HA_non_grant_social <- summarize_units_by_LA_and_year(LA_HA_non_grant_social)
sum_genuinely_affordable <- summarize_units_by_LA_and_year(genuinely_affordable)


# Combined function to rename, reorder, and apply the correct smoothing transformation
rename_reorder_and_smooth <- function(df) {
  # Step 1: Ensure LA.code.202223 remains the first column
  first_column <- df[, "LA.Code.2023", drop = FALSE]
  
  # Step 2: Extract other columns (excluding LA.code.202223)
  other_columns <- df[, !colnames(df) %in% "LA.Code.2023"]
  
  # Step 3: Extract first year from the remaining column names
  new_colnames <- gsub("^(\\d{4}).*", "\\1", colnames(other_columns))
  
  # Rename the columns in the other part of the dataframe
  colnames(other_columns) <- new_colnames
  
  # Reorder the other columns based on the new column names (as integers)
  other_columns <- other_columns[, order(as.integer(new_colnames))]
  
  # Store the original unmodified data for reference
  original_data <- other_columns
  
  # Step 4: Apply the smoothing formula: 0.75 * current_year + 0.25 * preceding_year (using original values)
  years <- as.integer(colnames(other_columns))  # Get the years as integers
  for (i in 2:length(years)) {
    # Use original values for the calculation
    other_columns[, i] <- 0.75 * original_data[, i] + 0.25 * original_data[, i - 1]
  }
  
  # Step 5: Combine the LA.code.202223 with the reordered and smoothed columns
  df <- cbind(first_column, other_columns)
  
  return(df)
}

# Apply the combined function to each dataframe
sum_affordable_any <- rename_reorder_and_smooth(sum_affordable_any)
sum_affordable_HAgrant <- rename_reorder_and_smooth(sum_affordable_HAgrant)
sum_affordable_HAother <- rename_reorder_and_smooth(sum_affordable_HAother)
sum_affordable_HAany <- rename_reorder_and_smooth(sum_affordable_HAany)
sum_affordable_LAgrant <- rename_reorder_and_smooth(sum_affordable_LAgrant)
sum_affordable_LAother <- rename_reorder_and_smooth(sum_affordable_LAother)
sum_affordable_LAany <- rename_reorder_and_smooth(sum_affordable_LAany)
sum_affordable_S106_nogrant <- rename_reorder_and_smooth(sum_affordable_S106_nogrant)
sum_affordable_S106_partgrant <- rename_reorder_and_smooth(sum_affordable_S106_partgrant)
sum_affordable_s106_any <- rename_reorder_and_smooth(sum_affordable_s106_any)
sum_affordable_other_provider <- rename_reorder_and_smooth(sum_affordable_other_provider)

sum_social_rent <- rename_reorder_and_smooth(sum_social_rent)
sum_LDN_affordable_rent <- rename_reorder_and_smooth(sum_LDN_affordable_rent)
sum_affordable_rent <- rename_reorder_and_smooth(sum_affordable_rent)
sum_intermediate_rent <- rename_reorder_and_smooth(sum_intermediate_rent)
sum_affordable_ownership <- rename_reorder_and_smooth(sum_affordable_ownership)
sum_shared_ownership <- rename_reorder_and_smooth(sum_shared_ownership)
sum_other_affordable_tenure <- rename_reorder_and_smooth(sum_other_affordable_tenure)

sum_questionably_affordable <- rename_reorder_and_smooth(sum_questionably_affordable)
sum_s106_social <- rename_reorder_and_smooth(sum_s106_social)
sum_LA_HA_non_grant_social <- rename_reorder_and_smooth(sum_LA_HA_non_grant_social)
sum_genuinely_affordable <- rename_reorder_and_smooth(sum_genuinely_affordable)


#bring in flagging data
path <- paste0(dirname,"/Centre for Cities/Centre for Cities POC - Documents/Research/Housing/History of Planning 2/Data/Input/", collapse = NULL)
setwd(path)

lad23_flags <- read.xlsx("2024-07-22_GB_Lookups.xlsx", sheet = "lad_23")

lad23_flags <- lad23_flags %>%
  rename(LA.Code.2023 = lad_23) 

#join flags to all the dataframes
# Define a function to join flag data
join_flags <- function(df) {
  df %>%
    left_join(lad23_flags, by = "LA.Code.2023")
}

# Apply the function to each summarized dataframe
sum_affordable_any <- join_flags(sum_affordable_any)
sum_affordable_HAgrant <- join_flags(sum_affordable_HAgrant)
sum_affordable_HAother <- join_flags(sum_affordable_HAother)
sum_affordable_HAany <- join_flags(sum_affordable_HAany)
sum_affordable_LAgrant <- join_flags(sum_affordable_LAgrant)
sum_affordable_LAother <- join_flags(sum_affordable_LAother)
sum_affordable_LAany <- join_flags(sum_affordable_LAany)
sum_affordable_S106_nogrant <- join_flags(sum_affordable_S106_nogrant)
sum_affordable_S106_partgrant <- join_flags(sum_affordable_S106_partgrant)
sum_affordable_s106_any <- join_flags(sum_affordable_s106_any)
sum_affordable_other_provider <- join_flags(sum_affordable_other_provider)
sum_social_rent <- join_flags(sum_social_rent)
sum_LDN_affordable_rent <- join_flags(sum_LDN_affordable_rent)
sum_affordable_rent <- join_flags(sum_affordable_rent)
sum_intermediate_rent <- join_flags(sum_intermediate_rent)
sum_affordable_ownership <- join_flags(sum_affordable_ownership)
sum_shared_ownership <- join_flags(sum_shared_ownership)
sum_other_affordable_tenure <- join_flags(sum_other_affordable_tenure)
sum_questionably_affordable <- join_flags(sum_questionably_affordable)
sum_s106_social <- join_flags(sum_s106_social)
sum_LA_HA_non_grant_social <- join_flags(sum_LA_HA_non_grant_social)
sum_genuinely_affordable <- join_flags(sum_genuinely_affordable)


### EXPORT to excel--------------------------------
# Generate file path for Excel workbook with date 02--------------------------------
date <- format(Sys.Date(), "%Y-%m-%d")
file_name_geog <- paste0(date, "affordable_housing_summaries_lad_2023", ".xlsx")
dir_path_geog = paste0(dirname,"/Centre for Cities/Centre for Cities POC - Documents/Research/Housing/History of Planning 2/Data/Output/From R scripts/Affordable housebuilding/")
file_path_geog <- file.path(dir_path_geog, file_name_geog)

# Create a list of data frames
df_list_geog <- list(affordable_new_builds, sum_affordable_any, sum_affordable_HAany, sum_affordable_HAgrant, sum_affordable_HAother, sum_affordable_LAany, sum_affordable_LAgrant, sum_affordable_LAother, sum_affordable_other_provider, 
                     sum_affordable_s106_any, sum_affordable_S106_nogrant, sum_affordable_S106_partgrant, sum_social_rent, sum_affordable_rent, sum_LDN_affordable_rent,
                     sum_intermediate_rent, sum_genuinely_affordable, sum_affordable_ownership, sum_shared_ownership, sum_other_affordable_tenure, sum_LA_HA_non_grant_social,
                     sum_questionably_affordable, sum_s106_social )
names(df_list_geog) <- c("non_summarised", "any_tenure_any_provider", "HA", "HA_grantonly", "HA_other", "LA", "LA_grantonly", "LA_other", "other_provider",  
                         "s106", "s106_nogrant", "s106_partgrant", "social_rent", "affordable_rent", "LDN_affordable_rent", 
                         "intermediate_rent", "genuinely_affordable_rent", "affordable_ownership", "shared_ownership", "other_affordable_tenure", "LA_HA_non_grant_social",
                         "questionably_affordable", "s106_social")                             

# Export data frames to Excel
export_to_excel(df_list_geog, file_path_geog) 


