library(tidyverse)
library(readxl)
library(dplyr)
library(openxlsx) 
library(ggplot2)
library(tidyr)
library(purrr)
library(zoo)


### Getting the data 
#-----------------------------------------------------------------------------------------
# Get today's date in the format "YYYY-MM-DD"
    today <- format(Sys.Date(), "%Y-%m-%d")

###bring in PUA, regions, etc. geography lookup for 2001 onwards 
    path = paste0(dirname("~"),"/Centre for Cities/Centre for Cities POC - Documents/Research/Core Data/Lookups/PUA and Small Geographies/", collapse = NULL)
    setwd(path)
    
    geographies <- read.xlsx("2024-06_all_LAs_into_LA23_into_CfC_geogs.xlsx", sheet = "LA") 
    geographies <- geographies[geographies$Country == 'England', ]
    geographies <- geographies[!grepl("^NA", row.names(geographies)), ]
    geographies <- geographies %>%
      mutate(across(everything(), as.character))
    geographies <- geographies %>%
      mutate(across(everything(), ~ replace_na(.x, '-')))
    colnames(geographies)[1] <- "LA.Code"
    colnames(geographies)[2] <- "LA.Name"
    colnames(geographies)[4] <- "LA.Code_2023"

    #bring in 1981 counties lookup for counties join later on 
    path <- paste0(dirname("~"),"/Centre for Cities/Centre for Cities POC - Documents/Research/Housing/History of Planning 2/Data/Input/", collapse = NULL)
    setwd(path)
    
    lad_23_to_cty_81 <- read.xlsx("2024-07-22_GB_Lookups.xlsx", sheet = "lad_23")
    colnames(lad_23_to_cty_81)[1] <- "LA.Code_2023"
    lad_23_to_cty_81 <- lad_23_to_cty_81[, c("LA.Code_2023", "cty_81")]
    
    ###bring in Inner London lookup - for 1981 census stock calculations 
    path = paste0(dirname("~"),"/Centre for Cities/Centre for Cities POC - Documents/Research/Housing/History of Planning 2/Data/Input/", collapse = NULL)
    setwd(path) 
    
    inner_LDN_lookup <- read.xlsx("Inner_LDN_lookup.xlsx")
    colnames(inner_LDN_lookup)[1] <- "LA.Code" 

###get our 1974 - 2000 data 
    path = paste0(dirname("~"),"/Centre for Cities/Centre for Cities POC - Documents/Research/Housing/History of Planning 2/Data/Output/From R scripts/Housebuilding/", collapse = NULL)
    setwd(path) 
    
    # Construct the file name dynamically
    file_name <- paste0(today, "built_and_population_1974_2000.xlsx")
    
    ####NOTE - you'll need to update the date on this file name, if there has been any update to the source of that code
    pop1974_1997 <- read.xlsx(file_name, sheet = "population")
    colnames(pop1974_1997)[1] <- "LA.Code"
    
    #multiply by 1000 so numbers match later population format 
    pop1974_1997[, 3:ncol(pop1974_1997)] <- pop1974_1997[, 3:ncol(pop1974_1997)] * 1000
    
    # Make separate sheet for net housebuilding
    netbuilt1974_2000 <- read.xlsx(file_name, sheet = "net built")
    colnames(netbuilt1974_2000)[1] <- "LA.Code"
    
    # Make separate sheet for total housebuilding
    built1974_2000 <- read.xlsx(file_name, sheet = "total built")
    colnames(built1974_2000)[1] <- "LA.Code"
    
    # Make separate sheet for private housebuilding
    privatebuilt1974_2000 <- read.xlsx(file_name, sheet = "private built")
    colnames(privatebuilt1974_2000)[1] <- "LA.Code"
    
    # Make separate sheet for local authority housebuilding
    LAbuilt1974_2000 <- read.xlsx(file_name, sheet = "LA built")
    colnames(LAbuilt1974_2000)[1] <- "LA.Code"
    
    # Make separate sheet for housing association housebuilding
    HAbuilt1974_2000 <- read.xlsx(file_name, sheet = "HA built")
    colnames(HAbuilt1974_2000)[1] <- "LA.Code"


###get government statistics on housing stock and housebuilding from different tenures 
    #get 2001 - 2022 total stock data 
    path = paste0(dirname("~"),"/Centre for Cities/Centre for Cities POC - Documents/Research/Housing/History of Planning 2/Data/Input/Housebuilding Tables/", collapse = NULL)
    setwd(path) 
    
    stock2001_2022 <- read.xlsx("LT_125.xlsx", sheet = "Table_125_(unrounded)")
    
    new_colnames <- as.character(stock2001_2022[3, ])
    colnames(stock2001_2022) <- new_colnames
    stock2001_2022 <- stock2001_2022[-c(1:3), ]
    
    stock2001_2022 <- stock2001_2022[, -c(1, 3)]
    
    #make new LA name column and move it to the right place
    stock2001_2022$LA_name <- ifelse(!is.na(stock2001_2022[, 2]), stock2001_2022[, 2], stock2001_2022[, 3])
    stock2001_2022 <- stock2001_2022[, c(1, ncol(stock2001_2022), 4:(ncol(stock2001_2022)-1))]
    
    colnames(stock2001_2022)[1] <- "LA.Code" 
  
    stock2001_2022 <- stock2001_2022 %>%
      filter(!is.na(LA.Code))

   
    #----
    #get 2001-2022 building by tenure data 
    # Define the file path
    file_path <- "LT253_tenure_building.xlsx"
    
    # Initialize the final data frame with geographies
    tenurebuilt_01_22 <- geographies
    
    # Loop through years 2001 to 2022
    for (year in 1990:2022) {
      
      # Construct sheet name based on the year
      if (year < 1999) {
        sheet_name <- sprintf("%04d-%02d", year, year + 1 - 1900 )
      } else {
        sheet_name <- sprintf("%04d-%02d", year, year +1 - 2000)
      }
      
      # Read the Excel sheet
      df <- read.xlsx(file_path, sheet = sheet_name)
      
      #!We only need completed, get rid of start here
      df <- df[,-c(7:10) ]
      
      # Extract and set new column names
      if (year >= 2020) {
        new_colnames <- as.character(df[2, ])
        df <- df[-c(1:4), ]
      } else {
        new_colnames <- as.character(df[3, ])
        df <- df[-c(1:5), ]
      }
      colnames(df) <- new_colnames
    
      # Remove rows in the 'Current' column that don't start with "E"
      current_col <- grep("Current", colnames(df), value = TRUE, ignore.case = TRUE)
      if (length(current_col) > 0) {
        df <- df[grepl("^E", df[[current_col]]), ]
        colnames(df)[colnames(df) == current_col] <- "LA.Code"  # Rename the 'Current' column to 'LA.Code'
      } else {
        warning(paste("No 'Current' columns found for year", year))
      }
      
      # Select columns containing 'local', 'associations', or 'private' (case insensitive)
      keyword_columns <- grep("local|associations|private|all", colnames(df), value = TRUE, ignore.case = TRUE)
      
      # Ensure to keep only the rightmost version of each keyword column
      if (length(keyword_columns) > 0) {
        keyword_columns <- keyword_columns[order(match(keyword_columns, colnames(df)))]
        keyword_columns <- sapply(c("local", "associations", "private", "all"), function(keyword) {
          rightmost <- grep(keyword, keyword_columns, ignore.case = TRUE, value = TRUE)
          if (length(rightmost) > 0) {
            return(tail(rightmost, 1))
          } else {
            return(NULL)
          }
        })
        keyword_columns <- unlist(keyword_columns)
        keyword_columns <- keyword_columns[!is.na(keyword_columns)] # Remove NAs if any
      } else {
        warning(paste("No keyword columns found for year", year))
      }
      
      # Select columns to keep
      columns_to_keep <- union("LA.Code", keyword_columns)
      if (length(columns_to_keep) == 0) {
        stop(paste("No columns to keep for year", year))
      }
      
      df <- df[, columns_to_keep, drop = FALSE] # Ensure drop = FALSE to keep the data frame structure
      
      # Prefix column names with the current year, except for 'LA.Code'
      colnames(df)[colnames(df) != "LA.Code"] <- paste(year, colnames(df)[colnames(df) != "LA.Code"], sep = '_')
      
      # Left join the final data frame with the current year's data frame on 'LA.Code'
      tenurebuilt_01_22 <- left_join(tenurebuilt_01_22, df, by = "LA.Code")
    }
    
    LAbuilt2000_2022 <- tenurebuilt_01_22 %>%
      select(1:13, contains("local", ignore.case = TRUE)) 
    
    HAbuilt2000_2022 <- tenurebuilt_01_22 %>%
      select(1:13, contains("association", ignore.case = TRUE))  
    
    privatebuilt2000_2022 <- tenurebuilt_01_22 %>%
      select(1:13, contains("private", ignore.case = TRUE))   
    
    built2000_2022 <- tenurebuilt_01_22 %>%
      select(1:13, contains("all", ignore.case = TRUE))   
    
    
##Filling blank (in LT-253)
#1.fill in blank for tenure year
## For all three LAbuilt2000_2022, HAbuilt2000_2022, privatebuilt2000_2022, fill in any blanks using the average increase calculated from the averages of the available 0-3 years before and after the blank year.
    #1) Identify the first and last occurrences of consecutive ..
    
    #2)calculate the average
    #If 3 years of meaningful data (not ..) are available before the identified first .. and after the identified last .., calculate the averages of these 3 years respectively.
    #If consecutive 3 years of meaningful data are not available on either side, use the available data (which could be 1 or 2 years) to calculate the average on that side.
    
    #3)calculate the average increase between the two averages
    #Find the difference between the two averages and divide it by the number of years in between, which equals to the year of last occurrences of consecutive .. minus the year of the first occurrences of consecutive ..
    
    #4)fill in the identified ..
    #Fill the first .. with the sum of the calculated average increase and the average of the meaningful data (not ..) before the identified first ..
    #For the following .. in this consecutive .. section, fill them with the sum of the previously filled number and the calculated average increase. Continue this until the last .. in the consecutive section.
    
    # Define the function to fill in ".." blanks while keeping NAs unchanged
    fill_blanks <- function(dataset) {
      
      # Extract the first 13 columns as they should remain unchanged
      text_columns <- dataset[, 1:13]
      
      # Extract the data part starting from the 14th column
      data_matrix <- dataset[, 14:ncol(dataset)]
      
      # Function to fill gaps in a row
      fill_row <- function(row) {
        # Convert row to numeric where ".." are set as a placeholder (-99999)
        row_numeric <- as.numeric(ifelse(row == "..", -99999, row))
        
        # Loop to handle multiple consecutive sections of ".."
        while(TRUE) {
          # Find indices of the gaps
          gap_indices <- which(row_numeric == -99999)
          
          if(length(gap_indices) == 0) {
            break  # No gaps left in this row
          }
          
          # Identify the first and last ".." in a sequence of gaps
          start_na <- gap_indices[1]
          end_na <- start_na
          while ((end_na + 1) %in% gap_indices) {
            end_na <- end_na + 1
          }
          
          # Get the meaningful indices before the gap
          before_indices <- integer(0)
          for (i in (start_na - 1):max(1, start_na - 3)) {
            if (i < 1 || is.na(row_numeric[i]) || row_numeric[i] == -99999) {
              break  # Stop if ".." is encountered or index is out of bounds
            }
            before_indices <- c(before_indices, i)
            if (length(before_indices) == 3) {
              break  # Stop if we've collected 3 values
            }
          }
          before_indices <- rev(before_indices)  # Reverse to maintain original order
          before_indices <- before_indices[row_numeric[before_indices] != -99999]
          
          # Get the meaningful indices after the gap
          after_indices <- integer(0)
          for (i in (end_na + 1):min(length(row_numeric), end_na + 3)) {
            if (i > length(row_numeric) || is.na(row_numeric[i]) || row_numeric[i] == -99999) {
              break  # Stop if ".." is encountered or index is out of bounds
            }
            after_indices <- c(after_indices, i)
            if (length(after_indices) == 3) {
              break  # Stop if we've collected 3 values
            }
          }
          after_indices <- after_indices[row_numeric[after_indices] != -99999]
          
          # Ensure only up to 3 values are collected
          before_indices <- head(before_indices, 3)
          after_indices <- head(after_indices, 3)
          
          # Calculate the averages of available data (ignoring NA)
          avg_before <- mean(row_numeric[before_indices], na.rm = TRUE)
          avg_after <- mean(row_numeric[after_indices], na.rm = TRUE)
          
          # Calculate average increase per year
          year_diff <- end_na - start_na
          avg_increase <- (avg_after - avg_before) / (year_diff + 2)  # Corrected to divide by year_diff + 1
          
          # Fill the gaps with the calculated values
          for (j in 0:year_diff) {
            row_numeric[start_na + j] <- avg_before + avg_increase * (j+1)
          }
          
          # Remove the handled gap indices from gap_indices list
          gap_indices <- gap_indices[!(gap_indices %in% (start_na:end_na))]
        }
        
        # Replace back -99999 with ".." where required
        row_filled <- ifelse(row_numeric == -99999, "..", row_numeric)
        
        return(row_filled)
      }
      
      # Apply the function to each row of the data matrix (starting from the 14th column)
      filled_data <- t(apply(data_matrix, 1, fill_row))
      
      # Combine the unchanged first 13 columns with the filled data
      filled_data <- cbind(text_columns, filled_data)
      
      # Convert the filled data back to a data frame and assign column names
      filled_data <- as.data.frame(filled_data)
      names(filled_data) <- names(dataset)
      
      return(filled_data)
    }
    
    
    # Apply the function to each dataset
    LAbuilt_filled <- fill_blanks(LAbuilt2000_2022)
    HAbuilt_filled <- fill_blanks(HAbuilt2000_2022)
    privatebuilt_filled <- fill_blanks(privatebuilt2000_2022)
    
    #Choose the columns we want to keep.
    LAbuilt2000_2022 <- LAbuilt_filled[, -c(14:23)]
    HAbuilt2000_2022 <- HAbuilt_filled[, -c(14:23)]
    privatebuilt2000_2022 <- privatebuilt_filled[, -c(14:23)] 
    built2000_2022 <- built2000_2022[, -c(14:23)]

    #Keep as records
    built2000_2022_old<-built2000_2022
    
#2.Fill in blank for built All (built2000_2022)
##If All is .., All= Private + HA + LA
# Iterate over each row in built2000_2022
for (i in 1:nrow(built2000_2022)) {
  
  # Get the LA.Code for the current row
  la_code <- built2000_2022$LA.Code[i]
  
  # Iterate over each year column starting from the 14th column
  for (col in colnames(built2000_2022)[-(1:13)]) {  # Skipping the first 13 columns
    
    # Extract the year from the column name (e.g., "2002_XXX" -> "2002")
    year <- gsub("_.*", "", col)
    
    # Check if the year is numeric
    if (!is.na(as.numeric(year))) {
      
      # Find the corresponding columns in each dataset
      la_col <- grep(paste0("^", year), colnames(LAbuilt2000_2022))
      ha_col <- grep(paste0("^", year), colnames(HAbuilt2000_2022))
      private_col <- grep(paste0("^", year), colnames(privatebuilt2000_2022))
      
      # Check if the columns were found
      if (length(la_col) > 0 && length(ha_col) > 0 && length(private_col) > 0) {
        
        # Get the corresponding values from the other datasets
        la_value <- LAbuilt2000_2022[LAbuilt2000_2022$LA.Code == la_code, la_col]
        ha_value <- HAbuilt2000_2022[HAbuilt2000_2022$LA.Code == la_code, ha_col]
        private_value <- privatebuilt2000_2022[privatebuilt2000_2022$LA.Code == la_code, private_col]
        
        # Convert values to numeric and handle missing values
        la_value <- as.numeric(as.character(la_value))
        ha_value <- as.numeric(as.character(ha_value))
        private_value <- as.numeric(as.character(private_value))
        
        # Ensure built2000_2022[, col] is character type for comparison
        cell_value <- as.character(built2000_2022[i, col])
        
        # Check if the value in built2000_2022 is ".."
        if (!is.na(cell_value) && cell_value == "..") {
          
          # Calculate the total value from the other datasets
          total_value <- sum(c(la_value, ha_value, private_value), na.rm = TRUE)
          
          # Update built2000_2022 with the computed sum if total_value is not NA
          if (!is.na(total_value)) {
            built2000_2022[i, col] <- total_value
          }
        }
      } else {
        # Inform about missing columns
        warning(sprintf("Year column %s not found in one or more datasets for LA.Code %s", year, la_code))
      }
    }
  }
}

  
###get public housebuilding data from 1991 Table 1011 
    #note = this is already summarised to 2023 LAs and financial year adjusted
    path = paste0(dirname("~"),"/Centre for Cities/Centre for Cities POC - Documents/Research/Housing/History of Planning 2/Data/Output/From R scripts/Affordable housebuilding/", collapse = NULL)
    setwd(path) 
    
    #bring in the relevant tenure types and builder
    file_name <- paste0(today, "affordable_housing_summaries_lad_2023.xlsx")
    
    all_affordable_built_1991_2022 <- read.xlsx(file_name, sheet = "any_tenure_any_provider")
    colnames(all_affordable_built_1991_2022)[1] <- "LA.Code_2023"
    
    social_built_1991_2022 <- read.xlsx(file_name, sheet = "social_rent")
    colnames(social_built_1991_2022)[1] <- "LA.Code_2023"
    
    LA_built_1991_2022 <- read.xlsx(file_name, sheet = "LA")
    colnames(LA_built_1991_2022)[1] <- "LA.Code_2023"
    
    HA_built_1991_2022 <- read.xlsx(file_name, sheet = "HA")
    colnames(HA_built_1991_2022)[1] <- "LA.Code_2023" 
    
    #calculate a non-social, affordable housing category
    affordable_not_social_1991_2022 <- all_affordable_built_1991_2022 %>% 
      left_join(social_built_1991_2022, by = "LA.Code_2023") 
    
    # Loop to calculate the difference for years 1991 to 2022
    for (year in 1991:2022) {
      col_x <- paste0(year, ".x")
      col_y <- paste0(year, ".y")
      new_col <- paste0("notsocial", year)
      
      # Calculate the difference and create a new column
      affordable_not_social_1991_2022[[new_col]] <- 
        affordable_not_social_1991_2022[[col_x]] - affordable_not_social_1991_2022[[col_y]]
    }
    
    # Remove columns with ".x" or ".y"
    cols_to_remove <- grep("\\.x$|\\.y$", names(affordable_not_social_1991_2022), value = TRUE)
    affordable_not_social_1991_2022 <- affordable_not_social_1991_2022[ , !(names(affordable_not_social_1991_2022) %in% cols_to_remove)]
    
    # Rename columns to remove "notsocial" prefix
    names(affordable_not_social_1991_2022) <- gsub("^notsocial", "", names(affordable_not_social_1991_2022))
    
###get 1998 - 2022 population data 
    path = paste0(dirname("~"),"/Centre for Cities/Centre for Cities POC - Documents/Research/Housing/History of Planning 2/Data/Input/Population/", collapse = NULL)
    setwd(path) 
    
    pop1992_2022 <- read.xlsx("2024-05-30_population.xlsx")  
    pop1992_2022 <- pop1992_2022 %>% select(1,2,4:34)
     

###get total dwelling stock data from Censuses
    #get 1981 dwelling stock data - and make the same adjustments as holmans
    path = paste0(dirname("~"),"/Centre for Cities/Centre for Cities POC - Documents/Research/Housing/History of Planning 2/Data/Input/Census 1981/", collapse = NULL)
    setwd(path)  
    
    dwellingstock_1981 <- read.csv("1981 - Household spaces.csv")
    
    new_colnames <- as.character(dwellingstock_1981[5, ])
    colnames(dwellingstock_1981) <- new_colnames
    dwellingstock_1981 <- dwellingstock_1981[-c(1:6), ]
    
    dwellingstock_1981 <- dwellingstock_1981[, -c(1, 3, 5, 6, 8:28)]  
    colnames(dwellingstock_1981)[1] <- "LA.Code" 
    colnames(dwellingstock_1981)[2] <- "1981 contained total" 
    colnames(dwellingstock_1981)[3] <- "1981 non contained households" 
    
    dwellingstock_1981 <- dwellingstock_1981 %>% 
      left_join(inner_LDN_lookup, by = "LA.Code") 
    
    # Convert the relevant columns to numeric
    dwellingstock_1981$`1981 non contained households` <- as.numeric(dwellingstock_1981$`1981 non contained households`)
    dwellingstock_1981$`1981 contained total` <- as.numeric(dwellingstock_1981$`1981 contained total`)
    dwellingstock_1981$Inner.London <- as.character(dwellingstock_1981$Inner.London)  # Ensure it's character for comparison
    
    # Replace NAs in the 'Inner.London' column with 'Outer London'
    dwellingstock_1981$Inner.London[is.na(dwellingstock_1981$Inner.London)] <- "Not Inner LDN"
    
    # Create the new column 'not_contained_dwellings' based on the conditions
    dwellingstock_1981$not_contained_dwellings <- ifelse(
      dwellingstock_1981$Inner.London == "Inner London",
      dwellingstock_1981$`1981 non contained households` * 0.19,
      dwellingstock_1981$`1981 non contained households` * 0.25
    )

    dwellingstock_1981$`1981 total stock` <- dwellingstock_1981$not_contained_dwellings + dwellingstock_1981$`1981 contained total`
    
    dwellingstock_1981 <- dwellingstock_1981[, -c(2:5)]  
    
    #get 1991 dwelling stock data 
    path = paste0(dirname("~"),"/Centre for Cities/Centre for Cities POC - Documents/Research/Housing/History of Planning 2/Data/Input/Census 1991/", collapse = NULL)
    setwd(path)  
    
    dwellingstock_1991 <- read.csv("1991 Households - occupied, empty, second homes etc.csv")
    
    new_colnames <- as.character(dwellingstock_1991[5, ])
    colnames(dwellingstock_1991) <- new_colnames
    dwellingstock_1991 <- dwellingstock_1991[-c(1:6), ]
    
    dwellingstock_1991 <- dwellingstock_1991[, -c(1, 4:18)] 
    
    colnames(dwellingstock_1991)[1] <- "LA.Code" 
    colnames(dwellingstock_1991)[2] <- "1991 total stock"  
    
    #spread the additional holmans caravans around the country! 
    dwellingstock_1991$`1991 total stock` <- as.numeric(dwellingstock_1991$`1991 total stock`)
    dwellingstock_1991$`1991 total stock` <- dwellingstock_1991$`1991 total stock` * 1.004747
  

#### NOTE - I have kept this code to input Census tenure stocks from 1981 and 1992 in here, in case we need it in future, but currently we don't use it. 
    #----
###get stock by tenure data from Censuses 
    #get 1981 tenure stock data 
    path = paste0(dirname("~"),"/Centre for Cities/Centre for Cities POC - Documents/Research/Housing/History of Planning 2/Data/Input/Census 1981/", collapse = NULL)
    setwd(path)  
    
    tenurestock_1981 <- read.csv("1981 Households - tenure type.csv")
    
    new_colnames <- as.character(tenurestock_1981[5, ])
    colnames(tenurestock_1981) <- new_colnames
    tenurestock_1981 <- tenurestock_1981[-c(1:6), ]
    
    colnames(tenurestock_1981)[1] <- "LA.Name" 
    colnames(tenurestock_1981)[2] <- "LA.Code" 
     
    LAstock1981 <- tenurestock_1981[, c(1, 2, 7)]
    colnames(LAstock1981)[3] <- "LA_stock" 
    HAstock1981 <- tenurestock_1981[, c(1, 2, 9)]
    colnames(HAstock1981)[3] <- "HA_stock" 
    
    #get 1991 tenure stock data 
    path = paste0(dirname("~"),"/Centre for Cities/Centre for Cities POC - Documents/Research/Housing/History of Planning 2/Data/Input/Census 1991/", collapse = NULL)
    setwd(path)  
    
    tenurestock_1991 <- read.csv("1991 Households - tenure type.csv")
    
    new_colnames <- as.character(tenurestock_1991[5, ])
    colnames(tenurestock_1991) <- new_colnames
    tenurestock_1991 <- tenurestock_1991[-c(1:6), ]
    
    colnames(tenurestock_1991)[1] <- "LA.Name" 
    colnames(tenurestock_1991)[2] <- "LA.Code" 
    
    LAstock1991 <- tenurestock_1991[, c(1, 2,10)]
    colnames(LAstock1991)[3] <- "LA_stock" 
    HAstock1991 <- tenurestock_1991[, c(1, 2, 9)]
    colnames(HAstock1991)[3] <- "HA_stock" 

#-----

 
#joining datasets to geographies - note this cuts out any areas that don't have codes in the geography
#lookup e.g. counties 
##summarising by 2023 geographies so can work with all the data together
###join population to geographies 
    joined_population <- geographies %>% 
      left_join(pop1974_1997, by = "LA.Code")
    
    joined_population <- joined_population %>% 
      left_join(pop1992_2022, by = "LA.Code")
    
    #replace all 1992 onward with ONS data - to get rid of issues with 90s data missing.
    #note, this may bring forward jumps because of later 90s boundary changes 
    joined_population <- joined_population %>%
      select(-c(`1992_POP`:`1997_POP`, `LA`))

    population_summarised <- joined_population %>%
      group_by(LA.Code_2023) %>%
      summarize(across(where(is.numeric), sum, na.rm = TRUE))
    
    #name columns year only 
    start_year <- 1974
    num_years <- ncol(population_summarised) - 1
    year_sequence <- start_year:(start_year + num_years - 1)
    
    # Assign new column names starting from the third column
    colnames(population_summarised)[2:ncol(population_summarised)] <- as.character(year_sequence)


###join census dwellings to geographies 
    #total census dwellings 
    joined81stock <- geographies %>% 
      left_join(dwellingstock_1981, by = "LA.Code") 
    
    joined81stock <- joined81stock %>%
      mutate(across(14, as.numeric))
    
    stock81_summarised <- joined81stock %>%
      group_by(LA.Code_2023) %>%
      summarize(across(where(is.numeric), sum, na.rm = TRUE))
    
    joined91stock <- geographies %>% 
      left_join(dwellingstock_1991, by = "LA.Code")
    
    joined91stock <- joined91stock %>%
      mutate(across(14, as.numeric))
    
    stock91_summarised <- joined91stock %>%
      group_by(LA.Code_2023) %>%
      summarize(across(where(is.numeric), sum, na.rm = TRUE)) 

    
###add our housebuilding data to geographies 
    #net built 
    joined_74_2000housebuilding <- geographies %>% 
      left_join(netbuilt1974_2000, by = "LA.Code")
    
    netbuilt74_00_summarised <- joined_74_2000housebuilding %>%
      group_by(LA.Code_2023) %>%
      summarize(across(where(is.numeric), sum, na.rm = TRUE)) 
    
    #total built
    joined_74_2000totalbuilding <- geographies %>%
      left_join(built1974_2000, by = "LA.Code")
    
    built74_00_summarised <- joined_74_2000totalbuilding %>%
      group_by(LA.Code_2023) %>%
      summarize(across(where(is.numeric), sum, na.rm = TRUE)) 
    
    #LA built 
    joined_74_00LAhousebuilding <- geographies %>% 
      left_join(LAbuilt1974_2000, by = "LA.Code")
    
    LAbuilt74_00_summarised <- joined_74_00LAhousebuilding %>%
      group_by(LA.Code_2023) %>%
      summarize(across(where(is.numeric), sum, na.rm = TRUE)) 
    
    #HA built 
    joined_74_00HAhousebuilding <- geographies %>% 
      left_join(HAbuilt1974_2000, by = "LA.Code")
    
    HAbuilt74_00_summarised <- joined_74_00HAhousebuilding %>%
      group_by(LA.Code_2023) %>%
      summarize(across(where(is.numeric), sum, na.rm = TRUE)) 
    
    #private built
    joined_74_00privatehousebuilding <- geographies %>% 
      left_join(privatebuilt1974_2000, by = "LA.Code")
    
    privatebuilt74_00_summarised <- joined_74_00privatehousebuilding %>%
      group_by(LA.Code_2023) %>%
      summarize(across(where(is.numeric), sum, na.rm = TRUE)) 
    

#calculate total demolished at 2023 geographies by subtracting net completed from total built 
    demolished74_79_summarised <- netbuilt74_00_summarised %>%
      mutate(
        `1974_Demolished` = `1974_COMPLETED_ALL` - `1974_Net_completed`,
        `1975_Demolished` = `1975_COMPLETED_ALL` - `1975_Net_completed`,
        `1976_Demolished` = `1976_COMPLETED_ALL` - `1976_Net_completed`,
        `1977_Demolished` = `1977_COMPLETED_ALL` - `1977_Net_completed`,
        `1978_Demolished` = `1978_COMPLETED_ALL` - `1978_Net_completed`,
        `1979_Demolished` = `1979_COMPLETED_ALL` - `1979_Net_completed`
      ) %>%
      select(LA.Code_2023, `1974_Demolished`, `1975_Demolished`, `1976_Demolished`, `1977_Demolished`, `1978_Demolished`, `1979_Demolished`)
    

#create continuous public total 
    ## first add LA and HA together to get public from the housebuilding reports 
    ## public built 74_00 - adding LA and HA together 
    publicbuilt74_00 <- LAbuilt74_00_summarised %>% 
      left_join(HAbuilt74_00_summarised, by= "LA.Code_2023")
    
    for (year in 1974:2000) {
      # Define the column names for LA and HA
      la_col <- paste0(year, "_COMPLETED_LOCAL")
      ha_col <- paste0(year, "_COMPLETED_HA")
      
      # Define the new column name for the public-built data
      public_col <- paste0(year, "_COMPLETED_PUBLIC")
      
      # Create the new column in publicbuilt74_00 by summing the LA and HA columns
      publicbuilt74_00[[public_col]] <- publicbuilt74_00[[la_col]] + publicbuilt74_00[[ha_col]]
    }
    
    # Create a vector of column names to keep: "LA.Code" and all "XXXX_COMPLETED_PUBLIC" columns
    columns_to_keep <- c("LA.Code_2023", paste0(1974:2000, "_COMPLETED_PUBLIC"))
    
    # Subset the dataframe to keep only these columns
    publicbuilt74_00 <- publicbuilt74_00[, columns_to_keep] 
    
    ##join with all affordable from Table 1011 
    publicbuilt74_22 <- publicbuilt74_00 %>% 
      left_join(all_affordable_built_1991_2022, by = "LA.Code_2023")
    
    #remove years where we use T1011 instead of housebuilding data 
    publicbuilt74_22 <- publicbuilt74_22 %>%
      select(-c(paste0(1991:2000, "_COMPLETED_PUBLIC"))) 

    
###add government data to geographies 
    #total dwelling stock 2001-2022
    joined_stock2001_2022 <- geographies %>% 
      left_join(stock2001_2022, by = "LA.Code")
    
    joined_stock2001_2022 <- joined_stock2001_2022 %>%
      mutate(across(15:36, as.numeric))
    
    stock01_22_summarised <- joined_stock2001_2022 %>%
      group_by(LA.Code_2023) %>%
      summarize(across(where(is.numeric), sum, na.rm = TRUE)) 
    
    #summarising data from table 253
    #all built 2000-2022
    built2000_2022 <- built2000_2022 %>%
      mutate(across(15:36, as.numeric))
    
    built00_22_summarised <- built2000_2022 %>%
      group_by(LA.Code_2023) %>%
      summarize(across(where(is.numeric), sum, na.rm = TRUE))  
    
    #LA built 2000-2022
    LAbuilt2000_2022 <- LAbuilt2000_2022 %>%
      mutate(across(15:36, as.numeric))
    
    LAbuilt00_22_summarised <- LAbuilt2000_2022 %>%
      group_by(LA.Code_2023) %>%
      summarize(across(where(is.numeric), sum, na.rm = TRUE))  
    
    #HA built 2000-2022
    HAbuilt2000_2022 <- HAbuilt2000_2022 %>%
      mutate(across(15:36, as.numeric))
    
    HAbuilt00_22_summarised <- HAbuilt2000_2022 %>%
      group_by(LA.Code_2023) %>%
      summarize(across(where(is.numeric), sum, na.rm = TRUE))  
    
    #private built 2000-2022
    privatebuilt2000_2022 <- privatebuilt2000_2022 %>%
      mutate(across(15:36, as.numeric))
    
    privatebuilt00_22_summarised <- privatebuilt2000_2022 %>%
      group_by(LA.Code_2023) %>%
      summarize(across(where(is.numeric), sum, na.rm = TRUE))  
    
    #Drop 2000
    privatebuilt00_22_summarised <- privatebuilt00_22_summarised[,-2]
    
    ## create T253 public for use later and convert to financial year 
    publicbuilt00_22 <- LAbuilt00_22_summarised %>% 
      left_join(HAbuilt00_22_summarised, by = "LA.Code_2023")
    
    for (year in 2000:2022) {
      # Define the column names for LA and HA
      la_col <- paste0(year, "_Local\nAuthority")
      ha_col <- paste0(year, "_Housing Associations")
      
      # Define the new column name for the public-built data
      public_col <- paste0(year, "_PUBLIC")
      
      # Create the new column in publicbuilt74_00 by summing the LA and HA columns
      publicbuilt00_22[[public_col]] <- publicbuilt00_22[[la_col]] + publicbuilt00_22[[ha_col]]
    }
    
    # Create a vector of column names to keep: "LA.Code" and all "XXXX_COMPLETED_PUBLIC" columns
    columns_to_keep <- c("LA.Code_2023", paste0(2000:2022, "_PUBLIC"))
    
    # Subset the dataframe to keep only these columns
    publicbuilt00_22 <- publicbuilt00_22[, columns_to_keep] 
    
    # Convert to financial years for 2001 to 2022
    for (year in 2001:2022) {
      prev_year <- year - 1
      current_col <- paste0(year, "_PUBLIC")
      prev_col <- paste0(prev_year, "_PUBLIC")
      new_col <- paste0(year, "_public_calendar_year")
      
      # Ensure the columns exist before performing calculations
      if (current_col %in% names(publicbuilt00_22) && prev_col %in% names(publicbuilt00_22)) {
        publicbuilt00_22[[new_col]] <- (publicbuilt00_22[[current_col]] * 0.75) + 
          (publicbuilt00_22[[prev_col]] * 0.25)
      } else {
        publicbuilt00_22[[new_col]] <- NA  # Assign NA if columns are missing
      }
    }
    
    # Remove columns with "_PUBLIC" in their names, keeping only "_public_calendar_year"
    publicbuilt00_22 <- publicbuilt00_22 %>%
      select(1, matches("_public_calendar_year$"))
    
#create new net building estimates for 1991 to 2000, using housebuilding data for private all the way through, housebuilding for public until 1990, T1011 for public after 1991
    #netbuilt works from 'completed all', so I need to subtract housebuilding public and then re-add T1011 public 
    netbuilt74_00_summarised <- netbuilt74_00_summarised %>% 
      left_join(publicbuilt74_00, by = "LA.Code_2023") 
    
    netbuilt74_00_summarised <- netbuilt74_00_summarised %>% 
      left_join(all_affordable_built_1991_2022, by = "LA.Code_2023") 
    
    # Loop through the years 1991 to 2000
    for (year in 1991:2000) {
      completed_all_col <- paste0(year, "_COMPLETED_ALL")
      completed_public_col <- paste0(year, "_COMPLETED_PUBLIC")
      year_col <- as.character(year)
      
      # Replace the 'COMPLETED_ALL' column for the given year
      netbuilt74_00_summarised[[completed_all_col]] <- netbuilt74_00_summarised[[completed_all_col]] - netbuilt74_00_summarised[[completed_public_col]] + netbuilt74_00_summarised[[year_col]]
          }

    # Step 1: Remove columns with "PUBLIC" in their title
    netbuilt74_00_summarised <- netbuilt74_00_summarised[, !grepl("PUBLIC", colnames(netbuilt74_00_summarised), ignore.case = TRUE)]
    
    # Step 2: Remove columns where the title is exactly 4 digits (e.g., "1991", "2000")
    netbuilt74_00_summarised <- netbuilt74_00_summarised[, !grepl("^\\d{4}$", colnames(netbuilt74_00_summarised))]
    
    
##Creating total dwelling stock for pre2001 building from 1981 and 1991 census data
  #------------------------------------------------------------------------------------------ 
  joined81stock_74building <- stock81_summarised %>% 
  left_join(netbuilt74_00_summarised, by = "LA.Code_2023") 

  joined81stock_74building$`1981Q1` <- joined81stock_74building$`1981_COMPLETED_ALL` / 4
  joined81stock_74building$`1981Q2_4` <- joined81stock_74building$`1981_COMPLETED_ALL` * 0.75

  joined81stock_74building$`1980_total` <- joined81stock_74building$`1981 total stock` - joined81stock_74building$`1981Q1`

  joined81stock_74building <- joined81stock_74building %>%
  mutate(`1979_total` = `1980_total` - `1980_COMPLETED_ALL`,
         `1978_total` = `1979_total` - `1979_Net_completed`,
         `1977_total` = `1978_total` - `1978_Net_completed`,
         `1976_total` = `1977_total` - `1977_Net_completed`,
         `1975_total` = `1976_total` - `1976_Net_completed`,
         `1974_total` = `1975_total` - `1975_Net_completed`,
         `1973_total` = `1974_total` - `1974_Net_completed`,
  )

  joined81stock_74building$`1981_total` <- joined81stock_74building$`1981 total stock` + joined81stock_74building$`1981Q2_4`

  # make 1982-1991 from 1981 census + created each year
  joined81stock_74building <- joined81stock_74building %>%
     mutate(`1982_total` = `1981_total` + `1982_COMPLETED_ALL`,
         `1983_total` = `1982_total` + `1983_COMPLETED_ALL`,
         `1984_total` = `1983_total` + `1984_COMPLETED_ALL`,
         `1985_total` = `1984_total` + `1985_COMPLETED_ALL`,
         `1986_total` = `1985_total` + `1986_COMPLETED_ALL`,
         `1987_total` = `1986_total` + `1987_COMPLETED_ALL`,
         `1988_total` = `1987_total` + `1988_COMPLETED_ALL`,
         `1989_total` = `1988_total` + `1989_COMPLETED_ALL`,
         `1990_total` = `1989_total` + `1990_COMPLETED_ALL`,
         `1991_total` = `1990_total` + `1991_COMPLETED_ALL`,)

  #add 1991 Census  
  joinedstock_74building <- joined81stock_74building %>% 
    left_join(stock91_summarised, by = "LA.Code_2023") 

  #work out difference in total stock between 1991 total and 1991 census 
  joinedstock_74building$adj1991 <- joinedstock_74building$`1991 total stock` - joinedstock_74building$`1991_total` 
  #divide by 10 to get yearly adjustment 
  joinedstock_74building$adj1991 <- joinedstock_74building$adj1991 / 10 

  #add adjustment to yearly totals to get adjusted totals
  joinedstock_74building <- joinedstock_74building %>%
     mutate(`1982_total` = `1982_total` + adj1991*1,
         `1983_total` = `1983_total` + adj1991*2,
         `1984_total` = `1984_total` + adj1991*3,
         `1985_total` = `1985_total` + adj1991*4,
         `1986_total` = `1986_total` + adj1991*5,
         `1987_total` = `1987_total` + adj1991*6,
         `1988_total` = `1988_total` + adj1991*7,
         `1989_total` = `1989_total` + adj1991*8,
         `1990_total` = `1990_total` + adj1991*9,
         `1991_total` = `1991_total` + adj1991*10,) 

  #remove Census data from 1981 and 1991 and adjustment for cleanliness 
  joinedstock_74building <- subset(joinedstock_74building, select = -`1981 total stock`)
  joinedstock_74building <- subset(joinedstock_74building, select = -`1991 total stock`)
  joinedstock_74building <- subset(joinedstock_74building, select = -`adj1991`)

  # Initialize the starting column `1981_total` (assuming it already exists)
  joinedstock_74building <- joinedstock_74building %>%
     mutate(`1992_total` = `1991_total` + `1992_COMPLETED_ALL`,
         `1993_total` = `1992_total` + `1993_COMPLETED_ALL`,
         `1994_total` = `1993_total` + `1994_COMPLETED_ALL`,
         `1995_total` = `1994_total` + `1995_COMPLETED_ALL`,
         `1996_total` = `1995_total` + `1996_COMPLETED_ALL`, 
         `1997_total` = `1996_total` + `1997_COMPLETED_ALL`,
         `1998_total` = `1997_total` + `1998_COMPLETED_ALL`,
         `1999_total` = `1998_total` + `1999_COMPLETED_ALL`,
         `2000_total` = `1999_total` + `2000_COMPLETED_ALL`)

  #keep only LA.Code.2023 and total columns 
  # Keep the first column and any column with 'total' in its title
  totals_columns <- c(TRUE, grepl("total", colnames(joinedstock_74building)[-1]))
  joinedstock_74building <- joinedstock_74building[, totals_columns] 

  #reorder columns so the years are in order 
  # Extract the column names except the first one
  col_names <- colnames(joinedstock_74building)[-1]
  # Extract the years from the column names using regular expressions
  years <- as.numeric(gsub("^(\\d{4})_.*", "\\1", col_names))
  # Sort the column names based on the extracted years
  sorted_col_names <- col_names[order(years)]
  # Reorder the columns in the original dataframe
  joinedstock_74building <- joinedstock_74building[, c("LA.Code_2023", sorted_col_names)]


##creating dataframe with all dwelling stock data in it 
  #--------------------------------------------------------------------------------------------
  #add geographies to dwelling stock 2001-2022 and summarise by 2023 LA code 
  joinedstock_1974_2022 <- joinedstock_74building %>% 
   left_join(stock01_22_summarised, by = "LA.Code_2023") 
  
  #calculate 1991-2000 average
  joinedstock_1974_2022$average91_00 <- (joinedstock_1974_2022$`2000_total` - joinedstock_1974_2022$`1991_total`) /10 
  #create projected 2001 on basis of this average 
  joinedstock_1974_2022$`2001_est` <- joinedstock_1974_2022$`2000_total` + joinedstock_1974_2022$`average91_00`

  #work out difference between 2001est and 2001 census, and divide by 10 to get adjustment 
  joinedstock_1974_2022$adj00_01 <- (joinedstock_1974_2022$`2001` - joinedstock_1974_2022$`2001_est`)/10
  
  #add adjustment to yearly totals to get adjusted totals
  joinedstock_1974_2022 <- joinedstock_1974_2022 %>%
   mutate(`1992_total` = `1992_total` + adj00_01*1,
         `1993_total` = `1993_total` + adj00_01*2,
         `1994_total` = `1994_total` + adj00_01*3,
         `1995_total` = `1995_total` + adj00_01*4,
         `1996_total` = `1996_total` + adj00_01*5,
         `1997_total` = `1997_total` + adj00_01*6,
         `1998_total` = `1998_total` + adj00_01*7,
         `1999_total` = `1999_total` + adj00_01*8,
         `2000_total` = `2000_total` + adj00_01*9,)

  #remove Census data from 1981 and 1991 and adjustment for cleanliness 
  joinedstock_1974_2022 <- subset(joinedstock_1974_2022, select = -`average91_00`)
  joinedstock_1974_2022 <- subset(joinedstock_1974_2022, select = -`2001_est`)
  joinedstock_1974_2022 <- subset(joinedstock_1974_2022, select = -`adj00_01`)
  joinedstock_1974_2022 <- subset(joinedstock_1974_2022, select = -`LA_name`)

  #name columns year only 
  start_year <- 1973
  num_years <- ncol(joinedstock_1974_2022) - 1
  year_sequence <- start_year:(start_year + num_years - 1)

  # Assign new column names starting from the third column
  colnames(joinedstock_1974_2022)[2:ncol(joinedstock_1974_2022)] <- as.character(year_sequence) 



##create total built, by LA, HA, and private estimates 1974-2022 - making complete year estimates (re-jigging financial year reporting)
#-----------------------------------------------------------------------------------------------------
  #total built - housebuilding all joined to T253
  built74_22 <- built74_00_summarised %>% 
    left_join(built00_22_summarised, by = "LA.Code_2023")  

  start_year <- 1974
  end_year <- 2022  # Adjust as needed
  colnames(built74_22)[-1] <- as.character(start_year:end_year)
  
  # Define the years to be filled in
  years_to_fill <- 2001:2022
  
  # Loop to create total columns
  for (year in years_to_fill) {
    prev_year <- year - 1
    built74_22 <- built74_22 %>%
      mutate(
        !!paste0(year, "_total") := get(paste0(prev_year)) * 0.25 + get(paste0(year)) * 0.75
      )
  }
  
  # Step 1: Identify columns to delete
  columns_to_delete <- colnames(built74_22) %in% c("2001", "2002", "2003", "2004", "2005",
                                                   "2006", "2007", "2008", "2009", "2010",
                                                   "2011", "2012", "2013", "2014", "2015",
                                                   "2016", "2017", "2018", "2019", "2020",
                                                   "2021", "2022")
  
  # Step 2: Delete identified columns
  built74_22 <- built74_22[, !columns_to_delete]
  colnames(built74_22)[-1] <- as.character(start_year:end_year)
  
  ### start work from here tomorrow!! need to produce public adjusted totals and output public / LA / HA in sensible way. All above this makes sense I think. 
  #watch out for financial years - don't need to make them like I have below. 
  
  #built works from 'completed all', so I need to subtract housebuilding public and then re-add T1011 public 
  built74_22 <- built74_22 %>% 
    left_join(publicbuilt74_00, by = "LA.Code_2023")
  
  built74_22 <- built74_22 %>% 
    left_join(publicbuilt00_22, by = "LA.Code_2023")
  
  built74_22 <- built74_22 %>% 
    left_join(all_affordable_built_1991_2022, by = "LA.Code_2023") 
  
  #remove housebuilding public 1991-2000 and T253 public 2001-2022, add T1011 back again 
  for (year in 1991:2022) {
    x_col <- paste0(year, ".x")
    y_col <- paste0(year, ".y")
    public_col <- ifelse(year <= 2000, 
                         paste0(year, "_COMPLETED_PUBLIC"), 
                         paste0(year, "_public_calendar_year"))
    pubadj_col <- paste0(year, "_pubadj")
    
    built74_22[[pubadj_col]] <- built74_22[[x_col]] - built74_22[[public_col]] + built74_22[[y_col]]
  }
  
  selected_columns <- c(1, 
                        grep("^\\d{4}$", names(built74_22)),   # 4-digit columns
                        grep("pubadj", names(built74_22)))    # 'pubadj' columns
  
  # Subset the dataframe
  built74_22 <- built74_22[, selected_columns]
  names(built74_22) <- gsub("_pubadj$", "", names(built74_22))

  
  ##LA built 
  LAbuilt74_22 <- LAbuilt74_00_summarised %>% 
    left_join(LA_built_1991_2022, by = "LA.Code_2023")  
  
  #remove 1991-2000 from housebuilding data and keep only T1011 data
  LAbuilt74_22 <- LAbuilt74_22[, -19:-28]
  
  names(LAbuilt74_22) <- gsub("_COMPLETED_LOCAL$", "", names(LAbuilt74_22))
  
  
  ##HA built 
  HAbuilt74_22 <- HAbuilt74_00_summarised %>% 
    left_join(HA_built_1991_2022, by = "LA.Code_2023")  
  
  #remove 1991-2000 from housebuilding data and keep only T1011 data
  HAbuilt74_22 <- HAbuilt74_22[, -19:-28]
  
  names(HAbuilt74_22) <- gsub("_COMPLETED_HA$", "", names(HAbuilt74_22))
  
  
  ## private built
  privatebuilt74_22 <- privatebuilt74_00_summarised %>% 
    left_join(privatebuilt00_22_summarised, by = "LA.Code_2023")  
  
  colnames(privatebuilt74_22)[-1] <- as.character(start_year:end_year)
  
  # Loop to create total columns
  for (year in years_to_fill) {
    prev_year <- year - 1
    privatebuilt74_22 <- privatebuilt74_22 %>%
      mutate(
        !!paste0(year, "_total") := get(paste0(prev_year)) * 0.25 + get(paste0(year)) * 0.75
      )
  }
  
  columns_to_delete <- colnames(privatebuilt74_22) %in% c(
                                                     "2001", "2002", "2003", "2004", "2005",
                                                     "2006", "2007", "2008", "2009", "2010",
                                                     "2011", "2012", "2013", "2014", "2015",
                                                     "2016", "2017", "2018", "2019", "2020",
                                                     "2021", "2022")
  
  privatebuilt74_22 <- privatebuilt74_22[, !columns_to_delete]
  colnames(privatebuilt74_22)[-1] <- as.character(start_year:end_year)
  
  ## public built - just edit column names from earlier made dataframe
  names(publicbuilt74_22) <- gsub("_COMPLETED_PUBLIC$", "", names(publicbuilt74_22))

  #tidy up dataframes: 
  columns_to_keep <- c(1, grep("^\\d{4}$", names(publicbuilt74_22)))
  publicbuilt74_22 <- publicbuilt74_22[, columns_to_keep]
  
  columns_to_keep <- c(1, grep("^\\d{4}$", names(HAbuilt74_22)))
  HAbuilt74_22 <- HAbuilt74_22[, columns_to_keep]
  
  columns_to_keep <- c(1, grep("^\\d{4}$", names(LAbuilt74_22)))
  LAbuilt74_22 <- LAbuilt74_22[, columns_to_keep]
  
  columns_to_keep <- c(1, grep("^\\d{4}$", names(social_built_1991_2022)))
  social_built_1991_2022 <- social_built_1991_2022[, columns_to_keep]
  
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

# Generate file path for Excel workbook with date
date <- format(Sys.Date(), "%Y-%m-%d")
file_name_geog <- paste0(date, "population_stock_housebuilding_1974_2022_2023geographies", ".xlsx")
dir_path_geog = paste0(dirname("~"),"/Centre for Cities/Centre for Cities POC - Documents/Research/Housing/History of Planning 2/Data/Output/From R scripts/Housebuilding/")
file_path_geog <- file.path(dir_path_geog, file_name_geog)

# Create a list of data frames
df_list_geog <- list(population_summarised, joinedstock_1974_2022, built74_22, LAbuilt74_22, HAbuilt74_22, privatebuilt74_22, publicbuilt74_22, demolished74_79_summarised, 
                     affordable_not_social_1991_2022, social_built_1991_2022)
names(df_list_geog) <- c("population_74_22", "net_stock_74_22", "gross_all_74_22", "gross_LA_74_22", "gross_HA_74_22", "gross_private_74_22", "gross_public_74_22", "total_demolished_74_79", 
                         "affordable_not_social_91_22", "social_91_22")                         

# Export data frames to Excel
export_to_excel(df_list_geog, file_path_geog)


#create new dataframes summarising by 1981 counties geographies 
#--------------------------------------------------------------------------------
population_summarised <- population_summarised %>% 
  left_join(lad_23_to_cty_81, by = "LA.Code_2023") 

pop_cty81 <- population_summarised %>%
  group_by(cty_81) %>%
  summarize(across(where(is.numeric), sum, na.rm = TRUE))  


joinedstock_1974_2022 <- joinedstock_1974_2022 %>% 
  left_join(lad_23_to_cty_81, by = "LA.Code_2023") 

net_stock_cty81 <- joinedstock_1974_2022 %>%
  group_by(cty_81) %>%
  summarize(across(where(is.numeric), sum, na.rm = TRUE))  


built74_22 <- built74_22 %>% 
  left_join(lad_23_to_cty_81, by = "LA.Code_2023") 

built_cty81 <- built74_22 %>%
  group_by(cty_81) %>%
  summarize(across(where(is.numeric), sum, na.rm = TRUE)) 


LAbuilt74_22 <- LAbuilt74_22 %>% 
  left_join(lad_23_to_cty_81, by = "LA.Code_2023") 

LAbuilt_cty81 <- LAbuilt74_22 %>%
  group_by(cty_81) %>%
  summarize(across(where(is.numeric), sum, na.rm = TRUE))  


HAbuilt74_22 <- HAbuilt74_22 %>% 
  left_join(lad_23_to_cty_81, by = "LA.Code_2023") 

HAbuilt_cty81 <- HAbuilt74_22 %>%
  group_by(cty_81) %>%
  summarize(across(where(is.numeric), sum, na.rm = TRUE))  


privatebuilt74_22 <- privatebuilt74_22 %>% 
  left_join(lad_23_to_cty_81, by = "LA.Code_2023") 

privatebuilt_cty81 <- privatebuilt74_22 %>%
  group_by(cty_81) %>%
  summarize(across(where(is.numeric), sum, na.rm = TRUE))  


publicbuilt74_22 <- publicbuilt74_22 %>% 
  left_join(lad_23_to_cty_81, by = "LA.Code_2023") 

publicbuilt_cty81 <- publicbuilt74_22 %>%
  group_by(cty_81) %>%
  summarize(across(where(is.numeric), sum, na.rm = TRUE))  


affordable_not_social_1991_2022 <- affordable_not_social_1991_2022 %>% 
  left_join(lad_23_to_cty_81, by = "LA.Code_2023") 

affordable_not_social_cty81 <- affordable_not_social_1991_2022 %>%
  group_by(cty_81) %>%
  summarize(across(where(is.numeric), sum, na.rm = TRUE))  


social_built_1991_2022 <- social_built_1991_2022 %>% 
  left_join(lad_23_to_cty_81, by = "LA.Code_2023") 

social_built_cty81 <- social_built_1991_2022 %>%
  group_by(cty_81) %>%
  summarize(across(where(is.numeric), sum, na.rm = TRUE))  


# Generate file path for Excel workbook with date-------------------------------
date <- format(Sys.Date(), "%Y-%m-%d")
file_name_geog <- paste0(date, "population_stock_housebuilding_1974_2022_1981geographies", ".xlsx")
dir_path_geog = paste0(dirname("~"),"/Centre for Cities/Centre for Cities POC - Documents/Research/Housing/History of Planning 2/Data/Output/From R scripts/Housebuilding/")
file_path_geog <- file.path(dir_path_geog, file_name_geog)

# Create a list of data frames
df_list_geog <- list(pop_cty81, net_stock_cty81, built_cty81, LAbuilt_cty81, HAbuilt_cty81, privatebuilt_cty81, publicbuilt_cty81, 
                     affordable_not_social_cty81, social_built_cty81)
names(df_list_geog) <- c("population_74_22", "net_stock_74_22", "gross_all_74_22", "gross_LA_74_22", "gross_HA_74_22", "gross_private_74_22", "gross_public_74_22", 
                         "affordable_not_social_91_22", "social_built_91_22")                         

# Export data frames to Excel
export_to_excel(df_list_geog, file_path_geog)







