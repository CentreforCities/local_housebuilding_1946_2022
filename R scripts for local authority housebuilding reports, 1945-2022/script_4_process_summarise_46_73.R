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


#get data!
#-----------------------------------------------------------------------------------------
# import geography lookup to get PUAs: 
path = paste0(dirname("~"),"/Centre for Cities/Centre for Cities POC - Documents/Research/Housing/History of Planning 2/Data/Input/Housing Returns/Phase 2 - geography joined/", collapse = NULL)
setwd(path) 

#Import flags and geography lookups for 1946 to 1973 
#bring in flagging data
path <- paste0(dirname("~"),"/Centre for Cities/Centre for Cities POC - Documents/Research/Housing/History of Planning 2/Data/Input/", collapse = NULL)
setwd(path)

flags_1971 <- read.xlsx("2024-07-22_GB_Lookups.xlsx", sheet = "lad_71")
colnames(flags_1971)[1] <- "equiv_1971_cd" 

flags_1981 <- read.xlsx("2024-07-22_GB_Lookups.xlsx", sheet = "cty_81")


###get data all from script 1 
#-----------------------------------------------------------------------------------------
# Get today's date in the format "YYYY-MM-DD"
today <- format(Sys.Date(), "%Y-%m-%d")

path = paste0(dirname("~"),"/Centre for Cities/Centre for Cities POC - Documents/Research/Housing/History of Planning 2/Data/Output/From R scripts/Housebuilding/", collapse = NULL)
setwd(path) 

# Construct the file name dynamically
file_name <- paste0(today, "built_and_population_1946_1973.xlsx")

####NOTE - you'll need to update the date on this file name, if there has been any update to the source of that code
population <- read.xlsx(file_name, sheet = "population")
Cumulative_built <- read.xlsx(file_name, sheet = "net built")
gross_all_building <- read.xlsx(file_name, sheet = "total built")
gross_LA_building <- read.xlsx(file_name, sheet = "LA built")
gross_HA_building <- read.xlsx(file_name, sheet = "HA built")
gross_private_building <- read.xlsx(file_name, sheet = "private built")
gross_public_building <- read.xlsx(file_name, sheet = "public built")
total_demolished <- read.xlsx(file_name, sheet = "total demolished")

#get household proportion data for each decade
path = paste0(dirname("~"),"/Centre for Cities/Centre for Cities POC - Documents/Research/Housing/History of Planning 2/Data/Input/", collapse = NULL)
setwd(path) 

district_proportions_of_counties <- read.xlsx("2024-07-30_District_cty_households_and_share_of_county_calcs.xlsx", sheet = "share_calcs")
colnames(district_proportions_of_counties)[6] <- "cty_81"

###process to get stocks data 
###---------------------------------------------------------------------------------------
path = paste0(dirname("~"),"/Centre for Cities/Centre for Cities POC - Documents/Research/Housing/History of Planning 2/Data/Output/From R scripts/Housebuilding/", collapse = NULL)
setwd(path) 

county_building_source <- paste0(today, "population_stock_housebuilding_46_73_1981geographies.xlsx")
county_stocks_source <-  paste0(today, "population_stock_housebuilding_1974_2022_1981geographies.xlsx")

starting_point <- read_excel(county_stocks_source, sheet="net_stock_74_22", range = cell_cols("A:B"))
ongoing_building <-  read_excel(county_building_source, sheet="built_minus_demos_cty_1981", range = cell_cols("A:AC")) 
gross_46_56 <- read_excel(county_building_source, sheet="gross_all_building_cty_1981", range = cell_cols("A:L")) 

combined <- merge(starting_point, ongoing_building, by.starting_point="cty_81") %>%
  rename(net_1971 = "1971_Net_completed", net_1961 = "1961_Net_completed")

#make draft county level stock totals on basis of 1973 estimate minus our built minus demolitions figures
combined$dec_73 <- combined$"1973"
combined$dec_72 <- combined$dec_73-combined$"1973_Net_completed"
combined$dec_71 <- combined$dec_72-combined$"1972_Net_completed"
combined$mar_71 <- combined$dec_71-(combined$net_1971*.75)

#bring in our holmans asjustment (removing wales on basis of proportion of households there in 1971), and comparing to our projected numbers
adj_holmans_stock_mar_71 <- 17024000*.945
our_stock_mar_71 <- sum(combined$mar_71)
adj_factor_70s <- (((our_stock_mar_71-adj_holmans_stock_mar_71)/our_stock_mar_71)*-1)/10

#save this value so I can use it to adjust the post 74 stock figures 
# Generate file path for RDS file
dir_path_geog = paste0(dirname("~"),"/Centre for Cities/Centre for Cities POC - Documents/Research/Housing/History of Planning 2/Data/Output/From R scripts/Housebuilding/")
rds_file_name <- paste0(today, "stock_70s_adj_factor", ".rds")
rds_file_path <- file.path(dir_path_geog, rds_file_name)

# Save the value to the RDS file
saveRDS(adj_factor_70s, file = rds_file_path)


#recreate our county stock estimates, adjusting for how far off our estimates were from the holmans whole country total 
combined$adj_dec_71 <- combined$dec_71*((adj_factor_70s*10)+1)
combined$adj_dec_72 <- combined$dec_72*((adj_factor_70s*9)+1)
combined$adj_dec_73 <- combined$dec_73*((adj_factor_70s*8)+1)

#understand our end of year 1971 numbers
our_stock_dec_71 <- sum(combined$dec_71)
our_adj_stock_dec_71 <- sum(combined$adj_dec_71)

#create draft county level stock totals, working backwards from 1971 number 
combined$dec_70 <- combined$adj_dec_71-combined$net_1971
combined$dec_69 <- combined$dec_70-combined$"1970_Net_completed"
combined$dec_68 <- combined$dec_69-combined$"1969_Net_completed"
combined$dec_67 <- combined$dec_68-combined$"1968_Net_completed"
combined$dec_66 <- combined$dec_67-combined$"1967_Net_completed"
combined$dec_65 <- combined$dec_66-combined$"1966_Net_completed"
combined$dec_64 <- combined$dec_65-combined$"1965_Net_completed"
combined$dec_63 <- combined$dec_64-combined$"1964_Net_completed"
combined$dec_62 <- combined$dec_63-combined$"1963_Net_completed"
combined$dec_61 <- combined$dec_62-combined$"1962_Net_completed"
combined$mar_61 <- combined$dec_61-(combined$net_1961*.75)

#bring in our holmans asjustment (removing wales on basis of proportion of households there in 1961), and comparing to our projected numbers
adj_holmans_stock_mar_61 <- 14646000*.943
our_stock_mar_61 <- sum(combined$mar_61)
adj_factor_60s <- (((our_stock_mar_61-adj_holmans_stock_mar_61)/our_stock_mar_61)*-1)/10

#recreate our county stock estimates, adjusting for how far off our estimates were from the holmans whole country total 
combined$adj_dec_61 <- combined$dec_61*((adj_factor_60s*10)+1)
combined$adj_dec_62 <- combined$dec_62*((adj_factor_60s*9)+1)
combined$adj_dec_63 <- combined$dec_63*((adj_factor_60s*8)+1)
combined$adj_dec_64 <- combined$dec_64*((adj_factor_60s*7)+1)
combined$adj_dec_65 <- combined$dec_65*((adj_factor_60s*6)+1)
combined$adj_dec_66 <- combined$dec_66*((adj_factor_60s*5)+1)
combined$adj_dec_67 <- combined$dec_67*((adj_factor_60s*4)+1)
combined$adj_dec_68 <- combined$dec_68*((adj_factor_60s*3)+1)
combined$adj_dec_69 <- combined$dec_69*((adj_factor_60s*2)+1)
combined$adj_dec_70 <- combined$dec_70*((adj_factor_60s*1)+1)

#understand our end of year 1961 numbers
our_stock_dec_61 <- sum(combined$dec_61)
our_adj_stock_dec_61 <- sum(combined$adj_dec_61)

#create draft county level stock totals, working backwards from 1971 number 
combined$dec_60 <- combined$adj_dec_61-combined$net_1961
combined$dec_59 <- combined$dec_60-combined$"1960_Net_completed"
combined$dec_58 <- combined$dec_59-combined$"1959_Net_completed"
combined$dec_57 <- combined$dec_58-combined$"1958_Net_completed"
combined$dec_56 <- combined$dec_57-combined$"1957_Net_completed" 

#add gross building data from early 1950s
combined <- merge(combined, gross_46_56, by.combined="cty_81") %>%
  rename(completed_1951 = "1951_COMPLETED_ALL") 

combined$dec_55 <- combined$dec_56-combined$"1956_COMPLETED_ALL"
combined$dec_54 <- combined$dec_55-combined$"1955_COMPLETED_ALL"
combined$dec_53 <- combined$dec_54-combined$"1954_COMPLETED_ALL"
combined$dec_52 <- combined$dec_53-combined$"1953_COMPLETED_ALL"
combined$dec_51 <- combined$dec_52-combined$"1952_COMPLETED_ALL"

#bring in demolitions data and summarise it by county
# Construct the file name dynamically
file_name <- paste0(today, "mastersheet_1946_1973.xlsx")

demos_1956 <-  read.xlsx(file_name, sheet = "master_demos")
demos_1956 <- demos_1956[, c("equiv_1971_cd", "1956_Total_demolished")] 
demos_1956 <- demos_1956 %>% 
  left_join(flags_1971, by = "equiv_1971_cd")

demos_1956_cty81 <- demos_1956 %>%
  group_by(cty_81) %>%
  summarize(across(where(is.numeric), sum, na.rm = TRUE))
demos_1956_cty81 <- demos_1956_cty81[, c("cty_81", "1956_Total_demolished")]  

combined <- merge(combined, demos_1956_cty81, by.combined="cty_81") 
combined$yearly_demos_51_55 <- combined$`1956_Total_demolished`/5  

#subtract the demos we know happened up to 1956 between 1951 and 1955 dec years
combined$dec_55_minus_demos <- combined$dec_55-combined$yearly_demos_51_55
combined$dec_54_minus_demos <- combined$dec_54-combined$yearly_demos_51_55
combined$dec_53_minus_demos <- combined$dec_53-combined$yearly_demos_51_55
combined$dec_52_minus_demos <- combined$dec_52-combined$yearly_demos_51_55
combined$dec_51_minus_demos <- combined$dec_51-combined$yearly_demos_51_55
combined$mar_51 <- combined$dec_51_minus_demos-(combined$completed_1951*.75)


#bring in our holmans asjustment (removing wales on basis of proportion of the trend from 71 to 61 (0.002 less than 1961), and comparing to our projected numbers
###### ALERT ALERT - update the wales proportion ~~~~~~~~~~~~ #############
adj_holmans_stock_mar_51 <- 12530000*.941
our_stock_mar_51 <- sum(combined$mar_51)
adj_factor_50s <- (((our_stock_mar_51-adj_holmans_stock_mar_51)/our_stock_mar_51)*-1)/10 

combined$adj_dec_51 <- combined$dec_51_minus_demos*((adj_factor_60s*10)+1)
combined$adj_dec_52 <- combined$dec_52_minus_demos*((adj_factor_60s*9)+1)
combined$adj_dec_53 <- combined$dec_53_minus_demos*((adj_factor_60s*8)+1)
combined$adj_dec_54 <- combined$dec_54_minus_demos*((adj_factor_60s*7)+1)
combined$adj_dec_55 <- combined$dec_55_minus_demos*((adj_factor_60s*6)+1)
combined$adj_dec_56 <- combined$dec_56*((adj_factor_60s*5)+1)
combined$adj_dec_57 <- combined$dec_57*((adj_factor_60s*4)+1)
combined$adj_dec_58 <- combined$dec_58*((adj_factor_60s*3)+1)
combined$adj_dec_59 <- combined$dec_59*((adj_factor_60s*2)+1)
combined$adj_dec_60 <- combined$dec_60*((adj_factor_60s*1)+1) 

#calculate estimated 1940s stocks numbers
combined$dec_50 <- combined$adj_dec_51-combined$completed_1951
combined$dec_49 <- combined$dec_50-combined$"1950_COMPLETED_ALL"
combined$dec_48 <- combined$dec_49-combined$"1949_COMPLETED_ALL"
combined$dec_47 <- combined$dec_48-combined$"1948_COMPLETED_ALL"
combined$dec_46 <- combined$dec_47-combined$"1947_COMPLETED_ALL" 
combined$dec_45 <- combined$dec_46-combined$"1946_COMPLETED_ALL" 

tidy_county_stocks_46_73 <- combined %>% select(cty_81, dec_45, dec_46, dec_47, dec_48, dec_49, dec_50, adj_dec_51, adj_dec_52,
                                                adj_dec_53, adj_dec_54, adj_dec_55, adj_dec_56, adj_dec_57, adj_dec_58, adj_dec_59,
                                                adj_dec_60, adj_dec_61, adj_dec_62, adj_dec_63, adj_dec_64, adj_dec_65, adj_dec_66, 
                                                adj_dec_67, adj_dec_68, adj_dec_69, adj_dec_70, adj_dec_71, adj_dec_72, adj_dec_73)



#produce district level stock estimates on the basis of county stocks, distributed by the proportion of households in census years
#-------------------------------------------------------------------------------------
#isolate the census year county stock totals
cty_stocks_71 <- tidy_county_stocks_46_73[, c("cty_81", "adj_dec_71")] 
cty_stocks_61 <- tidy_county_stocks_46_73[, c("cty_81", "adj_dec_61")] 
cty_stocks_51 <- tidy_county_stocks_46_73[, c("cty_81", "adj_dec_51")] 

#join district gross building data to James' calcs on districts' proportion of households in their counties
district_proportions_and_building <- gross_all_building %>% 
  left_join(district_proportions_of_counties, by = "equiv_1971_cd")

district_stocks <- district_proportions_and_building %>% 
  left_join(cty_stocks_71, by = "cty_81") 
district_stocks <- district_stocks %>% 
  left_join(cty_stocks_61, by = "cty_81") 
district_stocks <- district_stocks %>% 
  left_join(cty_stocks_51, by = "cty_81") 

#create estimates for district stocks in census years by multiplying county stocks by the share of county households in that district
district_stocks$`shared_dec1971` <- district_stocks$adj_dec_71 * district_stocks$share_of_cty_71 
district_stocks$`shared_dec1961` <- district_stocks$adj_dec_61 * district_stocks$share_of_cty_61 
district_stocks$`shared_dec1951` <- district_stocks$adj_dec_51 * district_stocks$share_of_cty_51 

#building forward from 1971 to get early 70s stock by district. 
#note - I don't adjust these. They start from an already adjusted 1971 number. If we were being fancy, we could try to work out how much the adjustment
#shfor these two years should be, but it will be tiny - so have ignored
district_stocks$`dec1972` <- district_stocks$`shared_dec1971` + district_stocks$"1972_COMPLETED_ALL"
district_stocks$`dec1973` <- district_stocks$`dec1972` + district_stocks$"1973_COMPLETED_ALL"

## subtract back to 61 and adjust  
district_stocks$`dec1970` <- district_stocks$`shared_dec1971` - district_stocks$"1971_COMPLETED_ALL"
district_stocks$`dec1969` <- district_stocks$`dec1970` - district_stocks$"1970_COMPLETED_ALL"
district_stocks$`dec1968` <- district_stocks$`dec1969` - district_stocks$"1969_COMPLETED_ALL"
district_stocks$`dec1967` <- district_stocks$`dec1968` - district_stocks$"1968_COMPLETED_ALL"
district_stocks$`dec1966` <- district_stocks$`dec1967` - district_stocks$"1967_COMPLETED_ALL"
district_stocks$`dec1965` <- district_stocks$`dec1966` - district_stocks$"1966_COMPLETED_ALL"
district_stocks$`dec1964` <- district_stocks$`dec1965` - district_stocks$"1965_COMPLETED_ALL"
district_stocks$`dec1963` <- district_stocks$`dec1964` - district_stocks$"1964_COMPLETED_ALL"
district_stocks$`dec1962` <- district_stocks$`dec1963` - district_stocks$"1963_COMPLETED_ALL"
district_stocks$`dec1961` <- district_stocks$`dec1962` - district_stocks$"1962_COMPLETED_ALL"

#work out difference in total stock between 1991 total and 1991 census 
district_stocks$adj1961 <- district_stocks$`shared_dec1961` - district_stocks$`dec1961` 
#divide by 10 to get yearly adjustment 
district_stocks$adj1961 <- district_stocks$adj1961 / 10 

#add adjustment to yearly totals to get adjusted totals
district_stocks <- district_stocks %>%
  mutate(`adjdec1970` = `dec1970` + adj1961*1,
         `adjdec1969` = `dec1969` + adj1961*2,
         `adjdec1968` = `dec1968` + adj1961*3,
         `adjdec1967` = `dec1967` + adj1961*4,
         `adjdec1966` = `dec1966` + adj1961*5,
         `adjdec1965` = `dec1965` + adj1961*6,
         `adjdec1964` = `dec1964` + adj1961*7,
         `adjdec1963` = `dec1963` + adj1961*8,
         `adjdec1962` = `dec1962` + adj1961*9)


## subtract back to 51 and adjust  
district_stocks$`dec1960` <- district_stocks$`shared_dec1961` - district_stocks$"1961_COMPLETED_ALL"
district_stocks$`dec1959` <- district_stocks$`dec1960` - district_stocks$"1960_COMPLETED_ALL"
district_stocks$`dec1958` <- district_stocks$`dec1959` - district_stocks$"1959_COMPLETED_ALL"
district_stocks$`dec1957` <- district_stocks$`dec1958` - district_stocks$"1958_COMPLETED_ALL"
district_stocks$`dec1956` <- district_stocks$`dec1957` - district_stocks$"1957_COMPLETED_ALL"
district_stocks$`dec1955` <- district_stocks$`dec1956` - district_stocks$"1955_COMPLETED_ALL"
district_stocks$`dec1954` <- district_stocks$`dec1955` - district_stocks$"1955_COMPLETED_ALL"
district_stocks$`dec1953` <- district_stocks$`dec1954` - district_stocks$"1954_COMPLETED_ALL"
district_stocks$`dec1952` <- district_stocks$`dec1953` - district_stocks$"1953_COMPLETED_ALL"
district_stocks$`dec1951` <- district_stocks$`dec1952` - district_stocks$"1952_COMPLETED_ALL"

#work out difference in total stock between 1991 total and 1991 census 
district_stocks$adj1951 <- district_stocks$`shared_dec1951` - district_stocks$`dec1951` 
#divide by 10 to get yearly adjustment 
district_stocks$adj1951 <- district_stocks$adj1951 / 10 

#add adjustment to yearly totals to get adjusted totals
district_stocks <- district_stocks %>%
  mutate(`adjdec1960` = `dec1960` + adj1951*1,
         `adjdec1959` = `dec1959` + adj1951*2,
         `adjdec1958` = `dec1958` + adj1951*3,
         `adjdec1957` = `dec1957` + adj1951*4,
         `adjdec1956` = `dec1956` + adj1951*5,
         `adjdec1955` = `dec1955` + adj1951*6,
         `adjdec1954` = `dec1954` + adj1951*7,
         `adjdec1953` = `dec1953` + adj1951*8,
         `adjdec1952` = `dec1952` + adj1951*9)

##subtract back to 45 
## subtract back to 41 and adjust  
district_stocks$`dec1950` <- district_stocks$`shared_dec1951` - district_stocks$"1951_COMPLETED_ALL"
district_stocks$`dec1949` <- district_stocks$`dec1950` - district_stocks$"1950_COMPLETED_ALL"
district_stocks$`dec1948` <- district_stocks$`dec1949` - district_stocks$"1949_COMPLETED_ALL"
district_stocks$`dec1947` <- district_stocks$`dec1948` - district_stocks$"1948_COMPLETED_ALL"
district_stocks$`dec1946` <- district_stocks$`dec1947` - district_stocks$"1947_COMPLETED_ALL"
district_stocks$`dec1945` <- district_stocks$`dec1946` - district_stocks$"1946_COMPLETED_ALL"

tidy_district_stocks_45_73 <- district_stocks %>% select(equiv_1971_cd, dec1945, dec1946, dec1947, dec1948, dec1949, dec1950, shared_dec1951, adjdec1952,
                                                         adjdec1953, adjdec1954, adjdec1955, adjdec1956, adjdec1957, adjdec1958, adjdec1959,
                                                  adjdec1960, shared_dec1961, adjdec1962, adjdec1963, adjdec1964, adjdec1965, adjdec1966, 
                                                  adjdec1967, adjdec1968, adjdec1969, adjdec1970, shared_dec1971, dec1972, dec1973)


#hard code population corrections for Crawley UD and horsham RD - because issues with non-reporting of population in raw data. 
#-------------------------------------------------------------------------------------------------------
#long story - chat to James and Maurice about how and why we use these numbers! 
#Import flags and geography lookups for 1946 to 1973 
#bring in flagging data
path <- paste0(dirname("~"),"/Centre for Cities/Centre for Cities POC - Documents/Research/Housing/History of Planning 2/Data/Input/", collapse = NULL)
setwd(path)

crawley_pop_fix <- read.xlsx("Crawley_population_fixer.xlsx") 

# Identify the columns to be updated (1946_POP to 1956_POP)
pop_columns <- paste0(1946:1956, "_POP")

# Perform a left join to bring in the relevant data from crawley_pop_fix
population <- population %>%
  left_join(crawley_pop_fix, by = "equiv_1971_cd", suffix = c("", "_fix")) %>%
  mutate(across(all_of(pop_columns), ~ ifelse(!is.na(get(paste0(cur_column(), "_fix"))),
                                              get(paste0(cur_column(), "_fix")),
                                              .))) %>%
  select(-ends_with("_fix"))


#------------------------------------------------------------------------------------------------------
#summarising statistics by cty_1981 (county 1981), 
#can add to this code easily - especially once we've added tags to the geography lookup 
#population
population_joined <- population %>%
  left_join(flags_1971, by = "equiv_1971_cd") 
population_joined$GBshare <- population_joined$gb_area_ha / population_joined$area_ha

population_PUA <- population_joined %>%
  group_by(pua) %>%
  summarize(across(where(is.numeric), sum, na.rm = TRUE))  
population_PUA$GBshare <- population_PUA$gb_area_ha / population_PUA$area_ha 

population_cty_1981 <- population_joined %>%
  group_by(cty_81) %>%
  summarize(across(contains("POP"), sum, na.rm = TRUE))
population_cty_1981 <- population_cty_1981 %>% 
  left_join(flags_1981, by = "cty_81")

#net stocks 
stocks_joined <- tidy_district_stocks_45_73 %>%
  left_join(flags_1971, by = "equiv_1971_cd")
stocks_joined$GBshare <- stocks_joined$gb_area_ha / stocks_joined$area_ha

stocks_PUA <- stocks_joined %>%
  group_by(pua) %>%
  summarize(across(where(is.numeric), sum, na.rm = TRUE))  
stocks_PUA$GBshare <- stocks_PUA$gb_area_ha / stocks_PUA$area_ha 

stocks_cty_1981 <- tidy_county_stocks_46_73 %>% 
  left_join(flags_1981, by = "cty_81")

#completed minus demolitions 
built_minus_demos <- Cumulative_built %>% 
  left_join(flags_1971, by = "equiv_1971_cd")
built_minus_demos$GBshare <- built_minus_demos$gb_area_ha / built_minus_demos$area_ha

built_minus_demos_PUA <- built_minus_demos %>% 
  group_by(pua) %>%
  summarize(across(where(is.numeric), sum, na.rm = TRUE)) 
built_minus_demos_PUA$GBshare <- built_minus_demos_PUA$gb_area_ha / built_minus_demos_PUA$area_ha 

built_minus_demos_cty_1981 <- built_minus_demos %>% 
  group_by(cty_81) %>%
  summarize(across(where(is.numeric), sum, na.rm = TRUE)) 
built_minus_demos_cty_1981 <- built_minus_demos_cty_1981 %>% 
  left_join(flags_1981, by = "cty_81")

#gross_all
gross_all_building_joined <- gross_all_building %>%
  left_join(flags_1971, by = "equiv_1971_cd") 
gross_all_building_joined$GBshare <- gross_all_building_joined$gb_area_ha / gross_all_building_joined$area_ha

gross_all_building_PUA <- gross_all_building_joined %>%
  group_by(pua) %>%
  summarize(across(where(is.numeric), sum, na.rm = TRUE))
gross_all_building_PUA$GBshare <- gross_all_building_PUA$gb_area_ha / gross_all_building_PUA$area_ha 

gross_all_building_cty_1981 <- gross_all_building_joined %>%
  group_by(cty_81) %>%
  summarize(across(where(is.numeric), sum, na.rm = TRUE)) 
gross_all_building_cty_1981 <- gross_all_building_cty_1981 %>% 
  left_join(flags_1981, by = "cty_81")

#gross_LA 
gross_LA_building_joined <- gross_LA_building %>%
  left_join(flags_1971, by = "equiv_1971_cd") 
gross_LA_building_joined$GBshare <- gross_LA_building_joined$gb_area_ha / gross_LA_building_joined$area_ha

gross_LA_building_PUA <- gross_LA_building_joined %>%
  group_by(pua) %>%
  summarize(across(where(is.numeric), sum, na.rm = TRUE))
gross_LA_building_PUA$GBshare <- gross_LA_building_PUA$gb_area_ha / gross_LA_building_PUA$area_ha 

gross_LA_building_cty_1981 <- gross_LA_building_joined %>%
  group_by(cty_81) %>%
  summarize(across(where(is.numeric), sum, na.rm = TRUE))  
gross_LA_building_cty_1981 <- gross_LA_building_cty_1981 %>% 
  left_join(flags_1981, by = "cty_81")

#gross_HA
gross_HA_building_joined <- gross_HA_building %>%
  left_join(flags_1971, by = "equiv_1971_cd") 
gross_HA_building_joined$GBshare <- gross_HA_building_joined$gb_area_ha / gross_HA_building_joined$area_ha

gross_HA_building_PUA <- gross_HA_building_joined %>%
  group_by(pua) %>%
  summarize(across(where(is.numeric), sum, na.rm = TRUE))
gross_HA_building_PUA$GBshare <- gross_HA_building_PUA$gb_area_ha / gross_HA_building_PUA$area_ha 

gross_HA_building_cty_1981 <- gross_HA_building_joined %>%
  group_by(cty_81) %>%
  summarize(across(where(is.numeric), sum, na.rm = TRUE))  
gross_HA_building_cty_1981 <- gross_HA_building_cty_1981 %>% 
  left_join(flags_1981, by = "cty_81")

#gross_private
gross_private_building_joined <- gross_private_building %>%
  left_join(flags_1971, by = "equiv_1971_cd") 
gross_private_building_joined$GBshare <- gross_private_building_joined$gb_area_ha / gross_private_building_joined$area_ha

gross_private_building_PUA <- gross_private_building_joined %>%
  group_by(pua) %>%
  summarize(across(where(is.numeric), sum, na.rm = TRUE))
gross_private_building_PUA$GBshare <- gross_private_building_PUA$gb_area_ha / gross_private_building_PUA$area_ha 

gross_private_building_cty_1981 <- gross_private_building_joined %>%
  group_by(cty_81) %>%
  summarize(across(where(is.numeric), sum, na.rm = TRUE)) 
gross_private_building_cty_1981 <- gross_private_building_cty_1981 %>% 
  left_join(flags_1981, by = "cty_81")

#gross_public
gross_public_building_joined <- gross_public_building %>%
  left_join(flags_1971, by = "equiv_1971_cd") 
gross_public_building_joined$GBshare <- gross_public_building_joined$gb_area_ha / gross_public_building_joined$area_ha

gross_public_building_PUA <- gross_public_building_joined %>%
  group_by(pua) %>%
  summarize(across(where(is.numeric), sum, na.rm = TRUE))
gross_public_building_PUA$GBshare <- gross_public_building_PUA$gb_area_ha / gross_public_building_PUA$area_ha 

gross_public_building_cty_1981 <- gross_public_building_joined %>%
  group_by(cty_81) %>%
  summarize(across(where(is.numeric), sum, na.rm = TRUE)) 
gross_public_building_cty_1981 <- gross_public_building_cty_1981 %>% 
  left_join(flags_1981, by = "cty_81") 

#total_demolished 
total_demolished_joined <- total_demolished %>% 
  left_join(flags_1971, by = "equiv_1971_cd") 

#make whole country totals for each tenure by summing all rows 
population_ENG <- population %>% 
  select(-equiv_1971_cd) %>% 
  select(-equiv_1971_nm) %>% 
  summarise(across(everything(), sum, na.rm = TRUE)) 

stocks_ENG <- tidy_district_stocks_45_73 %>% 
  select(-equiv_1971_cd) %>% 
  summarise(across(everything(), sum, na.rm = TRUE)) 

gross_total_building_ENG <- gross_all_building %>% 
  select(-equiv_1971_cd) %>% 
  select(-equiv_1971_nm) %>% 
  summarise(across(everything(), sum, na.rm = TRUE))

gross_public_building_ENG <- gross_public_building %>% 
  select(-equiv_1971_cd) %>% 
  select(-equiv_1971_nm) %>% 
  summarise(across(everything(), sum, na.rm = TRUE))

gross_private_building_ENG <- gross_private_building %>% 
  select(-equiv_1971_cd) %>% 
  select(-equiv_1971_nm) %>% 
  summarise(across(everything(), sum, na.rm = TRUE))

#calculate housebuilding rates at district level, total and by tenure
#----------------------------------------------------------------------------------------------------------
#total
housebuilding_rate_LA <- stocks_joined %>%
  left_join(gross_all_building_joined, by = "equiv_1971_cd") 

for (year in 1946:1973) {
  column_index <- year - 1900
  housebuilding_rate_LA[[paste0("GR", year)]] <- housebuilding_rate_LA[[column_index]] / housebuilding_rate_LA[[column_index - 44]]
}

housebuilding_rate_LA <- housebuilding_rate_LA[, c(1, grep("GR", colnames(housebuilding_rate_LA)))]

#private
private_housebuilding_rate_LA <- stocks_joined %>%
  left_join(gross_private_building_joined, by = "equiv_1971_cd") 

for (year in 1946:1973) {
  column_index <- year - 1900
  private_housebuilding_rate_LA[[paste0("GR", year)]] <- private_housebuilding_rate_LA[[column_index]] / private_housebuilding_rate_LA[[column_index - 44]]
}

private_housebuilding_rate_LA <- private_housebuilding_rate_LA[, c(1, grep("GR", colnames(private_housebuilding_rate_LA)))]

#public
public_housebuilding_rate_LA <- stocks_joined %>%
  left_join(gross_public_building_joined, by = "equiv_1971_cd") 

for (year in 1946:1973) {
  column_index <- year - 1900
  public_housebuilding_rate_LA[[paste0("GR", year)]] <- public_housebuilding_rate_LA[[column_index]] / public_housebuilding_rate_LA[[column_index - 44]]
}

public_housebuilding_rate_LA <- public_housebuilding_rate_LA[, c(1, grep("GR", colnames(public_housebuilding_rate_LA)))] 

#local authority
LA_housebuilding_rate_LA <- stocks_joined %>%
  left_join(gross_LA_building_joined, by = "equiv_1971_cd") 

for (year in 1946:1973) {
  column_index <- year - 1900
  LA_housebuilding_rate_LA[[paste0("GR", year)]] <- LA_housebuilding_rate_LA[[column_index]] / LA_housebuilding_rate_LA[[column_index - 44]]
}

LA_housebuilding_rate_LA <- LA_housebuilding_rate_LA[, c(1, grep("GR", colnames(LA_housebuilding_rate_LA)))]

#housing association
HA_housebuilding_rate_LA <- stocks_joined %>%
  left_join(gross_HA_building_joined, by = "equiv_1971_cd") 

for (year in 1962:1973) {
  # Calculate the index for the corresponding year
  gr_index <- year - 1962 + 46
  HA_housebuilding_rate_LA[[paste0("GR", year)]] <- HA_housebuilding_rate_LA[[gr_index]] / HA_housebuilding_rate_LA[[year - 1962 + 18]]
}

HA_housebuilding_rate_LA <- HA_housebuilding_rate_LA[, c(1, grep("GR", colnames(HA_housebuilding_rate_LA)))] 

#demolition rate 
demolition_rate_LA <- stocks_joined %>% 
  left_join(total_demolished_joined, by = "equiv_1971_cd") 

DRyears <- 1956:1973
numerator_indices <- 52:69
denominator_indices <- 12:29

# Loop through the years and calculate demolition rates
for (i in seq_along(DRyears)) {
  year <- DRyears[i]
  numerator_index <- numerator_indices[i]
  denominator_index <- denominator_indices[i]
  
  # Create a new column dynamically
  demolition_rate_LA[[paste0("DR", year)]] <- demolition_rate_LA[[numerator_index]] / demolition_rate_LA[[denominator_index]]
}

demolition_rate_LA <- demolition_rate_LA[, c(1, grep("DR", colnames(demolition_rate_LA)))] 

#calculate county-level rates
#------------------------------------------------------------------------------------
#total 
housebuilding_rate_cty <- stocks_cty_1981 %>% 
  left_join(gross_all_building_cty_1981, by = "cty_81")

for (year in 1946:1973) {
  # Calculate the index for the corresponding year
  gr_index_cty <- year - 1946 + 39
  housebuilding_rate_cty[[paste0("GR", year)]] <- housebuilding_rate_cty[[gr_index_cty]] / housebuilding_rate_cty[[year - 1946 + 2]]
} 

housebuilding_rate_cty <- housebuilding_rate_cty[, c(1, grep("GR", colnames(housebuilding_rate_cty)))]
 
#private 
private_housebuilding_rate_cty <- stocks_cty_1981 %>% 
  left_join(gross_private_building_cty_1981, by = "cty_81")

for (year in 1946:1973) {
  # Calculate the index for the corresponding year
  gr_index_cty <- year - 1946 + 39
  private_housebuilding_rate_cty[[paste0("GR", year)]] <- private_housebuilding_rate_cty[[gr_index_cty]] / private_housebuilding_rate_cty[[year - 1946 + 2]]
}

private_housebuilding_rate_cty <- private_housebuilding_rate_cty[, c(1, grep("GR", colnames(private_housebuilding_rate_cty)))]

#public
public_housebuilding_rate_cty <- stocks_cty_1981 %>% 
  left_join(gross_public_building_cty_1981, by = "cty_81")

for (year in 1946:1973) {
  # Calculate the index for the corresponding year
  gr_index_cty <- year - 1946 + 39
  public_housebuilding_rate_cty[[paste0("GR", year)]] <- public_housebuilding_rate_cty[[gr_index_cty]] / public_housebuilding_rate_cty[[year - 1946 + 2]]
}

public_housebuilding_rate_cty <- public_housebuilding_rate_cty[, c(1, grep("GR", colnames(public_housebuilding_rate_cty)))]

#local authority 
LA_housebuilding_rate_cty <- stocks_cty_1981 %>% 
  left_join(gross_LA_building_cty_1981, by = "cty_81")

for (year in 1946:1973) {
  # Calculate the index for the corresponding year
  gr_index_cty <- year - 1946 + 39
  LA_housebuilding_rate_cty[[paste0("GR", year)]] <- LA_housebuilding_rate_cty[[gr_index_cty]] / LA_housebuilding_rate_cty[[year - 1946 + 2]]
}

LA_housebuilding_rate_cty <- LA_housebuilding_rate_cty[, c(1, grep("GR", colnames(LA_housebuilding_rate_cty)))]

#housing association
HA_housebuilding_rate_cty <- stocks_cty_1981 %>% 
  left_join(gross_HA_building_cty_1981, by = "cty_81")

for (year in 1962:1973) {
  # Calculate the index for the corresponding year
  gr_index_cty <- year - 1962 + 39
  HA_housebuilding_rate_cty[[paste0("GR", year)]] <- HA_housebuilding_rate_cty[[gr_index_cty]] / HA_housebuilding_rate_cty[[year - 1962 + 18]]
}

HA_housebuilding_rate_cty <- HA_housebuilding_rate_cty[, c(1, grep("GR", colnames(HA_housebuilding_rate_cty)))]


#export a final version of the 1945-1973 data at district level 
# Generate file path for Excel workbook with date 01--------------------------------
date <- format(Sys.Date(), "%Y-%m-%d")
file_name_geog <- paste0(date, "FINAL_45_73_lad71_data_output", ".xlsx")
dir_path_geog = paste0(dirname("~"),"/Centre for Cities/Centre for Cities POC - Documents/Research/Housing/History of Planning 2/Data/Output/From R scripts/Housebuilding/")
file_path_geog <- file.path(dir_path_geog, file_name_geog)

# Create a list of data frames
df_list_geog <- list(population_joined, stocks_joined, gross_all_building_joined, gross_private_building_joined, gross_public_building_joined, 
                     gross_LA_building_joined, gross_HA_building_joined, total_demolished_joined, housebuilding_rate_LA, private_housebuilding_rate_LA, public_housebuilding_rate_LA, 
                     LA_housebuilding_rate_LA, HA_housebuilding_rate_LA, demolition_rate_LA)
names(df_list_geog) <- c("population", "stocks", "gross_all_building", "gross_private_building", "gross_public_building", 
                         "gross_LA_building", "gross_HA_building", "total_demolished", "total_building_rate", "private_building_rate", 
                         "public_building_rate", "LA_building_rate", "HA_building_rate", "demolition_rate")   

# Export data frames to Excel
export_to_excel(df_list_geog, file_path_geog)

#export a final version of the 1945-1973 data at higher geographies
# Generate file path for Excel workbook with date 01--------------------------------
date <- format(Sys.Date(), "%Y-%m-%d")
file_name_geog <- paste0(date, "FINAL_45_73_summarised_data_output", ".xlsx")
dir_path_geog = paste0(dirname("~"),"/Centre for Cities/Centre for Cities POC - Documents/Research/Housing/History of Planning 2/Data/Output/From R scripts/Housebuilding/")
file_path_geog <- file.path(dir_path_geog, file_name_geog)

# Create a list of data frames
df_list_geog <- list(population_cty_1981, stocks_cty_1981, built_minus_demos_cty_1981, gross_all_building_cty_1981, gross_LA_building_cty_1981, gross_HA_building_cty_1981,
                     gross_private_building_cty_1981, gross_public_building_cty_1981, housebuilding_rate_cty, private_housebuilding_rate_cty, public_housebuilding_rate_cty, 
                     LA_housebuilding_rate_cty, HA_housebuilding_rate_cty,
                     population_PUA, stocks_PUA, built_minus_demos_PUA, gross_all_building_PUA, gross_private_building_PUA, gross_public_building, gross_LA_building_PUA, gross_HA_building_PUA, 
                     population_ENG, stocks_ENG, gross_total_building_ENG, gross_public_building_ENG, gross_private_building_ENG)
names(df_list_geog) <- c("population_cty_1981", "stocks_cty_1981", "built_minus_demos_cty_1981", "gross_all_building_cty_1981", "gross_LA_building_cty_1981",
                         "gross_HA_building_cty_1981", "gross_private_building_cty_1981", "gross_public_building_cty_1981", "total_building_rate_cty_1981", "private_building_rate_cty_1981", 
                         "public_building_rate_cty_1981", "LA_building_rate_cty_1981", "HA_building_rate_cty_1981",
                         "population_PUA", "stocks_PUA", "built_minus_demos_PUA", "gross_total_PUA", "gross_private_PUA", "gross_public_PUA", "gross_LA_PUA", "gross_HA_PUA", 
                         "population_ENG", "stocks_ENG", "gross_total_building_ENG", "gross_public_building_ENG", "gross_private_building_ENG")                             

# Export data frames to Excel
export_to_excel(df_list_geog, file_path_geog) 


