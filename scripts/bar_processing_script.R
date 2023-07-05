# SETUP 

# Library load ==== 

libraries <- c("tidyverse", "janitor", "data.table", "readxl")
lapply(libraries, require, character.only = T) # the following line of code reads in the required libraries for this script

# Processing of BAR data - Script explained # ====

# This script was written for the purpose of collating multiple BAR templates into a single dataset
# This involves two steps:

# 1) Formatting the excel spreadsheet into a consistent format
# 2) Joining these formatted entries into a single data set

# Throughout this script, comments are used to explain what each function does, however (hopefully) all that would need to be done is to run the script and it should work.

# Formatting function ====

# First, we ideally want the function to detect all files within a single folder. We can do this using list.files function.

data_directory <- r"(C:/Users/u453623/Documents/OFFLINE/bar_template_processing/data/)" # THIS LINE OF CODE POINTS TO WHERE ALL THE DATA WILL BE STORED, **CHANGE AS NEEDED** 

names <- list.files(data_directory)

all_files <- paste0(data_directory, names) # This line of code saves the names of all the files from the folder specified above into a variable called "all_files"

df.list <- lapply(all_files, read_excel, sheet = 2) # Apply function read_xlsx to all files in file directory. Stored in a list . We are specifically only interested in the 2nd sheet, which as all the data 

df.list_clean_names <- df.list |> lapply(row_to_names, 1) |> lapply(clean_names)

convert_to_na_except_first <- function(data) {
  data %>%
    mutate(x11_sic_code = ifelse(row_number() > 1, NA, x11_sic_code))
}

df.list_clean <- df.list_clean_names |> lapply(convert_to_na_except_first)

#moves the first row to column headers and formats the names consistently

# Now that we have all of our excel sheets loaded, we can write a function that will format the data set. The main issue here is how the spreadsheet is laid out.
# Ideally, each column should be its each own variable, column X will contain all values corresponding to X. 
# However, here some columns have nested sub-columns. So column X is actually column Xa, Xb, Xc, and all the values are stored in a column next to it. 


sample <- df.list_clean[[1]]



# Sample pivot_longer code to flatten the spreadsheet
# Proof it works, but now to find a way to make this work first across all vars that need to be flattened but also making this functional so that it can be applied to any number of elements in our dflist

# Pivot Wider ####

names_from_vars <- c("x3_b_location_if_uprn_is_missing", "x4_full_property_address", "x5_assessor_details", "x13_property_occupancy", "x17_heat_demand_m_wh_year", "na_6", "x20_heat_network_connection", "na_8", "x26_expected_impact_of_changes")
values_from_vars <- c("na", "na_2", "na_3", "na_4", "na_5", "x18_a_primary_fuel_demand_unit_year", "na_7", "x22_a_primary_heating_technology", "na_9")

result <- sample |> 
  pivot_wider(names_from = names_from_vars, 
              values_from = values_from_vars)





perform_pivot_wider <- function(data, key_vars, value_vars) {

    df <- data
  
  for (i in seq_along(key_vars)) {
    
    pivot_key <- key_vars[i]
    pivot_value <- value_vars[i]
    
    #print(c(pivot_value, pivot_key))
    
    df <- df %>%
      pivot_wider(names_from = pivot_key, 
                  values_from = pivot_value, 
                  names_repair = "unique")
  }
  
  return(df)
}


testing <- perform_pivot_wider(sample, key_vars = names_from_vars, value_vars = values_from_vars)


