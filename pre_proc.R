# In this script I will create RDS files from the raw data, in order
# to upload them to shinyapps.io

# Load packages
library(data.table)
library(tidyverse)

# Read data
# Used data.table::fread instead of readr::read_csv because it's faster
data <- fread("unzip -p data/ships_04112020.zip")

# I decided to modify SHIP_ID column because it was not unique for each SHIPNAME
ship_data <- data %>% 
  group_by(SHIPTYPE, ship_type, SHIP_ID, SHIPNAME) %>% 
  summarise() %>% 
  # Arrange by SHIPNAME before assigning ID to improve UI
  arrange(SHIPNAME) %>% 
  # Create SHIP_ID_MOD (Modified SHIP_ID)
  rownames_to_column("SHIP_ID_MOD")

# Add new column to data
data <- data %>% 
  left_join(ship_data)

# Function to create ID - Description tibble
make_dict <- function(data, id, desc){
  id <- enquo(id)
  desc <- enquo(desc)
  
  data %>% 
    group_by(!!id, !!desc) %>% 
    summarise() %>% 
    arrange(!!desc)
}

# Create a table indicating ship type codes and description to improve 
# search performance
dict_shiptype <- ship_data %>% 
  make_dict(SHIPTYPE, ship_type)

# Save the useful datasets
saveRDS(data, "data/ships.RDS")
saveRDS(ship_data, "data/ship_data.RDS")
saveRDS(dict_shiptype, "data/dict_shiptype.RDS")
