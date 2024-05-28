library(dplyr)
#library(popmodules)
library(gglaplot)
library(ggplot2)
library(data.table)
library(lubridate)
library(plotly)
library(tidyverse)


data_dir <- 'C:/Families/Data_for_github/'

# Disable scientific notation
options(scipen=999)


#-------------------------------------------------------------------------------

# read data

input_longit_full <- readRDS(paste0(data_dir, "origin_destination_lad_2002_2022.rds"))  %>%   data.frame()

#

#-------------------------------------------------------------------------------
### OUT MIGRATION
#---------------------------------------------------------------------------------

## CREATE FILE WITH THOSE MOVING OUT FROM LONDON in single year July 2021 - June 2022

# Create domestic out-migration file selecting only for those originating in London
# Select only those who have destination out of London 
# Select only England and Wales
# The data are by single years and by gender.

input_london_dom_mign_out_2022 <- input_longit_full %>%
  filter(substr(gss_out,1,3) %in% c("E09")) %>% # "out" is origin
  filter(!(substr(gss_in,1,3) %in% c("E09"))) %>%   # "in" is destination
  filter(gss_in != "N92000002") %>% # Remove Northern Ireland
  filter(gss_in != "S92000003") %>% # Remove Scotland
  filter(year == "2022") %>%
  data.frame() 

write.csv(input_london_dom_mign_out_2022, paste0(data_dir, "/london_origin_2022.csv"))

## CREATE FILE WITH THOSE MOVING OUT FROM LONDON in 2002 - 2022

# Create domestic out-migration file selecting only for those originating in London
# Select only those who have destination out of London 
# Select only England and Wales
# The data are by single years and by gender.

input_london_dom_mign_out_2002to2022 <- input_longit_full %>%
  filter(substr(gss_out,1,3) %in% c("E09")) %>% # "out" is origin
  filter(!(substr(gss_in,1,3) %in% c("E09"))) %>%   # "in" is destination
  filter(gss_in != "N92000002") %>% # Remove Northern Ireland
  filter(gss_in != "S92000003") %>% # Remove Scotland
  data.frame() 

write.csv(input_london_dom_mign_out_2002to2022, paste0(data_dir, "/london_origin_2002to2022.csv"))


#------------------------------------------------------------------------
### IN MIGRATION
#---------------------------------------------------------------------------------

## CREATE FILE WITH THOSE MOVING INTO LONDON in single year JULY 2021 and JUNE 2022

# Create domestic in-migration file selecting only for those originating outside London
# Select only those who have destination in London 
# Select only England and Wales
# The data are by single years and by gender.

input_london_dom_mign_in_2022 <- input_longit_full %>%
  filter(substr(gss_in,1,3) %in% c("E09")) %>% # "in" is destination
  filter(!(substr(gss_out,1,3) %in% c("E09"))) %>%   # "out" is origin
  # so that data are comparable to out analysis, only include England and Wales
  filter(gss_out != "N92000002") %>% # Remove Northern Ireland
  filter(gss_out != "S92000003") %>% # Remove Scotland
  filter(year == "2022") %>%
  data.frame() 

write.csv(input_london_dom_mign_in_2022, paste0(data_dir, "london_destination_2022.csv"))

## CREATE FILE WITH THOSE MOVING INTO LONDON in 2002 - 2022

# Create domestic out-migration file selecting only for those originating outside London
# Select only those who have destination in London 
# Select only England and Wales
# The data are by single years and by gender.

input_london_dom_mign_in_2002to2022 <- input_longit_full %>%
  filter(substr(gss_in,1,3) %in% c("E09")) %>% # "in" is destination
  filter(!(substr(gss_out,1,3) %in% c("E09"))) %>%   # "out" is origin
  # so that data are comparable to out analysis, only include England and Wales
  filter(gss_out != "N92000002") %>% # Remove Northern Ireland
  filter(gss_out != "S92000003") %>% # Remove Scotland
  data.frame() 

write.csv(input_london_dom_mign_in_2002to2022, paste0(data_dir, "london_destination_2002to2022.csv"))


#------------------------------------------------------------------------
### WITHIN LONDON MIGRATION
#---------------------------------------------------------------------------------

## CREATE FILE WITH THOSE MOVING BETWEEN LONDON BOROUGHS during single year JULY 2021 - JUNE 2022

# Create domestic in-migration file selecting only for those moving within London
# Select only those who have destination in London 
# Select only those who have origin in London 
# The data are by single years and by gender.

input_london_dom_mign_within_2022 <- input_longit_full %>%
  filter(substr(gss_in,1,3) %in% c("E09")) %>% # "in" is destination
  filter(substr(gss_out,1,3) %in% c("E09")) %>%   # "out" is origin
  filter(year == "2022") %>%
  data.frame() 

write.csv(input_london_dom_mign_within_2022, paste0(data_dir, "london_within_2022.csv"))

## CREATE FILE WITH THOSE MOVING BETWEEN LONDON BOROUGHS in 2002 - 2022

# Create domestic out-migration file selecting only for those originating in London
# Select only those who have destination in London 
# Select only those who have origin in London 
# The data are by single years and by gender.

input_london_dom_mign_within_2002to2022 <- input_longit_full %>%
  filter(substr(gss_in,1,3) %in% c("E09")) %>% # "in" is destination
  filter(substr(gss_out,1,3) %in% c("E09")) %>%   # "out" is origin
 data.frame() 

write.csv(input_london_dom_mign_within_2002to2022, paste0(data_dir, "london_within_2002to2022.csv"))


#---------------------------------


