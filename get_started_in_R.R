# Day of Data 2020 - January 10th, 2020
# Data Visualization Challenge
# UMN Building Energy Benchmarking Data

# This is short script that loads the energy dataset into your R environment
# and gets you started exploring the data


# Install and load libraries-----------------------------------------------------

# To run this script you will need to install and load these packages from CRAN.
# If you have trouble installing, you may not have administrator rights on your computer.
# Switch to administrator mode, request administrator access, or switch to Python. ;-)

install.packages(c("tidyverse", "readxl", "lubridate", "skimr"))

# Load Libraries
library(tidyverse)
library(readxl)
library(lubridate)
library(skimr)

# Optional: Turn off scientific notation so plots are easier to read
#options(scipen = 999) 


# Import data into R------------------------------------------------------------

# We will load sheets one by one into R data frames. R doesn't have the concept of 
# multiple sheets/tabs like Excel does, so we need to load each sheet of data into 
# its own data frame.

# Date columns need some special handling depending on how they are stored in Excel,
# so we are converting them all to objects of class "Date" as we read in the dates
# so R will treat them nicely later on.

# bldg_nbr is treated as character since a few values contain letters.

file <- "U of M Buildings Space and Utility Data - Simplified and Cleaned.xlsx"

buildings    <- read_excel(file, sheet = "Buildings") %>% 
  mutate(original_occupancy_date = as.Date(original_occupancy_date, '%m/%d/%Y'))

space_usages <- read_excel(file, sheet = "Building Space Usages")

baseline  <- read_excel(file, sheet = "Baseline") %>% 
  mutate(first_reading = as.Date(first_reading),
         last_reading = as.Date(last_reading))

monthly <- read_excel(file, sheet = "Monthly Consumption by Site", guess_max = 50000) %>% 
  mutate(start_dt = as.Date(start_dt),
         end_dt = as.Date(end_dt))

res_hall <- read_excel(file, sheet = "Monthly Consumption - Res Halls") %>% 
  mutate(start_dt = as.Date(start_dt),
         end_dt = as.Date(end_dt),
         bldg_nbr = as.character(bldg_nbr))


# Exploratory data analysis--------------------------------------------------------------------

# Skim from the skimr package is a quick way to summarize the dataframes and check that 
# the data load went as planned 
skim(buildings)
skim(space_usages)
skim(baseline)
skim(monthly)
skim(res_hall)


# Select, filter, and join dataframes------------------------------------------------------------

# Below is an example of how to select, filter, and join dataframes

baseline_bldg <- baseline %>%                     # start with the 'baseline' dataframe
  select(-bldg_name, -bldg_type, -current_sf) %>% # de-select specific columns from the dataframe
  inner_join(buildings, by = "bldg_nbr") %>%      # join the 'buildings' dataframe to the 'baseline' dataframe, with "bldg_nbr" as the join key
  filter(status == "OK")                          # filter for buildings that have a status of "OK"


# Make some basic data visualizations------------------------------------------------------------

# SCATTER PLOT

# Current square footage vs. efficiency
ggplot(baseline_bldg, aes(x = current_sf, y = current_eui_kbtu_per_sf)) +
  geom_point()

# Facet by building type
ggplot(baseline_bldg, aes(x = current_sf, y = current_eui_kbtu_per_sf)) +
  geom_point() +
  facet_wrap(. ~ bldg_type)  

# Year the building was first occupied vs. efficiency, facet by building type
ggplot(baseline_bldg, aes(x = year(original_occupancy_date), y = current_eui_kbtu_per_sf)) +
  geom_point() +
  facet_wrap(. ~ bldg_type)  


# BOX PLOT

# Efficiency by building type 
ggplot(baseline_bldg, aes(y = current_eui_kbtu_per_sf, x = bldg_type)) +
  geom_boxplot() +
  coord_flip()

# Order box plot by median current_eui_kbtu_per_sf
ggplot(baseline_bldg, aes(y = current_eui_kbtu_per_sf, x = fct_reorder(bldg_type, current_eui_kbtu_per_sf))) +
  geom_boxplot() +
  coord_flip()


# TIME SERIES PLOT

# Visualize natural gas usage over time across the different residence halls
ggplot(res_hall, aes(x = start_dt, y = natural_gas_kbtu, color = bldg_name)) +
  geom_line() +
  scale_x_date(date_breaks = "6 month", date_labels = "%b %y") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

