#===============================================================================
# IMPORT AND FORMAT DATASETS FOR YT ECCC DATASETS
#===============================================================================

library(dplyr)
library(readr)
library(here)
library(tidyr)

source(here::here("R", "read_ECCC_csv.R"))


# Setup -------------------------------------------------------------------

#Put all station files you want to import/format and combine here. 
#files names must be in the following naming convention for current read_ECCC_csv.R code -- Exported_Samples_YT08AA0010_2022_09_19
pth <- here::here("data-raw", "station_datasets")


# Create and Format dataset -----------------------------------------------
# NOTE - this only has to be run the first time to create the alldata.rds file (or when you update with new files/data)

# vars is used to add group based on variable name
# you can edit the VariableGroup column to whatever you want
vars <- readr::read_csv(file=here::here("data-raw", "variable_xref.csv")) 

#create a stn file with 
stations <- readr::read_csv(file=here::here("data-raw", "YTstations.csv")) %>%
  dplyr::mutate(station_label = paste0(station_name, " (", station_no, ")"))

#Get list of datasets and import/format into single dataframe
#read_ECCC_csv code also takes station from file name and appends as a column
alldata <-
  #Creates a list of files in the pth folder in csv -- all with be run.
  fs::dir_ls(pth, regexp = "\\.csv$") %>%
  #runs read_ECCC_csv on all files in the pth folder list -- and converts to single dataframe
  purrr::map_dfr(read_ECCC_csv)  %>%
  
  #add variable Group and station info
  dplyr::left_join(vars, by="VariableName") %>%
  dplyr::left_join(stations, by=c("StationID" = "station_no")) %>%
  
  #add other date info
  dplyr::mutate(year = lubridate::year(Date), 
                month = lubridate::month(Date)) %>%
  
  #convert / in names to _ (because when using b/c / causes issues for looping with file names.)
  mutate(VariableName = stringr::str_replace(VariableName, "/", "_"))

#to view stucture: 
#glimpse(df)

#save as rds -- you can import this file directly into other scripts later
readr::write_rds(alldata, file=here::here("data", "alldata.rds"))
