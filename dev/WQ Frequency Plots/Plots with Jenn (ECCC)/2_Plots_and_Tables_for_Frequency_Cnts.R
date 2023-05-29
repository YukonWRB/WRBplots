#===============================================================================
# Create Plots and Summary Tables for sampling frequency
#===============================================================================

library(tidyverse)  #installs ggplot2, purrr, tibble, dplyr, tidyr, string, readr, forcats
library(here)

source(here::here("R", "contPlot.R"))


# Load dataset ------------------------------------------------------------

#run 1_Import_and_Format_dataset.R to create alldata.rds

alldata <- readr::read_rds(here::here("data", "alldata.rds"))


# Summary Tables -----------------------------------------------------------
#csv copy saved to results folder

#station summary
station_summary <- alldata %>% 
  group_by(StationID, station_name) %>%
  summarize(date_range = paste0(min(year, na.rm=TRUE), "-", max(year, na.rm=TRUE)), 
            startDate = min(Date, na.rm=TRUE), 
            endDate = max(Date, na.rm = TRUE), 
            nyrs = max(year, na.rm=TRUE) - min(year, na.rm=TRUE) + 1,
            nyrs_wData = n_distinct(year), 
            nsamples = n_distinct(SampleNumber))

readr::write_excel_csv(station_summary, here::here("results/station_summary.csv"))
           
#station-by-variable summary
stn_by_var_summary <- alldata %>% 
  group_by(StationID, station_name,  Variable_Group, VariableName) %>%
  summarize(date_range = paste0(min(year, na.rm=TRUE), "-", max(year, na.rm=TRUE)), 
            startDate = min(Date, na.rm=TRUE), 
            endDate = max(Date, na.rm = TRUE), 
            nyrs = max(year, na.rm=TRUE) - min(year, na.rm=TRUE) + 1,
            nyrs_wData = n_distinct(year), 
            n = n(),
            ncen = sum(Flag == "L", na.rm=TRUE),
            nsamples = n_distinct(SampleNumber),
            nunits = n_distinct(UnitCode), 
            nVMVs = n_distinct(VMVCode))
            
readr::write_excel_csv(station_summary, here::here("results/stn_by_var_summary.csv"))


# SampleNumber Counts by Station-Variable-Month-Year ----------------------

sampleN_byVar <- alldata %>% 
  group_by(StationID, station_name, station_label,  Variable_Group, VariableName, year, month) %>%
  summarize(nsamples = n_distinct(SampleNumber)) %>% #based on number of unique SampleNumber
  ungroup()

readr::write_excel_csv(sampleN %>% select(-station_label), here::here("results/sampleCounts_by_stn_variable_yr_mth.csv"))


# Continuity Plot ---------------------------------------------------------
#contPlot.R function can be used for various combinations, for example: 
#1) all variables at 1 station
#2) 1 variables for all stations -- color by vmv, units or other attribute

  # ALL VARIABLES AT 1 STATION:   -------------------------------------------
  
  #To run for 1 station you need to filter alldata by 1 station ID
  #----------------------------------------------------
  d1 <- alldata %>% 
    dplyr::filter(StationID=="YT10MA0011")

  #NOTE: with so many variables you could put dissolved and total metals in a seperate plot for better readability....
  #To do this run for each of the following, and use in place of d1 in the code below
  #unique(d1$Variable_Group)
  #metals <- d1 %>% filter(VariableName %in% c("Extractable Metals", "Total Metals", "Dissolved Metals"))
  #non_metals <- d1 %>% filter!VariableName %in% c("Extractable Metals", "Total Metals", "Dissolved Metals")
  
  p1 <- contPlot(d1, 
               datevar = Date, yvar = VariableName, 
               color_var = NULL, color_title = NULL,
               facet_var = Variable_Group,
               xlabel = NULL, ylabel = "Variable", 
               title = unique(d1$station_label), 
               subtitle=paste0("Record Period: ",  min(d1$Date, na.rm=TRUE), " to ", max(d1$Date, na.rm=TRUE)))

  p1
  
  #TO SAVE as image:
  #note: you can simply change the file extension in the name to get in different graphics device (pdf, png, ...)
  ggplot2::ggsave(plot=p1, filename=here::here("results", "test.png"), width=8.5, height=11)
  
  #CODE TO TO LOOP THROUGH ALL STATIONS: 
  #----------------------------------------------------
  all_stn_plots <- alldata %>% 
    mutate(StationID2 = StationID) %>% 
    tidyr::nest(data=c(-StationID2)) %>%
    mutate(plot = purrr::map(data,  ~contPlot(.x, 
                                              datevar = Date, yvar = VariableName, 
                                              color_var = NULL, color_title = NULL,
                                              facet_var = Variable_Group,
                                              xlabel = NULL, ylabel = "Variable", 
                                              title = unique(.x[["station_label"]]), 
                                              subtitle=paste0("Record Period: ",  min(.x$Date, na.rm=TRUE), " to ", max(.x$Date, na.rm=TRUE)))))
  
  #to view each plot use: 
  all_stn_plots$plot[[1]]
  
 
  # Display all plots using purrr::walk
  purrr::walk(all_stn_plots$plot, print) 
  
  #to loop through and save all plots
  file_names <- stringr::str_c(here::here("results", paste0(all_stn_plots$StationID2, "_DotPlot.png")))
  pwalk(list(file_names, all_stn_plots$plot), ggsave, width=8.5, height=11)
  
 
  # 1 VARIABLE AT ALL STATIONS: -------------------------------------------
  
  #To run for 1 station filter alldata by 1 Variable
  #----------------------------------------------------
  d2 <- alldata %>% 
    dplyr::filter(VariableName=="Zinc Total")
  

  p2 <- contPlot(d2, 
                 datevar = Date, yvar = StationID, 
                 color_var = "VMVCode", 
                 color_title = "VMV (Variable-Method)",
                 facet_var = NULL,
                 xlabel = NULL, ylabel = "Station", 
                 title = unique(d2$VariableName), 
                 subtitle=paste0("Record Period: ",  min(d2$Date, na.rm=TRUE), " to ", max(d2$Date, na.rm=TRUE)))
  
  p2
  
  #TO SAVE as image: change the test.png to whatever name you want
  #note: you can simply change the file extension in the name to get in different graphics device (pdf, png, ...)
  ggplot2::ggsave(plot=p2, filename=here::here("results", "test2.png"), width=8.5, height=11)
  
  
  #TO LOOP THROUGH ALL STATIONS: 
  #----------------------------------------------
  all_var_plots <- alldata %>% 
    mutate(Variable = VariableName) %>% 
    tidyr::nest(data=c(-Variable)) %>%
    mutate(plot = purrr::map(data,  ~contPlot(.x, 
                                              datevar = Date, yvar = StationID, 
                                              color_var = "VMVCode", color_title = "VMV (Variable-Method)",
                                              facet_var = NULL,
                                              xlabel = NULL, ylabel = "Station", 
                                              title = unique(.x$VariableName), 
                                              subtitle=paste0("Record Period: ",  min(.x$Date, na.rm=TRUE), " to ", max(.x$Date, na.rm=TRUE)))))
  
  #to view each plot use: 
  all_var_plots$plot[[1]]
  
  
  # Display all plots using purrr::walk
  purrr::walk(all_var_plots$plot, print) 
  
  #to loop through and save all plots
  file_names2 <- stringr::str_c(here::here("results", paste0(all_var_plots$Variable, "_DotPlot.png")))
  pwalk(list(file_names2, all_var_plots$plot), ggsave, width=8.5, height=11)
  


  
  
  
  
  

# Example to run for all variables or stations ----------------------------

  # Format list of all possible reports to run and required parameters
  # Run for all parameters
  all_params <- unique(alldata$StationID)
  all_variables <- unique(alldata$VariableName)
  



