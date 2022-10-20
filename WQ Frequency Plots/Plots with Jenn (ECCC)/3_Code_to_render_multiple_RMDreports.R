#===============================================================================
# Run multiple/batch Rmarkdown reports -- Station Report
#===============================================================================

#suggest restart R session CTRL+ SHIFT+F10

library(rmarkdown)
library(purrr)
library(tidyverse)

# Set path to data file to use -----------------------------------------

data_path <-  here::here("data", "alldata.rds")

# Set path to rmd file to run. --------------------------------------------

rmd_pth <- here::here("Rmd", "Stn_SamplingFrequency.Rmd")

# Load and format the datasets --------------------------------------------

alldata <- readr::read_rds(data_path)


# Format list of all possible reports to run -----------------------------

reports <- alldata  %>%  
  #get unique list of Sites
  select(StationID) %>% distinct() %>%
  #set filename and rmarkdown parameters for report
  mutate(filename = purrr::map_chr(StationID, ~paste0(.x, "_Station_Freq_Report.html" )), 
         rmd_params = purrr::map(StationID, ~list(alldata=data_path, 
                                                site=.x))
  )


# Render rmarkdown documents ----------------------------------------------
#will overwrite html reports with the same files names -- suggest moving existing ones in you haven't already 

for (i in 1:nrow(reports)){
  rmarkdown::render(
    rmd_pth,
    output_file =  reports$filename[i], 
    #output_dir = savepath,   #can't get this to work - would probably work with self contained option?
    params=reports$rmd_params[[i]])
}



# Set the report parameters -----------------------------------------------

site <- "YT08AA0010"

data_path <-  here::here("data", "alldata.rds")
rmd_pth <- here::here("Rmd", "Stn_SamplingFrequency.Rmd")


rmarkdown::render(rmd_pth, 
                  output_file = paste0(site, "_Station_Freq_Report.html" ), 
                  params=list(alldata=data_path , 
                              site=site))


# RUN STATION SUMMARY PLOTS RMD -------------------------------------------



