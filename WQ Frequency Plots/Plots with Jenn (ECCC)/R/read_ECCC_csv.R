#===============================================================================
# IMPORT AND FORMAT SINGLE DATASET DOWNLOADED FROM
#===============================================================================

#code takes filepth with
read_ECCC_csv <- function(filepth){
  
  #get stationID from filename (with format .....Exported_Samples_YT10MA0011_2022_09_19.csv
  stationID <- stringr::str_extract(filepth, "(?<=Exported_Samples_).*") %>% 
    stringr::str_extract("^[^_]+(?=_)")

  dat <- readr::read_csv(file = filepth) %>%
    #to get rid of spaces in column names
    dplyr::rename_all(~stringr::str_replace_all(., "\\s+", "")) %>%
    #format sample time to datetime
    dplyr::mutate(SampleTime2 = lubridate::parse_date_time(SampleTime, 
                                                           "%m/%d/%Y %H:%M")) %>%
    #add Date and stationID column
    dplyr::mutate(Date = lubridate::as_date(SampleTime2), 
                  StationID = stationID)
  
  return(dat)
}
