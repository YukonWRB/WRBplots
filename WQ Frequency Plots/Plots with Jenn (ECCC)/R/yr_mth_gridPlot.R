
# Year-Month grid of Sample Counts ----------------------------------------
# requires single station-variable dataset

#dat must be dataframe of samplecount by year/month with 1 station; or 1 station/variable combo
#required columns: SampleNumber, year as numeric, month as numeric, station_label, VariableName
#...used to groupby in addition to stationID, month, year columns
yr_mth_grid <- function(dat, title, ...){
  
  stn_label <- unique(dat$station_label)
  
  cnts <- dat %>% 
    group_by(StationID, station_name, station_label,  Variable_Group, VariableName, year, month) %>%
    #group_by(StationID, station_name, station_label,  Variable_Group, VariableName, year, month) %>%
    summarize(nsamples = n_distinct(SampleNumber)) %>% #based on number of unique SampleNumber
    ungroup() $>$
    select(..., nsamples, )

  
  #complete for all year/months
  pdat <- dd %>% select(StationID, station_name, station_label, year, month, nsamples)
    tidyr::complete(year = full_seq(year,1), month=full_seq(month,1)) 
    #dplyr::mutate(monthName = factor(month.abb[month], c(month.abb, "Total"), ordered = TRUE))
  
  #get and add totals by year
  yr_total <-  pdat %>% 
    group_by(StationID, station_name, station_label, year) %>% 
    summarise(year_total = sum(nsamples, na.rm=TRUE), .groups="drop") %>%
    bind_rows(pdat)
  
  
   
    mutate(monthName= factor('Total', c(month.abb, "Total"), ordered = TRUE))
    
  
  #create plot
  p <- ggplot(pdat , aes(x=factor(monthName), y=factor(year))) + 
    geom_tile(color="black", aes(fill=factor(nsamples))) + 
    geom_text(aes(label = nsamples), size=2) +
    scale_fill_brewer(palette = "Greens", na.value = "white") +
    
    #add totals
    geom_point(data = yr_total, size = 5, shape = 19, color="white") +
    geom_text(data = yr_total, size = 2, aes(label = year_total), face="bold") + 
    
  #format plot
    theme_bw() + 
    ggplot2::theme(
      axis.title=element_blank(), 
      #axis.text.x = element_text(angle = 90, vjust = 0.5), 
      legend.position = "right", 
      legend.justification = "top", 
      panel.grid.major.x = element_blank() 
      #panel.grid.minor = element_blank(), 
      #panel.background=element_rect(fill="white")
      ) + 
    ggplot2::labs(
      subtitle=unique(pdat$station_label), 
      title = title, 
      fill = "Cnt of SampleNo",   
      x=NULL, 
      y=NULL) 
    
    
  
  p  
  
  
} 



