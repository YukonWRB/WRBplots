# YOWN Aquarius Interface and Plotting
# November 2021
# Reads level data from input Aquarius time series and outputs a plot to a designated folder
# Code by Cole Fischer, Groundwater Technologist, YG-ENV-WRB-GW-OPS. Adapted by Ghislain de Laplante, Climate Change and Water Data Scientist, to work as a function without magick package.
# Aquarius code package (dependency) by Touraj Farahmand 2017-10-01

#TODO: check and remove packages below that are not necessary.
#TODO: add package names as package::function in the entire code to work as a function.


library(plyr)
library(data.table)
library(ggplot2)
library(cowplot)

#' Title Graphing utility for YOWN wells
#'
#' @param AQTSServerID The web address of your Aquarius server.
#' @param AQTSUsername The Aquarius username you wish to use.
#' @param AQTSPassword The password associated with the username.
#' @param dateRange The date range your wish to graph for. Use "all" for the entirety of the time-series (1950-01-01 to today) or specify two dates as such: c("2000-01-01", "2021-01-01")
#' @param timeRange Defaults to the entire day. Set in the same manner as dateRange with hh:mm:ss format if you wish to plot only a portion of a day; in that case dateRange should have two identical dates.
#' @param AQID The Aquarius ID of the site you wish to graph.
#' @param timeSeriesID The time-series you wish to graph, matching exactly how it is written in Aquarius (case sensitive).
#' @param chartXInterval The x-axis interval markings, specified as per scales::date_breaks
#' @param chartType The type of chart. Currently supporting only "Level", "SpC", "Temperature".
#'@param saveTo The directory where you wish to save your graph. Will create a folder with the AQID if it does not yet exist and saves the image as a .png with a name composed of the chartType, AQID and the current date.
#' @param specName Specify a file name here only if you wish to override the default naming of AQID and the current date; file name will still start with chartType. You should modify the NULL default if generating two graphs that will be named identically (same CharType, AQID, and date but different )
#'
#' @return A .png image of the time series with Yukon logos, saved to the directory specified in saveTo.
#'
#' @importFrom ggplot2 ggplot aes geom_line theme_bw theme labs scale_x_datetime scale_y_continuous scale_y_reverse element_text element_rect waiver
#' @export
#'


#TODO: make the chartType automatically determined according to the name of the timeSeriesID specified.
#TODO: Cole, check and make sure that the default for saveTo is correct. The script automatically creates a folder specific for each AQID (so YOWN-xxx) if it does not yet exist.
#TODO: Cole, let's make sure that the default behavior of overwriting a file if generated for the same site, TS type, and on the same day is desirable. This would happen unless specName is specified.

#YOWNplot <- function(AQTSServerID="https://yukon.aquaticinformatics.net/AQUARIUS", AQTSUsername, AQTSPassword, dateRange="all", timeRange=c("00:00:00", "23:59:59"), AQID, timeSeriesID="Wlevel_btoc.Calculated", chartXInterval="1 year", chartType="Level", saveTo="//envgeoserver/share/WaterResources/Groundwater/YOWN_WL_CHART_OUTPUTS", specName=NULL) {

#Use the code below to run the function with everything preset, by calling YOWNplot().
YOWNplot <- function(AQTSServerID="https://yukon.aquaticinformatics.net/AQUARIUS", AQTSUsername="gtdelapl", AQTSPassword="WQ*2021!", dateRange="all", timeRange=c("00:00:00", "23:59:59"), AQID="YOWN-1907", timeSeriesID="Water Temp.TEMPERATURE", chartXInterval="1 year", chartType="Temperature", saveTo="C:/Users/gtdelapl/Desktop", specName=NULL) {

#Use the code below to run the script outside of a function.
AQTSServerID="https://yukon.aquaticinformatics.net/AQUARIUS"
AQTSUsername="gtdelapl"
AQTSPassword="WQ*2021!"
dateRange="all"
timeRange=c("00:00:00", "23:59:59")
AQID="YOWN-1907"
timeSeriesID="Wlevel_btoc.Calculated"
chartXInterval="1 year"
chartType="Level"
saveTo="C:/Users/gtdelapl/Desktop"
specName=NULL

  # Aquarius Connection configuration, if statement to either download all or part of the time-series.
  if (dateRange[1]=="all"){
    config = list(
      # Aquarius server credentials
      server = AQTSServerID, username = AQTSUsername, password = AQTSPassword,
      # time series name@location EX: Wlevel_btoc.Calculated@YOWN-XXXX
      timeSeriesName = paste0(timeSeriesID,"@",AQID),
      # Analysis time period
      eventPeriodStartDay = "1950-01-01", eventPeriodEndDay = as.character(Sys.Date()),
      # Report title
      uploadedReportTitle = "Test Plot",
      # Remove pre-existing reports with the same name from Aquarius
      removeDuplicateReports = TRUE)
    dateRange <- c("1950-01-01", as.character(Sys.Date()))
  }
  if (((dateRange[1]=="all")==FALSE & (length(dateRange)==2))==TRUE) {
    config = list(
      # Aquarius server credentials
      server = AQTSServerID, username = AQTSUsername, password = AQTSPassword,
      # time series name@location EX: Wlevel_btoc.Calculated@YOWN-XXXX
      timeSeriesName = paste0(timeSeriesID,"@", AQID),
      # Analysis time period
      eventPeriodStartDay = dateRange[1], eventPeriodEndDay = dateRange[2],
      # Report title
      uploadedReportTitle = "Test Plot",
      # Remove pre-existing reports with the same name from Aquarius
      removeDuplicateReports = TRUE)
  }
  if(((dateRange[1]=="all")==FALSE &(length(dateRange)!=2))==TRUE){
    print("dateRange does not appear to be in the right format. Please verify that you are specifying either \"all\" or a vector of two dates in format yyy-mm-dd")
  }

  # Load supporting code
  source("R/timeseries_client.R")
  # Connect to Aquarius server
  timeseries$connect(config$server, config$username, config$password)

  # Data download
  # Get the location metadata
  locationData = timeseries$getLocationData(timeseries$getLocationIdentifier(config$timeSeriesName))

  utcOffset = timeseries$getUtcOffsetText(locationData$UtcOffset)
  startOfDay = "T00:00:00"
  endOfDay = "T23:59:59.9999999"

  # Prepare for downloading data points based on specified period start and end or for all data points
  fromPeriodStart = paste0(config$eventPeriodStartDay, startOfDay, utcOffset)
  toPeriodEnd = paste0(config$eventPeriodEndDay, endOfDay, utcOffset)
  periodLabel = sprintf("%s - %s", config$eventPeriodStartDay, config$eventPeriodEndDay)

  # Read corrected time-series data from Aquarius
  RawDL <- timeseries$getTimeSeriesCorrectedData(c(config$timeSeriesName),
                                                    queryFrom = fromPeriodStart,
                                                    queryTo = toPeriodEnd)
  #Fix the start time and end time to match either that specified or that in the time-series, whichever is shorter.
  trueStart <- RawDL$Points$Timestamp[1]
  trueStart <- substr(trueStart, 1, 10)
  trueEnd <- RawDL$Points$Timestamp[nrow(RawDL$Points)]
  trueEnd <- substr(trueEnd, 1, 10)

   if (dateRange[1]=="1950-01-01"){
   dateRange[1] <- trueStart
   dateRange[2] <- trueEnd
   }
  if (dateRange[1]!="1950-01-01"){
    if (dateRange[1] < trueStart){
      dateRange[1] <- trueStart
    }
    if (dateRange[2] > trueEnd){
      dateRange[2] <- trueEnd
    }
  }

  # Format data and prepare for plotting
  # Create full timestamp series spanning specified (or automatically selected) time range, 1hr intervals
  fullTS <- data.table::as.data.table(seq.POSIXt(strptime(paste(dateRange[1], timeRange[1]), format = "%Y-%m-%d %T"),
                                     strptime(paste(dateRange[2], timeRange[2]), format = "%Y-%m-%d %T"),
                                     by="hour"))
  data.table::setnames(fullTS, old = c("x"), new = c("timestamp"))

  # format base Aquarius time series
  timestamp <- data.table::setDT(data.table::as.data.table(strptime(substr(RawDL$Points$Timestamp,0,19), "%FT%T")))
  value <- data.table::setDT(data.table::as.data.table(RawDL$Points$Value))
  rawplotdata <- as.data.frame(cbind(timestamp, value))
  data.table::setnames(rawplotdata, old = c("x", "Numeric"), new = c("timestamp", "value"))

  # Join full timestamp series to native data time series
  fullplotdata <- dplyr::full_join(fullTS, rawplotdata)

  # Identify data gaps of greater than 6 hours (indicative of logger failure) to prevent gap fill on plot
  NAcomp <- rle(!is.na(fullplotdata$value))
  NAcomp$values[which(NAcomp$lengths>6 & !NAcomp$values)] <- TRUE
  NAadd <- inverse.rle(NAcomp)


  # Plot data ----
  if (chartType=="Level"){
  plot <- ggplot2::ggplot(fullplotdata[NAadd,], ggplot2::aes(x = timestamp, y = value)) +
      ggplot2::geom_line(data = fullplotdata[NAadd,],
                na.rm = TRUE,
                aes(x = timestamp, y = value),
                colour = "darkblue") +
      theme_bw() +
      theme(plot.margin = grid::unit(c(2,0.5,0.5,0.5), "cm")) +
      labs(y = "Water Level (m below top of casing)",
           title = paste0("Groundwater Hydrograph ", paste0("\n", locationData$LocationName, " ", "(",locationData$Identifier,")")),
           subtitle = paste0("\n Latitude: ",locationData$Latitude, "\n Longitude: ", locationData$Longitude, "\n Elevation: ", locationData$Elevation, " ", locationData$ElevationUnits),
           caption = c(paste0("Source Data: ", timeSeriesID, "@", AQID, "\nPlot generated: ", Sys.Date(), "\nYukon Observation Well Network"), "DISCLAIMER: Yukon Government accepts no liability for the accuracy, \n availability, suitability, reliability, usability, completeness, or timeliness of data.")) +
      theme(plot.title = element_text(hjust = 0.5, size = 14),
            plot.subtitle = element_text(hjust = 0, color = "darkgrey", size = 10),
            plot.caption.position = "plot",
            panel.border = element_rect(color = "black",
                                        fill = NULL,
                                        size = 1),
            plot.caption = element_text(hjust=c(0, 1), size = c(10, 7))) +
      theme(axis.text.x = element_text(angle = 0, hjust  = 1)) +
      scale_x_datetime(name = "",
                       date_breaks = chartXInterval,
                       date_labels = "%b %Y") +
      scale_y_reverse(name = "Water Level \n (metres below top of casing)",
                      limits = c(plyr::round_any(max(value), 0.25, f = ceiling), plyr::round_any(min(value), 0.25, f = floor)),
                      breaks = seq(ceiling(max(value)), floor(min(value)), by = -0.25))
  }




  if (chartType == "SpC") {
    plot <- ggplot2::ggplot(fullplotdata[NAadd,], aes(x = timestamp, y = value))+
      geom_line(data = fullplotdata[NAadd,],
                na.rm = TRUE,
                aes(x = timestamp, y = value),
                colour = "darkblue") +
      theme_bw() +
      theme(plot.margin = grid::unit(c(2,0.5,0.5,0.5), "cm")) +
      labs(y = "Specific Conductance (uS/cm)",
           title = paste0("Groundwater Conductivity ", paste0("\n", locationData$LocationName, " ", "(",locationData$Identifier,")")),
           subtitle = paste0("\n Latitude: ",locationData$Latitude, "\n Longitude: ", locationData$Longitude, "\n Elevation: ", locationData$Elevation, " ", locationData$ElevationUnits),
           caption = c(paste0("Source Data: ", timeSeriesID, "@", AQID, "\nPlot generated: ", Sys.Date(), "\nYukon Observation Well Network"), "DISCLAIMER: Yukon Government accepts no liability for the accuracy, \n availability, suitability, reliability, usability, completeness, or timeliness of data.")) +
      theme(plot.title = element_text(hjust = 0.5, size = 14),
            plot.subtitle = element_text(hjust = 0, color = "darkgrey", size = 10),
            plot.caption.position = "plot",
            panel.border = element_rect(color = "black",
                                        fill = NULL,
                                        size = 1),
            plot.caption = element_text(hjust=c(0, 1), size = c(10, 7))) +

      theme(axis.text.x = element_text(angle = 0, hjust  = 1)) +
      scale_x_datetime(name = "",
                       date_breaks = chartXInterval,
                       date_labels = "%b %Y") +
      scale_y_continuous(name = "Specific Conductance (uS/cm)",
                      limits = c(plyr::round_any(min(value), 0.25, f = floor), plyr::round_any(max(value), 0.25, f = ceiling)),
                      breaks = waiver())
  }

  if (chartType == "Temperature"){
    plot <- ggplot2::ggplot(fullplotdata[NAadd,], aes(x = timestamp, y = value))+
      geom_line(data = fullplotdata[NAadd,],
                na.rm = TRUE,
                aes(x = timestamp, y = value),
                colour = "darkblue") +
      theme_bw() +
      theme(plot.margin = grid::unit(c(2,0.5,0.5,0.5), "cm")) +
      labs(y = "Temperature Degrees Celcius",
           title = paste0("Groundwater Temperature ", paste0("\n", locationData$LocationName, " ", "(",locationData$Identifier,")")),
           subtitle = paste0("\n Latitude: ",locationData$Latitude, "\n Longitude: ", locationData$Longitude, "\n Elevation: ", locationData$Elevation, " ", locationData$ElevationUnits),
           caption = c(paste0("Source Data: ", timeSeriesID, "@", AQID, "\nPlot generated: ", Sys.Date(), "\nYukon Observation Well Network"), "DISCLAIMER: Yukon Government accepts no liability for the accuracy, \n availability, suitability, reliability, usability, completeness, or timeliness of data.")) +
      theme(plot.title = element_text(hjust = 0.5, size = 14),
            plot.subtitle = element_text(hjust = 0, color = "darkgrey", size = 10),
            plot.caption.position = "plot",
            panel.border = element_rect(color = "black",
                                        fill = NULL,
                                        size = 1),
            plot.caption = element_text(hjust=c(0, 1), size = c(10, 7))) +

      theme(axis.text.x = element_text(angle = 0, hjust  = 1)) +
      scale_x_datetime(name = "",
                       date_breaks = chartXInterval,
                       date_labels = "%b %Y") +
      scale_y_continuous(name = "Temperature (Degrees Celcius)",
                         limits = c(plyr::round_any(min(value), 0.25, f = floor), plyr::round_any(max(value), 0.25, f = ceiling)),
                         breaks = waiver())
  }


  # Final output and aesthetics
  finalplot <- cowplot::ggdraw(plot) +
    cowplot::draw_image("data/YGLogo.png", scale=0.16, vjust=-0.42, halign=-0.02) +
    cowplot::draw_image("data/YGWater.png", scale=0.18, vjust=-0.4, halign=1)


  #save the file
  dir.create(paste0(saveTo,"/", AQID)) #create the directory if it doesn't exist

  if (is.null(specName)==TRUE){
    ggplot2::ggsave(filename=paste0(saveTo,"/", AQID, "/", chartType, "_", AQID, "_", Sys.Date(), ".png"), plot=finalplot, height=8, width=12, units="in", device="png")
  }
  if (is.null(specName)!=TRUE){
    ggplot2::ggsave(filename=paste0(saveTo,"/", AQID, "/", chartType, "_", specName, ".png"), plot=finalplot, height=8, width=12, units="in",device="png")
  }

}
