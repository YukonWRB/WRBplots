#' YOWNplot
#'
#' Generalized YOWN plotting function
#'
#' @param AQID YOWN location for which a plot will be generated
#' @param timeSeriesID Aquarius time series ID exactly as in Aquarius (ie. "Wlevel_bgs.Calculated", "Wlevel_masl.Calculated"). Defaults to m bgs.
#' @param chartXinterval X axis interval, can be specified "auto" for best fit calculation, or as desired (ie. "1 day", "1 month", "1 year", etc.). Defaults to "auto"
#' @param dateRange X axis limits, can be "all" for all data, "1yr" for most recent year of data, or vector of 2 in format c("2020/01/01 00:00:00", "2023/01/01 00:00:00"). Defaults to "all"
#' @param stats Can be "line", "ribbon", or FALSE. Line shows years plotted in separate lines, ribbon shows max/min ribbon geom, and FALSO excludes stats.
#' @param smooth can be FALSE or a day value (ie. 14) for plotting rolling average
#' @param saveTo Directory in which the plot will be saved. Can specify "desktop" to create YOWN ID folder on desktop as save directory.
#' @param login Aquarius username and password, taken from Renviron files
#' @param AQTSServerID Aquarius server ID
#'
#' @return Writes a .pdf containing YOWN data in the specified directory.
#' @export
#'
#'
YOWNplot <- function(AQID,
                     timeSeriesID = "Wlevel_bgs.Calculated",
                     chartXinterval = "auto",
                     dateRange = "all",
                     stats = "ribbon",
                     smooth = 14,
                     saveTo = "desktop",
                     login = Sys.getenv(c("AQUSER", "AQPASS")),
                     AQTSServerID ="https://yukon.aquaticinformatics.net/AQUARIUS"){

  # Debug and development params. Leave as comments.
  # AQID = "YOWN-2201S"
  # timeSeriesID = "Wlevel_masl.Calculated"
  # chartXinterval = "auto"
  # dateRange = "all"
  # stats = "ribbon"
  # smooth = 14
  # saveTo = "desktop"
  # login = Sys.getenv(c("AQUSER", "AQPASS"))
  # AQTSServerID ="https://yukon.aquaticinformatics.net/AQUARIUS"

  #### Setup ####
  # Deal with file save location
  if(tolower(saveTo) == "desktop") {
    saveTo <- paste0("C:/Users/", Sys.getenv("USERNAME"), "/Desktop")
  }
  if(dir.exists(saveTo) == FALSE) {
    stop("Specified directory does not exist")
  }

  #### Download time series data from Aquarius, preliminary formatting ####
  # Download data from Aquarius
  print("Downloading data from Aquarius")
  datalist <- suppressMessages(WRBtools::aq_download(loc_id = AQID,
                                                     ts_name = timeSeriesID))

  # Unlist time series data
  timeseries <- datalist$timeseries

  # Replace all grades below C with "Redacted"
  timeseries$grade_description[timeseries$grade_description != "A" & timeseries$grade_description != "B" & timeseries$grade_description != "C" & timeseries$grade_description != "MISSING DATA"] <- "REDACTED"

  # Replace all values with  grade of less than C with NA, to remove values from plots. This screens out GW recovery patterns and bad data from plots and stat calculations
  timeseries$value[timeseries$grade_description != "A" & timeseries$grade_description != "B" & timeseries$grade_description != "C"] <- NA

  # Change timestamps from UTC to MST
  attr(timeseries$timestamp_UTC , "tzone") <- "MST"
  names(timeseries)[names(timeseries) == "timestamp_UTC"] <- "timestamp_MST"

  #### Advanced data processing and statistical manipulation ####
  fulldf <- timeseries

  # Identify data gaps greater than 6 hours, fill with 1hr intervals of NA
  fulldf$ts_lag <- dplyr::lag(fulldf$timestamp_MST) # Calculate lag time between each timestamp
  fulldf$lag_val <- difftime(fulldf$timestamp_MST, fulldf$ts_lag, units = "hours") # format lag as hours
  gapdf <- fulldf %>% # filter gap df to gaps of more than 6 hours
    dplyr::filter(lag_val > 6)
  gapdf$lag_val <- as.numeric(gapdf$lag_val) # convert to numeric

  # Check if gapdf has any entries, if so fill gaps with hourly timestamps and NA in teh value column
  if(nrow(gapdf != 0)){
    # Create a list of data frames for each identified data gap, fill in daily time stamps with NA in "value" column
    gaplist <- list()
    for(i in 1:nrow(gapdf)) {
      df <- data.frame(seq.POSIXt(from = as.POSIXct(gapdf[i, 1]), by = "-1 hour", length.out = gapdf[i, "lag_val"]), NA, as.character(-5), "MISSING DATA", gapdf$approval_level[i], gapdf$approval_description[i], NA, NA)
      colnames(df) <- colnames(gapdf)
      gaplist[[i]] <- df
    }
    # Merge all listed gap data frames, combine with original timeseries, order and format
    gapmerge <- do.call(rbind, gaplist)
    gapmerge <- gapmerge[order(gapmerge$timestamp_MST),] # Order by timestamp
    fulldf <- suppressMessages(dplyr::full_join(fulldf, gapmerge))
  }

  # Advanced data processing
  fulldf <- fulldf %>%
    dplyr::mutate(date = format(fulldf$timestamp_MST, "%Y-%m-%d"), # Add date column
                  year = format(fulldf$timestamp_MST, "%Y"), # Add year column
                  month = format(fulldf$timestamp_MST, "%m"), # Add month column
                  day = format(fulldf$timestamp_MST, "%d"), # Add day column
                  monthday = format(fulldf$timestamp_MST, "%m-%d")) # Add month-day column %>%
  datestats <- suppressWarnings(dplyr::group_by(fulldf, date) %>% # Calculate statistics by date (ie. Jan. 1, 2000)
                                  dplyr::summarize(datemin = min(value, na.rm = TRUE), datemax = max(value, na.rm = TRUE), datemean = mean(value, na.rm = TRUE)))
  fulldf <- suppressMessages(dplyr::full_join(fulldf, datestats)) # Join full df to datestats
  daystats <- suppressWarnings(dplyr::group_by(fulldf, monthday) %>% # Calculate year-over-year daily statistics (ie. Jan. 1)
                                 dplyr::summarize(daymin = min(datemin, na.rm = TRUE), daymax = max(datemax, na.rm = TRUE), daymean = mean(datemean, na.rm = TRUE), N = dplyr::n()))
  dayavg <- na.omit(daystats)
  fulldf <- suppressMessages(dplyr::full_join(fulldf, daystats)) # Join fulldf to daystats

  # Final fulldf formatting
  fulldf <- fulldf[match(unique(fulldf$date), fulldf$date),] # Extract first occurance of each date, to trim dataset to one entry per day
  is.na(fulldf) <- sapply(fulldf, is.infinite) # Replace infinite values with NA
  is.na(fulldf) <- sapply(fulldf, is.nan) # Repace NaN values with NA
  fulldf <- fulldf[order(fulldf$timestamp_MST),] # Order by timestamp
  fulldf <- fulldf[!duplicated(fulldf["timestamp_MST"]),] #Remove second entry for duplicated timestamps
  rownames(fulldf) <- NULL

  #### Format x axis limits, add colour column, check for sufficient data for stat calculation ####
  # Format and calculate x axis limits
  if(paste(tolower(dateRange), collapse = ",") == "all"){
    dateRange <- c(min(na.omit(fulldf$timestamp_MST)), max(na.omit(fulldf$timestamp_MST)))
  } else if(paste(tolower(dateRange), collapse = ",") == "1yr"){
    dateRange <- c((max(na.omit(fulldf$timestamp_MST)) - lubridate::years(1)), max(na.omit(fulldf$timestamp_MST)))
  } else if(length(dateRange) != 2){
    print("Chart X limits in incorrect format")
  } else {
    dateRange <- format(as.POSIXct(x = dateRange, tz = "MST"), format = "%Y/%m/%d %H:%M:%S")
  }

  #### Create extra chart blocks, for final customization in if statements below
  title <- ggplot2::ggplot() +
    ggplot2::geom_blank() +
    ggplot2::theme_minimal() +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0,
                                                      vjust = 0,
                                                      size = 14,
                                                      colour = "#244C5A",
                                                      face = "bold"),
                   plot.margin = ggplot2::unit(c(6.3, 0, 0, 0.51), "cm"))

  subtitle <- ggplot2::ggplot() +
    ggplot2::geom_blank() +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = paste0("Source Data: ", AQID, "@", timeSeriesID,  "\nLatitude: ", datalist[["metadata"]][5, 2], ", ", "Longitude: ", datalist[["metadata"]][6, 2],", ", "Elevation: ", datalist[["metadata"]][7, 2], " ", datalist[["metadata"]][8, 2])) +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0,
                                                      vjust = 0,
                                                      size = 10,
                                                      color = "#464646"),
                   plot.margin = ggplot2::unit(c(6.85, 0, 0, 0.6), "cm"))

  caption <- ggplot2::ggplot() +
    ggplot2::geom_blank() +
    ggplot2::theme_minimal() +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0,
                                                      vjust = 0,
                                                      size = 9,
                                                      colour = "#464646"),
                   plot.margin = ggplot2::unit(c(-2.39, 0, 0, 0.6), "cm"))

  # Trim data to specified limits
  plotdf <- subset(fulldf, fulldf$timestamp_MST >= (min(dateRange)) & fulldf$timestamp_MST <= (max(dateRange)))
  plotdf$monthday <- as.POSIXct(plotdf$monthday, format = "%m-%d")
  plotdf$year <- as.Date(plotdf$year, format = "%Y")

  # Apply smoothing function to "value" column for plotting
  if(is.numeric(smooth)){
    plotdf <- plotdf %>%
      dplyr::mutate(value = zoo::rollmean(x = plotdf$datemean, k = smooth, fill = "extend")) %>%
      dplyr::mutate(daymin = zoo::rollmean(x = plotdf$daymin, k = smooth, fill = "extend")) %>%
      dplyr::mutate(daymax = zoo::rollmean(x = plotdf$daymax, k = smooth, fill = "extend"))
  } else if(smooth == TRUE){
    print("ERROR: Specify smoothing value as a number")
  } else if(smooth == FALSE){
    smooth <- 0
  }

  # Check for sufficient data for ribbon stat plot generation
  if(tolower(stats) == "ribbon" & (max(fulldf$timestamp_MST, na.rm = TRUE) - min(fulldf$timestamp_MST, na.rm = TRUE) < 730)){
    print("Insufficient data for ribbon stats calculation, plot produced with no stats instead")
    stats <- FALSE
  }

  #### Generate stat plot ####
  if(tolower(stats) == "ribbon"){

    # Assign year as factor variable
    plotdf$year <- factor(plotdf$year)

    # Calculate chart X interval if "auto" specified
    if(chartXinterval == "auto"){
      diff <- as.numeric(difftime(max(plotdf$timestamp), min(plotdf$timestamp), units = "days"))
      chartXinterval <- dplyr::case_when(
        diff < 180 ~ "1 week",
        diff >= 180 & diff < 730 ~ "1 month",
        diff >= 730 & diff < 1460 ~ "2 months",
        diff >= 1460 & diff < 2920 ~ "6 months",
        diff >= 2920 ~ "1 year")
    }

    # Create  base plot, add aesthetic tweaks
    plot <- ggplot2::ggplot() +
      ggplot2::geom_ribbon(data = plotdf,
                           ggplot2::aes(ymin = daymin, ymax = daymax, x = timestamp_MST, fill = "Range of Historical Max & Min Daily Groundwater Levels")) +
      ggplot2::scale_fill_manual(name = "", values = c("Range of Historical Max & Min Daily Groundwater Levels" = "#B8BDC3")) +
      ggplot2::geom_line(data = plotdf,
                         ggplot2::aes(x = timestamp_MST, y = daymean, colour = "Historical Mean Daily Groundwater Level"),
                         linewidth = 0.3,
                         na.rm = TRUE) +
      ggplot2::scale_colour_manual(name = "", values = c("Historical Mean Daily Groundwater Level" = "#0097A9")) +
      ggnewscale::new_scale_colour() +
      ggplot2::geom_line(data = plotdf,
                         ggplot2::aes(x = timestamp_MST, y = value, colour = "Daily Average Groundwater Level"),
                         linewidth = 0.5,
                         na.rm = TRUE) +
      ggplot2::scale_colour_manual(name = "", values = c("Daily Average Groundwater Level" = "#244C5A")) +
      ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(size = 1.5))) +
      cowplot::theme_cowplot() +
      ggplot2::theme(plot.margin = ggplot2::unit(c(4.2, 1.6, 3.1, 1.2), "cm"),
                     panel.border = ggplot2::element_rect(color = "grey",
                                                          fill = NULL,
                                                          linewidth = 0.5),
                     axis.text.x = ggplot2::element_text(angle = 45,
                                                         hjust  = 1,
                                                         vjust = 1,
                                                         size = 10),
                     axis.line.x.bottom = ggplot2::element_blank(),
                     axis.text.y = ggplot2::element_text(hjust = 1,
                                                         size = 10),
                     axis.title.y = ggplot2::element_text(vjust = 2,
                                                          size = 12,
                                                          colour = "#464646"),
                     axis.line.y.left = ggplot2::element_blank(),
                     panel.grid.major = ggplot2::element_line(colour = "lightgrey", linewidth = 0.5, linetype = 1),
                     legend.position = "bottom",
                     legend.justification = "left",
                     legend.margin = ggplot2::margin(0,0,0,0),
                     legend.box.margin = ggplot2::margin(-18, 0, 0, -10),
                     legend.text = ggplot2::element_text(size = 9)) +
      ggplot2::scale_x_datetime(name = "",
                                limits = as.POSIXct(dateRange),
                                date_breaks = chartXinterval,
                                date_labels = "%b. %d, %Y",
                                expand = c(0, 0))
    # Customize title block
    title <- title + ggplot2::labs(title = paste0("Groundwater Level Statistics Chart: ",
                                                  datalist[["metadata"]][1, 2], " ",
                                                  "(", datalist[["metadata"]][3, 2], ")"))

    # Customize caption block
    caption <- caption + ggplot2::labs(title = paste0("Max & Min data calculated from period of record from ",
                                                      strftime(as.POSIXct(min(na.omit(timeseries$timestamp_MST))), format = "%Y-%m-%d"),
                                                      " to ",
                                                      strftime(as.POSIXct(max(na.omit(timeseries$timestamp_MST))), format = "%Y-%m-%d"),
                                                      " (Date of last data entry)\n",
                                                      paste0("Smoothing applied to data: ", smooth, " day rolling mean"),
                                                      "\nPlot generated: ",
                                                      Sys.Date(),
                                                      "; Yukon Observation Well Network"))

  } else if(tolower(stats) == "line"){

    # Separate current year from historical
    plotdf_hist <- plotdf %>%
      dplyr::filter(year != max(plotdf$year))
    plotdf_current <- plotdf %>%
      dplyr::filter(year == max(plotdf$year))

    # Calculate chart X interval if "auto" specified
    if(chartXinterval == "auto"){
      diff <- as.numeric(difftime(max(plotdf$timestamp), min(plotdf$timestamp), units = "days"))
      chartXinterval <- dplyr::case_when(
        diff < 180 ~ "1 week",
        diff >= 180 & diff < 730 ~ "1 month",
        diff >= 730 & diff < 1460 ~ "2 months",
        diff >= 1460 & diff < 2920 ~ "6 months",
        diff >= 2920 ~ "1 year")
    }

    # Create plot, add aesthetic tweaks
    plot <- ggplot2::ggplot() +
      ggplot2::geom_line(data = plotdf_hist,
                         ggplot2::aes(x = monthday, y = value, group = year, colour = year),
                         linewidth = 0.2) +
      ggplot2::scale_colour_gradient(trans = "date", low = "#7A9A01", high = "#DC4405", name = "", labels = scales::label_date(format = "%Y"), limits = c(min(plotdf_hist$year), max(plotdf_hist$year))) +
      ggnewscale::new_scale_color() +
      ggplot2::geom_line(data = plotdf_current,
                         ggplot2::aes(x = monthday, group = 1, y = value, colour = "Water Level (title year)"),
                         linewidth = 1) +
      ggplot2::scale_colour_manual(name = "", values = c("Water Level (title year)" = "#244C5A")) +
      ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(linewidth = 1.5))) +
      cowplot::theme_cowplot() +
      ggplot2::theme(plot.margin = ggplot2::unit(c(4.2, 1.6, 3.1, 1.2), "cm"),
                     panel.border = ggplot2::element_rect(color = "grey",
                                                          fill = NULL,
                                                          linewidth = 0.5),
                     axis.text.x = ggplot2::element_text(angle = 0,
                                                         hjust  = 0.5,
                                                         vjust = 1,
                                                         size = 10),
                     axis.line.x.bottom = ggplot2::element_blank(),
                     axis.text.y = ggplot2::element_text(hjust = 1,
                                                         size = 10),
                     axis.title.y = ggplot2::element_text(vjust = 2,
                                                          size = 12,
                                                          colour = "#464646"),
                     axis.line.y.left = ggplot2::element_blank(),
                     panel.grid.major = ggplot2::element_line(colour = "lightgrey", linewidth = 0.5, linetype = 1),
                     legend.position = "bottom",
                     legend.justification = "left",
                     legend.margin = ggplot2::margin(0,0,0,0),
                     legend.box.margin = ggplot2::margin(-18, 0, 0, -10),
                     legend.text = ggplot2::element_text(size = 9),
                     legend.key.width = ggplot2::unit(1, "cm")) +
      ggplot2::scale_x_datetime(name = "",
                                date_breaks = "1 month",
                                date_labels = "%b",
                                labels = plotdf$year,
                                expand = c(0, 0))
    # Create title block
    title <- title + ggplot2::labs(title = paste0("Groundwater Level Chart: ", datalist[["metadata"]][1, 2], " ", "(",datalist[["metadata"]][3, 2], ")", " in ", max(format(plotdf$year, "%Y"))))

    # Create caption block
    caption <- caption + ggplot2::labs(title = paste0("Period of record from ",
                                                      strftime(as.POSIXct(min(na.omit(timeseries$timestamp_MST))), format = "%Y-%m-%d"),
                                                      " to ",
                                                      strftime(as.POSIXct(max(na.omit(timeseries$timestamp_MST))), format = "%Y-%m-%d"),
                                                      " (Date of last data entry)",
                                                      "\nPlot generated: ",
                                                      Sys.Date(),
                                                      "\nYukon Observation Well Network"))
  } else if(stats == FALSE){
    plot <- ggplot2::ggplot() +
      ggplot2::geom_line(data = plotdf,
                         ggplot2::aes(x = timestamp_MST, y = value, colour = "Water Level"),
                         linewidth = 1) +
      ggplot2::scale_colour_manual(name = "", values = c("Water Level" = "#244C5A")) +
      ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(linewidth = 1.5))) +
      cowplot::theme_cowplot() +
      ggplot2::theme(plot.margin = ggplot2::unit(c(4.2, 1.6, 3.1, 1.2), "cm"),
                     panel.border = ggplot2::element_rect(color = "grey",
                                                          fill = NULL,
                                                          linewidth = 0.5),
                     axis.text.x = ggplot2::element_text(angle = 0,
                                                         hjust  = 1,
                                                         vjust = 1,
                                                         size = 10),
                     axis.line.x.bottom = ggplot2::element_blank(),
                     axis.text.y = ggplot2::element_text(hjust = 1,
                                                         size = 10),
                     axis.title.y = ggplot2::element_text(vjust = 2,
                                                          size = 12,
                                                          colour = "#464646"),
                     axis.line.y.left = ggplot2::element_blank(),
                     panel.grid.major = ggplot2::element_line(colour = "lightgrey", linewidth = 0.5, linetype = 1),
                     legend.position = "bottom",
                     legend.justification = "left",
                     legend.margin = ggplot2::margin(0,0,0,0),
                     legend.box.margin = ggplot2::margin(-18, 0, 0, -10),
                     legend.text = ggplot2::element_text(size = 9),
                     legend.key.width = ggplot2::unit(1, "cm")) +
      ggplot2::scale_x_datetime(name = "",
                                date_breaks = "1 month",
                                date_labels = "%b",
                                expand = c(0, 0))
    # Create title block
    title <- title + ggplot2::labs(title = paste0("Groundwater Level Chart: ", datalist[["metadata"]][1, 2], " ", "(",datalist[["metadata"]][3, 2], ")"))

    # Create caption block
    caption <- caption + ggplot2::labs(title = paste0("Period of record from ",
                                                      strftime(as.POSIXct(min(na.omit(timeseries$timestamp_MST))), format = "%Y-%m-%d"),
                                                      " to ",
                                                      strftime(as.POSIXct(max(na.omit(timeseries$timestamp_MST))), format = "%Y-%m-%d"),
                                                      " (Date of last data entry)",
                                                      "\nPlot generated: ",
                                                      Sys.Date(),
                                                      "\nYukon Observation Well Network"))
  }

  #### Final formatting of X and Y axes and adding grade colour lines ####
  # Set y axis title and name
  if(timeSeriesID == "Wlevel_btoc.Calculated"){
    ytitle <- "Water Level (m below top of casing)"
    name <- "mbtoc"
  } else if(timeSeriesID == "Wlevel_bgs.Calculated"){
    ytitle <- "Water Level (m below ground surface)"
    name <- "mbgs"
  } else if(timeSeriesID == "Wlevel_masl.Calculated"){
    ytitle <- "Water Level (m above sea level)"
    name <- "masl"
  }
  # If time series is bgs or btoc, check for stats and place grade colour line accordingly
  if(timeSeriesID == "Wlevel_bgs.Calculated" | timeSeriesID == "Wlevel_btoc.Calculated"){
    if(stats != FALSE){
      plot <- plot + ggplot2::scale_y_reverse(name = ytitle,
                                              limits = c(plyr::round_any(max(na.omit(plotdf$daymax)), 0.5, f = ceiling), plyr::round_any(min(na.omit(plotdf$daymin)), 0.25, f = floor)),
                                              breaks = seq(plyr::round_any(max(na.omit(plotdf$daymax)), 0.5, f = ceiling), plyr::round_any(min(na.omit(plotdf$daymin)), 0.25, f = floor), by = -0.25),
                                              expand = c(0, 0))

      if(tolower(stats) == "ribbon"){
        plot <- plot +
          ggnewscale::new_scale_colour() +
          ggplot2::geom_path(data = plotdf, ggplot2::aes(x = timestamp_MST, y = plyr::round_any(max(na.omit(daymax)), 0.5, f = ceiling), colour = factor(grade_description), group = 1), linewidth = 2.5, show.legend = FALSE) +
          ggplot2::scale_colour_manual(name = "Grades", values = c("A" = "#7A9A01", "B" = "#0097A9", "C" = "#F2A900", "REDACTED" = "#DC4405", "MISSING DATA" = "black"))

      } else if(tolower(stats) == "line"){
        plot <- plot +
          ggnewscale::new_scale_colour() +
          ggplot2::geom_path(data = plotdf_current, ggplot2::aes(x = monthday, y = plyr::round_any(max(na.omit(plotdf$datemax)), 0.5, f = ceiling), colour = factor(grade_description), group = 1), linewidth = 2.5, show.legend = FALSE) +
          ggplot2::scale_colour_manual(name = "Grades", values = c("A" = "#7A9A01", "B" = "#0097A9", "C" = "#F2A900", "REDACTED" = "#DC4405", "MISSING DATA" = "black"))
      }
    } else if(stats == FALSE){
      plot <- plot +
        ggnewscale::new_scale_colour() +
        ggplot2::geom_path(data = plotdf, ggplot2::aes(x = timestamp_MST, y = plyr::round_any(max(na.omit(value)), 0.5, f = ceiling), colour = factor(grade_description), group = 1), linewidth = 2.5, show.legend = FALSE) +
        ggplot2::scale_colour_manual(name = "Grades", values = c("A" = "#7A9A01", "B" = "#0097A9", "C" = "#F2A900", "REDACTED" = "#DC4405", "MISSING DATA" = "black")) +
        ggplot2::scale_y_reverse(name = ytitle,
                                 limits = c(plyr::round_any(max(na.omit(plotdf$value)), 0.5, f = ceiling), plyr::round_any(min(na.omit(plotdf$value)), 0.25, f = floor)),
                                 breaks = seq(plyr::round_any(max(na.omit(plotdf$value)), 0.5, f = ceiling), plyr::round_any(min(na.omit(plotdf$value)), 0.25, f = floor), by = -0.25),
                                 expand = c(0, 0))
    }

  } else if(timeSeriesID == "Wlevel_masl.Calculated") {
    # If time series is masl, check for stats and place grade colour line accordingly
    if(stats != FALSE){
      plot <- plot +
        ggnewscale::new_scale_colour() +
        ggplot2::geom_path(data = plotdf, ggplot2::aes(x = timestamp_MST, y = plyr::round_any(min(na.omit(min)), 0.25, f = floor), colour = factor(grade_description), group = 1), linewidth = 2.5, show.legend = FALSE) +
        ggplot2::scale_colour_manual(name = "Grades", values = c("A" = "#7A9A01", "B" = "#0097A9", "C" = "#F2A900", "REDACTED" = "#DC4405", "MISSING DATA" = "black")) +
        ggplot2::scale_y_continuous(name = ytitle,
                                    limits = c(plyr::round_any(min(na.omit(plotdf$min)), 0.25, f = floor), plyr::round_any(max(na.omit(plotdf$max)), 0.5, f = ceiling)),
                                    breaks = seq(floor(min(na.omit(plotdf$min))), ceiling(max(na.omit(plotdf$max))), by = 0.25),
                                    expand = c(0, 0))
    } else if(stats == FALSE){
      plot <- plot +
        ggnewscale::new_scale_colour() +
        ggplot2::geom_path(data = plotdf, ggplot2::aes(x = timestamp_MST, y = plyr::round_any(min(na.omit(value)), 0.25, f = floor), colour = factor(grade_description), group = 1), linewidth = 2.5, show.legend = FALSE) +
        ggplot2::scale_colour_manual(name = "Grades", values = c("A" = "#7A9A01", "B" = "#0097A9", "C" = "#F2A900", "REDACTED" = "#DC4405", "MISSING DATA" = "black")) +
        ggplot2::scale_y_continuous(name = ytitle,
                                    limits = c(plyr::round_any(min(na.omit(plotdf$value)), 0.25, f = floor), plyr::round_any(max(na.omit(plotdf$value)), 0.5, f = ceiling)),
                                    breaks = seq(floor(min(na.omit(plotdf$value))), ceiling(max(na.omit(plotdf$value))), by = 0.25),
                                    expand = c(0, 0))
    }
  }

  #### Final combination of plot, title, subtitle, caption blocks, format and save plot ####

  if(stats == FALSE){
    title <- title + ggplot2::labs(title = paste0("Groundwater Level Chart: ", datalist[["metadata"]][1, 2], " ", "(",datalist[["metadata"]][3, 2], ", ", max(plotdf$year), ")"))
  }

  # Use plot_grid method to combine titles, captions, and main plot in proper orientation
  final <- cowplot::plot_grid(title, subtitle, plot, caption, ncol = 1, nrow = 4, rel_heights = c(0.1, 0.1, 2, 0.1))

  # Draw arranged plots on background template file
  final_plot <- cowplot::ggdraw() +
    cowplot::draw_image("G:/water/Groundwater/2_YUKON_OBSERVATION_WELL_NETWORK/4_YOWN_DATA_ANALYSIS/1_WATER LEVEL/00_AUTOMATED_REPORTING/01_MARKUP_IMAGES/Template_grades.jpg") +
    cowplot::draw_plot(final)

  # Create save folder in specified directory
  dir.create(paste0(saveTo, "/", AQID), showWarnings = FALSE)

  # Final plot saving
  ggplot2::ggsave(plot = final_plot, filename = paste0(saveTo, "/", AQID, "/", AQID, "_", name, "_", "stats", stats, "_smooth", smooth, ".pdf"),  height = 8.5, width = 11, units = "in")

  print(paste0("Plot written to ", saveTo, "/", AQID))
}

