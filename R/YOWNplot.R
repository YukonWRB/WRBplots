#' YOWNplot
#'
#' Generalized YOWN plotting function
#'
#' @param AQID YOWN location for which a plot will be generated
#' @param timeSeriesID Aquarius time series ID exactly as in Aquarius (ie. "Wlevel_bgs.Calculated"). Defaults to m bgs.
#' @param chartXinterval X axis interval, can be specified "auto" for best fit calculation, or as desired (ie. "1 day", "1 month", "1 year", etc.). Defaults to "auto"
#' @param chartXlimits X axis limits, can be "all" for all data, "1yr" for most recent year of data, or vector of 2 in format c("2020/01/01 00:00:00", "2023/01/01 00:00:00"). Defaults to "all"
#' @param stats TRUE/FALSE Add historical max/min and average to plot, where sufficient data exists.
#' @param smooth TRUE/FALSE Plot rolling 5 day average instead of individual values.
#' @param saveTo Directory in which the plot will be saved. Can specify "desktop" to create YOWN ID folder on desktop as save directory.
#' @param login Aquarius username and password, taken from Renviron file
#' @param AQTSServerID Aquarius server ID
#'
#' @return Writes a .pdf containing YOWN data in the specified directory.
#' @export
#'
#'
YOWNplot <- function(AQID,
                     timeSeriesID = "Wlevel_bgs.Calculated",
                     chartXinterval = "auto",
                     chartXlimits = "all",
                     stats = TRUE,
                     smooth = TRUE, # Applies 5-day moving average smoothing function to data before plotting
                     saveTo = "desktop",
                     login = Sys.getenv(c("AQUSER", "AQPASS")), # Pull Aquarius login information from Renviron file
                     AQTSServerID ="https://yukon.aquaticinformatics.net/AQUARIUS"){

  # Debug and development params. Leave as comments.
  # AQID = "YOWN-0804" # Single YOWN site for plotting
  # timeSeriesID = "Wlevel_bgs.Calculated"
  # chartXinterval = "auto" # specify "auto" for best fit calculation, or can be specified as desired (ie. "1 day", "1 month", "1 year", etc.)
  # chartXlimits = "all" # can be "all", "1yr" for most recent year of data, or vector of 2 in format c("2020/01/01 00:00:00", "2023/01/01 00:00:00")
  # stats = TRUE
  # smooth = TRUE # Applies 5-day moving average smoothing function to data before plotting
  # saveTo = "desktop"
  # login = Sys.getenv(c("AQUSER", "AQPASS")) # Pull Aquarius login information from Renviron file
  # AQTSServerID ="https://yukon.aquaticinformatics.net/AQUARIUS"

  #### Preliminary stuff ####
  # Deal with file save location
  if(tolower(saveTo) == "desktop") {
    saveTo <- paste0("C:/Users/", Sys.getenv("USERNAME"), "/Desktop")
  }
  if(dir.exists(saveTo) == FALSE) {
    stop("Specified directory does not exist")
  }

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

  #### Download data from Aquarius, format and fill gaps ####
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

  # Find data gaps of greater than 6 hours (indicative of logger failure) and generate NA data sets to fill in gaps
  timeseries$ts_lag <- dplyr::lag(timeseries$timestamp_MST) # Calculate lag time between each timestamp
  timeseries$lag_val <- difftime(timeseries$timestamp_MST, timeseries$ts_lag, units = "hours") # format lag as hours
  gapdf <- timeseries %>% # filter gap df to gaps more than 6 hours
    dplyr::filter(lag_val > 6)
  gapdf$lag_val <- as.numeric(gapdf$lag_val) # convert to numeric

  # Check if gapdf has any entries, skip if not
  if(nrow(gapdf != 0)){
    # Create a list of data frames for each identified data gap, fill in hourly time stamps with NA in "value" column
    gaplist <- list()
    for(i in 1:nrow(gapdf)) {
      df <- data.frame(seq.POSIXt(from = gapdf[i, 1], by = "-1 hour", length.out = gapdf[i, 8]), NA, as.character(-5), "MISSING DATA", gapdf$approval_level[i], gapdf$approval_description[i], NA, NA)
      colnames(df) <- colnames(gapdf)
      gaplist[[i]] <- df
    }

    # Merge all listed gap data frames, combine with original timeseries, order and format
    gapmerge <- do.call(rbind, gaplist)
    fulldf <- suppressMessages(dplyr::full_join(timeseries, gapmerge))
    fulldf <- fulldf[order(fulldf$timestamp_MST),] # Order by timestamp
    fulldf <- fulldf[!duplicated(fulldf["timestamp_MST"]),] #Remove second entry for duplicated timestamps
  } else {
    fulldf <- timeseries
  }

  #### Calculate x axis limits, add colour column, check for sufficient data for stat calculation ####
  # Format and calculate x axis limits
  if(paste(tolower(chartXlimits), collapse = ",") == "all"){
    chartXlimits <- c(min(na.omit(fulldf$timestamp_MST)), max(na.omit(fulldf$timestamp_MST)))
  } else if(paste(tolower(chartXlimits), collapse = ",") == "1yr"){
    chartXlimits <- c((max(na.omit(fulldf$timestamp_MST)) - lubridate::years(1)), max(na.omit(fulldf$timestamp_MST)))
  } else if(length(chartXlimits) != 2){
    print("Chart X limits in incorrect format")
  } else {
    chartXlimits <- format(as.POSIXct(x = chartXlimits, tz = "MST"), format = "%Y/%m/%d %H:%M:%S")
  }

  # Add colour column to plot df, to facilitate grade colour coding later
  fulldf <- fulldf %>%
    dplyr::mutate(col = dplyr::case_when(
      grade_description == "A" ~ "#7A9A01",
      grade_description == "B" ~ "#0097A9",
      grade_description == "C" ~ "#F2A900",
      grade_description == "REDACTED" ~ "#DC4405",
      grade_description == "MISSING DATA" ~ "black"))

  # Print message if insufficient data for statistical calculations (less than 2 years), set "stats" to FALSE
  if((max(timeseries$timestamp_MST, na.rm = TRUE) - min(timeseries$timestamp_MST, na.rm = TRUE) < 730)){
    print("Insufficient data for stat calculations")
    stats <- FALSE
  }

  #### Calculate stats if selected and sufficient data available ####
  if(stats == TRUE){
    # Group by day, calculate daily max and min values
    fulldf$day <- format(fulldf$timestamp_MST, "%m-%d")
    dayrange <- dplyr::group_by(fulldf, day) %>%
      dplyr::summarize(min = min(value, na.rm = TRUE), max = max(value, na.rm = TRUE), mean = mean(value, na.rm = TRUE), N = dplyr::n())
    dayrange <- na.omit(dayrange)

    # Join statistics to plot data
    fulldf <-suppressMessages(dplyr::full_join(fulldf, dayrange))
    print("Statistics calculated")

    # Identify data gaps of more than 6 hours, indicative of logger failure, creating a vector of TRUE/FALSE that removes plot line between gaps
    NAcomp <- rle(!is.na(fulldf$value))
    NAcomp$values[which(NAcomp$lengths>6 & !NAcomp$values)] <- TRUE
    NAadd <- inverse.rle(NAcomp)

    # Trim data to chart x limits
    plotdf <- subset(fulldf, fulldf$timestamp_MST >= (min(chartXlimits)) & fulldf$timestamp_MST <= (max(chartXlimits)))

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

    # Create plots, add aesthetic tweaks
    plot <- ggplot2::ggplot() +
      ggplot2::geom_ribbon(data = plotdf,
                           ggplot2::aes(ymin = min, ymax = max, x = timestamp_MST, fill = "Range of Historical Max & Min Daily Groundwater Levels")) +
      ggplot2::scale_fill_manual(name = "", values = c("Range of Historical Max & Min Daily Groundwater Levels" = "#B8BDC3")) +
      ggplot2::geom_line(data = plotdf[NAadd,],
                         ggplot2::aes(x = timestamp_MST, y = mean, colour = "Historical Mean Daily Groundwater Level"),
                         na.rm = TRUE) +
      ggplot2::geom_line(data = plotdf[NAadd,],
                         ggplot2::aes(x = timestamp_MST, y = value, colour = "Water Level"),
                         na.rm = TRUE) +
      ggplot2::scale_colour_manual(name = "", values = c("Water Level" = "#244C5A", "Historical Mean Daily Groundwater Level" = "#907585")) +
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
                                limits = as.POSIXct(chartXlimits),
                                date_breaks = chartXinterval,
                                date_labels = "%b. %d, %Y",
                                expand = c(0, 0))
    # Create title block
    title <- ggplot2::ggplot() +
      ggplot2::geom_blank() +
      ggplot2::theme_minimal() +
      ggplot2::labs(title = paste0("Groundwater Level Statistics Chart: ", datalist[["metadata"]][1, 2], " ", "(",datalist[["metadata"]][3, 2],")")) +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0,
                                                        vjust = 0,
                                                        size = 14,
                                                        colour = "#244C5A",
                                                        face = "bold"),
                     plot.margin = ggplot2::unit(c(6.3, 0, 0, 0.51), "cm"))

    # Create caption block
    caption <- ggplot2::ggplot() +
      ggplot2::geom_blank() +
      ggplot2::theme_minimal() +
      ggplot2::labs(title = paste0("Max & Min data calculated from period of record from ", strftime(as.POSIXct(min(na.omit(timeseries$timestamp_MST))), format = "%Y-%m-%d"), " to ", strftime(as.POSIXct(max(na.omit(timeseries$timestamp_MST))), format = "%Y-%m-%d"), " (Date of last site visit)",  "\nPlot generated: ", Sys.Date(), "\nYukon Observation Well Network")) +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0,
                                                        vjust = 0,
                                                        size = 9,
                                                        colour = "#464646"),
                     plot.margin = ggplot2::unit(c(-2.39, 0, 0, 0.6), "cm"))

  } else if (stats == FALSE){
    # Trim data to chart x limits
    plotdf <- subset(fulldf, fulldf$timestamp_MST >= (min(chartXlimits)) & fulldf$timestamp_MST <= (max(chartXlimits)))

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

    #Identify data gaps of more than 6 hours, indicative of logger failure
    NAcomp <- rle(!is.na(plotdf$value))
    NAcomp$values[which(NAcomp$lengths>6 & !NAcomp$values)] <- TRUE
    NAadd <- inverse.rle(NAcomp)

    # Create plots, add aesthetic tweaks
    plot <- ggplot2::ggplot() +
      ggplot2::geom_line(data = plotdf[NAadd,],
                         ggplot2::aes(x = timestamp_MST, y = value, colour = "Water Level"),
                         na.rm = TRUE) +
      ggplot2::scale_colour_manual(name = "", values = c("Water Level" = "#244C5A")) +
      ggplot2::guides(color = ggplot2::guide_legend(override.aes = list(linewidth = 1.5))) +
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
                                limits = as.POSIXct(chartXlimits),
                                date_breaks = chartXinterval,
                                date_labels = "%b. %d, %Y",
                                expand = c(0, 0))
    # Create title block
    title <- ggplot2::ggplot() +
      ggplot2::geom_blank() +
      ggplot2::theme_minimal() +
      ggplot2::labs(title = paste0("Groundwater Level Chart: ", datalist[["metadata"]][1, 2], " ", "(",datalist[["metadata"]][3, 2],")")) +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0,
                                                        vjust = 0,
                                                        size = 14,
                                                        colour = "#244C5A",
                                                        face = "bold"),
                     plot.margin = ggplot2::unit(c(6.3, 0, 0, 0.51), "cm"))

    # Create caption block
    caption <- ggplot2::ggplot() +
      ggplot2::geom_blank() +
      ggplot2::theme_minimal() +
      ggplot2::labs(title = paste0("Period of record from ", strftime(as.POSIXct(min(na.omit(timeseries$timestamp_MST))), format = "%Y-%m-%d"), " to ", strftime(as.POSIXct(max(na.omit(timeseries$timestamp_MST))), format = "%Y-%m-%d"), " (Date of last site visit)",  "\nPlot generated: ", Sys.Date(), "\nYukon Observation Well Network")) +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0,
                                                        vjust = 0,
                                                        size = 9,
                                                        colour = "#464646"),
                     plot.margin = ggplot2::unit(c(-2.39, 0, 0, 0.6), "cm"))
  }

  #### Add grade colour lines and specify y axis directionality based on timeseries ID ####
  # If time series is bgs or btoc, check for stats and place grade colour line accordingly
  if(timeSeriesID == "Wlevel_bgs.Calculated" | timeSeriesID == "Wlevel_btoc.Calculated"){
    if(stats == TRUE){
      plot <- plot +
        ggnewscale::new_scale_colour() +
        ggplot2::geom_path(data = plotdf, ggplot2::aes(x = timestamp_MST, y = plyr::round_any(max(na.omit(max)), 0.5, f = ceiling), colour = factor(grade_description), group = 1), linewidth = 2.5, show.legend = FALSE) +
        ggplot2::scale_colour_manual(name = "Grades", values = c("A" = "#7A9A01", "B" = "#0097A9", "C" = "#F2A900", "REDACTED" = "#DC4405", "MISSING DATA" = "black")) +
        ggplot2::scale_y_reverse(name = ytitle,
                                 limits = c(plyr::round_any(max(na.omit(plotdf$max)), 0.5, f = ceiling), plyr::round_any(min(na.omit(plotdf$min)), 0.25, f = floor)),
                                 breaks = seq(plyr::round_any(max(na.omit(plotdf$max)), 0.5, f = ceiling), plyr::round_any(min(na.omit(plotdf$min)), 0.25, f = floor), by = -0.25),
                                 expand = c(0, 0))
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
    if(stats == TRUE){
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
  subtitle <- ggplot2::ggplot() +
    ggplot2::geom_blank() +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = paste0("Source Data: ", AQID, "@", timeSeriesID,  "\nLatitude: ", datalist[["metadata"]][5, 2], ", ", "Longitude: ", datalist[["metadata"]][6, 2],", ", "Elevation: ", datalist[["metadata"]][7, 2], " ", datalist[["metadata"]][8, 2])) +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0,
                                                      vjust = 0,
                                                      size = 10,
                                                      color = "#464646"),
                   plot.margin = ggplot2::unit(c(6.85, 0, 0, 0.6), "cm"))

  # Use plot_grid method to combine titles, captions, and main plot in proper orientation
  final <- cowplot::plot_grid(title, subtitle, plot, caption, ncol = 1, nrow = 4, rel_heights = c(0.1, 0.1, 2, 0.1))

  # Draw arranged plots on background template file
  final_plot <- cowplot::ggdraw() +
    cowplot::draw_image("G:/water/Groundwater/2_YUKON_OBSERVATION_WELL_NETWORK/4_YOWN_DATA_ANALYSIS/1_WATER LEVEL/00_AUTOMATED_REPORTING/01_MARKUP_IMAGES/Template_grades.jpg") +
    cowplot::draw_plot(final)

  # Create save folder in specified directory
  dir.create(paste0(saveTo, "/", AQID), showWarnings = FALSE)

  # Specify stats Y/N in file name
  if(stats == TRUE){
    stats <- "stats"
  } else {
    stats <- "nostats"
  }

  # Final plot saving
  ggplot2::ggsave(plot = final_plot, filename = paste0(saveTo, "/", AQID, "/", AQID, "_YOWNplot_", name, "_", stats, ".pdf"),  height = 8.5, width = 11, units = "in")

  print(paste0("Plot written to ", saveTo, "/", AQID))
}
