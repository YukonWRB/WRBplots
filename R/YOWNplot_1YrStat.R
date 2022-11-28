#' YOWN 1-Year Stat Plot Generation Function
#'
#' This function checks length of YOWN site, if greater than 2 years calculates and plots most recent year's level data and historical statistics. If period of record is shorter than 2 years, prints most recent year of level data only.
#'
#' To store login credentials in your .renviron profile, call usethis::edit_r_environ() and enter your username and password as value pairs, as AQUSER="your username" and AQPASS = "your password".
#'
#' @param AQID Identity of YOWN site in the following format: "YOWN-XXXX" or "YOWN-XXXXD"
#' @param timeSeriesID Identity of the time series exactly as written in Aquarius (eg."Wlevel_bgs.Calculated")
#' @param saveTo Location for data files to be saved. Default is creation of a new folder on your desktop.
#' @param AQTSServerID Defaults to Yukon Water Resources Branch Aquarius web server
#' @param login Your Aquarius login credentials as a character vector of two (eg. c("cmfische", "password") Default pulls information from your .renviron profile; see details.
#'
#'
#' @return Plot of most recent year's groundwater level, with calculated historical statistics if applicable.
#'
#' @export
#'
#' @examples
YOWNStatPlot1Yr <- function(AQID,
                            timeSeriesID = "Wlevel_bgs.Calculated",
                            saveTo = "desktop",
                            AQTSServerID ="https://yukon.aquaticinformatics.net/AQUARIUS",
                            login = Sys.getenv(c("AQUSER", "AQPASS")))
  {

  if(tolower(saveTo) == "desktop") {
    saveTo <- paste0("C:/Users/", Sys.getenv("USERNAME"), "/Desktop")
  }
  if(dir.exists(saveTo) == FALSE) {
    stop("Specified directory does not exist")
  }
  print(AQID)

  # Download data from Aquarius
  datalist <- WRBtools::aq_download(loc_id = AQID,
                                    ts_name = "Wlevel_bgs.Calculated")

  # Unlist time series data
  timeseries <- datalist$timeseries

  # Replace all grades below C with "Redacted"
  timeseries$grade_description[timeseries$grade_description != "A" & timeseries$grade_description != "B" & timeseries$grade_description != "C" & timeseries$grade_description != "MISSING DATA"] <- "REDACTED"

  # Replace all values with  grade of less than C with NA, to remove values from plots. This screens out GW recovery patterns and bad data from plots and calculations
  timeseries$value[timeseries$grade_description != "A" & timeseries$grade_description != "B" & timeseries$grade_description != "C"] <- NA

  # Change timestamps from UTC to MST
  attr(timeseries$timestamp_UTC , "tzone") <- "MST"
  names(timeseries)[names(timeseries) == "timestamp_UTC"] <- "timestamp_MST"

  #Find data gaps of greater than 6 hours (indicative of logger failure) and generate NA data sets to fill in gaps
  timeseries$ts_lag <- dplyr::lag(timeseries$timestamp_MST)
  timeseries$lag_val <- timeseries$timestamp_MST - timeseries$ts_lag
  gapdf <- timeseries %>%
    dplyr::filter(lag_val > 6)
  gapdf$lag_val <- as.numeric(gapdf$lag_val)

  if(nrow(gapdf != 0)){
    # Create a list of data frames for each identified data gap, fill in hourly time stamps
    gaplist <- list()
    for(i in 1:nrow(gapdf)) {
      df <- data.frame(seq.POSIXt(from = gapdf[i, 1], by = "-1 hour", length.out = gapdf[i, 8]), NA, as.character(-5), "MISSING DATA", gapdf$approval_level[i], gapdf$approval_description[i], NA, NA)
      colnames(df) <- colnames(gapdf)
      gaplist[[i]] <- df
    }

    # Merge all listed gap data frames, combine with original timeseries, order and format
    gapmerge <- do.call(rbind, gaplist)
    fulldf <- dplyr::full_join(timeseries, gapmerge)
    fulldf <- fulldf[order(fulldf$timestamp_MST),] # Order by timestamp
    fulldf <- fulldf[!duplicated(fulldf["timestamp_MST"]),] #Remove second entry for duplicated timestamps
  } else {
    fulldf <- timeseries
  }

  # Add colour column to plot df
  fulldf <- fulldf %>%
    dplyr::mutate(col = dplyr::case_when(
      grade_description == "A" ~ "#7A9A01",
      grade_description == "B" ~ "#0097A9",
      grade_description == "C" ~ "#F2A900",
      grade_description == "REDACTED" ~ "#DC4405",
      grade_description == "MISSING DATA" ~ "black"))

  #### Check length of time series, if less than 2 years do not calculate any stats ####
  if(max(timeseries$timestamp_MST, na.rm = TRUE) - min(timeseries$timestamp_MST, na.rm = TRUE) > 730){

    # Group by day, calculate daily max and min values
    fulldf$day <- format(fulldf$timestamp_MST, "%m-%d")
    dayrange <- dplyr::group_by(fulldf, day) %>%
      dplyr::summarize(min = min(value, na.rm = TRUE), max = max(value, na.rm = TRUE), mean = mean(value, na.rm = TRUE), N = dplyr::n())
    dayrange <- na.omit(dayrange)

    # Join statistics to plot data
    fulldf <-dplyr::full_join(fulldf, dayrange)
    print("Statistics calculated")

    # Subset 1 year of most recent data data, based on date of last site visit
    plotdf <- subset(fulldf, fulldf$timestamp_MST >= (max(na.omit(fulldf$timestamp_MST)) - lubridate::years(1)))

    #Identify data gaps of more than 6 hours, indicative of logger failure
    NAcomp <- rle(!is.na(plotdf$value))
    NAcomp$values[which(NAcomp$lengths>6 & !NAcomp$values)] <- TRUE
    NAadd <- inverse.rle(NAcomp)

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
      ggnewscale::new_scale_colour() +
      ggplot2::geom_path(data = plotdf, ggplot2::aes(x = timestamp_MST, y = plyr::round_any(max(na.omit(max)), 0.5, f = ceiling), colour = factor(grade_description), group = 1), linewidth = 2.5, show.legend = FALSE) +
      ggplot2::scale_colour_manual(name = "Grades", values = c("A" = "#7A9A01", "B" = "#0097A9", "C" = "#F2A900", "REDACTED" = "#DC4405", "MISSING DATA" = "black")) +
      cowplot::theme_cowplot() +
      ggplot2::theme(plot.margin = ggplot2::unit(c(4.2, 1.6, 3.1, 1.2), "cm"),
            panel.border = ggplot2::element_rect(color = "grey",
                                        fill = NULL,
                                        size = 0.5),
            axis.text.x = ggplot2::element_text(angle = 0,
                                       hjust  = 0.5,
                                       vjust = -0.5,
                                       size = 10),
            axis.line.x.bottom = ggplot2::element_blank(),
            axis.text.y = ggplot2::element_text(hjust = 1,
                                       size = 10),
            axis.title.y = ggplot2::element_text(vjust = 2,
                                        size = 12,
                                        colour = "#464646"),
            axis.line.y.left = ggplot2::element_blank(),
            panel.grid.major = ggplot2::element_line(colour = "lightgrey", size = 0.5, linetype = 1),
            legend.position = "bottom",
            legend.justification = "left",
            legend.margin = ggplot2::margin(0,0,0,0),
            legend.box.margin = ggplot2::margin(-18, 0, 0, -10),
            legend.text = ggplot2::element_text(size = 9)) +
      ggplot2::scale_x_datetime(name = "",
                       limits = c(max(plotdf$timestamp) - lubridate::years(1), max(plotdf$timestamp)),
                       date_breaks = "1 month",
                       date_labels = "%b-%y",
                       expand = c(0, 0)) +
      ggplot2::scale_y_reverse(name = "Water Level (m below ground surface)",
                      limits = c(plyr::round_any(max(na.omit(plotdf$max)), 0.5, f = ceiling), plyr::round_any(min(na.omit(plotdf$min)), 0.25, f = floor)),
                      breaks = seq(ceiling(max(na.omit(plotdf$max))), floor(min(na.omit(plotdf$min))), by = -0.25),
                      expand = c(0, 0))

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

    caption <- ggplot2::ggplot() +
      ggplot2::geom_blank() +
      ggplot2::theme_minimal() +
      ggplot2::labs(title = paste0("Max & Min data calculated from period of record from ", strftime(as.POSIXct(min(na.omit(timeseries$timestamp_MST))), format = "%Y-%m-%d"), " to ", strftime(as.POSIXct(max(na.omit(timeseries$timestamp_MST))), format = "%Y-%m-%d"), " (Date of last site visit)",  "\nPlot generated: ", Sys.Date(), "\nYukon Observation Well Network")) +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0,
                                      vjust = 0,
                                      size = 9,
                                      colour = "#464646"),
            plot.margin = ggplot2::unit(c(-2.39, 0, 0, 0.6), "cm"))
  } else {
    print("Insufficient data for statistics calculation")

    # Subset 1 year of most recent data data, based on date of last site visit
    plotdf <- subset(fulldf, fulldf$timestamp_MST >= (max(na.omit(fulldf$timestamp_MST)) - lubridate::years(1)))

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
      ggnewscale::new_scale_colour() +
      ggplot2::geom_path(data = plotdf, ggplot2::aes(x = timestamp_MST, y = plyr::round_any(max(na.omit(value)), 0.5, f = ceiling), colour = factor(grade_description), group = 1), size = 2, show.legend = FALSE) +
      ggplot2::scale_colour_manual(name = "Grades", values = c("A" = "#7A9A01", "B" = "#0097A9", "C" = "#F2A900", "REDACTED" = "#DC4405", "MISSING DATA" = "black")) +
      cowplot::theme_cowplot() +
      ggplot2::theme(plot.margin = ggplot2::unit(c(4.2, 1.6, 3.1, 1.2), "cm"),
            panel.border = ggplot2::element_rect(color = "grey",
                                        fill = NULL,
                                        size = 0.5),
            axis.text.x = ggplot2::element_text(angle = 0,
                                       hjust  = 0.5,
                                       vjust = -0.5,
                                       size = 10),
            axis.line.x.bottom = ggplot2::element_blank(),
            axis.text.y = ggplot2::element_text(hjust = 1,
                                       size = 10),
            axis.title.y = ggplot2::element_text(vjust = 2,
                                        size = 12,
                                        colour = "#464646"),
            axis.line.y.left = ggplot2::element_blank(),
            panel.grid.major = ggplot2::element_line(colour = "lightgrey", size = 0.5, linetype = 1),
            legend.position = "bottom",
            legend.justification = "left",
            legend.margin = ggplot2::margin(0,0,0,0),
            legend.box.margin = ggplot2::margin(-18, 0, 0, -10),
            legend.text = ggplot2::element_text(size = 9)) +
      ggplot2::scale_x_datetime(name = "",
                       limits = c(max(plotdf$timestamp) - lubridate::years(1), max(plotdf$timestamp)),
                       date_breaks = "1 month",
                       date_labels = "%b-%y",
                       expand = c(0, 0)) +
      ggplot2::scale_y_reverse(name = "Water Level (m below ground surface)",
                      limits = c(plyr::round_any(max(na.omit(plotdf$value)), 0.5, f = ceiling), plyr::round_any(min(na.omit(plotdf$value)), 0.25, f = floor)),
                      breaks = seq(ceiling(max(na.omit(plotdf$value))), floor(min(na.omit(plotdf$value))), by = -0.25),
                      expand = c(0, 0))

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

    caption <- ggplot2::ggplot() +
      ggplot2::geom_blank() +
      ggplot2::theme_minimal() +
      ggplot2::labs(title = paste0("Period of record from ", strftime(as.POSIXct(min(na.omit(timeseries$timestamp_MST))), format = "%Y-%m-%d"), " to ", strftime(as.POSIXct(max(na.omit(timeseries$timestamp_MST))), format = "%Y-%m-%d"), " (Date of last site visit)",  "\nPlot generated: ", Sys.Date(), "\nYukon Observation Well Network")) +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0,
                                      vjust = 0,
                                      size = 9,
                                      colour = "#464646"),
            plot.margin = ggplot2::unit(c(-2.39, 0, 0, 0.6), "cm"))
    print("1 Year Record Plot Generated")
  }

  #### Final metadata add, format and save plot ####
  subtitle <- ggplot2::ggplot() +
    ggplot2::geom_blank() +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = paste0("Source Data: ", AQID, "@", AQID,  "\nLatitude: ", datalist[["metadata"]][5, 2], ", ", "Longitude: ", datalist[["metadata"]][6, 2],", ", "Elevation: ", datalist[["metadata"]][7, 2], " ", datalist[["metadata"]][8, 2])) +
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

  dir.create(paste0(saveTo, "/", AQID), showWarnings = FALSE)
  ggplot2::ggsave(plot = final_plot, filename = paste0(saveTo, "/", AQID, "/", AQID, "_1YrStatPlot", ".pdf"),  height = 8.5, width = 11, units = "in")

  print(paste0("1 Year stat plot written to ", saveTo, "/", AQID))
  return(final)

}






