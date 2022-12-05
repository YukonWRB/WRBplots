#' YOWN site comparative plot generation, in m bgs
#'
#' Plots multiple YOWN stations on the same chart
#'
#' To store login credentials in your .renviron profile, call usethis::edit_r_environ() and enter your username and password as value pairs, as AQUSER="your username" and AQPASS = "your password".
#' Marsh Lake: 09AB004
#' Yukon River at Carmacks: 09AH001
#'
#' @param YOWNindex Character vector of YOWN site IDs (eg. c("YOWN-2201S", "YOWN-2201D", "YOWN-2202"))
#' @param WSCindex Character vector of WSC site IDs (eg. c("09AB004", "09AH001"))
#' @param saveTo Location for data files to be saved. Default is publication to a new folder on your desktop.
#' @param AQTSServerID Defaults to Yukon Water Resources Branch Aquarius web server
#' @param login Your Aquarius login credentials as a character vector of two (eg. c("cmfische", "password") Default pulls information from your .renviron profile; see details.
#'
#' @return Writes .pdf plot of WSC and YOWN data
#'
#' @export
YOWNplot_FullRecord <- function(YOWNindex = c("YOWN-2201S", "YOWN-2201D", "YOWN-2202", "YOWN-2203", "YOWN-2204", "YOWN-2205"),
                                AQTSServerID ="https://yukon.aquaticinformatics.net/AQUARIUS",
                                chartRange = "all",
                                chartXInterval ="1 year",
                                saveTo = "desktop") {

  AQTSServerID ="https://yukon.aquaticinformatics.net/AQUARIUS"
  index = c("YOWN-2201S", "YOWN-2201D", "YOWN-2202", "YOWN-2203", "YOWN-2204", "YOWN-2205")
  chartRange = "all"
  timeSeriesID="Wlevel_bgs.Calculated"
  chartXInterval ="1 year"
  saveTo = "desktop"

  if(tolower(saveTo) == "desktop") {
    saveTo <- paste0("C:/Users/", Sys.getenv("USERNAME"), "/Desktop")
  }
  if(dir.exists(saveTo) == FALSE) {
    stop("Specified directory does not exist")
  }

  # Create index of desired YOWN stations for plotting
  index <- c("YOWN-2201S", "YOWN-2201D", "YOWN-2202", "YOWN-2203", "YOWN-2204", "YOWN-2205")
  chartXInterval ="1 month"
  saveTo = "G:\\water\\Groundwater\\2_YUKON_OBSERVATION_WELL_NETWORK\\4_YOWN_DATA_ANALYSIS\\1_WATER LEVEL\\01_ARMY_BEACH\\"

  # Create a list of data frames for plotting
  sitelist <- list()

  # Populate list
  for (i in index) {

    print(i)

    # Download data from Aquarius
    datalist <- WRBtools::aq_download(loc_id = i,
                                      ts_name = "Wlevel_bgs.Calculated")

    # Unlist time series data
    timeseries <- datalist$timeseries

    # Change AQTS number grades into letters, replace all below C with "Redacted"
    timeseries$grade_description[timeseries$grade_description != "A" & timeseries$grade_description != "B" & timeseries$grade_description != "C" & timeseries$grade_description != "MISSING DATA"] <- "REDACTED"

    # Replace all values with  grade of less than C with NA, to remove values from plots. This screens out GW recovery patterns
    timeseries$value[timeseries$grade_description != "A" & timeseries$grade_description != "B" & timeseries$grade_description != "C" & timeseries$grade_description != "Missing Data"] <- NA

    # Change timestamps from UTC to MST
    attr(timeseries$timestamp_UTC , "tzone") <- "MST"
    names(timeseries)[names(timeseries) == "timestamp_UTC"] <- "timestamp_MST"

    #Find data gaps of greater than 6 hours (indicative of logger failure) and generate value NA data sets to fill in gaps
    timeseries$ts_lag <- dplyr::lag(timeseries$timestamp_MST)
    timeseries$lag_val <- difftime(timeseries$timestamp_MST, timeseries$ts_lag, units = "hours")
    gapdf <- timeseries %>%
      dplyr::filter(lag_val > 6)
    gapdf$lag_val <- as.numeric(gapdf$lag_val)

    # If there are gaps present, fill in gaps with hourly timestamps
    if(nrow(gapdf != 0)){
      # Create a list of data frames for each identified data gap, fill in hourly time stamps
      gaplist <- list()
      for(i in 1:nrow(gapdf)) {
        df <- data.frame(seq.POSIXt(from = gapdf[i, 1], by = "-1 hour", length.out = gapdf[i, 8]), NA, as.character(-5), "MISSING DATA", gapdf$approval_level[i], gapdf$approval_description[i], NA, NA)
        colnames(df) <- colnames(gapdf)
        gaplist[[i]] <- df
      }

      # Merge all listed gap data frames, combine with original timeseries, order and format. If no gaps proceed with base timeseries
      gapmerge <- do.call(rbind, gaplist)
      df <- dplyr::full_join(timeseries, gapmerge)
    } else {
      df <- timeseries
    }
    df$YOWNID <- i
    df <- df %>%
      dplyr::select(YOWNID, everything())
    sitelist[[i]] <- df
  }

  # Combine list into one data frame, adjust df to size based on chartRange function param (TODO)
  plotdf <- do.call(rbind, sitelist)
  rownames(plotdf) <- NULL
  # if(chartRange = "all")

  # Plot data, format and export
  plot <- ggplot2::ggplot() +
    geom_line(data = plotdf, aes(x = timestamp_MST, y = value, group = YOWNID, colour = YOWNID),
              na.rm = TRUE) +
    guides(color = guide_legend(override.aes = list(size = 1.5), nrow = 1)) +
    theme_cowplot() +
    theme(plot.margin = unit(c(4.2, 1.6, 3.1, 1.2), "cm"),
          panel.border = element_rect(color = "grey",
                                      fill = NULL,
                                      size = 0.5),
          axis.text.x = element_text(angle = 0,
                                     hjust  = 0.5,
                                     vjust = -0.5,
                                     size = 10),
          axis.line.x.bottom = element_blank(),
          axis.text.y = element_text(hjust = 1,
                                     size = 10),
          axis.title.y = element_text(vjust = 2,
                                      size = 12,
                                      colour = "#464646"),
          axis.line.y.left = element_blank(),
          panel.grid.major = element_line(colour = "lightgrey", size = 0.5, linetype = 1),
          legend.position = "bottom",
          legend.title = element_blank(),
          legend.justification = "left",
          legend.margin = margin(0,0,0,0),
          legend.box.margin = margin(-18, 0, 0, -10),
          legend.text = element_text(size = 9)) +
    scale_x_datetime(name = "",
                     limits = c(min(plotdf$timestamp_MST), max(plotdf$timestamp_MST)),
                     date_breaks = chartXInterval,
                     date_labels = "%b-%y",
                     expand = c(0, 0)) +
    scale_y_reverse(name = "Water Level (m below ground surface)",
                    limits = c(plyr::round_any(max(na.omit(plotdf$value)), 0.5, f = ceiling), plyr::round_any(min(na.omit(plotdf$value)), 0.5, f = floor)),
                    breaks = seq(ceiling(max(na.omit(plotdf$value))), floor(min(na.omit(plotdf$value))), by = -0.25),
                    expand = c(0, 0))

  title <- ggplot2::ggplot() +
    geom_blank() +
    theme_minimal() +
    labs(title = "Groundwater Level Record: Site Comparison") +
    theme(plot.title = element_text(hjust = 0,
                                    vjust = 0,
                                    size = 14,
                                    colour = "#244C5A",
                                    face = "bold"),
          plot.margin = unit(c(6.3, 0, 0, 0.51), "cm"))

  caption <- ggplot2::ggplot() +
    geom_blank() +
    theme_minimal() +
    labs(title = paste0("Period of Record: ", strftime(as.POSIXct(min(na.omit(timeseries$timestamp_MST))), format = "%Y-%m-%d"), " to ", strftime(as.POSIXct(max(na.omit(timeseries$timestamp_MST))), format = "%Y-%m-%d"), " (Date of last site visit)",  "\nPlot generated: ", Sys.Date(), "\nYukon Observation Well Network")) +
    theme(plot.title = element_text(hjust = 0,
                                    vjust = 0,
                                    size = 9,
                                    colour = "#464646"),
          plot.margin = unit(c(-2.39, 0, 0, 0.6), "cm"))

  subtitle <- ggplot2::ggplot() +
    geom_blank() +
    theme_minimal() +
    labs(title = paste0("Source Data: Aquarius Time Series", "\nWlevel_bgs.Calculated")) +
    theme(plot.title = element_text(hjust = 0,
                                    vjust = 0,
                                    size = 10,
                                    color = "#464646"),
          plot.margin = unit(c(6.85, 0, 0, 0.6), "cm"))

  # Use plot_grid method to combine titles, captions, and main plot in proper orientation
  final <- cowplot::plot_grid(title, subtitle, plot, caption, ncol = 1, nrow = 4, rel_heights = c(0.1, 0.1, 2, 0.1))

  # Add final aesthetic tweaks, print plot onto template
  final_plot <- cowplot::ggdraw() +
    draw_image("G:\\water\\Groundwater\\2_YUKON_OBSERVATION_WELL_NETWORK\\4_YOWN_DATA_ANALYSIS\\1_WATER LEVEL\\00_AUTOMATED_REPORTING\\01_MARKUP_IMAGES\\template_nogrades.jpg") +
    draw_plot(final)

  ggplot2::ggsave(plot = final_plot, filename = paste0(saveTo, "SiteCompare.pdf"),  height = 8.5, width = 11, units = "in")

  print("Site Comparison Plot Generated")
