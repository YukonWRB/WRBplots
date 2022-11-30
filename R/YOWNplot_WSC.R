#Load libraries and dependencies
library(dplyr)
library(cowplot)

#' YOWN-WSC comparative plot generation relative to a common datum
#'
#' Plots YOWN data alongside WSC stations, in m asl. Only works for stations that have values in the "Elevation" field in their Aquarius information page.
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

YOWNplot_WSC <- function(YOWNindex = c("YOWN-2201S", "YOWN-2201D", "YOWN-2202", "YOWN-2203", "YOWN-2204", "YOWN-2205"),
                         WSCindex = c("09AH001"),
                         saveTo = "desktop",
                         title = "YOWN wells vs. WSC Sites",
                         chartXinterval = "1 month") {

  # YOWNindex = c("YOWN-2006S", "YOWN-2006D")
  # WSCindex = c("09AH001")
  # saveTo = "desktop"
  # title = "YOWN wells vs. WSC Sites"
  # chartXinterval = "1 month"



  if(tolower(saveTo) == "desktop") {
    saveTo <- paste0("C:/Users/", Sys.getenv("USERNAME"), "/Desktop")
  }
  if(dir.exists(saveTo) == FALSE) {
    stop("Specified directory does not exist")
  }

  # Download all YOWN data
  YOWNlist <- list()
  for(i in YOWNindex) {

    print(i)

    # Download data from Aquarius
    datalist <- WRBtools::aq_download(loc_id = i,
                                      ts_name = "Wlevel_masl.Calculated")

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
      for(j in 1:nrow(gapdf)) {
        df <- data.frame(seq.POSIXt(from = gapdf[j, 1], by = "-1 hour", length.out = gapdf[j, 8]), NA, as.character(-5), "MISSING DATA", gapdf$approval_level[j], gapdf$approval_description[j], NA, NA)
        colnames(df) <- colnames(gapdf)
        gaplist[[j]] <- df
      }

      # Merge all listed gap data frames, combine with original timeseries, order and format. If no gaps proceed with base timeseries
      gapmerge <- do.call(rbind, gaplist)
      df <- suppressMessages(dplyr::full_join(timeseries, gapmerge))
    } else {
      df <- timeseries
    }

    # Reorder columns, add df to list
    df$ID <- as.character(i)
    df <- df %>%
      dplyr::select(ID, timestamp_MST, value)
    YOWNlist[[i]] <- df
  }

  # Merge all YOWN data frames together
  plotdf_YOWN <- do.call(rbind, YOWNlist)
  rownames(plotdf_YOWN) <- NULL

  # Download all WSC data
  WSClist <- list()
  for(i in WSCindex) {

    print(i)

    # Download data from Aquarius
    datalist <- WRBtools::aq_download(loc_id = i,
                                      ts_name = "Stage.Preliminary")

    # Unlist time series data
    timeseries <- datalist$timeseries

    # Change timestamps from UTC to MST
    attr(timeseries$timestamp_UTC , "tzone") <- "MST"
    names(timeseries)[names(timeseries) == "timestamp_UTC"] <- "timestamp_MST"

    #Filter out all observations not taken on the hour
    df <- timeseries
    df <- df %>%
      dplyr::filter(lubridate::minute(timestamp_MST) == 00)

    # Add station elevation to convert to m asl
    df$value <- df$value + as.numeric(datalist[["metadata"]][7, 2])

    # Reorder columns, add df to list
    df$ID <- as.character(i)
    df <- df %>%
      dplyr::select(ID, timestamp_MST, value)
    WSClist[[i]] <- df
  }

  # Merge all WSC data frames together
  plotdf_WSC <- do.call(rbind, WSClist)
  rownames(plotdf_WSC) <- NULL

  # Trim WSC data frame to time period of longest well record, as WSC records tend to be much longer
  plotdf_WSC <- plotdf_WSC %>%
    dplyr::filter(timestamp_MST >= min(na.omit(plotdf_YOWN$timestamp_MST)))

  # Create one massive data frame for chart parameter calculation
  fulldf <- dplyr::full_join(plotdf_YOWN, plotdf_WSC, by = c("ID", "timestamp_MST", "value"))

  diff <- as.numeric(difftime(max(fulldf$timestamp), min(fulldf$timestamp), units = "days"))
  chartXInterval <- dplyr::case_when(
    diff < 365 ~ "1 month",
    diff >= 365 & diff < 730 ~ "2 months",
    diff >= 730 & diff < 1460 ~ "6 months",
    diff > 1460 ~ "1 year")

  # Plot data
  plot <- ggplot2::ggplot() +
    ggplot2::geom_line(data = plotdf_YOWN,
                       ggplot2::aes(x = timestamp_MST, y = value, group = ID, colour = ID)) +
    ggplot2::geom_line(data = plotdf_WSC,
                       ggplot2::aes(x = timestamp_MST, y = value, group = ID, colour = ID)) +
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
          legend.title = ggplot2::element_blank(),
          legend.position = "bottom",
          legend.justification = "left",
          legend.margin = ggplot2::margin(0,0,0,0),
          legend.box.margin = ggplot2::margin(-18, 0, 0, -10),
          legend.text = ggplot2::element_text(size = 9)) +
    ggplot2::scale_x_datetime(name = "",
                     limits = c(min(plotdf_YOWN$timestamp_MST), max(plotdf_YOWN$timestamp_MST)),
                     date_breaks = chartXinterval,
                     date_labels = "%m-%Y",
                     expand = c(0, 0)) +
    ggplot2::scale_y_continuous(name = "Water Level (m above sea level)",
                       limits = c(plyr::round_any(min(na.omit(fulldf$value)), 0.5, f = floor), plyr::round_any(max(na.omit(fulldf$value)), 0.5, f = ceiling)),
                       breaks = seq(plyr::round_any(min(na.omit(fulldf$value)), 0.5, f = floor), plyr::round_any(max(na.omit(fulldf$value)), 0.5, f = ceiling), by = 0.5),
                       expand = c(0, 0))

  title <- ggplot2::ggplot() +
    ggplot2::geom_blank() +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = title) +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0,
                                    vjust = 0,
                                    size = 14,
                                    colour = "#244C5A",
                                    face = "bold"),
          plot.margin = ggplot2::unit(c(6.3, 0, 0, 0.51), "cm"))

  caption <- ggplot2::ggplot() +
    ggplot2::geom_blank() +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = paste0("Period of Record: ", strftime(as.POSIXct(min(na.omit(plotdf_YOWN$timestamp_MST))), format = "%Y-%m-%d"), " to ", strftime(as.POSIXct(max(na.omit(plotdf_YOWN$timestamp_MST))), format = "%Y-%m-%d"),  "\nPlot generated: ", Sys.Date(), "\nYukon Observation Well Network")) +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0,
                                    vjust = 0,
                                    size = 9,
                                    colour = "#464646"),
          plot.margin = ggplot2::unit(c(-2.39, 0, 0, 0.6), "cm"))

  subtitle <- ggplot2::ggplot() +
    ggplot2::geom_blank() +
    ggplot2::theme_minimal() +
    ggplot2::labs(title = paste0("YOWN Sites: ", paste(YOWNindex, collapse = ", "), "\n", "WSC Sites: ", paste(WSCindex, collapse = ", "))) +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0,
                                    vjust = 0,
                                    size = 10,
                                    color = "#464646"),
          plot.margin = ggplot2::unit(c(6.85, 0, 0, 0.6), "cm"))

  # Use plot_grid method to combine titles, captions, and main plot in proper orientation
  final <- cowplot::plot_grid(title, subtitle, plot, caption, ncol = 1, nrow = 4, rel_heights = c(0.1, 0.1, 2, 0.1))

  # Add final aesthetic tweaks, print plot onto template
  final_plot <- cowplot::ggdraw() +
    cowplot::draw_image("G:\\water\\Groundwater\\2_YUKON_OBSERVATION_WELL_NETWORK\\4_YOWN_DATA_ANALYSIS\\1_WATER LEVEL\\00_AUTOMATED_REPORTING\\01_MARKUP_IMAGES\\template_nogrades.jpg") +
    cowplot::draw_plot(final)

  ggplot2::ggsave(plot = final_plot, filename = paste0(saveTo, "/", "YOWN_WSC_compare", ".pdf"),  height = 8.5, width = 11, units = "in")

  print("Plot Generated")

}


