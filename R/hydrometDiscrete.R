#' Discrete hydromet data plotting
#'
#' @description
#' `r lifecycle::badge('stable')`
#'
#' Generate plots of snow survey data (SWE and depth) or other variables sampled at regular intervals (weekly or monthly).
#'
#' Notice: in many cases, you're better off using the Shiny app at [WRBfloods::hydroApp()] to generate and export your plot. Read on if you need additional control over the final product.
#'
#' This function plots data from the local hydrometric database (maintained by the WRBdatabase package) and yields consistent-looking plots for discrete data. This function can only plot what's in the database, use the function [DB_browse_ts()] to see what's in there first. Data can be represented as violin plots, as regular box plots or as a 'linedbox' plot (imitates plots currently used in the snow bulletin).
#'
#' @param location The location for which you want a plot.
#' @param parameter The parameter you wish to plot. The location:parameter combo must be in the local database.
#' @param startDay NOT CURRENTLY IN USE The start day of year for the plot x-axis. Can be specified as a number from 1 to 365, as a character string of form "yyyy-mm-dd", or as a date object. Either way the day of year is the only portion used, specify years to plot under parameter `years`.
#' @param endDay NOT CURRENTLY IN USE The end day of year for the plot x-axis. As per `startDay`.
#' @param tzone The timezone to use for fetching data. Datetimes are stored in the database in UTC offset, so this parameter could make a difference to what day a particular sample is considered to be on. In most cases you can ignore this parameter.
#' @param years The years to plot. If `startDay` and `endDay` cover December 31 - January 1, select the December year(s). Max 10 years, NULL = current year.
#' @param title Should a title be included?
#' @param plot_type Choose from "violin" , "boxplot" or "linedbox".
#' @param plot_scale Adjusts/scales the size of plot text elements. 1 = standard size, 0.5 = half size, 2 = double the size, etc. Standard size works well in a typical RStudio environment.
#' @param save_path Default is NULL and the graph will be visible in RStudio and can be assigned to an object. Option "choose" brings up the File Explorer for you to choose where to save the file, or you can also specify a save path directly.
#' @param dbPath The path to the local hydromet database, passed to [hydroConnect()].
#' @param discrete_data A dataframe with the data to be plotted. Must contain the following columns: year, month, value and units.
#' @return A .png file of the plot requested (if a save path has been selected), plus the plot displayed in RStudio. Assign the function to a variable to also get a plot in your global environment as a ggplot object which can be further modified
#' @export

# hydrometDiscrete(location=NULL, parameter='SWE', years=c(2021, 2022), title=TRUE, plot_type = "boxplot", discrete_data = swe_basin, save_path = "")

hydrometDiscrete <- function(location=NULL,
                             parameter,
                             startDay = 1,
                             endDay = 365,
                             tzone = "MST",
                             years = NULL,
                             title = TRUE,
                             plot_type = "violin",
                             plot_scale = 1,
                             save_path = NULL,
                             dbPath = "default",
                             discrete_data = NULL)
{
  # Commented code below is for testing...
  # location = "08AA-SC01"
  # parameter = "SWE"
  # startDay = 1
  # endDay = 365
  # tzone = "MST"
  # years = c(2022)
  # title = TRUE
  # plot_scale = 1
  # plot_type = "boxplot"
  # save_path = NULL
  # dbPath ="default"
  # discrete_data = NULL

  #TODO Should give a decent error message if the user requests something that doesn't exist. Station not existing, timeseries not existing, years not available (and where they are), etc.

  if (startDay != 1){
    startDay <- 1
    message("Parameter startDay is not currently in use and has been reset to the default of 1.")
  }
  if (endDay != 365){
    endDay <- 365
    message("Parameter endDay is not currently in use and has been reset to the default of 365.")
  }

  # Checks on input parameters  and other start-up bits------------------
  if (parameter != "SWE"){
    parameter <- tolower(parameter)
  }

  plot_type <- tolower(plot_type)
  if (!(plot_type %in% c("violin", "boxplot", "linedbox"))){
    stop("Parameter 'plot_type' must be one of 'violin' or 'boxplot'")
  }

  if (is.null(years)){
    years <- as.numeric(substr(Sys.Date(), 1, 4))
  } else {
    years <- as.numeric(years)
    years <- sort(years, decreasing = TRUE)
    if (length(years) > 10){
      years <- years[1:10]
      print("The parameter 'years' can only have up to 10 years. It's been truncated to the first 10 years in the vector.")
    }
  }
  # Select save path
  if (!is.null(save_path)){
    if (save_path %in% c("Choose", "choose")) {
      print("Select the folder where you want this graph saved.")
      save_path <- as.character(utils::choose.dir(caption="Select Save Folder"))
    }
  }


  if (is.null(discrete_data)) {
    #Connect
    con <- WRBtools::hydroConnect(path = dbPath, silent = TRUE)
    on.exit(DBI::dbDisconnect(con))

    # Dealing with start/end dates ----------------------
    # Sort out startDay and endDay into actual dates if needed
    last_year <- max(years)
    leap_list <- (seq(1800, 2100, by = 4))  # Create list of all leap years
    tryCatch({
      startDay <- as.character(startDay)
      startDay <- as.POSIXct(startDay, tz = tzone)
      lubridate::year(startDay) <- last_year
    }, error = function(e) {
      if (last_year %in% leap_list){
        if (startDay > 59){
          startDay <<- startDay + 1
        }
      }
      startDay <<- as.POSIXct(as.numeric(startDay)*60*60*24, origin = paste0(last_year-1, "-12-31"), tz = "UTC")
      startDay <<- lubridate::force_tz(startDay, tzone)
    })
    tryCatch({
      endDay <- as.character(endDay)
      endDay <- as.POSIXct(endDay, tz = tzone)
      lubridate::year(endDay) <- last_year
    }, error = function(e) {
      tempStartDay <- lubridate::yday(startDay) #using yday because start is now in proper Date format and needs to be back-converted to yday
      if (last_year %in% leap_list){
        if (endDay > 59){
          endDay <<- endDay + 1
        }
      }
      endDay <<- as.POSIXct(as.numeric(endDay)*60*60*24, origin = paste0(last_year-1, "-12-31 23:59:59"), tz = "UTC")
      endDay <<- lubridate::force_tz(endDay, tzone)
    })
    if (startDay > endDay){ #if the user is wanting a range overlapping the new year
      lubridate::year(endDay) <- lubridate::year(endDay)+1
      overlaps <- TRUE
    } else {
      overlaps <- FALSE
    }

    day_seq <- seq.POSIXt(startDay, endDay, by = "day")

    #Check for existence of timeseries, then for presence of data within the time range requested.
    exists <- DBI::dbGetQuery(con, paste0("SELECT * FROM timeseries WHERE location = '", location, "' AND parameter = '", parameter, "' AND type = 'discrete'"))
    if (nrow(exists) == 0){
      stop("There is no entry for the location and parameter combination that you specified of discrete data type. If you are trying to graph continuous data use hydrometContinuous.")
    } else if (nrow(exists) > 1){
      stop("There is more than one entry in the database for the location and parameter that you specified! Please alert the database manager ASAP.")
    }



    #Find the ts units
    units <- DBI::dbGetQuery(con, paste0("SELECT units FROM timeseries WHERE parameter = '", parameter, "' AND location = '", location, "'"))

    # Get the data ---------------------
    all_discrete <- DBI::dbGetQuery(con, paste0("SELECT * FROM discrete WHERE location = '", location, "' AND parameter = '", parameter, "' AND sample_date < '", paste0(max(years), substr(endDay, 5, 10)), "'"))
    if (nrow(all_discrete) == 0){
      stop(paste0("There doesn't appear to be any data for the year and days you specified: this timeseries starts ",  exists$start_datetime_UTC))
    }
    all_discrete$target_date <- as.Date(all_discrete$target_date)
    all_discrete$sample_date <- as.Date(all_discrete$sample_date)
    all_discrete$year <- lubridate::year(all_discrete$target_date)
    all_discrete$month <- lubridate::month(all_discrete$target_date)
    all_discrete$day <- lubridate::day(all_discrete$target_date)
    #Separate, modify, and re-bind feb29 days, if any
    feb29 <- all_discrete[all_discrete$month == 2 & all_discrete$day == 29, ]
    if (nrow(feb29) > 0){
      all_discrete <- all_discrete[!(all_discrete$month == 2 & all_discrete$day == 29), ]
      feb29$target_date <- feb29$target_date + 1
      feb29$month <- 3
      feb29$day <- 1
      all_discrete <- rbind(all_discrete, feb29)
    }

    #Make a fake date
    all_discrete$fake_date <- as.Date(gsub("[0-9]{4}", last_year, all_discrete$target_date))
    discrete <- data.frame()
    for (i in years){
      start <- as.Date(paste0(i, substr(startDay, 5, 10)))
      end <- as.Date(paste0(i, substr(endDay, 5, 10)))
      if (overlaps){
        lubridate::year(end) <- lubridate::year(end) +1
      }
      new_discrete <- all_discrete[all_discrete$target_date >= start & all_discrete$target_date <= end , ]
      discrete <- rbind(discrete, new_discrete)
    }
    if (nrow(discrete) == 0){
      stop("There is no data to graph after filtering for your specified year(s) and day range. Try again with different days.")
    }

  }

  if (!is.null(discrete_data)) {
    ## Create all_discrete
    all_discrete <- discrete_data
    # add fake_date
    all_discrete$fake_date <- as.Date(paste0(max(years), "-0", all_discrete$month, "-01" ))
    ## Create discrete
    #discrete <- all_discrete %>% dplyr::filter(year %in% years)
    discrete <- all_discrete[all_discrete$year %in% years, ]
    ## Give units
    units <- unique(discrete$units)

  }

  if (plot_type == 'linedbox') {
    stats_discrete <- all_discrete %>%
      dplyr::group_by(month) %>%
      dplyr::summarise(value = min(value), type = "min") %>%
      dplyr::bind_rows(all_discrete %>%
                  group_by(month) %>%
                  dplyr::summarise(value = max(value), type = "max")) %>%
      dplyr::bind_rows(all_discrete %>%
                  group_by(month) %>%
                  dplyr::summarise(value = stats::median(value), type = "median"))

    stats_discrete$fake_date <- as.Date(paste0(max(years), "-", stats_discrete$month, "-01"))

  }

  #Make the plot --------------------
  colours = c("blue", "black", "darkorchid3", "cyan2", "firebrick3", "aquamarine4", "gold1", "chartreuse1", "darkorange", "lightsalmon4")
  # c("black", "#DC4405", "#512A44", "#F2A900", "#244C5A", "#687C04", "#C60D58", "#0097A9", "#7A9A01", "#834333")
  legend_length <- length(years)
  plot <- ggplot2::ggplot(all_discrete, ggplot2::aes(x = fake_date, y = value, group = fake_date)) +
    ggplot2::labs(x = "", y = if (parameter == "SWE") paste0("SWE (", units, ")") else paste0(stringr::str_to_title(parameter), " (", units, ")")) +
    ggplot2::theme_classic() +
    ggplot2::theme(legend.position = "right", legend.justification = c(0, 0.95), legend.text = ggplot2::element_text(size = 8*plot_scale), legend.title = ggplot2::element_text(size = 10*plot_scale), axis.title.y = ggplot2::element_text(size = 12*plot_scale), axis.text.x = ggplot2::element_text(size = 9*plot_scale), axis.text.y = ggplot2::element_text(size = 9*plot_scale))

  if (plot_type == "linedbox") {
    for (m in unique(stats_discrete$month)) {
      plot <- plot +
        ggplot2::geom_rect(data = stats_discrete[stats_discrete$month == m,],   fill = 'grey87', ggplot2::aes(xmin=fake_date - 12, xmax=fake_date + 12, ymin=min(value), ymax=max(value)))
    }
    plot <- plot +
      ggplot2::geom_segment(data = stats_discrete, linewidth = plot_scale*1.5,
                            ggplot2::aes(color=type, yend=value,
                                         x=fake_date - 12, xend=fake_date + 12)) +
      ggplot2::scale_color_manual(name = "", labels = c("Maximum", "Median", "Minimum"), values=c("#0097A9", "#7A9A01", "#834333")) +
      ggnewscale::new_scale_color()

  } else if (plot_type == "violin") {
    plot <- plot +
      ggplot2::geom_violin(draw_quantiles = c(0.5), adjust = 0.7, width = 12, alpha = 0.8, fill = "aliceblue", scale = "width") #Using a scale other than "width" may result in issues for locations where there are many "0" values.
  } else if (plot_type == "boxplot"){
    plot <- plot +
      ggplot2::geom_boxplot(outlier.shape = 8 , outlier.size = 1.7*plot_scale, color = "black", fill = "aliceblue", varwidth = TRUE)
  }
  plot <- plot +
    ggplot2::geom_point(data = discrete, mapping = ggplot2::aes(x = fake_date, y = value, colour = as.factor(year), fill = as.factor(year)), size = plot_scale*3.5, shape = 21) +
    ggplot2::scale_colour_manual(name = "Year", labels = unique(discrete$year), values = colours[1:legend_length], aesthetics = c("colour", "fill"), na.translate = FALSE, breaks=unique(stats::na.omit(discrete$year))[1:legend_length])

  # Wrap things up and return() -----------------------
  if (title == TRUE){
    if (is.null(discrete_data)){
      stn_name <- DBI::dbGetQuery(con, paste0("SELECT name FROM locations where location = '", location, "'"))
      titl <- paste0("Location ", location, ": ", stn_name)
    } else {
      if (!is.null(location)) {
        titl <- paste0("Location: ", location)}
      else {
        titl <- paste0("Location: ", unique(all_discrete$location))
      }

    }

    plot <- plot +
      ggplot2::labs(title=titl) +
      ggplot2::theme(plot.title=ggplot2::element_text(hjust=0.05, size=14*plot_scale))
  }

  #Save it if requested
  if (!is.null(save_path)){
    ggplot2::ggsave(filename=paste0(save_path,"/", location, "_", parameter, "_", Sys.Date(), "_", lubridate::hour(as.POSIXct(format(Sys.time()), tz=tzone)), lubridate::minute(as.POSIXct(format(Sys.time()), tz=tzone)), ".png"), plot=plot, height=8, width=12, units="in", device="png", dpi=500)
  }

  return(plot)
}
