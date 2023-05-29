# Code developed in R 4.2.1
# KDV Decision Analysis LLC

#' Generate sample frequency plots
#'
#' Point plots illustrate sample frequency for one or more
#' variables in a ____ file.
#'
#' @param df A tibble or dataframe.  Data from a ___ station, so expects columns Date and Variable Name.
#' @param varNames A character vector of variable names for data to be plotted.  Must match values found in the Variable Name column.
#' @param site The site name which will be used for the title
#' @param dateBreaks The date breaks that will be used on teh x axis.  See scale_x_date for options and formatting.
#' @return A ggplot.
sampleFreqPlot <- function(df, varNames, site, dateBreaks, dateLimits = NULL){
	
	# Because I use the assertthat package - I'm causing the function to stop and prompt the user to install the package if they don't have it.  There are more elegant ways to do this, but this is a very simple way.
			stopifnot("Please install package assertthat to enable helpful messaging." = "assertthat" %in% installed.packages())
	
	# These lines check that the input data meet requirements.  If an assert fails, the code will stop and provide a helpful message.
	assertthat::assert_that(is.data.frame(df), msg="df must be a data.frame or tibble")
	assertthat::assert_that(all(c("Variable Name", "Date") %in% names(df)), msg="the df object must include columns named: 'Variable Name' and 'Date'.")
	assertthat::assert_that(is.character(df[["Variable Name"]]), msg="The column 'Variable Name' must be a character vector.")
	assertthat::assert_that(is.Date(df[["Date"]]), msg="The column 'Date' must be Date format.")
	assertthat::assert_that(all(varNames %in% unique(raw[["Variable Name"]])), msg="Check your varNames vector.  Some requested values are not found in the 'Variable Name' column.")
	
	# And now the actual function code:
	
	# Subset out the selected variables
	subData <- dplyr::filter(df, `Variable Name` %in% varNames)
	
	# Generate plot
	outPlot <- ggplot() +
		geom_point(data = subData, aes(x = Date, y = `Variable Name`, color = `Variable Name`), size = 1)
	
	# Apply custom date limits if provided as an argument
	if (!is.null(dateLimits)){
		outPlot <- outPlot +
			scale_x_date(date_breaks = dateBreaks, limits = dateLimits, expand = c(0,0))
	} else {
		outPlot <- outPlot +
			scale_x_date(date_breaks = dateBreaks, expand = c(0,0))
	}
	
	outPlot <- outPlot +
		# custom yukon color palette requires source("ggplot_custom_color_scales.R") so let's exclude it from the function - colors can be added after.
		#scale_color_yukon(yukon_palettes, sub_pal = "mixed") +
		guides(color = "none") +
		labs(title = paste0("Site: YT", site), x="", y="",
				 subtitle = paste0("Dates: ", min(subData$Date, na.rm=T), 
				 									" to ", max(subData$Date, na.rm=T))) +
		theme_bw() +
		theme(axis.text.x = element_text(angle = 90, size = 6),
					plot.title = element_text(hjust = 0.5),
					plot.subtitle = element_text(hjust = 0.5, color = "gray30"))
	
	return(outPlot)
}
