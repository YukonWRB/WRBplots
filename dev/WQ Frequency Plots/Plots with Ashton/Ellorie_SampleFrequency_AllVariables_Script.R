# Generate sample frequency plots

# Code developed in R 4.2.1

library(readr)
library(dplyr)
library(stringr)
library(lubridate)
library(ggplot2)
library(assertthat)

# Source (aka run) the custom yukon colors functions and the sampleFreqPlot function
source("ggplot_custom_color_scales.R")
source("function_sampleFreqPlot.R")

# Look in the folder of station data to find all the sample files
# This assumes you have one or more sample site *.csv files AND NOTHING ELSE 
# in a folder called Data
siteFiles <- list.files("Data") # I am not 100% certain this works on mac...?

# Each figure set will go in its own directory
# Check if the directory already exists - if not create it
if(!dir.exists("Figures")){
	dir.create("Figures")
}

for (i in seq(siteFiles)){
	
	file <- siteFiles[i]
	print(file)
	
	# Extract site name from file name
	# This assumes naming similar to the example file and that all sites start with "YT"
	# I can help you write a different regex search if this assumption is not true
	# This search looks for and extracts "everything after YT until there is an _".
	site <- stringr::str_extract(file, pattern="(?<=YT)[^_]*")
	
	# read in data and convert time to date format
	# This assumes the Sample Time column will have the same structure as the example file
	raw <- read_csv(paste0("Data/", file), show_col_types = FALSE) %>%
		dplyr::mutate(Date = as.Date(lubridate::mdy_hm(`Sample Time`)))
	print(paste0("This file has ", dplyr::n_distinct(raw[["Variable Name"]]), " variables."))
	
	# Apply the custom Yukon colors after generating the basic ggplot
	# see yukon_palettes object to view other sub_pal choices
	outPlot <- sampleFreqPlot(df = raw, varNames = unique(raw[["Variable Name"]]), 
														site = site, dateBreaks = "12 months",
														dateLimits = lubridate::ymd(c("1990/01/01", "2022/09/01"))) +
		scale_color_yukon(yukon_palettes, sub_pal = "main") +
		scale_y_discrete(limits = rev) +
		# reduce font size for all variables plot
		theme(axis.text.y = element_text(size = 5))
	
	# Print to full page for all variables plot (allowing for 1/2 inch margins)
	ggsave(outPlot, filename = paste0("Figures/AllVariables_", site, ".jpg"), width = 7.5, height = 10)
	
}
