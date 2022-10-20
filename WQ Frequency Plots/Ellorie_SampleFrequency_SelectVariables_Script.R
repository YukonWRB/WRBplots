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

# Define the desired figure sets
# These must match the Variable Names
figureSets <- list(
	al_alk = c("Aluminum Total", "Alkalinity Total CaCO3"),
	c_ar_cu = c("Carbon Total Organic", "Arsenic Total", "Copper Total"),
	nn_be = c("Nitrogen Nitrite", "Beryllium Extractable"),
	many =  c("Aluminum Total", "Arsenic Total", "Copper Total", "Lead Total", "Barium Total", "Phosphorus Total", "Bismuth Total", "Carbon Dissolved Organic", "Nitrogen Nitrite", "Beryllium Extractable")
)

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
	
	# run teh loop to create all figures and dump them into their folders
	for(j in seq(figureSets)){
		
		print(figureSets[[j]])
		
		# If the variable set figure subdirectory directory doesn't yet exist, create it.
		if(!dir.exists(paste0("Figures/", names(figureSets)[j]))){
			dir.create(paste0("Figures/", names(figureSets)[j]))
		}
		
		# Apply the custom Yukon colors after generating the basic ggplot
		# see yukon_palettes object to view other sub_pal choices
		outPlot <- sampleFreqPlot(df = raw, varNames = figureSets[[j]], site = site, dateBreaks = "12 months") +
			scale_color_yukon(yukon_palettes, sub_pal = "main")
		
		# You might decide you want to adjust the scaling factor
		# We have a scaling factor so that each plot will adjust height based on the number of requested Variables
		ggsave(outPlot, filename = paste0("Figures/", names(figureSets)[j], "/", names(figureSets)[j], site, ".jpg"), width = 6.5, height = 0.9*length(figureSets[[j]]))
		
	}
	
}
