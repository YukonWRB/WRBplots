# This script illustrates how I worked through steps to get to using a custom function to create a set of graphs via loops

# The script to actually use is Ellorie_SampleFrequency_Script.R

library(readr)
library(dplyr)
library(stringr)
library(lubridate)
library(ggplot2)


# Custom yukon colors for ggplots
source("ggplot_custom_color_scales.R")





# Step 1: Work out code for one example plot ----

# Identify file (and site name and download date?)
file <- "Data/Exported_Samples_YT08AB0009_2022_09_14.csv"

site <- stringr::str_extract(file, pattern="(?<=YT)[^_]*")

# read in data and convert time to date format
raw <- read_csv(file, show_col_types = FALSE) %>%
	dplyr::mutate(Date = as.Date(lubridate::mdy_hm(`Sample Time`)))

# check available variables
unique(raw$`Variable Name`)

# select subset of interest
subVars <- c("Aluminum Total", "Alkalinity Total CaCO3")
subData <- dplyr::filter(raw, `Variable Name` %in% subVars)

outPlot <- ggplot() +
	geom_point(data = subData, aes(x = Date, y = `Variable Name`, color = `Variable Name`)) +
	scale_x_date(date_breaks = "12 months") +
	scale_color_yukon(yukon_palettes, sub_pal = "mixed") +
	guides(color = "none") +
	labs(title = paste0("Site: YT", site), x="", y="",
			 subtitle = paste0("Dates: ", min(subData$Date, na.rm=T), 
			 									" to ", max(subData$Date, na.rm=T))) +
	theme_bw() +
	theme(axis.text.x = element_text(angle = 90, size = 6),
				plot.title = element_text(hjust = 0.5),
				plot.subtitle = element_text(hjust = 0.5, color = "gray30"))
outPlot
ggsave(outPlot, filename = "sample_frequency_test_plot.jpg", width = 6.5, height = 2)

# Step 2: Create function ----


sampleFreqPlot <- function(df, varNames, site, dateBreaks){
	
	# Subset out the selected variables
	subData <- dplyr::filter(df, `Variable Name` %in% varNames)
	
	# Generate plot
	outPlot <- ggplot() +
		geom_point(data = subData, aes(x = Date, y = `Variable Name`, color = `Variable Name`)) +
		scale_x_date(date_breaks = dateBreaks) +
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

sampleFreqPlot(df = raw, varNames = c("Aluminum Total", "Alkalinity Total CaCO3"), site = "08AB0009", dateBreaks = "12 months")

sampleFreqPlot(df = raw, varNames = c("Carbon Total Organic", "Arsenic Total", "Copper Total"), site = "08AB0009", dateBreaks = "12 months") +
	scale_color_yukon(yukon_palettes, sub_pal = "mixed")


# Step 3: Create a loop with the function inside ---

# Two loops to create 3 graphics for each of three sites.  This method assumes that each site will have it's own csv file names according to a standard method and containing the same column names.


# Look in the folder of station data to find all the sample files
siteFiles <- list.files("Data")
figureSets <- list(
	al_alk = c("Aluminum Total", "Alkalinity Total CaCO3"),
	c_ar_cu = c("Carbon Total Organic", "Arsenic Total", "Copper Total"),
	nn_be = c("Nitrogen Nitrite", "Beryllium Extractable")
)
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

# Step 4: Create an R file for this function

# I called it function_sampleFreqPlot.R

# In that file I added more documentation (roxygen style comments) and some hopefully helpful assertthat tests of the input data.
