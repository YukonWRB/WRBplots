# R Examples
# KDV Decision Analysis LLC
# ashton.drew@kdv-decisions.com


# YUKON CUSTOM COLOR PALETTE LESSON ----

# The Yukon Government provides a palette of colors for web page design.  It would be nice if we could easily use these colors in our R graphs.
#   - Yukon colors: https://guide.yukon.ca/en/content-and-design/brand-standard/print-colour-and-palettes

# Step 1: Create named vector of colors ----

# Match names with hex color values
yukon_colors <- c("Black" = "#000000",
                  "MidnightSun" = "#F2A900",
                  "BrynumMoss" = "#7A9A01",
                  "RiverBlue" = "#0097A9",
                  "Zinc" = "#244C5A",
                  "Twilight" = "#512A44",
                  "Lichen" = "#DC4405")

# A quick base graphics pie chart of the colors
# par(mar = rep(0, 4)) # Set the plot margins to 0 on all four sides
# pie(rep(1, length(yukon_colors)), col = yukon_colors, labels = names(yukon_colors))
# pie(rep(1, length(yukon_colors)), col = yukon_colors, labels = as.character(yukon_colors))
# Step 2: Create a function to subset of colors from your palette ----

base_pal <- function(full_pal, ...) {
  cols <- c(...)
  
  if (is.null(cols))
    return (full_pal)
  
  full_pal[cols]
}

# Test the function
# base_pal(full_pal = yukon_colors, "BrynumMoss", "RiverBlue")

# Use the function to create a suite of palettes from the Yukon colors
yukon_palettes <- list(
  main  = base_pal(full_pal = yukon_colors, "RiverBlue", "BrynumMoss", "MidnightSun"),
  
  cool  = base_pal(full_pal = yukon_colors, "RiverBlue", "BrynumMoss"),
  
  warm   = base_pal(full_pal = yukon_colors, "MidnightSun", "Lichen", "Twilight"),
  
  mixed = base_pal(full_pal = yukon_colors, "RiverBlue", "BryumMoss", "MidnightSun", "Lichen", "Twilight"),
  
  dark  = base_pal(full_pal = yukon_colors, "Zinc", "Black")
)

# Step 3: Create a function to turn a sub-palette into a color ramp
new_pal <- function(x, palette = "main", reverse = FALSE) {
  pal <- x[[palette]]
  
  if (reverse) pal <- rev(pal)
  
  return(colorRampPalette(pal))
}

# Quickly inspect the colors in a pie graph
# test <- new_pal(x=yukon_palettes, palette="mixed")(20)
# par(mar = rep(0, 4))
# pie(rep(1, length(test)), col = test, labels = test)

# Step 4a: Create a custom ggplot scale for color ----
scale_color_yukon <- function(x, sub_pal = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- new_pal(x, palette = sub_pal, reverse = reverse)
  
  # Use an if else to enable both discrete and continuous color scale creation
  if (discrete) {
    discrete_scale(aesthetic = "colour", 
                   scale_name = paste0("yukon_", sub_pal), 
                   palette = pal,
                   ...)
  } else {
    scale_color_gradientn(colours = pal(256), ...)
  }
}

# Step 4b: Create a custom ggplot scale for fill ----
scale_fill_yukon <- function(x, sub_pal = "main", discrete = TRUE, reverse = FALSE, ...) {
  pal <- new_pal(x, palette = sub_pal, reverse = reverse)
  
  # Use an if else to enable both discrete and continuous color scale creation
  if (discrete) {
    discrete_scale(aesthetic = "fill", 
                   scale_name = paste0("yukon_", sub_pal), 
                   palette = pal,
                   ...)
  } else {
    scale_color_gradientn(colours = pal(256), ...)
  }
}

# Test the color and fill scale functions with a discrete scale

# library(ggplot2)
# 
# ggplot(iris, aes(Sepal.Width, Sepal.Length, color = Species)) +
#   geom_point(size = 4) +
#   scale_color_yukon(yukon_palettes)
# 
# ggplot(iris, aes(Sepal.Width, Sepal.Length, color = Species)) +
#   geom_point(size = 4) +
#   scale_color_yukon(yukon_palettes, sub_pal = "warm")
# 
# ggplot(iris, aes(Sepal.Width, Sepal.Length, color = Species)) +
#   geom_point(size = 4) +
#   scale_color_yukon(yukon_palettes, sub_pal = "mixed", labels = c("a", "b", "c"), name = "Flowers")
# 
# ggplot(mpg, aes(manufacturer, fill = manufacturer)) +
#   geom_bar() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
#   scale_fill_yukon(yukon_palettes, sub_pal = "main", guide = "none")
# 
# # Test the color and fill scale functions with a continuous scale
# 
# ggplot(iris, aes(Sepal.Width, Sepal.Length, color = Sepal.Length)) +
#   geom_point(size = 4, alpha = .6) +
#   scale_color_yukon(yukon_palettes, sub_pal = "cool", discrete = FALSE)
# 
# ggplot(iris, aes(Sepal.Width, Sepal.Length, color = Sepal.Length)) +
#   geom_point(size = 4, alpha = .6) +
#   scale_color_yukon(yukon_palettes, sub_pal = "cool", discrete = FALSE, reverse = TRUE)
# 
# # To assign color as a constant, use the original named vector
# 
# ggplot(iris, aes(Sepal.Width, Sepal.Length, color = Species)) +
#   geom_point(size = 4, color = yukon_colors["BrynumMoss"])
# 
# # PRACTICE ----
# 
# # It is quite likely you'd want to create plots with 0 or NA values represented by a light grey ("#F2EDEE").  Modify yukon_colors to have this grey with name:"NovemberFog".  Then add two new palettes to the yukon_palettes list.  "blues" will have "NovemberFog" and "YukonRiver".  "greens" will have "NovemberFog" and "BrynumMoss".  Test that both palettes are accessible to you in ggplot.
# 
#              