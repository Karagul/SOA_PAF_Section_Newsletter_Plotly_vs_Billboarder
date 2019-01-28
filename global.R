# Function to check whether package is installed
is.installed <- function(pkg){
  is.element(pkg, installed.packages()[,1])
} 

# check if package "pacman" is installed
if (!is.installed("pacman")){
  install.packages("pacman")
} else {
  library(pacman)
}

p_load(shiny, shinydashboard, dplyr, plotly, billboarder, mlbench)

data("PimaIndiansDiabetes")


Bright.Blue <- rgb(red = 0, green = 129, blue = 227, maxColorValue = 255)
Gold <- rgb(red = 255, green = 162, blue = 0, maxColorValue = 255)