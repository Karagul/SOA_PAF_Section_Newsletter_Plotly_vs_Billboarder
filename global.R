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

# Use p_load function from "pacman" to load required libraries
p_load(shiny, shinydashboard, dplyr, reshape2, plotly, billboarder)

# Data for scatter plots: Iris dataset
data(iris)

# For bar charts: French electricity generation by year and branch.
energydata <- prod_filiere_long 
energydata_wide <- prod_par_filiere %>% select(-prod_total)

# For line charts: Monthly supply / demand balance (january 2007 to june 2017)
french_electricity <- equilibre_mensuel
