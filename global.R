# Function to check whether package is installed, if not, install the missing packages
required.packages = c("shiny", "shinydashboard", "dplyr", "reshape2", "plotly", "billboarder")
new.packages <- required.packages[!(required.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(required.packages, require, character.only = TRUE)

# Data for scatter plots: Iris dataset
data(iris)

# For bar charts: French electricity generation by year and branch.
energydata <- prod_par_filiere %>% select(-prod_total) 

# For line charts: Monthly supply / demand balance (january 2007 to june 2017)
french_electricity <- equilibre_mensuel
