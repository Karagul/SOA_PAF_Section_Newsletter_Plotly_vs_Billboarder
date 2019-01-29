# Function to check whether package is installed, if not, install the missing packages
requiredpackages = c("shiny", "shinydashboard", "dplyr", "reshape2", "plotly", "billboarder")
package.check <- lapply(requiredpackages, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE)
  }
})

# Data for scatter plots: Iris dataset
data(iris)

# For bar charts: French electricity generation by year and branch.
energydata <- prod_par_filiere %>% select(-prod_total) 

# For line charts: Monthly supply / demand balance (january 2007 to june 2017)
french_electricity <- equilibre_mensuel
