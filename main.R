
if (!requireNamespace("shiny", quietly = TRUE)) install.packages("shiny")
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
library(shiny)
library(dplyr)
library(ggplot2)
# reading xlsx package, Install, then load 
install.packages("readxl")
library("readxl")

###### GWP visualization 
data <- read_excel("./datasets/IPCC_AR4-AR6_GWPs.xlsx",sheet = "Main")
source("./scripts/gwp-cumulative-over-time.R")
gwplinegraph(data)

##### main dataset
# save as different main_data <- read_excel("")
install.packages("shinydashboard")
library(shinydashboard)

library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(),
  dashboardBody()
)

server <- function(input, output) { }

shinyApp(ui, server)


# emissions by industry
emissions_by_industry <- read_excel("./datasets/EDGAR-FOOD_v61_AP.xlsx",sheet = "Suppl. Table 3-Emi by stage")
# "non-dr" congo, elsalvador, singapore og danmark
# these are row  274, 1191,1167 ,361
countries <- slice(emissions_by_industry, 274,1191,1167,361)

##### Importing scripts
# source does not work recursively, else we have to define a custom 
# function that for each file sources that file  



