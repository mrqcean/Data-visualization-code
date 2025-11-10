
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
data <- read_excel("./datasets/IPCC_AR4-AR6_GWPs.xlsx")
source("./scripts/gwp-cumulative-over-time.R")
gwplinegraph(data)

##### Importing scripts
# source does not work recursively, else we have to define a custom 
# function that for each file sources that file  



