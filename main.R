library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(readxl)

# 1. DATA AND SCRIPTS (GLOBAL)
# Ensure your working directory is the folder containing 'datasets' and 'scripts'
data <- read_excel("./datasets/IPCC_AR4-AR6_GWPs.xlsx", sheet = "Main")
source("./scripts/gwp-cumulative-over-time.R")

# 2. UI DEFINITION
ui <- dashboardPage(
  dashboardHeader(title = "GWP Analysis"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("GWP Dashboard", tabName = "dashboard", icon = icon("chart-line"))
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "dashboard",
              fluidRow(
                box(
                  title = "Global Warming Potential Over Time",
                  status = "primary", 
                  solidHeader = TRUE, 
                  width = 12, # Full width usually looks better for graphs
                  plotOutput("my_line_graph")
                )
              )
      )
    )
  )
)

# 3. SERVER LOGIC
server <- function(input, output) {
  output$my_line_graph <- renderPlot({
    gwp_linegraph(data)
  })
}

# 4. RUN APP
shinyApp(ui, server)
