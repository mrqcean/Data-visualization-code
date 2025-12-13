library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(readxl)

#install.packages('gganimate')
#install.packages('tidyverse')
# library(gganimate)


data_gwp <- read_excel("./datasets/IPCC_AR4-AR6_GWPs.xlsx", sheet = "Main")
source("./scripts/gwp-cumulative-over-time.R")
# source("./scripts/gwp-bar-and-donut.R")

emissions_by_industry <- read_excel("./datasets/EDGAR-FOOD_v61_AP.xlsx",sheet = "Suppl. Table 4-Emi by Country")
# "non-dr" congo, elsalvador, singapore og danmark
# these are row  274, 1191,1167 ,361
countries <- slice(emissions_by_industry, 274,1191,1167,361)

filtereddatasetbar <- filter(
  emissions_by_industry,
  grepl('Denmark|Congo$|Singapore|El Salvador', ...2)
)
barcharta <- ggplot(filtereddatasetbar, aes(x = ...2, y = ...4, fill = ...3))+ geom_col()
barchartb <- ggplot(filtereddatasetbar, aes(x = ...2, y = ...24, fill = ...3))+ geom_col()
barchartc <- ggplot(filtereddatasetbar, aes(x = ...2, y = ...44, fill = ...3))+ geom_col()


sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("GWP Dashboard", tabName = "dashboard", icon = icon("chart-line"))
  )
)


body <- dashboardBody(
  tabItems(
    tabItem(tabName = "dashboard",
            fluidRow(
              # *** START OF THE NEW STRUCTURE ***
              tabBox(
                # Optional: Give the overall tab box a title
                title = "Historical Emissions ", 
                id = "tabset1",
                height = "500px", # Set a fixed height for the box
                width = 12,       # Make the tabBox take up the full width of the row
                
                # 1. First Panel (Existing Line Graph)
                tabPanel(
                  title = "GWP Over Time Graph", # Title for the tab user sees
                  icon = icon("chart-line"),     # Optional: icon for the tab
                  
                  # The content for this tab is your existing plot output
                  plotOutput("my_line_graph") 
                ),
                
                # 2. Second Panel (Empty Placeholder)
                tabPanel(
                  title = "Cumulative Summary", # Title for the second tab
                  icon = icon("table"),         # Optional: icon for the tab
                  
                  # This is the empty space, where you can add summary text or a table later
                  p("This panel is ready for your summary table or text output!")
                  #plotOutput("donought")
                ),
                tabPanel(
                  title = "Bar chart emissions for four countries",
                  icon = icon("bar-chart"),
                  
                  plotOutput("barchart1")
                )
              )
             
            )
    )
  )
)


ui <- dashboardPage(
  dashboardHeader(title = "GWP Analysis"),
  sidebar,
  body,
  

)


# 3. SERVER LOGIC
server <- function(input, output) {
  output$my_line_graph <- renderPlot({
    gwp_linegraph(data_gwp)
  })
  output$donought <- renderPlot({
    
  })
  output$barchart1 <- renderPlot({
    barcharta
  })
  
}

# 4. RUN APP
shinyApp(ui, server)

