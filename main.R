list.of.packages <- c("ggplot2", "shiny","s2", "gganimate","dplyr","readxl",)
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)


library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(plotly) # for interactable graphs
library(readxl)
library(tidyverse)
library(stringr)
library(viridis) # colorschems for better readability and color blindness


#install.packages('gganimate')
library(gganimate)

###### DEFINING GWP VALUES
data_gwp <- read_excel("./datasets/IPCC_AR4-AR6_GWPs.xlsx", sheet = "Main")
source("./scripts/gwp-cumulative-over-time.R")
# source("./scripts/gwp-bar-and-donut.R")
# Filter the dataset for specific gases
filtereddataset <- data_gwp %>%
  filter(grepl('NH3|OC|BC|Nitrous oxide|SO2|Carbon dioxide|Methane$', GHG))

# Sort to ensure consistent slicing (this part seems fine if your indices are correct)
sort.df <- filtereddataset %>%
  arrange(GHG, str_sort(Indicator, numeric = TRUE))

# Slice the data (ensure indices 6-8, 14-16, and 22-24 exist)
CO_data <- slice(sort.df, 6, 7, 8)
methane_data <- slice(sort.df, 14, 15, 16)
NO_data <- slice(sort.df, 22, 23, 24)

# below values are from 
#https://theicct.org/sites/default/files/BC_policy-relevant_summary_Final.pdf 
#they are are a bit old being from 2009 but should still be relatively applicable
black_carbon_values <- c(1600, 460, 140)
organic_carbon_values <- c(-240, -69, -21)

# 1. Extract the value column from the sliced data frames
CO_values <- CO_data$`GWP kgCO2e/kg GHG`
methane_values <- methane_data$`GWP kgCO2e/kg GHG`
NO_values <- NO_data$`GWP kgCO2e/kg GHG`

# 2. Combine all values into a single vector
all_values <- c(CO_values, methane_values, NO_values, 
                black_carbon_values, organic_carbon_values)


# Create a clean dataframe for plotting
# The length of the 'Gas' and 'time_point' vectors must match the length of 'all_values'
gwp_data_plotting <- data.frame(
  Gas = rep(c("Carbon Dioxide", "Methane", "Nitrous Oxide", "Black Carbon", "Organic Carbon"), each = 3),
  time_point = rep(c("GWP20", "GWP100", "GWP500"), times = 5), # Corrected 'times = 3' to 'times = 5'
  value = all_values # Use the combined vector
)
# Ensure the time points stay in chronological order on the X-axis
gwp_data_plotting$time_point <- factor(gwp_data_plotting$time_point, levels = c("GWP20", "GWP100", "GWP500"))




##### emission share
source("./scripts/emission-by-share.R")

# each row is its own group, while the column is the y values

# make slider for ammount of years to show relative to start forwards in time

## slider in sidebar, later only show when page/tab active

# make some way to show for a single country


##### Emission per industry  
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
   gwp_linegraph(gwp_data_plotting)
    
  })
  output$donought <- renderPlot({
    
  })
  output$barchart1 <- renderPlot({
    barcharta
  })
  
}

# 4. RUN APP
shinyApp(ui, server)

