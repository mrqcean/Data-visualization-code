library(shiny)
library(tidyverse)
library(plotly)
library(readxl)
emissions_by_industry <- read_excel("./datasets/EDGAR-FOOD_v61_AP.xlsx",sheet = "Suppl. Table 4-Emi by Country")
countries <- slice(emissions_by_industry, 274,1191,1167,361)

filtereddatasetbar <- filter(
  emissions_by_industry,
  grepl('Denmark|Congo$|Singapore|El Salvador', ...2)
)
ggplot(filtereddatasetbar, aes(x = ...2, y = ...4, fill = ...3))+ geom_col()
ggplot(filtereddatasetbar, aes(x = ...2, y = ...24, fill = ...3))+ geom_col()
ggplot(filtereddatasetbar, aes(x = ...2, y = ...44, fill = ...3))+ geom_col()
#-------------------------------------------------
#Use your dataset: filtereddatasetbar
#-------------------------------------------------
df_long <- filtereddatasetbar %>%
  rename(
    country = ...2,
    emission_type = ...3
  ) %>%
  pivot_longer(
    cols = ...4:...34,
    names_to = "year",
    values_to = "emission"
  ) %>%
  mutate(
    year = 1974 + (as.numeric(str_extract(year, "\\d+")) - 4)
  )

#----- AI APP 

#-------------------------------------------------
#Shiny App
#-------------------------------------------------
ui <- fluidPage(
  titlePanel("Interactive Emission Trends (1974–2004)"),

  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput(
        "countries", "Select Countries:",
        choices = sort(unique(df_long$country)),
        selected = unique(df_long$country)
      ),
      checkboxGroupInput(
        "types", "Select Emission Types:",
        choices = sort(unique(df_long$emission_type)),
        selected = unique(df_long$emission_type)
      )
    ),

    mainPanel(
      plotlyOutput("plot", height = "700px")
    )
  )
)

server <- function(input, output) {

  filtered_data <- reactive({
    df_long %>%
      filter(
        country %in% input$countries,
        emission_type %in% input$types
      )
  })

  output$plot <- renderPlotly({
    plot_ly(
      filtered_data(),
      x = ~year,
      y = ~emission,
      color = ~country,
      type = "scatter",
      mode = "lines+markers",
      hoverinfo = "text",
      text = ~paste0(
        "<b>Country:</b> ", country,
        "<br><b>Type:</b> ", emission_type,
        "<br><b>Year:</b> ", year,
        "<br><b>Emission:</b> ", emission
      )
    ) %>%
      layout(
        title = "Emission Trends (Click Filters on Left)",
        xaxis = list(title = "Year"),
        yaxis = list(title = "Emission"),
        legend = list(orientation = "h")
      )
  })
}

shinyApp(ui, server)
