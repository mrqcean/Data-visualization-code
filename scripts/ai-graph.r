# Load necessary libraries
library(shiny)
library(readxl)
library(tidyverse)
library(plotly)
library(bslib)
library(scales)

# scripts/ai-graph.r

# --- DATA PROCESSING FUNCTION ---
load_and_clean_combined_data <- function() {
  # Load Table 3
  df_emi <- read_excel("./datasets/EDGAR-FOOD_v61_AP.xlsx", 
                       sheet = "Suppl. Table 3-Emi by stage", skip = 2)
  
  # Load Table 5
  df_share <- read_excel("./datasets/EDGAR-FOOD_v61_AP.xlsx", 
                         sheet = "Suppl. Table 5 - FOOD shares", skip = 2)
  
  # Standardize Column Names
  colnames(df_emi)[1] <- "Country_ISO"
  colnames(df_share)[1] <- "Country_ISO"
  
  # Transform Table 3 to Long Format
  df_emi_long <- df_emi %>%
    pivot_longer(cols = matches("^[0-9]{4}$"), 
                 names_to = "Year", 
                 values_to = "Emissions_kton") %>%
    mutate(Year = as.numeric(Year))
  
  # Transform Table 5 to Long Format
  df_share_long <- df_share %>%
    pivot_longer(cols = matches("^[0-9]{4}$"), 
                 names_to = "Year", 
                 values_to = "Food_Share") %>%
    mutate(Year = as.numeric(Year))
  
  # Combine Tables
  combined_data <- df_emi_long %>%
    left_join(df_share_long %>% select(Country_ISO, Substance, Year, Food_Share), 
              by = c("Country_ISO", "Substance", "Year")) %>%
    filter(!is.na(Emissions_kton)) %>%
    replace_na(list(Food_Share = 0))
  
  return(combined_data)
}

# Create the global object that the main file is looking for
ai_data <- load_and_clean_combined_data()

# --- UI DEFINITION ---
ui <- page_sidebar(
  theme = bs_theme(version = 5, bootswatch = "flatly", primary = "#2c3e50"),
  title = "EDGAR-FOOD Global Insights",
  
  sidebar = sidebar(
    title = "Controls",
    selectInput("substance", "Select Substance:", choices = NULL),
    selectInput("country", "Select Country:", choices = NULL, selected = "World"),
    checkboxGroupInput("stages", "Food System Stages:", choices = NULL),
    sliderInput("year_range", "Year Range:", min = 1970, max = 2018, value = c(1990, 2018), sep = "")
  ),
  
  layout_column_wrap(
    width = 1/2,
    card(
      card_header("Emissions Trend by Stage (kton)"),
      plotlyOutput("trendPlot")
    ),
    card(
      card_header("Emissions Flow: Stage to Substance"),
      plotlyOutput("sankeyPlot")
    )
  ),
  card(
    card_header("Food Share Impact over Time"),
    plotlyOutput("sharePlot")
  )
)

# --- SERVER LOGIC ---
server <- function(input, output, session) {
  
  # Load data reactively
  raw_data <- reactive({
    load_and_clean_data()
  })
  
  # Update UI inputs based on data
  observe({
    df <- raw_data()
    updateSelectInput(session, "substance", choices = sort(unique(df$Substance)))
    updateSelectInput(session, "country", choices = sort(unique(df$Name)), selected = "World")
    updateCheckboxGroupInput(session, "stages", 
                             choices = unique(df$FOOD_system_stage),
                             selected = unique(df$FOOD_system_stage))
  })
  
  # Filter data based on user selection
  filtered_df <- reactive({
    req(input$substance, input$country, input$stages)
    raw_data() %>%
      filter(Substance == input$substance,
             Name == input$country,
             FOOD_system_stage %in% input$stages,
             Year >= input$year_range[1],
             Year <= input$year_range[2])
  })
  
  # Plot 1: Interactive Area Chart
  output$trendPlot <- renderPlotly({
    p <- ggplot(filtered_df(), aes(x = Year, y = Emissions_kton, fill = FOOD_system_stage)) +
      geom_area(alpha = 0.8, color = "white", linewidth = 0.1) +
      scale_fill_viridis_d(option = "mako") +
      theme_minimal() +
      labs(x = "Year", y = "Emissions (kton)")
    
    ggplotly(p) %>% layout(legend = list(orientation = 'h', y = -0.2))
  })
  
  # Plot 2: Interactive Sankey (Groundbreaking Visualization)
  output$sankeyPlot <- renderPlotly({
    df_sankey <- filtered_df() %>%
      filter(Year == input$year_range[2]) %>%
      group_by(FOOD_system_stage, Substance) %>%
      summarise(value = sum(Emissions_kton, na.rm = TRUE)) %>%
      ungroup()
    
    # Define nodes and links for Sankey
    nodes <- data.frame(name = unique(c(df_sankey$FOOD_system_stage, df_sankey$Substance)))
    df_sankey$source <- match(df_sankey$FOOD_system_stage, nodes$name) - 1
    df_sankey$target <- match(df_sankey$Substance, nodes$name) - 1
    
    plot_ly(
      type = "sankey",
      orientation = "h",
      node = list(label = nodes$name, pad = 15, thickness = 20, line = list(color = "black", width = 0.5)),
      link = list(source = df_sankey$source, target = df_sankey$target, value = df_sankey$value)
    ) %>% layout(title = paste("Flow for", input$year_range[2]))
  })
  
  # Plot 3: Food Share Trend
  output$sharePlot <- renderPlotly({
    p <- ggplot(filtered_df(), aes(x = Year, y = Food_Share, group = 1)) +
      geom_line(color = "#e74c3c", size = 1) +
      geom_point(aes(text = paste("Year:", Year, "<br>Share:", percent(Food_Share)))) +
      theme_minimal() +
      labs(title = "Food System Share of Total Country Emissions", y = "Share (0 to 1)")
    
    ggplotly(p, tooltip = "text")
  })
}

shinyApp(ui, server)