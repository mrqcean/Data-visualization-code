list.of.packages <- c("ggplot2", "shiny","s2", "gganimate","dplyr","readxl","viridis", "plotly", "gifski", "maps")
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
#install.packages('gifski')
library(gganimate)
library(gifski)
library(maps)

#Script Sources:
source("./scripts/gwp-bar-and-donut.R")
source("./scripts/gwp-cumulative-over-time.R")
source("./scripts/emission-by-share.R")
source("./scripts/ai-graph.r")

#Required Data Setups:
sum_emissions_by_stage <- librariesAndDataBarDonut()
breakdownData <- prepareBreakdown()
ai_data <- load_and_clean_combined_data()

###### DEFINING GWP VALUES
data_gwp <- read_excel("./datasets/IPCC_AR4-AR6_GWPs.xlsx", sheet = "Main")
# 
# Filter the dataset for specific gases
filtereddataset <- data_gwp %>%
  filter(grepl('NH3|OC|BC|Nitrous oxide|SO2|Carbon dioxide|Methane$', GHG))

# Sort to ensure consistent slicing (this part seems fine if your indices are correct)
sort.df <- filtereddataset %>%
  arrange(GHG, str_sort(Indicator, numeric = TRUE))

# Slice the data (ensure indices 6-8, 14-16, and 22-24 exist)
CO2_data <- slice(sort.df, 6, 7, 8)
#methane_data <- slice(sort.df, 14, 15, 16)
NO_data <- slice(sort.df, 22, 23, 24)

# below values are from
#https://theicct.org/sites/default/files/BC_policy-relevant_summary_Final.pdf
#they are are a bit old being from 2009 but should still be relatively applicable
black_carbon_values <- c(1600, 460, 140)
organic_carbon_values <- c(-240, -69, -21)

# CO has no direct GWP, instead it has oxidizes CH4(methane), which causes methane to have a longer lifetimealued
# https://archive.ipcc.ch/ipccreports/tar/wg1/249.htm#tab69
# CO is very short lived, so GWP is used for long lived gasses. gwp not used in modern climate science for short lived gasses instead gtp
# this from an article from 1998.... called Fuglestvedt et al. (1996): two-dimensional model including CH4 feedbacks and tropospheric O3 production by CO itself
# We do not try to calculate it ourselves as indirect requires fancy enviromental models.
carbon_monoxide_values <- c(10,3.0,	1.0)

# 1. Extract the value column from the sliced data frames
CO2_values <- CO2_data$`GWP kgCO2e/kg GHG`
#methane_values <- methane_data$`GWP kgCO2e/kg GHG`
NO_values <- NO_data$`GWP kgCO2e/kg GHG`

# 2. Combine all values into a single vector
all_values <- c(CO2_values,
                carbon_monoxide_values,
                NO_values,
                black_carbon_values,
                organic_carbon_values)


# Create a clean dataframe for plotting
# The length of the 'Gas' and 'time_point' vectors must match the length of 'all_values'
gwp_data_plotting <- data.frame(
  Gas = rep(c("Carbon Dioxide", "Carbon Monoxide", "Nitrous Oxide", "Black Carbon", "Organic Carbon"), each = 3),
  time_point = rep(c("GWP20", "GWP100", "GWP500"), times = 5), # times = 5 because we have 3x5 data points and we have to have this duplicated so many times for ggpplot.
  value = all_values # Use the combined vector
)


# Ensure the time points stay in chronological order on the X-axis
gwp_data_plotting$time_point <- factor(gwp_data_plotting$time_point, levels = c("GWP20", "GWP100", "GWP500"))
names(gwp_data_plotting)
### GWP VALUES FOR REUSE FOR OTHER GRAPHS 
df_gwp100 <- filter(gwp_data_plotting,`time_point` %in% "GWP100") 



##### emission share
# this contains
# source("./scripts/emission-by-share.R")

#emission_share_graph("OC")
#emissions, NOx, OC,BC, CO
# each row is its own group, while the column is the y values

# make slider for ammount of years to show relative to start forwards in time

## slider in sidebar, later only show when page/tab active

# make some way to show for a single country
# we just input the string can do without caching alright

##### Emission per industry
emissions_by_industry <- read_excel("./datasets/EDGAR-FOOD_v61_AP.xlsx",sheet = "Suppl. Table 4-Emi by Country")
# "non-dr" congo, elsalvador, singapore og danmark
# these are row  274, 1191,1167 ,361

# converted gwp values
names(emissions_by_industry)
emissions_indu_correct_names <- read_excel("./datasets/EDGAR-FOOD_v61_AP.xlsx",sheet = "Suppl. Table 4-Emi by Country",skip = 2)
names(emissions_indu_correct_names)
gasses <- c("CO", "NOx"  , "BC"  ,  "OC")
# do this transformation before extractiong country names to ensure dimensions are correct for plotting
# only keep rows which contain gwp generating gasses
 emissions_indu_correct_names <- filter(emissions_indu_correct_names, Substance %in% gasses)
# remove years which are not 2018
 # remove all columns which to not contain name, substance, or 2018 as the others are other years
 emissions_indu_correct_names <-  select(emissions_indu_correct_names, "2018", "Substance", "Name")
 
# arrow pointer indicates the way flows
countries <-  emissions_indu_correct_names %>% pull("Name") 
# countries is a vector to get looped through for generating dataframe with emissions per country




# calc_country_sum takes a string as arg
# returns a data frame contry,sum
calc_country_sum <- function(country){
  emissions_indu_correct_names
  df <- filter(emissions_indu_correct_names, Name %in% c(country) )
  # access manually even though it is not smart if we add more green house gasses
  # best way would be mutating to same chem name or full name, alfabetical sorting and doing and use the values 
  
  gwp100val <- df_gwp100$value
  bc_factor <- gwp100val[4]
  oc_factor <- gwp100val[5]
  no_factor <- gwp100val[3]
  co_factor <- gwp100val[2]
  country_val <- df$`2018`
  #overwrite the value to the column, this is peak unmaintainable code
  conv_vals <- c(bc_factor*country_val[1], 
                 co_factor *country_val[2], 
                 no_factor * country_val[3], 
                 oc_factor*country_val[4]
                 )
  
  
  return(data.frame(country,sum(conv_vals)))
}
calc_country_sum("Denmark")
  
# filter by year 2018
# for each country extract a the sum and return the country,sum dataframe for all countries 
data <- do.call(
  rbind.data.frame,
  # returns a list lapply is run values as input to argument 2 which is a function
  # YES Lisp style my beloved, you just pass arguments to function which takes a list as arg and it returns a list
  # pm2.5 and pm10 are not included as they are purely air pollution
  lapply(as.list(countries), calc_country_sum)
)
data
# convert values per country
colnames(data) <- c("Country", "Total_Emissions")

#Interactive plotly heat map
# Source: https://plotly.com/r/choropleth-maps/
interactive_heat_map <- plot_geo(data)
interactive_heat_map <- interactive_heat_map %>% add_trace(
    z = ~Total_Emissions,
    color = ~Total_Emissions,
    colors = "YlOrRd", 
    locations = ~Country,
    locationmode = 'country names',
    marker = list(line = list(width = 1))
  )

interactive_heat_map <- interactive_heat_map %>% colorbar(title = "Total GWP Equalized Sum")
interactive_heat_map <- interactive_heat_map %>% layout(
    title = "GWP100 Equalized Sum for Countries Food Emissions Heat Map 2018",
    geo = list(
      showframe = FALSE,
      showcoastlines = FALSE,
      projection = list(type = 'Mercator')
    )
  )

countries <- slice(emissions_by_industry, 274,1191,1167,361)

# Filter dataset for four countries used in bar chart anim
filtereddatasetbar <- filter(
  emissions_by_industry,
  grepl('Denmark|Congo$|Singapore|El Salvador', ...2)
)

# Gather the data for the different years from 1970 to 2010
data_a1 <- filtereddatasetbar %>% select(Country = ...2, Substance = ...3, Amount = ...4) %>% mutate(frame = "Year 1970")
data_b1 <- filtereddatasetbar %>% select(Country = ...2, Substance = ...3, Amount = ...5) %>% mutate(frame = "Year 1971")
data_c1 <- filtereddatasetbar %>% select(Country = ...2, Substance = ...3, Amount = ...6) %>% mutate(frame = "Year 1972")
data_d1 <- filtereddatasetbar %>% select(Country = ...2, Substance = ...3, Amount = ...7) %>% mutate(frame = "Year 1973")
data_e1 <- filtereddatasetbar %>% select(Country = ...2, Substance = ...3, Amount = ...8) %>% mutate(frame = "Year 1974")
data_f1 <- filtereddatasetbar %>% select(Country = ...2, Substance = ...3, Amount = ...9) %>% mutate(frame = "Year 1975")
data_g1 <- filtereddatasetbar %>% select(Country = ...2, Substance = ...3, Amount = ...10) %>% mutate(frame = "Year 1976")
data_h1 <- filtereddatasetbar %>% select(Country = ...2, Substance = ...3, Amount = ...11) %>% mutate(frame = "Year 1977")
data_i1 <- filtereddatasetbar %>% select(Country = ...2, Substance = ...3, Amount = ...12) %>% mutate(frame = "Year 1978")
data_j1 <- filtereddatasetbar %>% select(Country = ...2, Substance = ...3, Amount = ...13) %>% mutate(frame = "Year 1979")
data_k1 <- filtereddatasetbar %>% select(Country = ...2, Substance = ...3, Amount = ...14) %>% mutate(frame = "Year 1980")
data_l1 <- filtereddatasetbar %>% select(Country = ...2, Substance = ...3, Amount = ...15) %>% mutate(frame = "Year 1981")
data_m1 <- filtereddatasetbar %>% select(Country = ...2, Substance = ...3, Amount = ...16) %>% mutate(frame = "Year 1982")
data_n1 <- filtereddatasetbar %>% select(Country = ...2, Substance = ...3, Amount = ...17) %>% mutate(frame = "Year 1983")
data_o1 <- filtereddatasetbar %>% select(Country = ...2, Substance = ...3, Amount = ...18) %>% mutate(frame = "Year 1984")
data_p1 <- filtereddatasetbar %>% select(Country = ...2, Substance = ...3, Amount = ...19) %>% mutate(frame = "Year 1985")
data_q1 <- filtereddatasetbar %>% select(Country = ...2, Substance = ...3, Amount = ...20) %>% mutate(frame = "Year 1986")
data_r1 <- filtereddatasetbar %>% select(Country = ...2, Substance = ...3, Amount = ...21) %>% mutate(frame = "Year 1987")
data_s1 <- filtereddatasetbar %>% select(Country = ...2, Substance = ...3, Amount = ...22) %>% mutate(frame = "Year 1988")
data_t1 <- filtereddatasetbar %>% select(Country = ...2, Substance = ...3, Amount = ...23) %>% mutate(frame = "Year 1989")
data_u1 <- filtereddatasetbar %>% select(Country = ...2, Substance = ...3, Amount = ...24) %>% mutate(frame = "Year 1990")
data_w1 <- filtereddatasetbar %>% select(Country = ...2, Substance = ...3, Amount = ...25) %>% mutate(frame = "Year 1991")
data_v1 <- filtereddatasetbar %>% select(Country = ...2, Substance = ...3, Amount = ...26) %>% mutate(frame = "Year 1992")
data_x1 <- filtereddatasetbar %>% select(Country = ...2, Substance = ...3, Amount = ...27) %>% mutate(frame = "Year 1993")
data_y1 <- filtereddatasetbar %>% select(Country = ...2, Substance = ...3, Amount = ...28) %>% mutate(frame = "Year 1994")
data_z1 <- filtereddatasetbar %>% select(Country = ...2, Substance = ...3, Amount = ...29) %>% mutate(frame = "Year 1995")
data_a2 <- filtereddatasetbar %>% select(Country = ...2, Substance = ...3, Amount = ...30) %>% mutate(frame = "Year 1996")
data_b2 <- filtereddatasetbar %>% select(Country = ...2, Substance = ...3, Amount = ...31) %>% mutate(frame = "Year 1997")
data_c2 <- filtereddatasetbar %>% select(Country = ...2, Substance = ...3, Amount = ...32) %>% mutate(frame = "Year 1998")
data_d2 <- filtereddatasetbar %>% select(Country = ...2, Substance = ...3, Amount = ...33) %>% mutate(frame = "Year 1999")
data_e2 <- filtereddatasetbar %>% select(Country = ...2, Substance = ...3, Amount = ...34) %>% mutate(frame = "Year 2000")
data_f2 <- filtereddatasetbar %>% select(Country = ...2, Substance = ...3, Amount = ...35) %>% mutate(frame = "Year 2001")
data_g2 <- filtereddatasetbar %>% select(Country = ...2, Substance = ...3, Amount = ...36) %>% mutate(frame = "Year 2002")
data_h2 <- filtereddatasetbar %>% select(Country = ...2, Substance = ...3, Amount = ...37) %>% mutate(frame = "Year 2003")
data_i2 <- filtereddatasetbar %>% select(Country = ...2, Substance = ...3, Amount = ...38) %>% mutate(frame = "Year 2004")
data_j2 <- filtereddatasetbar %>% select(Country = ...2, Substance = ...3, Amount = ...39) %>% mutate(frame = "Year 2005")
data_k2 <- filtereddatasetbar %>% select(Country = ...2, Substance = ...3, Amount = ...40) %>% mutate(frame = "Year 2006")
data_l2 <- filtereddatasetbar %>% select(Country = ...2, Substance = ...3, Amount = ...41) %>% mutate(frame = "Year 2007")
data_m2 <- filtereddatasetbar %>% select(Country = ...2, Substance = ...3, Amount = ...42) %>% mutate(frame = "Year 2008")
data_n2 <- filtereddatasetbar %>% select(Country = ...2, Substance = ...3, Amount = ...43) %>% mutate(frame = "Year 2009")
data_o2 <- filtereddatasetbar %>% select(Country = ...2, Substance = ...3, Amount = ...44) %>% mutate(frame = "Year 2010")


# Bind the data gathered
animated_bar_data <- rbind(data_a1, data_b1, data_c1, data_d1, data_e1, data_f1, data_g1, data_h1, data_i1, data_j1, data_k1, data_l1, data_m1,
                           data_n1, data_o1, data_p1, data_q1, data_r1, data_s1, data_t1, data_u1, data_w1, data_v1, data_x1, data_y1, data_z1,
                           data_a2, data_b2, data_c2, data_d2, data_e2, data_f2, data_g2, data_h2, data_i2, data_j2, data_k2, data_l2, data_m2,
                           data_n2, data_o2)

anim_bar_plot <- ggplot(animated_bar_data, aes(x = Country, y = Amount, fill = Substance)) +
  geom_col() +
  labs(title = 'Emissions: {closest_state}') + # Dynamic title
  transition_states(frame, transition_length = 2, state_length =  1) +
  ease_aes('sine-in-out')

emissions_by_stage <- read_excel("./datasets/EDGAR-FOOD_v61_AP.xlsx",sheet = "Suppl. Table 3-Emi by stage", skip = 2)
emissions_by_stage_clean <- emissions_by_stage %>%
  filter(!is.na(FOOD_system_compartment), !is.na(`2018`)) %>%
  group_by(FOOD_system_compartment) %>%
  slice_max(order_by = `2018`, n = 10) %>%
  ungroup()
boxplot_stage <- ggplot(emissions_by_stage_clean, aes(x=as.factor(FOOD_system_compartment), y=`2018`)) +
  geom_boxplot(fill="lightblue", alpha=0.2) +
  xlab("Compartments") +
  ylab("Kton Substances/Year")

interactive_boxplot_stage <- ggplotly(boxplot_stage)

############# ai graph dataframe for faster rerenders
# this line works sometime idk why
#emissions_by_industry <- read_excel("../datasets/EDGAR-FOOD_v61_AP.xlsx",sheet = "Suppl. Table 4-Emi by Country")

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


###################### UI 
sidebar <- dashboardSidebar(
  sidebarMenu(
    id = "tabs",
    menuItem("GWP Dashboard", tabName = "dashboard", icon = icon("chart-line")),
    # first arg is the exported variable name
    selectInput("selected_substance", "Choose a Substance:",
                list(`Monoxide` = list("CO"),
                     `Black Carbon` = list("BC"),
                     `Organic Carbon` = list("OC"),
                     `Nitrogen Oxide` = list("NOx")
                     )
                
  )
),
# --- CONDITIONAL CONTROLS FOR AI GRAPH TAB ONLY ---
conditionalPanel(
  condition = "input.tabset1 == 'AI Graph'",
  hr(),
  h4("AI Graph Controls", style = "margin-left: 20px;"),
  selectInput("ai_country", "Select Country:", 
              choices = sort(unique(ai_data$Name)), selected = "World"),
  checkboxGroupInput("ai_stages", "Food System Stages:",
                     choices = unique(ai_data$FOOD_system_stage),
                     selected = unique(ai_data$FOOD_system_stage)),
  sliderInput("ai_year_range", "Year Range:", 
              min = 1970, max = 2018, value = c(1990, 2018), sep = "")
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
                tabPanel(
                  title = "Animated bar chart emissions for four countries",
                  icon = icon("bar-chart"),
                  imageOutput("animated_barchart")
                ),
                tabPanel(
                  title = "GWP100 Equalized Sum Heat Map 2018",
                  icon = icon("chart-area"),
                  plotlyOutput("world_heat_map", height = "1000px")
                  
                ),
                tabPanel(
                  title = "substance emittance inside each country",
                  icon = icon("table"),
                  #plotlyOutput(substance)
                  plotlyOutput("emit_share")
                ),
                tabPanel(
                  title = "Regional substance emissions",
                  plotOutput("emit_gbarchart"),
                  div(
                    style = "border: 1px solid green; padding: 10px; margin-top: 10px;",
                    "The group emissions inside each region do not add up to 1, as these are the normalized food share of total caused emissions"
                  )
                ),
                tabPanel(
                  title = "Total Pollutant Emissions",
                  # App title ----
                  titlePanel("Select Year"),
                  
                  # Sidebar layout with input and output definitions ----
                  sidebarLayout(
                    
                    # Sidebar to demonstrate various slider options ----
                    sidebarPanel(
                      
                      # Input: Simple integer interval ----
                      sliderInput("year", "Year:",
                                  min = 1970, max = 2018,
                                  value = 1970, sep = NULL, 
                                  step = 1, animate = TRUE),
                      plotOutput("total_emissions_donut"),
                      selectInput("stage", "Select Stage for Substance Breakdown:", 
                                  c('Consumption', 'Distribution', 'End_of_Life', 'Processing', 'Production'),
                                  selected = 'Consumption')
                    ),
                    mainPanel(
                      plotOutput("total_emissions_bar"),
                      plotOutput("total_emissions_breakdown")
                    )
                  )
                ),
                tabPanel(
                  title = "Boxplot Food Compartment Emissions Top 10 Emittors",
                  icon = icon("chart-column"),
                  plotlyOutput("boxplot_stages")
                ),
                tabPanel(
                  title = "AI Graph", 
                  icon = icon("project-diagram"),
                  fluidRow(
                    column(6, plotlyOutput("sankeyPlot")),
                    column(6, plotlyOutput("aiTrendPlot"))
                  ),
                  fluidRow(
                    column(12, plotlyOutput("aiSharePlot"))
                  )
                ),
                
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
  
 
  output$emit_share <- renderPlotly({
    emission_share_graph(input$selected_substance)
  })
  
  output$animated_barchart <- renderImage({
    outfile <- tempfile(fileext = '.gif')
    anim <- animate(anim_bar_plot, nframes = 100, fps = 5,
                    width = 600, height = 400,
                    renderer = gifski_renderer())
    anim_save(outfile, animation = anim)
    list(src = outfile,
         contentType = 'image/gif',
         width = 600,
         height = 400,
         alt = "Animated emissions bar chart")
  }, deleteFile = TRUE)
  
  output$emit_gbarchart <- renderPlot({
    pub_emission_share_grouped_barchart()
  })
  
  output$total_emissions_bar <- renderPlot({
    emissionPerStepBar(input$year, sum_emissions_by_stage)
  })
  
  output$total_emissions_donut <- renderPlot({
    emissionPerStepDonut(input$year, sum_emissions_by_stage)
  })
  
  output$total_emissions_breakdown <- renderPlot({
    emissionBreakdown(input$year, breakdownData, input$stage)
  })
  
  output$world_heat_map <- renderPlotly({
    interactive_heat_map
  })
  
  output$boxplot_stages <- renderPlotly({
    interactive_boxplot_stage
  })
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
      split = ~emission_type,
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
        #legend = list(orientation = "w")
        showlegend = FALSE
        # show text hover in comparion mode by default. text box too big
        #hovermode = "x unified"
      )
  })
  
  # 1. Reactive filter for the AI Graph tab
  filtered_ai <- reactive({
    req(input$ai_country, input$ai_stages, input$selected_substance)
    ai_data %>%
      filter(
        Name == input$ai_country,
        Substance == input$selected_substance,
        FOOD_system_stage %in% input$ai_stages,
        Year >= input$ai_year_range[1],
        Year <= input$ai_year_range[2]
      )
  })
  
  # 2. Output for the Sankey Plot
  output$sankeyPlot <- renderPlotly({
    req(nrow(filtered_ai()) > 0)
    
    # Grab data for the latest year in the selected range
    df_sankey <- filtered_ai() %>%
      filter(Year == input$ai_year_range[2]) %>%
      group_by(FOOD_system_stage, Substance) %>%
      summarise(value = sum(Emissions_kton, na.rm = TRUE), .groups = 'drop')
    
    nodes <- data.frame(name = unique(c(df_sankey$FOOD_system_stage, df_sankey$Substance)))
    df_sankey$source <- match(df_sankey$FOOD_system_stage, nodes$name) - 1
    df_sankey$target <- match(df_sankey$Substance, nodes$name) - 1
    
    plot_ly(
      type = "sankey",
      node = list(label = nodes$name, pad = 15, thickness = 20),
      link = list(source = df_sankey$source, target = df_sankey$target, value = df_sankey$value)
    ) %>% layout(title = paste("Flow for", input$ai_year_range[2]))
  })
  
  # 3. Output for the Area Trend Plot
  output$aiTrendPlot <- renderPlotly({
    p <- ggplot(filtered_ai(), aes(x = Year, y = Emissions_kton, fill = FOOD_system_stage)) +
      geom_area(alpha = 0.8) +
      scale_fill_viridis_d() +
      theme_minimal()
    ggplotly(p)
  })
  
  # 4. Output for the Food Share Plot
  output$aiSharePlot <- renderPlotly({
    p <- ggplot(filtered_ai(), aes(x = Year, y = Food_Share, group = 1)) +
      geom_line(color = "#e74c3c", size = 1) +
      theme_minimal() +
      labs(y = "Food Share (Ratio)")
    ggplotly(p)
  })

}

# 4. RUN APP
shinyApp(ui, server)

