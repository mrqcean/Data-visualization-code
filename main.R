list.of.packages <- c("ggplot2", "shiny","s2", "gganimate","dplyr","readxl","viridis", "plotly", "gifski")
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




##### emission share
# this contains
source("./scripts/emission-by-share.R")

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
countries <- slice(emissions_by_industry, 274,1191,1167,361)


filtereddatasetbar <- filter(
  emissions_by_industry,
  grepl('Denmark|Congo$|Singapore|El Salvador', ...2)
)

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



animated_bar_data <- rbind(data_a1, data_b1, data_c1, data_d1, data_e1, data_f1, data_g1, data_h1, data_i1, data_j1, data_k1, data_l1, data_m1,
                           data_n1, data_o1, data_p1, data_q1, data_r1, data_s1, data_t1, data_u1, data_w1, data_v1, data_x1, data_y1, data_z1,
                           data_a2, data_b2, data_c2, data_d2, data_e2, data_f2, data_g2, data_h2, data_i2, data_j2, data_k2, data_l2, data_m2,
                           data_n2, data_o2)

anim_bar_plot <- ggplot(animated_bar_data, aes(x = Country, y = Amount, fill = Substance)) +
  geom_col() +
  labs(title = 'Emissions: {closest_state}') + # Dynamic title
  transition_states(frame, transition_length = 2, state_length =  1) +
  ease_aes('sine-in-out')





sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("GWP Dashboard", tabName = "dashboard", icon = icon("chart-line")),
    # first arg is the exported variable name
    selectInput("selected_substance", "Choose a Substance:",
                list(`East Coast` = list("NY", "NJ", "CT"),
                     `West Coast` = list("WA", "OR", "CA"),
                     `Midwest` = list("MN", "WI", "IA")),
                textOutput("result")
  )
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
                  imageOutput("animated_barchart")
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
  
  output$result <- renderText({
    paste("You chose", input$selected_substance)
  })
  
  output$animated_barchart <- renderImage({
    outfile <- tempfile(fileext = '.gif')
    anim <- animate(anim_bar_plot, nframes = 200, fps = 10,
                    width = 600, height = 400,
                    renderer = gifski_renderer())
    anim_save(outfile, animation = anim)
    list(src = outfile,
         contentType = 'image/gif',
         width = 600,
         height = 400,
         alt = "Animated emissions bar chart")
  }, deleteFile = TRUE)

}

# 4. RUN APP
shinyApp(ui, server)

