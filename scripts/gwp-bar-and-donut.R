librariesAndDataBarDonut <- function() {
  library(shiny)
  library(dplyr)
  library(ggplot2)
  library(readxl)
  
  stage_emissions <- read_excel("./datasets/EDGAR-FOOD_v61_AP.xlsx",sheet = "Suppl. Table 3-Emi by stage",skip = 2)
  
  #Filter total kton pollutants for each stage:
  sum_emissions_by_stage <- stage_emissions %>%
    filter(!is.na(FOOD_system_stage)) %>%
    group_by(FOOD_system_stage) %>%
    summarize(
      across(`1970`:`2018`, sum, na.rm = TRUE)
    )
  
  #Filter pollutants for gwp conversion:
  pollutants_filtered <- stage_emissions %>%
    filter(Substance %in% c("OC", "BC", "NOx", "CO"))
  
}

emissionPerStepBar <- function(year) {
  #year = 2018
  year = as.character(year)
  bardata <- select(sum_emissions_by_stage, FOOD_system_stage, year)
  p <- ggplot(bardata, aes(x = FOOD_system_stage, y = .data[[year]], fill = FOOD_system_stage)) + 
    geom_col() + 
    geom_text(
      aes(label = scales::comma(.data[[year]])),
      vjust = -0.3,
      size = 3
    ) +
    scale_y_continuous(labels = scales::comma) +
    labs(
      title = paste("Emissions by Food System Stage in", year),
      x = NULL,
      y = "Total Pollutant Emissions (kilotonnes)",
      fill = "Food System Stage"
    ) + theme(
      legend.position = c(.05, .95),
      legend.justification = c("left", "top"),
      legend.box.just = "left",
      legend.margin = margin(6, 6, 6, 6)
    )
  #p
  return(p)
}


emissionPerStepDonut <- function() {
  
  
}



# Bar and Donut charts for the total emissions of 4 different countries at 1970 and 2018.
# The type of emission being energy consumption emissions. 


# #Bar chart 1970
# ggplot(countries, aes(x=...2, y=...7)) + 
#   geom_bar(stat = "identity") +
#   scale_fill_brewer(palette = "Set1") +
#   theme(legend.position="none")
# 
# 
# #Bar chart 2018
# ggplot(countries, aes(x=...2, y=...50)) + 
#   geom_bar(stat = "identity")
# 
# 
# ## Donut chart for 1970
# # Compute percentages
# countries$fraction = countries$...7 / sum(countries$...7)
# 
# # Compute the cumulative percentages (top of each rectangle)
# countries$ymax = cumsum(countries$fraction)
# 
# # Compute the bottom of each rectangle
# countries$ymin = c(0, head(countries$ymax, n=-1))
# 
# # Make the plot
# ggplot(countries, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=...2)) +
#   geom_rect() +
#   coord_polar(theta="y") + # Try to remove that to understand how the chart is built initially
#   xlim(c(2, 4)) # Try to remove that to see how to make a pie chart
# 
# 
# ## Donut chart for 2018
# # Compute percentages
# countries$fraction = countries$...50 / sum(countries$...50)
# 
# # Compute the cumulative percentages (top of each rectangle)
# countries$ymax = cumsum(countries$fraction)
# 
# # Compute the bottom of each rectangle
# countries$ymin = c(0, head(countries$ymax, n=-1))
# 
# # Make the plot
# ggplot(countries, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=...2)) +
#   geom_rect() +
#   coord_polar(theta="y") + # Try to remove that to understand how the chart is built initially
#   xlim(c(2, 4)) # Try to remove that to see how to make a pie chart
# 
# #For animated graphs
# install.packages("gganimate")
# library(gganimate)