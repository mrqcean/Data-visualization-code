# this defines the function the create the graph which is imported in main.r in root dir

gwplinegraph <- function(dataset) {
# we only use the data from ar6 as that is the newest iteration of the gwp estimations 
  library(tidyverse)
  library(stringr)
  if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
  library(ggplot2)
  # TODO REMOVE TEMP
  dataset <- read_excel("./datasets/IPCC_AR4-AR6_GWPs.xlsx",sheet = "Main")
  # our dataset contains the gasses of NH3 OC PM10 PM2.5 BC NOx NMVOC SO2 C
  # the database uses their chemical names, 
  
  # R
  filtereddataset <-   filtereddataset <- filter(
    dataset,
    # NOx is substituted with NO as no2 is not included in dataset
    # added methane CH4 and use $ to say end of line in regex
    # NMVOC are health hazard polutants as such not included in gwp dataset
    # pm are air pollutant clusters.
    # SO2 does not exist, Sulfur hexflouride exists
    # oc 
    grepl('NH3|OC|BC|Nitrous oxide|SO2|Carbon dioxide|Methane$', GHG)
  )
 
  #sort dataframe by col, then sort by indicator
  sort.df <- filtereddataset %>%
    arrange(GHG, str_sort(Indicator, numeric = TRUE))
  colnames(filtereddataset)
  
  # get 20,100,500 for each gwp preferebly from AR6 iteration
  CO <- slice(sort.df,6,7,8)
  methane <- slice(sort.df, 14,15,16)
  NO <- slice(sort.df,22,23,24)
  emissions <- rbind(CO,methane,NO)
  
  
  # todo fix up
  data<-data.frame(Study_ID=c("a","a","a","200","200","200","300","300","300"),time_point=c("GWP20","GWP100","GWP500","GWP20","GWP100","GPW500","GWP20","GWP100","GWP500"),value=emissions$`GWP kgCO2e/kg GHG`)
  
  ggplot(data, aes(time_point, value, group = Study_ID, color = Study_ID)) + 
    geom_point() + 
    geom_line()
  
  

  
  data <- data.frame(
    Study_ID = c("a", "a", "a", "200", "200", "200", "300", "300", "300"),
    time_point = c("GWP20", "GWP100", "GWP500", "GWP20", "GWP100", "GWP500", "GWP20", "GWP100", "GWP500"),
    value = emissions$`GWP kgCO2e/kg GHG`
  )
  
  # Convert 'time_point' to a factor and explicitly set the order of levels. 
  # The original code had a repeated level ("GWP500").
  # This version correctly lists the three unique levels in the desired order: GWP20, GWP100, GWP500.
  data$time_point <- factor(
    data$time_point, 
    levels = c("GWP20", "GWP100", "GWP500") 
  )
  
  # 3. Generate the plot
  library(ggplot2)
  
  ggplot(data, aes(x = time_point, y = value, group = Study_ID, color = Study_ID)) +
    geom_point() +
    geom_line()
}

