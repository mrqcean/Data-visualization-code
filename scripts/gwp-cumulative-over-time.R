gwp_linegraph <- function(dataset) {
  library(tidyverse)
  library(stringr)
  
  # Filter the dataset for specific gases
  # Note: regex '$' ensures we don't accidentally get partial matches
  filtereddataset <- dataset %>%
    filter(grepl('NH3|OC|BC|Nitrous oxide|SO2|Carbon dioxide|Methane$', GHG))
  
  # Sort to ensure consistent slicing
  sort.df <- filtereddataset %>%
    arrange(GHG, str_sort(Indicator, numeric = TRUE))
  
  # Slice the data (ensure indices 6-8, 14-16, and 22-24 exist in your specific Excel file)
  CO_data <- slice(sort.df, 6, 7, 8)
  methane_data <- slice(sort.df, 14, 15, 16)
  NO_data <- slice(sort.df, 22, 23, 24)
  
  emissions <- rbind(CO_data, methane_data, NO_data)
  
  # Create a clean dataframe for plotting
  # We repeat the Gas Name 3 times for each gas (for GWP20, 100, 500)
  data <- data.frame(
    Gas = rep(c("Carbon Dioxide", "Methane", "Nitrous Oxide"), each = 3),
    time_point = rep(c("GWP20", "GWP100", "GWP500"), times = 3),
    value = emissions$`GWP kgCO2e/kg GHG`
  )
  
  # Ensure the time points stay in chronological order on the X-axis
  data$time_point <- factor(data$time_point, levels = c("GWP20", "GWP100", "GWP500"))
  
  # Create the plot
  plot <- ggplot(data, aes(x = time_point, y = value, group = Gas, color = Gas)) +
    geom_point(size = 3) + 
    geom_line(linewidth = 1) +
    labs(
      title = "Global Warming Potential (GWP) Trends in years",
      x = "Years from emission",
      y = "CO2e / Greenhouse gas",
      color = "Greenhouse Gas"
    ) +
    theme_minimal()
  return(plot)
}

