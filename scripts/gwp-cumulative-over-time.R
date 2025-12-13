gwp_linegraph <- function(dataset) {
  library(tidyverse)
  library(stringr)
  
  # Filter the dataset for specific gases
  filtereddataset <- dataset %>%
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
  #they are are a bit old being from 2009 but 
  black_carbon_values <- c(1600, 460, 140)
  organic_carbon_values <- c(-240, -69, -21)
  
  # --- FIX STARTS HERE ---
  
  # 1. Extract the value column from the sliced data frames
  CO_values <- CO_data$`GWP kgCO2e/kg GHG`
  methane_values <- methane_data$`GWP kgCO2e/kg GHG`
  NO_values <- NO_data$`GWP kgCO2e/kg GHG`
  
  # 2. Combine all values into a single vector
  all_values <- c(CO_values, methane_values, NO_values, 
                  black_carbon_values, organic_carbon_values)
  
  # Create a clean dataframe for plotting
  # The length of the 'Gas' and 'time_point' vectors must match the length of 'all_values'
  data <- data.frame(
    Gas = rep(c("Carbon Dioxide", "Methane", "Nitrous Oxide", "Black Carbon", "Organic Carbon"), each = 3),
    time_point = rep(c("GWP20", "GWP100", "GWP500"), times = 5), # Corrected 'times = 3' to 'times = 5'
    value = all_values # Use the combined vector
  )
  
  # --- FIX ENDS HERE ---
  
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
