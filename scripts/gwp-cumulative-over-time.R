gwp_linegraph <- function(data) {

  

  
  
  
  
  
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
