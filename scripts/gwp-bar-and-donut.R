# Bar and Donut charts for the total emissions of 4 different countries at 1970 and 2018.
# The type of emission being energy consumption emissions. 


#Bar chart 1970
ggplot(countries, aes(x=...2, y=...7)) + 
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "Set1") +
  theme(legend.position="none")


#Bar chart 2018
ggplot(countries, aes(x=...2, y=...50)) + 
  geom_bar(stat = "identity")


## Donut chart for 1970
# Compute percentages
countries$fraction = countries$...7 / sum(countries$...7)

# Compute the cumulative percentages (top of each rectangle)
countries$ymax = cumsum(countries$fraction)

# Compute the bottom of each rectangle
countries$ymin = c(0, head(countries$ymax, n=-1))

# Make the plot
ggplot(countries, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=...2)) +
  geom_rect() +
  coord_polar(theta="y") + # Try to remove that to understand how the chart is built initially
  xlim(c(2, 4)) # Try to remove that to see how to make a pie chart


## Donut chart for 2018
# Compute percentages
countries$fraction = countries$...50 / sum(countries$...50)

# Compute the cumulative percentages (top of each rectangle)
countries$ymax = cumsum(countries$fraction)

# Compute the bottom of each rectangle
countries$ymin = c(0, head(countries$ymax, n=-1))

# Make the plot
ggplot(countries, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=...2)) +
  geom_rect() +
  coord_polar(theta="y") + # Try to remove that to understand how the chart is built initially
  xlim(c(2, 4)) # Try to remove that to see how to make a pie chart

#For animated graphs
install.packages("gganimate")
library(gganimate)