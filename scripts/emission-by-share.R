# first 2 rows are skipped as they are empty 


# import page with all country types, used for filtering countries out when doing a graph
country_classification <- read_excel("./datasets/EDGAR-FOOD_v61_AP.xlsx",sheet = "Suppl. Table 2- countries",skip = 1)
filtereddataset <- country_classification %>%
  filter(grepl('Europe', .data[["Regional grouping"]]))



# filter by eu contries
# convert to 1,n vector, we chose the country name as the column
# this can also be done in base R
countries_filter <- dplyr::pull(filtereddataset, `Country name`)

emissions_by_share <- read_excel("./datasets/EDGAR-FOOD_v61_AP.xlsx",sheet = "Suppl. Table 5 - FOOD shares",skip = 2)
#emisisons_by_share_stacked <- function(data) {
# remove climate pollution stats, pm10,pm2.5, VOC and keep green house gas affecting gasses.
emissions_by_share <- emissions_by_share %>%
  filter(Name %in% countries_filter)
# i am going to make 3 graphs instead of 1 which can switc(eyes over memory...)
# which just 3 wrapper functions passing argument "Substance" to main function



#emissions_by_share <- emissions_by_share %>%
#  filter(Substance %in% c("OC", "BC", "NOx", "CO"))
# just use a var as we only have 4 values, instead of implementing enums in R
# selected is set with a function inside server

# the most nice substance is gwp relevant such as OC,BC,NOx, and CO
internal_functiongen <- function(selected_substance){
# ensure an argument is a string
#stopifnot(is.character(internal_functiongen))



# just going to use 

data <- filter(emissions_by_share,Substance %in% c(selected_substance))
groups <- select(data,"Name")
# groups need to be 1xentry_ammount to be used as groups argument
groups <- t(groups)
# remove all columns not holding temporal data,  
# idk why you have to explicitly name the specific rows.
data <- data[-c(1,2, 3)]
#times is the timestamps/years
years <- names(data)
# convert all rows to vector

# collapse by rows, ie row 2 comes after row 1.
# idk why t is needed https://stackoverflow.com/questions/2545228/convert-a-dataframe-to-a-vector-by-rows
values <- as.vector(t(data))

## transforming data for input into plot function
# we use a stacked area graph as we have over 200 groups and want to show the timeseries evolution over time

# below has also the duplicated for OC, BC,NOx,CO
# it already has for above 200 counries, so Should be best to get a general overview so square graph it is 
# should probably split them up and make a graph for each substance, or make it automatically add these together
# and make dots on sideboard as to which to add to the whole



#len_column <- as.numeric(length(x))
# for each country has times ammount of data entries 
#multiplier <- as.numeric(length(times))
 
 #times <- length(groups)
#groups are countries, eg a row, 
# y values are the from the 3 third columns in that country 

# extract the numbers for each row and add onto a vector
# replicate groups x timeslots should be equal to how many datapoints we have
# stacked area graph is geom apparently
groupfactor <- length(values)/length(groups) # was 49 at some point
  values
year_factor <- length(values)/length(years) # was 218 at some point
  

plotdata <- data.frame(
  day = as.numeric(as.character(rep(years, times = year_factor))),
  amount = values,
  category = rep(groups, each = groupfactor)
)
# reorder by highest emitter
# do not remove NA values as we want to keep the table alignment the same
#plotdata <- plotdata %>%
 # mutate(category = fct_reorder(category, amount, .fun = sum, .desc = TRUE,.na_rm = FALSE))

# # year we should have each wave have the label
# ggplot(plotdata, aes(x=as.numeric(day), y=as.numeric(amount), fill=category)) + 
#   # The labels take up too much space, but if we remove them then the viewer has no Idea what they are
#   geom_area(show.legend = FALSE)+
#   labs(
#     title = "Stacked Area Chart Example",
#     x = "Year",
#     y = selected_substance,
#     fill = "Category"
#   ) +
#   theme_minimal()
# 
# 
# 
# 
# library(ggplot2)
# library(plotly)

# 1. Build the ggplot object
# We add a 'text' aesthetic to customize what shows up in the hover box
p <- ggplot(plotdata, aes(x = as.numeric(day), 
                          y = amount, 
                          color = category, # argument needed for coloring line graph
                          #fill = amount,
                          # This creates the custom hover label
                          text = paste("Country:", category, "<br>Year:", years, "<br>Value:", amount))) + 
  geom_line(aes(group = category)) + # Explicitly group by category
  theme_minimal() +
  # Hide the giant legend
  theme(legend.position = "none",
        # set the theme ready for different elements of graph
        # this kills axis
        #axis.line = element_blank(), 
        #axis.text = element_blank(),
        #      axis.ticks = element_blank(), 
        #axis.title = element_blank()
        ) +
          scale_fill_viridis(option="magma") + 

  labs(title = "evolution of each country's share of the Source",
       x = "Year",
       # concat to say, substance share of emitted substances for own country
       # internal country share of selected_substance
       y = selected_substance)


# 2. Convert the ggplot to an interactive plotly object
# 'tooltip' tells plotly to only show the custom 'text' we created above
interactive_plot <- ggplotly(p, tooltip = "text")

# 3. View the plot
#interactive_plot

return(interactive_plot)
}
# library(ggplot2)
# library(plotly)
# # TODO step over colors instead of using closest such that they bleed over
# # modify data in real time based on who becomes top, or just make it an animated graph
# # 1. Build the static ggplot object
# 
# fig <- 
# p <- ggplot(plotdata, aes(x = as.numeric(day), 
#                           y = amount, 
#                           fill = category,
#                           # Custom hover text
#                           text = paste("Country:", category, 
#                                        "<br>Year:", day, 
#                                        "<br>Value:", format(amount, big.mark=",")))) + 
#   geom_area(aes(group = category,)) +
#   # THE FIX: Invisible points to anchor the tooltips
#   # text only takes one vector as argument it does not dynamically update the other args, 
#   #this is why year, value are not updated.
#   geom_point(aes(text = paste("Country:", category, 
#                               "<br>Year:", day, 
#                               "<br>Value:", round(amount, 2))),alpha = 0,
#              # 2f means 2 decimals right
#              hovertemplate = paste('<i>Price</i>: $%{amount:.2f}',
#                         '<br><b>X</b>: %{day}<br>',
#                         '<b>%{text}</b>')
#   ) +
#   theme_minimal() +
#   theme(legend.position = "none") + # Keep the legend hidden so the graph has space
#   labs(title = "Global Trends (Zoomable)",
#        x = "Year",
#        y = selected_substance)
# 
# # 2. Convert to plotly and add the Range Slider
# interactive_plot <- ggplotly(p, tooltip = "text") %>%
#   layout(
#     xaxis = list(
#       rangeslider = list(type = "date"), # Adds the zoom slider at the bottom
#       rangeselector = list(
#         buttons = list(
#           list(count = 20, label = "10y", step = "year", stepmode = "backward"),
#           list(step = "all")
#         )
#       )
#     )
#   )
# 
# # 3. Render the plot
# interactive_plot

plot_CO <- internal_functiongen("CO")
plot_OC <- internal_functiongen("OC")
plot_NOx <- internal_functiongen("NOx")
plot_BC <- internal_functiongen("BC")
plot_CO
plot_BC
plot_OC
plot_NOx
