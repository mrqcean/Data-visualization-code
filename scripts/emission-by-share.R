# first 2 rows are skipped as they are empty 


# import page with all country types, used for filtering countries out when doing a graph
country_classification <- read_excel("./datasets/EDGAR-FOOD_v61_AP.xlsx",sheet = "Suppl. Table 2- countries",skip = 1)
# a more efficient way to sort rows my column values inside the name of regional grouping 
# filter by eu contries
# not going to rename filtered dataset to europe
# this is multiple regions so might have to be cut down
# removing greenland as all substances have the same value from  1997 to 2003
# get all countries but not Greenland
country_classification <- country_classification %>%
  filter(!grepl("Greenland", `Country name`))


# convert to 1,n vector, we chose the country name as the column
# this can also be done in base R
countries_filter <- dplyr::pull(filtereddataset, `Country name`)

# the dataset containing the food shares for inside each own country, I does not add up to 1 globally
emissions_by_share <- read_excel("./datasets/EDGAR-FOOD_v61_AP.xlsx",sheet = "Suppl. Table 5 - FOOD shares",skip = 2)

######## WHICH REGION RELIES ON WHAT EMISSIONS THE MOST BAR CHART


# hvor er mine macroer'er :( så skulle jeg ikke assigne vars manuelt
# this returns vectors 1,n
get_regiondata <- function(region_string) {
  return( 
    country_classification %>% filter(grepl(region_string, .data[["Regional grouping"]]))
                                      # convert the column Country name, of piped in db to a vector
                                      %>%dplyr::pull(`Country name`)
                                      
  )
}

# print is needed to allow unique() to not head
#print(unique(country_classification[, 3]), n = Inf)
# very big includes 25 countries each maybe cut as region is too wide
#db_Western_Africa <- get_regiondata('Western_Africa')
#db_Southern_Africa <- get_regiondata('Southern_Africa')

db_Middle_East <- get_regiondata('Middle_East')
db_Southeastern_Asia <- get_regiondata('Southeastern Asia')
db_Oceania <- get_regiondata('Oceania')
db_Rest_Central_America <- get_regiondata('Rest Central America')
db_Rest_South_America <- get_regiondata('Rest South America')
# generate for average region and substance, then use that value in a bar chart with the other regions
  # So we have

# regions is a 1,n vector


f_internal_substance <- function(substance){
  filtered <- emissions_by_share %>% filter(Substance %in% c(substance))
  # filter by substance,  
  
  # filter share by region countries, and return the median for 2018
  f_internal_get_med_region <- function(db_region) {
    return(
      # then get the year 2018 column as 1,n vector and take the median of that
    filter(filtered, Name %in% db_region) %>% pull("2018") %>% median()
    )
  }
  
  
  # we only have 1 var per region, ensure that the data frame is created before 
  db_list <- list(
    "Western Africa" = db_Western_Africa,
    "Southeastern Asia" = db_Southeastern_Asia,
    "Rest Central America" = db_Rest_Central_America,
    "Northern Africa" = db_Northern_Africa,
    "Middle East" = db_Middle_East,
    "Oceania" = db_Oceania,
    "Rest of South America" = db_Rest_South_America 
  )
  
  return ( data.frame(
    region = names(db_list),
    value  = sapply(db_list, f_internal_get_med_region)
  )
  )
  
}

f_internal_substance("BC")
f_internal_substance("OC")


# combine again and sort by country
# how do i make the gap for each

eys# why are lambda functions not a thing >:(
# calc median to find what value is the most reprensentitive

ggplot(df, aes(x=region, y=value,fill=region)) + 
  geom_bar(stat = "identity") +
  # maybe use another color for bars such that text is more visible
  geom_text(aes(label = round(value,3)), vjust = -0.5) +
  coord_flip()


####### START OF EUROPE LINE GRAPH

# OLD just an idea remove climate pollution stats, pm10,pm2.5, VOC and keep green house gas affecting gasses.
  
  # get what countries are in eastern europe, oecd europe, midde europe and so on.
  filtereddataset <- country_classification %>%
  filter(grepl('Europe', .data[["Regional grouping"]]))
# filter emssions rows for only countries that are in Europe
emissions_by_share <- emissions_by_share %>%
  filter(Name %in% countries_filter)




#  filter(Substance %in% c("OC", "BC", "NOx", "CO"))
# just use a var as we only have 4 values, instead of implementing enums in R
# selected substances is set with the a drop down menu sidebar inside server, 
#which achieves the same as an enum just graphically

# the most nice substance is gwp relevant such as OC,BC,NOx, and CO
emission_share_graph <- function(selected_substance){
# ensure an argument is a string
#stopifnot(is.character(internal_functiongen))



# just going to use 

data <- filter(emissions_by_share,Substance %in% c(selected_substance))
groups <- select(data,"Name")
# groups need to be 1xentry_ammount to be used as groups argument
groups <- t(groups)
# remove all columns not holding temporal data,  
# idk why you have to explicitly name the specific rows, I tried using a single number but did not work.
data <- data[-c(1,2, 3)]
#times is the timestamps/years
years <- names(data)
# convert all rows to vector

# collapse by rows, ie row 2 comes after row 1.
# idk why t is needed https://stackoverflow.com/questions/2545228/convert-a-dataframe-to-a-vector-by-rows
values <- as.vector(t(data))

## transforming data for input into plot function

#groups are countries, eg a row, 
# y values are the from the 3 third columns in that country 


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


# 1. Build the ggplot object
# We add a 'text' aesthetic to customize what shows up in the hover box
p <- ggplot(plotdata, aes(x = as.numeric(day), 
                          y = amount, 
                          color = category, # argument needed for coloring line graph
                          #fill = amount,
                          # This creates the custom hover label
                          text = paste("Country:", category, "<br>Year:", years, "<br>Value:", amount))) +
  geom_point(size=1,shape=1,alpha=0.7,aes(colour = category))+
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
  scale_color_viridis(discrete=TRUE) + 

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

#plot_CO <- internal_functiongen("CO")
#plot_OC <- internal_functiongen("OC")
#plot_NOx <- internal_functiongen("NOx")
#plot_BC <- internal_functiongen("BC")

#plot_CO

# who is burning trees or coal or peat
#plot_BC

# who is planting trees
#plot_OC

# combusting of coal and oil
#plot_NOx

