# first 2 rows are skipped as they are empty 
emissions_by_share <- read_excel("./datasets/EDGAR-FOOD_v61_AP.xlsx",sheet = "Suppl. Table 5 - FOOD shares",skip = 2)
emisisons_by_share_stacked <- function(data) {
# remove climate pollution stats, pm10,pm2.5, VOC and keep green house gas affecting gasses.

emissions_by_share <- emissions_by_share %>%
  filter(Substance %in% c("OC", "BC", "NOx", "CO"))
# just use a var as we only have 4 values, instead of implementing enums in R
# selected is set with a function inside server
# TODO make the function and datatype correct, to allow setting var outside script file
#substance_selected <- c()
#setselected <- function(substance_string){
  # values should be validated inside this function but will just be used by server.
#  possible_values <- c(  "OC","BC","NOx","CO") 
#  substance_selected <- c(substance_string)
#}
#setselected("OC")

data <- filter(emissions_by_share,Substance %in% c("CO"))
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
multiplier <- as.numeric(length(times))
 
 #times <- length(groups)
#groups are countries, eg a row, 
# y values are the from the 3 third columns in that country 

# extract the numbers for each row and add onto a vector
# replicate groups x timeslots should be equal to how many datapoints we have
# stacked area graph not geom
# TODO LOOK at how i did the replications in linear
# match orientation, each var should be 1 column with values ammount of rows.
length(values)/length(groups)
  
length(values)/length(years)
  
length(values)
  

plotdata <- data.frame(
  day = rep(years, times = 218),
  amount = values,
  category = rep(groups, each = 49)
)


# year we should have each wave have the label
ggplot(plotdata, aes(x=as.numeric(day), y=as.numeric(amount), fill=category)) + 
  # The labels take up too much space, but if we remove them then the viewer has no Idea what they are
  geom_area(show.legend = FALSE)+
  labs(
    title = "Stacked Area Chart Example",
    x = "Year",
    y = "Value",
    fill = "Category"
  ) 
  +
  theme_minimal()

}
