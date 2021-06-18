library(tidyverse)
library(ggplot2)
library(fmsb)



rental_data <- read_csv("C:\\Working\\github\\data\\immo_data.csv")
population_data <- read_csv("C:\\Working\\github\\mibma_rentals_germany\\data\\population.csv")
berlin_population_data <- read_csv("C:\\Working\\github\\mibma_rentals_germany\\data\\berlin_population.csv")

head(rental_data)
print(nrow(rental_data))
print(head(rental_data, 10))

# x <- c(0:10)
# plot(x, sin(x))

# First row and all columns
print(rental_data[1,])

# Print region2 column 
print(rental_data[,"regio2"])

# Get all cities of Germany
regio2 <- unique(rental_data[,"regio2"])

print(typeof(regio2))
regions <-c(regio2)
print(regions[])
print(typeof(rental_data))

# count by region2
rent_by_region <- rental_data %>% count(regio2, sort = TRUE)
rent_by_region

average_price_by_region <- rental_data %>% mean(regio2,)

grep("totalRent", colnames(rental_data))
rental_data[, 11]
totalRent_dat <- filter(rental_data, ! is.na(totalRent))
regio2_everage_price <- aggregate(totalRent_dat[, 11], list(totalRent_dat$regio2), mean)
regio2_everage_price

# count top 10
rent_by_region_top_10 <- rent_by_region[1:10,]
#rent_by_region_top_10 <- c(rent_by_region[1:10,1:2])
print(rent_by_region_top_10)

#sort rent data by city name (region)
rent_by_region_top_10 <- rent_by_region_top_10[order(rent_by_region_top_10$regio2),]
print()
region_count <- c(rent_by_region_top_10[,2]$n)
region_count



#region names top 10 regions
region_names <-  c(rent_by_region[1:10,1]$regio2)
region_names <- region_names[order(region_names)]
region_names


filter_pop <- population_data %>% filter(population_data$city %in% region_names)
filter_pop

# sort population data by city name
filter_pop_order <- filter_pop[order(filter_pop$city),]
filter_pop_order

population_by_city <- c(filter_pop$population)
population_by_city

grep("totalRent", colnames(rental_data))
rental_data[, 11]
totalRent_dat <- filter(rental_data, ! is.na(totalRent))
regio2_everage_price <- aggregate(totalRent_dat[, 11], list(totalRent_dat$regio2), mean)
regio2_everage_price <- regio2_everage_price %>% filter(regio2_everage_price$Group.1 %in% region_names)
regio2_everage_price[,2]


# create a new dataset
data <- data.frame(region_names,region_count, population_by_city, regio2_everage_price[,2])
data
class(data)
data_sort_by_region <- data[order(region_names),]

#graph by region
ggplot(data, aes( y= region_count, group = 1)) +
  geom_line(color="red")

# graph by population per city
ggplot(filter_pop_order, aes(x=region_names, y= population_by_city, group = 1)) +
  geom_line(color="green")

# transform population_by_city to rate by 100 to match with available apartment
data <- transform(data, y = population_by_city / 100, price = regio2_everage_price...2.* 10)
data

ggplot(data) + 
  geom_line(aes(x=region_names,y = y, group = 1), lwd=1.5, color ="red") + 
  geom_line(aes(x=region_names,y = region_count, group = 2), lwd=1.5, color = "green") + 
  theme(legend.position="none",plot.title = element_text(hjust = 0.5)) +
  ggtitle("Avaiable apartments (Green) vs Population (Red)")

ggplot(data) + 
  geom_line(aes(x=region_names,y = region_count, group = 1), lwd=1.5, color = "green") + 
  geom_line(aes(x=region_names,y = price, group = 3), lwd=1.5, color = "purple") + 
  
  theme(legend.position="none",plot.title = element_text(hjust = 0.5)) +
  
  ggtitle("Avaiable apartments (Green) vs Price")

ggplot(data) + 
  geom_line(aes(x=region_names,y = y, group = 1,), lwd=1.5, color ="red") + 
  
  geom_line(aes(x=region_names,y = price, group = 3), lwd=1.5, color = "purple") + 
  
  theme(legend.position="none",plot.title = element_text(hjust = 0.5)) +
  
  ggtitle("Population (Red) vs Price")


ggplot(data) + 
  geom_line(aes(x=region_names,y = y, group = 1,), color ="red") + 
  geom_line(aes(x=region_names,y = region_count, group = 2), color = "green") + 
  geom_line(aes(x=region_names,y = price, group = 3), color = "purple") + 
  
  theme(legend.position="none",plot.title = element_text(hjust = 0.5)) +
  
  ggtitle("Avaiable apartments (Green) vs Population (Red) vs Price")


ggplot(data) + 
  geom_line(aes(x=region_names,y = y, group = 1,), color ="red") + 
  
  geom_line(aes(x=region_names,y = price, group = 3), color = "purple") + 
  
  theme(legend.position="none",plot.title = element_text(hjust = 0.5)) +
  
  ggtitle("Population (Red) vs Price")
data
data[1,2]
data[5,1]
#Radar chart
radar_data <- data.frame(
  row.names = c("count","population","price"),
  Berlin = c(data[1,2], data[1,5], data[1,6]),
  Chemnitz = c(data[2,2], data[2,5], data[2,6]),
  Dresden = c(data[3,2], data[3,5], data[3,6]),
  Essen = c(data[4,2], data[4,5], data[4,6]),
  Frankfurt = c(data[5,2], data[5,5], data[5,6]),
  Halle_Saale = c(data[6,2], data[6,5], data[6,6]),
  Hamburg = c(data[7,2], data[7,5], data[7,6]),
  Leipzig = c(data[8,2], data[8,5], data[8,6]),
  Magdeburg = c(data[9,2], data[9,5], data[9,6]),
  Munchen = c(data[10,2], data[10,5], data[10,6])

)

max_min <- data.frame(
  Berlin = c(40000, 0), Chemnitz = c(40000, 0),
  Dresden = c(40000, 0), Essen = c(40000, 0), Frankfurt = c(40000,0),
  Halle_Saale = c(40000, 0), Hamburg = c(40000, 0), Leipzig = c(40000,0),
  Magdeburg = c(40000, 0), Munchen = c(40000, 0)
)
rownames(max_min) <- c("Max", "Min")
df <- rbind(max_min, radar_data)
df

student1_data <- df[c("Max", "Min", "count"), ]
radarchart(student1_data)

#main reports about price vs population vs count
op <- par(mar = c(1, 2, 2, 1))
create_beautiful_radarchart(data = df, caxislabels = c(0, 10000, 20000, 40000),
                            color = c("#00AFBB", "#E7B800", "#FC4E07"))
# Add an horizontal legend
legend(
  x = "top", legend = rownames(df[-c(1,2),]), horiz = TRUE,
  bty = "n", pch = 20 , col = c("#00AFBB", "#E7B800", "#FC4E07"),
  text.col = "black", cex = 1, pt.cex = 1.5
)
par(op)

radar_data

create_beautiful_radarchart <- function(data, color = "#00AFBB", 
                                        vlabels = colnames(data), vlcex = 0.7,
                                        caxislabels = NULL, title = NULL, ...){
  radarchart(
    data, axistype = 1,
    # Customize the polygon
    pcol = color, pfcol = scales::alpha(color, 0.5), plwd = 2, plty = 1,
    # Customize the grid
    cglcol = "grey", cglty = 1, cglwd = 0.8,
    # Customize the axis
    axislabcol = "grey", 
    # Variable labels
    vlcex = vlcex, vlabels = vlabels,
    caxislabels = caxislabels, title = title, ...
  )
}

create_beautiful_radarchart(radar_data, caxislabels = c(0, 10000, 20000, 40000))
colors <- c("#00AFBB", "#E7B800", "#FC4E07")
titles <- c("Student.1", "Student.2", "Student.3")
for(i in 1:3){
  create_beautiful_radarchart(
    data = radar_data[c(1, 2, i+2), ],  caxislabels = c(0, 2000, 5000, 10000, 40000),
    color = colors[i], title = titles[i]
  )
}

# Reduce plot margin using par()
op <- par(mar = c(1, 2, 2, 2))
# Create the radar charts
create_beautiful_radarchart(
  data = radar_data, 
  color = c("#00AFBB", "#E7B800", "#FC4E07")
)
# Add an horizontal legend
legend(
  x = "bottom", legend = rownames(df[-c(1,2),]), horiz = TRUE,
  bty = "n", pch = 20 , col = c("#00AFBB", "#E7B800", "#FC4E07"),
  text.col = "black", cex = 1, pt.cex = 1.5
)
par(op)

student1_data <- radar_data[c("count", "population", "price"), ]
radarchart(student1_data)

rownames(radar_data) <- c("count", "population", "price")


radarchart(radar_data)

op <- par(mar = c(1, 2, 2, 1))
create_beautiful_radarchart(radar_data, caxislabels = c(0, 5000, 10000, 15000, 200000))
par(op)

#Rental type
# Get all cities of Germany
typeOfFlat <- unique(rental_data[,"typeOfFlat"])

print(typeOfFlat)


# count by region2
rent_by_type_flat <- rental_data %>% filter( ! is.na(typeOfFlat)) %>% count(typeOfFlat, sort = TRUE)
rent_by_type_flat

options(scipen=10000)

ggplot(data=rent_by_type_flat, aes(x=typeOfFlat, y=n)) +
  geom_bar(stat="identity", fill="steelblue")

# type of heating
rent_by_type_heating <- rental_data  %>% filter( ! is.na(heatingType)) %>% count(heatingType, sort = TRUE)
rent_by_type_heating <- rent_by_type_heating[1:6,]

options(scipen=10000)

ggplot(data=rent_by_type_heating, aes(x=heatingType, y=n)) +
  geom_bar(stat="identity", fill="steelblue")


# number of room
rent_by_type_rooms <- rental_data %>% count(noRooms, sort = TRUE)
rent_by_type_rooms <- rent_by_type_rooms[1:10,]
rent_by_type_rooms

options(scipen=10000)

ggplot(data=rent_by_type_rooms, aes(x=noRooms, y=n)) +
  geom_bar(stat="identity", fill="steelblue")

region_names
#boxplot rent in Germany
base_rent_filter <- filter(rental_data, regio2 %in% region_names & baseRent<= 2000 & baseRent >=300 & ! is.na(typeOfFlat))

#berlin %>%  drop_na(typeOfFlat)

ggplot(data=base_rent_filter, aes(x=regio2, y=baseRent, fill=regio2)) +
  geom_boxplot(alpha=0.3) +
  theme(legend.position="none")

#boxplot rent in Germany

berlin_data <- rental_data[rental_data$regio2 == 'Berlin',]
berlin_data
rent_by_region3 <- berlin_data %>% count(regio3, sort = TRUE)
rent_by_region3
regio3_names <- c(rent_by_region3[1:15,1]$regio3)
regio3_names
regio3_names <- regio3_names[order(regio3_names)]

#berlin_base_rent_filter <- filter(berlin_base_rent_filter, regio3 %in% berlin_rent_by_region_top_10)
#berlin_base_rent_filter <- filter(rental_data, regio2 %in% c("Berlin"))
berlin_base_rent_filter <- filter(berlin_data, regio3 %in% regio3_names & totalRent<= 2000 & totalRent >=300 )
#berlin_base_rent_filter <- filter(berlin_base_rent_filter, regio2 %in% c("Berlin"))
berlin_base_rent_filter

ggplot(data=berlin_base_rent_filter, aes(x=regio3, y=totalRent, fill=regio3)) +
  geom_boxplot(alpha=0.3) +
  theme(legend.position="none") +
  scale_x_discrete(guide = guide_axis(n.dodge=3))

#Number of rent houses per city
crawled_date <- rental_data$date
crawled_date
max(crawled_date)

feb20Data <- filter(rental_data, date == 'Feb20' & regio2 %in% region_names)

rent_by_region_feb20 <- feb20Data %>% count(regio2, sort = TRUE)
rent_by_region_feb20_top_10 <- rent_by_region_feb20[1:10,]

ggplot(data=rent_by_region_feb20_top_10, aes(x=regio2, y=n)) +
  geom_bar(stat="identity", fill="steelblue")


# Get all cities of Germany
date <- unique(rental_data[,"date"])
date


#test star chat
# Define the variable ranges: maximum and minimum
exam_scores <- data.frame(
  row.names = c("Student.1", "Student.2", "Student.3"),
  Biology = c(7.9, 3.9, 9.4),
  Physics = c(10, 20, 0),
  Maths = c(3.7, 11.5, 2.5),
  Sport = c(8.7, 20, 4),
  English = c(7.9, 7.2, 12.4),
  Geography = c(6.4, 10.5, 6.5),
  Art = c(2.4, 0.2, 9.8),
  Programming = c(0, 0, 20),
  Music = c(20, 20, 20)
)
exam_scores
max_min <- data.frame(
  Biology = c(20, 0), Physics = c(20, 0), Maths = c(20, 0),
  Sport = c(20, 0), English = c(20, 0), Geography = c(20, 0),
  Art = c(20, 0), Programming = c(20, 0), Music = c(20, 0)
)
max_min
rownames(max_min) <- c("Max", "Min")
max_min
# Bind the variable ranges to the data
df <- rbind(max_min, exam_scores)
df
student1_data <- df[c("Max", "Min", "Student.1"), ]
radarchart(student1_data)



