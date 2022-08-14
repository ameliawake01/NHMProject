#Loading the required packages
install.packages(c("ggmap", "ggplot2", "ggthemes", "maps", "dplyr")
library(ggmap)
library(ggplot2)
library(ggthemes)
library(maps)
library(dplyr)

#Loading in coordinate data
map.data <- read.csv("/home/amelia-wake/Documents/NHMProject/PossibleDatasets/MapData.csv")

#Changing the scope of the global map based on the data in map.data
world_map <- map_data("world")
world_map = subset(world_map, long < max(map.data$Longitude)+5 & 
                     long > min(map.data$Longitude)-5)
world_map = subset(world_map, lat < max(map.data$Latitude)+5 & 
                     lat > min(map.data$Latitude)-5)

#Creating labels of country names
LAB = world_map %>%
  group_by(region) %>%
  select(region, long, lat) %>%
  summarise_all(mean)

#Creating the plot of Europe
ggplot(world_map, aes(x = long, y = lat)) +
  geom_polygon(aes(group = group, fill = region), show.legend = FALSE, alpha = 1/5) +
  geom_point(data = map.data, aes(x = Longitude, y = Latitude, colour = Soil.Type),
             size = 3) +
  geom_text(data = LAB, aes(label = region), size = 2) +
  scale_fill_grey() + theme_map()


