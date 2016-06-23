## installing required packages
packages <- c("ggmap", 
              "rgdal", 
              "rgeos", 
              "maptools", 
              "dplyr", 
              "tidyr", 
              "tmap", 
              "maps")
new_packages <- packages[!packages %in% installed.packages()]
if (length(new_packages) > 0){
  install.packages(new_packages, dependencies = TRUE)
}

## loading the relevant libraries
lapply(packages, library, character.only = TRUE)

## reading in the states .shp file
states_shp <- readShapeSpatial("USA_adm/USA_adm1.shp")

population <- read.csv("VotingPopulation.csv")

my_data <- data.frame(NAME_1 = states_shp$NAME_1,
                      id = states_shp$ID_1)
population <- rename(population, 
                     NAME_1 = State)
my_data <- full_join(my_data, population)

state_frequency <- full_join(my_data, tweets_states)
state_frequency$tweets_per_vote_pop <- state_frequency$Freq/ state_frequency$vote_population 

states_shp.f <- fortify(states_shp, region = "ID_1")
class(states_shp.f)

merge.shp.coef<-merge(states_shp.f, state_frequency, by="id", all.x=TRUE)
final.plot<-merge.shp.coef[order(merge.shp.coef$order), ]

## state wise choropleth plots

ggplot() +
  geom_polygon(data = final.plot, 
               aes(x = long, y = lat, group = group, fill = tweets_per_vote_pop), 
               color = "black", size = 0.25, alpha = 0.75) + 
  coord_map(projection="azequalarea") +
  xlim(-125, -60) +
  ylim(23, 50) +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_blank()) +
  ggtitle("Geo - distribution of Ted Cruz tweets")

library(grid)

## world map
mp <- NULL
mapWorld <- borders("world", colour="black", fill="white") # create a layer of borders
mp <- ggplot() +  mapWorld
mp <- mp + geom_point(data = geo_tagged_subset, 
                      aes(x = place_lon, y = place_lat), size = 0.6, alpha = 1/4, color = "darkblue") +
                      theme(axis.line=element_blank(),
                            axis.text.x=element_blank(),
                            axis.text.y=element_blank(),
                            axis.ticks=element_blank(),
                            axis.title.x=element_blank(),
                            axis.title.y=element_blank(),
                            panel.grid.major=element_blank(),
                            panel.grid.minor=element_blank(),
                            panel.border=element_blank()) +
  ggtitle("World distribution of Marco Rubio tweets")
mp


