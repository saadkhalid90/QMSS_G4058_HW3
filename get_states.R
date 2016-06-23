## reused code from homework 1

## defining the subset_tweets function
subset_tweets <- function(pattern, tweets_df){
  indices <- grepl(pattern = pattern, tweets_df$text)
  tweets_df[indices, ]
}

## regular expressions for each candidate
subset_patterns <- c("[Bb]ernie|[Ss]anders",
                     "[Hh]illary|[Cc]linton",
                     "[Dd]onald|[Tt]rump",
                     "[Tt]ed|[Cc]ruz",
                     "[Mm]arco|[Rr]ubio")

## defining party colors for color transitions based on party
party_colors <- c("blue", "blue", "red", "red")

## indexing subset_patterns and color by candidate name
names(subset_patterns) <- c("Bernie Sanders",
                            "Hillary Clinton",
                            "Donald Trump",
                            "Ted Cruz",
                            "Marco Rubio")

## CODE Obtained from stack overflow

library(sp)
library(maps)
library(maptools)
library(dplyr)

# The single argument to this function, pointsDF, is a data.frame in which:
#   - column 1 contains the longitude in degrees (negative in the US)
#   - column 2 contains the latitude in degrees

latlong2state <- function(pointsDF) {
  # Prepare SpatialPolygons object with one SpatialPolygon
  # per state (plus DC, minus HI & AK)
  states <- map('state', fill=TRUE, col="transparent", plot=FALSE)
  IDs <- sapply(strsplit(states$names, ":"), function(x) x[1])
  states_sp <- map2SpatialPolygons(states, IDs=IDs,
                                   proj4string=CRS("+proj=longlat +datum=WGS84"))
  
  # Convert pointsDF to a SpatialPoints object 
  pointsSP <- SpatialPoints(pointsDF, 
                            proj4string=CRS("+proj=longlat +datum=WGS84"))
  
  # Use 'over' to get _indices_ of the Polygons object containing each point 
  indices <- over(pointsSP, states_sp)
  
  # Return the state names of the Polygons object containing each point
  stateNames <- sapply(states_sp@polygons, function(x) x@ID)
  stateNames[indices]
}

## subsetting data
geo_tagged_subset <- subset_tweets(pattern = subset_patterns["Donald Trump"], geo_tagged)
## geo_tagged_subset <- geo_tagged
## checking the origin of tweets 
tweets_geo_points <- data.frame(x = geo_tagged_subset$place_lon, y = geo_tagged_subset$place_lat)

tweets_states <- latlong2state(tweets_geo_points)

tweets_outside_US <- sum(is.na(tweets_states))
tweets_states <- data.frame(table(tweets_states))

simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}
tweets_states <- rename(tweets_states, 
                        NAME_1 = tweets_states)

tweets_states$NAME_1 <- unname(sapply(as.character(tweets_states$NAME_1), simpleCap))
tweets_states$NAME_1 <- gsub(pattern = " Of ", replacement = " of ", x = tweets_states$NAME_1)
tweets_states$NAME_1 <- as.factor(tweets_states$NAME_1)


