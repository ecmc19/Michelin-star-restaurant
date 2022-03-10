library(ggplot2)
library(dplyr)
library(tidyr)

#load datasets
one <- read.csv ("one-star-michelin-restaurants.csv", head=T, sep=",")
two <- read.csv ("two-stars-michelin-restaurants.csv", head=T, sep=",")
three <- read.csv ("three-stars-michelin-restaurants.csv", head=T, sep=",")

#create column
##continents
asia <- c("Hong Kong", "Macau","Singapore","South Korea","Taipei","Thailand")

america <- c("California","Chicago","New York City","Washington DC", "Rio de 
Janeiro","Sao Paulo")

europe <- c("Austria", "Czech Republic", "Denmark", "Hungary", "Norway", "Finland", 
            "Ireland", "Poland", "United Kingdom", "Croatia", "Greece", "Sweden")

#fill column
one$continent <- ifelse(one$region %in% asia , "Asia", ifelse(one$region %in% america, 
                                                              "America", "Europe"))
two$continent <- ifelse(two$region %in% asia , "Asia", ifelse(two$region %in% 
                                                                america, "America", "Europe"))
three$continent <- ifelse(three$region %in% asia , "Asia", ifelse(three$region %in% 
                                                                    america, "America", "Europe"))


#stars
one$star <- 1
two$star <- 2 
three$star <- 3

#factor
one$year <- as.factor(one$year)
two$year <- as.factor(two$year)
three$year <- as.factor(three$year)
one$region <- as.factor(one$region)
two$region <- as.factor(two$region)
three$region <- as.factor(three$region)

#join
df <- rbind(one, two, three) 

#map
library(sf)
map <- st_read("CNTR_RG_60M_2020_4326.shp")
plot(map)
map<- map[ ,3:6]
map<- map[ ,-2:-3]
head(map)
colnames(map)<- c("region", "geometry")
plot(st_geometry(map))

library(sp)
xy <- df[,c("longitude","latitude")]
mich <- SpatialPointsDataFrame(coords = xy, data = df,
                               proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 
+towgs84=0,0,0"))
mich$star<-as.factor(mich$star)

#map plot
library(tmap)
tmap_mode("view")
tm_shape(map) + 
  tm_borders()+
  tm_shape(mich)+
  tm_layout(title= " Michelin Stars Restaurant - 2019", 
            title.position = c('top'))+
  tm_dots(col="star", size = 0.1, labels=c("1 Star","2 Stars","3 Stars"),palette = 
            c("aquamarine2", "cornflowerblue", "brown1"),popup.vars=c("Name"="name", 
                                                                      "Cuisine"="cuisine", "City"="city", "Price"="price"))




#Ploting countries by total number of stars with different colors in the European continent
#europe
EU <- aggregate(star~region, df, sum)
colnames(EU)<- c("region", "total")
EU$continent <-ifelse(EU$region %in% asia , "Asia", ifelse(EU$region %in% america, 
                                                           "America", "Europe"))
EU<-EU %>% 
  filter(continent == "Europe")
map2 <- map %>%
  inner_join(EU)
tmap_mode("view")
tm_shape(map2) + 
  tm_borders()+
  tm_fill("total", 
          title="Michelin Stars Restaurants - Europe", 
          breaks=c(0,25,50,75,100,125,150,175,200),
          palette=c("#DD517F" , "#E68E36" ,"#556DC8","#7998EE"))
