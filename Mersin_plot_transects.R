library(tidyverse)
library(lubridate)
library(sf)
library(hrbrthemes)
library(rworldmap)  # getMap()
library(raster)  # intersect()
library(rgdal)
library(graticule)
library(scales)
library(patchwork)

load("data/data_from_Mersin.Rda")

source("get_station_positions.R")

# get distinct sampling stations and add year / decade information
df_stn_TM36 <- get_station_positions(df)


# Mersin position (ca.)
# 36.799072446615035, 34.64082759057766

# ----- world map for map backgroud --------------------------------------

world <- getMap(resolution = "high")

# define the geographical projections
prjTM36 <- CRS("+init=epsg:5256")
prjWGS84 <- CRS("+init=epsg:4326")

# define a rectangle used to crop the world map and assign projection
clip_map_WGS84 <- as(extent(32, 37, 33, 39), "SpatialPolygons")
proj4string(clip_map_WGS84) <- prjWGS84

# crop the world map to the rectangle
world_clip_WGS84 <- raster::intersect(world, clip_map_WGS84)

# project the world map to TM36 and fortify for plotting in ggplot
world_clip_TM36 <- spTransform(world_clip_WGS84,prjTM36)
world_clip_TM36_f <- fortify(world_clip_TM36) 

# ----- Plot the Mersin transect -----------------------------------------------------------------

# start coordinates at Mersin (TM36)
xTM36 <- 381962 
yTM36 <- 4075951

grid_res <- 5000 # transect grid resolution (m)
grid_cells <- 10 # no of grid cells in transect

# create sequences of values defining the grid
xvalues_TM36 <- seq(xTM36-(grid_res/2), xTM36+(grid_res/2),by=grid_res)
yvalues_TM36 <- seq(yTM36-(grid_cells*grid_res),yTM36,by=grid_res)

# use the graticule package to create graticule line from the sequence of  
grat_TM36 <- graticule(xvalues_TM36, yvalues_TM36)
grat_TM36 <- fortify(grat_TM36)

p1<-ggplot() +
  geom_polygon(data = world_clip_TM36_f,
               aes(x = long, y = lat, group = group),
               fill = "#CCCCCC", colour = "#AAAAAA", alpha=0.4) +  #"black"
  geom_point(data=df_stn_TM36,aes(x=x,y=y)) +
  geom_path(data=grat_TM36,aes(x=long,y=lat,group=group),color="#FF0000",alpha=0.8) +
  ylab("") +
  xlab("TUREF_TM36 [EPSG:5256]") +
  coord_sf(xlim=c(200000,450000),ylim=c(3945000,4105000), expand=FALSE,crs=st_crs(5256)) +
  theme_ipsum() +
  theme(legend.position = c(0.98, 0.98),
        legend.justification = c(1,1),
        legend.background=element_rect(fill="#FFFFFF",
                                       size=0.5,
                                       linetype="solid",
                                       colour ="#AAAAAA"))
p1

# ----- Plot the Erdemli transect -----------------------------------------------------------------

# Read the shape file containing the five areas for grouping the stations
shape <- st_read(dsn="gis",layer="erdemli_coast_transect2_5km_20km")
plot(shape) 


df_stn_select <- df_stn_TM36 %>%
  filter(str_detect(Station,"ETS_500")) %>%
  mutate(Dist=30000,DistRnd=25000)

p2<-ggplot() +
  geom_polygon(data = world_clip_TM36_f,
               aes(x = long, y = lat, group = group),
               fill = "#CCCCCC", colour = "#AAAAAA", alpha=0.4) +  #"black"
  geom_point(data=df_stn_TM36,aes(x=x,y=y)) +
  geom_point(data=df_stn_select,aes(x=x,y=y),colour = "#FF0000") +
  geom_sf(data=shape,fill = NA, colour = "#FF0000", alpha=0.8) +
  ylab("") + xlab("TUREF_TM36 [EPSG:5256]") +
  coord_sf(xlim=c(200000,450000),ylim=c(3945000,4105000), expand=FALSE, crs=st_crs(5256) ) +
  theme_ipsum() +
  theme(legend.position = c(0.98, 0.98),
        legend.justification = c(1,1),
        legend.background=element_rect(fill="#FFFFFF",
                                       size=0.5,
                                       linetype="solid",
                                       colour ="#AAAAAA"))
p2

# ----- Save the plots as png -----------------------------------------------------------------

plots_aligned <- align_patches(p1, p2)

p1<-plots_aligned[[1]]
p2<-plots_aligned[[2]]

ggsave("png/transect_Mersin.png",p1,dpi=300,units="cm",width=25,height=15)
ggsave("png/transect_Erdemli.png",p2,dpi=300,units="cm",width=25,height=15)
