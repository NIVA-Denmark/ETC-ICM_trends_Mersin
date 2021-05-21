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

# get distinct sampling stations and add year / decade information
dfstn <- df %>%
  mutate(Year=year(Date)) %>%
  mutate(DecadeFrom = 10*(round((-4.5+Year)/10))) %>%
  mutate(Decade=paste0(DecadeFrom,"-",DecadeFrom+9)) 

dfstn <- dfstn %>%
  distinct(DecadeFrom,Decade,Station,St_ID,Lat,Lon)

# add an id row - this is mainly done if we need to export to ArcGis and import again
# matching by lat, lon can be problematic with data exported from ArcGIS
dfstn$ID <- 1:nrow(dfstn)


# convert to simple features (sf) spatial data, specifying the coordinate variables
sf_stn <- dfstn %>% 
  st_as_sf(coords=c("Lon", "Lat"))

# specify that the the coordinates are Lat / Lon (WGS84) [EPSG:4326]
sf_stn_geo = st_set_crs(sf_stn, 4326)
st_is_longlat(sf_stn_geo)

# transform the coordinates to Turkish Reference system 36 [EPSG:5256]

sf_stn_TM36 <- st_transform(sf_stn_geo, crs = st_crs(5256))

# convert the station sf spatial data back to a 'regular' data frame
df_stn_TM36 <- sf_stn_TM36 %>%
  mutate(x = unlist(map(sf_stn_TM36$geometry,1)),
         y = unlist(map(sf_stn_TM36$geometry,2)))
df_stn_TM36$geometry <- NULL


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
shape <- st_read(dsn="gis",layer="erdemli_coast_buffer_5")
plot(shape) 

p2<-ggplot() +
  geom_polygon(data = world_clip_TM36_f,
               aes(x = long, y = lat, group = group),
               fill = "#CCCCCC", colour = "#AAAAAA", alpha=0.4) +  #"black"
  geom_point(data=df_stn_TM36,aes(x=x,y=y)) +
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

ggsave("png/transect_Mersin.png",p1,dpi=300,units="cm",width=12,height=25)
ggsave("png/transect_Erdemli.png",p2,dpi=300,units="cm",width=12,height=25)
