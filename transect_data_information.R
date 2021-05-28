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
sf_stn_TM36 <- get_station_positions(df,as_sf=T)

# --- Erdemli transect data    ---------------------------------------------------------------------

# xTM36 <- 381962
# yTM36 <- 4075951
shape <- st_read(dsn="gis",layer="erdemli_coast_transect2_5km_20km_v2")

grid_res <- 5000

sf_stn_transect <- st_intersection(sf_stn_TM36,shape)
df_stn_TM36_transect_Erdemli <- sf_stn_transect %>%
  mutate(x = unlist(map(sf_stn_transect$geometry,1)),
         y = unlist(map(sf_stn_transect$geometry,2)))
df_stn_TM36_transect_Erdemli$geometry <- NULL

df_stn_TM36_transect_Erdemli <- df_stn_TM36_transect_Erdemli %>%
  mutate(DistRnd=distance-grid_res) %>%
  rename(Dist=distance)

df_trans_Erdemli <- df_stn_TM36_transect_Erdemli %>%
  left_join(df,by=c("Station","St_ID"))

df_trans_Erdemli <- df_trans_Erdemli %>%
  mutate(Month=month(Date)) %>%
  left_join(dfTargetMonths,by=c("Param","Month")) %>%
  filter(!is.na(Target))

df_trans_Erdemli_cruises <- df_trans_Erdemli %>%
  distinct(Cruise,Station) %>%
  arrange(Cruise,Station) %>%
  group_by(Cruise) %>%
  summarise(Stations=paste0(Station,collapse=","))

# --- Mersin transect data    ---------------------------------------------------------------------
xTM36 <- 381962
yTM36 <- 4075951

grid_res <- 5000 # transect grid resolution (m)
grid_cells <- 10 # no of grid cells in transect

df_stn_TM36_transect_Mersin <- df_stn_TM36 %>%
  filter(x>=xTM36-(grid_res/2)) %>%
  filter(x<=xTM36+(grid_res/2)) %>%
  filter(y<=yTM36) %>%
  filter(y>=yTM36-(grid_res*grid_cells)) %>%
  mutate(Dist=yTM36-y) %>%
  mutate(DistRnd =floor(Dist/grid_res)*grid_res) %>%
  dplyr::select(-ID)


df_trans_Mersin <- df_stn_TM36_transect_Mersin %>%
  left_join(df,by=c("Station","St_ID"))

df_trans_Mersin <- df_trans_Mersin %>%
  mutate(Month=month(Date)) %>%
  left_join(dfTargetMonths,by=c("Param","Month")) %>%
  filter(!is.na(Target))


df_trans_Mersin_cruises <- df_trans_Mersin %>%
  distinct(Cruise,Station) %>%
  arrange(Cruise,Station) %>%
  group_by(Cruise) %>%
  summarise(Stations=paste0(Station,collapse=","))


# --- save transect information    ---------------------------------------------------------------------
write.table(df_trans_Mersin_cruises,file="results/transect_Mersin_cruises.csv",sep=";",row.names=F)   # 51
write.table(df_trans_Erdemli_cruises,file="results/transect_Erdemli_cruises.csv",sep=";",row.names=F) # 79

