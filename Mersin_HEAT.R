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

# load cruise sampling results for all stations
load("data/data_from_Mersin.Rda")

source("get_station_positions.R")

# get distinct sampling stations and add year / decade information
df_stn_TM36 <- get_station_positions(df)
sf_stn_TM36 <- get_station_positions(df,as_sf=T)

# --- Erdemli transect data    ---------------------------------------------------------------------

# xTM36 <- 381962
# yTM36 <- 4075951
shape <- st_read(dsn="gis",layer="erdemli_coast_transect2_5km_20km")

grid_res <- 5000

sf_stn_transect <- st_intersection(sf_stn_TM36,shape)
df_stn_TM36_transect_Erdemli <- sf_stn_transect %>%
  mutate(x = unlist(map(sf_stn_transect$geometry,1)),
         y = unlist(map(sf_stn_transect$geometry,2)))
df_stn_TM36_transect_Erdemli$geometry <- NULL

df_stn_TM36_transect_Erdemli <- df_stn_TM36_transect_Erdemli %>%
  mutate(distance=distance*1000) %>%
  mutate(DistRnd=distance-grid_res) %>%
  rename(Dist=distance)

df_trans_Erdemli <- df_stn_TM36_transect_Erdemli %>%
  left_join(df,by=c("Station","St_ID"))

df_trans_Erdemli <- df_trans_Erdemli %>%
  mutate(Year=year(Date),Month=month(Date)) %>%
  left_join(dfTargetMonths,by=c("Param","Month")) %>%
  filter(!is.na(Target))

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
  mutate(Year=year(Date),Month=month(Date)) %>%
  left_join(dfTargetMonths,by=c("Param","Month")) %>%
  filter(!is.na(Target))

# ------ list of transect data ----------------------
list_trans <- list("Mersin"=df_trans_Mersin,"Erdemli"=df_trans_Erdemli)


# ------------- do HEAT caclulations for each transect ---------------------
for(i in 1:length(list_trans)){
  cat(paste0(i,": ",names(list_trans)[i],"\n"))
  df_trans <- list_trans[[i]]
  transect_name <- paste0("Transect ",names(list_trans)[i])
  
  
  df_HEAT <- df_trans %>%
    group_by(DistRnd,Year,Param,Target,Response,Criteria) %>%
    summarise(Value=mean(Value,na.rm=T)) %>%
    ungroup()
  
  df_HEAT <- df_HEAT %>%
    mutate(ER=ifelse(Response==-1,Target/Value,Value/Target))
  
  df_HEAT_C <- df_HEAT %>%
    group_by(DistRnd,Year,Criteria) %>%
    summarise(ER=mean(ER,na.rm=T))
  
  df_HEAT <- df_HEAT_C %>%
    arrange(DistRnd,Year,Criteria,desc(ER)) %>%
    group_by(DistRnd,Year) %>%
    slice(1)
  
  df_HEAT <- df_HEAT %>%
    mutate(Class = ifelse(ER>0.5,ifelse(ER>1,ifelse(ER>1.5,ifelse(ER>2,5,4),3),2),1))
  
  ClassNames <- c("High","Good","Mod","Poor","Bad")
  
  df_HEAT_plot <- df_HEAT %>%
    mutate(ClassID = ifelse(ER>0.5,ifelse(ER>1,ifelse(ER>1.5,ifelse(ER>2,5,4),3),2),1)) %>%
    mutate(Class=ClassNames[ClassID]) %>%
    mutate(DistRnd=DistRnd+(grid_res/2))
  
  df_HEAT_plot$Class <- factor(df_HEAT_plot$Class,levels=ClassNames)
  
  pal_class<-c("#007eff","#00d600","#ffff00","#ff8c2b","#ff0000")
  
  p<-ggplot(df_HEAT_plot) +
    geom_tile(aes(x=Year,y=DistRnd,fill=Class)) +
    labs(subtitle=transect_name) +
    ylab("Distance [m]") +
    xlab("Year") +
    scale_fill_manual(values=pal_class) +
    theme_ipsum() +
    theme(legend.position = "right",
          legend.background=element_rect(fill="#FFFFFF",
                                         size=0.5,
                                         linetype="solid",
                                         colour ="#AAAAAA"))
  p
  
  assign(paste0("p",i),p)
}

plots_aligned <- align_patches(p1, p2)

ggsave("png/HEAT_transect_Mersin.png",plots_aligned[[1]],dpi=300,units="cm",width=25,height=12)
ggsave("png/HEAT_transect_Erdemli.png",plots_aligned[[2]],dpi=300,units="cm",width=25,height=12)

