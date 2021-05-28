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
source("targets.R")
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

PlotMax<-list()

# ------------- do HEAT calculations for each transect ---------------------
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
  
  # ///////////////////////////////////////////////////////////////////
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
  
  # ///////////////////////////////////////////////////////////////////
  
  dfYear <- df_HEAT %>% ungroup() %>% distinct(Year) 
  dfDist <- df_HEAT %>% ungroup() %>% distinct(DistRnd)
  dfYearDist <- merge(dfYear,dfDist)
  
  df_HEAT_plot <- dfYearDist %>%
    left_join(df_HEAT,by=c("Year","DistRnd")) %>%
    mutate(Distance=paste0(DistRnd*0.001," km"))
  
  dist_levels <- rev(dfDist$DistRnd)
  dist_levels <- paste0(dist_levels/1000," km")
  
  df_HEAT_plot$Distance <- factor(df_HEAT_plot$Distance,levels=dist_levels)
  
  df_HEAT_plot$ER0<-0
  df_HEAT_plot$ER05<-0.5
  df_HEAT_plot$ER10<-1
  df_HEAT_plot$ER15<-1.5
  df_HEAT_plot$ER20<-2
  df_HEAT_plot$ERmax <- 99
  ERmax<- c(5,6,7,8,9,10,15,20,30,40,50)
  ERmax<- ERmax[ERmax>max(df_HEAT_plot$ER,na.rm=T)][1]
  PlotMax[[i]] <- ERmax
  
  df_HEAT_plot <-df_HEAT_plot %>%
    mutate(RibbonYear = Year) %>%
    mutate(RibbonYear=ifelse(RibbonYear==max(Year),2030,RibbonYear)) %>%
    mutate(RibbonYear=ifelse(RibbonYear==min(Year),1980,RibbonYear))
  

  cat(paste0(max(df_HEAT_plot$ER,na.rm=T)," ",ERmax,"\n"))
  alpha_bands <- 0.6 
  p_ts <-ggplot(df_HEAT_plot) +
    labs(subtitle=transect_name) +
    ylab("Eutrophication Ratio [ER]") +
    xlab("Year") +
    scale_fill_manual(values=pal_class) +
    facet_grid(Distance~.) + #,ncol=1
    geom_ribbon(aes(ymin=ER0,ymax=ER05,x=RibbonYear),fill="#007eff",alpha=alpha_bands)+
    geom_ribbon(aes(ymin=ER05,ymax=ER10,x=RibbonYear),fill="#00d600",alpha=alpha_bands)+
    geom_ribbon(aes(ymin=ER10,ymax=ER15,x=RibbonYear),fill="#ffff00",alpha=alpha_bands)+
    geom_ribbon(aes(ymin=ER15,ymax=ER20,x=RibbonYear),fill="#ff8c2b",alpha=alpha_bands)+
    geom_ribbon(aes(ymin=ER20,ymax=ERmaxX,x=RibbonYear),fill="#ff0000",alpha=alpha_bands)+
    geom_line(aes(x=Year,y=ER),size=1) +
    geom_point(aes(x=Year,y=ER),size=1) +
    theme_ipsum() +
    theme(legend.position = "right",
          strip.text = element_text(hjust = 0),
          panel.spacing.y = unit(0.1,units="cm"),
          legend.background=element_rect(fill="#FFFFFF",
                                         size=0.5,
                                         linetype="solid",
                                         colour ="#AAAAAA")) +
    coord_cartesian(xlim=c(1991,2019),ylim=c(0,PlotMax[[i]] ))
  p_ts
  assign(paste0("p",i,"_ts"),p_ts)
  
  
}

# ------------- save plots ---------------------
p_ts <- p1_ts + p2_ts 

plots_aligned <- align_patches(p1, p2)

ggsave("png/HEAT_transect_Mersin.png",plots_aligned[[1]],dpi=300,units="cm",width=25,height=12)
ggsave("png/HEAT_transect_Erdemli.png",plots_aligned[[2]],dpi=300,units="cm",width=25,height=12)

#plots_aligned_ts <- align_patches(p1_ts, p2_ts)

#ggsave("png/HEAT_timeseries_Mersin.png",plots_aligned_ts[[1]],dpi=300,units="cm",width=12,height=25)
#ggsave("png/HEAT_timeseries_Erdemli.png",plots_aligned_ts[[2]],dpi=300,units="cm",width=12,height=25)

ggsave("png/HEAT_timeseries.png",p_ts,dpi=300,units="cm",width=25,height=25)
