require(tidyverse)
require(lubridate)
require(sf)

get_station_positions <- function(df,crsfrom=4326,crsout=5256){
  
  dfstn <- df %>%
    mutate(Year=year(Date)) %>%
    mutate(DecadeFrom = 10*(round((-4.5+Year)/10))) %>%
    mutate(Decade=paste0(DecadeFrom,"-",DecadeFrom+9))

  dfstn <- dfstn %>%
    distinct(DecadeFrom,Decade,Station,St_ID,Lat,Lon)
 
  # add an id row - this is mainly done if we need to export to ArcGis and import again
  # matching by lat, lon can be problematic with data exported from ArcGIS
  dfstn$ID <- 1:nrow(dfstn)
  
  sf_stn <- dfstn %>% 
    st_as_sf(coords=c("Lon", "Lat"))
  
  sf_stn_geo = st_set_crs(sf_stn, crsfrom)

  sf_stn_proj <- st_transform(sf_stn_geo, crs = st_crs(crsout))
  df_stn_proj <- sf_stn_proj %>%
    mutate(x = unlist(map(sf_stn_proj$geometry,1)),
           y = unlist(map(sf_stn_proj$geometry,2)))
  df_stn_proj$geometry <- NULL
  
  return(df_stn_proj)
}