require(tidyverse)

# official targets
dfTargets <- data.frame(Param=c("TP_uM","NO3NO2_uM","Chla_ug","SD_m"),
                        Value=c(   0.23,       0.71,      1.0,  9.0),
                        Response=c(   1,          1,        1,   -1))

# METU targets
# dfTargets <- data.frame(Param=c("TP_uM","NO3NO2_uM","DIN_uM","PO4_uM","Chla_ug","SD_m"),
#                          Value=c(   0.25,       0.77,     1.15,  0.067,     0.35,  7.0),
#                          Response=c(   1,          1,        1,      1,        1,   -1))


# Get parameter-month combinations (i.e. only winter months for nutrients)

# -- Tugrul et al (2018) --
# Mersin Bay 2008–2013
# Autumn (October, November)
# Winter (January, February, March)
# Spring (April, May)
# Summer (July, August, September)

dfParamMonths <- data.frame(
  Param=c("TP_uM","NO3NO2_uM","DIN_uM","PO4_uM","Chla_ug","SD_m"),
  Criteria=c(1,1,1,1,2,2),
  Months=c("1,2,3","1,2,3","1,2,3","1,2,3","4,5","4,5,6,7,8,9")) %>%
  separate(Months,sep=",",into=c("M1","M2","M3","M4","M5","M6"))

dfParamMonths <- dfParamMonths %>%
  pivot_longer(c(M1,M2,M3,M4,M5,M6),names_to="MonthID",values_to="Month") %>%
  filter(!is.na(Month)) %>%
  mutate(Month=as.numeric(Month)) %>%
  dplyr::select(-MonthID)

dfTargetMonths <- dfParamMonths %>%
  left_join(dfTargets,by="Param") %>%
  rename(Target=Value) %>%
  filter(!is.na(Target))
