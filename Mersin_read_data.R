library(tidyverse)
library(lubridate)

df <- read.table("data/data_from_Mersin_for_R.txt",sep="\t",header=T,fileEncoding="UTF-8")

# We identify specific columns and give them new names

colnames <- c("Cruise", "Station", "Type", "yyyy.mm.ddThh.mm.ss.sss", "Longitude..degrees_east.", "Latitude..degrees_north.",
              "Bot..Depth..m.", "St_ID", "Pressure..dBar.", "Temperature..ITS.90..degrees.C.", "Salinity..per.mille.",
              "DOW..µM.", "Oxygen.Saturation....", "Oxygen..SBE.43..mg.l.", "Total.Chl.a..µg.l.", "pH", 
              "NH4..µM.", "NO2.N..µM.", "NO3.N.NO2.N..µM.", "PO4..µM.", "TP..µM.", "TPP..µM.", "Si..µM.", 
              "POC..µM.", "PON..µM.", "TSS..mg.l.", "SD..m.", "TRIX..Chem.", "TRIX")

colnamesnew <- c("Cruise", "Station", "Type", "Datetime", "Lon", "Lat", "BotDepth", "St_ID", "Depth",
                 "Temp_C", "Sal", "DOW_uM", "O2_Sat", "O2_mgl", "Chla_ug", "pH", "NH4_uM", "NO2_uM", "NO3NO2_uM",
                 "PO4_uM", "TP_uM", "TPP_uM", "Si_uM", "POC_uM", "PON_uM", "TSS_mgl", "SD_m", "TRIXChem", "TRIX")

infocols <- c("Cruise", "Station", "Type", "Date", "Lon", "Lat", "BotDepth", "St_ID", "Depth")

valuecols <- c("Temp_C", "Sal", "DOW_uM", "O2_Sat", "O2_mgl", "Chla_ug", "pH", "NH4_uM", "NO2_uM", "NO3NO2_uM", "DIN_uM",
               "PO4_uM", "TP_uM", "TPP_uM", "Si_uM", "POC_uM", "PON_uM", "TSS_mgl", "SD_m", "TRIXChem", "TRIX")

df <- df[,colnames]
names(df) <- colnamesnew

# calculate DIN by adding NO3NO2 and NH4 
# if either NO3NO2 or NH4 is NA, then no value is calculated

df <- df %>%
  mutate(Date=as.Date(Datetime)) %>%
  mutate(DIN_uM=NO3NO2_uM+NH4_uM)

df <- df[,c(infocols,valuecols)]

# input table has only date, location, cruise information etc. (columns 1-8) in the first row of each profile.
# the following lines read the cell in the first column in each row. If the first cell is empty,
# the data from the previous row is filled in for columns 1-8
# if the cell was not empty, then we have reached a new profile and the profile information is updated to be
# used in the following "empty" rows

n <- 1:8 # no of columns to fill down
v0 <- df[1,n]
for(i in 2:nrow(df)){
  if(df[i,1]==""){
    df[i,n] <- v0
  } else{
    v0 <- df[i,n]
  }
  
}

# transpose the data to 'long' format with each observed value on a separate row and 
#  a column called 'Param' to identify the parameters 

df <- df %>%
  pivot_longer(cols=all_of(valuecols), names_to="Param", values_to="Value")

#  remove empty rows with NA values
df <- df %>%
  filter(!is.na(Value))

# save the data for later calculations
save(df,file="data/data_from_Mersin.Rda")
