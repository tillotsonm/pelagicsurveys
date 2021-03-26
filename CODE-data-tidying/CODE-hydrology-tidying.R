#===========Code to integrate CDEC Dayflow tables and add other======
#=================Hydrology and environmental variables==============
#====================================================================
#Prepared by Michael Tillotson
#ICF
#Created March 15, 2021

require(tidyverse)
require(lubridate)

setwd("C:/Users/40545/Documents/GitHub/pelagicsurveys")

#==Read and join 4 DAYFLOW tables, correct date format and add Julian day column
#Data from CDEC Dayflow
dayflow <- read_csv("RawData/Hydrology/dayflow-results-1997-2020.csv")%>%
  rename(EXPORT = EXPORTS, EFFD = EFFDIV, DIVE = DIVER, EFFECT = EFFEC)%>%
  add_row(read_csv("RawData/Hydrology/dayflow-results-1984-1996.csv"))%>%
  add_row(read_csv("RawData/Hydrology/dayflow-results-1970-1983.csv"))%>%
  add_row(read_csv("RawData/Hydrology/dayflow-results-1956-1969.csv"))%>%
  mutate(Date = mdy(Date),
         JulianDay = yday(Date),.after = Month)%>%arrange(Date)%>%
  mutate(WaterYear = if_else(Month>=10,Year+1,Year),.after=Year)


#Data from Chronological Reconstructed Sacramento and San Joaquin Valley
#Water Year Hydrologic Classification Indices 
#(https://cdec.water.ca.gov/reportapp/javareports?name=WSIHIST)
WYType <- read_csv("RawData/Hydrology/WYTypes.csv",
                   col_types = "dddddfddddfdddddd")

Hydrology_Daily <- dayflow %>% left_join(WYType,by="WaterYear")%>%
  filter(Year>1958)

save(Hydrology_Daily,file = "TidyData/DATA_Hydrology_Tidy.rda")

