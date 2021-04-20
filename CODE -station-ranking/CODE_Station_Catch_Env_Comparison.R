#Prepared by Michael Tillotson
#ICF
#Created April 16, 2021

#Load libraries:
require(tidyverse)
require(lubridate)
theme_set(theme_bw())


#Set working directory
setwd("C:/Users/40545/Documents/GitHub/pelagicsurveys")


load("MASTER_Data/MASTER_Env_Hydro.rda")
load("MASTER_Data/CDFW_Tows_Target_Species.rda")

Env <- Environment_Hydrology %>% 
  mutate(StationCode = if_else(Survey=="EDSM",
                               as.character(SubRegion),
                               as.character(StationCode)))%>%
  mutate(StationCode = as.factor(StationCode))%>%
  filter(WaterYear>1970 & is.na(Salinity)==F)%>%
  select(-c(SAC:SacIndex,SJWinter:May8Riv,Field_coords))%>%
  mutate(DOWY = yday(Date-92))%>%
  mutate(WYWeek = ceiling(DOWY/7))%>%
  group_by(Survey,StationCode)%>%
  mutate(Station_Latitude = mean(Latitude,na.rm=T),
         Station_Longitude = mean(Longitude,na.rm=T)
         )%>%
  select(-c(Longitude,Latitude,Survey_Station))%>%
  group_by(Survey,StationCode,Month)%>%
  mutate(Salinity = if_else(Salinity<0,0,Salinity))



Core_Targets <- Core_Targets %>%
  mutate(Salinity = if_else(Salinity<0,0,Salinity))




Core_Targets %>% 
  group_by(SubRegion,Month)%>%
  mutate(N_Tows = n(),
         N_Stations = length(unique(StationCode)))%>%
  ungroup()%>%
  group_by(SubRegion,Month,N_Tows,N_Stations)%>%
  summarize_at(vars(contains("CPUV")|contains("Volume")),mean,na.rm=T)%>%
  mutate_if(is.numeric,round,2)%>%view()
  
  
