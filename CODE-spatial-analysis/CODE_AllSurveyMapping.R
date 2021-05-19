
theme_set(theme_bw())
library("sf")
library("tidyverse")
library("rnaturalearth")
library("rnaturalearthdata")
library("rnaturalearthhires")
library("stringr")
library("RColorBrewer")
library("spatialEco")
library("conflicted")

setwd("C:/Users/40545/Documents/GitHub/pelagicsurveys")

load("MASTER_Data/MASTER_Long_Format.rda")


#===============Delta Base Layer================================
marsh <- st_read(
  "CODE-spatial-analysis/hydro-delta-marsh/hydro_delta_marsh.shp")

EDSM_Strata <- st_read(
  "CODE-spatial-analysis/DSm_Subregions_UTM10NAD83/DSm_Subregions_UTM10NAD83.shp")



Long_Master %>% distinct(across(c(SampleDate,SurveySeason,StationCode)),.keep_all = T)%>%
  group_by(StationCode)%>%mutate(n.surveys = length(unique(SurveySeason)))%>%ungroup()%>%
  filter(n.surveys>1)%>%
  mutate(Location_Flag = if_else(StationCode %in% c("504","724","405","411", "704",
                                                    "812","815","906","915","323",
                                                    "340","345","418","501","519",
                                                    "703","705","706","711","716",
                                                    "723","801","901","914","804"),T,F))%>%
  ggplot(aes(x=SurveySeason,y=DepthBottom,fill=Location_Flag))+geom_boxplot()+scale_fill_viridis_d()+
  facet_wrap_paginate(~StationCode,ncol = 6, nrow = 5,page=2)

stations_compare_long <- All_Surveys %>% 
  distinct(across(c(SurveySeason,StationCode,Station_Latitude,Station_Longitude)),.keep_all = T)%>%
  filter(is.na(Station_Latitude)==F)



coord_ranges <- stations_compare_long%>%group_by(StationCode)%>%
  summarize(n.surveys =n(),
            Lat_Min=min(Station_Latitude),
            Lat_Max=max(Station_Latitude),
            Lon_Min=min(Station_Longitude),
            Lon_Max=max(Station_Longitude))%>%
  mutate(Lat_Diff = round(Lat_Max-Lat_Min,3),
         Lon_Diff = round(Lon_Max- Lon_Min,3),
         Sum_Diff = abs(Lon_Diff)+abs(Lat_Diff))%>%filter(n.surveys>1)%>%
  arrange(Sum_Diff)%>%filter(Sum_Diff>0.009)



all.stations <- stations_compare_long%>%
  filter(SurveySeason=="Bay Study" & StationCode %in% Check)%>%
  st_as_sf( coords = c("Station_Longitude", "Station_Latitude"), 
            crs = 4326, agr = "constant")


ggplot()+geom_sf(data = marsh, size = .5, color = "black", fill = "aquamarine2") + 
  geom_sf(data = EDSM_Strata, size = 1, color = "darkgreen",fill=NA) + 
  ggtitle("Stations 901-915") + 
  geom_sf(data = all.stations%>%filter(SurveySeason=="Bay Study"), size = 4,  aes(col=StationCode,shape = SurveySeason,fill=SurveySeason), alpha=.75)+
  coord_sf(xlim = c(-122.5, -121.2), ylim = c(37.5, 38.8), expand = FALSE)
  ggspatial::annotation_scale(location = 'bl')


#=========Map stations without EDSM Region
NASR.stations <- stations_compare_long%>%filter(is.na(SubRegion)==T)%>%
  st_as_sf( coords = c("Station_Longitude", "Station_Latitude"), 
            crs = 4326, agr = "constant")


ggplot()+geom_sf(data = marsh, size = .5, color = "black", fill = "aquamarine2") + 
  geom_sf(data = EDSM_Strata, size = 1, color = "darkgreen",fill=NA) + 
  ggtitle("Stations 901-915") + 
  geom_sf(data = NASR.stations%>%filter(), size = 4,  aes(col=Survey,shape = Survey,fill=Survey), alpha=.75)+
  coord_sf(xlim = c(-122.5, -121.25), ylim = c(37.5, 38.6), expand = FALSE)+
  scale_shape_manual(values=c(21,22,23,24,25))+
  ggspatial::annotation_scale(location = 'bl')



stationDist <- stations%>%
  arrange(StationNum)

Dist <- data.frame(stationDist%>%
                     st_distance())
  

names(Dist) <- stationDist$Survey_Station

Dist <- Dist %>% add_column(StationNum=stationDist$StationNum,Survey_Station=stationDist$Survey_Station,.before="FMWT_323")


Station_Dist <- function(WhichStation){
  Dist %>% filter(StationNum==WhichStation)%>%
    select(contains(WhichStation))%>%
    return()
  
}

  Station_Dist("915")
