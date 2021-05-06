

library("sf")
library("rnaturalearth")
library("rnaturalearthdata")
library("rnaturalearthhires")
library("stringr")
library("RColorBrewer")
library("spatialEco")
library("tidyverse")

setwd("C:/Users/40545/Documents/GitHub/pelagicsurveys")

load("MASTER_Data/MASTER_All_Surveys.rda")


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
  facet_wrap_paginate(~StationCode,ncol = 6, nrow = 5,page=3)

stations_compare_long <- Long_Master %>% 
  distinct(across(c(Station_Longitude,Station_Latitude,Survey_Station)),.keep_all = T)%>%
  filter(is.na(Station_Latitude)==F)%>%select(c(Station_Longitude,Station_Latitude,Survey_Station,SubRegion))%>%
  separate(Survey_Station,into=c("Survey","StationNum"),sep="_",remove = F)%>%
  mutate_at(c("Survey_Station","Survey","StationNum"),as.factor)



coord_ranges <- stations_compare_long%>%group_by(StationNum)%>%
  summarize(n.surveys =n(),
            Lat_Min=min(Station_Latitude),
            Lat_Max=max(Station_Latitude),
            Lon_Min=min(Station_Longitude),
            Lon_Max=max(Station_Longitude))%>%
  mutate(Lat_Diff = round(Lat_Max-Lat_Min,3),
         Lon_Diff = round(Lon_Max- Lon_Min,3),
         Sum_Diff = abs(Lon_Diff)+abs(Lat_Diff))%>%filter(n.surveys>1)%>%
  arrange(Sum_Diff)%>%filter(Sum_Diff>0.009)



all.stations <- stations_compare_long%>%filter(StationNum %in% coord_ranges$StationNum) %>%
                                             st_as_sf( coords = c("Station_Longitude", "Station_Latitude"), 
                  crs = 4326, agr = "constant")

all.stations <- stations_compare_long%>%
  st_as_sf( coords = c("Station_Longitude", "Station_Latitude"), 
            crs = 4326, agr = "constant")


stations.1 <- stations_compare_long%>%filter(StationNum %in% coord_ranges$StationNum) %>%
  st_as_sf( coords = c("Station_Longitude", "Station_Latitude"), 
            crs = 4326, agr = "constant")%>%
  filter(as.numeric(as.character(StationNum))<605)


stations.2 <- stations_compare_long%>%filter(StationNum %in% coord_ranges$StationNum) %>%
  st_as_sf( coords = c("Station_Longitude", "Station_Latitude"), 
            crs = 4326, agr = "constant")%>%
  filter(as.numeric(as.character(StationNum))>604&as.numeric(as.character(StationNum))<715)

stations.3 <- stations_compare_long%>%filter(StationNum %in% coord_ranges$StationNum) %>%
  st_as_sf( coords = c("Station_Longitude", "Station_Latitude"), 
            crs = 4326, agr = "constant")%>%
  filter(as.numeric(as.character(StationNum))>714&as.numeric(as.character(StationNum))<850)


stations.4 <- stations_compare_long%>%filter(StationNum %in% coord_ranges$StationNum) %>%
  st_as_sf( coords = c("Station_Longitude", "Station_Latitude"), 
            crs = 4326, agr = "constant")%>%
  filter(as.numeric(as.character(StationNum))>850&as.numeric(as.character(StationNum))<1000)

pdf(file="Maps/Station Comparison Maps.pdf",10,10)


#Plot station location discrepancies 1
ggplot()+geom_sf(data = marsh, size = .5, color = "black", fill = "aquamarine2") + 
  geom_sf(data = EDSM_Strata, size = 1, color = "darkgreen",aes(fill=Region),alpha=.25)+
  geom_sf_label(data=EDSM_Strata,aes(label=SubRegion,color=Region))+
  ylim(c(38,38.6))+xlim(c(-122.1,-121.3))





  ggtitle("Stations 323-519") + 
  geom_sf(data = stations.1, size = 4,  aes(col=StationNum,shape = Survey,fill=StationNum), alpha=.75)+
  coord_sf(xlim = c(-122.33, -121.93), ylim = c(38, 38.25), expand = FALSE)+
  scale_shape_manual(values=c(21,22,23,24,25))+
  ggspatial::annotation_scale(location = 'br')


#Plot station location discrepancies 2
ggplot()+geom_sf(data = marsh, size = .5, color = "black", fill = "aquamarine2") + 
  geom_sf(data = EDSM_Strata, size = 1, color = "darkgreen",fill=NA) + 
  ggtitle("Stations 703-711") + 
  geom_sf(data = stations.2, size = 4,  aes(col=StationNum,shape = Survey,fill=StationNum), alpha=.75)+
  coord_sf(xlim = c(-121.85, -121.6), ylim = c(38, 38.2), expand = FALSE)+
  scale_shape_manual(values=c(21,22,23,24,25))+
  ggspatial::annotation_scale(location = 'tl')

#Plot station location discrepancies 3
ggplot()+geom_sf(data = marsh, size = .5, color = "black", fill = "aquamarine2") + 
  geom_sf(data = EDSM_Strata, size = 1, color = "darkgreen",fill=NA) + 
  ggtitle("Stations 716-815") + 
  geom_sf(data = stations.3, size = 4,  aes(col=StationNum,shape = Survey,fill=StationNum), alpha=.75)+
  coord_sf(xlim = c(-121.9, -121.5), ylim = c(38, 38.3), expand = FALSE)+
  scale_shape_manual(values=c(21,22,23,24,25))+
  ggspatial::annotation_scale(location = 'tl')

#Plot station location discrepancies 4
ggplot()+geom_sf(data = marsh, size = .5, color = "black", fill = "aquamarine2") + 
  geom_sf(data = EDSM_Strata, size = 1, color = "darkgreen",fill=NA) + 
  ggtitle("Stations 901-915") + 
  geom_sf(data = stations.4, size = 4,  aes(col=StationNum,shape = Survey,fill=StationNum), alpha=.75)+
  coord_sf(xlim = c(-121.7, -121.45), ylim = c(37.9, 38.1), expand = FALSE)+
  scale_shape_manual(values=c(21,22,23,24,25))+
  ggspatial::annotation_scale(location = 'bl')

dev.off()

#===========Map All Stations==========================


all.stations <- stations_compare_long%>%
  st_as_sf( coords = c("Station_Longitude", "Station_Latitude"), 
            crs = 4326, agr = "constant")


ggplot()+geom_sf(data = marsh, size = .5, color = "black", fill = "aquamarine2") + 
  geom_sf(data = EDSM_Strata, size = 1, color = "darkgreen",fill=NA) + 
  ggtitle("Stations 901-915") + 
  geom_sf(data = all.stations%>%filter(), size = 4,  aes(col=Survey,shape = Survey,fill=Survey), alpha=.75)+
  coord_sf(xlim = c(-122.5, -121.25), ylim = c(37.5, 38.6), expand = FALSE)+
  scale_shape_manual(values=c(21,22,23,24,25))+
  ggspatial::annotation_scale(location = 'bl')


#=========Map stations without EDSM Region
NASR.stations <- stations_compare_long%>%filter(is.na(SubRegion)==T)%>%
  st_as_sf( coords = c("Station_Longitude", "Station_Latitude"), 
            crs = 4326, agr = "constant")


ggplot()+geom_sf(data = marsh, size = .5, color = "black", fill = "aquamarine2") + 
  geom_sf(data = EDSM_Strata, size = 1, color = "darkgreen",aes(fill=Region),alpha=.25) + 
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




