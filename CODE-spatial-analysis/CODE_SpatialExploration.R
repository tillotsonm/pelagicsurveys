library("ggplot2")
theme_set(theme_bw())
library("sf")
library("tidyverse")
library("rnaturalearth")
library("rnaturalearthdata")
library("rnaturalearthhires")


setwd("C:/Users/40545/Documents/GitHub/pelagicsurveys")




















stations_compare_wide <- read_csv("FMWTandSTNCoordinates.csv",
                                  col_types="fdddd")%>%
  distinct()%>%drop_na()


loc_diffs <- stations_compare_wide%>%
  mutate(Lon_dif = round(longitude_FMWT-longitude_STN,2),
         Lat_dif = round(latitude_FMWT-latitude_STN,2))%>%
  arrange(Lon_dif)%>%
  mutate(is.off =if_else(Lat_dif==0&Lon_dif==0,0,1))

off.stations <- loc_diffs %>% select(StationCode, is.off)


stations_compare <- stations_compare_wide %>% pivot_longer(!StationCode, 
               names_to = c(".value", "Survey"), 
               names_sep = "_", 
               values_drop_na = TRUE)%>%
  left_join(off.stations,by="StationCode")%>%
  filter(is.off==1)%>%select(-is.off)%>%
  rename(ID = StationCode)%>%distinct()%>%arrange(ID)

stations <- st_as_sf(stations_compare, coords = c("longitude", "latitude"), 
                  crs = 4326, agr = "constant")


world <- ne_download(scale=10,type="coastline",category = "physical",
                     returnclass = "sf")


world %>% ggplot()+geom_sf()+
  geom_sf(data = stations, size = 4,  aes(col = ID,shape = Survey,))+
  coord_sf(xlim = c(-122.4, -121.4), ylim = c(37.92, 38.15), expand = FALSE)


names(stations)


