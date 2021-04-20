#Prepared by Michael Tillotson
#ICF
#Created April 8, 2021

#Load libraries:
require(ggforce)
require(tidyverse)
require(lubridate)
require(ggridges)
require(vegan)
require(ggbiplot)
theme_set(theme_bw())


#Set working directory
setwd("C:/Users/40545/Documents/GitHub/pelagicsurveys")


load("MASTER_Data/MASTER_Env_Hydro.rda")

ClusteringData <- Environment_Hydrology %>% 
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
  mutate(Monthly_Surveys)





ClusteringData %>% group_by(Survey,StationCode,Month)%>%
  dplyr::summarise(n())%>%view()

length(unique(ClusteringData$StationCode))


plot(ClusteringData$Conductivity,ClusteringData$Salinity)

dat <- ClusteringData%>%select(c(Salinity,Conductivity))%>%drop_na()




Env_Summary <-
  Environment_Hydrology %>% 
  filter(WaterYear>1970)%>%
  mutate(DOWW = yday(Date-92))%>%
  mutate(WYWeek = ceiling(DOWW/7)) %>%
  group_by(SacWYType,Month,SubRegion)%>%
  dplyr::summarise(N_Samples = n(),
            Temperature_Mean = mean(Temperature,na.rm=T),
            Secchi_Mean = mean(Secchi,na.rm=T),
            Conductivity_Mean = mean(Conductivity,na.rm=T),
            Depth_Mean = mean(Depth,na.rm=T),
            Temperature_SD = sd(Temperature,na.rm=T),
            Secchi_SD = sd(Secchi,na.rm=T),
            Conductivity_SD = sd(Conductivity,na.rm=T),
            Depth_SD = sd(Depth,na.rm=T),
            Date = min(Date))%>%
  filter(N_Samples>4)%>%
  drop_na()%>%
  ungroup()



for(i in 1:12){
PCA_Matrix <- Env_Summary %>% filter(Month==i)%>%
  select(Temperature_Mean:Depth_SD)%>%decostand("standardize")%>%select(-c(Depth_Mean,Depth_SD))
  
Regions <- Env_Summary%>%filter(Month==i)%>%select(SubRegion)%>%with(.,as.vector(SubRegion))

PCA_Test <- prcomp(PCA_Matrix,groups=Regions)

print(ggbiplot(PCA_Test,ellipse=T,groups=Regions)+ggtitle(paste("Environment Biplot ",i)))

}

env.dist <- Env_Summary %>%
  select(Temperature_Mean:Depth_SD)%>%
  select(-c(Depth_Mean,Depth_SD))%>%
  vegdist("bray")


PMN1 <- adonis2(env.dist~SubRegion*Month,data=Env_Summary,permutations = 100)




