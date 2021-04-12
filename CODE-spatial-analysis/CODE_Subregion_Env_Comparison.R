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

Environment_Hydrology %>% distinct(across(c(WaterYear,SacWYType)))%>%with(.,table(SacWYType))


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

Env_Summary%>%
  ggplot(aes(x=Date,y=Secchi_SD,col=SubRegion))+
  geom_point()+geom_smooth()+ylim(c(0,300))


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

PWPMN1 <- pairwise.adonis(Env_Summary[,5:12],Env_Summary[,3])

unique(as.character(Env_Summary[,3]))
