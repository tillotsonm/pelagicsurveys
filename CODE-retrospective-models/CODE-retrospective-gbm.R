#===========Initial exploration of catch data using GBM==============
#====================================================================
#Prepared by Michael Tillotson
#ICF
#Started March 22, 2021
require(stringr)
require(gbm)
require(dismo)
require(tidyverse)
require(randomForest)
require(lubridate)


setwd("C:/Users/40545/Documents/GitHub/pelagicsurveys")

load("TidyData/DATA_All_Surveys_Tidy.rda")
load("TidyData/DATA_Hydrology_tidy.rda")
region_classifiers <- read_csv("SpatialData/station_join_depth_strata_edsm.csv",
                               col_types = "ddfffff")%>%
  rename("StationCode" = "StationNum")%>%select(-c(Station_Longitude,Station_Latitude))






Working <-  All_Surveys_Species %>%
  mutate(CommonName = recode(CommonName,"Age-0 Striped Bass" = "Striped Bass Age 0",
                             "Age 0-Striped Bass" = "Striped Bass Age 0",
                             "Age 1-Striped Bass" = "Striped Bass Age 1",
                             "Age 2-Striped Bass" = "Striped Bass Adult",
                             "Striped Bass Age-2" = "Striped Bass Adult",
                             "Striped Bass Age-3+" = "Striped Bass Adult",
                             "Striped Bass Age-0" = "Striped Bass Age 0",
                             "Striped Bass Age-1" = "Striped Bass Age 1",
                             "Striped Bass Age 2" = "Striped Bass Adult",
                             "Age-0 Striped Bass" = "Striped Bass Age 0",
                             "Age-1 Striped Bass" = "Striped Bass Age 1",
                             "Age-2 Striped Bass" = "Striped Bass Adult",
                             "Striped Bass" = "Striped Bass Age-0",
                             "Crangon Shrimp" = "Crangon",
                             "Palaemon_Spp." = "Palaemon"))%>%
  group_by(SampleDate,Survey_Station,CommonName)%>%
  mutate(RawCatch =sum(RawCatch),
         Mean_Length = weighted.mean(Mean_Length,RawCatch,na.rm = T))%>%
  ungroup()%>%distinct(across(c(SampleDate,Survey_Station,CommonName)),.keep_all = T)%>%
  select(c(1:21,ForkLength,WeightingFactor,CommonName,Mean_Length,RawCatch))%>%
  group_by(Survey_Station)%>%
  mutate(SpeciesAllTime = length(unique(CommonName)))%>%
  ungroup()%>%group_by(Survey_Station,Year)%>%
  mutate(SpeciesperYear = length(unique(CommonName)))%>%
  ungroup()%>%group_by(Survey_Station)%>%
  mutate(MeanSpeciesperYear = mean(SpeciesperYear))%>%
  select(-c(TowNumber,TemperatureBottom,SurveyNumber,JulianDay))


#Set a threshold for how common a species should be to remain in the dataset
#Currenlty using 0.1% representation of all-time catch as cutoff
SpecRanks <- Working %>% group_by(CommonName)%>%
  summarise(AllTime=round(sum(RawCatch)/sum(Working$RawCatch),3))%>%
  arrange(-AllTime)%>%filter(AllTime>0)


Data_Species <- Working %>% ungroup()%>%rename(Length = Mean_Length, Catch = RawCatch)%>%
  filter(CommonName %in% SpecRanks$CommonName) %>%
  mutate(CommonName = str_replace_all(CommonName, " ", "_"))%>%
  mutate(OrganismCategory = "Fish")%>%
  mutate(OrganismCategory = if_else(CommonName %in% c("Jellyfish","Maeotias","Polyorchis","Comb_Jelly_Or_Sea_Goosberry"),
                                    "Gelatinous",OrganismCategory))%>%
  mutate(OrganismCategory = if_else(CommonName %in% c("Siberian_Prawn","Crangon","Palaemon"),
                                    "Shrimp",OrganismCategory))%>%
  mutate(StationCode = if_else(StationCode=="724"&SurveySeason=="20mm","724.1",as.character(StationCode)))%>%
  mutate(StationCode = if_else(StationCode=="901"&SurveySeason=="STN","901.1",as.character(StationCode)))%>%
  mutate(StationCode = if_else(StationCode=="901"&SurveySeason=="20mm","901.2",as.character(StationCode)))%>%
  mutate(StationCode = if_else(StationCode=="915"&SurveySeason=="FMWT","915.1",as.character(StationCode)))%>%
  mutate(StationCode = if_else(StationCode=="914"&SurveySeason=="FMWT","914.1",as.character(StationCode)))%>%
  mutate(StationCode = if_else(StationCode=="705"&SurveySeason=="FMWT","705.1",as.character(StationCode)))%>%
  mutate(StationCode = if_else(StationCode=="703"&SurveySeason=="FMWT","703.1",as.character(StationCode)))%>%
  mutate(StationCode = if_else(StationCode=="323"&SurveySeason=="FMWT","323.1",as.character(StationCode)))%>%
  mutate(StationCode = if_else(StationCode=="501"&SurveySeason=="FMWT","501.1",as.character(StationCode)))%>%
  mutate(StationCode = as.factor(StationCode))



Data_Wide <- Data_Species %>% ungroup()%>%
  pivot_wider(names_from = CommonName,names_sep = "_",values_from = c(Catch,Length))%>%
  select(-c(ForkLength,WindDirection))%>%filter(is.na(Station_Latitude)==FALSE)%>%
  mutate_at(vars(Catch_Shiner_Perch:Catch_Inland_Silverside), ~replace_na(., 0))%>%
  mutate_at(vars(Length_Shiner_Perch:Length_Inland_Silverside), ~replace_na(., NA))%>%
  select(-c(Length_Shiner_Perch:Length_Inland_Silverside))%>%
  mutate(TotalCatch = rowSums(select(.,Catch_Shiner_Perch:Catch_Inland_Silverside)),
         ShrimpCatch = rowSums(select(.,c(Catch_Siberian_Prawn,Catch_Crangon,Catch_Palaemon))),
         JellyCatch = rowSums(select(.,c(Catch_Jellyfish,
                                         Catch_Maeotias,
                                         Catch_Polyorchis,
                                         Catch_Comb_Jelly_Or_Sea_Goosberry))),
         FishCatch = TotalCatch - ShrimpCatch - JellyCatch)%>%
  left_join(region_classifiers,by=c("StationCode"))%>%
  relocate(c(Region:category),.after = StationCode)%>%rename("Depth_Cat" = "category")%>%
  
  #===================Important Filtering Steps to Review===============================
filter(Year>1999 & is.na(Region)==F)
  

  
Data_Wide <- Data_Wide %>% mutate(SurveySeason = as.factor(SurveySeason))%>%data.frame()
  
catch.gbm <-   gbm.step(data=Data_Wide,
           gbm.y=65,
           gbm.x=c(1,4,8,9,13,14,15),
           family = "gaussian",
           tree.complexity=5,
           learning.rate=0.01,
           bag.fraction = 0.5,
           n.trees=100,
           step.size = 500,
           tolerance.method = "fixed")

gbm.plot(first.gmb)

summary(first.gmb)
Anchovy_gbm <-   gbm.step(data=Data_Wide,
                        gbm.y=28,
                        gbm.x=c(1,4,8,9,13,14,15),
                        family = "gaussian",
                        tree.complexity=5,
                        learning.rate=0.01,
                        bag.fraction = 0.5,
                        n.trees=100,
                        step.size = 100,
                        tolerance.method = "fixed")

gbm.plot(Anchovy_gbm)

SB0_gbm <-   gbm.step(data=Data_Wide,
                          gbm.y=34,
                          gbm.x=c(1,4,8,9,13,14,15),
                          family = "gaussian",
                          tree.complexity=5,
                          learning.rate=0.01,
                          bag.fraction = 0.5,
                          n.trees=100,
                          step.size = 500,
                          tolerance.method = "fixed")

plot.gbm

names(Data_Wide)

hist(Data_Wide$TemperatureTop)

splittail_gbm <-   gbm.step(data=Data_Wide,
                      gbm.y=36,
                      gbm.x=c(1,4,8,9,13,14,15),
                      family = "gaussian",
                      tree.complexity=5,
                      learning.rate=0.05,
                      bag.fraction = 0.5,
                      n.trees=100,
                      step.size = 500,
                      tolerance.method = "fixed")

gbm.plot(splittail_gbm)
#==============Time-series trend plots for various catch aggregations===================================
  
Data_Wide.plot %>% group_by(Year,SurveySeason) %>% summarize(Catch = sum(TotalCatch),Tows=n())%>%
  mutate(CPT = log10(Catch/Tows))%>%ggplot(aes(x=Year,y=CPT,col=SurveySeason, fill=SurveySeason))+
  geom_line(size=1)+theme_bw()+
  scale_color_viridis_d()+geom_smooth()+scale_fill_viridis_d()+facet_grid(~SurveySeason)+ylab("log10(Fish Catch/Tow")+
  ggtitle("Average Total Catch/Tow (Species comprising > 0.1% of Catch)")

Data_Wide.plot %>% group_by(Year,SurveySeason) %>% summarize(Catch = sum(FishCatch),Tows=n())%>%
  mutate(CPT = log10(Catch/Tows))%>%ggplot(aes(x=Year,y=CPT,col=SurveySeason, fill=SurveySeason))+
  geom_line(size=1)+theme_bw()+
  scale_color_viridis_d()+geom_smooth()+scale_fill_viridis_d()+facet_grid(~SurveySeason)+ylab("log10(Fish Catch/Tow")+
  ggtitle("Average Fish Catch/Tow (Species comprising > 0.1% of Catch)")
                                           

Data_Wide.plot %>% group_by(Year,SurveySeason) %>% summarize(Catch = sum(ShrimpCatch),Tows=n())%>%
  mutate(CPT = log10(Catch/Tows))%>%ggplot(aes(x=Year,y=CPT,col=SurveySeason, fill=SurveySeason))+
  geom_line(size=1)+theme_bw()+
  scale_color_viridis_d()+geom_smooth()+scale_fill_viridis_d()+facet_grid(~SurveySeason)+ylab("log10(Fish Catch/Tow")+
  ggtitle("Average Shrimp Catch/Tow (Species comprising > 0.1% of Catch)")


Data_Wide.plot %>% group_by(Year,SurveySeason) %>% summarize(Catch = sum(JellyCatch),Tows=n())%>%
  mutate(CPT = log10(Catch/Tows))%>%ggplot(aes(x=Year,y=CPT,col=SurveySeason, fill=SurveySeason))+
  geom_line(size=1)+theme_bw()+
  scale_color_viridis_d()+geom_smooth()+scale_fill_viridis_d()+facet_grid(~SurveySeason)+ylab("log10(Fish Catch/Tow")+
  ggtitle("Average Gelatenous Catch/Tow (Species comprising > 0.1% of Catch)")



Data_Wide %>% group_by(Year,SurveySeason) %>% summarize(Catch = sum(Catch_Delta_Smelt),Tows=n())%>%
  mutate(CPT = log10(Catch/Tows))%>%ggplot(aes(x=Year,y=CPT,col=SurveySeason, fill=SurveySeason))+
  geom_line(size=1)+theme_bw()+
  scale_color_viridis_d()+geom_smooth()+scale_fill_viridis_d()+facet_grid(~SurveySeason)+ylab("log10(Fish Catch/Tow")+
  ggtitle("Average Delta Smelt Catch/Tow (Species comprising > 0.1% of Catch)")



names(Data_Wide)
