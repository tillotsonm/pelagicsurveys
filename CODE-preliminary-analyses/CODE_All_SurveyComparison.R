#====================================================================
#===

#Prepared by Michael Tillotson
#ICF
#Created April 1, 2021

#Load libraries:
require(ggforce)
require(tidyverse)
require(lubridate)
require(ggridges)
library("sf")
library("rnaturalearth")
library("rnaturalearthdata")
library("rnaturalearthhires")
library("stringr")
library("RColorBrewer")
library("spatialEco")
library("vegan")
library("lubridate")
select <- dplyr::select
filter <- dplyr::filter
theme_set(theme_bw())

#Set working directory
setwd("C:/Users/40545/Documents/GitHub/pelagicsurveys")

#==================================================================================


load("MASTER_Data/MASTER_Env_Hydro.rda")
load("MASTER_Data/MASTER_All_Surveys.rda")
load("MASTER_Data/MASTER_All_Surveys_LF.rda")
load("MASTER_Data/MASTER_CDFWSurveys_Long_Format.rda")
load("FINAL_REVIEW_DATA/CDFW_Pelagic_Review_Data.rda")

#===================================================================================
#Load mapping stuff
Review_Strata <- read_csv("CODE-data-tidying/Review_Strata.csv",col_types = "fff")

marsh <- st_read(
  "CODE-spatial-analysis/hydro-delta-marsh/hydro_delta_marsh.shp")

EDSM_Strata_Temp <- st_read(
  "CODE-spatial-analysis/DSm_Subregions_UTM10NAD83/DSm_Subregions_UTM10NAD83.shp")

region_lookup <- Review_Data_By_Station %>% 
  select(Review_Region,Region,SubRegion,Review_Stratum)%>%
  distinct()%>%
  mutate(Review_Stratum = recode(Review_Stratum,"Napa River*" = "Napa River"))

EDSM_Strata<- EDSM_Strata_Temp %>%
  left_join(region_lookup,by=c("Region","SubRegion"))%>%
  mutate(Review_Stratum = replace_na(Review_Stratum,"South"))%>%
  mutate(Review_Region = replace_na(Review_Region,"South"))%>%  
  select(-c(OBJECTID,OBJECTID_1,Shape_Leng,Shape_Area))




#Add Season Variable===============================================

Environment_Hydrology <- Environment_Hydrology %>%
  mutate(Season = as.factor(recode(as.character(Month),
                         "1" = "Winter",
                         "2" = "Winter",
                         "3" = "Winter",
                         "4" = "Spring",
                         "5" = "Spring",
                         "6" = "Spring",
                         "7" = "Summer",
                         "8" = "Summer",
                         "9" = "Summer",
                         "10"= "Fall" ,
                         "11"= "Fall" ,
                         "12"= "Fall" ,
                         )))%>%
  mutate(SacWYType = factor(SacWYType, levels = c("W","AN","BN","D","C")))%>%
  mutate(Season = factor(Season, levels = c("Winter","Spring","Summer","Fall")))%>%
  mutate(SubRegion = as.factor(if_else(Latitude<37.8 & Longitude>-122,
                             "Upper San Joaquin River",
                             as.character(SubRegion))),
         Region = as.factor(if_else(Latitude<37.8 & Longitude>-122,
                          "South",
                          as.character(Region)
                          )
                          )
         )%>%
  group_by(SubRegion)%>%
  mutate(Mean_Cond = mean(Conductivity,na.rm=T))%>%
  ungroup()%>%
  arrange(-Mean_Cond)%>%
  mutate(SubRegion=factor(SubRegion, levels=unique(SubRegion)))



All_Surveys <- All_Surveys_Master %>% 
  #Arrange subregion by salinity
  mutate(Subregion = factor(SubRegion, levels = levels(Environment_Hydrology$SubRegion)))%>%
  mutate(Season = as.factor(recode(as.character(Month),
                                   "1" = "Winter",
                                   "2" = "Winter",
                                   "3" = "Winter",
                                   "4" = "Spring",
                                   "5" = "Spring",
                                   "6" = "Spring",
                                   "7" = "Summer",
                                   "8" = "Summer",
                                   "9" = "Summer",
                                   "10"= "Fall" ,
                                   "11"= "Fall" ,
                                   "12"= "Fall" ,)
                            )
         )%>%
  mutate(Season = factor(Season, levels = c("Winter","Spring","Summer","Fall")))%>%
  mutate(CommonName = recode(CommonName,
                             "Mississippi_Silverside" = "Inland_Silverside",
                             "Striped_Bass_Age_0" = "Striped_Bass",
                             "Striped_Bass_Age_1" = "Striped_Bass"))%>%
  left_join(Review_Strata, by="SubRegion")%>%relocate(Review_Region:Review_Stratum,.after=SubRegion)%>%
  mutate(SubRegion = if_else(SurveySeason=="FMWT",replace_na(as.character(SubRegion),"SF and Outer SP Bays"),as.character(SubRegion)))%>%
  mutate(SubRegion = as.factor(SubRegion))%>%
  mutate(Region = if_else(SubRegion == "SF and Outer SP Bays"&SurveySeason=="FMWT","Far West",as.character(Region)))%>%
  mutate(Region = as.factor(Region))%>%
  mutate(Review_Region = if_else(SurveySeason=="FMWT",replace_na(as.character(Review_Region),"SF and Outer SP Bays"),as.character(Review_Region)))%>%
  mutate(Review_Region = as.factor(Review_Region))%>%
  mutate(Review_Stratum = if_else(SurveySeason=="FMWT",replace_na(as.character(Review_Stratum),
                                                                  "San Pablo Bay and Carquinez Strait"),as.character(Review_Stratum)))%>%
  mutate(Review_Stratum = as.factor(Review_Stratum))
  

  #===List of target species
  Target_Species <- data.frame(CommonName = c("American_Shad",
                                              "Northern_Anchovy",
                                              "Pacific_Herring",
                                              "Starry_Flounder",
                                              "Striped_Bass",
                                              "Threadfin_Shad",
                                              "White_Sturgeon",
                                              "Crangon",
                                              "Chinook_Salmon",
                                              "Delta_Smelt",
                                              "Longfin_Smelt",
                                              "Steelhead",
                                              "White_Catfish",
                                              "Shokihaze_Goby",
                                              "Shimofuri_Goby",
                                              "Tridentiger_Spp.",
                                              "Prickly_Sculpin",
                                              "No_Catch"
                               ))
  
  All_Targets <- All_Surveys %>% 
    filter(CommonName %in% Target_Species$CommonName)%>%
    mutate(SurveyCategory = if_else(SurveySeason %in% c("SKT","FMWT","STN","20mm","SLS"),"CDFW","Other Pelagic"))%>%
    mutate(SurveyCategory = if_else(NetType=="Beach Seine","Beach Seine",SurveyCategory))%>%
    mutate(SurveyCategory = if_else(NetType=="Bottom Trawl","Bottom Trawl",SurveyCategory))
   
  
All_Surveys <- All_Surveys%>%mutate(Target = if_else(CommonName %in% Target_Species$CommonName,TRUE,FALSE))%>%
  filter(Year > 2001)
  
table(All_Surveys$SurveySeason,is.na(All_Surveys$Volume))
  
  #===Create tow-level, target species summary of all surveys and core surveys
  #These data will be used to generate catch maps
  
Target_Tows <-  All_Surveys %>%
  ungroup()%>%
  filter(is.na(Station_Latitude)==F)%>%
  #Add distinctions for fish vs. macroinverts
  mutate(OrganismCategory = "Other_Fish")%>%
  mutate(OrganismCategory = if_else(CommonName %in% c("Jellyfish",
                                                      "Maeotias",
                                                      "Polyorchis",
                                                      "Comb_Jelly_Or_Sea_Goosberry",
                                                      "Pleurobrachia_Jellyfish",
                                                      "Moon_Jelly",
                                                      "Jelly_(Unid)",
                                                      "Chrysaora_Fuscensens",
                                                      "Blackfordia_Virginica",
                                                      "Scrippsia_Pacifica",
                                                      "Lens_Jellyfish"
  ),
  "Gelatinous",OrganismCategory))%>%
  mutate(OrganismCategory = if_else(CommonName %in% c("Siberian_Prawn",
                                                      "Shrimp_(Unid)",
                                                      "Crangon",
                                                      "Palaemon",
                                                      "Mud_Shrimp",
                                                      "Dungeness_Crab"),
                                    "Other_Crustacean",OrganismCategory))%>%  
  mutate(CommonName = as.factor(if_else(CommonName %in% Target_Species$CommonName,CommonName,OrganismCategory)))%>%
  select(-c(Target,Season,Age0Cut,Age1Cut,OrganismCategory,ForkLength))%>%
  group_by(SurveySeason,SampleDate,StationCode,TowNumber,CommonName)%>%
  mutate(Catch = n())%>%
  ungroup()%>%
  distinct(across(c(StationCode,SampleDate,TowNumber,CommonName,SurveySeason,Gear,NetType)),.keep_all = TRUE)%>%
  group_by(SampleDate,SurveySeason,StationCode,TowNumber,Gear,NetType)%>%mutate(Volume = mean(Volume))%>%
  ungroup()%>%
  pivot_wider(values_from = Catch,names_prefix="Catch_",names_from = CommonName)%>%
  mutate_at(vars(contains("Catch")), ~replace_na(., 0))%>%
  filter(Year>2002 & Year< 2020)%>%
  distinct(across(c(StationCode,SampleDate,TowNumber,SurveySeason,Gear)),.keep_all = TRUE)%>% 
  mutate(SurveyCategory = if_else(SurveySeason %in% c("SKT","FMWT","STN","20mm","SLS"),"CDFW","Other Pelagic"))%>%
  mutate(SurveyCategory = if_else(NetType=="Beach Seine","Beach Seine",SurveyCategory))%>%
  mutate(SurveyCategory = if_else(NetType=="Bottom Trawl","Bottom Trawl",SurveyCategory))%>%
  relocate(SurveyCategory, .after = Gear)
  



#================Create Tabular Report of Fish Lengths by Survey===========

Length_Summary <- All_Targets %>% filter(is.na(ForkLength)==F)%>%
  group_by(SurveyCategory,Core_Survey,CommonName)%>%
  summarise(N_Fish = n(),
            Length_Mean = mean(ForkLength,na.rm = T),
            Length_SD = sd(ForkLength,na.rm=T),
            Length_Min = min(ForkLength,na.rm = T),
            Length_Q1 =  quantile(ForkLength,na.rm = T,probs = .01),
            Length_Q5 =  quantile(ForkLength,na.rm = T,probs = .05),
            Length_Q25 = quantile(ForkLength,na.rm = T,probs = .25),
            Length_Median = median(ForkLength,na.rm = T),
            Length_Q75 = quantile(ForkLength,na.rm = T,probs = .75),
            Length_Q95 = quantile(ForkLength,na.rm = T,probs = .95),
            Length_Q99 = quantile(ForkLength,na.rm = T,probs = .99),
            Length_Max = max(ForkLength,na.rm=T)
            )%>%
  mutate_if(is.numeric,round,digits=0)%>%
  filter(Length_Min != Length_Max)


write_csv(Length_Summary,file="TemporaryOutputs/All Surveys Length Summary CDFW vs Others.csv")


#===Age Distinctions=================================================
pdf(file="TemporaryOutputs/Target Species Length Frequencies by Survey.pdf",width=16,height = 10)
All_Targets %>% 
  filter(is.na(ForkLength)==F & CommonName != "Crangon")%>%
  group_by(CommonName)%>%
  mutate(Length_95 = quantile(ForkLength,.99))%>%
  filter(ForkLength < Length_95)%>%
  ungroup()%>%
  group_by(SurveyCategory,CommonName)%>%
  mutate(LogCatch = log10(n()/17))%>%ungroup()%>%
  ggplot(aes(x=ForkLength,y=SurveyCategory,fill=LogCatch))+
  scale_fill_viridis_c(name="Log10 of mean annual catch", labels = scales::comma,alpha=.5)+
  stat_density_ridges(quantile_lines = TRUE,quantiles = c(.10,.5,.90),alpha=.5)+
  theme(legend.position = "bottom",legend.key.width=unit(4,"cm"))+
  ggtitle("Length-frequencies observed by CDFW and other fish surveys")+
  facet_wrap(~CommonName,scales = "free_x")+ylab("Survey Category")

dev.off()

#===========Barplots showing Target species catch per tow================================

Catch_Summary_Survey <- All_Targets %>%
  group_by(SurveySeason,SampleDate,StationCode,TowNumber,CommonName)%>%
  mutate(Catch = n())%>%
  ungroup()%>%
  distinct(across(c(StationCode,SampleDate,TowNumber,CommonName,SurveySeason)),.keep_all = TRUE)%>%
  select(-ForkLength)%>%
  pivot_wider(values_from = Catch,names_from = CommonName)%>%
  mutate_at(vars(Pacific_Herring:Tridentiger_Spp.), ~replace_na(., 0))%>%
  pivot_longer(Pacific_Herring:Tridentiger_Spp.,names_to = "CommonName", values_to = "Catch")%>%
  group_by(SurveySeason,SubRegion,CommonName)%>%
  summarise(MinCatch = min(Catch),
            Catch_Q5 =  quantile(Catch,na.rm = T,probs = .05),
            MaxCatch = max(Catch),
            Catch_Q95 =  quantile(Catch,na.rm = T,probs = .95),
            MeanCatch = mean(Catch),
            MedianCatch = median(Catch),
            TotalCatch = sum(Catch))%>%
  ungroup()

Catch_Summary_Core <- All_Targets %>%
  group_by(SurveySeason,SampleDate,StationCode,TowNumber,CommonName)%>%
  mutate(Catch = n())%>%
  ungroup()%>%
  distinct(across(c(StationCode,SampleDate,TowNumber,CommonName,SurveySeason)),.keep_all = TRUE)%>%
  select(-ForkLength)%>%
  pivot_wider(values_from = Catch,names_from = CommonName)%>%
  mutate_at(vars(Pacific_Herring:Tridentiger_Spp.), ~replace_na(., 0))%>%
  pivot_longer(Pacific_Herring:Tridentiger_Spp.,names_to = "CommonName", values_to = "Catch")%>%
  group_by(Core_Survey,SubRegion,CommonName)%>%
  summarise(MinCatch = min(Catch),
            Catch_Q5 =  quantile(Catch,na.rm = T,probs = .05),
            MaxCatch = max(Catch),
            Catch_Q95 =  quantile(Catch,na.rm = T,probs = .95),
            MeanCatch = mean(Catch),
            MedianCatch = median(Catch),
            TotalCatch = sum(Catch))%>%
  ungroup()

pdf(file="Target Species Mean Catch Frequencies.pdf",width=12,height = 8)
for(i in 1:5){
print(Catch_Summary_Survey %>%
  mutate(SubRegion = factor(SubRegion, levels = unique(Environment_Hydrology$SubRegion)))%>%
  filter(is.na(SubRegion)==F & CommonName %in% Target_Species$CommonName)%>%
  ggplot(aes(y=SubRegion,x=MeanCatch,fill=SurveySeason))+
  geom_bar(stat="identity")+
  facet_wrap_paginate(~CommonName,scales="free_x",ncol=2,nrow=2,page=i))+
    ggtitle("Target species catch/ tow by subregion since 2002")

print(Catch_Summary_Core %>%
  mutate(SubRegion = factor(SubRegion, levels = unique(Environment_Hydrology$SubRegion)))%>%
  filter(is.na(SubRegion)==F & CommonName %in% Target_Species$CommonName)%>%
  ggplot(aes(y=SubRegion,x=MeanCatch,fill=Core_Survey))+
  geom_bar(stat="identity")+
  facet_wrap_paginate(~CommonName,scales="free_x",ncol=2,nrow=2,page=i))+
  ggtitle("Target species catch/ tow by survey category since 2002")

}
dev.off()


#Total Catch
pdf(file="Target Species Total Catch Frequencies.pdf",width=12,height = 8)
for(i in 1:5){
  print(Catch_Summary_Survey %>%
          mutate(SubRegion = factor(SubRegion, levels = unique(Environment_Hydrology$SubRegion)))%>%
          filter(is.na(SubRegion)==F & CommonName %in% Target_Species$CommonName)%>%
          ggplot(aes(y=SubRegion,x=TotalCatch,fill=SurveySeason))+
          geom_bar(stat="identity")+
          facet_wrap_paginate(~CommonName,scales="free_x",ncol=2,nrow=2,page=i)+
          ggtitle("Target species catch/ tow by subregion since 2002"))
  
  print(Catch_Summary_Core %>%
          mutate(SubRegion = factor(SubRegion, levels = unique(Environment_Hydrology$SubRegion)))%>%
          filter(is.na(SubRegion)==F & CommonName %in% Target_Species$CommonName)%>%
          ggplot(aes(y=SubRegion,x=TotalCatch,fill=Core_Survey))+
          geom_bar(stat="identity")+
          facet_wrap_paginate(~CommonName,scales="free_x",ncol=2,nrow=2,page=i)+
          ggtitle("Target species catch/ tow by survey category since 2002"))
  
}
dev.off()




#Target species relative to all >100


pdf("TemporaryOutputs/All vs Target Species.pdf",width =16, height = 11)
Long_Master %>% 
  group_by(CommonName,SurveySeason,Target)%>%
  summarise(SurveyCatch = (n()))%>%ungroup()%>%
  group_by(CommonName,Target)%>%
  mutate(TotalCatch= sum(SurveyCatch))%>%filter(TotalCatch>100)%>%
  arrange(-TotalCatch)%>%ungroup()%>%
  mutate(CommonName = factor(CommonName,levels=unique(CommonName)),
         `Focal Species` = Target)%>%
  ggplot(aes(y=CommonName,x=SurveyCatch,fill=`Focal Species`))+
  geom_bar(stat="identity")+xlab("Total catch since 2002")+xlim(c(0,610000))+
  geom_text(aes(y=1,x=150000,label="*1.5 Million"))


Long_Master %>% 
  group_by(CommonName,SurveySeason,Target)%>%
  summarise(RawSurvCatch = n())%>%
  mutate(SurveyCatch = log10(RawSurvCatch))%>%ungroup()%>%
  arrange(-RawSurvCatch)%>%ungroup()%>%
  mutate(CommonName = factor(CommonName,levels=unique(CommonName)),
         `Focal Species` = Target)%>%
  group_by(CommonName,Target)%>%
  mutate(TotalCatch= sum(SurveyCatch),
         filt = sum(RawSurvCatch))%>%
  filter(filt>100)%>%
  ggplot(aes(y=CommonName,x=SurveyCatch,fill=`Focal Species`))+facet_grid(~SurveySeason)+
  geom_bar(stat="identity")+xlab("Log10 of total catch since 2002")

dev.off()


#==============Comparison of Effort


