#====================================================================
#===Code to for maintaining Master data versions including 
#1) long format with all variables
#2) tow-level with all variables, counts and mean lengths
#)  tow-level with presence/absence binary
#3) station-level with all variables
#4) community matrix with all species


#Prepared by Michael Tillotson
#ICF
#Created March 24, 2021

#Load libraries:
require(ggforce)
require(tidyverse)


#Set working directory
setwd("C:/Users/40545/Documents/GitHub/pelagicsurveys")

load("MASTER_Data/MASTER_Env_Hydro.rda")
load("MASTER_Data/MASTER_Station.rda")
load("MASTER_Data/MASTER_Tow_Catch.rda")
load("MASTER_Data/MASTER_Long_Format.rda")

LTMR_Clusters <- read_csv("CODE -station-ranking/Complete_Rankings.csv",
                          col_types = "fddff")%>%
  mutate(LTMR_Only = if_else(is.na(TotalRank),TRUE,FALSE))%>%
  mutate(No_LTMR = if_else(is.na(LTMR_Demersal)==T&is.na(LTMR_OpenWater)==T,TRUE,FALSE))


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
  mutate(Season = factor(Season, levels = c("Winter","Spring","Summer","Fall")))


Long_Master <- Environment_Hydrology %>% select(c(Date,SacWYType,SJWYType,StationCode,Season))%>%
  distinct()%>%
  rename("SampleDate"="Date")%>%
  right_join(Long_Master,by=c("StationCode","SampleDate"))


#=======================Conductivity Comparison Figures============================

#Variation in Conductivity across EDSM SubRegions 

Environment_Hydrology%>%
  group_by(SubRegion)%>%
  mutate(Mean_Cond = mean(Conductivity,na.rm=T))%>%
  ungroup()%>%
  arrange(-Mean_Cond)%>%
  mutate(SubRegion=factor(SubRegion, levels=unique(SubRegion)))%>%
  filter(Conductivity<45000 & is.na(SacWYType)==F)%>%ggplot(aes(x=Conductivity,fill=SacWYType))+
  geom_density(alpha=0.25)+facet_wrap(~SubRegion,scales = "free_y")+
  theme_bw()+scale_fill_viridis_d()

#Variation in Conductivity across EDSM SubRegions in Jan-March

Environment_Hydrology%>%
  group_by(SubRegion)%>%
  mutate(Mean_Cond = mean(Conductivity,na.rm=T))%>%
  ungroup()%>%
  arrange(-Mean_Cond)%>%
  mutate(SubRegion=factor(SubRegion, levels=unique(SubRegion)))%>%
  filter(Conductivity<45000 & is.na(SacWYType)==F & Month<4)%>%ggplot(aes(x=Conductivity,fill=SacWYType))+
  geom_density(alpha=0.25)+facet_wrap(~SubRegion,scales = "free_y")+
  theme_bw()+scale_fill_viridis_d()


#Variation in Conductivity across EDSM SubRegions in Apr-Jun

Environment_Hydrology%>%
  group_by(SubRegion)%>%
  mutate(Mean_Cond = mean(Conductivity,na.rm=T))%>%
  ungroup()%>%
  arrange(-Mean_Cond)%>%
  mutate(SubRegion=factor(SubRegion, levels=unique(SubRegion)))%>%
  filter(Conductivity<45000 & is.na(SacWYType)==F & Month<7&Month>3)%>%ggplot(aes(x=Conductivity,fill=SacWYType))+
  geom_density(alpha=0.25)+facet_wrap(~SubRegion,scales = "free_y")+
  theme_bw()+scale_fill_viridis_d()


#Variation in Conductivity across EDSM SubRegions in Jul-Sep

Environment_Hydrology%>%
  group_by(SubRegion)%>%
  mutate(Mean_Cond = mean(Conductivity,na.rm=T))%>%
  ungroup()%>%
  arrange(-Mean_Cond)%>%
  mutate(SubRegion=factor(SubRegion, levels=unique(SubRegion)))%>%
  filter(Conductivity<45000 & is.na(SacWYType)==F & Month<10&Month>6)%>%ggplot(aes(x=Conductivity,fill=SacWYType))+
  geom_density(alpha=0.25)+facet_wrap(~SubRegion,scales = "free_y")+
  theme_bw()+scale_fill_viridis_d()


#Variation in Conductivity across EDSM SubRegions in Oct-Dec

Environment_Hydrology%>%
  group_by(SubRegion)%>%
  mutate(Mean_Cond = mean(Conductivity,na.rm=T))%>%
  ungroup()%>%
  arrange(-Mean_Cond)%>%
  mutate(SubRegion=factor(SubRegion, levels=unique(SubRegion)))%>%
  filter(Conductivity<45000 & is.na(SacWYType)==F &  Month>9)%>%ggplot(aes(x=Conductivity,fill=SacWYType))+
  geom_density(alpha=0.25)+facet_wrap(~SubRegion,scales = "free_y")+
  theme_bw()+scale_fill_viridis_d()




#=================Repeat as Boxplots================================================

Environment_Hydrology%>%
  group_by(SubRegion)%>%
  mutate(Mean_Cond = mean(Conductivity,na.rm=T))%>%
  ungroup()%>%
  arrange(-Mean_Cond)%>%
  mutate(SubRegion=factor(SubRegion, levels=unique(SubRegion)))%>%
  filter(Conductivity<45000 & is.na(SacWYType)==F)%>%
  ggplot(aes(y=SubRegion,x=Conductivity,fill=Season))+geom_boxplot()+
  facet_grid(~SacWYType)+scale_fill_viridis_d()
  
Environment_Hydrology%>%
  group_by(SubRegion)%>%
  mutate(Mean_Cond = mean(Conductivity,na.rm=T))%>%
  ungroup()%>%
  arrange(-Mean_Cond)%>%
  mutate(SubRegion=factor(SubRegion, levels=unique(SubRegion)))%>%
  filter(Conductivity<45000 & is.na(SacWYType)==F)%>%
  ggplot(aes(y=SubRegion,x=Temperature,fill=Season))+geom_boxplot()+
  facet_grid(~SacWYType)+scale_fill_viridis_d()+xlim(c(0,30))
  
  
Environment_Hydrology%>%
  group_by(SubRegion)%>%
  mutate(Mean_Cond = mean(Conductivity,na.rm=T))%>%
  ungroup()%>%
  arrange(-Mean_Cond)%>%
  mutate(SubRegion=factor(SubRegion, levels=unique(SubRegion)))%>%
  filter(Conductivity<45000 & is.na(SacWYType)==F)%>%
  ggplot(aes(y=SubRegion,x=Chlorophyll,fill=Season))+geom_boxplot()+
  facet_grid(~SacWYType)+scale_fill_viridis_d()+xlim(c(0,30))


Environment_Hydrology%>%
  group_by(SubRegion)%>%
  mutate(Mean_Cond = mean(Conductivity,na.rm=T))%>%
  ungroup()%>%
  arrange(-Mean_Cond)%>%
  mutate(SubRegion=factor(SubRegion, levels=unique(SubRegion)))%>%
  filter(Conductivity<45000 & is.na(SacWYType)==F)%>%
  ggplot(aes(y=SubRegion,x=Secchi,fill=Season))+geom_boxplot()+
  facet_grid(~SacWYType)+scale_fill_viridis_d()

#=======================Species Ranking Work=======================================

#Set a threshold for how common a species should be to remain in the dataset
#Currenlty using 0.1% representation of all-time catch as cutoff
SpecRanks_2000_LowSal <- Long_Master %>% 
  group_by(SubRegion)%>%
  mutate(Mean_Cond = mean(ConductivityTop,na.rm=T))%>%
  ungroup()%>%
  arrange(-Mean_Cond)%>%
  filter(OrganismCategory=="Fish"&Year>1999&Mean_Cond<5000)%>%
  add_tally()%>%
  group_by(CommonName)%>%
  summarise(Freq=round(n()/n,3))%>%distinct()%>%filter(Freq>0)%>%
  arrange(-Freq)%>%add_column(SalGroup = "Low", YearGroup="2000-2020")%>%
  ungroup()%>%
  mutate(Rank=rank(Freq))



SpecRanks_2000_MidSal <- Long_Master %>% 
  group_by(SubRegion)%>%
  mutate(Mean_Cond = mean(ConductivityTop,na.rm=T))%>%
  ungroup()%>%
  arrange(-Mean_Cond)%>%
  filter(OrganismCategory=="Fish"&Year>1999&Mean_Cond>5000&Mean_Cond<20000)%>%
  add_tally()%>%
  group_by(CommonName)%>%
  summarise(Freq=round(n()/n,3))%>%distinct()%>%filter(Freq>0)%>%
  arrange(-Freq)%>%add_column(SalGroup = "Mid", YearGroup="2000-2020")%>%
  ungroup()%>%
  mutate(Rank=rank(Freq))


SpecRanks_2000_HiSal <- Long_Master %>% 
  group_by(SubRegion)%>%
  mutate(Mean_Cond = mean(ConductivityTop,na.rm=T))%>%
  ungroup()%>%
  arrange(-Mean_Cond)%>%
  filter(OrganismCategory=="Fish"&Year>1999&Mean_Cond>0000)%>%
  add_tally()%>%
  group_by(CommonName)%>%
  summarise(Freq=round(n()/n,3))%>%distinct()%>%filter(Freq>0)%>%
  arrange(-Freq)%>%add_column(SalGroup = "High", YearGroup="2000-2020")%>%
  ungroup()%>%
  mutate(Rank=rank(Freq))

#====All Time

#Set a threshold for how common a species should be to remain in the dataset
#Currenlty using 0.1% representation of all-time catch as cutoff
SpecRanks_LowSal <- Long_Master %>% 
  group_by(SubRegion)%>%
  mutate(Mean_Cond = mean(ConductivityTop,na.rm=T))%>%
  ungroup()%>%
  arrange(-Mean_Cond)%>%
  filter(OrganismCategory=="Fish"&Mean_Cond<5000)%>%
  add_tally()%>%
  group_by(CommonName)%>%
  summarise(Freq=round(n()/n,3))%>%distinct()%>%filter(Freq>0)%>%
  arrange(-Freq)%>%add_column(SalGroup = "Low", YearGroup="All")%>%
  ungroup()%>%
  mutate(Rank=rank(Freq))


SpecRanks_MidSal <- Long_Master %>% 
  group_by(SubRegion)%>%
  mutate(Mean_Cond = mean(ConductivityTop,na.rm=T))%>%
  ungroup()%>%
  arrange(-Mean_Cond)%>%
  filter(OrganismCategory=="Fish"&Mean_Cond>5000&Mean_Cond<20000)%>%
  add_tally()%>%
  group_by(CommonName)%>%
  summarise(Freq=round(n()/n,3))%>%distinct()%>%filter(Freq>0)%>%
  arrange(-Freq)%>%add_column(SalGroup = "Mid", YearGroup="All")%>%
  ungroup()%>%
  mutate(Rank=rank(Freq))


SpecRanks_HiSal <- Long_Master %>% 
  group_by(SubRegion)%>%
  mutate(Mean_Cond = mean(ConductivityTop,na.rm=T))%>%
  ungroup()%>%
  arrange(-Mean_Cond)%>%
  filter(OrganismCategory=="Fish"&Mean_Cond>0000)%>%
  add_tally()%>%
  group_by(CommonName)%>%
  summarise(Freq=round(n()/n,3))%>%distinct()%>%filter(Freq>0)%>%
  arrange(-Freq)%>%add_column(SalGroup = "High", YearGroup="All")%>%
  ungroup()%>%
  mutate(Rank=rank(Freq))


All_Rankings <-SpecRanks_2000_LowSal%>%
  add_row(SpecRanks_2000_MidSal)%>%
  add_row(SpecRanks_2000_HiSal)%>%
  add_row(SpecRanks_LowSal)%>%
  add_row(SpecRanks_MidSal)%>%
  add_row(SpecRanks_HiSal)

Complete_Rankings <- All_Rankings %>% group_by(CommonName)%>%
  summarise(TotalRank = sum(Rank),N_Lists=n())%>%arrange(-TotalRank)%>%
  filter(CommonName != "Unknown" & N_Lists>1)


Working_Long <- Long_Master %>% filter(CommonName %in% c(Complete_Rankings$CommonName,"Crangon"))



Working_Tow <- Working_Long %>% 
  distinct(across(c(Survey_Station,SampleDate,TowNumber,CommonName)),.keep_all = TRUE)%>%
  select(-c(LengthFrequency,Family:Species,ForkLength:Dead,ReproductiveStage:Sex,OrganismCodeFMWT,OrganismCategory))%>%
  group_by(SubRegion)%>%
  mutate(Mean_Cond = mean(ConductivityTop,na.rm=T))%>%
  ungroup()%>%
  arrange(-Mean_Cond)%>%
  mutate(
    Depth_Cat = factor(Depth_Cat, levels = c("very deep","deep","intermediate","shallow")),
    SubRegion=factor(SubRegion, levels=unique(SubRegion)))


 Working_Tow %>% 
   ggplot(aes(x=Catch,y=SubRegion))+geom_boxplot()+theme_bw()+
   facet_wrap_paginate(~CommonName,scales="free_x",nrow=3,ncol=5,page=1)
  



# Number of Tows by Season, Subregion and Water Year Type
Working_Tow %>% 
  filter(is.na(Season)==F & is.na(SacWYType)==F)%>%
  group_by(SubRegion,Season,SacWYType)%>%
  mutate(N_Tows = n())%>%
  ungroup()%>%
  distinct(across(c(SubRegion,N_Tows,Season,SacWYType)))%>%
  ggplot(aes(y=SubRegion,x=N_Tows,col=Season,fill=Season))+geom_bar(stat="identity",position="dodge",alpha=0.5)+
  scale_color_viridis_d()+scale_fill_viridis_d()+facet_grid(~SacWYType)+theme_bw()
  

# Number of Tows by Season, Subregion and Depth Strata
Working_Tow %>% 
  filter(is.na(Season)==F & is.na(Depth_Cat)==F)%>%
  group_by(SubRegion,Season,Depth_Cat)%>%
  mutate(N_Tows = n())%>%
  ungroup()%>%
  distinct(across(c(SubRegion,N_Tows,Season,Depth_Cat)))%>%
  ggplot(aes(y=SubRegion,x=N_Tows,col=Season,fill=Season))+geom_bar(stat="identity",position="dodge",alpha=0.5)+
  scale_color_viridis_d()+scale_fill_viridis_d()+facet_grid(~Depth_Cat)+theme_bw()


# Catch Boxplots
Catch_Season_Region <-  Working_Tow %>% 
  filter(is.na(Season)==F)%>%
  ggplot(aes(x=Catch,y=SubRegion,fill=Season,col=Season))+geom_boxplot(alpha=.5)+theme_bw()+
  facet_wrap_paginate(~CommonName,scales="free_x",nrow=1,ncol=3)+
  scale_color_viridis_d()+
  scale_fill_viridis_d()
for(i in 1:5){
  print(Catch_Season_Region + facet_wrap_paginate(~CommonName,scales="free_x",nrow=2,ncol=3,page=i))
}


# Catch Barplots
Catch_Season_Region <-  Working_Tow %>% 
  filter(is.na(Season)==F)%>%
  ggplot(aes(x=Catch,y=SubRegion,fill=Season,col=Season))+geom_boxplot(alpha=.5)+theme_bw()+
  facet_wrap_paginate(~CommonName,scales="free_x",nrow=1,ncol=3)+
  scale_color_viridis_d()+
  scale_fill_viridis_d()
for(i in 1:5){
  print(Catch_Season_Region + facet_wrap_paginate(~CommonName,scales="free_x",nrow=2,ncol=3,page=i))
}