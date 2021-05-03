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
require(lubridate)


#Set working directory
setwd("C:/Users/40545/Documents/GitHub/pelagicsurveys")

load("MASTER_Data/MASTER_Env_Hydro.rda")
load("MASTER_Data/MASTER_Station.rda")
load("MASTER_Data/MASTER_Tow_Catch.rda")
load("MASTER_Data/MASTER_Long_Format.rda")
load("MASTER_Data/MASTER_Non_CDFW_Surveys_Long.rda")


#==========Create integrated long data frame with ALL surveys (CDFW+Others)
Additional_Surveys <- Additional_Surveys %>%
  mutate(Gear = recode(Gear,"KDTR" = "Kodiak"),
         Core_Survey = FALSE)

All_Surveys <- Long_Master %>% 
  rename("Depth_Stratum" = "Depth_Cat")%>%
  mutate(Volume = NA,
         Core_Survey = TRUE,
         Gear = recode(SurveySeason,
                       "FMWT" = "MWTR",
                       "SKT" = "Kodiak",
                       "STN" = "Townet",
                       "SLS" ="Egg and Larva Net"
         ))%>%
  select(SurveySeason,
         StationCode,
         Station_Latitude,
         Station_Longitude,
         SampleDate,
         TowNumber,
         Volume,
         CommonName,
         ForkLength,
         Gear,
         Year,
         Month,
         Region,
         SubRegion,
         Depth_Stratum,
         Core_Survey)%>%
  add_row(Additional_Surveys)%>%
  mutate(Year = year(SampleDate),
         Month = month(SampleDate))%>%
  filter(Year>2001)%>%
  mutate(SubRegion = as.factor(if_else(Station_Latitude<37.8 & Station_Longitude>-122,
                                       "Upper San Joaquin River",
                                       as.character(SubRegion))),
         Region = as.factor(if_else(Station_Latitude<37.8 & Station_Longitude>-122,
                                    "South",
                                    as.character(Region)
         )
         )
  )
  


#===========Summarize post filtering species coverage compared with LTMR
LTMR_Clusters <- read_csv("CODE -station-ranking/Complete_Rankings.csv",
                          col_types = "fddff")%>%
  mutate(LTMR_Only = if_else(is.na(TotalRank),TRUE,FALSE))%>%
  mutate(No_LTMR = if_else(is.na(LTMR_Demersal)==T&is.na(LTMR_OpenWater)==T,TRUE,FALSE))

LTMR_Clusters %>%with(.,table(LTMR_OpenWater))

LTMR_Clusters %>%filter(is.na(TotalRank)==F)%>%with(.,table(LTMR_OpenWater))

LTMR_Clusters %>%with(.,table(LTMR_Demersal))

LTMR_Clusters %>%filter(is.na(TotalRank)==F)%>%with(.,table(LTMR_Demersal))

#=====================Table of Pelagic Survey Review species by #

All_Species_CDFW <- Long_Master %>% arrange(CommonName)%>%group_by(CommonName)%>%
  summarise(AllTimeTotal = n())%>%filter(AllTimeTotal>100)

#write_csv(All_Species,"All_Species.csv")

#=====================Table of All Survey Review species by #

All_Species <- All_Surveys %>% arrange(CommonName)%>%group_by(CommonName)%>%
  summarise(AllTimeTotal = n())%>%filter(AllTimeTotal>100)

All_Surveys <- All_Surveys %>% filter(CommonName %in% unique(All_Species$CommonName))


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
         )


Long_Master <- Long_Master%>%
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
  mutate(Season = factor(Season, levels = c("Winter","Spring","Summer","Fall")))



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

pdf(file="CODE -station-ranking/EDSM Strata Environment Comparisons.pdf",width=16, height = 9)

Environment_Hydrology%>%
  group_by(SubRegion)%>%
  mutate(Mean_Cond = mean(Conductivity,na.rm=T))%>%
  ungroup()%>%
  arrange(-Mean_Cond)%>%
  mutate(SubRegion=factor(SubRegion, levels=unique(SubRegion)))%>%
  filter(Conductivity<45000 & is.na(SacWYType)==F)%>%
  ggplot(aes(y=SubRegion,x=Conductivity,fill=Season,col=Season))+geom_boxplot(alpha=0.5)+
  facet_grid(~SacWYType)+
  scale_fill_viridis_d()+
  scale_color_viridis_d()+
  theme_bw()+ggtitle("Conductivity across EDSM Strata, Season and Water Year Type")
  
Environment_Hydrology%>%
  group_by(SubRegion)%>%
  mutate(Mean_Cond = mean(Conductivity,na.rm=T))%>%
  ungroup()%>%
  arrange(-Mean_Cond)%>%
  mutate(SubRegion=factor(SubRegion, levels=unique(SubRegion)))%>%
  filter(Conductivity<45000 & is.na(SacWYType)==F)%>%
  ggplot(aes(y=SubRegion,x=Secchi,fill=Season,col=Season))+geom_boxplot(alpha=0.5)+
  facet_grid(~SacWYType)+
  scale_fill_viridis_d()+
  scale_color_viridis_d()+
  theme_bw()+ggtitle("Secchi Depth across EDSM Strata, Season and Water Year Type")

Environment_Hydrology%>%
  group_by(SubRegion)%>%
  mutate(Mean_Cond = mean(Conductivity,na.rm=T))%>%
  ungroup()%>%
  arrange(-Mean_Cond)%>%
  mutate(SubRegion=factor(SubRegion, levels=unique(SubRegion)))%>%
  filter(Conductivity<45000 & is.na(SacWYType)==F)%>%
  ggplot(aes(y=SubRegion,x=Temperature,fill=Season,col=Season))+geom_boxplot(alpha=0.5)+
  facet_grid(~SacWYType)+
  scale_fill_viridis_d()+
  scale_color_viridis_d()+xlim(c(0,30))+
  theme_bw()+ggtitle("Temperature across EDSM Strata, Season and Water Year Type")

Environment_Hydrology%>%
  group_by(SubRegion)%>%
  mutate(Mean_Cond = mean(Conductivity,na.rm=T))%>%
  ungroup()%>%
  arrange(-Mean_Cond)%>%
  mutate(SubRegion=factor(SubRegion, levels=unique(SubRegion)))%>%
  filter(Conductivity<45000 & is.na(SacWYType)==F)%>%
  ggplot(aes(y=SubRegion,x=Chlorophyll,fill=Season,col=Season))+geom_boxplot(alpha=0.5)+
  facet_grid(~SacWYType)+
  scale_fill_viridis_d()+
  scale_color_viridis_d()+
  theme_bw()+ggtitle("Chlorophyll across EDSM Strata, Season and Water Year Type")

dev.off()

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
  summarise(N=n(),Freq=round(n()/n,4))%>%distinct()%>%filter(Freq>0)%>%
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
  summarise(N=n(),Freq=round(n()/n,4))%>%distinct()%>%filter(Freq>0)%>%
  arrange(-Freq)%>%add_column(SalGroup = "Mid", YearGroup="2000-2020")%>%
  ungroup()%>%
  mutate(Rank=rank(Freq))


SpecRanks_2000_HiSal <- Long_Master %>% 
  group_by(SubRegion)%>%
  mutate(Mean_Cond = mean(ConductivityTop,na.rm=T))%>%
  ungroup()%>%
  arrange(-Mean_Cond)%>%
  filter(OrganismCategory=="Fish"&Year>1999&Mean_Cond>20000)%>%
  add_tally()%>%
  group_by(CommonName)%>%
  summarise(N=n(),Freq=round(n()/n,4))%>%distinct()%>%filter(Freq>0)%>%
  arrange(-Freq)%>%add_column(SalGroup = "High", YearGroup="2000-2020")%>%
  ungroup()%>%
  mutate(Rank=rank(Freq))




All_Rankings <-SpecRanks_2000_LowSal%>%
  add_row(SpecRanks_2000_MidSal)%>%
  add_row(SpecRanks_2000_HiSal)

Complete_Rankings <- All_Rankings %>% group_by(CommonName)%>%
  summarise(TotalRank = sum(Rank),N_Lists=n(),N=mean(N))%>%arrange(-TotalRank)%>%
  filter(CommonName != "Unknown"&N>150 & N_Lists>1)

table(Environment_Hydrology$Survey)

#====================Do some wrangling before catch comparison figures==============

Working_Long <- Long_Master %>% filter(CommonName %in% c(Complete_Rankings$CommonName,"Crangon"))



Working_Tow <- Working_Long %>% 
  distinct(across(c(Survey_Station,SampleDate,TowNumber,CommonName,SurveySeason)),.keep_all = TRUE)%>%
  select(-c(LengthFrequency,Family:Species,ForkLength:Dead,ReproductiveStage:Sex,OrganismCodeFMWT,OrganismCategory))%>%
  group_by(SubRegion)%>%
  mutate(Mean_Cond = mean(ConductivityTop,na.rm=T))%>%
  ungroup()%>%
  arrange(-Mean_Cond)%>%
  mutate(
    Depth_Cat = factor(Depth_Cat, levels = c("very deep","deep","intermediate","shallow")),
    SubRegion=factor(SubRegion, levels=unique(SubRegion)))



#================================================================================
pdf("CODE -station-ranking/Tows by Subregion Season and WY Type and Depth.pdf",width = 16, height =9)

# Number of Tows by Season, Subregion and Water Year Type
Working_Tow %>% filter(Year>1999)%>%
  filter( is.na(SacWYType)==F)%>%
  group_by(SubRegion,SurveySeason,SacWYType)%>%
  mutate(N_Tows = n())%>%
  ungroup()%>%
  distinct(across(c(SubRegion,N_Tows,SurveySeason,SacWYType)))%>%
  ggplot(aes(y=SubRegion,x=N_Tows,fill=SurveySeason))+geom_bar(stat="identity")+
  scale_color_viridis_d()+scale_fill_viridis_d()+facet_grid(~SacWYType)+theme_bw()
  

# Number of Tows by Season, Subregion and Depth Strata
Working_Tow %>% filter(Year>1999)%>%
  filter(is.na(Season)==F & is.na(Depth_Cat)==F)%>%
  group_by(SubRegion,Season,Depth_Cat)%>%
  mutate(N_Tows = n())%>%
  ungroup()%>%
  distinct(across(c(SubRegion,N_Tows,Season,Depth_Cat)))%>%
  ggplot(aes(y=SubRegion,x=N_Tows,col=Season,fill=Season))+geom_bar(stat="identity",position="dodge",alpha=0.5)+
  scale_color_viridis_d()+scale_fill_viridis_d()+facet_grid(~Depth_Cat)+theme_bw()

dev.off()
#================================================================
# Catch Boxplots
Catch_Season_Region <-  Working_Tow %>% filter(is.na(SubRegion)==F)%>%
  group_by(SubRegion,Season)%>%
  mutate(Date_Station_Tow = paste(SampleDate,"_",StationCode,"_",TowNumber))%>%
  mutate(N_Tows = length(unique(Date_Station_Tow)))%>%
  ungroup()%>%
  mutate(CatchPerTow = Catch/N_Tows)%>%
  filter(is.na(Season)==F)%>%
  ggplot(aes(x=CatchPerTow,y=SubRegion,fill=Season,col=Season))+geom_boxplot(alpha=.5)+theme_bw()+
  facet_wrap_paginate(~CommonName,scales="free_x",nrow=2,ncol=3)+
  scale_color_viridis_d()+
  scale_fill_viridis_d()

for(i in 1:5){
  print(Catch_Season_Region + facet_wrap_paginate(~CommonName,scales="free_x",nrow=2,ncol=3,page=i))
}


# Catch Barplots
pdf("CODE -station-ranking/Catch Per Tow by Subregion and Season.pdf",width = 16, height =9)
#======Mean Catch==================
Catch_Season_Region_Bar <-  Working_Tow %>% 
  filter(is.na(SubRegion)==F,Year>1999)%>%
  group_by(SubRegion,Season)%>%
  mutate(Date_Station_Tow = paste(SampleDate,"_",StationCode,"_",TowNumber))%>%
  mutate(N_Tows = length(unique(Date_Station_Tow)))%>%
  ungroup()%>%
  mutate(CatchPerTow = Catch/N_Tows)%>%
  group_by(SubRegion, SurveySeason,CommonName)%>%
  summarise(Mean_CPT = mean(CatchPerTow,na.rm=T),
            SD_CPT = sd(CatchPerTow,na.rm=T))%>%
  ggplot(aes(y=SubRegion,x=Mean_CPT,fill=SurveySeason,col=SurveySeason))+
  geom_bar(stat="identity",alpha=.5)+
  facet_wrap_paginate(~CommonName,scales="free_x",nrow=2,ncol=3)+
  theme_bw()+
  scale_color_viridis_d()+
  scale_fill_viridis_d()


for(i in 1:6){
  print(Catch_Season_Region_Bar + facet_wrap_paginate(~CommonName,scales="free_x",nrow=2,ncol=3,page=i))+
    ggtitle("Mean Catch Per Tow by EDSM Subregion and Survey sine 2000, free x-axis")
}

#Mean Catch, Fixed X

for(i in 1:5){
  print(Catch_Season_Region_Bar + facet_wrap_paginate(~CommonName,nrow=2,ncol=3,page=i))+
    ggtitle("Mean Catch Per Tow by EDSM Subregion and Season, fixed x-axis")
}

#CV Catch
Catch_Season_Region_Bar_CV <-  Working_Tow %>% 
  filter(is.na(SubRegion)==F)%>%
  group_by(SubRegion,Season)%>%
  mutate(Date_Station_Tow = paste(SampleDate,"_",StationCode,"_",TowNumber))%>%
  mutate(N_Tows = length(unique(Date_Station_Tow)))%>%
  ungroup()%>%
  mutate(CatchPerTow = Catch/N_Tows)%>%
  group_by(SubRegion, Season,CommonName)%>%
  summarise(Mean_CPT = mean(CatchPerTow,na.rm=T),
            CV_CPT = sd(CatchPerTow,na.rm=T)/Mean_CPT)%>%
  filter(is.na(Season)==F)%>%
  ggplot(aes(y=SubRegion,x=CV_CPT,fill=Season,col=Season))+
  geom_bar(stat="identity",alpha=.5)+
  facet_wrap_paginate(~CommonName,scales="free_x",nrow=2,ncol=3)+
  theme_bw()+
  scale_color_viridis_d()+
  scale_fill_viridis_d()


for(i in 1:5){
  print(Catch_Season_Region_Bar_CV + facet_wrap_paginate(~CommonName,scales="free_x",nrow=2,ncol=3,page=i))+
    ggtitle("CV of Catch Per Tow by EDSM Subregion and Season, free x-axis")
}

#CV Catch, Fixed X

for(i in 1:5){
  print(Catch_Season_Region_Bar_CV + facet_wrap_paginate(~CommonName,nrow=2,ncol=3,page=i))+
    ggtitle("CV of Catch Per Tow by EDSM Subregion and Season, fixed x-axis")
}

dev.off()


#==========================================================================

for(i in 1:3){
print(Working_Long %>% filter(Year>1999)%>%
        group_by(CommonName)%>%
        mutate(Length95 = quantile(ForkLength,probs=.95,na.rm=T))%>%
        ungroup()%>%
        filter(ForkLength<Length95)%>%
  filter(is.na(ForkLength)==F)%>%
  ggplot(aes(x=ForkLength,fill=SurveySeason))+
  geom_density(alpha=.5)+scale_fill_viridis_d()+
  facet_wrap_paginate(~CommonName,ncol=4,nrow=3,scales="free",page=i))
}



#================Create Tabular Report of Fish Lengths by Survey===========

Length_Summary <- All_Surveys %>% filter(is.na(ForkLength)==F)%>%
  group_by(SurveySeason,SubRegion,CommonName)%>%
  summarise(N_Fish = n(),
            Length_Mean = mean(ForkLength,na.rm = T),
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
  filter(Length_Min != Length_Max)


Catch_Summary <- All_Surveys %>% group_by(SurveySeason,SampleDate,StationCode,TowNumber,CommonName)%>%
  mutate(Catch = n())%>%
  ungroup()%>%
  distinct(across(c(StationCode,SampleDate,TowNumber,CommonName,SurveySeason)),.keep_all = TRUE)%>%
  select(-ForkLength)%>%
  pivot_wider(values_from = Catch,names_from = CommonName)%>%
  mutate_at(vars(Striped_Bass_Adult:Diamond_Turbot), ~replace_na(., 0))%>%
  pivot_longer(Striped_Bass_Adult:Diamond_Turbot,names_to = "CommonName", values_to = "Catch")%>%
  group_by(SurveySeason,SubRegion,CommonName)%>%
  summarise(MinCatch = min(Catch),
            MaxCatch = max(Catch),
            MeanCatch = mean(Catch),
            MedianCatch = median(Catch))%>%
  ungroup()%>%group_by(CommonName)%>%
  mutate(Overall_Mean = mean(MeanCatch))





