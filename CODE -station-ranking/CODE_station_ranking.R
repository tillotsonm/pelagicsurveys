#===========Code to generate station-level summary variables=========
#=================And composite rankings=============================
#===========for unmeasured individuals and calculate CPUE============
#Prepared by Michael Tillotson
#ICF
#Updated March 11, 2021
#Currently incomplete. Does not have CPUE calculated.
require(tidyverse)

setwd("C:/Users/40545/Documents/GitHub/pelagicsurveys")

load("TidyData/DATA_All_Surveys_Tidy.rda")


Working <-
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


Data <- Working %>% ungroup()%>%rename(Length = Mean_Length, Catch = RawCatch)%>%
  filter(CommonName %in% SpecRanks$CommonName) %>%
  mutate(CommonName = str_replace_all(CommonName, " ", "_"))%>%
  mutate(OrganismCategory = "Fish")%>%
  mutate(OrganismCategory = if_else(CommonName %in% c("Jellyfish","Maeotias","Polyorchis","Comb_Jelly_Or_Sea_Goosberry"),
                                    "Gelatinous",OrganismCategory))%>%
  mutate(OrganismCategory = if_else(CommonName %in% c("Siberian_Prawn","Crangon","Palaemon"),
                                    "Shrimp",OrganismCategory))



Data %>% filter(ForkLength<150 & ForkLength > 0)%>%
  filter()%>%
  ggplot(aes(x=ForkLength,y=Catch,fill=SurveySeason))+stat_density()+
  scale_fill_viridis_d()






