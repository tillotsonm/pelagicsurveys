#===========Code to generate station-level summary variables=========
#=================And composite rankings=============================

#Prepared by Michael Tillotson
#ICF
#Updated March 11, 2021
#Currently incomplete. Does not have CPUE calculated.
require(tidyverse)


setwd("C:/Users/40545/Documents/GitHub/pelaagicsurveys")
load("FINAL_REVIEW_DATA/CDFW_Pelagic_Review_Data.rda")


#Load strata volume table in 1000s of Cubic Meters
Strata_Volumes <- read_csv("SpatialData/Strata_Volumes.csv")%>%
  mutate(SubRegion = as.factor(SubRegion))

#==============================================================================
#==============================================================================

#Sampled Percent of all habitat Through Years 
Review_Data_Tows%>%filter(is.na(SubRegion)==F)%>%
  group_by(SurveySeason,Month,SubRegion)%>%
  summarise(SampledVolume = mean(Volume))%>%
  full_join(Strata_Volumes,by="SubRegion")%>%
  mutate(SampledPercent = (SampledVolume/TotalVolume)*100)%>%
  ggplot(aes(x=Month,y=SampledPercent,col=SurveySeason))+geom_line()+
  facet_wrap(~SubRegion,scales = "free_y")+
  ggtitle("Monthly mean % of total volume sampled and survey")


#Create volume summary table
Core_Targets %>% ungroup()%>%filter(is.na(SubRegion)==F)%>%
  group_by(SurveySeason,Month,SubRegion,Year,Region)%>%
  summarise(SampledVolume = sum(Volume))%>%
  ungroup()%>%
  group_by(SurveySeason,Month,SubRegion,Region)%>%
  summarise(SampledVolume=mean(SampledVolume))%>%
  full_join(Strata_Volumes,by="SubRegion")%>%
  select(-c(SurfaceArea:Volume10_Bottom))%>%arrange(Month)%>%
  pivot_wider(names_from="Month",values_from=c("SampledVolume"))%>%
  write_csv(file="TemporaryOutputs/SampledVolumeSummary.csv")


Review_Data_Tows%>%
  ggplot(aes(x=Volume))+
  geom_histogram()+facet_wrap(~SurveySeason,scales="free")+
  theme_bw()+
  xlab("Tow volume (1,000 cubic meters)")+ 
  geom_text(
    data=Review_Data_Tows%>%group_by(SurveySeason)%>%
      mutate(MeanVolume = round(mean(Volume),2),
             Label = paste("Survey mean= ",MeanVolume))%>%
      ungroup()%>%
      distinct(across("SurveySeason"),.keep_all=T),
    mapping = aes(x = (MeanVolume), y = c(1250,3000,450,500,6200), label = Label),
    hjust   = -0.1)+
  geom_vline(data=Review_Data_Tows%>%group_by(SurveySeason)%>%
               mutate(MeanVolume = round(mean(Volume),2)),
                      aes(xintercept = MeanVolume))


















Rank_Table <- Core_Targets %>% 
  group_by(SubRegion,StationCode)%>%
  mutate(N_Years = length(unique(Year)),
         Station_Longitude=mean(Station_Longitude))%>%
  filter(N_Years>10)%>%ungroup()%>%
  group_by(SubRegion,StationCode,N_Years,Station_Longitude)%>%
  summarise_at(vars(contains("CPUV")),mean)%>%
  ungroup()%>%
  mutate_at(vars(contains("CPUV")),round,0)%>%
  mutate_at(vars(contains("CPUV")),dense_rank)%>%
  rowwise() %>% 
  mutate(SumRank = sum(c_across(contains("CPUV"))))

Rank_Table%>%arrange(-SumRank)%>%
  mutate(StationCode = factor(StationCode,levels=unique(StationCode)))%>%
  ggplot(aes(y=StationCode,x=SumRank,fill=Station_Longitude))+geom_bar(stat="identity")+
  scale_fill_viridis_c()

RankTable
