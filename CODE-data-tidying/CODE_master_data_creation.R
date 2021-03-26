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

#Load tidied catch data
load("TidyData/DATA_All_Surveys_Tidy.rda")


#Load hydrology data
load("TidyData/DATA_Hydrology_tidy.rda")

#Load environmental data

load("TidyData/EDI_EnvironmentalData.rda")

#Load regional and depth classifications
region_classifiers <- read_csv("SpatialData/station_join_depth_strata_edsm.csv",
                               col_types="ddfffff")%>%
                      select(-c(Station_Longitude,Station_Latitude))



Working_Long <-  All_Surveys_Long %>%
  #Drops three rows with NA common names
  filter(is.na(CommonName)==F)%>%
  #Condense redundant common names
  mutate(SurveySeason = as.factor(SurveySeason),
         CommonName = recode(CommonName,"Age-0 Striped Bass" = "Striped Bass Age 0",
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
                             "Striped Bass" = "Striped Bass Age 0",
                             "Crangon Shrimp" = "Crangon",
                             "Palaemon Spp." = "Palaemon",
                             "Palaemon Shrimp" = "Palaemon",
                             "Three Spine Stickleback" = "Threespine Stickleback",
                             "Aequorea Spp. (Lens Jellyfish)" = "Lens Jellyfish",
                             "Bluegill" = "Bluegill Sunfish",
                             "Centrarchids (Unid)" = "Centrarchid (Unid)",
                             "Moon Jellies" = "Moon Jelly",
                             "Polyorchis Penicillatus" = "Polyorchis",
                             "Shiner Surfperch" = "Shiner Perch",
                             "Smelt Family" = "Smelt (Unid)",
                             "Gobies (Unid)" = "Goby (Unid)",
                             "Unid" = "Unknown",
                             "Unidentified (Unid)" = "Unknown",
                             "Unknown Fish (Unid)" ="Unknown",
                             "Atherinopsidae (Unk)" = "Silversides (Unid)")
                             )%>%
  mutate(CommonName = str_replace_all(CommonName, " ", "_"))%>%
  
  
  #Add distinctions for fish vs. macroinverts
  mutate(OrganismCategory = "Fish")%>%
  mutate(OrganismCategory = if_else(CommonName %in% c("Jellyfish",
                                                      "Maeotias",
                                                      "Polyorchis",
                                                      "Comb_Jelly_Or_Sea_Goosberry",
                                                      "Pleurobrachia_Jellyfish",
                                                      "Moon_Jelly",
                                                      "Jelly_(Unid)",
                                                      "Chrysaora_Fuscensens",
                                                      "Blackfordia_Virginica",
                                                      "Scrippsia_Pacifica"
                                                      ),
                                    "Gelatinous",OrganismCategory))%>%
  mutate(OrganismCategory = if_else(CommonName %in% c("Siberian_Prawn",
                                                      "Crangon",
                                                      "Palaemon",
                                                      "Mud_Shrimp",
                                                      "Dungeness_Crab"),
                                    "Crustacean",OrganismCategory))%>%
  mutate(StationCode = if_else(StationCode=="724"&SurveySeason=="20mm","724.1",as.character(StationCode)))%>%
  mutate(StationCode = if_else(StationCode=="716"&SurveySeason=="SKT","716.1",as.character(StationCode)))%>%
  mutate(StationCode = if_else(StationCode=="716"&SurveySeason=="SLS","716.1",as.character(StationCode)))%>%
  mutate(StationCode = if_else(StationCode=="901"&SurveySeason=="STN","901.1",as.character(StationCode)))%>%
  mutate(StationCode = if_else(StationCode=="901"&SurveySeason=="20mm","901.1",as.character(StationCode)))%>%
  mutate(StationCode = if_else(StationCode=="915"&SurveySeason=="FMWT","915.1",as.character(StationCode)))%>%
  mutate(StationCode = if_else(StationCode=="914"&SurveySeason=="FMWT","914.1",as.character(StationCode)))%>%
  mutate(StationCode = if_else(StationCode=="705"&SurveySeason=="FMWT","705.1",as.character(StationCode)))%>%
  mutate(StationCode = if_else(StationCode=="706"&SurveySeason=="FMWT","706.1",as.character(StationCode)))%>%
  mutate(StationCode = if_else(StationCode=="703"&SurveySeason=="FMWT","703.1",as.character(StationCode)))%>%
  mutate(StationCode = if_else(StationCode=="323"&SurveySeason=="FMWT","323.1",as.character(StationCode)))%>%
  mutate(StationCode = if_else(StationCode=="501"&SurveySeason=="FMWT","501.1",as.character(StationCode)))%>%
  mutate(StationCode = if_else(StationCode=="501"&SurveySeason=="SLS","501.2",as.character(StationCode)))%>%
  mutate(StationCode = if_else(StationCode=="418"&SurveySeason=="FMWT","418.1",as.character(StationCode)))%>%
  mutate(StationCode = if_else(StationCode=="801"&SurveySeason=="FMWT","801.1",as.character(StationCode)))%>%
  mutate(StationCode = if_else(StationCode=="804"&SurveySeason=="FMWT","804.1",as.character(StationCode)))%>%
  mutate(StationCode = if_else(StationCode=="405"&SurveySeason=="FMWT","405.1",as.character(StationCode)))%>%
  mutate(StationCode = if_else(StationCode=="411"&SurveySeason=="FMWT","401.1",as.character(StationCode)))%>%
  mutate(StationCode = if_else(StationCode=="504"&SurveySeason=="FMWT","504.1",as.character(StationCode)))%>%
  mutate(StationCode = if_else(StationCode=="504"&SurveySeason=="SLS","504.2",as.character(StationCode)))%>%
  mutate(StationCode = if_else(StationCode=="812"&SurveySeason=="FMWT","812.1",as.character(StationCode)))%>%
  mutate(StationCode = if_else(StationCode=="815"&SurveySeason=="FMWT","815.1",as.character(StationCode)))%>%
  mutate(StationCode = if_else(StationCode=="915"&SurveySeason=="FMWT","915.1",as.character(StationCode)))%>%
  mutate(StationCode = if_else(StationCode=="330"&SurveySeason=="FMWT","330.1",as.character(StationCode)))%>%
  
  #Remove taxa observed fewer than 10 times
  mutate%>%group_by(CommonName)%>%add_tally()%>%filter(n>9)%>%ungroup()%>%select(-n)%>%

  #add regional separations
  
  left_join(region_classifiers,by="StationCode")


#==Create Wide Format data by tow with species counts and mean lengths

Working_Tow <- Working_Long %>% 
  ungroup()%>%
  distinct(across(c(Survey_Station,SampleDate,TowNumber,CommonName,ForkLength)),.keep_all = TRUE)%>%
  mutate(ForkLength = na_if(ForkLength,0))%>%
  group_by(Survey_Station,SampleDate,TowNumber,CommonName)%>%
  mutate(Length_Mean = mean(ForkLength,na.rm = T),
         Length_Var = var(ForkLength, na.rm=T))%>%
  ungroup%>%
  group_by(Survey_Station,SampleDate,TowNumber)%>%
  mutate(MaxLength = max(ForkLength,na.rm = T),
         Q75Length = quantile(ForkLength,.75,na.rm=T),
         MedianLength = median(ForkLength, na.rm=T),
         Q25Length = quantile(ForkLength,.25,na.rm=T),
         MinLength = min(ForkLength, na.rm=T))%>%
  ungroup()%>%
  distinct(across(c(Survey_Station,SampleDate,TowNumber,CommonName)),.keep_all = TRUE)%>%
  select(-c(LengthFrequency,Family:Species,ForkLength:Dead,ReproductiveStage:Sex,OrganismCodeFMWT,OrganismCategory))%>% 
  ungroup()%>%
  pivot_wider(names_from = CommonName,names_sep = "_",values_from = c(Catch,Length_Mean,Length_Var))%>%
  mutate_at(vars(contains("Catch")), ~replace_na(., 0))%>%
  mutate(TotalCatch = rowSums(select(.,contains("Catch"))),
         GelatenousCatch = rowSums(select(.,c(  Catch_Jellyfish,
                                                Catch_Maeotias,
                                                Catch_Polyorchis,
                                                Catch_Comb_Jelly_Or_Sea_Goosberry,
                                                Catch_Pleurobrachia_Jellyfish,
                                                Catch_Moon_Jelly,
                                                `Catch_Jelly_(Unid)`,
                                                Catch_Blackfordia_Virginica,
                                                Catch_Scrippsia_Pacifica))),
         CrustaceanCatch = rowSums(select(.,c(  Catch_Siberian_Prawn,
                                                Catch_Crangon,
                                                Catch_Palaemon,
                                                Catch_Dungeness_Crab))),
         FishCatch = TotalCatch - CrustaceanCatch - GelatenousCatch)%>%
  relocate(c(TotalCatch:FishCatch),.after = MinLength)

#=====Replace Inf and NaN with NA
Working_Tow[is.na(Working_Tow)] <- NA
Working_Tow$MinLength[is.infinite(Working_Tow$MinLength)] <- NA
Working_Tow$MaxLength[is.infinite(Working_Tow$MaxLength)] <- NA


#========Convert to presence/absence=========================


PresAbs_All <- Working_Tow %>% 
  select(-contains("Length_Mean"))%>%
  select(-contains("Length_Var"))%>%
  mutate_at(vars(contains("Catch_")),~ifelse(.==0, 0, 1))


names(Working_Station)

names(Working_Tow)
#Towards station level summaries
Working_Station <- Working_Tow%>%
  select(-c(SampleDate,Survey_Station,SurveyNumber,TemperatureTop,TemperatureBottom,
                        Secchi,ConductivityBottom,CableOut:OrderNum,Station_Origin))%>%
  select(-contains("Length_Mean"))%>%
  select(-contains("Length_Var"))%>%
  select(-contains("Catch_"))%>%
  group_by(StationCode)%>%
  mutate(N_Tows = n(),
         Mean_Tows = round(N_Tows/length(unique(Year)),0))%>%
  mutate(Surveys = paste(unique(SurveySeason),collapse=","),.after = StationCode)%>%
  mutate(Station_Longitude = mean(Station_Longitude),Station_Latitude=mean(Station_Latitude))%>%
  mutate(Location=paste(unique(Location),collapse=","))%>%
  mutate(StationActive = if_else(any(StationActive)==T,T,F))%>%
  select(-c(SurveySeason,Comment1:Comment3,MeterSerial,Q25Length,Q75Length,Area,WeightingFactor))%>%
  relocate(c(StationCode,Surveys),.before=Year)%>%
  group_by(StationCode,Surveys,Station_Longitude,Station_Latitude,
           StationActive,Location,Region,SubRegion,strata_depth,category,N_Tows,Mean_Tows)%>%
  summarise_all(list(min=min, max=max,mean=mean),na.rm=T)%>%
  select(-c(Year_mean,Month_mean,JulianDay_mean))
  relocate()
  
  
  Working_Station[is.na(Working_Station)] <- NA
          
  Station_Master <- do.call(data.frame,lapply(Working_Station, function(x) replace(x, is.infinite(x),NA)))%>%
    arrange(-ConductivityTop_mean)



  
  #===================Important Filtering Steps to Review===============================
#Set a threshold for how common a species should be to remain in the dataset
#Currenlty using 0.1% representation of all-time catch as cutoff
SpecRanks_2000 <- Working_Long %>% filter(OrganismCategory=="Fish"&Year>1999)%>%add_tally()%>%
  group_by(CommonName)%>%
  summarise(AllTime=round(n()/n,3))%>%distinct()%>%filter(AllTime>0)%>%
  arrange(-AllTime)



#Set a threshold for how common a species should be to remain in the dataset, exclude herring and anchovy
#Currenlty using 0.1% representation of all-time catch as cutoff
SpecRanks_Upstream_2000 <- Working_Long %>% filter(OrganismCategory=="Fish"&
                                                     CommonName != "Northern_Anchovy"&
                                                     CommonName != "Pacific_Herring"&
                                                     Year>1999)%>%add_tally()%>%
  group_by(CommonName)%>%
  summarise(AllTime=round(n()/n,3))%>%distinct()%>%
  arrange(-AllTime)%>%filter(AllTime>0)


#Set a threshold for how common a species should be to remain in the dataset
#Currenlty using 0.1% representation of all-time catch as cutoff
SpecRanks <- Working_Long %>% filter(OrganismCategory=="Fish")%>%add_tally()%>%
  group_by(CommonName)%>%
  summarise(AllTime=round(n()/n,3))%>%distinct()%>%filter(AllTime>0)%>%
  arrange(-AllTime)


#Set a threshold for how common a species should be to remain in the dataset, exclude herring and anchovy
#Currenlty using 0.1% representation of all-time catch as cutoff
SpecRanks_Upstream <- Working_Long %>% filter(OrganismCategory=="Fish"&
                                                CommonName != "Northern_Anchovy"&
                                                CommonName != "Pacific_Herring")%>%add_tally()%>%
  group_by(CommonName)%>%
  summarise(AllTime=round(n()/n,3))%>%distinct()%>%
  arrange(-AllTime)%>%filter(AllTime>0)
