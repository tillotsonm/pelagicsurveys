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

Environment_Master <- read_csv("RawData/Environmental Data/Delta_integrated_WQ.csv",
                               col_types="cfffddlDddddfdddddddcfff")%>%
  rename("Depth_Cat" = "category")%>%
  mutate(StationCode = if_else(StationCode=="724"&Survey=="20mm","724.1",as.character(StationCode)))%>%
  mutate(StationCode = if_else(StationCode=="716"&Survey=="SKT","716.1",as.character(StationCode)))%>%
  mutate(StationCode = if_else(StationCode=="716"&Survey=="SLS","716.1",as.character(StationCode)))%>%
  mutate(StationCode = if_else(StationCode=="901"&Survey=="STN","901.1",as.character(StationCode)))%>%
  mutate(StationCode = if_else(StationCode=="901"&Survey=="20mm","901.1",as.character(StationCode)))%>%
  mutate(StationCode = if_else(StationCode=="915"&Survey=="FMWT","915.1",as.character(StationCode)))%>%
  mutate(StationCode = if_else(StationCode=="914"&Survey=="FMWT","914.1",as.character(StationCode)))%>%
  mutate(StationCode = if_else(StationCode=="705"&Survey=="FMWT","705.1",as.character(StationCode)))%>%
  mutate(StationCode = if_else(StationCode=="706"&Survey=="FMWT","706.1",as.character(StationCode)))%>%
  mutate(StationCode = if_else(StationCode=="703"&Survey=="FMWT","703.1",as.character(StationCode)))%>%
  mutate(StationCode = if_else(StationCode=="323"&Survey=="FMWT","323.1",as.character(StationCode)))%>%
  mutate(StationCode = if_else(StationCode=="501"&Survey=="FMWT","501.1",as.character(StationCode)))%>%
  mutate(StationCode = if_else(StationCode=="501"&Survey=="SLS","501.2",as.character(StationCode)))%>%
  mutate(StationCode = if_else(StationCode=="418"&Survey=="FMWT","418.1",as.character(StationCode)))%>%
  mutate(StationCode = if_else(StationCode=="801"&Survey=="FMWT","801.1",as.character(StationCode)))%>%
  mutate(StationCode = if_else(StationCode=="804"&Survey=="FMWT","804.1",as.character(StationCode)))%>%
  mutate(StationCode = if_else(StationCode=="405"&Survey=="FMWT","405.1",as.character(StationCode)))%>%
  mutate(StationCode = if_else(StationCode=="411"&Survey=="FMWT","401.1",as.character(StationCode)))%>%
  mutate(StationCode = if_else(StationCode=="504"&Survey=="FMWT","504.1",as.character(StationCode)))%>%
  mutate(StationCode = if_else(StationCode=="504"&Survey=="SLS","504.2",as.character(StationCode)))%>%
  mutate(StationCode = if_else(StationCode=="812"&Survey=="FMWT","812.1",as.character(StationCode)))%>%
  mutate(StationCode = if_else(StationCode=="815"&Survey=="FMWT","815.1",as.character(StationCode)))%>%
  mutate(StationCode = if_else(StationCode=="915"&Survey=="FMWT","915.1",as.character(StationCode)))%>%
  mutate(StationCode = if_else(StationCode=="330"&Survey=="FMWT","330.1",as.character(StationCode)))%>%
  select(-Source)%>%mutate(StationCode = as.factor(StationCode))%>%
  mutate(SubRegion = replace_na(as.character(SubRegion),"SF and Outer SP Bays"))%>%
  mutate(SubRegion = as.factor(SubRegion))


#Load regional and depth classifications
region_classifiers <- read_csv("SpatialData/station_join_depth_strata_edsm.csv",
                               col_types="ddfffff")%>%
                      select(-c(Station_Longitude,Station_Latitude))%>%
  rename("Depth_Cat" = "category")%>%
  mutate(SubRegion = replace_na(as.character(SubRegion),"SF and Outer SP Bays"))%>%
  mutate(SubRegion = as.factor(SubRegion))

#Join environmental and hydrological data
Environment_Hydrology <- Hydrology_Daily %>% 
  full_join(Environment_Master,by="Date")%>%
  filter(is.na(StationCode)==F)%>%relocate("Time", .after = "Date")

#Rename date column for joining with catch data
Hydrology_Daily <- Hydrology_Daily %>% rename("SampleDate" ="Date")



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


Tow_PresAbs <- Working_Tow %>% 
  select(-contains("Length_Mean"))%>%
  select(-contains("Length_Var"))%>%
  mutate_at(vars(contains("Catch_")),~ifelse(.==0, 0, 1))


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
           StationActive,Location,Region,SubRegion,strata_depth,Depth_Cat,N_Tows,Mean_Tows)%>%
  summarise_all(list(min=min, max=max,mean=mean),na.rm=T)%>%
  select(-c(Year_mean,Month_mean,JulianDay_mean))
  
  
  Working_Station[is.na(Working_Station)] <- NA
  
  
  #===========================Convert working data to master and save======================================
          
  Station_Master <- do.call(data.frame,lapply(Working_Station, function(x) replace(x, is.infinite(x),NA)))%>%
    arrange(-ConductivityTop_mean)

  Long_Master <- Working_Long

  Tow_Master <- Working_Tow%>%left_join(Hydrology_Daily,by=c("SampleDate","Year","Month","JulianDay"))%>%
    relocate(WaterYear:May8Riv,.after=JulianDay)
  
  

save(Long_Master,file ="MASTER_Data/MASTER_Long_Format.rda")

save(Tow_Master,file ="MASTER_Data/MASTER_Tow_Catch.rda")

save(Tow_PresAbs,file ="MASTER_Data/MASTER_Tow_PresAbs.rda")

save(Station_Master,file ="MASTER_Data/MASTER_Station.rda")

save(Environment_Hydrology,file ="MASTER_Data/MASTER_Env_Hydro.rda")


  
Tow_Master %>% ggplot(aes(x=FishCatch))+geom_histogram(stat="density")+facet_wrap(~SubRegion)+xlim(c(0,200))
  
 





                                                 