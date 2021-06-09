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
select <- dplyr::select


#Set working directory
setwd("C:/Users/40545/Documents/GitHub/pelagicsurveys")

#Load age-length cutoffs
cutoffs <- read_csv("CODE-data-tidying/Target_Species_Length_Cutoffs.csv")%>%
  mutate(CommonName = as.factor(CommonName))%>%
  mutate(CommonName = str_replace_all(CommonName, " ", "_"))


#Load tidied catch data
load("TidyData/DATA_CDFW_Surveys_Tidy.rda")


#Load hydrology data
load("TidyData/DATA_Hydrology_tidy.rda")
 

#Load tidied additional survey data
load("TidyData/Tidy_Non_CDFW_Surveys_Long.rda")

#Load environmental data

Environment_Master <- read_csv("RawData/Environmental Data/Delta_integrated_WQ.csv",
                               col_types="cfffddlDddddfdddddddcfff")%>%
  dplyr::rename("Depth_Cat" = "category")%>%
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
  mutate(StationCode = if_else(StationCode=="711"&Survey=="FMWT","711.1",as.character(StationCode)))%>%
  select(-Source)%>%mutate(StationCode = as.factor(StationCode))%>%
  mutate(SubRegion = replace_na(as.character(SubRegion),"SF and Outer SP Bays"))%>%
  mutate(SubRegion = as.factor(SubRegion),
         Microcystis = as.character(Microcystis))


#Load regional and depth classifications
region_classifiers <- read_csv("SpatialData/All_Survey_Locations_EDSM_Strata.csv",
                               col_types="ffddfffll")%>%
  filter(SurveySeason=="CDFW_5_Surveys")%>%
                      select(-c(Station_Longitude,Station_Latitude,CatchData,EnvData,SurveySeason))%>%
  dplyr::rename("strata_depth" = "category")



#=============Add SLS data to Environment Master===============
SLS_Env <- CDFW_Surveys_Long %>% filter(SurveySeason=="SLS")%>%
  distinct(across(c(SampleDate,TowNumber,StationCode)),.keep_all = T)%>%
  select(SurveySeason:Waves)%>%
  dplyr::rename("Date" = "SampleDate",
         "Survey" = "SurveySeason",
         "Longitude" = "Station_Longitude",
         "Latitude" = "Station_Latitude",
         "Conductivity" ="ConductivityTop",
         "Temperature" = "TemperatureTop",
         "Time" = "TimeStart",
         "Depth" = "DepthBottom"
  )%>%  left_join(region_classifiers,by=c("StationCode"))%>%
select(-c(Year:JulianDay,SurveyNumber,TowDirection,TowNumber,Turbidity,CableOut,TemperatureBottom,Weather,
          TimeStop:MeterDifference,TowDuration,strata_depth,WindDirection,ConductivityBottom,Waves))


#Predict salinity from conductivity where missing
Environment_Master <- Environment_Master %>% add_row(SLS_Env)%>%
  mutate(Sal_Pred = predict(lm(Salinity ~ Conductivity + I(Conductivity^2),
                               data=Environment_Master),newdata=Environment_Master%>% add_row(SLS_Env)))%>%
  mutate(Salinity = if_else(is.na(Salinity)==T,Sal_Pred,Salinity))%>%
  dplyr::select(-Sal_Pred)





#Join environmental and hydrological data
Environment_Hydrology <- Hydrology_Daily %>% 
  full_join(Environment_Master,by="Date")%>%
  filter(is.na(StationCode)==F)%>%relocate("Time", .after = "Date")
  

#Rename date column for joining with catch data
Hydrology_Daily <- Hydrology_Daily %>% dplyr::rename("SampleDate" ="Date")



Working_Long <-  CDFW_Surveys_Long %>%
  #Drops three rows with NA common names
  filter(is.na(CommonName)==F)%>%
  #Condense redundant common names
  mutate(SurveySeason = as.factor(SurveySeason),
         Area = NA,
         CommonName = recode(CommonName,"Age-0 Striped Bass" = "Striped Bass",
                             "Age 0-Striped Bass" = "Striped Bass",
                             "Age 1-Striped Bass" = "Striped Bass",
                             "Age 2-Striped Bass" = "Striped Bass",
                             "Striped Bass Age-2" = "Striped Bass",
                             "Striped Bass Age-3+" = "Striped Bass",
                             "Striped Bass Age-0" = "Striped Bass",
                             "Striped Bass Age-1" = "Striped Bass",
                             "Striped Bass Age 2" = "Striped Bass",
                             "Age-0 Striped Bass" = "Striped Bass",
                             "Age-1 Striped Bass" = "Striped Bass",
                             "Age-2 Striped Bass" = "Striped Bass",
                             "Striped Bass" = "Striped Bass",
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
  mutate(StationCode = if_else(StationCode=="711"&SurveySeason=="FMWT","711.1",as.character(StationCode)))%>%
  mutate(StationCode = if_else(StationCode=="802"&SurveySeason=="FMWT","802.1",as.character(StationCode)))%>%
  
  #Remove taxa observed fewer than 10 times
  mutate%>%group_by(CommonName)%>%add_tally()%>%filter(n>9)%>%ungroup()%>%select(-n)%>%
  
  #Replace 0 lengths with NA
  
  mutate(ForkLength = na_if(ForkLength,0))%>%

  #add regional separations
  
  left_join(region_classifiers,by=c("StationCode"))%>%
  
  #Correct Pacific herring species name
 mutate(Species = recode(Species, "pallasi" = "pallasii"))%>% 
  
  #Remove supplementary SKT tows
  filter(case_when(SurveySeason=="SKT" ~ SurveyNumber <8,T ~ SurveyNumber <40))%>%
  
  #Correct flowmeter readings. Calculate difference where not reported, add
  #1,000,000 when meter rolled over (i.e. Out < In)
  mutate(MeterOut = if_else(MeterOut<MeterIn,MeterOut+1000000,MeterOut),
         MeterDifference1 = MeterOut-MeterIn,
         MeterDifference = if_else(MeterDifference<0|MeterDifference1>10000,
                                   MeterDifference1,MeterDifference))%>%
  mutate(MeterDifference = if_else(is.na(MeterDifference)==T,MeterDifference1,MeterDifference))%>%
  #select(-MeterDifference1)%>%
  
  #Calculate Tow Volume using standard General Oceanics model 2030R K 
  #0.026873 and the following net opening areas:
  #SLS = 0.37
  #20mm = 1.51
  #STN = 1.49
  #FMWT= 13.4*0.8 assumes 80% door opening
  #SKT = 13.95
  
  mutate(NetArea = as.double(recode(SurveySeason, "20mm" = 1.51,
                                        "FMWT" =  10.7,
                                        "SKT" = 13.95,
                                        "STN" = 1.49,
                                        "SLS" =0.37)))%>%
  mutate(Volume = MeterDifference*0.026873*NetArea)%>%
  
  
  #Add age cutoffs 
  
  left_join(cutoffs,by=c("CommonName","Month"))%>%
  
  #Caclulate tow depth from CableOut based on equations in TN2 Mitchell, Polansky and Newman 2018
  
  #Create variable for BlockHeight by survey
  mutate(BlockHeight = as.numeric(recode(SurveySeason,
                              "20mm" = 8.1,
                              "FMWT" = 7.04,
                              "STN" = 8.1,
                              "SLS" = 8.1,
                              "SKT" = 0)))%>%
  mutate(Angle = as.numeric(recode(SurveySeason,
                                         "20mm" = 0.156434465,
                                         "FMWT" = 0.139173101,
                                         "STN" = 0.165047606,
                                         "SLS" = 0.325568154,
                                         "SKT" = 0)))%>%
  mutate(Additional = as.numeric(recode(SurveySeason,
                                   "20mm" = 4.14,
                                   "FMWT" = 100,
                                   "STN" = 4.17,
                                   "SLS" = 2.67,
                                   "SKT" = 0)))%>%
  
  mutate(CableOut = na_if(CableOut,0))%>%
  #Calculate TowDepth
  mutate(TowDepth = Angle*(CableOut+Additional)-BlockHeight)%>%
  
  mutate(TowDepth = if_else(SurveySeason=="SKT",6,TowDepth))%>%
  
  #Remove BlockHeight and CableOut
  select(-c(BlockHeight,Angle,Additional))

#====================================================================================
#==Create Wide Format data by tow with species counts and mean lengths

# Working_Tow <- Working_Long %>% 
#   ungroup()%>%
#   distinct(across(c(Survey_Station,SampleDate,TowNumber,CommonName,ForkLength)),.keep_all = TRUE)%>%
#   mutate(ForkLength = na_if(ForkLength,0))%>%
#   group_by(Survey_Station,SampleDate,TowNumber,CommonName)%>%
#   mutate(Length_Mean = mean(ForkLength,na.rm = T),
#          Length_Var = var(ForkLength, na.rm=T))%>%
#   ungroup%>%
#   group_by(Survey_Station,SampleDate,TowNumber)%>%
#   mutate(MaxLength = max(ForkLength,na.rm = T),
#          Q75Length = quantile(ForkLength,.75,na.rm=T),
#          MedianLength = median(ForkLength, na.rm=T),
#          Q25Length = quantile(ForkLength,.25,na.rm=T),
#          MinLength = min(ForkLength, na.rm=T))%>%
#   ungroup()%>%
#   distinct(across(c(Survey_Station,SampleDate,TowNumber,CommonName)),.keep_all = TRUE)%>%
#   select(-c(LengthFrequency,Family:Species,ForkLength:Dead,ReproductiveStage:Sex,OrganismCodeFMWT,OrganismCategory))%>% 
#   ungroup()%>%
#   pivot_wider(names_from = CommonName,names_sep = "_",values_from = c(Catch,Length_Mean,Length_Var))%>%
#   mutate_at(vars(contains("Catch")), ~replace_na(., 0))%>%
#   mutate(TotalCatch = rowSums(select(.,contains("Catch"))),
#          GelatenousCatch = rowSums(select(.,c(  Catch_Jellyfish,
#                                                 Catch_Maeotias,
#                                                 Catch_Polyorchis,
#                                                 Catch_Comb_Jelly_Or_Sea_Goosberry,
#                                                 Catch_Pleurobrachia_Jellyfish,
#                                                 Catch_Moon_Jelly,
#                                                 `Catch_Jelly_(Unid)`,
#                                                 Catch_Blackfordia_Virginica,
#                                                 Catch_Scrippsia_Pacifica))),
#          CrustaceanCatch = rowSums(select(.,c(  Catch_Siberian_Prawn,
#                                                 Catch_Crangon,
#                                                 Catch_Palaemon,
#                                                 Catch_Dungeness_Crab))),
#          FishCatch = TotalCatch - CrustaceanCatch - GelatenousCatch)%>%
#   relocate(c(TotalCatch:FishCatch),.after = MinLength)%>%
#   select(-c("Catch_No_Catch","Length_Var_No_Catch","Length_Mean_No_Catch"))
# 
# #=====Replace Inf and NaN with NA
# Working_Tow[is.na(Working_Tow)] <- NA
# Working_Tow$MinLength[is.infinite(Working_Tow$MinLength)] <- NA
# Working_Tow$MaxLength[is.infinite(Working_Tow$MaxLength)] <- NA


#========Convert to presence/absence=========================


# Tow_PresAbs <- Working_Tow %>% 
#   select(-contains("Length_Mean"))%>%
#   select(-contains("Length_Var"))%>%
#   mutate_at(vars(contains("Catch_")),~ifelse(.==0, 0, 1))


#Towards station level summaries
# Working_Station <- Working_Tow%>%
#   select(-c(SampleDate,Survey_Station,SurveyNumber,TemperatureTop,TemperatureBottom,
#                         Secchi,ConductivityBottom,CableOut:OrderNum,Station_Origin))%>%
#   select(-contains("Length_Mean"))%>%
#   select(-contains("Length_Var"))%>%
#   select(-contains("Catch_"))%>%
#   group_by(StationCode)%>%
#   mutate(N_Tows = n(),
#          Mean_Tows = round(N_Tows/length(unique(Year)),0))%>%
#   mutate(Surveys = paste(unique(SurveySeason),collapse=","),.after = StationCode)%>%
#   mutate(Station_Longitude = mean(Station_Longitude),Station_Latitude=mean(Station_Latitude))%>%
#   mutate(Location=paste(unique(Location),collapse=","))%>%
#   mutate(StationActive = if_else(any(StationActive)==T,T,F))%>%
#   select(-c(SurveySeason,Comment1:Comment3,MeterSerial,Q25Length,Q75Length,Area,WeightingFactor))%>%
#   relocate(c(StationCode,Surveys),.before=Year)%>%
#   group_by(StationCode,Surveys,Station_Longitude,Station_Latitude,
#            StationActive,Location,Region,SubRegion,strata_depth,Depth_Cat,N_Tows,Mean_Tows)%>%
#   summarise_all(list(min=min, max=max,mean=mean),na.rm=T)%>%
#   select(-c(Year_mean,Month_mean,JulianDay_mean))
#   
#   
#   Working_Station[is.na(Working_Station)] <- NA
  
  
  #===========================Convert working data to master and save======================================
          
  # Station_Master <- do.call(data.frame,lapply(Working_Station, function(x) replace(x, is.infinite(x),NA)))%>%
  #   arrange(-ConductivityTop_mean)
  # 
  # 
  Long_Master <- Working_Long 
  # 
  # 
  # Tow_Master <- Working_Tow%>%left_join(Hydrology_Daily,by=c("SampleDate","Year","Month","JulianDay"))%>%
  #   relocate(WaterYear:May8Riv,.after=JulianDay)
  # 

Tow_Position_Data <- Working_Long %>% filter(Year>2001, is.na(Start_Latitude)==F)%>%
  select(SampleDate,Year,Month,TowNumber,Region,SubRegion,SurveySeason,StationCode,
         Start_Longitude:End_Latitude,Station_Longitude,Station_Latitude,Volume)%>%
  mutate(Start_Latitude = abs(Start_Latitude),
         End_Latitude = abs(End_Latitude))%>%
  distinct()%>%
  filter((Start_Longitude != 0 & 
           Start_Latitude != 0 & 
           End_Longitude!=0 & 
           End_Latitude>30) %>% replace_na(TRUE))%>%
  pivot_longer(cols=Start_Longitude:Station_Latitude,names_to=c("Type","Axis"),values_to="Coordinate",names_sep="_")%>%
  pivot_wider(names_from=Axis,values_from=Coordinate)%>%
  filter(Longitude> -122.55 & Latitude > 37.5)




write_csv(Tow_Position_Data,"SpatialData/TowStartEndPositions.csv")

#====================================================================================
#================================Add non-CDFW surveys================================

#==========Create integrated long data frame with ALL surveys (CDFW+Others)
All_Surveys_Master <- Long_Master %>% 
  rename("Depth_Stratum" = "strata_depth")%>%
  mutate(Area = NA,
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
         Area,
         CommonName,
         ForkLength,
         Gear,
         Year,
         Month,
         Region,
         SubRegion,
         Depth_Stratum,
         Core_Survey,
         WindDirection,
         Secchi,
         ConductivityTop,
         TemperatureTop,
         Turbidity,
         Weather,
         Waves,
         Tide,
         Microcystis,
         TowDirection,
         DepthBottom,
         CableOut,
         TowDepth)%>%
  add_row(Additional_Surveys)%>%
  mutate(Year = year(SampleDate),
         Month = month(SampleDate))%>%
  #remove low effort months from surveys
  filter(case_when(SurveySeason=="EDSM" ~ Month<4|Month>6,
                   T ~ Month < 13))%>%
  mutate(NetType = Gear)%>%
  mutate(NetType = recode(NetType,
                          "20mm" = "Pelagic Trawl",
                          "Egg and Larva Net" = "Pelagic Trawl",
                          "Kodiak" = "Pelagic Trawl",
                          "Midwater trawl" = "Pelagic Trawl",
                          "Tenera" = "Pelagic Trawl",
                          "Otter" = "Bottom Trawl",
                          "Otter trawl" = "Bottom Trawl",
                          "SEIN" = "Beach Seine",
                          "MWTR" = "Pelagic Trawl",
                          "Townet" = "Pelagic Trawl",
                          "Mamou" = "Pelagic Trawl"
                          ))%>%
  mutate(CommonName = as.factor(CommonName))%>%
  
  #Add age cutoffs 
  
  left_join(cutoffs,by=c("CommonName","Month"))%>%
  
  #convert volume to 1000s of cubic meters
  
  mutate(Volume = Volume/1000)
  

Long_Master <- Long_Master %>% mutate(CommonName = as.factor(CommonName))

All_Surveys_LF_Master <- All_Surveys_Master %>% group_by(SurveySeason,StationCode,SampleDate,TowNumber,CommonName,ForkLength)%>%
  mutate(Count = n())%>%
  ungroup()%>%
  distinct(across(c("SurveySeason","StationCode","SampleDate","TowNumber","CommonName","ForkLength")),.keep_all = T)



save(Long_Master,file ="MASTER_Data/MASTER_CDFWSurveys_Long_Format.rda")

save(Environment_Hydrology,file ="MASTER_Data/MASTER_Env_Hydro.rda")

save(All_Surveys_Master,file =  "MASTER_Data/MASTER_All_Surveys.rda")

save(All_Surveys_LF_Master,file =  "MASTER_Data/MASTER_All_Surveys_LF.rda")
