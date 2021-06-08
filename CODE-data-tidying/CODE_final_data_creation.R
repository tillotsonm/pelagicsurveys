#====================================================================
#===Code to for maintaining concise data versions for use in CDFW survey review
#Anlaysis

#1) long format with all variables
#2) tow-level with all variables, CPUV and mean lengths
#)  tow-level with presence/absence binary
#3) station-level 


#Prepared by Michael Tillotson
#ICF
#Created April 23, 2021

#Load libraries:
require(tidyverse)
require(lubridate)
select <- dplyr::select

#Set working directory
setwd("C:/Users/40545/Documents/GitHub/pelagicsurveys")

#Load MASTER Data versions

load("MASTER_Data/MASTER_Env_Hydro.rda")
load("MASTER_Data/MASTER_All_Surveys.rda")

#Read in cluster-based regions and strata

Review_Strata <- read_csv("CODE-data-tidying/Review_Strata.csv",col_types = "fff")


#Add Season Variable===============================================

Review_Enviro_Hydro <- Environment_Hydrology %>%
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
  mutate(SubRegion=factor(SubRegion, levels=unique(SubRegion)))%>%
  
  #Calculate salinity from conductivity where missing, keep salinity only
  mutate(Salinity_Pred = predict(lm(Salinity ~ Conductivity + I(Conductivity^2),
                               data=Environment_Hydrology),newdata = .))%>%
  mutate(Salinity = if_else(is.na(Salinity)==T&is.na(Salinity_Pred)==F,Salinity_Pred,Salinity))%>%
  mutate(Salinity = if_else(Salinity <0,0,Salinity))%>%
  select(-c(Salinity_Pred,Conductivity))%>%
  filter(is.na(Longitude)==F & Year >1974 & is.na(Region)==F)%>%
  select(-c(JulianDay,SAC:SacTotal,SJWinter:SJTotal,Dec8Riv:May8Riv))%>%
  mutate(Current_Historical = if_else(Year>2001,"Current","Historical"))%>%
  
  #Add review strata
  left_join(Review_Strata,by="SubRegion")%>%

  group_by(Review_Region,Year,Month)%>%
  mutate(Region_Tows_Month = n())%>%
  ungroup()%>%
  group_by(StationCode,Survey,Month,Year)%>%
  mutate(Station_Tows_Month = n())%>%
  ungroup()%>%
  group_by(Review_Stratum,Survey,Month,Year)%>%
  mutate(Strata_Tows_Month = n())%>%
  ungroup()%>%
  relocate(c(Review_Region,Review_Stratum),.after=SubRegion)


#Create target species list
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
))%>%
  mutate(CommonName = as.factor(CommonName))

#Apply filters and add needed variables
Working_Data <- All_Surveys_Master %>%ungroup()%>%
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
  mutate(Target = if_else(CommonName %in% Target_Species$CommonName,TRUE,FALSE))%>%
  
  #Assign length-based ages, retain NAs, but drop 2+ year olds
  
  mutate(Age = "Age_2+")%>%
  mutate(Age = if_else(ForkLength>Age0Cut&ForkLength<Age1Cut,"Age_1",Age))%>%
  mutate(Age = if_else(ForkLength<Age0Cut,"Age_0",Age))%>%
  
  mutate(SubRegion = droplevels(SubRegion))%>%
  dplyr::rename("Conductivity" = "ConductivityTop",
                "Depth" = "DepthBottom",
                "Temperature" = "TemperatureTop")%>%
  
  #Calculate salinity from conductivity where missing
  mutate(Salinity = predict(lm(Salinity ~ Conductivity + I(Conductivity^2),
                               data=Environment_Hydrology),newdata = .))%>%
  mutate(Salinity = if_else(Salinity <0,0,Salinity))%>%
  ungroup()%>%
  
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
  
  #Add review strata
  left_join(Review_Strata,by="SubRegion")%>%
  relocate(c(Review_Region,Review_Stratum),.after=SubRegion)


#Working Data contains all surveys including Non-CDFW and can be used as needed
#Proceed to filter towards final CDFW dataset


Working_Data_Review <- Working_Data %>%
  filter(Year>2001&Core_Survey==T)%>%
  mutate(CommonName = as.factor(if_else(CommonName %in% Target_Species$CommonName,CommonName,OrganismCategory)))%>%
  dplyr::select(c(SurveySeason,Year,Month,SampleDate,
                  StationCode,Temperature,
                  Secchi,Turbidity,
                  CableOut,Depth,
                  TowDepth,Salinity,
                  Station_Latitude,
                  Station_Longitude,
                  Tide,Weather,Waves,
                  CommonName,Volume,
                  ForkLength,Region,
                  SubRegion,TowNumber,
                  TowDirection,Microcystis,
                  Age,
                  Review_Region,
                  Review_Stratum))%>%
  group_by(SurveySeason,SampleDate,StationCode,TowNumber,CommonName)%>%
  mutate(Catch = n())%>%
  mutate(CPUV = Catch/Volume)%>%
  mutate(CPUV = round(CPUV,2))%>%
  ungroup()%>%
  select(-Catch)%>%
  mutate(CommonName = recode(CommonName,
                             "Shimofuri_Goby"= "Tridentiger_Spp.",
                             "Shokihaze_Goby" = "Tridentiger_Spp."))%>%
  mutate(CommonName = droplevels(CommonName))%>%
  filter(Age != "Age_2+"|is.na(Age)==T)%>%
  mutate(Age = as.factor(Age),
         CommonName = as.character(CommonName))%>%
  mutate(CommonName = if_else(is.na(Age),CommonName,paste0(CommonName,"_",Age)))%>%
  filter(!(CommonName %in% c("American_Shad","Chinook_Salmon","Delta_Smelt",
                           "Longfin_Smelt","Northern_Anchovy","Striped_Bass",
                           "Threadfin_Shad","White_Catfish","Tridentiger_Spp.")))%>%
  
  #Calculate mean forklength by age and species
  group_by(SurveySeason,SampleDate,StationCode,TowNumber,CommonName)%>%  
  mutate(Mean_Length = mean(ForkLength,na.rm=T),
    Mean_Length = if_else(!(CommonName %in% c("Other_Crustacean",
                                                   "Other_Fish",
                                                   "Crangon",
                                                   "Gelatinous",
                                                   "No_Catch")),
                               Mean_Length,0))%>%
  mutate(Mean_Length = na_if(Mean_Length,0))%>%
  
  ungroup()%>%
  #==Average coordinates by station code
  group_by(StationCode)%>%
  mutate(Station_Longitude = mean(Station_Longitude,na.rm = T),
         Station_Latitude = mean (Station_Latitude,na.rm=T))%>%
  ungroup()%>%
  mutate(SurveySeason = droplevels(SurveySeason))%>%
  
  #Remove likely volume calculation errors
  mutate(VolCut = as.numeric(recode(SurveySeason,
                                    "20mm" = .5,
                                    "FMWT" = 2,
                                    "STN" = .5,
                                    "SLS" = .1,
                                    "SKT" = 2)),.after=Volume)%>%
  filter(Volume > VolCut)%>%
  select(-VolCut)%>%
  filter(Volume <12)%>%
  
  #Deal with NA regions/subregions
  mutate(SubRegion = replace_na(as.character(SubRegion),"SF and Outer SP Bays"))%>%
  mutate(SubRegion = as.factor(SubRegion))%>%
  mutate(Region = if_else(SubRegion == "SF and Outer SP Bays","Far West",as.character(Region)))%>%
  mutate(Region = as.factor(Region))%>%
  mutate(Review_Region = replace_na(as.character(Review_Region),"Far West"))%>%
  mutate(Review_Region = as.factor(Review_Region))%>%
  mutate(Review_Stratum = replace_na(as.character(Review_Stratum),"San Pablo Bay and Carquinez Strait"))%>%
  mutate(Review_Stratum = as.factor(Review_Stratum))




Review_Data_Tows <- Working_Data_Review %>%
  filter(is.na(Station_Latitude)==F)%>%
  select(-c(Age,ForkLength))%>%
  distinct(across(c(StationCode,SampleDate,TowNumber,CommonName,SurveySeason)),.keep_all = TRUE)%>%
  pivot_wider(values_from = c(CPUV,Mean_Length),names_from = CommonName)%>%
  mutate_at(vars(contains("CPUV")), ~replace_na(., 0))%>%
  select(-c(CPUV_No_Catch,Mean_Length_No_Catch))%>%
  
  #Drop presumed duplicated tows that have the same date, station and tow number, but differences in 
  #Environmental measurements. 
  
  distinct(across(c("SampleDate","StationCode","SurveySeason","TowNumber")),.keep_all = T)%>%
  
  #Add review strata

  
  #Arrange factors according to mean longitude
  mutate(StationCode = factor(StationCode))%>%
  group_by(Review_Region)%>%
  mutate(Mean_Lat = mean(Station_Latitude),
         Mean_Lon = mean(Station_Longitude))%>%
  ungroup()%>%
  arrange(Mean_Lon)%>%
  mutate(Review_Region = factor(Review_Region,levels=unique(Review_Region)))%>%
  group_by(StationCode)%>%
  mutate(Mean_Lat = mean(Station_Latitude),
         Mean_Lon = mean(Station_Longitude))%>%
  ungroup()%>%
  arrange(Mean_Lon)%>%
  mutate(StationCode = factor(StationCode,levels=unique(StationCode)))%>%
  group_by(SubRegion)%>%
  mutate(Mean_Lat = mean(Station_Latitude),
         Mean_Lon = mean(Station_Longitude))%>%
  ungroup()%>%
  arrange(Mean_Lon)%>%  
  mutate(SubRegion = factor(SubRegion,levels=unique(SubRegion)))%>%
  mutate(Year_Month = paste0(Year,"_",Month),.after=Month)%>%
  select(-c(Mean_Lat,Mean_Lon))%>%
  
  #add variable listing surveys
  group_by(StationCode)%>%
  mutate(Surveys = paste(unique(SurveySeason),collapse=","),.after = StationCode)%>%
  ungroup()%>%
  select(-c(CPUV_Tridentiger_Spp._Age_1,CPUV_Starry_Flounder_Age_1,CPUV_Chinook_Salmon_Age_1))


#Check proportion of 0-catch tows for each species/age
#Review_Data_Tows%>%select(contains("CPUV"))%>%apply(.,MARGIN=2,FUN=function(x){round(length(x[x==0])/length(x),3)})


Review_Data_Long <- Working_Data_Review%>%filter(CommonName != "No_Catch")%>%
  mutate(CommonName = as.factor(CommonName))


Review_Data_LF <- Working_Data_Review %>%
  group_by(SurveySeason,StationCode,SampleDate,TowNumber,CommonName,ForkLength)%>%
  mutate(Count = n(),.after=ForkLength)%>%
  ungroup()%>%
  distinct(across(c("SurveySeason","StationCode","SampleDate","TowNumber","CommonName","ForkLength")),.keep_all = T)



#Condense repeated tows to date/station level (mean CPUV, drop lengths)
Review_Data_By_Station <- Review_Data_Tows%>%
  filter(is.na(Region)==F)%>%
  select(-contains("Length"))%>%
  group_by(Region, Review_Region,Review_Stratum,SubRegion, Surveys, SurveySeason,StationCode,SampleDate,
           Year,Month,Station_Longitude,Station_Latitude)%>%
  summarise_at(vars(c(Salinity,
                      Secchi,
                      Turbidity,
                      Temperature,
                      Depth,
                      TowDepth,
                      contains("CPUV"),
  )),mean,na.rm=T)%>%
  ungroup()%>%
  mutate_all(~replace(., is.nan(.), NA))


#Create station-level summary

Mean <- function(x){return(mean(x,na.rm=T))}
CV <- function(x){return(sd(x,na.rm=T)/mean(x,na.rm=T))}


Review_Data_Locations <- Review_Data_Tows %>% 
  select(SurveySeason:CPUV_Prickly_Sculpin_Age_0)%>%
  select(-c(Tide,Weather,Waves,TowNumber))%>%
  group_by(StationCode,SurveySeason)%>%
  mutate(N_Dates = length(unique(SampleDate)),
         N_Years = length(unique(Year)),
         Mean_TowsPerYear = round(N_Dates/N_Years,0))%>%
  mutate(Surveys = paste(unique(SurveySeason),collapse=","),.after = StationCode)%>%
  group_by(StationCode)%>%
  mutate_at(c("Turbidity","Salinity","Temperature","Depth","TowDepth","Volume"),list(Mean=Mean,CV=CV))%>%
  mutate_at(vars(contains("CPUV")),mean)%>%
  select(-c(SurveySeason:SampleDate,Temperature:Salinity))%>%
  ungroup()%>%
  distinct(across(c(StationCode)),.keep_all = T)#%>%
  #filter(N_Years>9)#%>%
  # mutate(FMWT = if_else(grepl("FMWT",Surveys),TRUE,FALSE),
  #        STN = if_else(grepl("STN",Surveys),TRUE,FALSE),
  #        SLS = if_else(grepl("SLS",Surveys),TRUE,FALSE),
  #        SKT = if_else(grepl("SKT",Surveys),TRUE,FALSE),
  #        TMM = if_else(grepl("20mm",Surveys),TRUE,FALSE),
  #        .after = Surveys
  #        )

save(Review_Data_Long,
     Review_Data_LF,
     Review_Data_Tows,
     Review_Data_By_Station,
     Review_Data_Locations,
     Review_Enviro_Hydro,
     file="FINAL_REVIEW_DATA/CDFW_Pelagic_Review_Data.rda")


write_csv(Review_Data_Tows,"Pelagic_Review_Data.csv")


# Review_Data_Tows%>%group_by(Region,SurveySeason,SubRegion)%>%
#   mutate(N_Stations = length(unique(StationCode)))%>%
#   select(SurveySeason,Region,SubRegion,N_Stations)%>%
#   distinct()%>%
#   write_csv("StationsPerSubregion.csv")



Prop_Detect <- Review_Data_By_Station%>%
  group_by(Review_Region,Review_Stratum,SurveySeason)%>%
  summarise_at(vars(contains("CPUV")), ~round(sum(.!= 0)/n(),2))





