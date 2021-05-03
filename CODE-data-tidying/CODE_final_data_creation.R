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


#Set working directory
setwd("C:/Users/40545/Documents/GitHub/pelagicsurveys")

#Load MASTER Data versions

load("MASTER_Data/MASTER_Env_Hydro.rda")
load("MASTER_Data/MASTER_All_Surveys.rda")

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
  select(-c(Salinity_Pred,Conductivity))


                             
#Load strata volume table
Strata_Volumes <- read_csv("SpatialData/Strata_Volumes.csv")%>%
  mutate(SubRegion = as.factor(SubRegion))

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
  
  #Deal with NA regions/subregions
  mutate(Region = if_else(SubRegion == "SF and Outer SP Bays","Far West",as.character(Region)))%>%
  mutate(Region = as.factor(Region))



#Working Data contains all surveys including Non-CDFW and can be used as needed
#Proceed to filter towards final CDFW dataset


Working_Data_Review <- Working_Data %>%
  filter(Year>2001&Core_Survey==T)%>%
  mutate(CommonName = as.factor(if_else(CommonName %in% Target_Species$CommonName,CommonName,OrganismCategory)))%>%
  dplyr::select(c(SurveySeason,Year,Month,SampleDate,StationCode,Temperature,Secchi,Turbidity,CableOut,Depth,TowDepth,Salinity,
                  Station_Latitude,Station_Longitude,Tide,Weather,Waves,CommonName,Volume,
                  ForkLength,Region,SubRegion,TowNumber,Age))%>%
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
  select(-VolCut)




Review_Data_Tows <- Working_Data_Review %>%select(-c(Age,ForkLength))%>%
  distinct(across(c(StationCode,SampleDate,TowNumber,CommonName,SurveySeason)),.keep_all = TRUE)%>%
  pivot_wider(values_from = c(CPUV,Mean_Length),names_from = CommonName)%>%
  mutate_at(vars(contains("CPUV")), ~replace_na(., 0))%>%
  select(-c(CPUV_No_Catch,Mean_Length_No_Catch))%>%
  
  #Drop presumed duplicated tows that have the same date, station and tow number, but differences in 
  #Environmental measurements. 
  
  distinct(across(c("SampleDate","StationCode","SurveySeason","TowNumber")),.keep_all = T)


Review_Data_Long <- Working_Data_Review%>%filter(CommonName != "No_Catch")%>%
  mutate(CommonName = as.factor(CommonName))




#Create station-level summary

Mean <- function(x){return(mean(x,na.rm=T))}
CV <- function(x){return(sd(x,na.rm=T)/mean(x,na.rm=T))}


Review_Data_Stations <- Review_Data_Tows %>% 
  select(SurveySeason:CPUV_Prickly_Sculpin_Age_0)%>%
  select(-c(Tide,Weather,Waves,TowNumber))%>%
  group_by(StationCode)%>%
  mutate(N_Dates = length(unique(SampleDate)),
         N_Years = length(unique(Year)),
         Mean_TowsPerYear = round(N_Dates/N_Years,0))%>%
  mutate(Surveys = paste(unique(SurveySeason),collapse=","),.after = StationCode)%>%
  group_by(StationCode,SurveySeason)%>%
  mutate_at(c("Turbidity","Salinity","Temperature","Depth","TowDepth","Volume"),list(Mean=Mean,CV=CV))%>%
  mutate_at(vars(contains("CPUV")),mean)%>%
  select(-c(SurveySeason:SampleDate,Temperature:Salinity))%>%
  ungroup()%>%
  distinct(across(c(StationCode,SurveySeason)),.keep_all = T)%>%
  filter(N_Years>9)#%>%
  # mutate(FMWT = if_else(grepl("FMWT",Surveys),TRUE,FALSE),
  #        STN = if_else(grepl("STN",Surveys),TRUE,FALSE),
  #        SLS = if_else(grepl("SLS",Surveys),TRUE,FALSE),
  #        SKT = if_else(grepl("SKT",Surveys),TRUE,FALSE),
  #        TMM = if_else(grepl("20mm",Surveys),TRUE,FALSE),
  #        .after = Surveys
  #        )


save(Review_Data_Long,Review_Data_Tows,Review_Data_Stations,Review_Enviro_Hydro,file="FINAL_REVIEW_DATA/CDFW_Pelagic_Review_Data.rda")

