#====================================================================
#===Code to prepare additional study data outside of the CDFW review studies
#Includes:
#EDSM
#DJFMP
#Bay Study
#ICF's Tidal Parr Study


#Prepared by Michael Tillotson
#ICF
#Created March 30, 2021

#Load libraries:
require(ggforce)
require(tidyverse)
require(lubridate)


#Set working directory
setwd("C:/Users/40545/Documents/GitHub/pelagicsurveys")


#Prepare ancillary study data for comparison with CDFW surveys

#Ancillary survey station location classifications

Ancillary_Stations <- read_csv("SpatialData/ancillary_stations_join_EDSM_depth_strata.csv",
                               col_types = "fddfffff")%>%
  select(-c(strata_depth,Station_Longitude,Station_Latitude))%>%
  rename("Depth_Stratum" = "category")


#Load long format Five Surveys data

load("TidyData/DATA_CDFW_Surveys_Tidy.rda")

#Load DJFMP Data

load("RawData/DJFMP/DJFMP_Raw.rda")
DJFMP_Raw <- DJFMP_Raw %>% 
  left_join(read_csv("RawData/DJFMP/DJFMP_Site_Locations.csv"),
            by=c("StationCode","MethodCode","Location"))%>%
  rename("Station_Latitude" = "Latitude_location",
         "Station_Longitude" = "Longitude_location")%>%
  mutate(CommonName = str_replace(str_to_title(CommonName)," ","_"))
#Load ICF Tidal Parr data


TPS_Tow <- read_csv("RawData/ICF TPS/TPS_Tow.csv")%>%
  rename("StationCode" = "Site_Id",
         "Station_Latitude" = "Latitude",
         "Station_Longitude" = "Longitude",
         "Volume" = "FishTowVolume")

TPS_Length <- read_csv("RawData/ICF TPS/TPS_Lengths.csv")%>%
  rename("CommonName" = "Species",
         "StationCode" = "Site_Id")

#Load EDSM Data

EDSM_20mm <- read_csv("RawData/EDSM/EDSM_20mm.csv")%>%
  rename("Station_Latitude" = "TargetLat",
         "Station_Longitude" = "TargetLong",
         "StationCode" = "Station",
         "SampleDate" = "Date",
         "DOTop" = "TopDO",
         "TemperatureTop" = "TopTemp",
         "ConuctivityTop" = "TopEC",
         "DepthBottom" = "Depth",
         "Turbidity" = "TopTurb",
         "TowNumber" = "Tow")%>%
  add_column("Gear" = "20mm","TowMax" = NA)

EDSM_Kodiak <- read_csv("RawData/EDSM/EDSM_KDTR.csv")%>%
  rename("Station_Latitude" = "TargetLat",
         "Station_Longitude" = "TargetLong",
         "StationCode" = "Station",
         "DOTop" = "DO",
         "TemperatureTop" = "Temp",
         "ConuctivityTop" = "EC",
         "DepthBottom" = "StartDepth",
         "Turbidity" = "Turb",
         "TowNumber" = "Tow")%>%
  add_column("Gear" = "Kodiak")%>%select(-c(TagCode,RaceByTag,Comments))







#LTMR Data package required, obtain from GitHub if needed
require(LTMRdata)



#Extract full LTMR data from package
LTMR_Raw <- LTMRdata::LTMRpilot()%>%filter((is.na(Length_NA_flag)==T|Length_NA_flag=="No fish caught")&Source != "FMWT")%>%
  rename("Gear" = "Method")%>%
  mutate(Taxa = if_else(is.na(Taxa)==T,"No Catch",Taxa),
         Count = if_else(Taxa=="No Catch",1,round(Count,0)))


#Compile common name taxa list from Five Surveys file

Master_Taxa <- CDFW_Surveys_Long %>% distinct (across(CommonName),.keep_all = T)%>%select(CommonName,Genus,Species)%>%
mutate(CommonName = recode(CommonName,"Age-0 Striped Bass" = "Striped Bass",
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
                                    "Striped Bass Adult" = "Striped Bass",
                                    "Striped Bass Age 0" = "Striped Bass",
                                    "Striped Bass Age 1" = "Striped Bass"),
       Species = if_else(CommonName=="No Catch","Catch",Species),
       Genus = if_else(CommonName=="No Catch","No",Genus))%>%
  distinct()%>%
mutate(CommonName = str_replace(str_to_title(CommonName)," ","_")) #Common Name to title case and add _ %>%  


#Convert Bay Study to long format, add CommonName field

LTMR_Tidy <- LTMR_Raw %>% uncount(Count)%>%select(-c(Datetime,SampleID,Secchi_estimated,
                                                     Length_NA_flag,Notes_catch,Notes_tow))%>%
  rename("SurveySeason" = "Source",
         "StationCode" = "Station",
         "Station_Latitude" = "Latitude",
         "Station_Longitude" = "Longitude",
         "SampleDate" = "Date",
         "ForkLength" = "Length",
         "Volume" = "Tow_volume",
         "Area" = "Tow_area")%>%
  separate(Taxa, into = c("Genus","Species"))%>%
  left_join(Master_Taxa,by=c("Genus","Species"))%>%
  select(SurveySeason,StationCode,Station_Latitude,Station_Longitude,SampleDate,Volume,Area,CommonName,ForkLength,Gear)



#Combine and extend EDSM data

EDSM_Tidy <- (EDSM_20mm)%>%add_row(EDSM_Kodiak)%>% 
  rename("LengthFrequency" = "SumOfCatchCount")%>%
  mutate(CommonName = str_replace(str_to_title(CommonName)," ","_"))%>% #Common Name to title case and add _
  group_by(SampleDate,StationCode,TowNumber,CommonName)%>%
  mutate(TotalCount = sum(LengthFrequency))%>%
  mutate(TotalMeasured = sum(LengthFrequency[ForkLength>0]))%>%
  ungroup()%>% 
  mutate(LengthFrequency_Adjusted = round(TotalCount*(LengthFrequency/TotalMeasured),0))%>%
  mutate(LengthFrequency_Adjusted = if_else(is.infinite(LengthFrequency_Adjusted),
                                            LengthFrequency,LengthFrequency_Adjusted))%>%
  filter((ForkLength > 0 & TotalCount != 0)|CommonName=="No_Catch")%>%
  mutate(ForkLength = na_if(ForkLength,0))%>%
  select(-c(TotalCount,TotalMeasured,Expression:FishComments,PairedDepth,GearConditionCode))%>%
  filter(is.na(LengthFrequency_Adjusted)==F)%>%
  uncount(LengthFrequency_Adjusted)%>%
  select(StationCode,Station_Latitude,Station_Longitude,SampleDate,TowNumber,Volume,CommonName,ForkLength,Gear)%>%
  mutate(SurveySeason = "EDSM",
         Area = NA)
  
  


#Wrangle TPS Data


TPS_Tidy <- TPS_Tow %>% pivot_longer(cols = c(American_Shad:Yellowfin_Goby),
                                     names_to = "CommonName",
                                     values_to = "TotalCatch")%>%
  filter(is.na(TotalCatch)==F)%>%
  right_join(TPS_Length,by=c("CommonName","StationCode"))%>%
  filter(Fish_Length>0 & CommonName != "No_Catch")%>%
  group_by(StationCode,CommonName)%>%
  mutate(TotalMeasured = n())%>%ungroup()%>%
  group_by(StationCode,CommonName,Fish_Length)%>%
  mutate(LengthFrequency = n())%>%
  distinct()%>%
  rename("ForkLength" = "Fish_Length",
         "SampleDate" = "Date",
         "Gear" = "Fish_Net")%>%
  mutate(LengthFrequency_Adjusted = round(TotalCatch*(LengthFrequency/TotalMeasured),0))%>%
  ungroup()%>%
  select(-c(TotalMeasured:LengthFrequency))%>%
  filter(LengthFrequency_Adjusted>0)%>%
  uncount(LengthFrequency_Adjusted)%>%
  select(StationCode,Station_Latitude,Station_Longitude,SampleDate,CommonName,ForkLength,Gear)%>%
  mutate(SurveySeason = "ICF_TPS")


#Wrangle DJFMP Data

DJFMP_Tidy <- DJFMP_Raw%>%tibble()%>%
  mutate(Year= year(SampleDate),Month=month(SampleDate))%>%
  group_by(SampleDate,StationCode,TowNumber,CommonName)%>%
  mutate(TotalCount = sum(Count))%>%
  mutate(TotalMeasured = sum(Count[ForkLength>0]))%>%
  ungroup()%>% 
  mutate(LengthFrequency_Adjusted = round(TotalCount*(Count/TotalMeasured),0))%>%
  mutate(LengthFrequency_Adjusted = if_else(is.infinite(LengthFrequency_Adjusted),
                                            Count,LengthFrequency_Adjusted))%>%
  filter(ForkLength > 0 & TotalCount != 0)%>%
  uncount(LengthFrequency_Adjusted)%>%
  select(StationCode,Station_Latitude,Station_Longitude,SampleDate,TowNumber,Volume,CommonName,ForkLength,MethodCode)%>%
  mutate(SurveySeason = "DJFMP")%>%
  rename("Gear" = "MethodCode")


#=====================================================================================
#Join all additional surveys
Additional_Surveys <- EDSM_Tidy %>% 
  add_row(DJFMP_Tidy)%>%
  add_row(LTMR_Tidy)%>%
  add_row(TPS_Tidy)%>%
  mutate(Year = year(SampleDate),Month = month(SampleDate),
          Station_Longitude = round(Station_Longitude,5),
          Station_Latitude = round(Station_Latitude,5))%>%
  mutate(StationCode = as.factor(StationCode),
         SurveySeason = as.factor(SurveySeason))%>%
  left_join(Ancillary_Stations,by=c("SurveySeason","StationCode"))


Additional_Surveys <- Additional_Surveys %>% 
  group_by(CommonName,SurveySeason)%>%
  mutate(N=n())%>%
  ungroup()%>%
  filter(CommonName != "Unid_Fish"&
         CommonName != "Sculpin_Unknown",
         CommonName != "Smelt_Unknown",
         CommonName != "Minnow_Unknown",
         CommonName != "Lamprey_Unknown",)%>%
  mutate(CommonName = recode(CommonName,"Bass_Unknown" = "Centrarchid_(Unid)",
         "Steelhead_Trout" = "Steelhead",
         "Sacramento_Splittail" = "Splittail",
         "Pacific_Staghorn_Sculpin" = "Pacific_Staghorn Sculpin",
         "Centrarchid_Unknown" = "Centrarchid_(Unid)"
  ))%>%
  group_by(CommonName)%>%
  mutate(Max.N = max(N))%>%
  filter(Max.N>100) %>%
select(-c(N,Max.N))%>%
  ungroup() %>%
  
  mutate(Gear = recode(Gear,"KDTR" = "Kodiak"),
                    Core_Survey = FALSE)%>%
  #Assign DJFMP stations in Upper SJR
  mutate(SubRegion = as.factor(if_else(Station_Latitude<37.8 & Station_Longitude>-122,
                                       "Upper San Joaquin River",
                                       as.character(SubRegion))),
         Region = as.factor(if_else(Station_Latitude<37.8 & Station_Longitude>-122,
                                    "South",
                                    as.character(Region))))%>%
  
  #Assign DJFMP stations in Upper Sac
  mutate(SubRegion = as.factor(if_else(Station_Latitude>38.5 & SurveySeason=="DJFMP",
                                       "Upper Sacramento River",
                                       as.character(SubRegion))),
         Region = as.factor(if_else(Station_Latitude>38.5 & SurveySeason=="DJFMP",
                                    "North",
                                    as.character(Region))))%>%
  #Assign outer bay stations
  mutate(SubRegion = as.factor(if_else(is.na(SubRegion)==T&Station_Latitude<38 & Station_Longitude>-122,
                                       "SF and Outer SP Bays",
                                       as.character(SubRegion))),
         Region = as.factor(if_else(is.na(SubRegion)==T&Station_Latitude<38 & Station_Longitude>-122,
                                    "Far West",
                                    as.character(Region))))%>%
  mutate(SubRegion = as.factor(if_else(is.na(SubRegion)==T&SurveySeason=="Bay Study",
                                       "SF and Outer SP Bays",
                                       as.character(SubRegion))),
         Region = as.factor(if_else(is.na(SubRegion)==T&SurveySeason=="Bay Study",
                                    "Far West",
                                    as.character(Region))))%>%
  filter(is.na(SubRegion)==F)



save(Additional_Surveys,file="TidyData/Tidy_Non_CDFW_Surveys_Long.rda")


