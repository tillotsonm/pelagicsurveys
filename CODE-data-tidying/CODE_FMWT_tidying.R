#==============================================================================
#=====Code for integrating Fall Midwater Trawl database files to Tidy Format===
#==============================================================================
#Prepared by Michael Tillotson, ICF
#Created February 17, 2021
#==============================================================================
#Load necessary packages
library(tidyverse)
library(lubridate)

#Set working directory, adjust as needed to locate pelagicsurveys folder
setwd("C:/Users/40545/Documents/GitHub/pelagicsurveys")

#Read database files and assign column types if needed
Sample <- read_csv("RawData/FMWT/Sample.csv",
                   col_types = "dDdffdddddddlddddddffffffddddddddddddfd")[,c(1,2,4,5,3,6,7,12,8,9,20,11,10,13,21,25,23,14,22,24,26:37,38,39,15:19)]%>% 
  mutate_at(vars(StationCode:MethodCode,
                 WindDirection,
                 Microcystis,
                 WeatherCode,
                 WaveCode,
                 Gear),as.factor)%>%
  mutate_at(vars(SurveyNumber:CableOut,
                 Turbidity,
                 StartLatDegrees:EndLongSeconds,
                 MeterDifference,
                 TowDirectionCode),as.numeric)


Catch <- read_csv("RawData/FMWT/Catch.csv",col_types = "ddddD")%>%
  filter(Catch>0)

Length <- read_csv("RawData/FMWT/Length.csv",col_types="dddffdfD")

Station <- read_csv("RawData/FMWT/StationsLookUp.csv")%>%
  rename(longitude_FMWT = DD_Longitude, latitude_FMWT = DD_Latitude)

#Lookup tables
luMicrocystis <- read_csv("RawData/FMWT/luMicrocystis.csv",col_types = "fc")

luOrganism <- read_csv("RawData/FMWT/OrganismsLookup.csv")

luTide <- read_csv("RawData/FMWT/luTide.csv")

luWaves <- read_csv("RawData/FMWT/luWaves.csv",col_types = "ff")

luWeather <- read_csv("RawData/FMWT/luWeather.csv",col_types = "ff")

luTowDirection <- read_csv("RawData/FMWT/luTowDirection.csv")

luAreaWeights <- read_csv("RawData/FMWT/StationArea.csv",
                        col_types = "dfdddddd")%>%rename("AREA"="Area")%>%
                          left_join(read_csv("RawData/FMWT/AreaWT.csv"),by="AREA")%>%
  rename("Area"="AREA","StationCode"="Station")


#Join all tibbles and replace numeric codes with descriptive factors.
#Removes all zooplankton tows
FMWT_Tidy_All <-Sample %>% 
  rename("TowDirectionID"="TowDirectionCode")%>%  #========Rename TowDirection======
left_join(luTowDirection,by="TowDirectionID")%>%
  select(-TowDirectionID)%>%
  rename("TideRowID"="TideCode")%>%#========Rename Tide Stage======
left_join(luTide,by="TideRowID")%>%
  select(-TideRowID)%>%
  rename("MicrocystisID"="Microcystis")%>%#========Rename Microcystis======
left_join(luMicrocystis,by="MicrocystisID")%>%
  select(-MicrocystisID)%>%
  left_join(luWeather,by="WeatherCode")%>%#========Add Weather======
select(-WeatherCode)%>%
  left_join(luWaves,by="WaveCode")%>%#========Add Waves======
#Convert coordinates to decimal degrees
mutate(Start_Longitude=-(StartLongDegrees+StartLongMinutes/60+StartLongSeconds/3600),
       Start_Latitude=-(StartLatDegrees+StartLatMinutes/60+StartLatSeconds/3600),
       End_Longitude=-(EndLongDegrees+EndLongMinutes/60+EndLongSeconds/3600),
       End_Latitude=-(EndLatDegrees+EndLatMinutes/60+EndLatSeconds/3600))%>%
  select(-c(StartLatDegrees:EndLongSeconds))%>%
  select(-WaveCode)%>%
  
#Join to catch table
  full_join(Catch,by="SampleRowID")%>%
  
#Join to length table
  full_join(Length,by="CatchRowID")%>%
  
#Join to station table
  left_join(Station,Station,by="StationCode")%>%
  left_join(luAreaWeights,by="StationCode")%>%
  #========Add Taxonomy======
left_join(luOrganism,by="OrganismCode")%>%
  add_column(SurveySeason="FMWT",.after = "SampleRowID")%>%
  mutate(Year=year(SampleDate),
         Month=month(SampleDate),
         JulianDay = yday(SampleDate),
         Survey_Station = paste(SurveySeason,StationCode,sep="_"),
         .after="SampleDate")%>% 
  
#Create CommonName for No Catch Tows and correct Catch, LengthFrequency columns
mutate(CommonName = if_else(is.na(Catch)==T,"No Catch",CommonName),
       Catch = if_else(is.na(Catch)==T,1,Catch),
       LengthFrequency = if_else(is.na(Catch)==T,1,LengthFrequency))%>%

#Rename a selection of variables
  rename("StationComments" = "Comments.x",
         "StationActive" = "Active.x",
         "SpeciesComment" =  "Comments.y",
         "SpeciesActive" = "Active.y",
         "TemperatureTop"="WaterTemperature",
         "TemperatureBottom"="BottomTemperature",
         "TimeStart" = "SampleTimeStart",
         "TimeStop" = "SampleTimeEnd",
         "MeterIn" = "MeterStart",
         "MeterOut" = "MeterEnd",
         "OrganismCodeFMWT" = "OrganismCode",
         "Station_Longitude" = "longitude_FMWT",
         "Station_Latitude" = "latitude_FMWT",
         "WeightingFactor" = "WTFACTOR"
         )%>%
  mutate(TowNumber = 1,.after=TowDuration)%>%
  mutate(ZeroLength=if_else(ForkLength==0,TRUE,FALSE))%>%
  relocate(Phylum:Species,.after = OrganismCodeFMWT)%>%
  relocate(Station_Longitude:Station_Latitude,.after = StationCode)%>%
  relocate(ZeroLength,.after = ForkLength)%>%
  relocate(Area,.after = OrderNum)%>%
  relocate(WeightingFactor, .after = Area)%>%
  relocate(CommonName, .after = WeightingFactor)%>%
  relocate(LengthFrequency, .after = StationActive)%>%
  mutate_at(vars(Survey_Station:StationCode,Area,CommonName),as.factor)%>%
  mutate(OrderNum = as.numeric(OrderNum))
  

#=========
# FMWT_Tidy_All$Dead[FMWT_Tidy_All$Dead=="n/p"] <- NA
# FMWT_Tidy_All$MarkCode[FMWT_Tidy_All$MarkCode=="n/p"] <- NA


#Remove redundant or non-essential variables
FMWT_Tidy <- FMWT_Tidy_All %>% 
  select(-c(SampleRowID,CatchRowID,DateEntered.x,DateEntered.y,LengthRowID,StationRowID,
            Gear,OrganismRowID,X,Y,Offset,MethodCode,SecchiEstimated,AreaRowID,
            RKI,`Channel/Shoal`:OrganismSymbol,NormalizedCode))%>%
  rename(Comment1 = StationComments,
         Comment2 = SpeciesComment)%>%
  add_column(Comment3=NA)%>%
  
  #Convert secchi depth to CM
  mutate(Secchi = Secchi*100)

save(FMWT_Tidy,file="TidyData/Individual Surveys/DATA_FMWT_Tidy.rda")


