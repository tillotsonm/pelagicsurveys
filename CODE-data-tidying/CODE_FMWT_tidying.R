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
  right_join(Catch,by="SampleRowID")%>%
  right_join(Length,by="CatchRowID")%>%
  left_join(Station,Station,by="StationCode")%>%
  left_join(luAreaWeights,by="StationCode")%>%
  #========Add Taxonomy======
left_join(luOrganism,by="OrganismCode")%>%
  add_column(SurveySeason="FMWT",.after = "SampleRowID")%>%
  mutate(Year=year(SampleDate),
         Month=month(SampleDate),
         JulianDay = yday(SampleDate),
         Survey_Station = paste(SurveySeason,StationCode,sep="_"),
         .after="SampleDate")%>% filter(is.na(LengthFrequency)==F)%>%
  rename("StationComments" = "Comments.x",
         "StationActive" = "Active.x",
         "SpeciesComment" =  "Comments.y",
         "SpeciesActive" = "Active.y")

#=========
FMWT_Tidy_All$Dead[FMWT_Tidy_All$Dead=="n/p"] <- NA
FMWT_Tidy_All$MarkCode[FMWT_Tidy_All$MarkCode=="n/p"] <- NA


names(FMWT_Tidy_All)

table(is.na(FMWT_Tidy_All$ConductivityBottom))
#Remove redundant or non-essential variables
FMWT_Tidy <- FMWT_Tidy_All %>% 
  select(-c(SampleRowID,CatchRowID,DateEntered.x,DateEntered.y,LengthRowID,StationRowID,Gear,OrganismRowID,X,Y,Offset))%>%
  mutate(ZeroLength=if_else(ForkLength==0,TRUE,FALSE))




FMWT_Tidy$ForkLength[FMWT_Tidy$ForkLength==0] <-NA
#========Assign tow-average lengths to unmeasured individuals==================
FMWT_Tidy_Counts <- FMWT_Tidy %>% uncount(LengthFrequency)%>%
  group_by(SampleDate,StationCode,OrganismCode)%>%
  mutate_at("ForkLength",~ifelse(is.na(.), mean(., na.rm = TRUE),.))%>%
  filter(is.nan(ForkLength)==T)

table(FMWT_Tidy_Counts$CommonName)

FMWT_Tidy_Long %>% filter(ForkLength <300 & ForkLength > 0) %>% ggplot(aes(x=ForkLength))+geom_density()+facet_grid(.~Species)

                           