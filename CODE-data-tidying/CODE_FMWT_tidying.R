#==============================================================================
#=====Code for integrating Fall Midwater Trawl database files to Tidy Format===
#==============================================================================
#Prepared by Michael Tillotson, ICF
#Created February 17, 2021
#==============================================================================
#Load necessary packages
library(tidyverse)
library(lubridate)

#Set working directory, adjust as needed


setwd("C:/Users/40545/ICF/IEP Fish Survey Evaluation - General/Survey Evaluation and Design - Shared/Public/1. Data")

#Read database files and assign column types if needed
glimpse(FMWT_Tidy_All)
Sample <- read_csv("Raw Data/FMWT/CSVs/Sample.csv",
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


Catch <- read_csv("Raw Data/FMWT/CSVs/Catch.csv",col_types = "ddddD")%>%
  filter(Catch>0)

Length <- read_csv("Raw Data/FMWT/CSVs/Length.csv",col_types="dddffdfD")

Station <- read_csv("Raw Data/FMWT/CSVs/StationsLookUp.csv")%>%
  rename(longitude_FMWT = DD_Longitude, latitude_FMWT = DD_Latitude)
#Lookup tables
luMicrocystis <- read_csv("Raw Data/STN/CSVs/luMicrocystis.csv",col_types = "fc")

luOrganism <- read_csv("Raw Data/FMWT/CSVs/OrganismsLookup.csv")

luTide <- read_csv("Raw Data/STN/CSVs/luTide.csv")

luTowDirection <- read_csv("Raw Data/STN/CSVs/luTowDirection.csv")

luAreaWeights <- read_csv("Raw Data/FMWT/CSVs/StationArea.csv",
                        col_types = "dfdddddd")%>%rename("AREA"="Area")%>%
                          left_join(read_csv("Raw Data/FMWT/CSVs/AreaWT.csv"),by="AREA")%>%
  rename("Area"="AREA","StationCode"="Station")


#Join all tibbles and replace numeric codes with descriptive factors.
#Removes all zooplankton tows
FMWT_Tidy_All <-Sample %>% 
  right_join(Catch,by="SampleRowID")%>%
  right_join(Length,by="CatchRowID")%>%
  left_join(Station,Station,by="StationCode")%>%
  left_join(luAreaWeights,by="StationCode")%>%
  rename("TowDirectionID"="TowDirectionCode")%>%  #========Rename TowDirection======
left_join(luTowDirection,by="TowDirectionID")%>%
  select(-TowDirectionID)%>%
  rename("TideRowID"="TideCode")%>%#========Rename Tide Stage======
left_join(luTide,by="TideRowID")%>%
  select(-TideRowID)%>%
  rename("MicrocystisID"="Microcystis")%>%#========Rename Microcystis======
left_join(luMicrocystis,by="MicrocystisID")%>%
  select(-MicrocystisID)%>%
#========Add Taxonomy======
left_join(luOrganism,by="OrganismCode")%>%
  add_column(SurveySeason="FMWT",.after = "SampleRowID")%>%
  mutate(Year=year(SampleDate),
         Month=month(SampleDate),
         JulianDay = yday(SampleDate),
         Survey_Station = paste(SurveySeason,StationCode,sep="_"),
         .after="SampleDate")%>% filter(is.na(LengthFrequency)==F)


SuperLong <- FMWT_Tidy_All %>% uncount(LengthFrequency)

SuperLong %>% filter(ForkLength <300 & ForkLength > 0) %>% ggplot(aes(x=ForkLength))+geom_density()

                           