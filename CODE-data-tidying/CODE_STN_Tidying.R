#==============================================================================
#========Code for integrating Summer Townet database files to Tidy Format======
#==============================================================================
#Prepared by Michael Tillotson, ICF
#Created February 3, 2021
#==============================================================================
#Load necessary packages
library(tidyverse)
library(lubridate)

#Set working directory, adjust as needed
setwd("C:/Users/40545/Documents/GitHub/pelagicsurveys")

#Read database files and assign column types if needed
Sample <- read_csv("RawData/STN/Sample.csv",
                   col_types=c("dDffdddddddddcdffdffddddddddddddlllllll"))

TowEffort <- read_csv("RawData/STN/TowEffort.csv",col_types = "dddccddddcl")

Catch <- read_csv("RawData/STN/Catch.csv")

Length <- read_csv("RawData/STN/Length.csv")

Station <- read_csv("RawData/STN/luStation.csv",col_types = "dflddccddddddcffcfddddddddd")%>%
  rename("StationCode"="StationCodeSTN")


#Lookup tables

luMicrocystis <- read_csv("RawData/STN/luMicrocystis.csv",col_types = "fc")

luOrganism <- read_csv("RawData/STN/luOrganism.csv")

luTide <- read_csv("RawData/STN/luTide.csv")

luTowDirection <- read_csv("RawData/STN/luTowDirection.csv")

luIndexWeights <- read_csv("RawData/STN/IndexWeights.csv",col_types = "fffdd")

luWaves <- read_csv("RawData/STN/luWaves.csv",col_types = "ff")

luWeather <- read_csv("RawData/STN/luWeather.csv",col_types = "ff")


STN_Tidy_All <- Sample %>% 
  full_join(TowEffort,by="SampleRowID")%>%
  full_join(Catch,by="TowRowID")%>%
  full_join(Length, by = "CatchRowID")%>%
  rename("TowDirectionID"="TowDirection")%>%#========Rename TowDirection======
  left_join(luTowDirection,by="TowDirectionID")%>%
  select(-TowDirectionID)%>%
  rename("TideRowID"="TideCode")%>%#========Rename Tide Stage======
  left_join(luTide,by="TideRowID")%>%
  select(-TideRowID)%>%
  rename("MicrocystisID"="Microcystis")%>%#========Rename Microcystis======
  left_join(luMicrocystis,by="MicrocystisID")%>%
  select(-MicrocystisID)%>%
  rename("OrganismCodeSTN"="OrganismCode")%>%#========Add Taxonomy======
  left_join(luOrganism,by="OrganismCodeSTN",na_matches="never")%>%
  rename("WeatherCode" ="Weather")%>%#========Rename Weather======
  left_join(luWeather,by="WeatherCode")%>%
  select(-WeatherCode)%>%
  rename("WaveCode" = "Waves")%>%#========Rename Waves======
  left_join(luWaves,by="WaveCode")%>%
  select(-WaveCode)%>%
  add_column(SurveySeason="STN",.before = "SampleRowID")%>%
  left_join(luIndexWeights,by="StationCode")%>%
  mutate(Year=year(SampleDate),
         Month=month(SampleDate),
         JulianDay = yday(SampleDate),
         Survey_Station = paste(SurveySeason,StationCode,sep="_"),
         .after="SampleDate")%>%
  left_join(Station,by="StationCode")%>%
  mutate(Station_Longitude=-(LonD+LonM/60+LonS/3600),Station_Latitude=LatD+LatM/60+LatS/3600)%>%
  select(-c(LatD:LonS)) %>%
  mutate(longitude_STN_2019=-(LonD_2019+LonM_2019/60+LonS_2019/3600),
         latitude_STN_2019=LatD_2019+LatM_2019/60+LatS_2019/3600)%>%
  mutate(Start_Longitude=-(StartLongDegrees+StartLongMinutes/60+StartLongSeconds/3600),
         Start_Latitude=-(StartLatDegrees+StartLatMinutes/60+StartLatSeconds/3600),
         End_Longitude=-(EndLongDegrees+EndLongMinutes/60+EndLongSeconds/3600),
         End_Latitude=-(EndLatDegrees+EndLatMinutes/60+EndLatSeconds/3600))%>%
  select(-c(StartLatDegrees:EndLongSeconds))%>%
  select(-c(LatD_2019:LonS_2019,SampleRowID))%>%
  rename("StationActive" = "Active.x",
         "SpeciesActive" = "Active.y")%>%
  mutate(ZeroLength=if_else(ForkLength==0,TRUE,FALSE))%>%
  relocate(NetErrors:FieldsheetCorrected,.after=ZeroLength)%>%
  relocate(TowDirection:Microcystis,.after=MeterDifference)%>%
  relocate(Weather:Waves,.after=Microcystis)%>%
  relocate(Start_Longitude:End_Latitude,.after=Waves)%>%
  relocate(Phylum:Species,.after = OrganismCodeSTN)%>%
  relocate(Station_Longitude:Station_Latitude,.after = StationCode)%>%
  relocate(OrganismCodeMWT,.after=End_Latitude)%>%
  relocate(OrganismCodeSTN,.after=OrganismCodeMaster)%>%
  relocate(ZeroLength,.after = ForkLength)%>%
  relocate(Area,.after = OrderNum)%>%
  relocate(WeightingFactor, .after = Area)%>%
  relocate(CommonName, .after = WeightingFactor)%>%
  relocate(StationActive, .after = CommonName)%>%
  relocate(LengthFrequency, .after = StationActive)%>%
  rename("SurveyNumber"="Survey",
         "Turbidity" = "TurbidityTop",
         "OrganismCodeFMWT" = "OrganismCodeMWT")%>%
  add_column(TowDuration=NA,.after = "TowNumber")%>%
  
  #Create CommonName for No Catch Tows and correct Catch, LengthFrequency columns
  mutate(CommonName = if_else(is.na(Catch)==T,"No Catch",CommonName),
         Catch = if_else(is.na(Catch)==T,1,Catch),
         LengthFrequency = if_else(is.na(Catch)==T,1,LengthFrequency),
         CommonName = as.factor(CommonName))%>%
  
  mutate_at(vars(Survey_Station,MarkCode,Dead,CommonName),as.factor)%>%
  mutate_at(vars(TimeStart:TimeStop,TowDuration),as.numeric)%>%
  relocate(SampleComments,.after=Comments)%>%
  relocate(TowComments,.after=Comments)


STN_Tidy <-  STN_Tidy_All%>%
  select(-c(Dead,MarkCode,CatchRowID,LengthRowID,StationRowID,OrganismRowID,UserName,NetErrors:FieldsheetCorrected,
            TowRowID,MeterSerial,MeterEstimate,OrganismCodeMaster:Region,
            RKI:`TNS Diet Area`,STRArea:FieldsheetCorrected))%>%
  rename(Comment1 = Comments,
         Comment2 = TowComments,
         Comment3 = SampleComments)





save(STN_Tidy,file="TidyData/Individual Surveys/DATA_STN_Tidy.rda")




table(STN_FMWT_FLAdjusted$Dead)
