#==============================================================================
#=====Code for integrating Smelt Larval Survey database files to Tidy Format===
#==============================================================================
#Prepared by Michael Tillotson, ICF
#Created March 11, 2021
#==============================================================================
require(tidyverse)
require(lubridate)


#Set working directory, adjust as needed
setwd("C:/Users/40545/Documents/GitHub/pelagicsurveys")

#Read Survey data
Survey <- read_csv("RawData/20mm/Survey.csv",col_types = "dDdc")%>%
  rename(Comment1 = Comments)

#Read station data and read/join with station lookup information
Station <- read_csv("RawData/20mm/Station.csv",col_types = "ddddddddddddddc")%>%
  left_join(read_csv("RawData/20mm/20mmStations.csv",col_types="dddddddfcfc"),
            by="Station")%>%
  mutate(Start_Longitude=-(LonDeg+LonMin/60+LonSec/3600),Start_Latitude=LatDeg+LatMin/60+LatSec/3600)%>%
  select(-c(LatDeg:LonSec)) %>%
  mutate(Station_Longitude=-(LonD+LonM/60+LonS/3600),Station_Latitude=LatD+LatM/60+LatS/3600)%>%
  select(-c(LatD:LonS))%>%mutate(Station = as.factor(Station))

#Read tow data
Tow <- read_csv("RawData/20mm/Tow.csv",col_types = "ddddfddd")

#Read gear data
Gear <- read_csv("RawData/20mm/Gear.csv",col_types="dddddddc")%>%
  left_join(read_csv("RawData/20mm/GearCodesLkp.csv"),by="GearCode")%>%
  select(-GearCode)%>%rename(Comment2 = Comments)

#Read tide lookup data
luTide <- read_csv("RawData/SLS/luTide.csv",col_types = "ff")

#Load and join fish FishSample, FishLength and FishCode data
FishSample <- read_csv("RawData/20mm/FishSample.csv")%>%
  full_join(read_csv("RawData/20mm/FishLength.csv",col_types="dddlldd"),by="FishSampleID")%>%
  left_join(read_csv("RawData/20mm/FishCodes.csv"),by="FishCode")%>%
  rename(CommonName = `Common Name`)%>%
  mutate(Dead = !ReleasedAlive)%>%
  select(-c(FishSampleID,FieldRace:FinalRace,
            `TNS Field`,Symbol,`MWT Field`,FishLengthID,
            ReleasedAlive))

#Join all sheets based on database structure
Tidy_20mm_all <- Survey %>% 
  full_join(Station, by ="SurveyID")%>%
  full_join(Tow,by="StationID")%>%
  full_join(Gear,"TowID")%>%
  rename("TideRowID"="Tide")%>%#========Rename Tide Stage======
 left_join(luTide,by="TideRowID")%>%#=Replace tide code with factor text
  select(-TideRowID)%>%
  full_join(FishSample,"GearID")%>%
  add_column(SurveySeason="20mm",.before = "SampleDate")%>%
  rename(StationCode = Station,TemperatureTop = Temp,
         ConductivityTop = TopEC, ConductivityBottom = BottomEC,
         DepthBottom = BottomDepth, MeterIn = MeterStart,TimeStart = TowTime,
         MeterOut = MeterEnd, ForkLength = Length, MarkCode = AdFinPresent,
         OrganismCodeFMWT = Species_CodeMWT, SurveyNumber = Survey,
         Comment3 = Comments, TowNumber = TowNum, TowDuration = Duration)%>%
  mutate(Year=year(SampleDate),
         Month=month(SampleDate),
         JulianDay = yday(SampleDate),
         Survey_Station = paste(SurveySeason,StationCode,sep="_"),
         .after="SampleDate")%>%
  select(-c(SurveyID))%>%filter(Gear=="Net")%>%
  #Create CommonName for No Catch Tows and correct Catch, LengthFrequency columns
  mutate(CommonName = if_else(is.na(Catch)==T,"No Catch",as.character(CommonName)),
         Catch = if_else(is.na(Catch)==T,1,Catch),
         CommonName = as.factor(CommonName))%>%
#Remove duplicate length rows for species-date-catch-Tow combinations
distinct(across(c(SampleDate, StationCode, CommonName, ForkLength,TowNumber)),.keep_all = T)%>%
  #==================Deal with unmeasured fish==============================
group_by(SampleDate, StationCode, CommonName)%>%
  add_tally(name="TotalMeasured")%>%
  group_by(SampleDate, StationCode, CommonName, ForkLength)%>%
  add_tally(name="LengthFrequency")%>%
  mutate(LengthFrequency_Adjusted = round(Catch*(LengthFrequency/TotalMeasured),0))%>%
  uncount(LengthFrequency_Adjusted)%>%select(-c(LengthFrequency,TotalMeasured))


  

Tidy_20mm <- Tidy_20mm_all %>% 
  select(-c(StationID,TowID,Gear,
            GearDescription,Order,
            SampleCode,FishCode,RKI,
            MeterCheck,Active,GearID,Notes
            ))%>%
  mutate(across(c("MarkCode","Dead"), as.factor))%>% 
  rename("Area" = "AreaCode")

save(Tidy_20mm,file="TidyData/Individual Surveys/DATA_20mm_Tidy.rda")




