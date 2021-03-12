#==============================================================================
#=====Code for integrating Smelt Larval Survey database files to Tidy Format===
#==============================================================================
#Prepared by Michael Tillotson, ICF
#Created March 11, 2021
#==============================================================================

#Set working directory, adjust as needed
setwd("C:/Users/40545/Documents/GitHub/pelagicsurveys")


Survey <- read_csv("RawData/20mm/Survey.csv",col_types = "dDdc")%>%
  rename(Comment1 = Comments)
Station <- read_csv("RawData/20mm/Station.csv",col_types = "ddfdddddddddddc")
Tow <- read_csv("RawData/20mm/Tow.csv",col_types = "ddddfddd")
Gear <- read_csv("RawData/20mm/Gear.csv",col_types="dddddddc")%>%
  left_join(read_csv("RawData/20mm/GearCodesLkp.csv"),by="GearCode")%>%
  select(-GearCode)%>%rename(Comment2 = Comments)
luTide <- read_csv("RawData/SLS/luTide.csv",col_types = "ff")

FishSample <- read_csv("RawData/20mm/FishSample.csv")%>%
  right_join(read_csv("RawData/20mm/FishLength.csv",col_types="dddlldd"),by="FishSampleID")%>%
  left_join(read_csv("RawData/20mm/FishCodes.csv"),by="FishCode")%>%
  rename(CommonName = `Common Name`)%>%
  mutate(Dead = !ReleasedAlive)%>%
  select(-c(FishSampleID,FieldRace:FinalRace,
            `TNS Field`,Symbol,`MWT Field`,FishLengthID,
            ReleasedAlive))

Tidy_20mm_all <- Survey %>% 
  right_join(Station, by ="SurveyID")%>%
  right_join(Tow,by="StationID")%>%
  right_join(Gear,"TowID")%>%
  rename("TideRowID"="Tide")%>%#========Rename Tide Stage======
 left_join(luTide,by="TideRowID")%>%#=Replace tide code with factor text
  select(-TideRowID)%>%
  right_join(FishSample,"GearID")%>%
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
  mutate(Station_Longitude=-(LonDeg+LonMin/60+LonSec/3600),Station_Latitude=LatDeg+LatMin/60+LatSec/3600,
         MarkCode = as.factor(MarkCode),Dead = as.factor(Dead))%>%
  select(-c(LatDeg:LonSec)) %>%
  select(-c(SurveyID))%>%filter(Gear=="Net")
  

Tidy_20mm <- Tidy_20mm_all %>% 
  select(-c(StationID,TowID,Gear,
            GearDescription,Order,
            SampleCode,FishCode,
            MeterCheck,Active,GearID))%>%
  add_column(LengthFrequency=1)

save(Tidy_20mm,file="TidyData/DATA_20mm_Tidy.rda")




