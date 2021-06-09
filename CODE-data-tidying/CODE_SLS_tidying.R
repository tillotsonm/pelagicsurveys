#==============================================================================
#=====Code for integrating Smelt Larval Survey database files to Tidy Format===
#==============================================================================
#Prepared by Michael Tillotson, ICF
#Created March 11, 2021
#==============================================================================

#Set working directory, adjust as needed
setwd("C:/Users/40545/Documents/GitHub/pelagicsurveys")


Sample <- read_csv("RawData/SLS/Tow Info.csv",
                   col_types = "Dfddfdddddddddddc",
                   col_names = c("SampleDate",
                                 "StationCode",
                                 "TowNumber",
                                 "TimeStart",
                                 "Tide",
                                 "DepthBottom",
                                 "CableOut",
                                 "TowDuration",
                                 "NetMeterSerial",
                                 "MeterIn",
                                 "MeterOut",
                                 "NetMeterCheck",
                                 "CBMeterSerial",
                                 "CBMeterStart",
                                 "CBMeterEnd",
                                 "CBMeterCheck",
                                 "Comment1"))[-1,]%>%
  select(-c(CBMeterSerial:CBMeterCheck))%>%
  add_column(TowDirection=NA)

Water <-read_csv("RawData/SLS/Water Info.csv",
                 col_types = "dDfdddddccc")%>%
  rename(SampleDate = Date, StationCode = Station, SurveyNumber = Survey, Comment2 = Comments)


Catch <- read_csv("RawData/SLS/Catch.csv",
                  col_types = "Dfdfdddd")%>%
  select(-c(6:7))%>%
  rename(StationCode = Station, TowNumber=Tow, SampleDate = Date)

Length <- read_csv("RawData/SLS/Lengths.csv",col_types = "Dfdfddl")%>%
  rename(StationCode=Station, TowNumber=Tow, SampleDate = Date)%>%
  select(-entryorder)

Station <- read_csv("RawData/SLS/20mm Stations.csv",col_types = "fddddddfffc")%>%
  mutate(Station_Longitude=-(LonD+LonM/60+LonS/3600),
         Station_Latitude=LatD+LatM/60+LatS/3600)%>%
  select(-c(LatD:LonS))%>%
  rename(StationCode = Station)
  

luOrganism <- read_csv("RawData/SLS/FishCodes.csv",col_types = "fcccfffff")%>%
  rename(FishCode = `Fish Code`)


luTide <- read_csv("RawData/SLS/luTide.csv",col_types = "ff")

luWeightingFactors <-read_csv("RawData/SLS/Wt_factors.csv",col_types = "dfdc")%>%
  rename(StationCode = Station,Comment3 = Comments)%>%select(-1)

SLS_Tidy_All <- Sample %>%  
  left_join(Water,by=c("SampleDate","StationCode"))%>%#Join with Water tibble
  relocate(SurveyNumber, .before = TowNumber)%>%#====Relocate Survey Number towards front
  rename("TideRowID"="Tide")%>%#========Rename Tide Stage======
  left_join(luTide,by="TideRowID")%>%#=Replace tide code with factor text
  select(-TideRowID)%>%
  full_join(Catch,by=c("SampleDate","StationCode","TowNumber"))%>%#Join with Catch Table
  left_join(luOrganism,by="FishCode")%>%
  full_join(Length,by=c("SampleDate","StationCode","TowNumber","FishCode"))%>%
  left_join(Station, by="StationCode")%>%
  left_join(luWeightingFactors,by="StationCode")%>%
  rename(ForkLength = Length,WeightingFactor=`Wt Factor`,
         CommonName = `Common Name`,TemperatureTop = Temp,
         ConductivityTop = TopEC, ConductivityBottom = BottomEC,
         OrganismCode = `MWT Species Code`,MeterSerial = NetMeterSerial,
         Area = AreaCode)%>%
  mutate(SurveySeason = "SLS",
         Year=year(SampleDate),
         Month=month(SampleDate),
         JulianDay = yday(SampleDate),
         Survey_Station = paste(SurveySeason,StationCode,sep="_"),
         .after="SampleDate")%>%
#Create CommonName for No Catch Tows and correct Catch, LengthFrequency columns
mutate(CommonName = if_else(is.na(Catch)==T,"No Catch",as.character(CommonName)),
         Catch = if_else(is.na(Catch)==T,1,Catch),
         CommonName = as.factor(CommonName))%>%
  #==================Deal with unmeasured fish==============================
#Remove duplicate length rows for species-date-catch combinations
distinct(across(c(SampleDate, StationCode, CommonName, ForkLength)),.keep_all = T)%>%
group_by(SampleDate, StationCode, CommonName)%>%
  add_tally(name="TotalMeasured")%>%
  group_by(SampleDate, StationCode, CommonName, ForkLength)%>%
  add_tally(name="LengthFrequency")%>%
  mutate(LengthFrequency_Adjusted = max(1,round(Catch*(LengthFrequency/TotalMeasured),0)))%>%
  filter(is.na(LengthFrequency_Adjusted)==F & is.na(Catch)==F)%>%
  uncount(LengthFrequency_Adjusted)%>%select(-c(LengthFrequency,TotalMeasured))



SLS_Tidy <- SLS_Tidy_All %>%   select(-c(Lat,Long,`TNS Field`:`MWT Field`,
                                         NetMeterCheck,CatchID,
                                         Symbol,YolkSacorOilPresent,Notes,RKI,FishCode))

#For Checking variable compatibility with other data tables
#names(SLS_Tidy)[which(is.na(match(names(SLS_Tidy),
#                                  names(Primary_2000_Long))))]

save(SLS_Tidy,file="TidyData/Individual Surveys/DATA_SLS_Tidy.rda")
  


