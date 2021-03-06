#==============================================================================
#========Code for integrating Spring Kodiak database files to Tidy Format======
#==============================================================================
#Prepared by Michael Tillotson, ICF
#Created March 9, 2021
#==============================================================================
select <- dplyr::select

#Set working directory, adjust as needed
setwd("C:/Users/40545/Documents/GitHub/pelagicsurveys")


Sample <- read_csv("RawData/SKT/tblSample.csv",
                   col_types=c("dDdfdddddddddffdccc"))%>%
  rename(MeterSerial = MeterNumber)%>%
  #Combine replicated tows
  distinct(across(c("StationCode","SampleDate")),.keep_all=T)





Catch <- read_csv("RawData/SKT/tblCatch.csv",
                  col_types = "ddfdc")%>%
  filter(Catch>0)

Length <- read_csv("RawData/SKT/tblFishInfo.csv",col_types = "ddffddfffff")%>%
  filter(ForkLength>0)

Station <- read_csv("RawData/SKT/lktblStationsSKT.csv",
                    col_types = "dfdddddddcc")%>%
  mutate(Station_Longitude=-(LongDec+LongMin/60+LongSec/3600),
         Station_Latitude=LatDeg+LatMin/60+LatSec/3600)%>%
  select(-c(LatDeg:LongSec))%>%
  mutate(Location=as.factor(Location))

luOrganism <- read_csv("RawData/SKT/tblOrganismCodes.csv",
                       col_types = "ffcccffc")

luTowDirection <- read_csv("RawData/SKT/luTowDirection.csv",col_types = "ff")

luTide <- read_csv("RawData/SKT/luTide.csv",col_types = "ff")

luSex <- read_csv("RawData/SKT/tblSexLookUp.csv",col_types = "ff")


SKT_Tidy_All <- Sample %>% 
  rename("TowDirectionID"="TowDirectionCode")%>%  #========Rename TowDirection======
  left_join(luTowDirection,by="TowDirectionID")%>%
  select(-TowDirectionID)%>%
  rename("TideRowID"="TideCode")%>%#========Rename Tide Stage======
  left_join(luTide,by="TideRowID")%>%
  select(-TideRowID)%>%
  #Join with Catch Table
  full_join(Catch,by="SampleRowID")%>%
  left_join(luOrganism,by="OrganismCode")%>%
  #Join with Fish Info Table
  full_join(Length,"CatchRowID")%>%
  rename(Station = StationCode)%>%
  left_join(Station, by="Station")%>%
  rename(CODE = Sex)%>%
  left_join(luSex,by="CODE")%>%
  rename(Sex = Description)%>%
  select(-c("ReproductiveStage","2nd Stage",LengthRowID,CatchRowID,FishID1,FishID2,SampleRowID,Sex))%>%
  rename(Station_Origin = `Station Origin`,
         StationCode = Station,
         TemperatureTop = WaterTemperature,
         TimeStart = SampleTimeStart,
         TimeStop = SampleTimeEnd,
         MeterIn = MeterStart,
         MeterOut = MeterEnd,
         Dead = ReleasedAlive,
         MarkCode = AdFinPresent,
         Start_Longitude = Longitude,
         Start_Latitude = Latitude
         )%>%
  add_column(SurveySeason="SKT",.before = "SampleDate",)%>%
  mutate(Year=year(SampleDate),
         Month=month(SampleDate),
         JulianDay = yday(SampleDate),
         Survey_Station = paste(SurveySeason,StationCode,sep="_"),)%>%
  relocate(Year:JulianDay, .after=SampleDate)%>%
  relocate(Station_Longitude:Station_Latitude, .after=StationCode)%>%
  relocate(SurveyNumber, .after=Station_Latitude)%>%
  relocate(Survey_Station, .after=JulianDay)%>%
  add_column(TowNumber=1,.after = "SurveyNumber")%>%
  #Create CommonName for No Catch Tows and correct Catch, LengthFrequency columns
  ungroup()%>%
  group_by(StationCode,SampleDate)%>%
  mutate(sumCatch = sum(Catch,na.rm=T))%>%
  mutate(CommonName = if_else(sumCatch==0,"No Catch",as.character(CommonName)),
         Catch = if_else(CommonName=="No Catch",1,Catch),
         CommonName = as.factor(CommonName))%>%
  #Remove lingering NA catch rows. Not sure where they're coming from, but it's only a handful of tows
  filter(is.na(Catch)==F)%>%
  ungroup()%>%select(-sumCatch)%>%
  rename(Comment1 = SampleComments,
         Comment2 = CatchComments,
         Comment3 = Comments)%>%
  #Remove duplicate length rows for species-date-catch combinations
  #==================Deal with unmeasured fish==============================
  group_by(SampleDate, StationCode, CommonName)%>%
  add_tally(name="TotalMeasured")%>%
  group_by(SampleDate, StationCode, CommonName, ForkLength)%>%
  add_tally(name="LengthFrequency")%>%
  mutate(LengthFrequency_Adjusted = round(Catch*(LengthFrequency/TotalMeasured),0))%>%
  ungroup()%>%
  uncount(LengthFrequency_Adjusted)%>%select(-c(LengthFrequency,TotalMeasured))





SKT_Tidy <- SKT_Tidy_All %>%   select(-c(CODE,
                                         `Sort Order`,AlternateName,Depth,
                                         OrganismCode,NameInSAS,Volume,Start_Longitude,Start_Latitude))

save(SKT_Tidy,file="TidyData/Individual Surveys/DATA_SKT_Tidy.rda")

  

