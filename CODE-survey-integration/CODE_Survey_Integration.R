#===========Code to integrate survey tables, adjust length frequencies
#===========for unmeasured individuals and calculate CPUE============
#Prepared by Michael Tillotson
#ICF
#Updated March 11, 2021
#Currently incomplete. Integrates SKT, STN and FMWT, but does not include
#20mm or SLS and does not have CPUE calculated.
require(tidyverse)
require(stringr)

setwd("C:/Users/40545/Documents/GitHub/pelagicsurveys")

load("TidyData/DATA_STN_Tidy.rda")
load("TidyData/DATA_FMWT_Tidy.rda")
load("TidyData/DATA_SKT_Tidy.rda")
load("TidyData/DATA_SLS_Tidy.rda")
load("TidyData/DATA_20mm_Tidy.rda")


STN_FMWT <- FMWT_Tidy%>%add_row(STN_Tidy)%>%
  filter(is.na(StationCode)==F)%>%
  select(-c(Phylum,Class,Order))%>%
  mutate(Station_Origin = SurveySeason)%>%
  add_column(ReproductiveStage=NA,Sex=NA)%>%
  filter(LengthFrequency>0)%>%
  rowid_to_column("ID")%>%add_column(MeterSerial = NA)


#==========Adjust length-frequencies for unmeasured fish===================
MeasuredCounts <- STN_FMWT %>% 
  group_by(SampleDate,Survey_Station,TowNumber,CommonName)%>%
  mutate(TotalCount = sum(LengthFrequency))%>%
  mutate(TotalMeasured = sum(LengthFrequency[ZeroLength==FALSE]))%>%
  ungroup()%>%
  select(TotalCount,TotalMeasured,ID)


STN_FMWT_FLAdjusted <- STN_FMWT %>% 
  left_join(MeasuredCounts, by = c("ID"))%>%
  mutate(LengthFrequency_Adjusted = round(TotalCount*(LengthFrequency/TotalMeasured),0))%>%
  mutate(LengthFrequency_Adjusted = if_else(is.infinite(LengthFrequency_Adjusted),
                                            LengthFrequency,LengthFrequency_Adjusted))%>%
  filter((ZeroLength==T&TotalMeasured==0|ZeroLength==F))%>%
  #Might want to deal with these 17 NAs later on
  filter(is.na(LengthFrequency_Adjusted)==F)%>%
  uncount(LengthFrequency_Adjusted)%>%select(-c(TotalCount:TotalMeasured))


All_Surveys_Long <- STN_FMWT_FLAdjusted%>%
  add_row(SKT_Tidy)%>%
  add_row(SLS_Tidy)%>%
  add_row(Tidy_20mm)%>%
  select(-ID)%>%
  mutate(across(c("CommonName","TowDirection"), as.factor))%>% 
  mutate(CommonName = as.character(CommonName),
         CommonName = str_to_title(CommonName),
         CommonName = as.factor(CommonName))

All_Surveys_LF <- All_Surveys_Long %>% 
  group_by(Survey_Station,SampleDate,TowNumber,CommonName,ForkLength)%>%
  mutate(LengthFrequency = n())%>%ungroup()%>%
  group_by(Survey_Station,SampleDate,TowNumber,CommonName)%>%
  mutate(RawCatch=n())%>%ungroup()%>%
  distinct(across(c(Survey_Station,SampleDate,TowNumber,CommonName,ForkLength)),.keep_all = TRUE)%>%
  mutate(ForkLength = na_if(ForkLength,0))%>%select(-Catch)

All_Surveys_Species <- All_Surveys_LF %>%
  group_by(Survey_Station,SampleDate,TowNumber,CommonName,ForkLength)%>%
  mutate(Mean_Length = mean(ForkLength,na.rm = T))%>%ungroup%>%
  distinct(across(c(Survey_Station,SampleDate,TowNumber,CommonName)),.keep_all = TRUE)%>%
  select(-LengthFrequency)


All_2000_Long <- All_Surveys_Long %>% filter(Year > 1999)

All_2000_LF <- All_Surveys_LF %>% filter(Year > 1999)

All_2000_Species <- All_Surveys_Species %>% filter(Year > 1999)



save(All_Surveys_Long,All_Surveys_LF,All_Surveys_Species,
     file="TidyData/All_Surveys_Tidy.rda")
save(All_2000_Long,All_2000_LF,All_2000_Species,
     file="TidyData/All_2000_Tidy.rda")


levels(All_2000_Long$CommonName)
  