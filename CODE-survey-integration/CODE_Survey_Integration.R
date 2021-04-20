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

load("TidyData/Individual Surveys/DATA_STN_Tidy.rda")
load("TidyData/Individual Surveys/DATA_FMWT_Tidy.rda")
load("TidyData/Individual Surveys/DATA_SKT_Tidy.rda")
load("TidyData/Individual Surveys/DATA_SLS_Tidy.rda")
load("TidyData/Individual Surveys/DATA_20mm_Tidy.rda")



STN_FMWT <- FMWT_Tidy%>%add_row(STN_Tidy)%>%
  filter(is.na(StationCode)==F)%>%
  select(-c(Phylum,Class,Order))%>%
  mutate(Station_Origin = SurveySeason)%>%
  add_column(ReproductiveStage=NA,Sex=NA)%>%
  filter(LengthFrequency>0|is.na(LengthFrequency)==T)%>%
  rowid_to_column("ID")%>%add_column(MeterSerial = NA)


#==========Adjust length-frequencies for unmeasured fish===================
MeasuredCounts <- STN_FMWT %>% 
  mutate(ZeroLength = if_else(CommonName=="No Catch",FALSE,ZeroLength))%>%
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
  mutate(LengthFrequency_Adjusted = if_else(CommonName=="No Catch",1,LengthFrequency_Adjusted))%>%
  filter((ZeroLength==T&TotalMeasured==0|ZeroLength==F|CommonName=="No Catch"))%>%
  filter(is.na(LengthFrequency_Adjusted)==F)%>%
  uncount(LengthFrequency_Adjusted)%>%select(-c(TotalCount:TotalMeasured))


CDFW_Surveys_Long <- STN_FMWT_FLAdjusted%>%
  add_row(SKT_Tidy)%>%
  add_row(SLS_Tidy)%>%
  add_row(Tidy_20mm)%>%
  select(-ID)%>%
  mutate(across(c("CommonName","TowDirection"), as.factor))%>% 
  mutate(CommonName = as.character(CommonName),
         CommonName = str_to_title(CommonName),
         CommonName = as.factor(CommonName))



save(CDFW_Surveys_Long,
     file="TidyData/DATA_CDFW_Surveys_Tidy.rda")




  