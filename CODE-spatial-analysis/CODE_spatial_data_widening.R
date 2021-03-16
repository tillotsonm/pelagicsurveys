#====================================================================
#===Code to generate a consolidated .csv file for spatial analysis===
#====================================================================
#Prepared by Michael Tillotson
#ICF
#Created March 15, 2021

setwd("C:/Users/40545/Documents/GitHub/pelagicsurveys")

load("TidyData/DATA_All_Surveys_Tidy.rda")

#Remove longer form tibbles
rm(All_Surveys_LF,All_Surveys_Long)



#===Removing replicated tows by summing for catch and calulating a weighted
#mean for average length

Working <-  All_Surveys_Species %>%
  group_by(SampleDate,StationCode,CommonName)%>%
  mutate(RawCatch =sum(RawCatch,na.rm=T),
         Mean_Length = weighted.mean(Mean_Length,RawCatch,na.rm = T))%>%
  ungroup()%>%filter(TowNumber==1)%>%
  mutate(CommonName = recode(CommonName,"Age-0 Striped Bass" = "Striped Bass Age-0",
                             "Age 0-Striped Bass" = "Striped Bass Age 0",
                             "Age 1-Striped Bass" = "Striped Bass Age 1",
                             "Age 2-Striped Bass" = "Striped Bass Adult",
                             "Striped Bass Age-2" = "Striped Bass Adult",
                             "Striped Bass Age-3+" = "Striped Bass Adult",
                             "Striped Bass Age-0" = "Striped Bass Age 0",
                             "Striped Bass Age-1" = "Striped Bass Age 1",
                             "Striped Bass Age 2" = "Striped Bass Adult",
                             "Age-0 Striped Bass" = "Striped Bass Age 0",
                             "Age-1 Striped Bass" = "Striped Bass Age 1",
                             "Age-2 Striped Bass" = "Striped Bass Adult",
                             "Striped Bass" = "Striped Bass Age-0"))%>%
  select(c(1:21,ForkLength,WeightingFactor,CommonName,Mean_Length,RawCatch))%>%
  group_by(Survey_Station)%>%
  mutate(SpeciesAllTime = length(unique(CommonName)))%>%
  ungroup()%>%group_by(Survey_Station,Year)%>%
  mutate(SpeciesperYear = length(unique(CommonName)))%>%
  ungroup()%>%group_by(Survey_Station)%>%
  mutate(MeanSpeciesperYear = mean(SpeciesperYear))%>%
  select(-c(TowNumber,TemperatureBottom,SurveyNumber,JulianDay))

#Set a threshold for how common a species should be to remain in the dataset
#Currenlty using 0.1% representation of all-time catch as cutoff
SpecRanks <- Working %>% group_by(CommonName)%>%
  summarise(AllTime=round(sum(RawCatch)/sum(Working$RawCatch),3))%>%
  arrange(-AllTime)%>%filter(AllTime>0)


Wide_Data_All <- Working %>% ungroup()%>%rename(Length = Mean_Length, Catch = RawCatch)%>%
  filter(CommonName %in% SpecRanks$CommonName) %>%
  mutate(CommonName = str_replace_all(CommonName, " ", "_"))%>%
  pivot_wider(names_from = CommonName,names_sep = "_",values_from = c(Catch,Length))%>%
  select(-c(ForkLength,WindDirection))%>%filter(is.na(Station_Latitude)==FALSE)%>%
  mutate_at(vars(Catch_Shiner_Perch:Catch_Inland_Silverside), ~replace_na(., 0))%>%
  mutate_at(vars(Length_Shiner_Perch:Length_Inland_Silverside), ~replace_na(., NA))
  


Wide_Data_2000 <- Wide_Data_All %>% filter(Year>1999)


save(Wide_Data_2000,Wide_Data_All,file="TidyData/DATA_Wide_Spatial_Analysis.rda")

write_csv(Wide_Data_All,file="all-years-wide-format.csv")
write_csv(Wide_Data_All,file="since-2000-wide-format.csv")
