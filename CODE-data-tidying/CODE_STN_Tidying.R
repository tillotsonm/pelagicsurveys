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


setwd("C:/Users/40545/ICF/IEP Fish Survey Evaluation - General/Survey Evaluation and Design - Shared/Public/1. Data")

#Read database files and assign column types if needed
Sample <- read_csv("Raw Data/STN/CSVs/Sample.csv",
                   col_types=c("dDffdddddddddcdffdffddddddddddddlllllll"))

TowEffort <- read_csv("Raw Data/STN/CSVs/TowEffort.csv",col_types = "dddccddddcl")

Catch <- read_csv("Raw Data/STN/CSVs/Catch.csv")

Length <- read_csv("Raw Data/STN/CSVs/Length.csv")

Station <- read_csv("Raw Data/STN/CSVs/luStation.csv",col_types = "dflddccddddddcffcfddddddddd")%>%
  rename("StationCode"="StationCodeSTN")


#Lookup tables

luMicrocystis <- read_csv("Raw Data/STN/CSVs/luMicrocystis.csv",col_types = "fc")

luOrganism <- read_csv("Raw Data/STN/CSVs/luOrganism.csv")

luTide <- read_csv("Raw Data/STN/CSVs/luTide.csv")

luTowDirection <- read_csv("Raw Data/STN/CSVs/luTowDirection.csv")

luIndexWeights <- read_csv("Raw Data/STN/CSVs/IndexWeights.csv",col_types = "fffdd")


STN_Tidy_All <- Sample %>% right_join(TowEffort,by="SampleRowID")%>%
  right_join(Catch,by="TowRowID")%>%
  right_join(Length, by = "CatchRowID")%>%
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
  left_join(luOrganism,by="OrganismCodeSTN")%>%
  add_column(SurveySeason="STN",.before = "SampleRowID")%>%
  left_join(luIndexWeights,by="StationCode")%>%
  mutate(Year=year(SampleDate),
         Month=month(SampleDate),
         JulianDay = yday(SampleDate),
         .after="SampleDate")%>%
  left_join(Station,by="StationCode")%>%
  mutate(longitude_STN=-(LonD+LonM/60+LonS/3600),latitude_STN=LatD+LatM/60+LatS/3600)%>%
  select(-c(LatD:LonS)) %>%
  mutate(longitude_STN_2019=-(LonD_2019+LonM_2019/60+LonS_2019/3600),
         latitude_STN_2019=LatD_2019+LatM_2019/60+LatS_2019/3600)%>%
  select(-c(LatD_2019:LonS_2019))   


plot(STN_Tidy_All$longitude_STN,STN_Tidy_All$latitude_STN)
points(STN_Tidy_All$longitude_STN_2019,STN_Tidy_All$latitude_STN_2019,pch=".")


STN_Tidy <- STN_Tidy_All %>% select(c(1:4,6:13,19:30,38:45,48:53,57:60,62,63,67,68))

which(names(STN_Tidy_All)=="OrganismCodeSTN")

names(STN_Tidy_All)

