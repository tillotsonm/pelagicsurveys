
library("sf")
library("tidyverse")
library("rnaturalearth")
library("rnaturalearthdata")
library("rnaturalearthhires")
library("stringr")
library("RColorBrewer")
library("spatialEco")
library("vegan")
library("lubridate")
theme_set(theme_bw())
select <- dplyr::select
filter <- dplyr::filter

setwd("C:/Users/40545/Documents/GitHub/pelagicsurveys")

load("FINAL_REVIEW_DATA/CDFW_Pelagic_Review_Data.rda")

#====================================================================================================
#Effort plot of stations ranked by # of SurveySeasons 




#====================================================================================================
#Effort figures by # of dates visited

pdf("TemporaryOutputs/Effort Survey Heatmap Status and Trends.pdf",height = 16, width = 12)
Review_Data_By_Station %>% 
  filter(SurveySeason %in% c("20mm","STN","FMWT")&Year<2020)%>%
  group_by(Year,StationCode,SurveySeason,Review_Region,Month)%>%
  summarise(N_Surveys = n())%>%
  ungroup()%>%
  mutate(SurveySeason = factor(SurveySeason,levels = c("20mm","STN","FMWT")))%>%
  arrange(N_Surveys)%>%
  mutate(N_Surveys = factor(N_Surveys,levels=unique(N_Surveys)))%>%
  mutate(Review_Region = recode_factor(Review_Region,
                                "Napa River" = "NR",
                                "Suisun Marsh" = "SM",
                                "Cache Slough and Liberty Island" = "Cache"))%>%
  mutate(Review_Region = factor(Review_Region,levels=c("NR","Far West","SM","Confluence","Cache","North","South")))%>%
  ggplot(aes(x=Year,y=StationCode,fill=N_Surveys))+
  geom_tile()+scale_fill_viridis_d()+
  facet_grid(rows=vars(Review_Region),cols = vars(SurveySeason,Month),drop=T,scales = "free_y",space="free_y")+
  theme_bw()
dev.off()

Review_Data_By_Station %>% 
filter(SurveySeason %in% c("SLS","SKT","20mm")&Year<2020)%>%
  group_by(Year,StationCode,SurveySeason,SubRegion,Month)%>%
  summarise(N_Surveys = n())%>%
  ungroup()%>%
  mutate(SurveySeason = factor(SurveySeason,levels = c("SLS","SKT","20mm")))%>%
  arrange(N_Surveys)%>%
  mutate(N_Surveys = factor(N_Surveys,levels=unique(N_Surveys)))%>%
  ggplot(aes(x=Year,y=StationCode,fill=N_Surveys))+
  geom_tile()+scale_fill_viridis_d()+
  facet_grid(cols=vars(SubRegion),rows = vars(SurveySeason,Month),drop=T,scales = "free_y",space="free_y")+
  theme_bw()



pdf("TemporaryOutputs/Effort Survey Heatmap Operations Monitoring.pdf",height = 16, width = 12)
Review_Data_By_Station %>% 
  filter(SurveySeason %in% c("SLS","20mm","SKT")&Year<2020)%>%
  group_by(Year,StationCode,SurveySeason,Review_Region)%>%
  summarise(N_Surveys = n())%>%
  ungroup()%>%
  mutate(SurveySeason = factor(SurveySeason,levels = c("SLS","20mm")))%>%
  arrange(N_Surveys)%>%
  mutate(N_Surveys = factor(N_Surveys,levels=unique(N_Surveys)))%>%
  mutate(Review_Region = recode_factor(Review_Region,
                                       "Suisun Marsh" = "SM",
                                       "Cache Slough and Liberty Island" = "Cache Slough"))%>%
  mutate(Review_Region = factor(Review_Region,levels=c("Napa River","Far West","SM","Confluence",
                                                       "Cache Slough","North","South")))%>%
  ggplot(aes(x=Year,y=StationCode,fill=N_Surveys))+
  geom_tile()+scale_fill_viridis_d()+
  facet_grid(rows=vars(Review_Region),cols = vars(SurveySeason),drop=T,scales = "free_y",space="free_y")+
  theme_bw()
dev.off()

pdf("TemporaryOutputs/Monthly Status and Trends Effort Heatmap.pdf",height = 16, width = 16)
Review_Data_By_Station %>% 
  filter(SurveySeason %in% c("STN","FMWT","SKT")&Year<2020)%>%
  group_by(Year,StationCode,Month,Review_Region)%>%
  summarise(N_Surveys = n())%>%
  ungroup()%>%
  mutate(Review_Region = recode_factor(Review_Region,
                                       "Napa River" = "NR",
                                       "Suisun Marsh" = "SM",
                                       "Cache Slough and Liberty Island" = "Cache"))%>%
  mutate(Review_Region = factor(Review_Region,levels=c("NR","Far West","SM","Confluence","Cache","North","South")))%>%
  arrange(N_Surveys)%>%
  mutate(N_Surveys = factor(N_Surveys,levels=unique(N_Surveys)))%>%
  ggplot(aes(x=Year,y=StationCode,fill=N_Surveys))+
  geom_tile()+scale_fill_viridis_d()+
  facet_grid(rows=vars(Review_Region),cols = vars(Month),drop=T,scales = "free_y",space="free_y")+
  theme_bw()+
  ggtitle("Monthly status and trends (STN,FMWT,SKT) survey per month 2002-2019")
dev.off()

pdf("TemporaryOutputs/Monthly Operations Monitoring Effort Heatmap.pdf",height = 16, width = 16)
Review_Data_By_Station %>% 
  filter(SurveySeason %in% c("20mm","SLS")&Year<2020)%>%
  group_by(Year,StationCode,Month,Review_Region)%>%
  summarise(N_Surveys = n())%>%
  ungroup()%>%
  mutate(Review_Region = recode_factor(Review_Region,
                                       "Suisun Marsh" = "SM",
                                       "Cache Slough and Liberty Island" = "Cache Slough"))%>%
  mutate(Review_Region = factor(Review_Region,levels=c("Napa River","Far West","SM","Confluence",
                                                       "Cache Slough","North","South")))%>%
  arrange(N_Surveys)%>%
  mutate(N_Surveys = factor(N_Surveys,levels=unique(N_Surveys)))%>%
  ggplot(aes(x=Year,y=StationCode,fill=N_Surveys))+
  geom_tile()+scale_fill_viridis_d()+
  facet_grid(rows=vars(Review_Region),cols = vars(Month),drop=T,scales = "free_y",space="free_y")+
  theme_bw()+
  ggtitle("Monthly operations monitoring (SLS, 20mm) surveys per month, 2002-2019")
dev.off()

#====================================================================================================
#Effort figures by # of tows

pdf("TemporaryOutputs/Effort Survey Heatmap Status and Trends - Tows.pdf",height = 16, width = 12)
Review_Data_Tows %>% 
  filter(SurveySeason %in% c("STN","FMWT","SKT")&Year<2020)%>%
  group_by(Year,StationCode,SurveySeason,Review_Region)%>%
  summarise(N_Tows = n())%>%
  ungroup()%>%
  mutate(SurveySeason = factor(SurveySeason,levels = c("STN","FMWT","SKT")))%>%
  arrange(N_Tows)%>%
  mutate(N_Tows = factor(N_Tows,levels=unique(N_Tows)))%>%
  mutate(Review_Region = recode_factor(Review_Region,
                                       "Napa River" = "NR",
                                       "Suisun Marsh" = "SM",
                                       "Cache Slough and Liberty Island" = "Cache"))%>%
  mutate(Review_Region = factor(Review_Region,levels=c("NR","Far West","SM","Confluence","Cache","North","South")))%>%
  ggplot(aes(x=Year,y=StationCode,fill=N_Tows))+
  geom_tile()+scale_fill_viridis_d()+
  facet_grid(rows=vars(Review_Region),cols = vars(SurveySeason),drop=T,scales = "free_y",space="free_y")+
  theme_bw()
dev.off()

pdf("TemporaryOutputs/Effort Survey Heatmap Operations Monitoring - Tows.pdf",height = 16, width = 12)
Review_Data_Tows %>% 
  filter(SurveySeason %in% c("SLS","20mm")&Year<2020)%>%
  group_by(Year,StationCode,SurveySeason,Review_Region)%>%
  summarise(N_Tows = n())%>%
  ungroup()%>%
  mutate(SurveySeason = factor(SurveySeason,levels = c("SLS","20mm")))%>%
  arrange(N_Tows)%>%
  mutate(N_Tows = factor(N_Tows,levels=unique(N_Tows)))%>%
  mutate(Review_Region = recode_factor(Review_Region,
                                       "Suisun Marsh" = "SM",
                                       "Cache Slough and Liberty Island" = "Cache Slough"))%>%
  mutate(Review_Region = factor(Review_Region,levels=c("Napa River","Far West","SM","Confluence",
                                                       "Cache Slough","North","South")))%>%
  ggplot(aes(x=Year,y=StationCode,fill=N_Tows))+
  geom_tile()+scale_fill_viridis_d()+
  facet_grid(rows=vars(Review_Region),cols = vars(SurveySeason),drop=T,scales = "free_y",space="free_y")+
  theme_bw()
dev.off()

pdf("TemporaryOutputs/Monthly Status and Trends Effort Heatmap - Tows.pdf",height = 16, width = 16)
Review_Data_Tows %>% 
  filter(SurveySeason %in% c("STN","FMWT","SKT")&Year<2020)%>%
  group_by(Year,StationCode,Month,Review_Region)%>%
  summarise(N_Tows = n())%>%
  ungroup()%>%
  mutate(Review_Region = recode_factor(Review_Region,
                                       "Napa River" = "NR",
                                       "Suisun Marsh" = "SM",
                                       "Cache Slough and Liberty Island" = "Cache"))%>%
  mutate(Review_Region = factor(Review_Region,levels=c("NR","Far West","SM","Confluence","Cache","North","South")))%>%
  arrange(N_Tows)%>%
  mutate(N_Tows = factor(N_Tows,levels=unique(N_Tows)))%>%
  ggplot(aes(x=Year,y=StationCode,fill=N_Tows))+
  geom_tile()+scale_fill_viridis_d()+
  facet_grid(rows=vars(Review_Region),cols = vars(Month),drop=T,scales = "free_y",space="free_y")+
  theme_bw()+
  ggtitle("Monthly status and trends (STN,FMWT,SKT) tows per month 2002-2019")
dev.off()

pdf("TemporaryOutputs/Monthly Operations Monitoring Effort Heatmap - Tows.pdf",height = 16, width = 16)
Review_Data_Tows %>% 
  filter(SurveySeason %in% c("20mm","SLS")&Year<2020)%>%
  group_by(Year,StationCode,Month,Review_Region)%>%
  summarise(N_Tows = n())%>%
  ungroup()%>%
  mutate(Review_Region = recode_factor(Review_Region,
                                       "Suisun Marsh" = "SM",
                                       "Cache Slough and Liberty Island" = "Cache Slough"))%>%
  mutate(Review_Region = factor(Review_Region,levels=c("Napa River","Far West","SM","Confluence",
                                                       "Cache Slough","North","South")))%>%
  arrange(N_Tows)%>%
  mutate(N_Tows = factor(N_Tows,levels=unique(N_Tows)))%>%
  ggplot(aes(x=Year,y=StationCode,fill=N_Tows))+
  geom_tile()+scale_fill_viridis_d()+
  facet_grid(rows=vars(Review_Region),cols = vars(Month),drop=T,scales = "free_y",space="free_y")+
  theme_bw()+
  ggtitle("Monthly operations monitoring (SLS, 20mm) tows per month, 2002-2019")
dev.off()

#Salinity Figures
pdf("TemporaryOutputs/Enivronment by Region and Survey figures.pdf",height = 16, width = 12)
Station_Date %>% 
  #Add regional mean
  group_by(Region) %>% 
  mutate(Region_Mean = mean(Salinity,na.rm=T))%>%
  ungroup()%>%
  #Order surveys in time
  mutate(SurveySeason = factor(SurveySeason,levels = c("SLS","SKT","20mm","STN","FMWT")))%>%
  #Add survey-region mean
  group_by(Region,SurveySeason) %>% 
  mutate(Region_Survey_Mean = mean(Salinity,na.rm=T))%>%
  ungroup()%>%
  #Calculate annual means
  group_by(StationCode,SubRegion,Region,SurveySeason,Year,Region_Mean,Region_Survey_Mean) %>% 
  summarise(Salinity = mean(Salinity,na.rm=T))%>%
  ungroup()%>%
  ggplot(aes(y=StationCode,x=Salinity,fill=SubRegion,col=SubRegion))+
  geom_vline(aes(xintercept = Region_Mean),col="black",size=1.25)+
  geom_vline(aes(xintercept = Region_Survey_Mean),col="gray30",size=1.25,alpha=.5)+
  geom_boxplot()+scale_fill_viridis_d(alpha=.4)+scale_color_viridis_d()+
  facet_grid(rows=vars(Region),cols=vars(SurveySeason),drop=T,scales="free_y",space="free_y")+
  theme_bw()+
  theme(legend.position = "bottom")+
  ggtitle("Geographic and seasonal patterns in salinity",
          "Boxplots reflect between-year variability")




#Secchi Figure

Station_Date %>% 
  #Add regional mean
  group_by(Region) %>% 
  mutate(Region_Mean = mean(Secchi,na.rm=T))%>%
  ungroup()%>%
  #Order surveys in time
  mutate(SurveySeason = factor(SurveySeason,levels = c("SLS","SKT","20mm","STN","FMWT")))%>%
  #Add survey-region mean
  group_by(Region,SurveySeason) %>% 
  mutate(Region_Survey_Mean = mean(Secchi,na.rm=T))%>%
  ungroup()%>%
  #Calculate annual means
  group_by(StationCode,SubRegion,Region,SurveySeason,Year,Region_Mean,Region_Survey_Mean) %>% 
  summarise(Secchi = mean(Secchi,na.rm=T))%>%
  ungroup()%>%
  ggplot(aes(y=StationCode,x=Secchi,fill=SubRegion,col=SubRegion))+
  geom_vline(aes(xintercept = Region_Mean),col="black",size=1.25)+
  geom_vline(aes(xintercept = Region_Survey_Mean),col="gray30",size=1.25,alpha=.5)+
  geom_boxplot()+scale_fill_viridis_d(alpha=.4)+scale_color_viridis_d()+
  facet_grid(rows=vars(Region),cols=vars(SurveySeason),drop=T,scales="free_y",space="free_y")+
  theme_bw()+
  theme(legend.position = "bottom")+
  ggtitle("Geographic and seasonal patterns in Secchi",
          "Boxplots reflect between-year variability")

#Temperature Figure

Station_Date %>% 
  #Add regional mean
  group_by(Region) %>% 
  mutate(Region_Mean = mean(Temperature,na.rm=T))%>%
  ungroup()%>%
  #Order surveys in time
  mutate(SurveySeason = factor(SurveySeason,levels = c("SLS","SKT","20mm","STN","FMWT")))%>%
  #Add survey-region mean
  group_by(Region,SurveySeason) %>% 
  mutate(Region_Survey_Mean = mean(Temperature,na.rm=T))%>%
  ungroup()%>%
  #Calculate annual means
  group_by(StationCode,SubRegion,Region,SurveySeason,Year,Region_Mean,Region_Survey_Mean) %>% 
  summarise(Temperature = mean(Temperature,na.rm=T))%>%
  ungroup()%>%
  ggplot(aes(y=StationCode,x=Temperature,fill=SubRegion,col=SubRegion))+
  geom_vline(aes(xintercept = Region_Mean),col="black",size=1.25)+
  geom_vline(aes(xintercept = Region_Survey_Mean),col="gray30",size=1.25,alpha=.5)+
  geom_boxplot()+scale_fill_viridis_d(alpha=.4)+scale_color_viridis_d()+
  facet_grid(rows=vars(Region),cols=vars(SurveySeason),drop=T,scales="free_y",space="free_y")+
  theme_bw()+
  theme(legend.position = "bottom")+
  ggtitle("Geographic and seasonal patterns in Temperature",
          "Boxplots reflect between-year variability")
#===========================================================================================

#Striped Bass Examples

Review_Data_By_Station %>% 
  #Add regional mean
  group_by(Review_Region) %>% 
  mutate(Region_Mean = mean(CPUV_Striped_Bass_Age_0^(1/3),na.rm=T))%>%
  ungroup()%>%
  #Order surveys in time
  mutate(SurveySeason = factor(SurveySeason,levels = c("SLS","SKT","20mm","STN","FMWT")))%>%
  #Add survey-region mean
  group_by(Review_Region,SurveySeason) %>% 
  mutate(Region_Survey_Mean = mean(CPUV_Striped_Bass_Age_0^(1/3),na.rm=T))%>%
  ungroup()%>%
  #Calculate annual means
  group_by(StationCode,SubRegion,Review_Region,SurveySeason,Year,Region_Mean,Region_Survey_Mean) %>% 
  summarise(CPUV = mean(CPUV_Striped_Bass_Age_0^(1/3),na.rm=T))%>%
  ungroup()%>%
  ggplot(aes(y=StationCode,x=CPUV,fill=SubRegion,col=SubRegion))+
  geom_vline(aes(xintercept = Region_Mean),col="black",size=1.25)+
  geom_vline(aes(xintercept = Region_Survey_Mean),col="gray30",size=1.25,alpha=.5)+
  geom_boxplot()+scale_fill_viridis_d(alpha=.4)+scale_color_viridis_d()+
  facet_grid(rows=vars(Review_Region),cols=vars(SurveySeason),drop=T,scales="free_y",space="free_y")+
  theme_bw()+xlab("Cube Root Age-0 Striped Bass CPUV")+
  theme(legend.position = "bottom")+
  ggtitle("Geographic and seasonal patterns in CPUV",
          "Boxplots reflect between-year variability")



Review_Data_By_Station %>% 
  #Add regional mean
  group_by(Review_Region) %>% 
  mutate(Region_Mean = mean(CPUV_Striped_Bass_Age_0,na.rm=T))%>%
  ungroup()%>%
  #Order surveys in time
  mutate(SurveySeason = factor(SurveySeason,levels = c("SLS","SKT","20mm","STN","FMWT")))%>%
  #Add survey-region mean
  group_by(Review_Region,SurveySeason) %>% 
  mutate(Region_Survey_Mean = mean(CPUV_Striped_Bass_Age_0,na.rm=T))%>%
  ungroup()%>%
  #Calculate annual means
  group_by(StationCode,SubRegion,Review_Region,SurveySeason,Year,Region_Mean,Region_Survey_Mean) %>% 
  summarise(CPUV = mean(CPUV_Striped_Bass_Age_0,na.rm=T))%>%
  ungroup()%>%
  ggplot(aes(y=StationCode,x=CPUV,fill=SubRegion,col=SubRegion))+
  geom_vline(aes(xintercept = Region_Mean),col="black",size=1.25)+
  geom_vline(aes(xintercept = Region_Survey_Mean),col="gray30",size=1.25,alpha=.5)+
  geom_boxplot()+scale_fill_viridis_d(alpha=.4)+scale_color_viridis_d()+
  facet_grid(rows=vars(Review_Region),cols=vars(SurveySeason),drop=T,scales="free_y",space="free_y")+
  theme_bw()+xlab("Age-0 Striped Bass CPUV")+
  theme(legend.position = "bottom")+
  ggtitle("Geographic and seasonal patterns in CPUV",
          "Boxplots reflect between-year variability")
dev.off()





Review_Data_By_Station %>% 
  group_by(Region) %>% 
  mutate(Region_Mean = mean(Secchi,na.rm=T))%>%
  ungroup()%>%
  mutate(SurveySeason = factor(SurveySeason,levels = c("SLS","SKT","20mm","STN","FMWT")))%>%
  group_by(Region,SurveySeason) %>% 
  mutate(Region_Survey_Mean = mean(Secchi,na.rm=T))%>%
  ungroup()%>%
  ggplot(aes(y=StationCode,x=Secchi,fill=SubRegion,col=SubRegion))+
  geom_vline(aes(xintercept = Region_Mean),col="black",size=1.25)+
  geom_vline(aes(xintercept = Region_Survey_Mean),col="gray30",size=1.25,alpha=.5)+
  geom_boxplot()+scale_fill_viridis_d(alpha=.4)+scale_color_viridis_d()+
  facet_grid(rows=vars(Region),cols=vars(SurveySeason),drop=T,scales="free_y",space="free_y")+
  theme_bw()+
  theme(legend.position = "bottom")




CV_Summary_Station <- function(response,grouping="StationCode",surveys = c("FMWT","SLS","SKT","STN","20mm")){
  
  Targets <- match(c(response,grouping),names(Station_Date))
  
  names(Station_Date)[Targets] <-c("Response","Grouping")
  
  #Mean and CV within years
  Output <-  Station_Date %>% select(1:9,Response,Grouping)%>%
    filter(SurveySeason)
    group_by(Grouping,Year)%>%
    mutate(Month_Mean = mean(Response,na.rm=T),
           Month_CV = if_else(Month_Mean==0,0,sd(Response,na.rm=T)/Month_Mean))
  
  #Variability within years
  
  return(Output)
}




#Validating regional strata by analysis of environmental conditions and variability





#+===============VOLUME SAMPLED SUMMARY==========================================


#Annual total volume by station
Annual_SampVol_Station <-
  Review_Data_Tows%>%
  group_by(SurveySeason,Review_Region,Review_Stratum,SubRegion,StationCode,Year)%>%
  summarise(Station_Surveys = length(unique(SampleDate)),
            Station_Tows = n(),
            Station_Volume= sum(Volume,na.rm=T))%>%
  ungroup()

#Annual total volume by subregion
Annual_SampVol_Subregion <-
  Review_Data_Tows%>%
  group_by(SurveySeason,Review_Region,Review_Stratum,SubRegion,Year)%>%
  summarise(
            Subregion_Tows = n(),
            Subregion_Volume= sum(Volume,na.rm=T))%>%
  ungroup()

#Annual total volume by stratum
Annual_SampVol_Stratum <-
  Review_Data_Tows%>%
  group_by(SurveySeason,Review_Region,Review_Stratum,Year)%>%
  summarise(
            Strata_Tows = n(),
            Strata_Volume= sum(Volume,na.rm=T))%>%
  ungroup()

#Annual total volume by Review Region

Annual_SampVol_Region <-
  Review_Data_Tows%>%
  group_by(SurveySeason,Review_Region,Year)%>%
  summarise(
            Region_Tows = n(),
            Region_Volume= sum(Volume,na.rm=T))%>%
  ungroup()




SampVol_Summary <- Annual_SampVol_Station%>%
  left_join(Annual_SampVol_Subregion,by=c("Review_Region","Review_Stratum","SubRegion","Year","SurveySeason"))%>%
  left_join(Annual_SampVol_Stratum,by=c("Review_Region","Review_Stratum","Year","SurveySeason"))%>%
  left_join(Annual_SampVol_Region,by=c("Review_Region","Year","SurveySeason"))%>%
  group_by(SubRegion,Year,SurveySeason)%>%
  mutate(Subregion_Surveys = sum(Station_Surveys),.before=Subregion_Tows)%>%
  ungroup()%>%
  group_by(Review_Stratum,Year,SurveySeason)%>%
  mutate(Strata_Surveys = sum(Station_Surveys),.before=Strata_Tows)%>%
  ungroup()%>%
  group_by(Review_Region,Year,SurveySeason)%>%
  mutate(Region_Surveys = sum(Station_Surveys),.before=Region_Tows)%>%
  ungroup()

write_csv(SampVol_Summary,"Volume Sampled Summary.csv")



#
Mean <- function(x){mean(x,na.rm=T)}

Station_Summary <- Review_Data_Tows %>% 
  select(SurveySeason:CPUV_Prickly_Sculpin_Age_0)%>%
  select(-c(Tide,Weather,Waves,TowNumber))%>%
  group_by(StationCode)%>%
  mutate(N_Dates = length(unique(SampleDate)),
         N_Years = length(unique(Year)),
         Mean_TowsPerYear = round(N_Dates/N_Years,0))%>%
  mutate(Surveys = paste(unique(SurveySeason),collapse=","),.after = StationCode)%>%
  group_by(StationCode,SurveySeason)%>%
  mutate_at(c("Turbidity","Salinity","Temperature","Depth","TowDepth","Volume"),list(Mean=Mean))%>%
  mutate_at(vars(contains("CPUV")),mean)%>%
  select(-c(SurveySeason:SampleDate,Temperature:Salinity))%>%
  ungroup()%>%
  distinct(across(c(StationCode)),.keep_all = T)%>%
  filter(N_Years>9)%>%
  mutate(FMWT = if_else(grepl("FMWT",Surveys),1,0),
       STN = if_else(grepl("STN",Surveys),1,0),
       SLS = if_else(grepl("SLS",Surveys),1,0),
       SKT = if_else(grepl("SKT",Surveys),1,0),
       TMM = if_else(grepl("20mm",Surveys),1,0),
       .after = Surveys
       )%>%
  select(-SurveySeason)%>%
  mutate(N_Seasons = STN+SLS+SKT+TMM+FMWT,.after=TMM)%>%
  mutate(STN= recode(STN, `1` = "Sampled", `0` = "Not Sampled"))%>%
  mutate(FMWT= recode(FMWT, `1` = "Sampled", `0` = "Not Sampled"))%>%
  mutate(SKT= recode(SKT, `1` = "Sampled", `0` = "Not Sampled"))%>%
  mutate(TMM= recode(TMM, `1` = "Sampled", `0` = "Not Sampled"))%>%
  mutate(SLS= recode(SLS, `1` = "Sampled", `0` = "Not Sampled"))%>%
  pivot_longer(FMWT:TMM,names_to="SurveySeason",values_to="Is_Surveyed")%>%
  relocate(SurveySeason:Is_Surveyed,.after=N_Seasons)%>%
  arrange(N_Seasons)%>%
  mutate(StationCode = factor(StationCode,levels=unique(StationCode)))%>%
  mutate(Review_Stratum = factor(Review_Stratum, levels=c("San Pablo Bay and Carquinez Strait",
                                                          "Napa River*",
                                                          "Suisun and Honker Bays",
                                                          "Suisun Marsh",
                                                          "Confluence",
                                                          "Cache Slough",
                                                          "North and South Forks Mokelumne River",
                                                          "South",
                                                          "Sacramento Mainstem",
                                                          "Sacramento Ship Channel")
                                 ))



pdf("TemporaryOutputs/Seasonal effort by strata grid.pdf",height = 11, width = 20) 
Station_Summary %>% 
  mutate(SurveySeason = factor(SurveySeason,levels=c("STN","TMM","SLS","SKT","FMWT")))%>%
  ggplot(aes(y=SurveySeason,x=StationCode,fill=Is_Surveyed,color="green"))+
  scale_fill_manual(values = c("white","black")) +
  geom_tile(alpha=.5,color="black")+
  labs(fill = "")+
  facet_grid(cols=vars(Review_Stratum),space = "free_x",scales = "free_x",
            labeller = label_wrap_gen(width = 2, multi_line = TRUE))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
dev.off()

pdf("TemporaryOutputs/Seasonal effort by strata points.pdf",height = 11, width = 8)
Station_Summary%>% distinct(across("StationCode"),.keep_all=T)%>%
  add_column(Plot_Sep = c(rep("b",77),rep("a",77)))%>%
  ggplot(aes(y=StationCode,x=N_Seasons,color=Review_Stratum))+
  geom_point()+theme_bw()+facet_wrap(~Plot_Sep,scales = "free_y")
dev.off()




