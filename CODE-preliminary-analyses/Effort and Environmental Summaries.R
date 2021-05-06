require(gbm)
require(vegan)
require(mgcv)
require(dismo)
require(tidyverse)
library(lubridate)
select <- dplyr::select

setwd("C:/Users/40545/Documents/GitHub/pelagicsurveys")

load("FINAL_REVIEW_DATA/CDFW_Pelagic_Review_Data.rda")

#====================================================================================================


#====================================================================================================
#Effort figures

pdf("TemporaryOutputs/Effort Survey Heatmap.pdf",height = 16, width = 12)
Review_Data_By_Station %>% 
  group_by(Year,StationCode,SurveySeason,Review_Region)%>%
  summarise(N_Surveys = n())%>%
  ungroup()%>%
  mutate(SurveySeason = factor(SurveySeason,levels = c("SLS","SKT","20mm","STN","FMWT")))%>%
  arrange(-N_Surveys)%>%
  mutate(N_Surveys = factor(N_Surveys,levels=unique(N_Surveys)))%>%
  ggplot(aes(x=Year,y=StationCode,fill=N_Surveys))+
  geom_tile()+scale_fill_manual(values=terrain.colors(10))+
  facet_grid(rows=vars(Review_Region),cols = vars(SurveySeason),drop=T,scales = "free_y",space="free_y")+
  theme_bw()
dev.off()

pdf("TemporaryOutputs/Effort Month Heatmap.pdf",height = 16, width = 16)
Station_Date %>% 
  group_by(Year,StationCode,Month,Region)%>%
  summarise(N_Surveys = n())%>%
  ungroup()%>%
  arrange(N_Surveys)%>%
  mutate(N_Surveys = factor(N_Surveys,levels=unique(N_Surveys)))%>%
  ggplot(aes(x=Year,y=StationCode,fill=N_Surveys))+
  geom_tile()+scale_fill_viridis_d()+
  facet_grid(rows=vars(Region),cols = vars(Month),drop=T,scales = "free_y",space="free_y")+
  theme_bw()
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






