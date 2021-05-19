theme_set(theme_bw())
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
select <- dplyr::select
filter <- dplyr::filter

setwd("C:/Users/40545/Documents/GitHub/pelagicsurveys")

load("FINAL_REVIEW_DATA/CDFW_Pelagic_Review_Data.rda")

#===============Delta Base Layer================================
marsh <- st_read(
  "CODE-spatial-analysis/hydro-delta-marsh/hydro_delta_marsh.shp")

EDSM_Strata <- st_read(
  "CODE-spatial-analysis/DSm_Subregions_UTM10NAD83/DSm_Subregions_UTM10NAD83.shp")

Env_Points <-Review_Enviro_Hydro%>%
  select(Survey,Review_Region,SubRegion,StationCode,Longitude,Latitude)%>%
  distinct(across(c("Survey","StationCode")),.keep_all=T)%>%
  st_as_sf( coords = c("Longitude", "Latitude"), 
            crs = 4326, agr = "constant")



#==========Environmental clusters based on full environmental data================

#====================================================================================================

CV <-function(x){sd(x,na.rm = T)/mean(x,na.rm=T)}
Mean <-function(x){mean(x,na.rm=T)}


Env_Dat <- Review_Enviro_Hydro %>%
  filter(Year>2001)%>%
  ungroup()%>%
  select(Review_Region,SubRegion,Year,Month,SacWYType,StationCode,Current_Historical,
         Temperature,Salinity,Secchi,Chlorophyll)%>%
  filter(is.na(SacWYType)==F)%>%
  group_by(SubRegion,Year)%>%
  mutate(n = n())%>%
  #Remove Year-Subregion combos with fewer than 30 samples
  filter(n>2)%>%
  mutate_at(vars(c(Temperature,Salinity,Secchi,Chlorophyll)),list(Annual_Mean=Mean,WithinYear_CV=CV))%>%
  ungroup()%>%
  group_by(SubRegion)%>%
  mutate(n_years = length(unique(Year)))%>%
#Remove Subregion with fewer than 20 years
  filter(n_years>3)%>%
  select(-c(n,n_years))%>%
  distinct(across(c("SubRegion","Year")),.keep_all=T)%>%
  select(-c(Temperature:Chlorophyll))%>%
  group_by(SubRegion,Year)%>%
  mutate_at(vars(Temperature_Annual_Mean:Chlorophyll_WithinYear_CV),mean,na.rm=T)%>%
  distinct(across(c("SubRegion","SacWYType")),.keep_all=T)%>%
  ungroup()%>%
  group_by(SubRegion)%>%
  mutate_at(vars(Temperature_Annual_Mean:Chlorophyll_Annual_Mean),list(BetweenYear_CV = CV))%>%
  mutate_at(vars(Temperature_Annual_Mean:Chlorophyll_Annual_Mean_BetweenYear_CV),mean,na.rm=T)%>%
  ungroup()%>%
  distinct(across("SubRegion"),.keep_all=T)%>%
  rename("Salinity_BetweenYear_CV" = "Salinity_Annual_Mean_BetweenYear_CV",
         "Temperature_BetweenYear_CV" = "Temperature_Annual_Mean_BetweenYear_CV",
         "Secchi_BetweenYear_CV" = "Secchi_Annual_Mean_BetweenYear_CV",
         "Chlorophyll_BetweenYear_CV" = "Chlorophyll_Annual_Mean_BetweenYear_CV")%>%
  select(-c(SacWYType,Year,Month,StationCode,Current_Historical,Review_Region))
  


map_dat <- EDSM_Strata %>% left_join(Env_Dat,by="SubRegion")%>%
  filter(SubRegion != "Rock Slough and Discovery Bay")%>%
  mutate(SubRegion = as.factor(SubRegion))
  
  

Community_Matrix <- Env_Dat%>%
  column_to_rownames("SubRegion")%>%
  select(-c(Chlorophyll_Annual_Mean,Chlorophyll_WithinYear_CV,Chlorophyll_BetweenYear_CV))


CM_Stand <- Community_Matrix%>%
  .^(1/3) %>%
  vegan::decostand("standardize")


clust1 <- hclust(dist(CM_Stand),method="ward.D")

plot(clust1)

Env_Clust <- Env_Dat %>%
  add_column(Cluster =as.factor(cutree(clust1,k=7)))


map_dat_envClust <- EDSM_Strata %>% left_join(Env_Clust,by="SubRegion")%>%
  mutate(SubRegion = as.factor(SubRegion))%>%filter(is.na(Cluster)==F)

ggplot()+geom_sf(data = marsh, size = .5, color = "black", fill = NA)+
  geom_sf(data=map_dat_envClust,size = 1, color = "darkgreen",aes(fill=Cluster),alpha=.75)+
  geom_sf(data=Env_Points,color="black")+
  coord_sf(ylim = c(37.75, 38.65), expand = FALSE)


ggplot()+geom_sf(data = marsh, size = .5, color = "black", fill = NA)+
  geom_sf(data=map_dat_envClust,size = 1, color = "darkgreen",aes(fill=Salinity_Annual_Mean),alpha=.75)+
  coord_sf(ylim = c(37.75, 38.65), expand = FALSE)+
  scale_fill_viridis_c("Salinity")+
  ggtitle("Salinity","Overal mean")

ggplot()+geom_sf(data = marsh, size = .5, color = "black", fill = NA)+
  geom_sf(data=map_dat_envClust,size = 1, color = "darkgreen",aes(fill=Salinity_BetweenYear_CV),alpha=.75)+
  coord_sf(ylim = c(37.75, 38.65), expand = FALSE)+
  scale_fill_viridis_c("CV")+
  ggtitle("Salinity","Within-year coefficient of variation")

ggplot()+geom_sf(data = marsh, size = .5, color = "black", fill = NA)+
  geom_sf(data=map_dat_envClust,size = 1, color = "darkgreen",aes(fill=Salinity_WithinYear_CV),alpha=.75)+
  coord_sf(ylim = c(37.75, 38.65), expand = FALSE)+
  scale_fill_viridis_c("CV")+
  ggtitle("Salinity","Between-year coefficient of variation")

#Temperature
ggplot()+geom_sf(data = marsh, size = .5, color = "black", fill = NA)+
  geom_sf(data=map_dat_envClust,size = 1, color = "darkgreen",aes(fill=Temperature_Annual_Mean),alpha=.75)+
  coord_sf(ylim = c(37.75, 38.65), expand = FALSE)+
  scale_fill_viridis_c("Temperature")+
  ggtitle("Temperature","Overal mean")

ggplot()+geom_sf(data = marsh, size = .5, color = "black", fill = NA)+
  geom_sf(data=map_dat_envClust,size = 1, color = "darkgreen",aes(fill=Temperature_BetweenYear_CV),alpha=.75)+
  coord_sf(ylim = c(37.75, 38.65), expand = FALSE)+
  scale_fill_viridis_c("CV")+
  ggtitle("Temperature","Within-year coefficient of variation")

ggplot()+geom_sf(data = marsh, size = .5, color = "black", fill = NA)+
  geom_sf(data=map_dat_envClust,size = 1, color = "darkgreen",aes(fill=Temperature_WithinYear_CV),alpha=.75)+
  coord_sf(ylim = c(37.75, 38.65), expand = FALSE)+
  scale_fill_viridis_c("CV")+
  ggtitle("Temperature","Between-year coefficient of variation")

#Secchi
ggplot()+geom_sf(data = marsh, size = .5, color = "black", fill = NA)+
  geom_sf(data=map_dat_envClust,size = 1, color = "darkgreen",aes(fill=Secchi_Annual_Mean),alpha=.75)+
  coord_sf(ylim = c(37.75, 38.65), expand = FALSE)+
  scale_fill_viridis_c("Secchi")+
  ggtitle("Secchi","Overal mean")

ggplot()+geom_sf(data = marsh, size = .5, color = "black", fill = NA)+
  geom_sf(data=map_dat_envClust,size = 1, color = "darkgreen",aes(fill=Secchi_BetweenYear_CV),alpha=.75)+
  coord_sf(ylim = c(37.75, 38.65), expand = FALSE)+
  scale_fill_viridis_c("CV")+
  ggtitle("Secchi","Within-year coefficient of variation")

ggplot()+geom_sf(data = marsh, size = .5, color = "black", fill = NA)+
  geom_sf(data=map_dat_envClust,size = 1, color = "darkgreen",aes(fill=Secchi_WithinYear_CV),alpha=.75)+
  coord_sf(ylim = c(37.75, 38.65), expand = FALSE)+
  scale_fill_viridis_c("CV")+
  ggtitle("Secchi","Between-year coefficient of variation")
#====================================================================================================
#Environmental space and time variability

Env_Var_Dat <- Review_Enviro_Hydro %>% 
  select(Survey,Review_Region,SubRegion,Year,Month,StationCode,SacWYType,SJWYType,
         Latitude,Longitude,Temperature,Salinity,Secchi)%>%
  mutate(Latitude=mean(Latitude,na.rm=T),Longitude=mean(Longitude,na.rm=T))%>%
  ungroup()


CV <- function(x){sd(x,na.rm = T)/mean(x,na.rm=T)}

Subreg_CVs <- Env_Var_Dat%>%
  group_by(Review_Region,SubRegion)%>%
  mutate(Longitude = mean(Longitude),
         Latitude = mean(Latitude))%>%
  group_by(Review_Region,SubRegion,Longitude,Latitude)%>%
  summarise_at(c("Salinity","Temperature","Secchi"),CV)
  
Subreg_Means <- Env_Var_Dat%>%
  group_by(Review_Region,SubRegion)%>%
  mutate(Longitude = mean(Longitude),
         Latitude = mean(Latitude))%>%
  group_by(Review_Region,SubRegion,Longitude,Latitude)%>%
  summarise_at(c("Salinity","Temperature","Secchi"),mean,na.rm=T)

Subreg_Means %>% ggplot(aes(x=Longitude,y=Latitude,col=Secchi))+geom_point(size=4)+scale_color_viridis_c()

Station_Within_Year_CV <- Env_Var_Dat%>%
  group_by(Review_Region,StationCode,Station_Longitude,Station_Latitude,Year)%>%
  summarise_at(c("Salinity","Temperature","Secchi"),CV)%>%
  ungroup()%>%
  group_by(Review_Region,StationCode,Station_Longitude,Station_Latitude)%>%
  summarise_at(c("Salinity","Temperature","Secchi"),mean,na.rm=T)


Station_Between_Year_CV <- Env_Var_Dat%>%
  group_by(Review_Region,StationCode,Station_Longitude,Station_Latitude,Year)%>%
  summarise_at(c("Salinity","Temperature","Secchi"),mean,na.rm=T)%>%
  ungroup()%>%
  group_by(Review_Region,StationCode,Station_Longitude,Station_Latitude)%>%
  summarise_at(c("Salinity","Temperature","Secchi"),CV)

Station_Between_Year_CV %>%ggplot(aes(x=Station_Longitude,y=Station_Latitude,col=Secchi))+geom_point()+
  scale_color_viridis_c()+theme_bw()


Station_Within_Year_CV %>%ggplot(aes(x=Station_Longitude,y=Station_Latitude,col=Secchi))+geom_point()+
  scale_color_viridis_c()+theme_bw()








#====================================================================================================
#Effort figures by # of dates visited

pdf("TemporaryOutputs/Effort Survey Heatmap Status and Trends.pdf",height = 16, width = 12)
Review_Data_By_Station %>% 
  filter(SurveySeason %in% c("STN","FMWT","SKT")&Year<2020)%>%
  group_by(Year,StationCode,SurveySeason,Review_Region)%>%
  summarise(N_Surveys = n())%>%
  ungroup()%>%
  mutate(SurveySeason = factor(SurveySeason,levels = c("STN","FMWT","SKT")))%>%
  arrange(N_Surveys)%>%
  mutate(N_Surveys = factor(N_Surveys,levels=unique(N_Surveys)))%>%
  mutate(Review_Region = recode_factor(Review_Region,
                                "Napa River" = "NR",
                                "Suisun Marsh" = "SM",
                                "Cache Slough and Liberty Island" = "Cache"))%>%
  mutate(Review_Region = factor(Review_Region,levels=c("NR","Far West","SM","Confluence","Cache","North","South")))%>%
  ggplot(aes(x=Year,y=StationCode,fill=N_Surveys))+
  geom_tile()+scale_fill_viridis_d()+
  facet_grid(rows=vars(Review_Region),cols = vars(SurveySeason),drop=T,scales = "free_y",space="free_y")+
  theme_bw()
dev.off()

pdf("TemporaryOutputs/Effort Survey Heatmap Operations Monitoring.pdf",height = 16, width = 12)
Review_Data_By_Station %>% 
  filter(SurveySeason %in% c("SLS","20mm")&Year<2020)%>%
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






