#Need All Targets
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


FMWT_Dat_SubReg <- Review_Data_Tows %>% 
  filter(SurveySeason=="FMWT"&is.na(SubRegion)==F)%>%
  group_by(SubRegion)%>%
  mutate(Station_Latitude=mean(Station_Latitude,na.rm=T),Station_Longitude=mean(Station_Longitude,na.rm=T))%>%
  group_by(SubRegion,Station_Longitude,Station_Latitude)%>%
  summarise_at(vars(contains("CPUV")),mean)%>%
  ungroup()%>%
  mutate_at(vars(contains("CPUV")),round,3)%>%
  rowwise() %>% 
  mutate(CPUVTotal = sum(c_across(contains("CPUV"))))%>%
  filter(CPUVTotal>0)%>%
  column_to_rownames("SubRegion")%>%
  select(-c(CPUV_Other_Fish,CPUV_Other_Crustacean,CPUV_Prickly_Sculpin_Age_0,CPUVTotal))

Community_Matrix <- FMWT_Dat_SubReg%>%
  select(contains("CPUV"))


CM_Stand <- Community_Matrix%>%
  .^(1/3) %>%
  vegan::decostand("standardize")



FMWTClust <- hclust(dist(CM_Stand))

plot(FMWTClust)

FMWT_Clusters <- FMWT_Dat_SubReg %>% rownames_to_column(var="SubRegion")%>%
  add_column(Cluster =as.factor(cutree(FMWTClust,k=5)))%>%select(SubRegion,Cluster)


table(FMWT_Clusters$Cluster)
#====================================================================================================


STN_Dat_SubReg <- Review_Data_Tows %>% 
  filter(SurveySeason=="STN"&is.na(SubRegion)==F)%>%
  group_by(SubRegion)%>%
  mutate(Station_Latitude=mean(Station_Latitude,na.rm=T),Station_Longitude=mean(Station_Longitude,na.rm=T))%>%
  group_by(SubRegion,Station_Longitude,Station_Latitude)%>%
  summarise_at(vars(contains("CPUV")),mean)%>%
  ungroup()%>%
  mutate_at(vars(contains("CPUV")),round,3)%>%
  rowwise() %>% 
  mutate(CPUVTotal = sum(c_across(contains("CPUV"))))%>%
  filter(CPUVTotal>0)%>%
  column_to_rownames("SubRegion")%>%
  select(-c(CPUV_Other_Fish,CPUV_Other_Crustacean,CPUVTotal,CPUV_White_Sturgeon_Age_0))

Community_Matrix <- STN_Dat_SubReg%>%
  select(contains("CPUV"))


CM_Stand <- Community_Matrix%>%
  .^(1/3) %>%
  vegan::decostand("standardize")



clust1 <- hclust(dist(CM_Stand))

plot(clust1)

STN_Dat_SubReg %>% rownames_to_column(var="StationCode")%>%
  add_column(Cluster =as.factor(cutree(clust1,k=6)))%>%
  ggplot(aes(x=Station_Longitude,y=Station_Latitude,col=Cluster,label=StationCode))+geom_text()


#====================================================================================================


SKT_Dat_SubReg <- Review_Data_Tows %>% 
  filter(SurveySeason=="SKT"&is.na(SubRegion)==F)%>%
  group_by(SubRegion)%>%
  mutate(Station_Latitude=mean(Station_Latitude,na.rm=T),Station_Longitude=mean(Station_Longitude,na.rm=T))%>%
  group_by(SubRegion,Station_Longitude,Station_Latitude)%>%
  summarise_at(vars(contains("CPUV")),mean)%>%
  ungroup()%>%
  mutate_at(vars(contains("CPUV")),round,3)%>%
  rowwise() %>% 
  mutate(CPUVTotal = sum(c_across(contains("CPUV"))))%>%
  filter(CPUVTotal>0)%>%
  column_to_rownames("SubRegion")%>%
  select(-c(CPUV_Other_Fish,CPUV_Other_Crustacean,CPUVTotal,CPUV_Striped_Bass_Age_0,
            CPUV_Tridentiger_Spp._Age_0,CPUV_Prickly_Sculpin_Age_0,CPUV_White_Sturgeon_Age_0))

Community_Matrix <- SKT_Dat_SubReg%>%
  select(contains("CPUV"))


CM_Stand <- Community_Matrix%>%
  .^(1/3) %>%
  vegan::decostand("standardize")



clust1 <- hclust(dist(CM_Stand))

plot(clust1)

SKT_Dat_SubReg %>% rownames_to_column(var="StationCode")%>%
  add_column(Cluster =as.factor(cutree(clust1,k=4)))%>%
  ggplot(aes(x=Station_Longitude,y=Station_Latitude,col=Cluster,label=StationCode))+geom_text()

#====================================================================================================


TMM_Dat_SubReg <- Review_Data_Tows %>% 
  filter(SurveySeason=="20mm"&is.na(SubRegion)==F)%>%
  group_by(SubRegion)%>%
  mutate(Station_Latitude=mean(Station_Latitude,na.rm=T),Station_Longitude=mean(Station_Longitude,na.rm=T))%>%
  group_by(SubRegion,Station_Longitude,Station_Latitude)%>%
  summarise_at(vars(contains("CPUV")),mean)%>%
  ungroup()%>%
  mutate_at(vars(contains("CPUV")),round,3)%>%
  rowwise() %>% 
  mutate(CPUVTotal = sum(c_across(contains("CPUV"))))%>%
  filter(CPUVTotal>0)%>%
  column_to_rownames("SubRegion")%>%
  select(-c(CPUV_Other_Fish,CPUV_Other_Crustacean,CPUVTotal,CPUV_Crangon,CPUV_Gelatinous,CPUV_Steelhead_Age_0))

Community_Matrix <- TMM_Dat_SubReg%>%
  select(contains("CPUV"))


CM_Stand <- Community_Matrix%>%
  .^(1/3) %>%
  vegan::decostand("standardize")



clust1 <- hclust(dist(CM_Stand))

plot(clust1)

TMM_Dat_SubReg %>% rownames_to_column(var="StationCode")%>%
  add_column(Cluster =as.factor(cutree(clust1,k=5)))%>%
  ggplot(aes(x=Station_Longitude,y=Station_Latitude,col=Cluster,label=StationCode))+geom_text()


#====================================================================================================


SLS_Dat_SubReg <- Review_Data_Tows %>% 
  filter(SurveySeason=="SLS"&is.na(SubRegion)==F)%>%
  group_by(SubRegion)%>%
  mutate(Station_Latitude=mean(Station_Latitude,na.rm=T),Station_Longitude=mean(Station_Longitude,na.rm=T))%>%
  group_by(SubRegion,Station_Longitude,Station_Latitude)%>%
  summarise_at(vars(contains("CPUV")),mean)%>%
  ungroup()%>%
  mutate_at(vars(contains("CPUV")),round,3)%>%
  rowwise() %>% 
  mutate(CPUVTotal = sum(c_across(contains("CPUV"))))%>%
  filter(CPUVTotal>0)%>%
  column_to_rownames("SubRegion")%>%
  select(-c(CPUV_Other_Fish,CPUV_Other_Crustacean,CPUVTotal,CPUV_Crangon,CPUV_Gelatinous,CPUV_Steelhead_Age_0,
            CPUV_American_Shad_Age_1,CPUV_Striped_Bass_Age_1,CPUV_Threadfin_Shad_Age_0,CPUV_Starry_Flounder_Age_0,
            CPUV_American_Shad_Age_1,CPUV_Chinook_Salmon_Age_1,CPUV_Starry_Flounder_Age_1,CPUV_American_Shad_Age_0))

Community_Matrix <- SLS_Dat_SubReg%>%
  select(contains("CPUV"))


CM_Stand <- Community_Matrix%>%
  .^(1/3) %>%
  vegan::decostand("standardize")



clust1 <- hclust(dist(CM_Stand))

plot(clust1)

SLS_Dat_SubReg %>% rownames_to_column(var="StationCode")%>%
  add_column(Cluster =as.factor(cutree(clust1,k=5)))%>%
  ggplot(aes(x=Station_Longitude,y=Station_Latitude,col=Cluster,label=StationCode))+geom_text()



#====================================================================================================



Env_Dat_Stations <- Review_Data_Tows %>% 
 filter(StationCode != "794")%>%
 select(SubRegion,StationCode,Station_Latitude,Station_Longitude,Temperature,Salinity,Secchi)%>%
 filter(is.na(SubRegion)==F)%>%
 group_by(SubRegion,StationCode)%>%
 mutate(N_Tows = n())%>%
 mutate(Station_Latitude=mean(Station_Latitude,na.rm=T),Station_Longitude=mean(Station_Longitude,na.rm=T))%>%
 group_by(StationCode,SubRegion,Station_Longitude,Station_Latitude,N_Tows)%>%
 drop_na()%>%
 summarise_at(vars(c(Temperature,Salinity,Secchi)),list(mean=mean,CV=CV))%>%
 ungroup()%>%
 column_to_rownames("StationCode")
 
Community_Matrix <- Env_Dat_Stations%>%
  select(Temperature_mean:Secchi_CV)


CM_Stand <- Community_Matrix%>%
  .^(1/3) %>%
  vegan::decostand("standardize")



clust1 <- hclust(dist(CM_Stand))

plot(clust1)

Env_Dat_Stations %>% 
  rownames_to_column("StationCode")%>%
  add_column(Cluster =as.factor(cutree(clust1,k=4)))%>%
  ggplot(aes(x=Station_Longitude,y=Station_Latitude,col=Cluster,label=StationCode))+geom_text()


Env_Dat_Stations %>% 
  rownames_to_column("StationCode")%>%
  add_column(Cluster =as.factor(cutree(clust1,k=4)))%>%
  ggplot(aes(x=as.factor(Cluster),y=Temperature_mean))+geom_boxplot()

#===============================================================================================

FMWT_PCA_Input <- Review_Data_Tows %>% 
  filter(SurveySeason=="FMWT" & is.na(SubRegion)==F) %>%
  group_by(Cluster,Year)%>%
  summarise_at(vars(contains("CPUV")),mean,na.rm=T)%>%
  ungroup()%>%
  select((contains("CPUV")))%>%
  .^(1/3) %>%
  vegan::decostand("standardize")%>%
  select_if(~ !any(is.na(.)))

FMWT_PCA <- prcomp(FMWT_PCA_Input)

biplot(FMWT_PCA)

Review_Data_Tows %>% 
  left_join(FMWT_Clusters,by="SubRegion")%>%
  filter(SurveySeason=="FMWT" & is.na(SubRegion)==F) %>%
  group_by(Cluster,Year)%>%
  summarise_at(vars(c(contains("CPUV"),Salinity)),mean,na.rm=T)%>%
  ungroup()%>%
  mutate(Salinity = round(Salinity,1))%>%
  select(-contains("Length"))%>%
  add_column(PC1 = FMWT_PCA$x[,1])%>%
  add_column(PC2 = FMWT_PCA$x[,2])%>%
  ggplot(aes(x=PC1,y=PC2,fill=Cluster))+
  stat_ellipse(geom = "polygon", alpha = 1/2, aes(fill = Cluster))+
  geom_text(aes(col=Salinity,label=Salinity))+
  scale_color_viridis_c(option="magma")+
  scale_fill_viridis_d()


Review_Data_Tows %>% 
  left_join(FMWT_Clusters,by="SubRegion")%>%
  filter(SurveySeason=="FMWT" & is.na(SubRegion)==F) %>%
  group_by(Cluster,Year)%>%
  summarise_at(vars(c(contains("CPUV"),Salinity,Secchi,Temperature,Depth)),mean,na.rm=T)%>%
  ungroup()%>%
  mutate(Salinity = round(Salinity,1))%>%
  select(-contains("Length"))%>%
  add_column(PC1 = FMWT_PCA$x[,1])%>%
  add_column(PC2 = FMWT_PCA$x[,2])%>%
  ggplot(aes(x=Depth,y=PC1))+
  geom_point()



#==========================Single variable analysis==============================

Full_PCA_Variables<- Review_Data_Tows %>% 
  filter(SurveySeason != "SLS",is.na(SubRegion)==F) %>%
  group_by(StationCode,Year,Month,SurveySeason,Region,SubRegion,Station_Latitude,Station_Longitude)%>%
  summarise_at(vars(c(contains("CPUV"),Salinity,Secchi,Temperature,Depth,)),mean,na.rm=T)%>%
  ungroup()

Full_PCA_Input <- Full_PCA_Variables%>%
  select((contains("CPUV")))%>%
  .^(1/3) %>%
  vegan::decostand("standardize")%>%
  select_if(~ !any(is.na(.)))

Station_PCA <- prcomp(Full_PCA_Input)

plot(Station_PCA$x[,1:2],col=Full_PCA_Variables$SurveySeason)
plot(Station_PCA$x[,1:2],col=Full_PCA_Variables$Region)

pca_brt_data <- Full_PCA_Variables %>% 
  add_column(PC1 = Station_PCA$x[,1])%>% 
  add_column(PC2 = Station_PCA$x[,2])%>%
  data.frame()



brt_PC1 <- gbm.step(data=pca_brt_data,
                    gbm.x = c(3,4,8,36:39),
                    gbm.y= 40,
                    family="gaussian",
                    step.size = 100,
                    learning.rate=.5
)

summary(brt_PC1)

gbm.plot(brt_PC1)


plot(dat_binary$Salinity,dat_binary$PC2)

#==============================================================================================

dat_binary <- Review_Data_Tows%>%
  filter(is.na(SubRegion)==F) %>%
  mutate(TowDepth = if_else(is.na(TowDepth)==F&TowDepth<0,0,TowDepth))%>%
  group_by(StationCode,Year,Month,SurveySeason,Region,SubRegion,Station_Latitude,Station_Longitude)%>%
  summarise_at(vars(c(contains("CPUV"),Salinity,Secchi,Temperature,Depth,TowDepth)),mean,na.rm=T)%>%
  ungroup()%>%
  mutate(Salinity = round(Salinity,1))%>%
  mutate_at(vars(contains("CPUV")),~if_else(.==0,0,1))%>%
  data.frame()

dat_binary$Salinity[is.nan(dat_binary$Salinity)==T] <- NA

names(dat_binary)

#Anchovy age 0
brt_full_Anchovy_0 <- gbm.step(data=data.frame(dat_binary),
                       gbm.x = c(3,4,8,36:39),
                       gbm.y= 10,
                       family="bernoulli",
                       step.size = 50,
                       learning.rate=.75
                       )
summary(brt_full_Anchovy_0)
gbm.plot(brt_full_Anchovy_0)

#Delta Smelt age-0
brt_full_DS_0 <- gbm.step(data=data.frame(dat_binary),
                          gbm.x = c(3,4,8,36:39),
                          gbm.y= 22,
                          family="bernoulli",
                          step.size = 100,
                          learning.rate = .5
)

summary(brt_full_DS_0)
gbm.plot(brt_full_DS_0)




#Striped bass age-0
brt_full_SB_0 <- gbm.step(data=data.frame(dat_binary),
                     gbm.x = c(3,4,8,36:39),
                     gbm.y= 19,
                     family="bernoulli",
                     step.size = 100,
                     learning.rate = .4
)

summary(brt_full_SB_0)
gbm.plot(brt_full_SB_0)


#Longfin smelt age-1
brt_full_LF_0 <- gbm.step(data=data.frame(dat_binary),
                     gbm.x = c(3,4,8,36:39),
                     gbm.y= 12,
                     family="bernoulli",
                     step.size = 100,
                     learning.rate=.5
)

summary(brt_full_LF_0)
gbm.plot(brt_full_LF_0)







Station_Date <- Review_Data_Tows%>%
  filter(is.na(Region)==F)%>%
  group_by(Region, SubRegion, SurveySeason,StationCode,SampleDate,Station_Longitude,Station_Latitude)%>%
  summarise_at(vars(c(contains("CPUV"),
                      contains("Length"),
                      Salinity,
                      Secchi,
                      Turbidity,
                      Temperature,
                      Depth,
                      TowDepth)),mean,na.rm=T)%>%
  ungroup()%>%
  mutate_all(~replace(., is.nan(.), NA))%>%
  mutate(Region = as.character(Region))%>%
  mutate(Region = if_else(SubRegion %in% c("Suisun Marsh",
                                           "Upper Napa River",
                                           "Lower Napa River",
                                           "Cache Slough and Liberty Island",
                                           "West Suisun Bay",
                                           "Mid Suisun Bay",
                                           "Carquinez Strait"),as.character(SubRegion),Region))%>%
  mutate(Region = as.factor(recode(Region, 
                         "Upper Napa River" = "Napa River",
                         "Lower Napa River" = "Napa River",
                         "West" = "Confluence")))

table(is.na(Station_Date$Station_Latitude))


