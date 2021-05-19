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


#===============Delta Base Layer================================

region_lookup <- Review_Data_By_Station %>% select(Review_Region,Region,SubRegion)%>%
  distinct()

marsh <- st_read(
  "CODE-spatial-analysis/hydro-delta-marsh/hydro_delta_marsh.shp")

EDSM_Strata_Temp <- st_read(
  "CODE-spatial-analysis/DSm_Subregions_UTM10NAD83/DSm_Subregions_UTM10NAD83.shp")

EDSM_Strata<- EDSM_Strata_Temp %>%
  left_join(region_lookup,by=c("Region","SubRegion"))%>%
  select(-c(OBJECTID,OBJECTID_1,Shape_Leng,Shape_Area))

Station_Points <-Review_Data_By_Station%>%
  select(SurveySeason,Review_Region,SubRegion,StationCode,Station_Longitude,Station_Latitude)%>%
  distinct(across(c("SurveySeason","StationCode")),.keep_all=T)%>%
  st_as_sf( coords = c("Station_Longitude", "Station_Latitude"), 
            crs = 4326, agr = "constant")

#write_csv(Station_Points,file="StationPoints.csv")

#====================================================================================================

Cluster_Catch <- function(Survey = "FMWT", plot_tree =T , level = "Station", N_Clusters = 6, clust_method = "ward.d"){
  
  if(level=="Station"){
    Cluster_Data <- Review_Data_By_Station %>% 
      filter(SurveySeason==Survey)%>%
      group_by(StationCode,Year)%>%
      mutate(N_Surveys = n())%>%
      group_by(StationCode)%>%
      summarise_at(vars(c(N_Surveys,contains("CPUV"))),mean)%>%
      ungroup()%>%
      mutate_at(vars(contains("CPUV")),round,3)%>%
      rowwise() %>% 
      mutate(CPUVTotal = sum(c_across(contains("CPUV"))))%>%
      filter(CPUVTotal>0)%>%
      column_to_rownames("StationCode")%>%
      select(-c(CPUV_Other_Fish,CPUV_Other_Crustacean))%>%
      select_if(colSums(.)!=0)
    
    Community_Matrix <- Cluster_Data%>%
      .^(1/3)%>%
      select(contains("CPUV"))
    
    
    CM_Stand <- Community_Matrix%>%
      vegan::decostand("standardize")
    
    Clustered <- hclust(dist(CM_Stand),method=clust_method)
    
    if(plot_tree==T){print(plot(Clustered))}
    
    Clusters <- Cluster_Data %>%
      rownames_to_column("StationCode")%>%
      add_column(Cluster =as.factor(cutree(Clustered,k=N_Clusters)))
    
    
    Station_Points_Plot <- Station_Points %>%
      ungroup()%>%
      filter(SurveySeason==Survey)%>%
      left_join(Clusters,by="StationCode")%>%
      select(c(SurveySeason:geometry,Cluster))%>%
      filter(is.na(Cluster)==F)
    
    
    print(ggplot()+geom_sf(data = marsh, size = .5, color = "black", fill = NA)+
      geom_sf(data=EDSM_Strata,size = 1, color = "black",fill=NA)+
      geom_sf(data=Station_Points_Plot,aes(color=Cluster),size=5)+
      coord_sf(ylim = c(37.75, 38.65), expand = FALSE)+
      ggtitle(paste(Survey,"Station-Based Clustering")))
    
  }
  
  
  if(level=="SubRegion"){
    Cluster_Data <- Review_Data_By_Station %>% 
      filter(SurveySeason==Survey)%>%
      group_by(SubRegion,Year)%>%
      mutate(N_Surveys = n())%>%
      group_by(SubRegion)%>%
      summarise_at(vars(c(N_Surveys,contains("CPUV"))),mean)%>%
      ungroup()%>%
      mutate_at(vars(contains("CPUV")),round,3)%>%
      rowwise() %>% 
      mutate(CPUVTotal = sum(c_across(contains("CPUV"))))%>%
      filter(CPUVTotal>0)%>%
      column_to_rownames("SubRegion")%>%
      select(-c(CPUV_Other_Fish,CPUV_Other_Crustacean))%>%
      select_if(colSums(.)!=0)
    
    Community_Matrix <- Cluster_Data%>%
      .^(1/3)%>%
      select(contains("CPUV"))
    
    
    CM_Stand <- Community_Matrix%>%
      vegan::decostand("standardize")
    
    Clustered <- hclust(dist(CM_Stand),method=clust_method)
    
    if(plot_tree==T){print(plot(Clustered))}
    
    Clusters <- Cluster_Data %>%
      rownames_to_column("SubRegion")%>%
      add_column(Cluster =as.factor(cutree(Clustered,k=N_Clusters)))
    
    
    Station_Points_Plot <- Station_Points %>%
      ungroup()%>%
      filter(SurveySeason==Survey)%>%
      left_join(Clusters,by="SubRegion")%>%
      select(c(SurveySeason:geometry,Cluster))%>%
      filter(is.na(Cluster)==F)
    
    map_dat_plot <- EDSM_Strata %>% left_join(Clusters,by="SubRegion")%>%
      filter(SubRegion != "Rock Slough and Discovery Bay")%>%
      mutate(SubRegion = as.factor(SubRegion))%>%filter(is.na(Cluster)==F)
    
    print(ggplot()+geom_sf(data = marsh, size = .5, color = "black", fill = NA)+
            geom_sf(data=EDSM_Strata,size = 1, color = "black",fill=NA)+
            geom_sf(data=map_dat_plot,size = 1, color = NA,aes(fill=Cluster),alpha=.75)+
            geom_sf(data=Station_Points_Plot,color="black",size=1)+
            coord_sf(ylim = c(37.75, 38.65), expand = FALSE)+
            ggtitle(paste(Survey,"Station-Based Clustering")))
    
  }
  return(list(Clustered,map_dat_plot))
}





clusta <- Cluster_Catch(level="SubRegion",Survey = "SLS",N_Clusters = 4,clust_method="ward.D2")


write_csv(clusta[[2]],file="SLS_Clusta.csv")

view(clusta[[2]])


  



ggplot()+geom_sf(data = marsh, size = .5, color = "black", fill = NA)+
  geom_sf(data=EDSM_Strata,size = 1, color = "black",fill=NA)+
  geom_sf(data=Station_Points_FMWT,aes(color=Cluster),size=5)+
  coord_sf(ylim = c(37.75, 38.65), expand = FALSE)+
  ggtitle("FMWT Station-Based Clustering")
  #geom_sf_label(data=map_dat_envClust,aes(label=SubRegion))


ggplot()+geom_sf(data = marsh, size = .5, color = "black", fill = NA)+
  geom_sf(data=EDSM_Strata,size = 1, color = "black",fill=NA)+
  geom_sf(data=map_dat_FMWTClust,size = 1, color = NA,aes(fill=Cluster),alpha=.75)+
  geom_sf(data=Station_Points_FMWT,color="black",size=1)+
  coord_sf(ylim = c(37.75, 38.65), expand = FALSE)+
  ggtitle("FMWT Subregion-Based Clustering")
#geom_sf_label(data=map_dat_envClust,aes(label=SubRegion))


ggplot()+geom_sf(data = marsh, size = .5, color = "black", fill = NA)+
  geom_sf(data=map_dat_FMWTClust,size = 1, color = NA,aes(fill=N_Surveys),alpha=.75)+
  geom_sf(data=EDSM_Strata,size = 1, color = "black",fill=NA)+
  scale_fill_viridis_c()+
  geom_sf_label(data=map_dat_FMWTClust,aes(label=round(N_Surveys,0)))+
  coord_sf(ylim = c(37.75, 38.65), expand = FALSE)+
  ggtitle("FMWT Annual Effort by Subregion","Average Surveys per Year")



#====================================================================================================


STN_Dat_SubReg <- Review_Data_Tows %>% 
  filter(SurveySeason=="STN"&is.na(SubRegion)==F)%>%
  group_by(SubRegion,Year)%>%
  mutate(N_Surveys = n())%>%
  group_by(SubRegion)%>%
  summarise_at(vars(c(N_Surveys,contains("CPUV"))),mean)%>%
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



clust1 <- hclust(dist(CM_Stand),method="ward.D")

plot(clust1)

STN_Clust <- STN_Dat_SubReg %>%
  rownames_to_column("SubRegion")%>%
  add_column(Cluster =as.factor(cutree(clust1,k=7)))

Station_Points_STN <- Station_Points %>%
  ungroup()%>%
  filter(SurveySeason=="STN")%>%
  select(c(SurveySeason:geometry))

map_dat_STNClust <- EDSM_Strata %>% left_join(STN_Clust,by="SubRegion")%>%
  filter(SubRegion != "Rock Slough and Discovery Bay")%>%
  mutate(SubRegion = as.factor(SubRegion))%>%filter(is.na(Cluster)==F)

ggplot()+geom_sf(data = marsh, size = .5, color = "black", fill = NA)+
  geom_sf(data=map_dat_STNClust,size = 1, color = NA,aes(fill=Cluster),alpha=.75)+
  geom_sf(data=EDSM_Strata,size = 1, color = "black",fill=NA)+
  geom_sf(data=Station_Points_STN,color="black",size=1)+
  coord_sf(ylim = c(37.75, 38.65), expand = FALSE)+
  ggtitle("STN Subregion-Based Clustering")#+
#geom_sf_label(data=map_dat_envClust,aes(label=SubRegion))

ggplot()+geom_sf(data = marsh, size = .5, color = "black", fill = NA)+
  geom_sf(data=map_dat_STNClust,size = 1, color = NA,aes(fill=N_Surveys),alpha=.75)+
  geom_sf(data=EDSM_Strata,size = 1, color = "black",fill=NA)+
  scale_fill_viridis_c()+
  geom_sf_label(data=map_dat_STNClust,aes(label=round(N_Surveys,0)))+
  coord_sf(ylim = c(37.75, 38.65), expand = FALSE)+
  ggtitle("STN Annual Effort by Subregion","Average Surveys per Year")

#====================================================================================================


SKT_Dat_SubReg <- Review_Data_Tows %>% 
  filter(SurveySeason=="SKT"&is.na(SubRegion)==F)%>%
  group_by(SubRegion,Year)%>%
  mutate(N_Surveys = n())%>%
  group_by(SubRegion)%>%
  summarise_at(vars(c(N_Surveys,contains("CPUV"))),mean)%>%
  ungroup()%>%
  mutate_at(vars(contains("CPUV")),round,3)%>%
  rowwise() %>% 
  mutate(CPUVTotal = sum(c_across(contains("CPUV"))))%>%
  filter(CPUVTotal>0)%>%
  column_to_rownames("SubRegion")%>%
  select(-c(CPUV_Other_Fish,CPUV_Other_Crustacean,CPUVTotal,CPUV_Striped_Bass_Age_0,CPUV_Tridentiger_Spp._Age_1,
            CPUV_Tridentiger_Spp._Age_0,CPUV_Prickly_Sculpin_Age_0,CPUV_White_Sturgeon_Age_0))

Community_Matrix <- SKT_Dat_SubReg%>%
  select(contains("CPUV"))


CM_Stand <- Community_Matrix%>%
  .^(1/3) %>%
  vegan::decostand("standardize")



clust1 <- hclust(dist(CM_Stand))

plot(clust1)

SKT_Clust <- SKT_Dat_SubReg %>%
  rownames_to_column("SubRegion")%>%
  add_column(Cluster =as.factor(cutree(clust1,k=7)))


Station_Points_SKT <- Station_Points %>%
  ungroup()%>%
  filter(SurveySeason=="SKT")%>%
  select(c(SurveySeason:geometry))

map_dat_SKTClust <- EDSM_Strata %>% left_join(SKT_Clust,by="SubRegion")%>%
  filter(SubRegion != "Rock Slough and Discovery Bay")%>%
  mutate(SubRegion = as.factor(SubRegion))%>%filter(is.na(Cluster)==F)

ggplot()+geom_sf(data = marsh, size = .5, color = "black", fill = NA)+
  geom_sf(data=map_dat_SKTClust,size = 1, color = NA,aes(fill=Cluster),alpha=.75)+
  geom_sf(data=EDSM_Strata,size = 1, color = "black",fill=NA)+
  geom_sf(data=Station_Points_SKT,color="black",size=1)+
  coord_sf(ylim = c(37.75, 38.65), expand = FALSE)+
  ggtitle("SKT Subregion-Based Clustering")

ggplot()+geom_sf(data = marsh, size = .5, color = "black", fill = NA)+
  geom_sf(data=map_dat_SKTClust,size = 1, color = NA,aes(fill=N_Surveys),alpha=.75)+
  geom_sf(data=EDSM_Strata,size = 1, color = "black",fill=NA)+
  scale_fill_viridis_c()+
  geom_sf_label(data=map_dat_SKTClust,aes(label=round(N_Surveys,0)))+
  coord_sf(ylim = c(37.75, 38.65), expand = FALSE)+
  ggtitle("SKT Annual Effort by Subregion","Average Surveys per Year")
#====================================================================================================


TMM_Dat_SubReg <- Review_Data_Tows %>% 
  filter(SurveySeason=="20mm"&is.na(SubRegion)==F)%>%
  group_by(SubRegion,Year)%>%
  mutate(N_Surveys = n())%>%
  group_by(SubRegion)%>%
  summarise_at(vars(c(N_Surveys,contains("CPUV"))),mean)%>%
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



clust1 <- hclust(dist(CM_Stand),method="ward.D")

plot(clust1)

TMM_Clust <- TMM_Dat_SubReg %>%
  rownames_to_column("SubRegion")%>%
  add_column(Cluster =as.factor(cutree(clust1,k=7)))


map_dat_TMMClust <- EDSM_Strata %>% left_join(TMM_Clust,by="SubRegion")%>%
  filter(SubRegion != "Rock Slough and Discovery Bay")%>%
  mutate(SubRegion = as.factor(SubRegion))%>%filter(is.na(Cluster)==F)%>%
  mutate(N_Surveys = round(N_Surveys,0))


Station_Points_TMM <- Station_Points %>%
  ungroup()%>%
  filter(SurveySeason=="20mm")%>%
  select(c(SurveySeason:geometry))

ggplot()+geom_sf(data = marsh, size = .5, color = "black", fill = NA)+
  geom_sf(data=map_dat_TMMClust,size = 1, color = NA,aes(fill=Cluster),alpha=.75)+
  geom_sf(data=EDSM_Strata,size = 1, color = "black",fill=NA)+
  geom_sf(data=Station_Points_TMM,color="black",size=1)+
  coord_sf(ylim = c(37.75, 38.65), expand = FALSE)+
  ggtitle("20mm Subregion-Based Clustering")

ggplot()+geom_sf(data = marsh, size = .5, color = "black", fill = NA)+
  geom_sf(data=map_dat_TMMClust,size = 1, color = NA,aes(fill=N_Surveys),alpha=.75)+
  geom_sf(data=EDSM_Strata,size = 1, color = "black",fill=NA)+
  scale_fill_viridis_c(breaks = c(0,40,70,100,130))+
  geom_sf_label(data=map_dat_TMMClust,aes(label=round(N_Surveys,0)))+
  coord_sf(ylim = c(37.75, 38.65), expand = FALSE)+
  ggtitle("20mm Annual Effort by Subregion","Average Surveys per Year")



#====================================================================================================


SLS_Dat_SubReg <- Review_Data_Tows %>% 
  filter(SurveySeason=="SLS"&is.na(SubRegion)==F)%>%
  group_by(SubRegion,Year)%>%
  mutate(N_Surveys = n())%>%
  group_by(SubRegion)%>%
  summarise_at(vars(c(N_Surveys,contains("CPUV"))),mean)%>%
  ungroup()%>%
  mutate_at(vars(contains("CPUV")),round,3)%>%
  rowwise() %>% 
  mutate(CPUVTotal = sum(c_across(contains("CPUV"))))%>%
  filter(CPUVTotal>0)%>%
  column_to_rownames("SubRegion")%>%
  select(-c(CPUV_Other_Fish,CPUV_Other_Crustacean,CPUVTotal,CPUV_Crangon,CPUV_Gelatinous,CPUV_Steelhead_Age_0,
            CPUV_American_Shad_Age_1,CPUV_Striped_Bass_Age_1,CPUV_Threadfin_Shad_Age_0,CPUV_Starry_Flounder_Age_0,
            CPUV_American_Shad_Age_1,CPUV_Chinook_Salmon_Age_1,CPUV_Starry_Flounder_Age_1,CPUV_American_Shad_Age_0,
            CPUV_Tridentiger_Spp._Age_1))

Community_Matrix <- SLS_Dat_SubReg%>%
  select(contains("CPUV"))


CM_Stand <- Community_Matrix%>%
  .^(1/3) %>%
  vegan::decostand("standardize")



clust1 <- hclust(dist(CM_Stand),method="ward.D")

plot(clust1)

SLS_Clust <- SLS_Dat_SubReg %>%
  rownames_to_column("SubRegion")%>%
  add_column(Cluster =as.factor(cutree(clust1,k=7)))


map_dat_SLSClust <- EDSM_Strata %>% left_join(SLS_Clust,by="SubRegion")%>%
  filter(SubRegion != "Rock Slough and Discovery Bay")%>%
  mutate(SubRegion = as.factor(SubRegion))%>%filter(is.na(Cluster)==F)

Station_Points_SLS <- Station_Points %>%
  ungroup()%>%
  filter(SurveySeason=="SLS")%>%
  select(c(SurveySeason:geometry))

ggplot()+geom_sf(data = marsh, size = .5, color = "black", fill = NA)+
  geom_sf(data=map_dat_SLSClust,size = 1, color = NA,aes(fill=Cluster),alpha=.75)+
  geom_sf(data=EDSM_Strata,size = 1, color = "black",fill=NA)+
  geom_sf(data=Station_Points_SLS,color="black",size=1)+
  coord_sf(ylim = c(37.75, 38.65), expand = FALSE)+
  ggtitle("SLS Subregion-Based Clustering")

ggplot()+geom_sf(data = marsh, size = .5, color = "black", fill = NA)+
  geom_sf(data=map_dat_SLSClust,size = 1, color = NA,aes(fill=N_Surveys),alpha=.75)+
  geom_sf(data=EDSM_Strata,size = 1, color = "black",fill=NA)+
  scale_fill_viridis_c()+
  geom_sf_label(data=map_dat_SLSClust,aes(label=round(N_Surveys,0)))+
  coord_sf(ylim = c(37.75, 38.65), expand = FALSE)+
  ggtitle("SLS Annual Effort by Subregion","Average Surveys per Year")




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

dat_binary <- Review_Data_By_Station%>%
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




Review_Data_By_Station %>% distinct(across(c("StationCode","SurveySeason","SubRegion")))%>%
  group_by(SurveySeason)%>%
  mutate(N_Subregions = length(unique(SubRegion)))%>%
  group_by(SurveySeason,SubRegion)%>%
  mutate(N_Stations = n())%>%
  ungroup()%>%
  select(-c(StationCode,SubRegion))%>%
  group_by(SurveySeason)%>%
  mutate(Min_Stations = min(N_Stations),
         Max_Stations = max(N_Stations),
         Mean_Stations = mean(N_Stations))%>%
  select(-N_Stations)%>%
  distinct()







