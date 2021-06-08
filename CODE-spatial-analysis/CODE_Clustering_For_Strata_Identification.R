#====================================================================
#===Code to for clustering survey stations and subregions by catch and 
#environmental characteristics

#Prepared by Michael Tillotson
#ICF
#Created April 4, 2021
#Updated May 20, 2021

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

#===============Spatial Layers================================
Review_Enviro_Hydro <- ungroup(Review_Enviro_Hydro)

region_lookup <- Review_Data_By_Station %>% 
  select(Review_Region,Region,SubRegion,Review_Stratum)%>%
  distinct()%>%
  mutate(Review_Stratum = recode(Review_Stratum,"Napa River*" = "Napa River"))

#Read in cluster-based regions and strata

Review_Strata <- read_csv("CODE-data-tidying/Review_Strata.csv",col_types = "fff")

marsh <- st_read(
  "CODE-spatial-analysis/hydro-delta-marsh/hydro_delta_marsh.shp")

EDSM_Strata_Temp <- st_read(
  "CODE-spatial-analysis/DSm_Subregions_UTM10NAD83/DSm_Subregions_UTM10NAD83.shp")

EDSM_Strata<- EDSM_Strata_Temp %>%
  left_join(region_lookup,by=c("Region","SubRegion"))%>%
  mutate(Review_Stratum = replace_na(Review_Stratum,"South"))%>%
  mutate(Review_Region = replace_na(Review_Region,"South"))%>%  
  select(-c(OBJECTID,OBJECTID_1,Shape_Leng,Shape_Area))

Station_Points <-Review_Data_By_Station%>%
  select(SurveySeason,Review_Region,SubRegion,Region,Review_Stratum,StationCode,Station_Longitude,Station_Latitude)%>%
  distinct(across(c("SurveySeason","StationCode")),.keep_all=T)%>%
  st_as_sf( coords = c("Station_Longitude", "Station_Latitude"), 
            crs = 4326, agr = "constant")



#========Plot candidate strata========================================================

ggplot()+geom_sf(data = marsh, size = .5, color = "black", fill = NA)+
  geom_sf(data=EDSM_Strata,size = 1, color = "black",aes(fill=Review_Stratum),alpha=.5)+
  coord_sf(ylim = c(37.75, 38.65), expand = FALSE)

#========Plot regions strata========================================================

ggplot()+geom_sf(data = marsh, size = .5, color = "black", fill = NA)+
  geom_sf(data=EDSM_Strata,size = 1, color = "black",aes(fill=Region))+
  coord_sf(ylim = c(37.75, 38.65), expand = FALSE)
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
    
    return(list(Clustered,Station_Points_Plot))
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
    
    Euclidean <- vegdist(CM_Stand,method="euclidean")
    
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
            geom_sf(data=map_dat_plot,size = 1, color = NA,aes(fill=Cluster),alpha=.75)+
            geom_sf(data=Station_Points_Plot,color="black",size=1)+
            geom_sf(data=EDSM_Strata,size = 1, color = "black",fill=NA)+
            coord_sf(ylim = c(37.75, 38.65), expand = FALSE)+
            ggtitle(paste(Survey,"Subregion-Based Clustering")))
    
    print(round(cor(Euclidean,cophenetic(Clustered)),2))
    
    return(list(Clustered,map_dat_plot,Euclidean))
  }
  
}



SLS_clusta <- Cluster_Catch(level="SubRegion",Survey = "SLS",N_Clusters = 9,clust_method="ward.D2",plot_tree = F)



SKT_clusta <- Cluster_Catch(level="SubRegion",Survey = "SKT",N_Clusters = 9,clust_method="ward.D2",plot_tree = F)
TMM_clusta <- Cluster_Catch(level="SubRegion",Survey = "20mm",N_Clusters = 9,clust_method="ward.D2",plot_tree = F)
STN_clusta <- Cluster_Catch(level="SubRegion",Survey = "STN",N_Clusters = 9,clust_method="ward.D2",plot_tree = F)
FMWT_clusta <- Cluster_Catch(level="SubRegion",Survey = "FMWT",N_Clusters = 9,clust_method="ward.D2",plot_tree = F)




#================================================================================================================
SLS_clusta_st <- Cluster_Catch(level="Station",Survey = "SLS",N_Clusters = 9,clust_method="ward.D2",plot_tree = F)
SKT_clusta_st <- Cluster_Catch(level="Station",Survey = "SKT",N_Clusters = 9,clust_method="ward.D2",plot_tree = F)
TMM_clusta_st <- Cluster_Catch(level="Station",Survey = "20mm",N_Clusters = 9,clust_method="ward.D2",plot_tree = F)
STN_clusta_st <- Cluster_Catch(level="Station",Survey = "STN",N_Clusters = 9,clust_method="ward.D2",plot_tree = F)
FMWT_clusta_st <-Cluster_Catch(level="Station",Survey = "FMWT",N_Clusters = 9,clust_method="ward.D2",plot_tree = F)


  
#==========Environmental clusters based on full environmental data================
#=================================================================================

CV <-function(x){sd(x,na.rm = T)/mean(x,na.rm=T)}
Mean <-function(x){mean(x,na.rm=T)}


#Function for clustering subregions based on environmental means and CVs
#Allows for subsetting of months to match survey dates

Cluster_Env <- function(Survey = "FMWT",Months = 9:12, plot_tree =T , 
                        level = "Station", N_Clusters = 6, clust_method = "ward.D2",
                        SubRegions = NA,trim_regions=F){
  
  Env_Points <-Review_Enviro_Hydro%>%
    filter(Year>2001&Month %in% Months)%>%
    select(Survey,Review_Region,SubRegion,StationCode,Longitude,Latitude)%>%
    distinct(across(c("Survey","StationCode")),.keep_all=T)%>%
    st_as_sf( coords = c("Longitude", "Latitude"), 
              crs = 4326, agr = "constant")
  
  Env_Dat_a <- Review_Enviro_Hydro %>%
    ungroup()%>%
    filter(Year>2001&Month %in% Months)
  
  if(trim_regions==T){
    Env_Dat_a <- Review_Enviro_Hydro %>%
    ungroup()%>%
    filter(Year>2001&Month %in% Months & SubRegion %in% SubRegions)
    
    
    Env_Points <-Review_Enviro_Hydro%>%
      filter(Year>2001&Month %in% Months & SubRegion %in% SubRegions)%>%
      select(Survey,Review_Region,SubRegion,StationCode,Longitude,Latitude,)%>%
      distinct(across(c("Survey","StationCode")),.keep_all=T)%>%
      st_as_sf( coords = c("Longitude", "Latitude"), 
                crs = 4326, agr = "constant")
    
    
    }
  
  Env_Dat <- Env_Dat_a%>%
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
  
  
  CM_Stand <- Community_Matrix %>%
    vegan::decostand("standardize")
  
  Euclidean <- vegdist(CM_Stand,method="euclidean")
  
  clust1 <- hclust(dist(CM_Stand),method=clust_method)
  
  if(plot_tree==T){print(plot(clust1))}
  
  Env_Clust <- Env_Dat %>%
    add_column(Cluster =as.factor(cutree(clust1,k=N_Clusters)))
  
  
  map_dat_envClust <- EDSM_Strata %>% left_join(Env_Clust,by="SubRegion")%>%
    mutate(SubRegion = as.factor(SubRegion))%>%filter(is.na(Cluster)==F)
  
  print(ggplot()+geom_sf(data = marsh, size = .5, color = "black", fill = NA)+
          geom_sf(data=map_dat_envClust,size = 1, color = "darkgreen",aes(fill=Cluster),alpha=.75)+
          geom_sf(data=Env_Points,color="black")+
          coord_sf(ylim = c(37.75, 38.65), expand = FALSE)+
          ggtitle(paste(Survey,"Environmental Clustering")))
  
  print(round(cor(Euclidean,cophenetic(clust1)),2))
  
  return(list(clust1,Env_Clust))
}




#================================================================================================================
SLS_clusta_env <- Cluster_Env(Survey = "SLS",Months=c(1:3),N_Clusters = 9,clust_method="ward.D2",plot_tree = F,
                              trim_regions = T, SubRegions = unique(as.character(SLS_clusta[[2]]$SubRegion)))
SKT_clusta_env <- Cluster_Env(Survey = "SKT",Months=c(1:5),N_Clusters = 9,clust_method="ward.D2",plot_tree = F,
                              trim_regions = T, SubRegions = unique(as.character(SKT_clusta[[2]]$SubRegion)))
TMM_clusta_env <- Cluster_Env(Survey = "20mm",Months=c(3:7),N_Clusters = 9,clust_method="ward.D2",plot_tree = F,
                              trim_regions = T, SubRegions = unique(as.character(TMM_clusta[[2]]$SubRegion)))
STN_clusta_env <- Cluster_Env(Survey = "STN",Months=c(6:8),N_Clusters = 9,clust_method="ward.D2",plot_tree = F,
                              trim_regions = T, SubRegions = unique(as.character(STN_clusta[[2]]$SubRegion)))
FMWT_clusta_env <-Cluster_Env(Survey = "FMWT",Months=c(9:12),N_Clusters = 9,clust_method="ward.D2",plot_tree = F,
                              trim_regions = T, SubRegions = unique(as.character(FMWT_clusta[[2]]$SubRegion)))


#================Join Candidate Clusters from catch-station, catch-subregion and environment-subregion===========


STN_All_Strata <- Review_Data_By_Station%>%
  select(SurveySeason,StationCode,Station_Longitude,Station_Latitude)%>%
  distinct(across(c("SurveySeason","StationCode")),.keep_all=T)%>%
  right_join(data.frame(STN_clusta_st[[2]]),by=c("StationCode","SurveySeason"))%>% 
  select(-geometry)%>%
  left_join(data.frame(STN_clusta[[2]]),by=c("SubRegion","Review_Region"))%>%
  mutate(Catch_Station_Cluster = Cluster.x,
         Catch_Subregion_Cluster = Cluster.y,
         .after=SubRegion)%>%
  select(-c(Cluster.y,Cluster.x))%>%
  left_join(STN_clusta_env[[2]],by="SubRegion")%>%
  relocate(Cluster,.after=Catch_Subregion_Cluster)%>%
  relocate(Review_Region,.before=Review_Region)%>%
  relocate(geometry,.after = last_col())%>%
  rename("Env_Cluster" = "Cluster")

#==============================Environmenta variable figures===============================
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



ggplot()+geom_sf(data = marsh, size = .5, color = "black", fill = NA)+
  geom_sf(data=EDSM_Strata,size = 1, color = "darkgreen",aes(fill=Review_Stratum),alpha=.75)+
  ggtitle("Final Review Strata")

ggplot()+geom_sf(data = marsh, size = .5, color = "black", fill = NA)+
  geom_sf(data=EDSM_Strata,size = 1, color = "darkgreen",aes(fill=Review_Region),alpha=.75)+
  ggtitle("Final Review Regions")
