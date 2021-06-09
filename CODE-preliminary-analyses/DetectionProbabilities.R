theme_set(theme_bw())
library("sf")
library("vegan")
library("tidyverse")
library("rnaturalearth")
library("rnaturalearthdata")
library("rnaturalearthhires")
library("stringr")
library("RColorBrewer")
library("spatialEco")
library("wesanderson")
library("multcomp")
library(geosphere)
select <- dplyr::select
filter <- dplyr::filter


setwd("C:/Users/40545/Documents/GitHub/pelagicsurveys")
#===============Delta Base Layer================================
marsh <- st_read(
  "CODE-spatial-analysis/hydro-delta-marsh/hydro_delta_marsh.shp")

EDSM_Strata <- st_read(
  "CODE-spatial-analysis/DSm_Subregions_UTM10NAD83/DSm_Subregions_UTM10NAD83.shp")



load("FINAL_REVIEW_DATA/CDFW_Pelagic_Review_Data.rda")




StartEnd <- read_csv("SpatialData/TowStartEndPositions.csv")%>%
  mutate_at(c("StationCode","SurveySeason"),as.factor)%>%
  pivot_wider(names_from = "Type",values_from=(c("Longitude","Latitude")))



FieldDistances <- StartEnd %>%
  mutate(TowDist = pmap_dbl(., ~
                           distm(x = c(..10, ..13), y = c(..11, ..14), fun = distHaversine))/1000,
         DistToStation_Start = pmap_dbl(., ~
                              distm(x = c(..10, ..13), y = c(..12, ..15), fun = distHaversine))/1000,
         DistToStation_End = pmap_dbl(., ~
                              distm(x = c(..11, ..14), y = c(..12, ..15), fun = distHaversine))/1000
         )%>%
  filter(DistToStation_Start<3)%>%
  filter((TowDist<3) %>% replace_na(TRUE))%>%
  filter((DistToStation_End<3) %>% replace_na(TRUE))%>%
  group_by(SurveySeason)%>%
  mutate(Station_Mean_Towdist = mean(TowDist,na.rm=T),
         Station_Mean_StartDist = mean(DistToStation_Start,na.rm=T),
         Station_Mean_EndDist = mean(DistToStation_End,na.rm=T))%>%
  ungroup()


FieldDistances %>% 
  group_by(StationCode,SurveySeason)%>%
  mutate(N_Tows = n())%>%
  ungroup()%>%
  ggplot(aes(x=DistToStation_Start,y=StationCode))+
  geom_boxplot()+facet_grid(~SurveySeason,scales="free_y",space="free_y")+
  geom_vline(aes(xintercept = Station_Mean_StartDist))+
  #scale_fill_viridis_c()+
  #scale_color_viridis_c()+
  xlab("Distance between tow start and station coordinates (km)")+
  ggtitle("Tow start distance from station (km)")

FieldDistances %>% 
  group_by(StationCode,SurveySeason)%>%
  mutate(N_Tows = n())%>%
  ungroup()%>%
  ggplot(aes(x=DistToStation_End,y=StationCode))+
  geom_boxplot()+facet_grid(~SurveySeason,scales="free_y",space="free_y")+
  geom_vline(aes(xintercept = Station_Mean_EndDist))+
  #scale_fill_viridis_c()+
  #scale_color_viridis_c()+
  xlab("Distance between tow end and station coordinates (km)")+
  ggtitle("Tow end distance from station (km)")



FieldDistances %>% 
  group_by(StationCode,SurveySeason)%>%
  mutate(N_Tows = n())%>%
  ungroup()%>%
  ggplot(aes(x=TowDist,y=StationCode))+
  geom_boxplot()+facet_grid(~SurveySeason,scales="free_y",space="free_y")+
  geom_vline(aes(xintercept = Station_Mean_Towdist))+
  #scale_fill_viridis_c()+
  #scale_color_viridis_c()+
  xlab("Tow distances (km)")+ggtitle("Tow distance over ground based on start/end coordinates (km)")


Stations <- FieldDistances %>% select(StationCode,Longitude_Station,Latitude_Station)%>%
  group_by(StationCode)%>%
  summarise_all(mean)%>%
  distinct()%>%column_to_rownames("StationCode")


Station_Distances <- distm(Stations,fun = distHaversine)%>%data.frame()

names(Station_Distances) <- rownames(Stations)
rownames(Station_Distances) <- rownames(Stations)


Problem_Children <- Station_Distances %>% rownames_to_column("station_1")%>%
  mutate(station_1 = as.character(station_1))%>%
  pivot_longer(cols=`305`:`923`,names_to="station_2",values_to="Distance")%>%
  mutate(Distance = Distance/1000)%>%
  filter(Distance >.5 & Distance < 1.25)





#PMN1 <- adonis2(tmat.g1[,19:42]~tmat.g1$Review_Stratum*tmat.g1$SurveySeason)

Review_Data_Tows %>% group_by(SurveySeason,Review_Stratum)%>%
  summarise(DeltaSmelt = mean(CPUV_Delta_Smelt_Age_0^1/3),
            stdv = sd(CPUV_Delta_Smelt_Age_0^1/3))%>%
  ggplot(aes(x=Review_Stratum,fill=SurveySeason,y=DeltaSmelt))+
  geom_bar(stat="identity",position="dodge")+
  geom_errorbar(aes(ymin=DeltaSmelt-stdv, ymax=DeltaSmelt+stdv), width=.2,
                position=position_dodge(.9)) 

#==============================================================================
#Calculate encounter probabilities by Review Stratum, Survey Season, Year and Month

Prop0 <- function(x){sum(x!=0)/length(x)}


DetectionData <- Review_Data_Tows %>%
  group_by(SurveySeason,Review_Region,Review_Stratum,Year,Month)%>%
  mutate(N_Tows = n())%>%
  group_by(SurveySeason,Review_Region,Review_Stratum,Year,Month,N_Tows)%>%
  summarize_at(vars(contains("CPUV")),Prop0)%>%
  mutate(Review_Stratum = factor(Review_Stratum,
                                 levels=c("San Pablo Bay and Carquinez Strait",
                                          "Napa River",
                                          "Suisun and Honker Bays",
                                          "Suisun Marsh",
                                          "Confluence",
                                          "South",
                                          "North and South Forks Mokelumne River",
                                          "Cache Slough",
                                          "Sacramento Mainstem",
                                          "Sacramento Ship Channel")))





names(DetectionData) <- gsub("CPUV_","",colnames(DetectionData))

DetectionData %>% filter(N_Tows>2)%>%
  ggplot(aes(x=Year,y=White_Sturgeon_Age_0,col=SurveySeason))+geom_line()+
  facet_grid(rows=vars(Month),cols=vars(Review_Region))+theme_bw()


names(DetectionData)




plot_detections <- function(taxa){
  
    Data <- DetectionData %>% select(Year,Month,Review_Region,Review_Stratum,SurveySeason,taxa,N_Tows)

    names(Data)[6] <- "Encounter Proportion"

    Data %>% filter(N_Tows>2)%>%
      ggplot(aes(x=Year,y=`Encounter Proportion`,col=SurveySeason))+geom_point()+geom_smooth(method="gam",k=1)+
      facet_grid(rows=vars(Month),cols=vars(Review_Region))+theme_bw()+ggtitle(paste(taxa))

}

plot_detections("Tridentiger_Spp._Age_0")
plot_detections("Pacific_Herring_Age_0")
plot_detections("Longfin_Smelt_Age_0")
plot_detections("American_Shad_Age_0")
plot_detections("Crangon")
plot_detections("Striped_Bass_Age_1")
plot_detections("Pacific_Herring_Age_0")
plot_detections("Delta_Smelt_Age_0")



#==========================Species ANOVAS====================================

Binary_Data <- Review_Data_Tows%>%mutate_at(vars(contains("CPUV")),~if_else(.==0,0,1))%>%ungroup()%>%
  select(-(contains("Length")|contains("Other")))
names(Binary_Data)



a_glm <-  glm(data=Binary_Data,CPUV_Delta_Smelt_Age_0~Review_Stratum+as.factor(Year),family="binomial")

summary(a_glm)

summary(glht(a_glm, mcp(Review_Stratum="Tukey")))


Plot_Comparisons <- function(taxa = "CPUV_American_Shad_Age_0", scale = "Review_Stratum",print_name="Age-0 Delta Smelt"){
  
  region_lookup <- Binary_Data %>% 
    select(Review_Region,Review_Stratum,SurveySeason)%>%
    group_by(Review_Region,Review_Stratum,SurveySeason)%>%
    summarise(N_Tows=n())
  print(region_lookup)
  
  which_surveys <- Binary_Data %>% ungroup %>%select(SurveySeason,taxa)%>%group_by(SurveySeason)%>%tally()%>%filter(n>99)%>%
    ungroup()%>%
    select(SurveySeason)%>%with(.,as.vector(SurveySeason))
  
  
  
  trim_dat <- Binary_Data %>% filter(SurveySeason==which_surveys[1]) %>% 
    select(scale,taxa,Year)%>%mutate(Year = as.factor(Year))
  
  names(trim_dat) <- c("Review_Stratum","species","Year")
  
  fit_glm <-  glm(data=trim_dat,species~Review_Stratum+Year,family="binomial")
  
  plot_dat <- effect("Review_Stratum",fit_glm)%>%data.frame()%>%mutate_if(is.numeric,round,4)%>%as_tibble()%>%
    mutate(upper = if_else(lower==0&upper==1,0,upper))%>%add_column(Survey = paste(which_surveys[1]),.before="Review_Stratum")%>%
    left_join(region_lookup,by="Review_Stratum")
  
  
  
  for(i in 2:length(which_surveys)){
    
    trim_dat <- Binary_Data %>% filter(SurveySeason==which_surveys[i]) %>% 
      select(scale,taxa)
    
    names(trim_dat) <- c("Review_Stratum","species")
    
    fit_glm <-  glm(data=trim_dat,species~Review_Stratum,family="binomial")
    
    plot_dat_a <- effect("Review_Stratum",fit_glm)%>%data.frame()%>%mutate_if(is.numeric,round,4)%>%as_tibble()%>%
      mutate(upper = if_else(lower==0&upper==1,0,upper))%>%add_column(Survey = which_surveys[i])%>%
      left_join(region_lookup,by="Review_Stratum")
    
    plot_dat <- plot_dat %>% bind_rows(plot_dat_a)
    
  }
  plot_dat <- plot_dat %>% mutate(Review_Stratum = factor(Review_Stratum,levels=c("San Pablo Bay and Carquinez Strait",
                                                                                  "Napa River",
                                                                                  "Suisun and Honker Bays",
                                                                                  "Suisun Marsh",
                                                                                  "Confluence",
                                                                                  "South",
                                                                                  "North and South Forks Mokelumne River",
                                                                                  "Cache Slough",
                                                                                  "Sacramento Mainstem",
                                                                                  "Sacramento Ship Channel")))%>%
    rename("Review Stratum" = "Review_Stratum")%>%
    rename("Review Region" = "Review_Region")
  
  print(plot_dat %>% ggplot(aes(y=`Review Stratum`,x=fit,col=`Review Region`))+theme_bw()+
          scale_color_manual(values=wes_palette(n=5, name="Zissou1"))+
          geom_vline(xintercept = 0)+xlab("Probability of Catch")+
          geom_linerange(aes(xmin=lower,xmax=upper),size=1.5)+ggtitle(paste(print_name,"catch probabilities by region and strata"))+
          geom_point(size=2.5)+
          facet_grid(cols=vars(Survey)))
  return(plot_dat)
}

names(Binary_Data)

Plot_Comparisons(taxa="CPUV_Delta_Smelt_Age_0",print_name = "Age-0 delta smelt")
Plot_Comparisons(taxa="CPUV_Delta_Smelt_Age_1",print_name = "Age-1 delta smelt")
Plot_Comparisons(taxa="CPUV_Striped_Bass_Age_0",print_name = "Age-0 striped bass")
Plot_Comparisons(taxa="CPUV_Striped_Bass_Age_1",print_name = "Age-1 striped bass")
Plot_Comparisons(taxa="CPUV_American_Shad_Age_0",print_name = "Age-0 American shad")
Plot_Comparisons(taxa="CPUV_Crangon",print_name = "Crangon")
Plot_Comparisons(taxa="CPUV_Gelatinous",print_name = "Gelatinous")


table(Review_Data_Long$CommonName,Review_Data_Long$SurveySeason)


