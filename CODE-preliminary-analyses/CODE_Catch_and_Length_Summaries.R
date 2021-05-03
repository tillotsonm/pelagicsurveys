

#Generate high level catch and length summary figures
setwd("C:/Users/40545/Documents/GitHub/pelaagicsurveys")


load("FINAL_REVIEW_DATA/CDFW_Pelagic_Review_Data.rda")



Survey_Station_CPUV <- Review_Data_Stations %>% select(contains("CPUV"))

Survey_Station_Environment <- Review_Data_Stations %>% select(contains("CPUV"))
  

CPUV_Table_Stand <- (CPUV_Table^(1/3))%>%vegan::decostand(method="standardize")


PCA_Test <- prcomp(CPUV_Table_Stand)

biplot(PCA_Test)

plot_regions <- Review_Data_Tows%>%
  filter(is.na(Region)==F)%>%
  group_by(Year,Month,Region)%>%
  mutate_at(vars(contains("CPUV")), ~sum(.,na.rm=T))%>%
  mutate_at(vars(contains("Length")),~mean(.,na.rm=T))%>%
  ungroup()%>%
  distinct(across(c("Year","Month","Region")),.keep_all = T)


plot_regions %>% ggplot(aes(x=Month,y=Mean_Length_Prickly_Sculpin_Age_0,col=as.factor(Year)))+xlim(c(1,12))+geom_point()+
  geom_line()+facet_wrap(~Region)+scale_color_viridis_d()+theme_bw()


Review_Data_Stations %>% ggplot(aes(x=Station_Longitude,y=Station_Latitude,col=Salinity_CV))+
  geom_point(size=3,alpha=.5)+
  scale_color_viridis_c()+facet_wrap(~SurveySeason)


plot(Review_Data_Stations$Station_Longitude,Review_Data_Stations$Salinity_CV,col=Review_Data_Stations$SurveySeason)

Review_Data_Stations %>% ggplot(aes(x=Station_Longitude,y=Station_Latitude,col=Salinity_CV))+
  geom_point(size=3,alpha=.5)+
  scale_color_viridis_c()


#===========================ALL CPUV PLOTS================================


plot_regions %>% ggplot(aes(x=Month,y=CPUV_Northern_Anchovy_Age_0,fill=as.factor(Year)))+xlim(c(1,12))+geom_area()+
  facet_wrap(~Region)+scale_fill_viridis_d()+theme_bw()+labs(fill="Year")


plot_regions %>% ggplot(aes(x=Month,y=CPUV_Pacific_Herring_Age_0,fill=as.factor(Year)))+xlim(c(1,12))+geom_area()+
  facet_wrap(~Region)+scale_fill_viridis_d()+theme_bw()+labs(fill="Year")

plot_regions %>% ggplot(aes(x=Month,y=CPUV_Striped_Bass_Age_0,fill=as.factor(Year)))+xlim(c(1,12))+geom_area()+
  facet_wrap(~Region)+scale_fill_viridis_d()+theme_bw()+labs(fill="Year")

plot_regions %>% ggplot(aes(x=Month,y=CPUV_Striped_Bass_Age_1,fill=as.factor(Year)))+xlim(c(1,12))+geom_area()+
  facet_wrap(~Region)+scale_fill_viridis_d()+theme_bw()+labs(fill="Year")


plot_regions %>% ggplot(aes(x=Month,y=CPUV_Delta_Smelt_Age_0,fill=as.factor(Year)))+xlim(c(1,12))+geom_area()+
  facet_wrap(~Region)+scale_fill_viridis_d()+theme_bw()+labs(fill="Year")

plot_regions %>% ggplot(aes(x=Month,y=CPUV_Delta_Smelt_Age_1,fill=as.factor(Year)))+xlim(c(1,12))+geom_area()+
  facet_wrap(~Region)+scale_fill_viridis_d()+theme_bw()+labs(fill="Year")


plot_regions %>% ggplot(aes(x=Month,y=CPUV_American_Shad_Age_0,fill=as.factor(Year)))+xlim(c(1,12))+geom_area()+
  facet_wrap(~Region)+scale_fill_viridis_d()+theme_bw()+labs(fill="Year")


plot_regions %>% ggplot(aes(x=Month,y=CPUV_American_Shad_Age_1,fill=as.factor(Year)))+xlim(c(1,12))+geom_area()+
  facet_wrap(~Region)+scale_fill_viridis_d()+theme_bw()+labs(fill="Year")

plot_regions %>% ggplot(aes(x=Month,y=CPUV_Tridentiger_Spp._Age_0,fill=as.factor(Year)))+xlim(c(1,12))+geom_area()+
  facet_wrap(~Region)+scale_fill_viridis_d()+theme_bw()+labs(fill="Year")

plot_regions %>% ggplot(aes(x=Month,y=CPUV_White_Catfish_Age_0,fill=as.factor(Year)))+xlim(c(1,12))+geom_area()+
  facet_wrap(~Region)+scale_fill_viridis_d()+theme_bw()+labs(fill="Year")

plot_regions %>% ggplot(aes(x=Month,y=CPUV_Starry_Flounder_Age_0,fill=as.factor(Year)))+xlim(c(1,12))+geom_area()+
  facet_wrap(~Region)+scale_fill_viridis_d()+theme_bw()+labs(fill="Year")

plot_regions %>% ggplot(aes(x=Month,y=CPUV_Starry_Flounder_Age_1,fill=as.factor(Year)))+xlim(c(1,12))+geom_area()+
  facet_wrap(~Region)+scale_fill_viridis_d()+theme_bw()+labs(fill="Year")

plot_regions %>% ggplot(aes(x=Month,y=CPUV_Longfin_Smelt_Age_0,fill=as.factor(Year)))+xlim(c(1,12))+geom_area()+
  facet_wrap(~Region)+scale_fill_viridis_d()+theme_bw()+labs(fill="Year")

plot_regions %>% ggplot(aes(x=Month,y=CPUV_Longfin_Smelt_Age_1,fill=as.factor(Year)))+xlim(c(1,12))+geom_area()+
  facet_wrap(~Region)+scale_fill_viridis_d()+theme_bw()+labs(fill="Year")

plot_regions %>% ggplot(aes(x=Month,y=CPUV_White_Sturgeon_Age_0,fill=as.factor(Year)))+xlim(c(1,12))+geom_area()+
  facet_wrap(~Region)+scale_fill_viridis_d()+theme_bw()+labs(fill="Year")


plot_regions %>% ggplot(aes(x=Month,y=CPUV_Chinook_Salmon_Age_0,fill=as.factor(Year)))+xlim(c(1,12))+geom_area()+
  facet_wrap(~Region)+scale_fill_viridis_d()+theme_bw()+labs(fill="Year")

plot_regions %>% ggplot(aes(x=Month,y=CPUV_Chinook_Salmon_Age_1,fill=as.factor(Year)))+xlim(c(1,12))+geom_area()+
  facet_wrap(~Region)+scale_fill_viridis_d()+theme_bw()+labs(fill="Year")

plot_regions %>% ggplot(aes(x=Month,y=CPUV_Steelhead_Age_0,fill=as.factor(Year)))+xlim(c(1,12))+geom_area()+
  facet_wrap(~Region)+scale_fill_viridis_d()+theme_bw()+labs(fill="Year")

plot_regions %>% ggplot(aes(x=Month,y=CPUV_Threadfin_Shad_Age_0,fill=as.factor(Year)))+xlim(c(1,12))+geom_area()+
  facet_wrap(~Region)+scale_fill_viridis_d()+theme_bw()+labs(fill="Year")

plot_regions %>% ggplot(aes(x=Month,y=CPUV_Threadfin_Shad_Age_1,fill=as.factor(Year)))+xlim(c(1,12))+geom_area()+
  facet_wrap(~Region)+scale_fill_viridis_d()+theme_bw()+labs(fill="Year")

plot_regions %>% ggplot(aes(x=Month,y=CPUV_Crangon,fill=as.factor(Year)))+xlim(c(1,12))+geom_area()+
  facet_wrap(~Region)+scale_fill_viridis_d()+theme_bw()+labs(fill="Year")


plot_regions %>% ggplot(aes(x=Month,y=CPUV_Other_Crustacean,fill=as.factor(Year)))+xlim(c(1,12))+geom_area()+
  facet_wrap(~Region)+scale_fill_viridis_d()+theme_bw()+labs(fill="Year")

plot_regions %>% ggplot(aes(x=Month,y=CPUV_Gelatinous,fill=as.factor(Year)))+xlim(c(1,12))+geom_area()+
  facet_wrap(~Region)+scale_fill_viridis_d()+theme_bw()+labs(fill="Year")

plot_regions %>% ggplot(aes(x=Month,y=CPUV_Other_Fish,fill=as.factor(Year)))+xlim(c(1,12))+geom_area()+
  facet_wrap(~Region)+scale_fill_viridis_d()+theme_bw()+labs(fill="Year")

















DS0GAM <- gam(CPUV_Delta_Smelt_Age_0~s(Salinity,by=Region)+s(Station_Longitude),
              data=Review_Data_Tows,
              family="negbin")

hist(Review_Data_Tows$CPUV_Prickly_Sculpin_Age_0)





scales::asn_trans(Review_Data_Tows$CPUV_Delta_Smelt_Age_0)



