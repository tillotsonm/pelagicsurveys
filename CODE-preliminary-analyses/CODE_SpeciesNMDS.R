

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







