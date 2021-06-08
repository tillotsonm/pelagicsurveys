library("vegan")
library("lubridate")
library("gbm")
library("dismo")

theme_set(theme_bw())
select <- dplyr::select
filter <- dplyr::filter

setwd("C:/Users/40545/Documents/GitHub/pelagicsurveys")

load("FINAL_REVIEW_DATA/CDFW_Pelagic_Review_Data.rda")


#===============================================================================================

FMWT_PCA_Input <- Review_Data_Tows %>% 
  filter(SurveySeason=="FMWT" & is.na(SubRegion)==F) %>%
  group_by(Review_Stratum,Year)%>%
  summarise_at(vars(contains("CPUV")),mean,na.rm=T)%>%
  ungroup()%>%
  select((contains("CPUV")))%>%
  .^(1/3) %>%
  vegan::decostand("standardize")%>%
  select_if(~ !any(is.na(.)))

FMWT_PCA <- prcomp(FMWT_PCA_Input)

biplot(FMWT_PCA)

Review_Data_Tows %>% 
  filter(SurveySeason=="FMWT" & is.na(SubRegion)==F) %>%
  group_by(Review_Stratum,Year)%>%
  summarise_at(vars(c(contains("CPUV"),Salinity)),mean,na.rm=T)%>%
  ungroup()%>%
  mutate(Salinity = round(Salinity,1))%>%
  select(-contains("Length"))%>%
  add_column(PC1 = FMWT_PCA$x[,1])%>%
  add_column(PC2 = FMWT_PCA$x[,2])%>%
  ggplot(aes(x=PC1,y=PC2,fill=Review_Stratum))+
  stat_ellipse(geom = "polygon", alpha = 1/2, aes(fill = Review_Stratum))+
  geom_text(aes(col=Salinity,label=Salinity))+
  scale_color_viridis_c(option="magma")+
  scale_fill_viridis_d()




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



names(Review_Data_By_Station)






Review_Data_By_Station[,22]

summary(brt_PC1)

gbm.plot(brt_PC1)


plot(dat_binary$Salinity,dat_binary$PC2)

#==============================================================================================

dat_binary <- Review_Data_By_Station%>%
  filter(is.na(SubRegion)==F) %>%
  mutate(TowDepth = if_else(is.na(TowDepth)==F&TowDepth<0,0,TowDepth))%>%
  group_by(StationCode,Year,Month,SurveySeason,Region,SubRegion,Review_Stratum,Station_Latitude,Station_Longitude)%>%
  summarise_at(vars(c(contains("CPUV"),Salinity,Secchi,Temperature,Depth,TowDepth)),mean,na.rm=T)%>%
  ungroup()%>%
  mutate(Salinity = round(Salinity,1))%>%
  mutate_at(vars(contains("CPUV")),~if_else(.==0,0,1))%>%
  data.frame()


dat_cpuv <- Review_Data_By_Station%>%
  filter(is.na(SubRegion)==F) %>%
  mutate(TowDepth = if_else(is.na(TowDepth)==F&TowDepth<0,0,TowDepth))%>%
  group_by(StationCode,Year,Month,SurveySeason,Region,SubRegion,Review_Stratum,Station_Latitude,Station_Longitude)%>%
  summarise_at(vars(c(contains("CPUV"),Salinity,Secchi,Temperature,Depth,TowDepth)),mean,na.rm=T)%>%
  ungroup()%>%
  mutate(Salinity = round(Salinity,1))%>%
  data.frame()

dat_binary$Salinity[is.nan(dat_binary$Salinity)==T] <- NA
dat_cpuv$Salinity[is.nan(dat_binary$Salinity)==T] <- NA

names(dat_binary)




#A function that compares the performance of BRT models with many variables to 
#simple models with only season and strata.
#

test_strata <- function(response_taxa,
                        step_size_p_c=50,learning_rate_p_c=.1,
                        step_size_c_c=100,learning_rate_c_c=.25,
                        step_size_p_s=50,learning_rate_p_s=.1,
                        step_size_c_s=100,learning_rate_c_s=.1
                        ){
  
 response_location <- which(names(dat_cpuv)==response_taxa)
 
 print(response_location)
  
 #Simple model including SurveySeason and Review_Stratum
 
 #Presence/absence
 print("fitting P_S")
 presabs_strata <- gbm.step(data=data.frame(dat_binary),
                            gbm.x = c(4,7),
                            gbm.y= response_location,
                            family="bernoulli",
                            step.size = step_size_p_s,
                            learning.rate = learning_rate_p_s  
 )
 #CPUV
 print("fitting C_S") 
 cpuv_strata <- gbm.step(data=data.frame(dat_cpuv),
                         gbm.x = c(4,7),
                         gbm.y= response_location,
                         family="gaussian",
                         step.size = step_size_c_s,
                         learning.rate = learning_rate_c_s
 ) 
 
 
  #Complex model including temperature, salinity, depth, secchi, SurveySeason,
  #longitude and latitude
 

  
 #Presence/absence
 print("fitting P_C")
 presabs_complex <- gbm.step(data=data.frame(dat_binary),
                            gbm.x = c(4,8,9,34,35,36,38),
                            gbm.y= response_location,
                            family="bernoulli",
                            step.size = step_size_p_c,
                            learning.rate = learning_rate_p_c
                            )
 #CPUV
 print("Fitting C_C")
 cpuv_complex <- gbm.step(data=data.frame(dat_cpuv),
                         gbm.x = c(4,8,9,34,35,36,38),
                         gbm.y= response_location,
                         family="gaussian",
                         step.size = step_size_c_c,
                         learning.rate = learning_rate_c_c
                          )
  
print("Done fitting")

sum_table <- data.frame(Model = c("Binary Stratum","Binary Complex","CPUV Stratum","CPUV Complex"),CV_ROC_AUC=NA,CV_Cor = NA)

print(presabs_strata$cv.statistics$cv.statistics$discrimination.mean)

sum_table[1,2] <- presabs_strata$cv.statistics$discrimination.mean
sum_table[2,2] <- presabs_complex$cv.statistics$discrimination.mean

sum_table[1,3] <-presabs_strata$cv.statistics$correlation.mean
sum_table[2,3] <-presabs_complex$cv.statistics$correlation.mean
sum_table[3,3] <-cpuv_strata$cv.statistics$correlation.mean
sum_table[4,3] <-cpuv_complex$cv.statistics$correlation.mean  
  
  
output <- list(presabs_complex,presabs_strata,cpuv_complex,cpuv_strata,sum_table)



}


test1 <- test_strata("CPUV_Delta_Smelt_Age_0",
                     step_size_p_s=50,learning_rate_p_s=.1,
                     step_size_c_s=50,learning_rate_c_s=.025,
                     step_size_p_c=50,learning_rate_p_c=.1,
                     step_size_c_c=50,learning_rate_c_c=.025)


test1[[5]]



#Delta Smelt age-0 Presence
brt_full_DS_0 <- gbm.step(data=data.frame(dat_cpuv),
                          gbm.x = c(4,8,9,34,35,36,38),
                          gbm.y= 23,
                          family="gaussian",
                          step.size = 50,
                          learning.rate = .1
)


brt_full_DS_0$cv.statistics$correlation.mean




summary(brt_full_DS_0)
gbm.plot(brt_full_DS_0)
plot.gbm(brt_full_DS_0,i.var=1,type="response")
plot.gbm(brt_full_DS_0,i.var=2,type="response")




Review_Data_By_Station %>% ggplot(aes(x=Review_Stratum,y=CPUV_Delta_Smelt_Age_0))+geom_boxplot()+
  facet_grid(rows=vars(SurveySeason))





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







