#Need All Targets
require(vegan)
require(tidyverse)
require(mgcv)

Core_Targets <- Long_Master %>% 
  filter(Year>2001&is.na(SubRegion)==F & SubRegion != "SF and Outer SP Bays" & DepthBottom > 0)%>%
  mutate(SubRegion = droplevels(SubRegion))%>%
  dplyr::rename("Conductivity" = "ConductivityTop",
                "Depth" = "DepthBottom",
                "Temperature" = "TemperatureTop")%>%
  dplyr::select(c(SurveySeason,Year,Month,SampleDate,StationCode,Temperature,Secchi,Turbidity,Depth,Conductivity,
                  Station_Latitude,Station_Longitude,Tide,Microcystis,Weather,Waves,CommonName,Catch,Volume,
                  ForkLength,Region,SubRegion,TowNumber))%>%
  mutate(Salinity = predict(lm(Salinity ~ Conductivity + I(Conductivity^2),
                               data=Environment_Hydrology),newdata = .))%>%
  filter(is.na(Salinity)==F)%>%
  filter(is.na(Station_Latitude)==F)%>%
  ungroup()%>%
  
  #Add distinctions for fish vs. macroinverts
  mutate(OrganismCategory = "Other_Fish")%>%
  mutate(OrganismCategory = if_else(CommonName %in% c("Jellyfish",
                                                      "Maeotias",
                                                      "Polyorchis",
                                                      "Comb_Jelly_Or_Sea_Goosberry",
                                                      "Pleurobrachia_Jellyfish",
                                                      "Moon_Jelly",
                                                      "Jelly_(Unid)",
                                                      "Chrysaora_Fuscensens",
                                                      "Blackfordia_Virginica",
                                                      "Scrippsia_Pacifica",
                                                      "Lens_Jellyfish"
  ),
  "Gelatinous",OrganismCategory))%>%
  mutate(OrganismCategory = if_else(CommonName %in% c("Siberian_Prawn",
                                                      "Shrimp_(Unid)",
                                                      "Crangon",
                                                      "Palaemon",
                                                      "Mud_Shrimp",
                                                      "Dungeness_Crab"),
                                    "Other_Crustacean",OrganismCategory))%>%  
  mutate(CommonName = as.factor(if_else(CommonName %in% Target_Species$CommonName,CommonName,OrganismCategory)))%>%
  select(-c(ForkLength,OrganismCategory))%>%
  group_by(SurveySeason,SampleDate,StationCode,TowNumber,CommonName)%>%
  mutate(Catch = n())%>%
  mutate(CPUV = Catch/Volume)%>%
  mutate(CPUV = round(CPUV*10000,2))%>%
  ungroup()%>%
  select(-Catch)%>%
  distinct(across(c(StationCode,SampleDate,TowNumber,CommonName,SurveySeason)),.keep_all = TRUE)%>%
  pivot_wider(values_from = CPUV,names_prefix="CPUV_",names_from = CommonName)%>%
  mutate_at(vars(contains("CPUV")), ~replace_na(., 0))%>%
  select(-c(CPUV_No_Catch))%>%
  
  #Deal with duplicated/ split SKT tows by addition
  
  group_by(StationCode,SampleDate,TowNumber,SurveySeason)%>%
  mutate_at(vars(contains("CPUV")|contains("Volume")),sum)%>%
  mutate_at(vars(c(Temperature,Salinity,Secchi,Turbidity,Depth,Conductivity)),mean,na.rm=T)%>%
  distinct()

#====================================================================================================


FMWT_Dat_Station <- Core_Targets %>% 
  mutate(CPUV_S_Gobies = CPUV_Tridentiger_Spp.+CPUV_Shimofuri_Goby+CPUV_Shokihaze_Goby)%>%
  select(-c(CPUV_Tridentiger_Spp.,
            CPUV_Shimofuri_Goby,
            CPUV_Shokihaze_Goby,
            CPUV_Other_Fish,
            CPUV_Other_Crustacean,
            CPUV_Gelatinous))%>%
  filter(SurveySeason=="FMWT")%>%
  group_by(SubRegion,StationCode)%>%
  mutate(N_Years = length(unique(Year)),
         Station_Longitude=mean(Station_Longitude))%>%
  filter(N_Years>10)%>%ungroup()%>%
  group_by(SubRegion,StationCode)%>%
  summarise_at(vars(contains("CPUV")),mean)%>%
  ungroup()%>%
  mutate_at(vars(contains("CPUV")),round,0)%>%
  rowwise() %>% 
  mutate(CPUVTotal = sum(c_across(contains("CPUV"))))%>%
  filter(CPUVTotal>0)%>%
  column_to_rownames("StationCode")

Community_Matrix <- FMWT_Dat_Station_Month%>%
  select(contains("CPUV"))


CM_Stand <- Community_Matrix%>%
  vegan::decostand("standardize")

subregs <-FMWT_Dat_Station_Month$SubRegion


nmds_test <- vegan::metaMDS(Community_Matrix,k=2)

vegan::stressplot(nmds_test)

vegan::ordiplot(nmds_test,type="n",xlim=c(-1.5,1.5))
vegan::ordihull(nmds_test,groups=subregs,draw="polygon",col="grey90",label=F)
vegan::orditorp(nmds_test,display="species",col="red",air=0.01)


vegan::ordiplot(nmds_test,type="n")
vegan::ordihull(nmds_test,groups=subregs,draw="polygon",col="grey90",label=T)
vegan::orditorp(nmds_test,display="species",col="red",air=0.01)



clust1 <- hclust(dist(CM_Stand))

plot(clust1)

FMWT_Dat_Station_Month %>% add_column(Cluster =as.factor(cutree(clust1,k=26)))%>%
  ggplot(aes(x=Cluster))+geom_bar()

#====================================================================================================


STN_Dat_Station <- Core_Targets %>% 
  mutate(CPUV_S_Gobies = CPUV_Tridentiger_Spp.+CPUV_Shimofuri_Goby+CPUV_Shokihaze_Goby)%>%
  select(-c(CPUV_Tridentiger_Spp.,
            CPUV_Shimofuri_Goby,
            CPUV_Shokihaze_Goby,
            CPUV_Other_Fish,
            CPUV_Other_Crustacean,
            CPUV_Gelatinous))%>%
  filter(SurveySeason=="STN")%>%
  group_by(SubRegion,StationCode)%>%
  mutate(N_Years = length(unique(Year)),
         Station_Longitude=mean(Station_Longitude))%>%
  filter(N_Years>10)%>%ungroup()%>%
  group_by(SubRegion,StationCode)%>%
  summarise_at(vars(contains("CPUV")),mean)%>%
  ungroup()%>%
  mutate_at(vars(contains("CPUV")),round,0)%>%
  rowwise() %>% 
  mutate(CPUVTotal = sum(c_across(contains("CPUV"))))%>%
  filter(CPUVTotal>0)%>%
  column_to_rownames("StationCode")

Community_Matrix <- STN_Dat_Station_Month%>%
  select(contains("CPUV"))


CM_Stand <- Community_Matrix%>%
  vegan::decostand("standardize")

subregs <-STN_Dat_Station_Month$SubRegion


nmds_test <- vegan::metaMDS(Community_Matrix,k=2)

nmds_test$stress

vegan::stressplot(nmds_test)

vegan::ordiplot(nmds_test,type="n",xlim=c(-1.5,1.5))
vegan::ordihull(nmds_test,groups=subregs,draw="polygon",col="grey90",label=F)
vegan::orditorp(nmds_test,display="species",col="red",air=0.01)


vegan::ordiplot(nmds_test,type="n")
vegan::ordihull(nmds_test,groups=subregs,draw="polygon",col="grey90",label=T)
vegan::orditorp(nmds_test,display="species",col="red",air=0.01)



clust1 <- hclust(dist(CM_Stand))

plot(clust1)

FMWT_Dat_Station_Month %>% add_column(Cluster =as.factor(cutree(clust1,k=26)))%>%
  ggplot(aes(x=Cluster))+geom_bar()


