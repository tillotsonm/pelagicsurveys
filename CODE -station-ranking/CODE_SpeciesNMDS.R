#Need All Targets
require(vegan)
require(tidyverse)
require(mgcv)

load("MASTER_Data/MASTER_Env_Hydro.rda")
load("MASTER_Data/MASTER_Long_Format.rda")

Target_Species <- data.frame(CommonName = c("American_Shad",
                                            "Northern_Anchovy",
                                            "Pacific_Herring",
                                            "Starry_Flounder",
                                            "Striped_Bass",
                                            "Threadfin_Shad",
                                            "White_Sturgeon",
                                            "Crangon",
                                            "Chinook_Salmon",
                                            "Delta_Smelt",
                                            "Longfin_Smelt",
                                            "Steelhead",
                                            "White_Catfish",
                                            "Shokihaze_Goby",
                                            "Shimofuri_Goby",
                                            "Tridentiger_Spp.",
                                            "Prickly_Sculpin",
                                            "No_Catch"
))



Core_Targets <- Long_Master %>% filter(Year>2001)%>%
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

save(Core_Targets, file = "MASTER_Data/CDFW_Tows_Target_Species.rda")


for (i in 1:length(unique(Core_Targets$CommonName))){
print(Core_Targets %>% 
  filter(is.na(ForkLength)==F & CommonName == unique(Core_Targets$CommonName)[i])%>%
  group_by(CommonName)%>%
  mutate(Length_95 = quantile(ForkLength,.99))%>%
  filter(ForkLength < Length_95)%>%
  ungroup()%>%
  group_by(Month,CommonName,SurveySeason)%>%
  mutate(LogCatch = log10(n()/17))%>%ungroup()%>%
  ggplot(aes(x=ForkLength,y=as.factor(Month),fill=LogCatch))+
  stat_density_ridges(quantile_lines = TRUE,quantiles = c(.10,.5,.90),alpha=.5)+
  theme(legend.position = "bottom",legend.key.width=unit(4,"cm"))+
  ggtitle(paste(unique(Core_Targets$CommonName)[i]," Age Separation"))+scale_fill_viridis_c()+
    facet_wrap(~SurveySeason))

}


for (i in 1:length(unique(Core_Targets$CommonName))){
  print(Core_Targets %>% 
  filter(is.na(ForkLength)==F & CommonName == unique(Core_Targets$CommonName)[i])%>%
  group_by(CommonName)%>%
  mutate(Length_95 = quantile(ForkLength,.99))%>%
  filter(ForkLength < Length_95)%>%
  ungroup()%>%
  group_by(Month,CommonName)%>%
  mutate(LogCatch = log10(n()/17))%>%ungroup()%>%
  ggplot(aes(x=ForkLength,y=as.factor(Month),fill=LogCatch))+
  stat_density_ridges(quantile_lines = TRUE,quantiles = c(.10,.5,.90),alpha=.5)+
  theme(legend.position = "bottom",legend.key.width=unit(4,"cm"))+
  ggtitle(paste(unique(Core_Targets$CommonName)[i]," Age Separation"))+scale_fill_viridis_c())
  }


Core_Targets %>% 
  modify_at(c("StationCode","Month"),as.character)%>%
  modify_at(c("StationCode","Month"),as.factor)%>%
  group_by(StationCode,Month)%>%
  tally()

unique(Core_Targets$)