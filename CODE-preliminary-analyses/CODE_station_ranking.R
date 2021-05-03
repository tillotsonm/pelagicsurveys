#===========Code to generate station-level summary variables=========
#=================And composite rankings=============================

#Prepared by Michael Tillotson
#ICF
#Updated March 11, 2021
#Currently incomplete. Does not have CPUE calculated.
require(tidyverse)


setwd("C:/Users/40545/Documents/GitHub/pelaagicsurveys")

load("MASTER_Data/MASTER_Env_Hydro.rda")
load("MASTER_Data/MASTER_Long_Format.rda")

Strata_Volumes <- read_csv("SpatialData/Strata_Volumes.csv")%>%
  mutate(SubRegion = as.factor(SubRegion))

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
))%>%
  mutate(CommonName = as.factor(CommonName))

Long_Master <- Long_Master %>% 
  filter(Volume<15000)%>%
  mutate(Volume = Volume/1000)


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
  mutate(CommonName = as.factor(if_else(CommonName %in% Target_Species$CommonName,
                                        as.character(CommonName),
                                        as.character(OrganismCategory))))%>%
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



Core_Targets %>% group_by(SubRegion,StationCode,Month,Station_Longitude)%>%
  summarise(N = n(),
            Var_Mean = mean(Salinity),
            Var_CV = sd(Salinity)/mean(Salinity))%>%filter(N>5)%>%
  filter(Var_CV>0&Var_CV<5)%>%
  ggplot(aes(x=Station_Longitude,y=Var_CV,col=Var_Mean))+
  geom_point()+ylim(c(0,5))+scale_color_viridis_c()

Core_Targets %>% group_by(SubRegion,StationCode,Month,Station_Longitude)%>%
summarise(N = n(),
          Var_Mean = mean(Depth),
          Var_CV = sd(Depth)/mean(Depth))%>%filter(N>5)%>%
  ggplot(aes(x=Station_Longitude,y=Var_Mean,col=Var_CV))+
  geom_point()+scale_color_viridis_c()


pdf(file="TemporaryOutputs/Percent Sampled by Strata.pdf",width=16,height = 10)
#Sampled Percent of all habitat Through Years 
Core_Targets %>% ungroup()%>%filter(is.na(SubRegion)==F)%>%
  group_by(SurveySeason,Year,SubRegion)%>%
  summarise(SampledVolume = sum(Volume))%>%
  full_join(Strata_Volumes,by="SubRegion")%>%
  mutate(SampledPercent = (SampledVolume/TotalVolume)*100)%>%
  ggplot(aes(x=Year,y=SampledPercent,col=SurveySeason))+geom_line()+
  facet_wrap(~SubRegion,scales = "free_y")+
  ggtitle("Annual % of total volume sampled by survey")


#Sampled Percent of >4m  habitat Through Years 
# Core_Targets %>% ungroup()%>%filter(is.na(SubRegion)==F)%>%
#   group_by(SurveySeason,Year,SubRegion)%>%
#   summarise(SampledVolume = sum(Volume))%>%
#   full_join(Strata_Volumes,by="SubRegion")%>%
#   mutate(AvailableVolume = TotalVolume-Volume2_4m-Volume2_Surface)%>%
#   mutate(SampledPercent = (SampledVolume/AvailableVolume)*100)%>%
#   ggplot(aes(x=Year,y=SampledPercent,col=SurveySeason))+geom_line()+
#   facet_wrap(~SubRegion,scales = "free_y")+
#   ggtitle("Annual % of available (>4m) volume sampled by survey")

#==============================================================================
#==============================================================================

#Sampled Percent of all habitat Through Years 
Core_Targets %>% ungroup()%>%filter(is.na(SubRegion)==F)%>%
  group_by(SurveySeason,Month,SubRegion)%>%
  summarise(SampledVolume = mean(Volume))%>%
  full_join(Strata_Volumes,by="SubRegion")%>%
  mutate(SampledPercent = (SampledVolume/TotalVolume)*100)%>%
  ggplot(aes(x=Month,y=SampledPercent,col=SurveySeason))+geom_line()+
  facet_wrap(~SubRegion,scales = "free_y")+
  ggtitle("Monthly mean % of total volume sampled and survey")


#Sampled Percent of >4m  habitat Through Years 
# Core_Targets %>% ungroup()%>%filter(is.na(SubRegion)==F)%>%
#   group_by(SurveySeason,Month,SubRegion)%>%
#   summarise(SampledVolume = mean(Volume))%>%
#   full_join(Strata_Volumes,by="SubRegion")%>%
#   mutate(AvailableVolume = TotalVolume-Volume2_4m-Volume2_Surface)%>%
#   mutate(SampledPercent = (SampledVolume/AvailableVolume)*100)%>%
#   ggplot(aes(x=Month,y=SampledPercent,col=SurveySeason))+geom_line()+
#   facet_wrap(~SubRegion,scales = "free_y")+
#   ggtitle("Monthly mean % of available (>4m) volume sampled by survey")
dev.off()


#Create volume summary table
Core_Targets %>% ungroup()%>%filter(is.na(SubRegion)==F)%>%
  group_by(SurveySeason,Month,SubRegion,Year,Region)%>%
  summarise(SampledVolume = sum(Volume))%>%
  ungroup()%>%
  group_by(SurveySeason,Month,SubRegion,Region)%>%
  summarise(SampledVolume=mean(SampledVolume))%>%
  full_join(Strata_Volumes,by="SubRegion")%>%
  select(-c(SurfaceArea:Volume10_Bottom))%>%arrange(Month)%>%
  pivot_wider(names_from="Month",values_from=c("SampledVolume"))%>%
  write_csv(file="TemporaryOutputs/SampledVolumeSummary.csv")


pdf(file="TemporaryOutputs/Tow Volume Histograms.pdf",width=16,height = 10)
Core_Targets%>%
  ggplot(aes(x=Volume))+
  geom_histogram()+facet_wrap(~SurveySeason,scales="free")+
  theme_bw()+
  xlab("Tow volume (1,000 cubic meters)")+ 
  geom_text(
    data=Core_Targets%>%group_by(SurveySeason)%>%
      mutate(MeanVolume = round(mean(Volume),2),
             Label = paste("Survey mean= ",MeanVolume))%>%
      ungroup()%>%
      distinct(across("SurveySeason"),.keep_all=T),
    mapping = aes(x = (MeanVolume), y = c(1250,3000,450,500,6200), label = Label),
    hjust   = -0.1)+
  geom_vline(data=Core_Targets%>%group_by(SurveySeason)%>%
               mutate(MeanVolume = round(mean(Volume),2)),
                      aes(xintercept = MeanVolume))
dev.off()



















Rank_Table <- Core_Targets %>% 
  group_by(SubRegion,StationCode)%>%
  mutate(N_Years = length(unique(Year)),
         Station_Longitude=mean(Station_Longitude))%>%
  filter(N_Years>10)%>%ungroup()%>%
  group_by(SubRegion,StationCode,N_Years,Station_Longitude)%>%
  summarise_at(vars(contains("CPUV")),mean)%>%
  ungroup()%>%
  mutate_at(vars(contains("CPUV")),round,0)%>%
  mutate_at(vars(contains("CPUV")),dense_rank)%>%
  rowwise() %>% 
  mutate(SumRank = sum(c_across(contains("CPUV"))))

Rank_Table%>%arrange(-SumRank)%>%
  mutate(StationCode = factor(StationCode,levels=unique(StationCode)))%>%
  ggplot(aes(y=StationCode,x=SumRank,fill=Station_Longitude))+geom_bar(stat="identity")+
  scale_fill_viridis_c()

RankTable
