#====================================================================
#===Calculate CPUV

#Prepared by Michael Tillotson
#ICF
#Created April 1, 2021

#Load libraries:

require(tidyverse)
require(ggforce)
require(lubridate)
require(ggridges)


theme_set(theme_bw())


#Set working directory
setwd("C:/Users/40545/Documents/GitHub/pelagicsurveys")

load("FINAL_REVIEW_DATA/CDFW_Pelagic_Review_Data.rda")





#================Create Tabular Report of Fish Lengths by Survey===========

Length_Summary <- Review_Data_Long %>% filter(is.na(ForkLength)==F)%>% 
  group_by(CommonName)%>%
  summarise(N_Fish = n(),
            Length_Mean = mean(ForkLength,na.rm = T),
            Length_SD = sd(ForkLength,na.rm=T),
            Length_Min = min(ForkLength,na.rm = T),
            Length_Q1 =  quantile(ForkLength,na.rm = T,probs = .01),
            Length_Q5 =  quantile(ForkLength,na.rm = T,probs = .05),
            Length_Q25 = quantile(ForkLength,na.rm = T,probs = .25),
            Length_Median = median(ForkLength,na.rm = T),
            Length_Q75 = quantile(ForkLength,na.rm = T,probs = .75),
            Length_Q95 = quantile(ForkLength,na.rm = T,probs = .95),
            Length_Q99 = quantile(ForkLength,na.rm = T,probs = .99),
            Length_Max = max(ForkLength,na.rm=T)
            )%>%
  mutate_if(is.numeric,round,digits=0)%>%
  filter(Length_Min != Length_Max)


#write_csv(Length_Summary,file="TemporaryOutputs/All Surveys Length Summary CDFW vs Others.csv")


#===Length-frequencies by survey======================================
#pdf(file="TemporaryOutputs/Target Species Length Frequencies by Survey.pdf",width=16,height = 10)
All_Targets %>% 
  filter(is.na(ForkLength)==F & CommonName != "Crangon")%>%
  group_by(CommonName)%>%
  mutate(Length_95 = quantile(ForkLength,.99))%>%
  filter(ForkLength < Length_95)%>%
  ungroup()%>%
  group_by(SurveyCategory,CommonName)%>%
  mutate(LogCatch = log10(n()/17))%>%ungroup()%>%
  ggplot(aes(x=ForkLength,y=SurveyCategory,fill=LogCatch))+
  scale_fill_viridis_c(name="Log10 of mean annual catch", labels = scales::comma,alpha=.5)+
  stat_density_ridges(quantile_lines = TRUE,quantiles = c(.10,.5,.90),alpha=.5)+
  theme(legend.position = "bottom",legend.key.width=unit(4,"cm"))+
  ggtitle("Length-frequencies observed by CDFW and other fish surveys")+
  facet_wrap(~CommonName,scales = "free_x")+ylab("Survey Category")

#dev.off()

#===========================================================================
#============CPUV Comparisons===============================================
Region_Catch_Summary <- Target_Tows %>%
  mutate(SubRegion=factor(SubRegion, levels=unique(Environment_Hydrology$SubRegion)))%>%
  pivot_longer(cols=contains("CPUV"),names_to = "CommonName",names_prefix = "CPUV_",values_to="CPUV")%>%
  group_by(SurveySeason,SubRegion,CommonName)%>%
  summarise(
    CPUV_Mean = mean(CPUV,na.rm = T),
    CPUV_CV = if_else(CPUV_Mean==0,0,sd(CPUV,na.rm=T)/CPUV_Mean),
    CPUV_Min = min(CPUV,na.rm = T),
    CPUV_Q1 =  quantile(CPUV,na.rm = T,probs = .01),
    CPUV_Q5 =  quantile(CPUV,na.rm = T,probs = .05),
    CPUV_Q25 = quantile(CPUV,na.rm = T,probs = .25),
    CPUV_Median = median(CPUV,na.rm = T),
    CPUV_Q75 = quantile(CPUV,na.rm = T,probs = .75),
    CPUV_Q95 = quantile(CPUV,na.rm = T,probs = .95),
    CPUV_Q99 = quantile(CPUV,na.rm = T,probs = .99),
    CPUV_Max = max(CPUV,na.rm=T)
  )%>%
  filter(is.finite(CPUV_Mean)==T&
           CPUV_Max<1000000)



Station_Catch_Summary <- Target_Tows_Core %>% 
  mutate(SubRegion=factor(SubRegion, levels=unique(Environment_Hydrology$SubRegion)))%>%
  pivot_longer(cols=contains("CPUV"),names_to = "CommonName",names_prefix = "CPUV_",values_to="CPUV")%>%
  group_by(SurveySeason,SubRegion,StationCode,CommonName)%>%
  summarise(
    CPUV_Mean = mean(CPUV,na.rm = T),
    CPUV_CV = if_else(CPUV_Mean==0,0,sd(CPUV,na.rm=T)/CPUV_Mean),
    CPUV_Min = min(CPUV,na.rm = T),
    CPUV_Q1 =  quantile(CPUV,na.rm = T,probs = .01),
    CPUV_Q5 =  quantile(CPUV,na.rm = T,probs = .05),
    CPUV_Q25 = quantile(CPUV,na.rm = T,probs = .25),
    CPUV_Median = median(CPUV,na.rm = T),
    CPUV_Q75 = quantile(CPUV,na.rm = T,probs = .75),
    CPUV_Q95 = quantile(CPUV,na.rm = T,probs = .95),
    CPUV_Q99 = quantile(CPUV,na.rm = T,probs = .99),
    CPUV_Max = max(CPUV,na.rm=T)
  )

Station_Catch_Summary%>%
  filter(CommonName == unique(CommonName)[10]&
           is.na(SubRegion)==F)%>%
  ggplot(aes(y=SubRegion,x=CPUV_Mean,fill=StationCode))+geom_bar(stat="identity")+
  facet_wrap_paginate(~SurveySeason,ncol=5,nrow=1)+
  ggtitle(paste(unique(Station_Catch_Summary$CommonName)[10]))





#=====================Age-determination plots=======================================


ct1 <- Core_Targets %>% mutate(CommonName = as.factor(CommonName))%>%
  mutate(CommonName = recode(CommonName,
                             "Shimofuri_Goby"= "Tridentiger_Spp.",
                             "Shokihaze_Goby" = "Tridentiger_Spp."))%>%
  filter(CommonName != "Crangon" & CommonName != "No_Catch")%>%
  mutate(CommonName = droplevels(CommonName))



for (i in 1:length(unique(ct1$CommonName))){
  print(Core_Targets %>% 
          filter(is.na(ForkLength)==F & CommonName == unique(ct1$CommonName)[i])%>%
          group_by(CommonName)%>%
          mutate(Length_95 = quantile(ForkLength,.999))%>%
          filter(ForkLength < Length_95)%>%
          ungroup()%>%
          group_by(Month,CommonName,SurveySeason)%>%
          mutate(LogCatch = log10(n()/17))%>%ungroup()%>%
          ggplot(aes(x=ForkLength,y=as.factor(Month),fill=LogCatch))+ylab("Month")+
          stat_density_ridges(quantile_lines = TRUE,quantiles = c(.10,.5,.90),alpha=.5)+
          theme(legend.position = "bottom",legend.key.width=unit(4,"cm"))+
          ggtitle(paste(unique(ct1$CommonName)[i]," Age Separation"))+scale_fill_viridis_c()+
          facet_wrap(~SurveySeason))
  
}




pdf(file="TemporaryOutputs/Age Distinction Plots_FullRange.pdf",width=12,height = 10)
for (i in 1:(length(unique(ct1$CommonName)))){
  print(ct1 %>%
          filter(is.na(ForkLength)==F & CommonName == unique(ct1$CommonName)[i])%>%
          mutate(CommonName = factor(CommonName))%>%
          group_by(CommonName)%>%
          ungroup()%>%
          group_by(Month,CommonName)%>%
          mutate(LogCatch = log10(n()/17))%>%ungroup()%>%
          ggplot(aes(x=ForkLength,y=as.factor(Month),fill=LogCatch))+ylab("Month")+
          stat_density_ridges(quantile_lines = TRUE,quantiles = c(.10,.5,.90),alpha=.5)+
          theme(legend.position = "bottom",legend.key.width=unit(4,"cm"))+
          ggtitle(paste(unique(ct1$CommonName)[i]," Age Separation"))+scale_fill_viridis_c())
}

dev.off()



(ct1 %>%
    filter(is.na(ForkLength)==F & CommonName == "White_Sturgeon")%>%
    group_by(Month,CommonName)%>%
    mutate(LogCatch = log10(n()/17))%>%ungroup()%>%
    ggplot(aes(x=ForkLength,fill=LogCatch))+ylab("Month")+
    geom_histogram()+facet_wrap(~Month,scales = "free")+
    theme(legend.position = "bottom",legend.key.width=unit(4,"cm"))+
  scale_fill_viridis_c())+
  xlim(c(0,500))+xlim(c(0,100))










#Target species relative to all >100


pdf("TemporaryOutputs/All vs Target Species.pdf",width =16, height = 11)
Long_Master %>% 
  group_by(CommonName,SurveySeason,Target)%>%
  summarise(SurveyCatch = (n()))%>%ungroup()%>%
  group_by(CommonName,Target)%>%
  mutate(TotalCatch= sum(SurveyCatch))%>%filter(TotalCatch>100)%>%
  arrange(-TotalCatch)%>%ungroup()%>%
  mutate(CommonName = factor(CommonName,levels=unique(CommonName)),
         `Focal Species` = Target)%>%
  ggplot(aes(y=CommonName,x=SurveyCatch,fill=`Focal Species`))+
  geom_bar(stat="identity")+xlab("Total catch since 2002")+xlim(c(0,610000))+
  geom_text(aes(y=1,x=150000,label="*1.5 Million"))


Long_Master %>% 
  group_by(CommonName,SurveySeason,Target)%>%
  summarise(RawSurvCatch = n())%>%
  mutate(SurveyCatch = log10(RawSurvCatch))%>%ungroup()%>%
  arrange(-RawSurvCatch)%>%ungroup()%>%
  mutate(CommonName = factor(CommonName,levels=unique(CommonName)),
         `Focal Species` = Target)%>%
  group_by(CommonName,Target)%>%
  mutate(TotalCatch= sum(SurveyCatch),
         filt = sum(RawSurvCatch))%>%
  filter(filt>100)%>%
  ggplot(aes(y=CommonName,x=SurveyCatch,fill=`Focal Species`))+facet_grid(~SurveySeason)+
  geom_bar(stat="identity")+xlab("Log10 of total catch since 2002")

dev.off()





Target_Tows
