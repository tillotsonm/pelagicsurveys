

#Generate high level catch and length summary figures
setwd("C:/Users/40545/Documents/GitHub/pelaagicsurveys")


load("FINAL_REVIEW_DATA/CDFW_Pelagic_Review_Data.rda")


Review_Data_Stations %>% filter(SurveySeason=="FMWT")%>%
  select(contains("CPUV"))%>%cor()



With_Cluster <- Review_Data_Stations %>% add_column(Cluster = as.vector(cutree(clusta,k=15)))

table(With_Cluster$SurveySeason,With_Cluster$Cluster)


PCA_FMWT <- prcomp(Survey_Station_CPUV,scale. = T)
summary(PCA_Test)


Survey_Station_Environment <- Review_Data_Stations %>% select(contains("CPUV"))
  

biplot(PCA_Test)



plot_regions <- Review_Data_Tows%>%
  filter(is.na(Region)==F&SurveySeason!="SLS")%>%
  mutate(SurveySeason = droplevels(SurveySeason))%>%
  group_by(Year,Month,Region)%>%
  mutate_at(vars(contains("CPUV")), ~sum(.,na.rm=T))%>%
  mutate_at(vars(contains("Length")),~mean(.,na.rm=T))%>%
  ungroup()%>%
  distinct(across(c("Year","Month","Region")),.keep_all = T)%>%
  select(-SurveySeason)%>%
  complete(Year,Month,Region)%>%
  mutate_at(vars(contains("CPUV")), ~replace(., is.na(.), 0))

plot_regions_sls <- Review_Data_Tows%>%
  filter(is.na(Region)==F)%>%
  mutate(SurveySeason = droplevels(SurveySeason))%>%
  group_by(Year,Month,Region)%>%
  mutate_at(vars(contains("CPUV")), ~sum(.,na.rm=T))%>%
  mutate_at(vars(contains("Length")),~mean(.,na.rm=T))%>%
  ungroup()%>%
  distinct(across(c("Year","Month","Region")),.keep_all = T)%>%
  select(-SurveySeason)%>%
  complete(Year,Month,Region)%>%
  mutate_at(vars(contains("CPUV")), ~replace(., is.na(.), 0))



#===========================ALL CPUV PLOTS NO SLS==============================

cpuv.plot <- function(plot.var,title.text){
  
  col.ref <- match(plot.var,names(plot_regions))  
  names(plot_regions)[col.ref] <- "Plot_This"
  
  plot_regions %>%
    ggplot(aes(x=Month,y=Plot_This,fill=as.factor(Year)))+
    facet_wrap(~Region)+scale_fill_viridis_d()+theme_bw()+labs(fill="Year")+
    scale_x_continuous(limits=c(1,12),breaks=c(1:12),expand=c(.01,0))+
    annotate("rect", xmin = 1, xmax = 6, ymin = 0, ymax = Inf,fill="purple",
             alpha = .1)+
    annotate("text", x = 1.4, y = Inf,label="SKT",vjust=1,col="purple")+
    annotate("rect", xmin = 6, xmax = 9, ymin = 0, ymax = Inf,fill="red",
             alpha = .2)+
    annotate("text", x = 6.4, y = Inf,label="STN",vjust=1,col="red")+
    geom_area()+
    annotate("rect", xmin = 3, xmax = 8, ymin = 0, ymax = Inf,col="orange",fill=NA,
             alpha = .01)+
    annotate("text", x = 3.5, y = Inf,label="20mm",vjust=1,col="orange")+
    annotate("rect", xmin = 9, xmax = 12, ymin = 0, ymax = Inf,col="black",fill=NA,
             alpha = .01)+
    annotate("text", x = 9.6, y = Inf,label="FMWT",vjust=1)+ylab("Mean CPUV")+
    
    
    ggtitle(title.text, expression(paste("catch/1,000",m^3, " by region and month - No SLS")))

}


pdf(file="TemporaryOutputs/Target Species CPUV by Month and Region.pdf",width=11,height = 6)
cpuv.plot("CPUV_Pacific_Herring_Age_0","Age-0 Pacific herring") #1

cpuv.plot("CPUV_Northern_Anchovy_Age_0","Age-0 northern anchovy") #2

cpuv.plot("CPUV_Striped_Bass_Age_0","Age-0 striped bass")#3

cpuv.plot("CPUV_Striped_Bass_Age_1","Age-1 striped bass") #4

cpuv.plot("CPUV_Delta_Smelt_Age_0","Age-0 delta smelt") #5

cpuv.plot("CPUV_Delta_Smelt_Age_1","Age-1 delta smelt") #6

cpuv.plot("CPUV_Longfin_Smelt_Age_0","Age-0 longfin smelt") #7

cpuv.plot("CPUV_Longfin_Smelt_Age_1","Age-1 longfin smelt") #8

cpuv.plot("CPUV_American_Shad_Age_0","Age-0 American shad") #9

cpuv.plot("CPUV_American_Shad_Age_1","Age-1 American shad") #10

cpuv.plot("CPUV_Threadfin_Shad_Age_0","Age-0 threadfin shad") #11

cpuv.plot("CPUV_Threadfin_Shad_Age_1","Age-1 threadfin shad") #12

cpuv.plot("CPUV_Chinook_Salmon_Age_0","Age-1 Chinook salmon") #13

cpuv.plot("CPUV_Chinook_Salmon_Age_1","Age-1 Chinook salmon") #14

cpuv.plot("CPUV_Steelhead_Age_0","Age-1 steelhead") #15

cpuv.plot("CPUV_White_Sturgeon_Age_0","Age-0 white sturgeon") #16

cpuv.plot("CPUV_White_Catfish_Age_0","Age-0 white catfish") #17

cpuv.plot("CPUV_Starry_Flounder_Age_0","Age-0 starry flounder") #18

cpuv.plot("CPUV_Starry_Flounder_Age_1","Age-1 starry flounder") #19

cpuv.plot("CPUV_Tridentiger_Spp._Age_0","Age-0 Tridentiger Spp.") #20

cpuv.plot("CPUV_Prickly_Sculpin_Age_0","Age-0 prickly sculpin") # 21

cpuv.plot("CPUV_Crangon","Crangon") #22

cpuv.plot("CPUV_Gelatinous","gelatinous species")

cpuv.plot("CPUV_Other_Crustacean","other crustacean species")

cpuv.plot("CPUV_Other_Fish","other fish species")
dev.off()


#===========================ALL CPUV PLOTS WITH SLS==============================

cpuv.plot.sls <- function(plot.var,title.text){
  
  col.ref <- match(plot.var,names(plot_regions_sls))  
  names(plot_regions_sls)[col.ref] <- "Plot_This"
  
  plot_regions_sls %>%
    ggplot(aes(x=Month,y=Plot_This,fill=as.factor(Year)))+
    facet_wrap(~Region)+scale_fill_viridis_d()+theme_bw()+labs(fill="Year")+
    scale_x_continuous(limits=c(1,12),breaks=c(1:12),expand=c(.01,0))+
    annotate("rect", xmin = 1, xmax = 6, ymin = 0, ymax = Inf,fill="purple",
             alpha = .1)+
    annotate("rect", xmin = 1, xmax = 4, ymin = 0, ymax = Inf,fill="grey",
             alpha = .5)+
    annotate("text", x = 1.4, y = Inf,label="SKT",vjust=1,col="purple")+
    annotate("rect", xmin = 6, xmax = 9, ymin = 0, ymax = Inf,fill="red",
             alpha = .2)+
    annotate("text", x = 6.4, y = Inf,label="STN",vjust=1,col="red")+
    geom_area()+
    annotate("rect", xmin = 3, xmax = 8, ymin = 0, ymax = Inf,col="orange",fill=NA,
             alpha = .01)+
    annotate("text", x = 3.5, y = Inf,label="20mm",vjust=1,col="orange")+
    annotate("rect", xmin = 9, xmax = 12, ymin = 0, ymax = Inf,col="black",fill=NA,
             alpha = .01)+
    annotate("text", x = 9.6, y = Inf,label="FMWT",vjust=1)+ylab("Mean CPUV")+
    annotate("text", x = 2, y = Inf,label="SLS",vjust=1,col="grey25")+ylab("Mean CPUV")+
    
    ggtitle(title.text, expression(paste("catch/1,000",m^3, " by region and month - with SLS")))
  
}


pdf(file="TemporaryOutputs/Target Species CPUV by Month and Region With SLS.pdf",width=11,height = 6)

cpuv.plot.sls("CPUV_Pacific_Herring_Age_0","Age-0 Pacific herring") #1

cpuv.plot.sls("CPUV_Northern_Anchovy_Age_0","Age-0 northern anchovy") #2

cpuv.plot.sls("CPUV_Striped_Bass_Age_0","Age-0 striped bass")#3

cpuv.plot.sls("CPUV_Striped_Bass_Age_1","Age-1 striped bass") #4

cpuv.plot.sls("CPUV_Delta_Smelt_Age_0","Age-0 delta smelt") #5

cpuv.plot.sls("CPUV_Delta_Smelt_Age_1","Age-1 delta smelt") #6

cpuv.plot.sls("CPUV_Longfin_Smelt_Age_0","Age-0 longfin smelt") #7

cpuv.plot.sls("CPUV_Longfin_Smelt_Age_1","Age-1 longfin smelt") #8

cpuv.plot.sls("CPUV_American_Shad_Age_0","Age-0 American shad") #9

cpuv.plot.sls("CPUV_American_Shad_Age_1","Age-1 American shad") #10

cpuv.plot.sls("CPUV_Threadfin_Shad_Age_0","Age-0 threadfin shad") #11

cpuv.plot.sls("CPUV_Threadfin_Shad_Age_1","Age-1 threadfin shad") #12

cpuv.plot.sls("CPUV_Chinook_Salmon_Age_0","Age-1 Chinook salmon") #13

cpuv.plot.sls("CPUV_Chinook_Salmon_Age_1","Age-1 Chinook salmon") #14

cpuv.plot.sls("CPUV_Steelhead_Age_0","Age-1 steelhead") #15

cpuv.plot.sls("CPUV_White_Sturgeon_Age_0","Age-0 white sturgeon") #16

cpuv.plot.sls("CPUV_White_Catfish_Age_0","Age-0 white catfish") #17

cpuv.plot.sls("CPUV_Starry_Flounder_Age_0","Age-0 starry flounder") #18

cpuv.plot.sls("CPUV_Starry_Flounder_Age_1","Age-1 starry flounder") #19

cpuv.plot.sls("CPUV_Tridentiger_Spp._Age_0","Age-0 Tridentiger Spp.") #20

cpuv.plot.sls("CPUV_Prickly_Sculpin_Age_0","Age-0 prickly sculpin") # 21

cpuv.plot.sls("CPUV_Crangon","Crangon") #22

cpuv.plot.sls("CPUV_Gelatinous","gelatinous species")

cpuv.plot.sls("CPUV_Other_Crustacean","other crustacean species")

cpuv.plot.sls("CPUV_Other_Fish","other fish species")
dev.off()


Review_Data_Tows%>%
  rename("Depth_Ft" = "Depth",
         "TowDepth_ft" = "TowDepth",
         "CableOut_ft" = "CableOut",
         "Secchi_ft" = "Secchi",
         "Volume_1000m3" = "Volume",
         "Salinity_ppt" = "Salinity",
         "Turbidity_ntu" = "Turbidity")%>%
 write_csv(file="Pelagic_Review_Data.csv")



table(Review_Data_Tows)
