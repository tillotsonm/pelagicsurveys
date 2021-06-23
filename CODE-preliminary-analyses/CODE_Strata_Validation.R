library("multcomp")
library("lme4")
library("effects")
library("phia")
library("AICcmodavg")
library("tidyverse")
library("wesanderson")
library("stringr")

select <- dplyr::select
filter <- dplyr::filter


setwd("C:/Users/40545/Documents/GitHub/pelagicsurveys")
#===============Delta Base Layer================================

load("FINAL_REVIEW_DATA/CDFW_Pelagic_Review_Data.rda")





#==========================Species ANOVAS====================================

Binary_Data <- Review_Data_Tows%>%mutate_at(vars(contains("CPUV")),~if_else(.==0,0,1))%>%ungroup()%>%
  select(-(contains("Length")|contains("Other")))%>%mutate(Year=as.factor(Year))



Plot_Comparisons <- function(taxa = "CPUV_American_Shad_Age_0",print_name="Age-0 American Shad",draw=T){
  
  region_lookup <- Binary_Data %>% 
    select(Review_Region,Review_Stratum)%>%
    distinct()
  
  which_surveys <- Binary_Data %>%
    ungroup %>%
    select(SurveySeason,taxa)
  
  names(which_surveys) <- c("SurveySeason","taxa")
  
  
  which_surveys <-  which_surveys%>%
    group_by(SurveySeason)%>%
    summarise(n=n(),detection=sum(taxa))%>%
    filter(n>99&detection!=0)%>%
    ungroup()%>%
    select(SurveySeason)%>%with(.,as.vector(SurveySeason))
  
  print(which_surveys)
  
  
  trim_dat <- Binary_Data %>% filter(SurveySeason==which_surveys[1]) %>%
    group_by(SurveySeason,Review_Stratum)%>%mutate(N_Tows = n())%>%ungroup()%>%filter(N_Tows>40)%>% 
    select(Review_Region,Review_Stratum,taxa,Year,StationCode)%>%mutate(Year = as.factor(Year))
  
  names(trim_dat) <- c("Review_Region","Review_Stratum","species","Year","StationCode")
  
  #fit_glm_region <-  glmer(data=trim_dat, species~0+Review_Region+(1|Year)+(1|StationCode),family="binomial",nAGQ=0)  
  fit_glm <-  glmer(data=trim_dat, species~0+Review_Stratum+(1|Year)+(1|StationCode),family="binomial",nAGQ=0)
  
  ilink <- family(fit_glm)$linkinv
  
  plot_dat <- 
    summary(fit_glm)$coefficients%>%data.frame()%>%
    rownames_to_column("Review_Stratum")%>%
    rename("SE" = "Std..Error", "p_value" = "Pr...z..")%>%
    select(-z.value)%>%
    mutate(Upper=Estimate+(2*SE),
           Lower=Estimate-(2*SE))%>%
    mutate_at(vars(c(Estimate,Upper,Lower)),ilink)%>%
    mutate_if(is.numeric,round,4)%>%as_tibble()%>%
    mutate(Review_Stratum = str_remove(Review_Stratum,"Review_Stratum"))%>%
    add_column(Survey = which_surveys[1])%>%
    left_join(region_lookup,by="Review_Stratum")
  
  
  
  for(i in 2:length(which_surveys)){
    
    trim_dat <- Binary_Data %>% filter(SurveySeason==which_surveys[i]) %>% 
      select(Review_Region,Review_Stratum,taxa,Year,StationCode)%>%mutate(Year = as.factor(Year))
    
    names(trim_dat) <- c("Review_Region","Review_Stratum","species","Year","StationCode")
    
    #fit_glm_region <-  glmer(data=trim_dat, species~0+Review_Region+(1|Year)+(1|StationCode),family="binomial",nAGQ=0)  
    fit_glm <-  glmer(data=trim_dat, species~0+Review_Stratum+(1|Year)+(1|StationCode),family="binomial",nAGQ=0)  
    
    plot_dat_a <- 
      summary(fit_glm)$coefficients%>%data.frame()%>%
      rownames_to_column("Review_Stratum")%>%
      rename("SE" = "Std..Error", "p_value" = "Pr...z..")%>%
      select(-z.value)%>%
      mutate(Upper=Estimate+(2*SE),
             Lower=Estimate-(2*SE))%>%
      mutate_at(vars(c(Estimate,Upper,Lower)),ilink)%>%
      mutate_if(is.numeric,round,4)%>%as_tibble()%>%
      mutate(Review_Stratum = str_remove(Review_Stratum,"Review_Stratum"))%>%
      add_column(Survey = which_surveys[i])%>%
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
    rename("Review Region" = "Review_Region")%>%
    mutate(Upper = if_else(Upper==1 & Estimate==0,0,Upper))%>%
    mutate(Lower = if_else(Upper==1 & Estimate==1,1,Lower))
  
p <- plot_dat %>% ggplot(aes(y=`Review Stratum`,x=Estimate,col=`Review Region`))+theme_bw()+
          scale_color_manual(values=wes_palette(n=5, name="Zissou1"))+
          geom_vline(xintercept = 0)+xlab("Probability of Catch")+
          geom_linerange(aes(xmin=Lower,xmax=Upper),size=1.5)+ggtitle(paste(print_name,"catch probabilities by region and strata"))+
          geom_point(size=2.5)+
          facet_grid(cols=vars(Survey))

if(draw==TRUE){print(p)}
  
  return(list(p,plot_dat))
}



Multiple_Comparisons <- function(taxa = "CPUV_American_Shad_Age_0"){
  
  which_surveys <- Binary_Data %>%
    ungroup %>%
    select(SurveySeason,taxa)
  
  names(which_surveys) <- c("SurveySeason","taxa")
  
  
  which_surveys <-  which_surveys%>%
    group_by(SurveySeason)%>%
    summarise(n=n(),detection=sum(taxa))%>%
    filter(n>99&detection!=0)%>%
    ungroup()%>%
    select(SurveySeason)%>%with(.,as.vector(SurveySeason))
  
  
  which_regions <- Binary_Data %>%
    ungroup %>%
    filter(SurveySeason %in% which_surveys)%>%
    select(SurveySeason,Review_Region,Review_Stratum,taxa)
  
  names(which_regions) <- c("SurveySeason","Review_Region","Review_Stratum","taxa")
  
  which_regions <- which_regions %>% 
    group_by(SurveySeason,Review_Region,Review_Stratum)%>%
    summarise(
              detection=sum(taxa),
              N_Tows = n(),
              prop.detect = detection/N_Tows)%>%
    filter(N_Tows > 49)
   
  which_regions%>%filter(Review_Region=="Confluence")%>%print()
  
  which_regions <- which_regions%>%
    ungroup()%>%
    group_by(SurveySeason,Review_Region)%>%
    summarise(N_Strata =  n(),
              N_Occupied = length(prop.detect[(prop.detect>.002)==T]),
              detection = sum(prop.detect))%>%
    filter(N_Strata>1&detection !=0)%>%
    ungroup()
  
  
  #omit 

  # Region level model fits and output
  
  How_Many_Groups <- data.frame(SurveySeason = which_surveys, min_groups=NA, max_groups=NA,best_groups=NA,min_DAICc = NA)
  
  
  for(i in 1:length(which_surveys)){
  reg_dat <- Binary_Data%>%filter(SurveySeason==which_surveys[i])%>%
    select(Review_Region,Year,StationCode,taxa)%>%
    mutate(Review_Region = droplevels(Review_Region))%>%data.frame()

  names(reg_dat) <- c("Review_Region","Year","StationCode","species")

  #fit_glm_region <-  glmer(data=trim_dat, species~0+Review_Region+(1|Year)+(1|StationCode),family="binomial",nAGQ=0)
  fit_reg <-  glmer(data=reg_dat, species~Review_Region+(1|Year)+(1|StationCode),family="binomial",nAGQ=0)

  reg_output <- multComp(fit_reg,"Review_Region",correction = "bonferroni")

  print(which_surveys[i])

  temp <- reg_output$model.table%>%data.frame()%>%filter(Delta_AICc<4)%>%mutate(K=K-2)

  How_Many_Groups[i,2] <- min(temp$K)
  How_Many_Groups[i,3] <- max(temp$K)
  How_Many_Groups[i,4] <- temp$K[1]
  How_Many_Groups[i,5] <- temp$Delta_AICc[2]

  }
  

  
  Stratum_Significance <- data.frame(SurveySeason = which_regions$SurveySeason,
                                     Review_Region = which_regions$Review_Region,
                                     N_Groups_a = NA,
                                     N_Groups_b =  NA,
                                     DeltaAICc = NA)
  for(k in 1:nrow(which_regions)){
    
    strat_dat <- Binary_Data%>%
      filter(Review_Region==which_regions$Review_Region[k]& SurveySeason ==  which_regions$SurveySeason[k])%>%
      select(Review_Stratum,Year,StationCode,taxa)%>%
      mutate(Review_Stratum = droplevels(Review_Stratum))%>%data.frame()
    
    names(strat_dat) <- c("Review_Stratum","Year","StationCode","species")
    
    #fit_glm_region <-  glmer(data=trim_dat, species~0+Review_Region+(1|Year)+(1|StationCode),family="binomial",nAGQ=0)  
    fit_strat <-  glmer(data=strat_dat, species~Review_Stratum+(1|Year)+(1|StationCode),family="binomial",nAGQ=0)  
    
    strat_output <- multComp(fit_strat,"Review_Stratum",correction = "bonferroni")    
    
    temp2 <- strat_output$model.table%>%data.frame()
    
    Stratum_Significance[k,3] <- temp2$K[1]-2
    Stratum_Significance[k,4] <- temp2$K[2]-2
    Stratum_Significance[k,5] <- round(temp2$Delta_AICc[2],2)
    
  print(k)
  }
  return(list(which_regions,How_Many_Groups,Stratum_Significance))
}

#Age 0

MOD_Pacific_Herring_0 <- Plot_Comparisons(taxa="CPUV_Pacific_Herring_Age_0",print_name = "Age-0 Pacific herring")
MC_Pacific_Herring_0 <- Multiple_Comparisons("CPUV_Pacific_Herring_Age_0")

MOD_Northern_Anchovy_0 <- Plot_Comparisons(taxa="CPUV_Northern_Anchovy_Age_0",print_name = "Age-0 northern anchovy")
MC_Northern_Anchovy_0 <- Multiple_Comparisons("CPUV_Northern_Anchovy_Age_0")

MOD_Longfin_Smelt_0 <- Plot_Comparisons(taxa="CPUV_Longfin_Smelt_Age_0",print_name = "Age-0 longfin smelt")
MC_Longfin_Smelt_0 <- Multiple_Comparisons("CPUV_Longfin_Smelt_Age_0")

MOD_American_Shad_0 <- Plot_Comparisons(taxa="CPUV_American_Shad_Age_0",print_name = "Age-0 American shad")
MC_American_Shad_0 <- Multiple_Comparisons("CPUV_American_Shad_Age_0")

MOD_Crangon <- Plot_Comparisons(taxa="CPUV_Crangon",print_name = "Crangon")
MC_Crangon <- Multiple_Comparisons("CPUV_Crangon")

MOD_Striped_Bass_0 <- Plot_Comparisons(taxa="CPUV_Striped_Bass_Age_0",print_name = "Age-0 striped bass")
MC_Striped_Bass_0 <- Multiple_Comparisons("CPUV_Striped_Bass_Age_0")

MOD_Delta_Smelt_0 <- Plot_Comparisons(taxa="CPUV_Delta_Smelt_Age_0",print_name = "Age-0 delta smelt")
MC_Delta_Smelt_0 <- Multiple_Comparisons("CPUV_Delta_Smelt_Age_0")

MOD_Threadfin_Shad_0 <- Plot_Comparisons(taxa="CPUV_Threadfin_Shad_Age_0",print_name = "Age-0 threadfin shad")
MC_Threadfin_Shad_0 <- Multiple_Comparisons("CPUV_Threadfin_Shad_Age_0")

MOD_Starry_Flounder_0 <- Plot_Comparisons(taxa="CPUV_Starry_Flounder_Age_0",print_name = "Age-0 starry flounder")
MC_Starry_Flounder_0 <- Multiple_Comparisons("CPUV_Starry_Flounder_Age_0")

MOD_White_Catfish_0 <- Plot_Comparisons(taxa="CPUV_White_Catfish_Age_0",print_name = "Age-0 white catfish")
MC_White_Catfish_0 <- Multiple_Comparisons("CPUV_White_Catfish_Age_0")

MOD_Tridentiger_Spp._0 <- Plot_Comparisons(taxa="CPUV_Tridentiger_Spp._Age_0",print_name = "Age-0 Tridentiger Spp.")
MC_Tridentiger_Spp._0 <- Multiple_Comparisons("CPUV_Tridentiger_Spp._Age_0")

MOD_Chinook_Salmon_0 <- Plot_Comparisons(taxa="CPUV_Chinook_Salmon_Age_0",print_name = "Age-0 Chinook salmon")
MC_Chinook_Salmon_0 <- Multiple_Comparisons("CPUV_Chinook_Salmon_Age_0")

MOD_Steelhead_0 <- Plot_Comparisons(taxa="CPUV_Steelhead_Age_0",print_name = "Age-0 steelhead")
MC_Steelhead_0 <- Multiple_Comparisons("CPUV_Steelhead_Age_0")

MOD_Prickly_Sculpin_0 <- Plot_Comparisons(taxa="CPUV_Prickly_Sculpin_Age_0",print_name = "Age-0 prickly sculpin")
MC_Prickly_Sculpin_0 <- Multiple_Comparisons("CPUV_Prickly_Sculpin_Age_0")

MOD_White_Sturgeon_0 <- Plot_Comparisons(taxa="CPUV_White_Sturgeon_Age_0",print_name = "Age-0 white sturgeon")
MC_White_Sturgeon_0 <- Multiple_Comparisons("CPUV_White_Sturgeon_Age_0")


#Age 1

MOD_Longfin_Smelt_1 <- Plot_Comparisons(taxa="CPUV_Longfin_Smelt_Age_1",print_name = "Age-1 longfin smelt")
MC_Longfin_Smelt_1 <- Multiple_Comparisons("CPUV_Longfin_Smelt_Age_1")

MOD_American_Shad_1 <- Plot_Comparisons(taxa="CPUV_American_Shad_Age_1",print_name = "Age-1 American shad")
MC_American_Shad_1 <- Multiple_Comparisons("CPUV_American_Shad_Age_1")

MOD_Striped_Bass_1 <- Plot_Comparisons(taxa="CPUV_Striped_Bass_Age_1",print_name = "Age-1 striped bass")
MC_Striped_Bass_1 <- Multiple_Comparisons("CPUV_Striped_Bass_Age_1")

MOD_Delta_Smelt_1 <- Plot_Comparisons(taxa="CPUV_Delta_Smelt_Age_1",print_name = "Age-1 delta smelt")
MC_Delta_Smelt_1 <- Multiple_Comparisons("CPUV_Delta_Smelt_Age_1")

MOD_Threadfin_Shad_1 <- Plot_Comparisons(taxa="CPUV_Threadfin_Shad_Age_1",print_name = "Age-1 threadfin shad")
MC_Threadfin_Shad_1 <- Multiple_Comparisons("CPUV_Threadfin_Shad_Age_1")

MOD_Prickly_Sculpin_1 <- Plot_Comparisons(taxa="CPUV_Prickly_Sculpin_Age_1",print_name = "Age-1 prickly sculpin")
MC_Prickly_Sculpin_1 <- Multiple_Comparisons("CPUV_Prickly_Sculpin_Age_1")


dat_slice <- Binary_Data %>% filter(SurveySeason=="20mm" & Review_Region=="Far West")%>% select(CPUV_Pacific_Herring_Age_0,Review_Stratum,StationCode,Year)%>%
  data.frame()%>%mutate(Review_Stratum = droplevels(Review_Stratum))
  
test_mod <- glmer(data=dat_slice, CPUV_Pacific_Herring_Age_0~Review_Stratum+(1|Year)+(1|StationCode),family="binomial",nAGQ=0)


glob.ilink <- family(fit_glm)$linkinv

predict(test_mod)%>%glob.ilink()%>%bind_cols(dat_slice$CPUV_Pacific_Herring_Age_0)%>%rename(Pred=...1,Obs=...2)%>%mutate(Obs = as.factor(Obs))%>%
  ggplot(aes(x=Obs,y=Pred))+geom_boxplot()


inv.logit
