#!usr/bin/env Rscript
#
# Purpose: Wrangle Bayes catch model output and plot trends through time 
#   by region.
# Notes: 
#   - This code was adapted from LTRM Ch6: https://github.com/sbashevkin/LTMRdata
#  
# Author: John R. Brandon, PhD (ICF)
#         john.brandon at icf.com
# 
library(pacman)
p_load(lubridate, here, janitor, tidyverse, tidybayes)  # Running this line will install (and load) packages if not already

# Wrangle model output ---------------------------------------------------------
min_year = 2002
max_year = 2020
# Derive posteriors for pointwise annual catch within each region


load("tfs_model_full_5e3.rds")

out_tfs_full = expand.grid(volume_c = 0, 
                           review_region = factor(unique(Review_Data_Tows$Review_Region)),
                           year_fac=factor(2002:2020))%>%
    mutate(year = as.numeric(as.character(year_fac)))%>%
    add_fitted_draws(model_full_tfs, re_formula = NA, scale = "response")%>%  # re_formula=NA -> includes no group-level effects
    ungroup()%>%
    mutate(Mean = mean(.value))%>%
    group_by(review_region, .draw)%>%
    mutate(Lag = lag(.value, order_by = year))%>%
    mutate(Change_global = (.value - Lag) / (Mean),
           Change_local = (.value - Lag) / (.value + Lag)) %>%
    ungroup()

out_tfs_full %>% 
  group_by(review_region, year) %>% 
  median_qi(.value, .width = 0.95) %>%  # calculate medians and 95% credibility intervals
  ggplot(aes(x = year, y = .value)) +
  geom_ribbon(aes(x = year, ymin = .lower, ymax = .upper), fill = 'grey') +
  geom_line() +
  facet_grid(rows = vars(review_region), scales = 'free_y') +  # change scales argument if want fixed y-axis range
  theme_bw(base_size = 16) +
  labs(x = 'Year', y = 'Predicted count', title = 'FMWT Age-0 Threadfin Shad')
# Note, the labeling on the y-axis above follows Ch6, but I'm not sure this is 
# technically the "predicted" count [i.e. the posterior predictive distribution for 
# catch], as opposed to the posterior for the expected count.
# Something to double check.





Review_Data_Tows%>%filter(SurveySeason=="FMWT")%>%
  mutate(Catch = CPUV_Threadfin_Shad_Age_0*mean(Volume))%>%
  group_by(Year,Review_Region)%>%
  summarise(Catch = mean(Catch),
            Volume = mean(Volume))%>%
  ungroup()%>%
  ggplot(aes(x=Year,y=Catch))+geom_line()+facet_wrap(~Review_Region,scales = "free")


newdata <- expand.grid(volume_c = 0, review_region = factor(unique(Review_Data_Tows$Review_Region)),year_fac=factor(2002:2020))

observed <- Review_Data_Tows%>%filter(SurveySeason=="FMWT")%>%
  mutate(Catch = mean(Volume)*CPUV_Threadfin_Shad_Age_0)%>%
  group_by(Review_Region,Year)%>%
  summarise(Catch = mean(Catch))


