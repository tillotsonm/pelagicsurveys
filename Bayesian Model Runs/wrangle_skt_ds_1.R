#!usr/bin/env Rscript
#
# Purpose: Read and wrangle FMWT Threadfin Shad (TFS) age-0 catch data for 
#   Bayesian Poisson GLMM analysis
#  
# Author: John R. Brandon, PhD (ICF)
#         john.brandon at icf.com
# 
library(pacman)
p_load(lubridate, here, janitor, tidyverse)

# Read data --------------------------------------------------------------------
# This loads `Review_Data_Tows` which is wrangled in subsequent section below. 
# Directory structure still needs a bit of work at this stage to be consistent 
#   across machines.
# For the time being, you can ignore these commands, and just make sure
#   the `Review_Data_Tows` data.frame above is loaded. Then proceed to the
#   "Wrangle data" section below.
# 
# dat_dir = here('data', 'FINAL_REVIEW_DATA')
# load(file = here(dat_dir, 'CDFW_Pelagic_Review_Data.rda'))

# Wrangle data -----------------------------------------------------------------
dat_fmwt_ds_age1 = Review_Data_Tows %>% 
  filter(SurveySeason == 'SKT') %>% 
  mutate(ds_age1_catch = CPUV_Delta_Smelt_Age_1 * Volume,
         ds_age1_catch = round(ds_age1_catch),  # correct any minor rounding error
         ds_age1_catch = as.integer(ds_age1_catch)) %>% 
  select(ds_age1_catch, CPUV_Threadfin_Shad_Age_0, 
         Volume, everything()) %>%
  select(-(CPUV_Pacific_Herring_Age_0:Mean_Length_Prickly_Sculpin_Age_0)) %>%  # remove extraneous columns 
  janitor::clean_names() %>% 
  mutate(month = factor(month, levels = 1:5)) %>% 
  rename(lon = station_longitude,
         lat = station_latitude) %>% 
  drop_na(ds_age1_catch, review_region, region) %>% # Remove rows with NAs in key variables
  arrange(sample_date) %>% 
  mutate(year_fac = ordered(year),   # Create ordered factor version of year
         month_fac = ordered(month), # Create ordered factor version of month
         station_fac = factor(station_code),
         id = row_number()) %>%      # Create observation (tow) level identifier (ID))
  select(id, everything()) %>% 
  mutate(lat_c = scale(lat),         # Normalize covariates for model fitting
         lon_c = scale(lon),
         volume_c = scale(volume)) 

# distinct(dat_fmwt_tfs_age0, review_stratum)  # Check


