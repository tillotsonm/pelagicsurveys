#!usr/bin/env Rscript
#
# Purpose: Fit Bayesian Poisson regression models to FMWT LFS catch data. This
#   code has been adapted from Bashevkin (IEP 2020, ch 6), whom developed this 
#   approach using Splittail as a case study. The repository for that code is
#   available here: https://github.com/sbashevkin/LTMRpilot
#   
# IEP Long-term Survey Review Team. 2020. Interagency Ecological Program Long-term
# Monitoring Element Review: Pilot approach and methods development (2020). IEP Technical
# Report. 201 pp.
# 
# Notes: 
#  - Data are wrangled in 'wrangle_fmwt_lfs.R' script
# 
# Author: John R. Brandon, PhD (ICF)
#         john.brandon at icf.com
# 
library(pacman)
p_load(lubridate, here, janitor, tidyverse, brms, broom.mixed, tidybayes, rstan)
# source(here('LTMRpilot', 'Univariate analyses', 'Survey assessment functions.R'))
# source(here('R', 'wrangle_fmwt_tfs.R'))
source('Survey assessment functions.R')  # Load some helper functions, e.g. `model_diagnose`
source('wrangle_fmwt_tfs.R')

# Initialize -------------------------------------------------------------------
rstan_options(auto_write = TRUE)  # helps stan run faster?
iterations = 5e3
warmup = iterations / 4

model_full_tfs = brm(tfs_age0_catch ~ volume_c + year_fac*review_region + (1|station_fac) + (1|id),
           family = poisson, data = dat_fmwt_tfs_age0,
           prior=prior(normal(0,5), class="Intercept")+
             prior(normal(0,5), class="b")+
             prior(cauchy(0,5), class="sd"),
           chains = 3, cores = 3, control = list(max_treedepth = 15),
           iter = iterations, warmup = warmup)

save(model_full_tfs, file = 'tfs_model_full_5e3.Rds')  # save model
# load(file = tfs_model_file)  

# Model without random effects on individual tows for comparison 
# iterations = 3e3
# warmup = iterations / 4
# model_full_tfs_2 = brm(tfs_age0_catch ~ volume_c + year_fac * review_region + (1|station_fac), # No random-effect for individual tows
#               family = poisson, data = dat_fmwt_tfs_age0,
#               prior=prior(normal(0,5), class="Intercept")+
#                 prior(normal(0,5), class="b")+
#                 prior(cauchy(0,5), class="sd"),
#               chains = 3, cores = 3, control = list(max_treedepth = 15),
#               iter = iterations, warmup = warmup)
# save(model_full_tfs_2, file = here::here('model_fits', 'model_full_tfs_2.Rds'))  # save model to avoid recompiling
# summary(model_2)

# Diagnose model ---------------------------------------------------------------
model_diagnose(model_full_tfs)    # `model_diagnose` is loaded in 'Survey assessment functions.R' sourced at top



