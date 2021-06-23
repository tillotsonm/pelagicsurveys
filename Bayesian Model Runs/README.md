# Bayesian catch analysis

This directory contains files for a Bayesian analysis of FMWT catch data for several species: 
1. Longfin Smelt (age-0 and age-1); 
2. Striped Bass (age-0); 
3. American Shad (age-0 and age-1); 
4. Northern Anchovy (age-0), and; 
5. Threadfin Shad (age-0).

The analysis generally follows the methods for chapter 6 of the LTRM report and [adapts the code developed by Bashevkin](https://github.com/sbashevkin/LTMRpilot/tree/master/Univariate%20analyses), i.e. a Bayesian GLMM is used to perform something of a retrospective analysis to compare annual estimates of catch (a proxy for abundance) under scenarios with varying levels of sampling effort (e.g. dropping a certain percentage of tows in a seasonal survey stratum). 

Unlike Ch6 of the LTRM report, which focused on Sacramento Splittail otter trawl catch data from the Bay Study and the UC Davis Suisun Marsh Study, these analyses focus on the species listed above, and catch data collected in the Fall Midwater Trawl during 2002-2020. Additionally, because the Fall Midwater Trawl has a relatively limited period of sampling, no attempt is made here to incorporate a seasonal component to the catch models. Instead, season is replaced by survey region in the regression. This step involved post-stratifying the catch data for each species into five survey regions (from seaward to landward):
1. Far West
2. Suisun
3. Confluence
4. South, and;
5. North

