#===========Code to generate station-level summary variables=========
#=================And composite rankings=============================
#===========for unmeasured individuals and calculate CPUE============
#Prepared by Michael Tillotson
#ICF
#Updated March 11, 2021
#Currently incomplete. Does not have CPUE calculated.
require(tidyverse)

setwd("C:/Users/40545/Documents/GitHub/pelagicsurveys")


All_Surveys_Long %>% filter(ForkLength<150 & ForkLength > 0)%>%
  ggplot(aes(x=ForkLength,fill=SurveySeason))+geom_density()+
  scale_fill_viridis_d()
