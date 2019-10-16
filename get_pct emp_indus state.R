setproj(19)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(tfplot)
library(miceadds)
library(choroplethr)
library(choroplethrMaps)

# --- LOAD THE DATA ---
setwd('/Users/zhaochenhe/Google Drive/research/proj_019_hb56')
load('/Users/zhaochenhe/Google Drive/research/data/qwi/qwi_allvars_naics2_allpersons_allcnty.Rda')
load('/Users/zhaochenhe/Google Drive/research/data/qwi/qwi_allvars_naics3_allpersons_allcnty.Rda')

# --- MAP FOR AGRICULTURAL ---
# create geography variables
z1 = qwiNaics2[time == '2011-Q1' & statefips == '01'][, c(1:5, 11)]
z1 = z1[, geo := as.numeric(paste0(statefips, cntyfips))]
# create value variables
z2 = z1[, allIndustry := sum(EmpTotal, na.rm = T), by = cntyfips]
z3 = z2[industry == 11]
z3 = z3[, prcntAgricultue := 100*EmpTotal/allIndustry][,c(7,9)]
names(z3) = c('region', 'value')
# plot
county_choropleth(z3, state_zoom = 'alabama')




