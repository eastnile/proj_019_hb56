setproj(19)
#library(ggplot2)
library(tidyverse)
library(dplyr)
library(data.table)

# --- LOAD THE DATA ---
setwd('/Users/zhaochenhe/Google Drive/research/proj_019_hb56')
load('/Users/zhaochenhe/Google Drive/research/data/qwi/qwi_allvars_naics2_allpersons_allcnty.Rda')

# --- State Groups ---
stateIN = c('06', '26', '17', '21', '39')
stateAL = c('28', '47', '13', '01', '12', '37', '45')
stateAZ = c('06', '32', '49', '04', '08', '35', '16', '56')

# --- Create an object to show all the percentiles ---
qwi = qwiNaics2
qwi = qwi[statefips %in% stateAL][, geo := as.numeric(paste0(statefips, cntyfips))][, c('dt','geo','industry','EmpTotal')]
qwi = qwi[, allIndusAllTimeEmp := sum(EmpTotal, na.rm = T), by = geo]
qwi = qwi[, eachIndusAllTimeEmp := sum(EmpTotal, na.rm = T), by = c('geo', 'industry') ]
qwi = qwi[, percntIndustryEmp := eachIndusAllTimeEmp/allIndusAllTimeEmp]
qwi = qwi[dt=='2001-01-01'][,c('geo','industry','percntIndustryEmp')]

temp = qwi[industry == 11]
temp = temp[, percntile := rank(percntIndustryEmp)/length(percntIndustryEmp)]
naics = unique(qwiNaics2$industry)
for (i in naics[-1]) {
  temp1 = qwi[industry == i][, percntile := rank(percntIndustryEmp)/length(percntIndustryEmp)]
  temp = rbind(temp, temp1)
}
percntile = temp[,-3]
percntile = dcast(percntile, geo ~ industry, value.var = 'percntile')


# --- Generate all possible county pairs ---
z = as.data.table(expand.grid(county1 = percntile$geo, county2 = percntile$geo))
z = z[county1 != county2]
dupCheck = function(x, y) {
  if(x < y) {
    z = paste0(x, ' & ', y)
    return(z)
  } else {
    z = paste0(y, ' & ', x)
    return(z)
  }
}
z = z[, dupCheck := mapply(dupCheck, z$county1, z$county2)]
z = unique(z, by = 'dupCheck')[,-3]
countyPairsScore = z


# --- Calculate scores for all possible county pairs ---
corScore = function(x,y) {
  z = cor(as.numeric(percntile[geo == x][,-1]), as.numeric(percntile[geo == y][,-1]), use='pairwise.complete.obs', method = 'pearson')
  return(z)
}
scoreValue = vector(mode = 'numeric', length = nrow(countyPairsScore))
for (i in 1:nrow(countyPairsScore)) {
  z = corScore(countyPairsScore[i][[1]],countyPairsScore[i][[2]])
  scoreValue[i] = z
}
scoreValueAL = scoreValue
countyPairsScoreAL = cbind(countyPairsScore, scoreValueAL)






