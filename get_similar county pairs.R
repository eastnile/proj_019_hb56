setproj(19)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(data.table)
library(choroplethr)
library(choroplethrMaps)
library(cdlTools)
library(maps)

# --- LOAD THE DATA ---
setwd('/Users/zhaochenhe/Google Drive/research/proj_019_hb56')
load('/Users/zhaochenhe/Google Drive/research/data/qwi/qwi_allvars_naics2_allpersons_allcnty.Rda')
qwiNaics2 = qwiNaics2[, geo := as.numeric(paste0(statefips, cntyfips))]
#qwiNaics2$statefips = as.numeric(qwiNaics2$statefips)
naics = unique(qwiNaics2$industry)


# --- Get the Score of Similarity Between a Certain County and All the Other Counties---
# Function 1: Get All Valuable County Pairs
getCountyPairs = function(countyfips) {
  z = unique(qwiNaics2, by = 'geo')
  #z = z[statefips %in% c('01','28','47','12')]
  z = as.data.table(expand.grid(county1 = z$geo, county2 = z$geo))
  z = z[county1 == countyfips][county1 != county2]
  assign('countyPairs', z,.GlobalEnv)
} #Notes: modify the code here if we only want the pairs in a smaller area instead of across the whole country

# Function 2: Get Score for the County Pairs
matchScores = function(countyPairs) {
  countyList = unique(c(countyPairs$county1, countyPairs$county2))
  z = qwiNaics2[geo %in% countyList][, c('dt','geo','industry','EmpTotal')]
  z = z[, allIndusAllTimeEmp := sum(EmpTotal, na.rm = T), by = geo]
  z = z[, eachIndusAllTimeEmp := sum(EmpTotal, na.rm = T), by = c('geo', 'industry') ]
  z = z[, percntIndustryEmp := eachIndusAllTimeEmp/allIndusAllTimeEmp]
  z = z[dt=='2017-01-01'][,c('geo','industry','percntIndustryEmp')]
  temp = z[industry == 11]
  temp = temp[, percntile := rank(percntIndustryEmp)/length(percntIndustryEmp)]
  for (i in naics[-1]) {
    temp1 = z[industry == i][, percntile := rank(percntIndustryEmp)/length(percntIndustryEmp)]
    temp = rbind(temp, temp1)
  }
  percntile = temp[,-3]
  percntile = dcast(percntile, geo ~ industry, value.var = 'percntile')
  corScore = function(x,y) {
    z = cor(as.numeric(percntile[geo == x][,-1]), as.numeric(percntile[geo == y][,-1]), use='pairwise.complete.obs', method = 'pearson')
    return(z)
  }
  scoreValue = vector(mode = 'numeric', length = nrow(countyPairs))
  for (i in 1:nrow(countyPairs)) {
    z = corScore(countyPairs[i][[1]],countyPairs[i][[2]])
    scoreValue[i] = z
  }
  countyPairsScore = cbind(countyPairs, scoreValue)
  names(countyPairsScore) = c('countyFips1','countyFips2','score')
  assign('countyPairsScore', countyPairsScore,.GlobalEnv)
} # some code can be add here to select only the top 5% most similar counties

# Function 3: Plot All Similar Counties for a Certain County---
plotpairs = function(countyfips) {
  #z = data.table(t(c(countyfips,1)))
  #names(z) = c('region','value')
  z1 = countyPairsScore[countyFips2 == countyfips][,-2]
  z2 = countyPairsScore[countyFips1 == countyfips][,-1]
  z1 = rbind(z1, z2, use.names = F)
  names(z1) = c('region','value')
  #z1 = rbind(z,z1)
  county_choropleth(z1)
  #county_choropleth(z1, state_zoom = c("alabama", "tennessee", "mississippi",'florida'))  # Modify this line to only show certain states in the graph
} # this function can be run only after "getCountyPairs" and "matchScores" are run

# sample
matchScores(getCountyPairs(51710))
plotpairs(51710)





