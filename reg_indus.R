setproj(19)
#library(ggplot2)
library(tidyverse)
library(dplyr)
library(data.table)
library(choroplethr)
library(choroplethrMaps)
library(cdlTools)
library(maps)
library(lfe)

# --- LOAD THE DATA ---
setwd('/Users/zhaochenhe/Google Drive/research/proj_019_hb56')
load('/Users/zhaochenhe/Google Drive/research/data/qwi/qwi_allvars_naics2_allpersons_allcnty.Rda')
qwiNaics2 = qwiNaics2[, geo := as.numeric(paste0(statefips, cntyfips))]

# --- State Groups ---
# Note: the target state should be the 1st element within each vector
stateIN = c('18', '26', '17', '21', '39')
stateAZ = c('04', '32', '08', '35', '06')
stateUT = c('49', '32', '16', '56', '08')
stateAL = c('01', '12', '28', '47')
stateGA = c('13', '37', '12', '47')
stateSC = c('45', '37')
naics = unique(qwiNaics2$industry)
time = unique(qwiNaics2, by = 'dt')[dt >= '2001-01-01', dt]

# Function 1: Get All Valuable State Pairs
getCountyPairs = function(statelist) {
  z = qwiNaics2[statefips %in% statelist][, c('dt','geo','industry','EmpTotal')]
  z = unique(z, by = 'geo')
  z = as.data.table(expand.grid(county1 = z$geo, county2 = z$geo))
  z = z[county1 != county2]
  #z = z[floor(county1/1000) != floor(county2/1000)]
  z = z[floor(county1/1000) == as.numeric(statelist[1]) | floor(county2/1000) == as.numeric(statelist[1]),]
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
  assign('countyPairs', z,.GlobalEnv)
}

# Function 2: Get Score for the State Pairs
matchScores = function(countyPairs) {
  countyList = unique(c(countyPairs$county1, countyPairs$county2))
  z = qwiNaics2[geo %in% countyList][, c('dt','geo','industry','EmpTotal')]
  z = z[, allIndusAllTimeEmp := sum(EmpTotal, na.rm = T), by = geo]
  z = z[, eachIndusAllTimeEmp := sum(EmpTotal, na.rm = T), by = c('geo', 'industry') ]
  z = z[, percntIndustryEmp := eachIndusAllTimeEmp/allIndusAllTimeEmp]
  z = z[dt=='2017-01-01'][,c('geo','industry','percntIndustryEmp')]
  temp = z[industry == '11']
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
  countyPairsScore = countyPairsScore[, percntile := rank(score)/length(score)][percntile >= 0.95]
  assign('countyPairsScore', countyPairsScore,.GlobalEnv)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# # Another Measurement to Consider: Herfindahl–Hirschman Index ---
# z = qwiNaics2[, c('dt','geo','industry','EmpTotal')]
# z = z[, allIndusAllTimeEmp := sum(EmpTotal, na.rm = T), by = geo]
# z = z[, eachIndusAllTimeEmp := sum(EmpTotal, na.rm = T), by = c('geo', 'industry') ]
# z = z[, percntIndustryEmp := eachIndusAllTimeEmp/allIndusAllTimeEmp]
# z = z[dt=='2017-01-01'][,c('geo','industry','percntIndustryEmp')]
# 
# z = z[, s := percntIndustryEmp^2]
# z = z[, index := sum(s, na.rm = T), by = geo]
# z = unique(z, by = 'geo')
# z = z[, c(1,5)]
# names(z) = c('region','value')
# county_choropleth(z)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

states = list(stateIN, stateAZ, stateUT, stateAL, stateGA, stateSC)
scores = list()

for (i in 1:6) {
  matchScores(getCountyPairs(states[[i]]))
  scores[[i]] = countyPairsScore
  names(scores)[i] = states[[i]][1]
}

# --- Data for Regression ---
# create law variable
qwi = qwiNaics2[,legis:=0]
qwi[(statefips == '01' & dt >'2011-07-01'),legis:=1]
qwi[(statefips == '04' & dt >'2010-04-01'),legis:=1]
qwi[(statefips == '13' & dt >'2011-07-01'),legis:=1]
qwi[(statefips == '45' & dt >'2011-07-01'),legis:=1]
qwi[(statefips == '49' & dt >'2011-04-01'),legis:=1]
qwi[(statefips == '18' & dt >'2011-01-01'),legis:=1]

# build up the structure for regression data
getStructure = function(countyPairsList) {
  z = data.table(rowid_to_column(countyPairsList, "ID"))[, state1 := fips(floor(countyFips1/1000), to = 'Name')][, state2 := fips(floor(countyFips2/1000), to = 'Name')]
  z1 = expand.grid(time,naics,z$ID)
  names(z1) = c('time','industry','ID')
  z2 = merge(z1, z, by = 'ID', all = T)
  return(z2)
}

# get the data for regression
z1 = qwi[, allIndusEmp := sum(EmpTotal, na.rm = T), by = c('geo', 'dt')][dt > '2000-10-01'][, prctEmpTotal := EmpTotal/allIndusEmp][, c("dt","geo",'industry',"prctEmpTotal",'legis')]
getData = function(countyPairsList) {
  z = getStructure(countyPairsList)
  z = merge(z, z1, by.x = c('time','industry','countyFips1'), by.y = c('dt','industry','geo'), all.x = T)
  names(z)[(ncol(z)-1):ncol(z)] = c('y1','law1')
  z = merge(z, z1, by.x = c('time','industry','countyFips2'), by.y = c('dt','industry','geo'), all.x = T)
  names(z)[(ncol(z)-1):ncol(z)] = c('y2','law2')
  return(z)
}

allStateScores = do.call(rbind,scores) # get scores for all countypairs/states
#z = getStructure()
wrkdata = getData(allStateScores[,1:2])
wrkdata = data.table(wrkdata)[, y1_y2 := y1-y2][, law1_law2 := law1-law2]

# --- Run Regression ---
# Regression for All Industry
reg = lm(y1_y2 ~ law1_law2, wrkdata)

# Regression for Each Industry Separately
regAllIndus = matrix(0L, nrow = length(naics), ncol = 2)
for (i in 1:length(naics)) {
  z = wrkdata[industry == naics[i]]
  #reg1 = lm(y1_y2 ~ law1_law2, z)
  reg1 = felm(y1_y2 ~ law1_law2 | time + ID, data = z)
  print(summary(reg1))
  regAllIndus[i,] = summary(reg1)$coefficients[1,c(1,4)]
}
regAllIndus = cbind(naics, regAllIndus)
regAllIndus = as.data.table(regAllIndus)
names(regAllIndus) = c('industry','estimate','p_value')
regAllIndus$estimate=as.numeric(regAllIndus$estimate)
regAllIndus$p_value=as.numeric(regAllIndus$p_value)

# Fixed Effects for All Industry
reg1 = felm(y1_y2 ~ law1_law2 | time, data = wrkdata)
reg1 = felm(y1_y2 ~ law1_law2 | ID, data = wrkdata)
reg1 = felm(y1_y2 ~ law1_law2 | time + ID, data = wrkdata)




