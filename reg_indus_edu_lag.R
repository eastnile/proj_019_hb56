setproj(19)
#library(ggplot2)
library(tidyverse)
library(dplyr)
library(data.table)
library(lfe)

# --- LOAD AND CLEAN THE DATA ----
setwd('/Users/zhaochenhe/Google Drive/research/proj_019_hb56')
qwi = fread('wrkdata/qwi_naics2_eth_edu.csv')
qwi = qwi[, yq := paste(year,'-',quarter)][, dt := yq(yq)]
qwi = qwi[,statefipsn := floor(geography/1000)][,cntyfipsn := geography - statefipsn*1000]
qwi = qwi[, c("dt","geography","statefipsn","cntyfipsn","industry","race","ethnicity","education","EmpTotal","Payroll")]
names(qwi)[2] = 'geo'
qwi = qwi[!duplicated(qwi)]

naics = unique(qwiIndus$industry)
time = unique(qwi, by = 'dt')[dt >= '2006-01-01', dt]
edu = unique(qwi, by = "education")[,education][-1]

stateIN = c(18,26,17,21,39)
stateAZ = c(4,32,8,35,6)
stateUT = c(49,32,16,56,8)
stateAL = c(1,12,28,47)
stateGA = c(13,37,12,47)
stateSC = c(45,37)

# --- FIND SIMILAR COUNTIES ----
qwiIndus = qwi[race == 'A0' & ethnicity == 'A0' & industry != '00' & education == 'E0']
getCountyPairs = function(statelist) {
  z = qwiIndus[statefipsn %in% statelist][, c('dt','geo','industry','EmpTotal')]
  z = unique(z, by = 'geo')
  z = as.data.table(expand.grid(county1 = z$geo, county2 = z$geo))
  z = z[county1 != county2]
  z = z[floor(county1/1000) != floor(county2/1000)]
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
matchScores = function(countyPairs) {
  countyList = unique(c(countyPairs$county1, countyPairs$county2))
  z = qwiIndus[geo %in% countyList][, c('dt','geo','industry','EmpTotal')]
  z = z[, allIndusAllTimeEmp := sum(EmpTotal, na.rm = T), by = geo]
  z = z[, eachIndusAllTimeEmp := sum(EmpTotal, na.rm = T), by = c('geo', 'industry')]
  z = z[, percntIndustryEmp := eachIndusAllTimeEmp/allIndusAllTimeEmp]
  z = z[dt=='2016-10-01'][,c('geo','industry','percntIndustryEmp')]
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
states = list(stateIN, stateAZ, stateUT, stateAL, stateGA, stateSC)
scores = list()
for (i in 1:6) {
  matchScores(getCountyPairs(states[[i]]))
  scores[[i]] = countyPairsScore
  names(scores)[i] = states[[i]][1]
}
allpairs = do.call(rbind,scores)[,1:2]

# --- BUILD THE DATA FOR REGRESSION ----
qwi = qwi[,legis:=0]
qwi[(statefipsn == 1 & dt >'2011-07-01'),legis:=1]
qwi[(statefipsn == 4 & dt >'2010-04-01'),legis:=1]
qwi[(statefipsn == 13 & dt >'2011-07-01'),legis:=1]
qwi[(statefipsn == 45 & dt >'2011-07-01'),legis:=1]
qwi[(statefipsn == 49 & dt >'2011-04-01'),legis:=1]
qwi[(statefipsn == 18 & dt >'2011-01-01'),legis:=1]

qwiIndEdu = qwi[race == 'A0' & ethnicity == 'A0' & industry != '00' & education != 'E0'][, Aveg := mean(EmpTotal, na.rm = T), by = c('geo','industry','education')][, EmptoAveg := EmpTotal/Aveg]
getStructure = function(countyPairsList) {
  z = rowid_to_column(countyPairsList, "ID")
  z1 = expand.grid(time,naics,edu,z$ID)
  names(z1) = c('time','industry','education','ID')
  z2 = merge(z1, z, by = 'ID', all = T)
  return(z2)
}
z = getStructure(allpairs)
z1 = qwiIndEdu[, c('dt','geo','industry','education','EmptoAveg','legis')]
z2 = merge(z, z1, by.x = c('time','industry','education','countyFips1'), by.y = c('dt','industry','education','geo'), all.x = T)
names(z2)[(ncol(z2)-1):ncol(z2)] = c('y1','law1')
z3 = merge(z2, z1, by.x = c('time','industry','education','countyFips2'), by.y = c('dt','industry','education','geo'), all.x = T)
names(z3)[(ncol(z3)-1):ncol(z3)] = c('y2','law2')
wrkdata = as.data.table(z3)[, y1_y2 := y1-y2][, law1_law2 := law1-law2]

temp = wrkdata[, lag1_y1_y2 := shift(y1_y2, n = 1, type ='lag'), by = c('industry','education','ID')][, Dy := y1_y2 - lag1_y1_y2][, lag1_law1_law2 := shift(law1_law2, n = 1, type ='lag'), by = c('industry','education','ID')][, Dlaw := law1_law2 - lag1_law1_law2]
temp = temp[, lag1_Dlaw := shift(Dlaw, n = 1, type ='lag'), by = c('industry','education','ID')][, lag2_Dlaw := shift(Dlaw, n = 2, type ='lag'), by = c('industry','education','ID')][, lag3_Dlaw := shift(Dlaw, n = 3, type ='lag'), by = c('industry','education','ID')]

# --- REGRESSION ----
# Create the Structure for Regression
lag = c('0', '1', '2', '3')
data = c('est','p_value')
regResult = array(data = vector(mode='numeric',length=800), dim = c(20,5,4,2), dimnames = list(naics,edu,lag,data))

for (i in 1:length(naics)) {
  for (j in 1:length(edu)) {
    print(paste(i,j))
    z = temp[industry == naics[i] & education == edu[j]]
    reg = felm(Dy ~ Dlaw + lag1_Dlaw + lag2_Dlaw + lag3_Dlaw | time + ID, data = z)
    for (k in 1:4) {
      regResult[[i,j,k,1]] = summary(reg)$coefficients[k,1]
      regResult[[i,j,k,2]] = summary(reg)$coefficients[k,2]
    }
  }
}
# Save the result
#fwrite(temp, file = '1stD_aveg_est_indus_edu.csv')

# --- PLOT ----
temp = data.table(plyr::adply(regResult, c(1,2,3,4), .id = c('industry','education','lag','statistics')))
names(temp)[5] = 'value'
regEstPlot = list()
for (i in 1:5) {
  z = ggplot(temp[education == paste0('E',i) & statistics == 'est'], aes(x=lag, y=value, color=industry, group = industry)) + geom_smooth() + labs(title=paste0('Education Level_',i,'_Cofficients Estimates'), x="Lag", y="Value")
  regEstPlot[[i]] = z
}

# --- TEST CODE ----
#Store in a Numeric Array
# est = array(data = vector(mode='numeric',length=100), dim = c(20, 5), dimnames = list(naics,edu))
# p_value = array(data = vector(mode='numeric',length=100), dim = c(20, 5), dimnames = list(naics,edu))
#store in a list arrayz

# Add naics lable into the reg result
# temp = data.table(est, keep.rownames = T)
# temp1 = data.table(p_value, keep.rownames = T)
# temp = merge(temp,schmea$qwi$industry[,-3], by.x = 'rn', by.y = 'industry', all.x = T)
# setcolorder(temp, neworder = c("rn","label","E1","E2","E3","E4","E5"))
# temp1 = merge(temp1,schmea$qwi$industry[,-3], by.x = 'rn', by.y = 'industry', all.x = T)
# setcolorder(temp1, neworder = c("rn","label","E1","E2","E3","E4","E5"))