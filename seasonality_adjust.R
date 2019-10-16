setproj(19)
#library(ggplot2)
library(tidyverse)
library(dplyr)
library(data.table)
library(lubridate)
library(lfe)
library(seasonal)
library(seasonalview)
library(ggplot2)

# --- Pass One to The Package ---
setwd('/Users/zhaochenhe/Google Drive/research/proj_019_hb56')
qwi = fread('wrkdata/qwi_naics2_eth_edu.csv')
qwi = qwi[, yq := paste(year,'-',quarter)]
qwi = qwi[, dt := yq(yq)]
qwi = qwi[,statefipsn := floor(geography/1000)][,cntyfipsn := geography - statefipsn*1000]
qwi = qwi[, c("dt","geography","statefipsn","cntyfipsn","industry","race","ethnicity","education","EmpTotal","Payroll")]
names(qwi)[2] = 'geo'
qwi = qwi[!duplicated(qwi)]


# Write The Function
adjust = function(x) {
  z = ts(x, start = 2009, frequency = 4)
  z = final(seas(z))
  return(z)
}

# --- Small Set of Data ----
my_ts = qwi[geo == 1003 & industry == '00' & race == 'A0' & ethnicity == 'A0' & education == 'E0', c('dt','EmpTotal')]
z = adjust(my_ts$EmpTotal)

my_ts = data.table(my_ts, z)
names(my_ts) = c('time','raw','adjusted')

ggplot(my_ts, aes(x=time)) +
  geom_line(aes(y = raw), color ="darkred") +
  geom_line(aes(y = adjusted), color="steelblue")


# --- Apply to All ----
samp = qwi[race == 'A0' & ethnicity == 'A0'][,c('dt','geo','industry','education','EmpTotal')]
# samp[is.na(samp$EmpTotal) == T]$EmpTotal = 0  #set all NA to zero
# samp[, Adjusted := adjust(EmpTotal), by = c('geo','industry','education')]
geo = unique(samp$geo)
indus = unique(samp$industry)
edu = unique(samp$education)

i = 2013
j = '11'
k = 'E3'
my_ts = samp[geo == i & industry == j & education == k, EmpTotal]
z = adjust(my_ts)

my_ts = data.table(my_ts, z)
names(my_ts) = c('time','raw','adjusted')

ggplot(my_ts, aes(x=time)) +
  geom_line(aes(y = raw), color ="darkred") +
  geom_line(aes(y = adjusted), color="steelblue")

