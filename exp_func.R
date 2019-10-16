setproj(19)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(data.table)
library(lfe)
library(seasonal)
library(seasonalview)

getSample = function(duration,law_start,law_duration,phase,degree_of_impact) {
  z = degree_of_impact
  v1 = 1:duration
  v2 = vector(mode="numeric", length=length(v1))
  v2[law_start:(law_start+law_duration-1)] = 1
  v3 = vector(mode="numeric", length=length(v1))
  getv3 = function(x, phase) {
    z = (x-phase)%%4
    if (z == 0) {
      return(0)
    } else if (z == 1) {
      return(1)
    } else if (z == 2) {
      return(0)
    } else {
      return(-1)
    }
  }
  for (i in v1) {
    v3[i] = getv3(i,phase)
    if (v2[i] == 1) {
      v3[i] = v3[i] + degree_of_impact
      degree_of_impact = degree_of_impact + z
    }
  }
  return(list(law = v2, emp = v3))
}

counties = 1:200
qtr = 1:duration

duration = 80
law_start = 20
law_duration = 18
phase = 1
degree_of_impact = 0.005

dt = as.data.table(expand.grid(county = counties,qtr = qtr))
z = getSample(duration,law_start,law_duration,phase,degree_of_impact)
dt[,law:= z$law, by=county][,emp:= z$emp, by=county]
dt = dt[,lag_law:= shift(law, n=1), by = county]
dt = dt[,lag_emp:= shift(emp, n=1), by = county]
dt = dt[, dlaw := law - lag_law]
dt = dt[, demp := emp - lag_emp]

# Seasonal Package
z = dt[county == 1, c("county","qtr","law","emp")]
my_ts = ts(z$emp, start = 2000, frequency = 4)
m = seas(my_ts)
#plot(m)
view(m)


# Plot
# dat = dt[county == 1,c('qtr','law','emp')]
# ggplot(dat, aes(x=qtr)) +
#   geom_line(aes(y = law), color ="darkred") +
#   geom_line(aes(y = emp), color="steelblue")

# Reg
# dt$qtr = as.factor(dt$qtr)
# reg1 = lm(demp ~ dlaw + qtr, dt)
# summary(reg1)$coefficients[2,]
