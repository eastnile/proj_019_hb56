setproj(19)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(data.table)
library(lfe)

# Time Variable-Quarter
v1 = 1:40

# Law Variable
v2 = vector(mode="numeric", length=length(v1))
v2[10:21] = 1

# Employment Variable with Seasonal Pattern
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
v3 = vector(mode="numeric", length=length(v1))
for (i in v1) {
  v3[i] = getv3(i,4)
  if (v2[i] == 1) {
    v3[i] = v3[i] + 2
  }
}

dat = data.table(v1,v2,v3)

# --- TEST CODE --- 
# z = melt(dat, id.vars = v1)
# ggplot(data=dat, aes(x=v1, y=v2, group=1)) +
#   geom_line()

ggplot(dat, aes(x=v1)) + 
  geom_line(aes(y = v2), color = "darkred") + 
  geom_line(aes(y = v3), color="steelblue") 












# --- TEST CODE ---
#v2 = 10*(sin(0.01*v1)+2)
#v2 = v2 + 3*rnorm(length(v1))

# # Create Law Variable (with repeated pattern)
# v3 = vector(mode="numeric", length=length(v1))
# duration = 12
# getv3 = function(x) {
#   z = x%/%duration
#   if((z %% 2) == 0) {
#     return(0)
#   } else {
#     return(1)
#   }
# }
# for (i in 1:length(v1)) {
#   v3[i] = getv3(i)
# }
