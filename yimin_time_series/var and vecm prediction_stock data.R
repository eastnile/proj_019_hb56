library(quantmod)
library(tidyverse)
library(stats)
library(vars)
library(data.table)
library(tsDyn)

# --- Input ---
stocks = c('AAPL','MSFT','JPM','BA','CAT','JNJ','GE','F','NKE','MCD','KO','DIS')
#stocks = c('AAPL','MSFT','AMZN','GOOGL')
train_startpoint = '2005-01-03' # specify the starting point of the training set
train_length = 365*8
N = 10 # number of lags
n.power = 1 # number of powers
n = 15 # number of days to predict
plot = 'MSFT'

# --- Upload the Data ---
getSymbols(stocks, src = 'yahoo', from="2000-01-01") # the start date here can also be customized

# --- Process ----
#stocks = c('GSPC','AAPL','MSFT','JPM','BA','CAT','JNJ','GE','F','NKE','MCD','KO','DIS') # fix the issue of ^GSPC & GSPC
for (object in stocks) {
  z = data.table(get(object), keep.rownames = T)[,c(1,5)]
  assign(object,z)
}
z = get(stocks[1])
for (object in stocks[-1]) {
  z = merge(z, get(object), all.x = T)
}
z = z[, ID := .I]
if (n.power >1) {
  for (i in 2:n.power) {
    for (stock in stocks) {
      z = z[, paste0('power',i,'.',stock,'.Close') := get(paste0(stock,'.Close'))^i]
    }
  }
}
dat = z

# train
train = dat[index %in% as.Date(train_startpoint):(as.Date(train_startpoint)+train_length-1)][,-c('index','ID')]
varModel = VAR(train, p = N, type = "const")
vecmModel = VECM(train, N, include = 'const', estim = 'ML')

# predict
varPred = predict(varModel, n.ahead = n)
vecmPred = predict(vecmModel, n.ahead = n)

# re-organize the result for var prediction
z = matrix(data = 0, nrow = n, ncol = length(varPred$fcst))
for (i in 1:length(varPred$fcst)) {
  z1 = varPred$fcst[[i]][,1]
  z[,i] = z1
}
z = data.table(z)
if (n.power>1) {
  z = z[, 1:length(stocks)]
}
names(z) = paste0(stocks, '.var.est')
z = z[, ID := (dat[index == as.Date(train_startpoint)][,ID]+nrow(train)):(dat[index == train_startpoint][,ID]+nrow(train)+n-1)]

# re-organize the result for vecm prediction
z1 = data.table(vecmPred)
if (n.power>1) {
  z1 = z1[, 1:length(stocks)]
}
names(z1) = paste0(stocks, '.vecm.est')
z1 = z1[, ID := (dat[index == as.Date(train_startpoint)][,ID]+nrow(train)):(dat[index == train_startpoint][,ID]+nrow(train)+n-1)]

# merge actual data, var prediction, and vecm prediction
z = merge(dat,z,by = 'ID', all.y = T)
z = merge(z, z1, by = 'ID')
z = melt(z[,-1], id = 'index')

# plot
plot = 'AAPL'
object = paste0('^',plot)
plotdata = z[variable %in% unique(grep(object,z$variable,value = T))]
ggplot(data = plotdata) + geom_line(aes(x = index, y = value, group = variable, color = variable))




