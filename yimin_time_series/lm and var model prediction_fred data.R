library(fredr)
library(tidyverse)
library(lubridate)
library(stats)
library(vars)

# --- GET THE DATA ----
fredr_set_key("d803a867a9418b83a12ecaeeccc128a7")

unemp = as.data.table(fredr(
  series_id = "UNRATE",
  observation_start = as.Date("1960-01-01"),
  frequency = 'q'))

cpi = as.data.table(fredr(
  series_id = "CPALTT01USQ657N",
  observation_start = as.Date("1960-01-01"),
  frequency = 'q'))

gdppc = as.data.table(fredr(
  series_id = "A939RX0Q048SBEA",
  observation_start = as.Date("1960-01-01"),
  frequency = 'q'))

data = list(unemp, cpi, gdppc)
names(data) = c('unemp','cpi','gdppc')

for (i in 1:3) {
  z = data[[i]]
  z = z[,-2]
  names(z)[2] = names(data)[i]
  data[[i]] = z
}

z = merge(data$unemp,data$cpi,by='date',all=T)
z = merge(z, data$gdppc, by='date',all = T)
dat = z

# --- WRITE THE FUNCTION ----
# this function selects the training set and testing set
getSets = function(start_point, length_training, length_testing) {
  training = dat[date >= start_point & date <= start_point+length_training]
  testing = dat[date > start_point+length_training & date <= start_point+length_training+length_testing]
  output = list(training,testing)
  names(output) = c('train','test')
  return(output)
} # note: the unit of length argument in getSets is DAY

# this function selects the a series of training sets and testing sets, whose starting points has a certain frequency, respectively
getAllSets = function(dat,start_point, frequency, length_training, length_testing) {
  getSets = function(dat,start_point, length_training, length_testing) {
    training = dat[date >= start_point & date < start_point+length_training]
    testing = dat[date >= start_point+length_training & date < start_point+length_training+length_testing]
    if (max(dat$date) < start_point+length_training+length_testing) {
      warning('Ending date exceeds final date in the data')
    }
    output = list(training,testing)
    names(output) = c('train','test')
    return(output)
  }
  set = list()
  set$training_set = list()
  set$testing_set = list()
  dates = seq.Date(start_point, as.Date('2019-04-01'), by = frequency)
  for (i in 1:length(dates)) {
    z = getSets(dat,dates[i], length_training, length_testing)
    set$training_set[[i]] = z$train
    names(set$training_set)[i] = as.character(dates[i])
    set$testing_set[[i]] = z$test
    names(set$testing_set)[i] = as.character(dates[i])
  }
  return(set)
}

# --- TRAIN THE DATA ----
allSet = getAllSets(dat,as.Date('1960-01-01'), '1 quarter', months(120), months(24))

# --- Train without lag (Linear Tegression) ---
myArray = array(data = vector(mode = "list"), dim = c(238,3), dimnames = list(as.character(dat$date), c('unemp', 'cpi', 'gdppc')))
var = c("unemp","cpi","gdppc")
# trainings = array(data = vector(mode = "list"), dim = c(nrow(dat),(ncol(dat)-1)), dimnames = list(as.character(dat$date), names(dat)[-1]))
# var = names(dat)[-1]
for (i in 1:length(allSet$training_set)) {
  for (j in 1:length(var)) {
    reg = lm(as.formula(paste(c(var[j],paste(var[-j],collapse='+')),collapse='~')), data = allSet$training_set[[i]])
    myArray[[i,j]] = reg
    # if (j == 1) {
    #   names(myArray)[i] = paste(names(allSet$training_set)[i], var[j], sep = ' ')
    # } else if (j == 2) {
    #   names(myArray)[i+238] = paste(names(allSet$training_set)[i], var[j], sep = ' ')
    # } else {
    #   names(myArray)[i+238*2] = paste(names(allSet$training_set)[i], var[j], sep = ' ')
    # }
  }
}

# --- Train with 1st lag (Linear Tegression) ---
# add 1st lag for each variable
for (i in 1:length(allSet$training_set)) {
  allSet$training_set[[i]] = allSet$training_set[[i]][, unemp_l1 := shift(unemp, n=1, type = 'lag')][, cpi_l1 := shift(cpi, n=1, type = 'lag')][, gdppc_l1 := shift(gdppc, n=1, type = 'lag')]
  allSet$testing_set[[i]] = allSet$testing_set[[i]][, unemp_l1 := shift(unemp, n=1, type = 'lag')][, cpi_l1 := shift(cpi, n=1, type = 'lag')][, gdppc_l1 := shift(gdppc, n=1, type = 'lag')]
}

myArray = array(data = vector(mode = "list"), dim = c(238,3), dimnames = list(as.character(dat$date), c('unemp', 'cpi', 'gdppc')))
var = c("unemp","cpi","gdppc","unemp_l1","cpi_l1","gdppc_l1")

for (i in 1:length(allSet$training_set)) {
  for (j in 1:(length(var)/2)) {
    reg = lm(as.formula(paste(c(var[j],paste(var[4:6][-j],collapse='+')),collapse='~')), data = allSet$training_set[[i]])
    myArray[[i,j]] = reg
  }
}

# --- Using the Trained Data to Predict ----
# this function returns a table of the predicted variables' values for a specified number of periods
getPrediction = function(models, n, time, dat) {
  initial_data = dat[date == time]
  names(initial_data)[-1] = paste(names(initial_data)[-1],'l1', sep = '_')
  prediction = matrix(0, nrow = n, ncol = length(models))
  colnames(prediction) = names(initial_data)[-1]
  for (i in 1:n) {
    for (j in 1:length(models)) {
      prediction[i,j] = predict(models[[j]], newdata = initial_data)
    }
    initial_data = data.table(t(prediction[i,]))
  }
  date = as.Date(time) + months(3)*(1:n)
  prediction = data.table(cbind.data.frame(date, prediction))
  return(prediction)
} # "models" argument should be a list of three regression results, each one corresponds to a dependend variable

models = list(myArray[['2005-01-01','unemp']],myArray[['2005-01-01','cpi']],myArray[['2005-01-01','gdppc']])
z1 = getPrediction(models, 40, '2005-01-01',dat)
names(z1) = c('date','unemp_est','cpi_est','gdppc_est')

# --- Plot the Estimates and Real Data --- 
dates = as.Date('2005-01-01') + months(3)*(1:40)
z = dat[date %in% dates]

z = merge(z,z1,by = 'date', all.x = T)
z = melt(z, id = 'date')

unemp = c('unemp','unemp_est')
cpi = c('cpi','cpi_est')
gdppc = c('gdppc','gdppc_est')

temp = z[variable %in% cpi]
ggplot(data = temp) + geom_line(aes(x = date, y = value, group = variable, color = variable))


# --- Predict with VARS package ---
N = 3 # number of lags
n = 40 # number of periods to predict
time = '2007-01-01' # the previous period of the starting point
dat = dat

varModel = VAR(train, p = N, type = "const")

z = predict(varModel, n.ahead = 40)
var_est = z$fcst$unemp[,1]

my_est = temp[date>= '2007-01-01' &  variable == 'unemp_est']
my_est = my_est[-nrow(my_est)]

z1 = cbind(my_est, var_est)
z1 = z1[,-(1:2)]
plot(z1)
