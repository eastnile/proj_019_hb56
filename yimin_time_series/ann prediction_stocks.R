library(keras)
library(tensorflow)
library(tidyverse)
library(devtools)
library(quantmod)
library(data.table)
library(stats)

install_tensorflow()
devtools::install_github("rstudio/tensorflow")

# --- Upload the Stock Data ---
stocks = c('AAPL','MSFT','AMZN','GOOGL') # Spevify which stocks' data to upload
getSymbols(stocks, src = 'yahoo', from="2005-01-01") # Uplead the data

for (object in stocks) {
  z = data.table(get(object), keep.rownames = T)[,c(1,5)]
  assign(object,z)
}
z = get(stocks[1])
for (object in stocks[-1]) {
  z = merge(z, get(object), all.x = T)
}
dat = z[,-1] # these steps merge the date into a single table, which is dat

# --- Prepare the Data ---
getLags = function (y, p, type = c("const", "trend", "both", "none"), 
                    season = NULL, exogen = NULL, lag.max = NULL, ic = c("AIC", 
                                                                         "HQ", "SC", "FPE")) 
{
  y <- as.matrix(y)
  if (any(is.na(y))) 
    stop("\nNAs in y.\n")
  if (ncol(y) < 2) 
    stop("The matrix 'y' should contain at least two variables. For univariate analysis consider ar() and arima() in package stats.\n")
  if (is.null(colnames(y))) {
    colnames(y) <- paste("y", 1:ncol(y), sep = "")
    warning(paste("No column names supplied in y, using:", 
                  paste(colnames(y), collapse = ", "), ", instead.\n"))
  }
  colnames(y) <- make.names(colnames(y))
  y.orig <- y
  type <- match.arg(type)
  obs <- dim(y)[1]
  K <- dim(y)[2]
  if (!is.null(lag.max)) {
    lag.max <- abs(as.integer(lag.max))
    ic <- paste(match.arg(ic), "(n)", sep = "")
    p <- VARselect(y, lag.max = lag.max, type = type, season = season, 
                   exogen = exogen)$selection[ic]
  }
  sample <- obs - p
  ylags <- embed(y, dimension = p + 1)[, -(1:K)]
  temp1 <- NULL
  for (i in 1:p) {
    temp <- paste(colnames(y), ".l", i, sep = "")
    temp1 <- c(temp1, temp)
  }
  colnames(ylags) <- temp1
  yend <- y[-c(1:p), ]
  if (type == "const") {
    rhs <- cbind(ylags, rep(1, sample))
    colnames(rhs) <- c(colnames(ylags), "const")
  }
  else if (type == "trend") {
    rhs <- cbind(ylags, seq(p + 1, length = sample))
    colnames(rhs) <- c(colnames(ylags), "trend")
  }
  else if (type == "both") {
    rhs <- cbind(ylags, rep(1, sample), seq(p + 1, length = sample))
    colnames(rhs) <- c(colnames(ylags), "const", "trend")
  }
  else if (type == "none") {
    rhs <- ylags
    colnames(rhs) <- colnames(ylags)
  }
  if (!(is.null(season))) {
    season <- abs(as.integer(season))
    dum <- (diag(season) - 1/season)[, -season]
    dums <- dum
    while (nrow(dums) < obs) {
      dums <- rbind(dums, dum)
    }
    dums <- dums[1:obs, ]
    colnames(dums) <- paste("sd", 1:ncol(dums), sep = "")
    rhs <- cbind(rhs, dums[-c(1:p), ])
  }
  if (!(is.null(exogen))) {
    exogen <- as.matrix(exogen)
    if (!identical(nrow(exogen), nrow(y))) {
      stop("\nDifferent row size of y and exogen.\n")
    }
    if (is.null(colnames(exogen))) {
      colnames(exogen) <- paste("exo", 1:ncol(exogen), 
                                sep = "")
      warning(paste("No column names supplied in exogen, using:", 
                    paste(colnames(exogen), collapse = ", "), ", instead.\n"))
    }
    colnames(exogen) <- make.names(colnames(exogen))
    tmp <- colnames(rhs)
    rhs <- cbind(rhs, exogen[-c(1:p), ])
    colnames(rhs) <- c(tmp, colnames(exogen))
  }
  x <- as.data.frame(rhs)
  data <- list(x, y)
  names(data) = c('x','y')
  return(data)
} # This function take a table of time series as input, and returns a table that contains 1st-nth lags of all the series

train = dat[1:3000,] # specify how many pieces of data in dat will be the training set
test = dat[3001:nrow(dat),] # similarly, how many pieces of data in dat will be the tesing set

c(train_data, train_labels) %<-% getLags(train, 3, type = 'none')
train_labels = train_labels[-(1:3),]
c(test_data, test_labels) %<-% getLags(test, 3, type = 'none')
test_labels = test_labels[-(1:3),] # these code specify the dependend variable ("_labels") and independent variables ("_data") for the training set and testing set

# --- Normalize the Data---
train_data <- scale(train_data)
col_means_train <- attr(train_data, "scaled:center") 
col_stddevs_train <- attr(train_data, "scaled:scale")
test_data <- scale(test_data, center = col_means_train, scale = col_stddevs_train)

# --- Create the ANN Model ---
build_model <- function() {
  model <- keras_model_sequential() %>%
    layer_dense(units = (dim(train_labels)[2]+dim(train_data)[2])/2, activation = "relu",
                input_shape = dim(train_data)[2]) %>%
    layer_dense(units = (dim(train_labels)[2]+dim(train_data)[2])/2, activation = "relu") %>%
    layer_dense(units = dim(train_labels)[2])
  model %>% compile(
    loss = "mse",
    optimizer = optimizer_rmsprop(),
    metrics = list("mean_absolute_error")
  )
  model
}
model <- build_model()
model %>% summary()

# --- Train the Data ---
# Display training progress by printing a single dot for each completed epoch.
print_dot_callback <- callback_lambda(
  on_epoch_end = function(epoch, logs) {
    if (epoch %% 80 == 0) cat("\n")
    cat(".")
  }
)    

epochs <- 500
# Fit the model and store training stats
history <- model %>% fit(
  train_data,
  train_labels,
  epochs = epochs,
  validation_split = 0.2,
  verbose = 0,
  callbacks = list(print_dot_callback)
)
#plot(history, metrics = "mean_absolute_error", smooth = FALSE)

# --- Prediction ---
# Predict every new period with actual data for the previous three periods
n = 200 # specify the number of periods to predict

prediction = data.table(cbind(predict(model,test_data)[1:n,2],rep('pred',length.out = n)))[, ID := .I]
actual = data.table(cbind(test_labels[1:n,2],rep('actual',length.out = n)))[, ID := .I]
plotdata = rbind(prediction,actual)
names(plotdata) = c('stock_price', 'category', 'date')

ggplot(data = plotdata) + geom_line(aes(x = date, y = stock_price, group = category, color = category)) + theme(axis.text.y=element_blank(), axis.ticks.y=element_blank())

# Predict the data completely based on the initial several pieces of data
n = 100 # specify the number of periods to predict

prediction = matrix(0, nrow = n, ncol = length(stocks)*3) # 3 lags + current value
colnames(prediction) = colnames(train_data)
prediction[1:3,] = test_data[1:3,]

for (i in 1:n) {
  temp = predict(model,prediction[i:(i+1),])[1,]
  temp = data.table(t(c(temp, rep(0, length.out = 8))))
  temp = scale(temp, center = col_means_train, scale = col_stddevs_train)[1:4]
  
  prediction[i+1, 1:4] = temp
  prediction[i+2, 5:8] = temp
  prediction[i+3, 9:12] = temp
}
pred = data.table(prediction[1:n,1],rep('pred',length.out = n))[, ID := .I]
actual = data.table(test_data[1:n,1],rep('actual',length.out = n))[, ID := .I]
plotdata = rbind(pred,actual)
names(plotdata) = c('stock_price', 'category', 'date')

ggplot(data = plotdata) + geom_line(aes(x = date, y = stock_price, group = category, color = category))
