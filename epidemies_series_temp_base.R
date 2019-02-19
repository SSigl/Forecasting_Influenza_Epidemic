#=======================================================================================#



#                      Epidemies et series temporelles : base                           #
  


#=======================================================================================#
rm(list = ls())

#%%%%%%%%%
# PACKAGES
library(dygraphs)
library(xts)
library(MASS)
library(caret)
library(lubridate)
library(glmnet)
library(dplyr)
library(readr)
library(stringr)
library(tibble)
library(forecast)
library(RColorBrewer)
library(magrittr)
library(zoo)
library(tseries)
library(lmtest)
library(ggplot2)
library(tidyr)
library(Metrics)
forecast::accuracy



#%%%%%%%%%%%%%%%%%%%%%%%%
# IMPORTATION DES DONNÉES

# ensemble des données
data <- read_csv("/Users/SuzanneSigalla/Documents/ENSAE/3A/Semestre 1/S1 StatsML/Projet EDF/data/data merged/Merged_data.csv")
data$inclog <- log(data$inc100+1)
data$inclog_lag <- log(data$inc100lag+1)

# données par région
code_reg <- levels(as.factor(data$geo_insee))
for(i in code_reg){
  df <- subset(data, data$geo_insee == i)
  df$Temp <- 0.5*(df$TMax.moy+df$TMin.moy)
  assign(paste("data",i,sep="_"),df)
  sel=which(year(df$date) < 2016)
  assign(paste("data","train",i,sep="_"),df[sel,])
  assign(paste("data","test",i,sep="_"),df[-sel,])
  rm(df)
}



#%%%%%%%%%%%%%%%%%%%%
# FONCTIONS GÉNÉRALES


rollHW <- function (hw.model, ynew, horizon = 1) 
{
  prevHW <- array(0, dim = length(ynew))
  prevHW[1] <- forecast(hw.model, h = horizon)$mean[horizon]
  refit <- hw.model
  for (i in 1:(length(ynew) - 1)) {
    if(hw.model$gamma==T)
    {
      ts2 <- ts(c(hw.model$x, ynew[1:i]), frequency = frequency(fit.hw$x))
      refit <- HoltWinters(ts2, seasonal=hw.model$seasonal,l.start=refit$coefficients["a"], b.start=refit$coefficients["b"]
                           , s.start=refit$coefficients[grep("s",names(fit.hw$coefficients))]
                           , optim.start = c(alpha = refit$alpha, beta = refit$beta, gamma = refit$gamma))
    }
    
    else{
      ts2 <- ts(c(hw.model$x, ynew[1:i]), frequency = frequency(fit.hw$x))
      refit <- HoltWinters(ts2,l.start=refit$coefficients["a"], b.start=refit$coefficients["b"]
                           , optim.start = c(alpha = refit$alpha, beta = refit$beta, gamma = F))
    }
    prevHW[i + 1] <- forecast(refit, h = horizon)$mean[horizon]
  }
  return(prevHW)
}



rollETS <- function (ets.model, ynew, horizon = 1) 
{
  prevETS <- array(0, dim = length(ynew))
  prevETS[1] <- forecast(ets.model, h = horizon)$mean[horizon]
  for (i in 1:(length(ynew) - 1)) {
    ts2 <- ts(c(ets.model$x, ynew[1:i]), frequency=frequency(ets.model$x))
    refit <- ets(ts2, model = ets.model)
    prevETS[i + 1] <- forecast(refit, h = horizon)$mean[horizon]
  }
  return(prevETS)
}

rollArima<-function (arima.model, ynew, horizon = 1) 
{
  prevARIMA <- array(0, dim = length(ynew))
  prevARIMA[1] <- forecast(arima.model, h = horizon)$mean[horizon]
  for (i in 1:(length(ynew) - 1)) {
    ts2 <- c(arima.model$x, ynew[1:i])
    refit <- Arima(ts2, model = arima.model)
    prevARIMA[i + 1] <- forecast(refit, h = horizon)$mean[horizon]
  }
  return(prevARIMA)
}



