#=======================================================================================#



#                              Epidemies : séries temporelles                           #



#=======================================================================================#

source("/Users/SuzanneSigalla/Documents/ENSAE/3A/Semestre 1/S1 StatsML/Projet EDF/R script/epidemies_series_temp_base.R")

err_arima_aic = data.frame("region" =code_reg,"rmse"=0,"mae"=0,"mase"=0,"rae"=0,"rrse"=0)
err_ets = data.frame("region" =code_reg,"rmse"=0,"mae"=0,"mase"=0,"rae"=0,"rrse"=0)
err_hw = data.frame("region" =code_reg,"rmse"=0,"mae"=0,"mase"=0,"rae"=0,"rrse"=0)
err_hw_mu = data.frame("region" =code_reg,"rmse"=0,"mae"=0,"mase"=0,"rae"=0,"rrse"=0)
err_hw_ad = data.frame("region" =code_reg,"rmse"=0,"mae"=0,"mase"=0,"rae"=0,"rrse"=0)

for(i in code){
  print(i)
  data_train = get(paste("data_train",i,sep="_"))
  data_test = get(paste("data_test",i,sep="_"))
  
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  # SARIMA
  ts0 <- ts(data_train$inclog, frequency=52)
  #ts0 <- diff(diff(ts(data_train$inclog,frequency=52), lag=52))
  #par(mfrow=c(1,2))
  #acf(ts0, lag = 52*7,main=paste("ACF pour",i,sep=" "))
  #pacf(ts0, lag = 52*7,main=paste("PACF pour",i,sep=" "))
  #acf(ts0, lag = 52*7,main=paste("PACF pour",i,sep=" "))
  #pacf(ts0, lag = 52*7,main=paste("PACF pour",i,sep=" "))
  crit="aic"
  assign(paste("fit.arima",crit,i,sep="."),auto.arima(ts0,d=1, D=1, max.P=4, max.Q=1, max.p=6, max.q = 2, ic = crit, trace = T, xreg=data_train$Temp))
  assign(paste("prev.arima",crit,i,sep="."),  rollArima(get(paste("fit.arima",crit,i,sep=".")),
                                                        ynew=data_test$inclog,
                                                        horizon=dim(data_test)[1]))
  plot(forecast(fit.arima.aic.11, h=145, xreg= data_test$inclog))
  for(type_err in c("rmse","mae","mase","rae","rrse")){
    print(type_err)
    fct_err <- get(type_err)
    err_int <- get(paste("err_arima",crit,sep="_"))
    err_int[which(err_int$region==i),type_err] <- fct_err(actual=data_test$inclog,predicted = get(paste("prev.arima.with.reg", crit, i ,sep=".")))
    assign(paste("err_arima",crit,sep="_"),err_int)
  }
  #}
  
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  # simple, non saisonnier
  # ts1<-ts(data_train$inclog) 
  # assign(paste("fit.ets",i,sep="."), ets(ts1))
  # assign(paste("prev.ets",i,sep="."), rollETS(get(paste("fit.ets",i,sep=".")),
  #                                     ynew=data_test$inclog,
  #                                     horizon=dim(data_test)[1]))
  # 
  # for(type_err in c("rmse","mae","mase","rae","rrse")){
  #   print(type_err)
  #   fct_err <- get(type_err)
  #   err_int <- get(paste("err_ets",sep="_"))
  #   err_int[which(err_int$region==i),type_err] <- fct_err(actual=data_test$inclog,predicted = get(paste("prev.ets", i ,sep=".")))
  #   assign(paste("err_ets",sep="_"),err_int)
  # }
  
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  # double, non saisonnier
  # ts2<-ts(data_train$inclog, frequency=52)
  # assign(paste("fit.hw",i,sep="."), HoltWinters(ts2, gamma=FALSE))
  # assign(paste("prev.hw",i,sep="."), rollHW(get(paste("fit.hw",i,sep=".")),
  #                                           ynew=data_test$inclog,
  #                                           horizon=dim(data_test)[1]))
  # 
  # for(type_err in c("rmse","mae","mase","rae","rrse")){
  #   print(type_err)
  #   fct_err <- get(type_err)
  #   err_int <- get(paste("err_hw",sep="_"))
  #   err_int[which(err_int$region==i),type_err] <- fct_err(actual=data_test$inclog,predicted = get(paste("prev.hw", i ,sep=".")))
  #   assign(paste("err_hw",sep="_"),err_int)
  # }
  
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  # double, non saisonnier
  ## multiplicatif
  ts3<-ts(data_train$inclog+1, frequency=52)
  assign(paste("fit.hw.mu",i,sep="."), HoltWinters(ts3,alpha = NULL, beta = NULL, gamma = NULL,
                                                   seasonal="multiplicative",
                                                   optim.start = c(alpha = 0.3, beta = 0.1, gamma = 0.1),
                                                   optim.control = list()))
  assign(paste("prev.hw.mu",i,sep="."), rollHW(get(paste("fit.hw.mu",i,sep=".")),
                                               ynew=data_test$inclog,
                                               horizon=dim(data_test)[1]))
  
  for(type_err in c("rmse","mae","mase","rae","rrse")){
    print(type_err)
    fct_err <- get(type_err)
    err_int <- get(paste("err_hw_mu",sep="_"))
    err_int[which(err_int$region==i),type_err] <- fct_err(actual=data_test$inclog+1,predicted = get(paste("prev.hw.mu", i ,sep=".")))
    assign(paste("err_hw_mu",sep="_"),err_int)
  }
  
  ## additif
  # ts4<-ts(data_train$inclog, frequency=52)
  # assign(paste("fit.hw.ad",i,sep="."),HoltWinters(ts4,seasonal="additive"))
  # assign(paste("prev.hw.ad",i,sep="."), rollHW(get(paste("fit.hw.ad",i,sep=".")),
  #                                              ynew=data_test$inclog,
  #                                              horizon=dim(data_test)[1]))
  # 
  # for(type_err in c("rmse","mae","mase","rae","rrse")){
  #   print(type_err)
  #   fct_err <- get(type_err)
  #   err_int <- get(paste("err_hw_ad",sep="_"))
  #   err_int[which(err_int$region==i),type_err] <- fct_err(actual=data_test$inclog+1,predicted = get(paste("prev.hw.ad", i ,sep=".")))
  #   assign(paste("err_hw_ad",sep="_"),err_int)
  #}
  
  
}

for(i in code_reg){
  par(ask=F)
  par(mfrow=c(1,1))
  col.pal<-brewer.pal(2, "Spectral")
  #plot(data_test$inclog,type='l', ylim=c(-5,10),main=paste("Forescast for region", i, sep=" "))
  plot(data_test$inclog,type='l', ylim=c(-5,10),main=paste("Forescast for region", i, sep=" "), ylab = "Taux d'incidence")
  lines(get(paste("prev.arima.aic",i,sep=".")),col=col.pal[1])
  #lines(get(paste("prev.ets",i,sep=".")),col=col.pal[2])
  #lines(get(paste("prev.hw",i,sep=".")),col=col.pal[3])
  lines(get(paste("prev.hw.mu",i,sep=".")),col=col.pal[4])
  #lines(get(paste("prev.hw.ad",i,sep=".")),col=col.pal[5])
  #legend("topright",col=col.pal,legend=c("ARIMA","ETS","HW","HW_mu","HW_ad"),lty=1, cex =0.75)
  legend("topright",col=c(col.pal[1],col.pal[4]),legend=c("ARIMA","HW_mu"),lty=1, cex =0.75)
}


write.csv(err_arima_aic, file = "/Users/SuzanneSigalla/Documents/ENSAE/3A/Semestre 1/S1 StatsML/Projet EDF/data/data results/err_arima_aic.csv")
write.csv(err_ets, file = "/Users/SuzanneSigalla/Documents/ENSAE/3A/Semestre 1/S1 StatsML/Projet EDF/data/data results/err_ets.csv")
write.csv(err_hw, file = "/Users/SuzanneSigalla/Documents/ENSAE/3A/Semestre 1/S1 StatsML/Projet EDF/data/data results/err_hw.csv")
write.csv(err_hw_mu, file = "/Users/SuzanneSigalla/Documents/ENSAE/3A/Semestre 1/S1 StatsML/Projet EDF/data/data results/err_hw_mu.csv")
write.csv(err_hw_ad, file = "/Users/SuzanneSigalla/Documents/ENSAE/3A/Semestre 1/S1 StatsML/Projet EDF/data/data results/err_hw_ad.csv")

