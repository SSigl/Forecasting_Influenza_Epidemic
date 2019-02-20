#=======================================================================================#



#                           Epidemies : agrégation d'experts                            #



#=======================================================================================#

source("/Users/SuzanneSigalla/Documents/ENSAE/3A/Semestre 1/S1 StatsML/Projet EDF/R script/epidemies_series_temp_base.R")

# All experts
experts=cbind(y_predict_lasso[,which.min(mase_lasso)],y_predict_ridge[,100],rpart0.forecast.11,rf.11.forecast,prev.arima.aic.11,prev.hw.mu.11)
colnames(experts)<-c("lasso", "ridge","arbre","forest","arima","arima exponentiel")

# Agregation
agg.online<- mixture(Y = data_test$inclog , experts = experts, model = 'EWA', loss.type = "square", loss.gradient = F)
summary(agg.online)

# Plotting
col<-piratepal("basel", length.out = ncol(experts))
plot(agg.online, pause=F, col=col)
par(mfrow=c(1,1))
matplot(experts, type='l', col='grey',main="Agregation d'expert pour la prevision en Ile-de-France")
lines(data_test$inclog,col='black')
lines(agg.online$prediction, col='blue')
legend("topright",c("experts","taux observe","taux  predit"),col=c("grey","blue","black"),lty=c(1,1,1))
