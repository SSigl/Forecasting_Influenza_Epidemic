#=======================================================================================#


    
#                       Epidemies : régression (LASSO, RIDGE)                           #



#=======================================================================================#

source("/Users/SuzanneSigalla/Documents/ENSAE/3A/Semestre 1/S1 StatsML/Projet EDF/R script/epidemies_series_temp_base.R")

err_lasso = data.frame("region" =code_reg,"rmse"=0,"mae"=0,"mase"=0,"rae"=0,"rrse"=0)
err_ridge = data.frame("region" =code_reg,"rmse"=0,"mae"=0,"mase"=0,"rae"=0,"rrse"=0)

# one problem : region 21 for which regression does not work
code_reg_rest <- code_reg[-which(code_reg==21)]

for(i in code_reg_rest){
  print(i)
  data_train = get(paste("data_train",i,sep="_"))
  data_test = get(paste("data_test",i,sep="_"))
  data_train <- data_train %>% drop_na(Temp, PopTot)
  data_test <- data_test %>% drop_na(Temp, PopTot)
  data_train.matrix = data.matrix(data_train[,c("Temp","PopTot","inclog_lag")])
  data_test.matrix = data.matrix(data_test[,c("Temp","PopTot","inclog_lag")])
  x <- as.matrix(data_train[,c("Temp","PopTot","inclog_lag")])
  y <- data_train$inclog
  
  for(reg in c("lasso","ridge")){
  if(reg=="lasso"){alpha=1}else{alpha=0}
  assign(paste(reg,"reg",i ,sep="_"), glmnet(x, y = y, alpha = alpha))
  assign(paste("y_predict", reg,sep="_"),predict(get(paste(reg,"reg", i,sep="_")), data_test.matrix))
  for(type_err in c("rmse","mae","mase","rae","rrse")){
    assign(paste(type_err,reg,sep="_"),apply(get(paste("y_predict", reg,sep="_")),2,type_err,actual=data_test$inclog))
    err_int <- get(paste("err",reg,sep="_"))
    err_int[which(err_int$region==i),type_err] <- min(get(paste(type_err,reg,sep="_")))
    assign(paste("err",reg,sep="_"),err_int)}
  }
  
  par(mfrow=c(1,2))
  plot(y_predict_lasso[,which.min(mase_lasso)],col='black',type = 'l',main=paste("Lasso for region",i,sep=" "))
  lines(data_test$inclog,col='blue',lty=2)
  plot(y_predict_ridge[,100],col='black',type = 'l',main=paste("Ridge for region",i,sep=" "))
  lines(data_test$inclog,col='green',lty=2)
}

write.csv(err_lasso, file = "/Users/SuzanneSigalla/Documents/ENSAE/3A/Semestre 1/S1 StatsML/Projet EDF/data/data results/err_lasso.csv")
write.csv(err_ridge, file = "/Users/SuzanneSigalla/Documents/ENSAE/3A/Semestre 1/S1 StatsML/Projet EDF/data/data results/err_ridge.csv")


