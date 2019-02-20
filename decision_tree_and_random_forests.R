#=======================================================================================#



#                           Epidemies : arbres et forêts aléatoires                     #



#=======================================================================================#

source("/Users/SuzanneSigalla/Documents/ENSAE/3A/Semestre 1/S1 StatsML/Projet EDF/R script/epidemies_series_temp_base.R")

#err_tree = data.frame("region" =code_reg,"rmse"=0,"mae"=0,"mase"=0,"rae"=0,"rrse"=0)
#err_bagging = data.frame("region" =code_reg,"rmse"=0,"mase"=0)
#err_rf = data.frame("region" =code_reg,"rmse"=0,"mase"=0)

eq<-inclog ~ inclog_lag + TMin.moy + TMin.max +  TMin.min + TMax.moy + TMax.min + TMax.max + PopTot 

# %%%%%%
# ARBRES

for(i in code_reg){
  print(i)
  data_train = get(paste("data_train",i,sep="_"))
  data_test = get(paste("data_test",i,sep="_"))
  assign(paste("rpart",i,sep="."), rpart(eq, data = data_train,control=c(maxsurrogate=6)))
  assign(paste("rpart0.forecast",i,sep="."), predict(get(paste("rpart",i,sep=".")),newdata=data_test))
  
  for(type_err in c("rmse","mae","mase","rae","rrse")){
    print(type_err)
    fct_err <- get(type_err)
    err_int <- get("err_tree")
    err_int[which(err_int$region==i),type_err] <- fct_err(actual=data_test$inclog+1,predicted = get(paste("rpart0.forecast",i,sep=".")))
    assign("err_tree",err_int)
  }
}


plot(data_test$date, data_test$inclog, type='l',col='red',ylab="Taux d'incidence",xlab="Date",main="Prediction du taux d'incidence en Ile-de-France en utilisant un arbre de regression")
lines(data_test$date, rpart0.forecast.11, type = 'l', col='blue')
legend("topright",c("Data","Prevision"),col=c("red","blue"),lty=c(1,1))

# %%%%%%%
# BAGGING

# bagging<-function(Nbag,data_app,size =nrow(data_app) ) 
# {
#   n<-size  
#   rpart.bagg<-list()
#   for(i in c(1:Nbag))
#   {
#     s<-sample(c(1:n),replace=TRUE) 
#     data.bagg<-data_app[s,] 
#     rpart.bagg[[i]]<- rpart(eq<-inclog ~ inclog_lag + TMin.moy + TMin.max +  TMin.min + TMax.moy + TMax.min + TMax.max + PopTot 
#                             , data = data.bagg,control=c(maxsurrogate=1)) 
#   }
#   return(rpart.bagg)
# }
# 
# for(i in code_reg){
#   print(i)
#   data_train = get(paste("data_train",i,sep="_"))
#   data_test = get(paste("data_test",i,sep="_"))
#   Nbag<-100
#   size <- floor(0.8*nrow(data_train)) # 80 % des données
#   assign(paste("rpart.bagg",i,sep="."), bagging(Nbag,data_app=data_train,size=size))
#   assign(paste("bagg.forecast",i,sep="."),lapply(get(paste("rpart.bagg",i,sep=".")),predict,newdata=data_test))
#   assign(paste("bagg.forecast",i,sep="."),matrix(unlist(bagg.forecast),nrow=Nbag,ncol=nrow(data_test),byrow = TRUE))
#   
#   err_bagging[which(err_bagging$region==i),"rmse"]<- rmse(actual=data_test$inclog,predicted = get(paste("bagg.forecast",i,sep=".")))
#   err_bagging[which(err_bagging$region==i),"mase"]<- mase(data_test$inclog,colMeans(get(paste("bagg.forecast",i,sep="."))))
#   }


# %%%%%%%%%%%%%
# RANDOM FOREST
for(i in code_reg){
  print(i)
  data_train = get(paste("data_train",i,sep="_"))
  data_test = get(paste("data_test",i,sep="_"))
  assign(paste("rf",i,sep="."),randomForest(eq,ntree=500,data=data_train, importance=TRUE,na.action=na.roughfix ))
  assign(paste("rf",i,"fitted",sep="."), predict(get(paste("rf",i,sep=".")),newdata=data_train))
  assign(paste("rf",i,"forecast",sep="."), predict(get(paste("rf",i,sep=".")),newdata=data_test))
  
  err_rf[which(err_rf$region==i),"rmse"]<- rmse(actual=data_test$inclog,predicted = get(paste("rf",i,"forecast",sep=".")))
  err_rf[which(err_rf$region==i),"mase"]<- mase(actual=data_test$inclog,predicted = get(paste("rf",i,"forecast",sep=".")))

}

plot(data_test$date, data_test$inclog, type='l',col='red',ylab="Taux d'incidence",xlab="Date",main="Prediction du taux d'incidence en Ile-de-France en utilisant une forêt aléatoire")
lines(data_test$date, rf.11.forecast, type = 'l', col='darkgreen')
legend("topright",c("Data","Prevision"),col=c("red","darkgreen"),lty=c(1,1))

write.csv(err_tree_vois, file = "/Users/SuzanneSigalla/Documents/ENSAE/3A/Semestre 1/S1 StatsML/Projet EDF/data/data results/err_tree_vois.csv")
write.csv(err_bagging, file = "/Users/SuzanneSigalla/Documents/ENSAE/3A/Semestre 1/S1 StatsML/Projet EDF/data/data results/err_baggin.csv")
write.csv(err_rf, file = "/Users/SuzanneSigalla/Documents/ENSAE/3A/Semestre 1/S1 StatsML/Projet EDF/data/data results/err_rf.csv")
