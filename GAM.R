#=======================================================================================#



#                             Epidemies : modèle GAM                                    #



#=======================================================================================#


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# OBJECTIF : selectionner les variables et construire un modele GAM realisant la meilleur prevision
# CRITERE d'evaluation de la prevision: RMSE et MAPE
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


#=======================================================================================#
#%%%%%%%%%%%%%%%%%%%%
# IMPORTATION DONNEES
full_data<-read.csv("/Users/SuzanneSigalla/Documents/ENSAE/3A/Semestre 1/S1 StatsML/Projet EDF/data/data merged/Donnees_agregees.csv", header = TRUE)
full0 <- subset(full_data, year(full_data$date)<2015)
full1 <- subset(full_data, year(full_data$date)>=2015)

timepop0 <- full0[,c("geo_insee","week_year","week","date","Pop0_19","Pop20_39","Pop40_59","Pop60_74","Pop75p","PopTot","inc100lag","inc100")]

TPweather0 <- full0[,c("geo_insee","week_year","week","date","Pop0_19","Pop20_39","Pop40_59","Pop60_74","Pop75p","PopTot","TMin.min","TMin.moy","TMin.max","TMax.min","TMax.moy","TMax.max","inc100lag","inc100")]
TPweather0 <- subset(TPweather0, !is.na(TPweather0$TMin.min) & !is.na(TPweather0$TMax.min))

TPWmob0 <- full0
TPWmob0 <- subset(TPWmob0, !is.na(TPWmob0$Mobilite_Pr_MCm_1))
#=======================================================================================#


#=======================================================================================#
Nblock<-10
borne_block<-seq(1, nrow(timepop0), length=Nblock+1)%>%floor
block_list<-list()
l<-length(borne_block)
for(i in c(2:(l-1)))
{
  block_list[[i-1]] <- c(borne_block[i-1]:(borne_block[i]-1))
}
block_list[[l-1]]<-c(borne_block[l-1]:(borne_block[l]))
#=======================================================================================#

#=======================================================================================#
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Block Cross Validation for choosing k (lag)

univ<-function(k, block, bs)
{
  g<- gam(inc100~s(inc100lag, k=k, bs=bs), data=timepop0[-block,])
  forecast<-predict(g, newdata=timepop0[block,])
  return(timepop0[block,]$inc100-forecast)
}

K<-c(3:15)
rmseKcr<-lapply(K, function(k){lapply(block_list, univ, k=k, bs='cr')%>%unlist%>%e_rmse} )
rmseKtp<-lapply(K, function(k){lapply(block_list, univ, k=k, bs='tp')%>%unlist%>%e_rmse} )
rmseKps<-lapply(K, function(k){lapply(block_list, univ, k=k, bs='ps')%>%unlist%>%e_rmse} )
rmseKcs<-lapply(K, function(k){lapply(block_list, univ, k=k, bs='cs')%>%unlist%>%e_rmse} )

col<-piratepal("basel", length.out = 9)
plot(K, rmseKcr, type='b', pch=20, ylim= range(rmseKcr, rmseKcs), col=col[1])
lines(K, rmseKtp, type='b', pch=20, col=col[2])
#lines(K, rmseKps, type='b', pch=20, col=col[3])
lines(K, rmseKcs, type='b', pch=20, col=col[4])
legend("top", col=col, c("cr", "tp", "cs"), pch=20, ncol=2, bty='n')

lag_gam <- gam(inc100~s(inc100lag, k=4, bs="cr"), data=timepop0)
summary(lag_gam)
plot(lag_gam)

#=======================================================================================#



#=======================================================================================#
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Block Cross Validation for choosing k (Week)

univ<-function(k, block, bs)
{
  g<- gam(inc100~s(inc100lag, k=4, bs="cr") + s(week, k=k, bs=bs), data=timepop0[-block,])
  forecast<-predict(g, newdata=timepop0[block,])
  return(timepop0[block,]$inc100-forecast)
}

K<-c(5:35)
rmseKcc<-lapply(K, function(k){lapply(block_list, univ, k=k, bs='cc')%>%unlist%>%e_rmse} )
rmseKcp<-lapply(K, function(k){lapply(block_list, univ, k=k, bs='cp')%>%unlist%>%e_rmse} )

plot(K, rmseKcc, type='b', pch=20, ylim= range(rmseKcc, rmseKcp), col=col[1])
lines(K, rmseKcp, type='b', pch=20, col=col[2])
legend("top", col=col, c("cc", "cp"), pch=20, ncol=2, bty='n')

week_gam <- gam(inc100~s(inc100lag, k=4, bs="cr") + s(week, k=28, bs="cp"), data=timepop0)
summary(week_gam)
plot(week_gam)
#=======================================================================================#



#=======================================================================================#
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Block Cross Validation for choosing k (PopTot)
univ<-function(k, block, bs)
{
  g<- gam(inc100~s(inc100lag, k=4, bs="cr") + s(week, k=28, bs="cp") + s(PopTot, k=k, bs=bs), data=timepop0[-block,])
  forecast<-predict(g, newdata=timepop0[block,])
  return(timepop0[block,]$inc100-forecast)
}

K<-c(3:15)
rmseKcr<-lapply(K, function(k){lapply(block_list, univ, k=k, bs='cr')%>%unlist%>%e_rmse} )
rmseKtp<-lapply(K, function(k){lapply(block_list, univ, k=k, bs='tp')%>%unlist%>%e_rmse} )
#rmseKps<-lapply(K, function(k){lapply(block_list, univ, k=k, bs='ps')%>%unlist%>%e_rmse} )
rmseKcs<-lapply(K, function(k){lapply(block_list, univ, k=k, bs='cs')%>%unlist%>%e_rmse} )

plot(K, rmseKcr, type='b', pch=20, ylim= range(rmseKcr, rmseKcs), col=col[1])
lines(K, rmseKtp, type='b', pch=20, col=col[2])
#lines(K, rmseKps, type='b', pch=20, col=col[3])
lines(K, rmseKcs, type='b', pch=20, col=col[3])
legend("top", col=col, c("cr", "tp", "cs"), pch=20, ncol=2, bty='n')

weekpop_gam <- gam(inc100~s(inc100lag, k=4, bs="cr") + s(week, k=28, bs="cp") + s(PopTot, k=6, bs="cs"), data=timepop0)
summary(weekpop_gam)
plot(weekpop_gam)
#=======================================================================================#


#=======================================================================================#
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Block Cross Validation for choosing k (Pop0_19)

univ<-function(k, block, bs)
{
  g<- gam(inc100~s(inc100lag, k=4, bs="cr") + s(week, k=28, bs="cp") + s(PopTot, k=6, bs="cs") + s(Pop0_19, k=k, bs=bs), data=timepop0[-block,])
  forecast<-predict(g, newdata=timepop0[block,])
  return(timepop0[block,]$inc100-forecast)
}

K<-c(5:15)
rmseKcr<-lapply(K, function(k){lapply(block_list, univ, k=k, bs='cr')%>%unlist%>%e_rmse} )
rmseKtp<-lapply(K, function(k){lapply(block_list, univ, k=k, bs='tp')%>%unlist%>%e_rmse} )
#rmseKps<-lapply(K, function(k){lapply(block_list, univ, k=k, bs='ps')%>%unlist%>%e_rmse} )
rmseKcs<-lapply(K, function(k){lapply(block_list, univ, k=k, bs='cs')%>%unlist%>%e_rmse} )

plot(K, rmseKcr, type='b', pch=20, ylim= range(rmseKcr, rmseKcs), col=col[1])
lines(K, rmseKtp, type='b', pch=20, col=col[2])
#lines(K, rmseKps, type='b', pch=20, col=col[3])
lines(K, rmseKcs, type='b', pch=20, col=col[3])
legend("top", col=col, c("cr", "tp", "cs"), pch=20, ncol=2, bty='n')

weekpop2_gam <- gam(inc100~s(inc100lag, k=4, bs="cr") + s(week, k=28, bs="cp") + s(PopTot, k=6, bs="cs") + s(Pop0_19, k=8, bs="cs"), data=timepop0)
summary(weekpop2_gam)
plot(weekpop2_gam)

#=======================================================================================#


#=======================================================================================#
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Block Cross Validation for choosing k (Pop60+)

univ<-function(k, block, bs)
{
  g<- gam(inc100~s(inc100lag, k=4, bs="cr") + s(week, k=28, bs="cp") + s(PopTot, k=6, bs="cs")+ s(Pop0_19, k=8, bs="cs") + s(Pop60_74 + Pop75p, k=k, bs=bs), data=timepop0[-block,])
  forecast<-predict(g, newdata=timepop0[block,])
  return(timepop0[block,]$inc100-forecast)
}

K<-c(3:10)
rmseKcr<-lapply(K, function(k){lapply(block_list, univ, k=k, bs='cr')%>%unlist%>%e_rmse} )
rmseKtp<-lapply(K, function(k){lapply(block_list, univ, k=k, bs='tp')%>%unlist%>%e_rmse} )
#rmseKps<-lapply(K, function(k){lapply(block_list, univ, k=k, bs='ps')%>%unlist%>%e_rmse} )
rmseKcs<-lapply(K, function(k){lapply(block_list, univ, k=k, bs='cs')%>%unlist%>%e_rmse} )

plot(K, rmseKcr, type='b', pch=20, ylim= range(rmseKcs, rmseKtp), col=col[1])
lines(K, rmseKtp, type='b', pch=20, col=col[2])
#lines(K, rmseKps, type='b', pch=20, col=col[3])
lines(K, rmseKcs, type='b', pch=20, col=col[3])
legend("top", col=col, c("cr", "tp", "cs"), pch=20, ncol=2, bty='n')

weekpops_gam <- gam(inc100~s(inc100lag, k=4, bs="cr") + s(week, k=28, bs="cp") + s(Pop0_19, k=8, bs="cs") + s(Pop60_74 + Pop75p, k=3, bs="cs"), data=timepop0, na.action = na.omit)
summary(weekpops_gam)
plot(weekpops_gam, scale=0)
#=======================================================================================#




#=======================================================================================#
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Calcul des résidus pour ajout météo

TPweather0[,"Residus"] <- NA
weekpops_gam$forecast<-predict(weekpops_gam, newdata=TPweather0)
TPweather0[,"Residus"] <-TPweather0$inc100-weekpops_gam$forecast
TPWmob0[,"Residus"] <- NA
weekpops_gam$forecast<-predict(weekpops_gam, newdata=TPWmob0)
TPWmob0[,"Residus"] <-TPWmob0$inc100-weekpops_gam$forecast


Nblock<-10
borne_block<-seq(1, nrow(TPweather0), length=Nblock+1)%>%floor
block_list<-list()
l<-length(borne_block)
for(i in c(2:(l-1)))
{
  block_list[[i-1]] <- c(borne_block[i-1]:(borne_block[i]-1))
}
block_list[[l-1]]<-c(borne_block[l-1]:(borne_block[l]))
#=======================================================================================#



#=======================================================================================#
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Block Cross Validation for choosing k (TMin)

univ<-function(k, block, bs)
{
  g<- gam(Residus~s(TMin.min, k=k, bs=bs) + s(TMin.moy, k=k, bs=bs) + s(TMin.max, k=k, bs=bs), data=TPweather0[-block,])
  #g<- gam(Residus~s(TMin.max, k=k, bs=bs), data=TPweather0[-block,])
  forecast<-predict(g, newdata=TPweather0[block,])
  return(TPweather0[block,]$Residus-forecast)
}

K<-c(3:10)#c(3,5,10,15)#
#rmseKcr<-lapply(K, function(k){lapply(block_list, univ, k=k, bs='cr')%>%unlist%>%e_rmse} )
#rmseKtp<-lapply(K, function(k){lapply(block_list, univ, k=k, bs='tp')%>%unlist%>%e_rmse} )
#rmseKps<-lapply(K, function(k){lapply(block_list, univ, k=k, bs='ps')%>%unlist%>%e_rmse} )
rmseKcs<-lapply(K, function(k){lapply(block_list, univ, k=k, bs='cs')%>%unlist%>%e_rmse} )

plot(K, rmseKcs, type='b', pch=20, ylim= range(rmseKcs), col=col[1])
#lines(K, rmseKtp, type='b', pch=20, col=col[2])
#lines(K, rmseKps, type='b', pch=20, col=col[3])
#lines(K, rmseKcs, type='b', pch=20, col=col[4])
#legend("top", col=col, c("cr", "tp", "cs"), pch=20, ncol=2, bty='n')

weather_gam <- gam(Residus~s(TMin.min, k=5, bs="cs") + s(TMin.moy, k=5, bs="cs") + s(TMin.max, k=5, bs="cs"), data=TPweather0)
summary(weather_gam)
plot(weather_gam)
#=======================================================================================#



#=======================================================================================#
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Block Cross Validation for choosing k (TMax)

univ<-function(k, block, bs)
{
  g<- gam(Residus~s(TMin.min, k=5, bs="cs") + s(TMin.moy, k=5, bs="cs") + s(TMin.max, k=5, bs="cs") + s(TMax.min, k=k, bs=bs) + s(TMax.moy, k=k, bs=bs) + s(TMax.max, k=k, bs=bs), data=TPweather0[-block,])
  #g<- gam(Residus~s(TMin.max, k=k, bs=bs), data=TPweather0[-block,])
  forecast<-predict(g, newdata=TPweather0[block,])
  return(TPweather0[block,]$Residus-forecast)
}

K<-c(3:10)#c(3,5,10,15)#
#rmseKcr<-lapply(K, function(k){lapply(block_list, univ, k=k, bs='cr')%>%unlist%>%e_rmse} )
rmseKtp<-lapply(K, function(k){lapply(block_list, univ, k=k, bs='tp')%>%unlist%>%e_rmse} )
#rmseKps<-lapply(K, function(k){lapply(block_list, univ, k=k, bs='ps')%>%unlist%>%e_rmse} )
rmseKcs<-lapply(K, function(k){lapply(block_list, univ, k=k, bs='cs')%>%unlist%>%e_rmse} )

plot(K, rmseKcs, type='b', pch=20, ylim= range(rmseKcs), col=col[1])
lines(K, rmseKtp, type='b', pch=20, col=col[2])
#lines(K, rmseKps, type='b', pch=20, col=col[3])
#lines(K, rmseKcs, type='b', pch=20, col=col[3])
legend("top", col=col, c("cs", "tp"), pch=20, ncol=2, bty='n')

weathers_gam <- gam(Residus~s(TMin.min, k=5, bs="cs") + s(TMin.moy, k=5, bs="cs") + s(TMin.max, k=5, bs="cs") + s(TMax.min, k=4, bs="cs") + s(TMax.moy, k=4, bs="cs") + s(TMax.max, k=4, bs="cs"), data=TPweather0, na.action = na.omit)
summary(weathers_gam)
plot(weathers_gam, scale=0)
#=======================================================================================#



#=======================================================================================#
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Calcul des résidus pour ajout mobilité


#TPWmob0_na <- subset(TPWmob0, is.na(TPWmob0$TMin.min) | is.na(TPWmob0$TMax.min))

TPWmob0[,"Residus2"] <- NA
weathers_gam$forecast<-predict(weathers_gam, newdata=TPWmob0)
TPWmob0[,"Residus2"] <-TPWmob0$Residus-weathers_gam$forecast
sel = which(is.na(TPWmob0$Residus2))
TPWmob0[sel,"Residus2"] <- TPWmob0[sel,"Residus"]


Nblock<-10
borne_block<-seq(1, nrow(TPWmob0), length=Nblock+1)%>%floor
block_list<-list()
l<-length(borne_block)
for(i in c(2:(l-1)))
{
  block_list[[i-1]] <- c(borne_block[i-1]:(borne_block[i]-1))
}
block_list[[l-1]]<-c(borne_block[l-1]:(borne_block[l]))
#=======================================================================================#


#=======================================================================================#
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Block Cross Validation for choosing k (Mob1)

univ<-function(k, block, bs)
{
  g<- gam(Residus2~s(Mobilite_Sc_MCm_1, k=k, bs=bs) + s(Mobilite_Sc_MRg_1, k=k, bs=bs) + s(Mobilite_Pr_MCm_1, k=k, bs=bs) + s(Mobilite_Pr_MRg_1, k=k, bs=bs), data=TPWmob0[-block,])
  #g<- gam(Residus~s(TMin.max, k=k, bs=bs), data=TPWmob0[-block,])
  forecast<-predict(g, newdata=TPWmob0[block,])
  return(TPWmob0[block,]$Residus2-forecast)
}

K<-c(10:20)#c(3,5,10,15,20)#
#rmseKcr<-lapply(K, function(k){lapply(block_list, univ, k=k, bs='cr')%>%unlist%>%e_rmse} )
#rmseKtp<-lapply(K, function(k){lapply(block_list, univ, k=k, bs='tp')%>%unlist%>%e_rmse} )
#rmseKps<-lapply(K, function(k){lapply(block_list, univ, k=k, bs='ps')%>%unlist%>%e_rmse} )
rmseKcs<-lapply(K, function(k){lapply(block_list, univ, k=k, bs='cs')%>%unlist%>%e_rmse} )

plot(K, rmseKcs, type='b', pch=20, ylim= range(rmseKcs), col=col[1])
#lines(K, rmseKtp, type='b', pch=20, col=col[2])
#lines(K, rmseKps, type='b', pch=20, col=col[3])
#lines(K, rmseKcs, type='b', pch=20, col=col[3])
#legend("top", col=col, c("cr", "tp", "cs"), pch=20, ncol=2, bty='n')

mob_gam <- gam(Residus2~s(Mobilite_Pr_MRg_1, k=16, bs="cs"), data=TPWmob0)
summary(mob_gam)
plot(mob_gam)
#=======================================================================================#




#=======================================================================================#
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Block Cross Validation for choosing k (Mob2)

univ<-function(k, block, bs)
{
  g<- gam(Residus2~s(Mobilite_Pr_MRg_1, k=16, bs="cs") + s(Mobilite_Sc_MCm_2, k=k, bs=bs) + s(Mobilite_Sc_MRg_2, k=k, bs=bs) + s(Mobilite_Pr_MCm_2, k=k, bs=bs) + s(Mobilite_Pr_MRg_2, k=k, bs=bs), data=TPWmob0[-block,])
  #g<- gam(Residus~s(TMin.max, k=k, bs=bs), data=TPWmob0[-block,])
  forecast<-predict(g, newdata=TPWmob0[block,])
  return(TPWmob0[block,]$Residus2-forecast)
}

K<-c(3:15)#c(3,5,10,15,20)#
rmseKcr<-lapply(K, function(k){lapply(block_list, univ, k=k, bs='cr')%>%unlist%>%e_rmse} )
#rmseKtp<-lapply(K, function(k){lapply(block_list, univ, k=k, bs='tp')%>%unlist%>%e_rmse} )
#rmseKps<-lapply(K, function(k){lapply(block_list, univ, k=k, bs='ps')%>%unlist%>%e_rmse} )
rmseKcs<-lapply(K, function(k){lapply(block_list, univ, k=k, bs='cs')%>%unlist%>%e_rmse} )

plot(K, rmseKcr, type='b', pch=20, ylim= range(rmseKcr,rmseKtp), col=col[1])
#lines(K, rmseKtp, type='b', pch=20, col=col[2])
#lines(K, rmseKps, type='b', pch=20, col=col[3])
lines(K, rmseKcs, type='b', pch=20, col=col[2])
legend("top", col=col, c("cr", "cs"), pch=20, ncol=2, bty='n')

mob2_gam <- gam(Residus2~s(Mobilite_Pr_MRg_1, k=16, bs="cs"), data=TPWmob0)
summary(mob2_gam)
plot(mob2_gam)
#=======================================================================================#




#=======================================================================================#
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Block Cross Validation for choosing k (Mob3)

univ<-function(k, block, bs)
{
  g<- gam(Residus2~s(Mobilite_Pr_MRg_1, k=16, bs="cs") + s(Mobilite_Sc_MCm_3, k=k, bs=bs) + s(Mobilite_Sc_MRg_3, k=k, bs=bs) + s(Mobilite_Pr_MCm_3, k=k, bs=bs) + s(Mobilite_Pr_MRg_3, k=k, bs=bs), data=TPWmob0[-block,])
  #g<- gam(Residus~s(TMin.max, k=k, bs=bs), data=TPWmob0[-block,])
  forecast<-predict(g, newdata=TPWmob0[block,])
  return(TPWmob0[block,]$Residus2-forecast)
}

K<-c(3,5,10,15,20)#c(3:10)#
rmseKcr<-lapply(K, function(k){lapply(block_list, univ, k=k, bs='cr')%>%unlist%>%e_rmse} )
rmseKtp<-lapply(K, function(k){lapply(block_list, univ, k=k, bs='tp')%>%unlist%>%e_rmse} )
#rmseKps<-lapply(K, function(k){lapply(block_list, univ, k=k, bs='ps')%>%unlist%>%e_rmse} )
rmseKcs<-lapply(K, function(k){lapply(block_list, univ, k=k, bs='cs')%>%unlist%>%e_rmse} )

plot(K, rmseKcr, type='b', pch=20, ylim= range(rmseKcr), col=col[1])
lines(K, rmseKtp, type='b', pch=20, col=col[2])
#lines(K, rmseKps, type='b', pch=20, col=col[3])
lines(K, rmseKcs, type='b', pch=20, col=col[4])
legend("top", col=col, c("cr", "tp", "cs"), pch=20, ncol=2, bty='n')

mob3_gam <- gam(Residus2~s(Mobilite_Pr_MRg_1, k=16, bs="cs"), data=TPWmob0)
summary(mob3_gam)
plot(mob3_gam)


mobs_gam <- gam(Residus2~s(Mobilite_Pr_MRg_1, k=16, bs="cs"), data=TPWmob0, na.action = na.omit)
summary(mobs_gam)
plot(mobs_gam, scale=0)
#=======================================================================================#


#=======================================================================================#
# %%%%%%%%%%%%%%%%
# Test full_data 1


full1[,"GAM1"] <- NA
full1[,"GAM1"] <- predict(weekpops_gam, newdata=full1)

full1[,"GAM2"] <- NA
full1[,"GAM2"] <- full1[,"GAM1"] + predict(weathers_gam, newdata=full1)
sel = which(is.na(full1$GAM2))
full1[sel,"GAM2"] <- full1[sel,"GAM1"]

full1[,"GAM3"] <- NA
full1[,"GAM3"] <- full1[,"GAM2"] + predict(mobs_gam, newdata=full1)
sel = which(is.na(full1$GAM3))
full1[sel,"GAM3"] <- full1[sel,"GAM2"]

RMSE1 = e_rmse(full1[sel,"GAM3"]-full1[sel,"inc100"])
RELAVGERR1 = round(100*mean(abs(full1[sel,"inc100"]-full1[sel,"GAM3"]))/mean(abs(full1[sel,"inc100"])),digits=2)
#=======================================================================================#

