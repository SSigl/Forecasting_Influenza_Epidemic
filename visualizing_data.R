#=======================================================================================#


#                                 VISUALIZING OUR DATA                                  #


#=======================================================================================#

source("/Users/SuzanneSigalla/Documents/ENSAE/3A/Semestre 1/S1 StatsML/Projet EDF/R script/epidemies_series_temp_base.R")

#=======================================================================================
# %%%%%%%%%
# ON FRANCE

ggplot(data_fr, aes(x=date, y=inc100, color = inc100))+
  geom_line() + scale_color_gradient(low="green", high="blue")+ theme_bw() + ggtitle("Taux d'incidence pour 100 000 - France") 

ggplot(data_fr, aes(x=date, y=inclog, color = inclog))+
  geom_line() + scale_color_gradient(low="green", high="blue")+ theme_bw()+ ggtitle("Log du taux d'incidence pour 100 000 - France") 


#=======================================================================================
# %%%%%%%%%%
# ON REGIONS

i = code_reg[1]
data_reg = get(paste("data",i,sep="_"))
Inclog=xts(data_reg$inclog,order.by=data_reg$date)
Temp = xts(data_reg$Temp, order.by=data_reg$date)

time.series=cbind(Inclog/sd(Inclog),Temp/sd(Temp,na.rm=T))
names(time.series)=c("Inclog","Temp")
dygraph(time.series, 
        main=paste("Evolution conjointe du log du taux d'incidence et de la température de 2016 a 2018 en region",i, sep=" "))%>% dyRangeSelector()


