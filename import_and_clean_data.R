#=======================================================================================#



#                           IMPORTATION AND CLEANING OF DATA                            #



#=======================================================================================#

rm(list = ls())
library(readr)
library(lubridate)
library(stringr)
library(dplyr)
library(tibble)
#setwd("")


#=======================================================================================
# %%%%%%%%%%%%%%%%
# USEFUL FUNCTIONS


GetDateLundi = function(x){
  return(as.Date(paste("1",str_sub(x,5,6),str_sub(x,1,4),sep=""),"%u%W%Y"))
}
GetYearWeek = function(x){
  return(paste(str_sub(x,1,4),str_sub(x,5,6),sep="-"))
}
GetSubsetWithValue = function(data, var, value){
  return(subset(data,data[,var] == value))
}
#=======================================================================================


#=======================================================================================
# %%%%
# DATA

# EPIDEMIC IN FRANCE
data_fr <- read.csv("/Users/SuzanneSigalla/Documents/ENSAE/3A/Semestre 1/S1 StatsML/Projet EDF/data/data grippe/bdd_syndromes_grippaux.csv",sep=";")

# EPIDEMIC IN EACH REGION OF FRANCE
data = read.csv("Epidemie/bdd_syndromes_grippaux_reg.csv",sep=";")

# MOBILITIES FOR FRANCE
df_pca <- read.csv("Mobilite/MobilitesPCA_2006-2015.txt", sep=";")
df_pca <- subset(df_pca, df_pca$REGION2 %in% c("MCm","MRg"))
#=======================================================================================

#=======================================================================================
# %%%%%%%%%%%%%%%%
# CLEANING THE DATA


# --- KEEPING USEFUL VARIABLES --- #
data <- subset(data, select = c("geo_insee","week","inc100"))
data_fr <- subset(data_fr, select = c("week","inc100"))

# --- DATE CREATION ---# 
data$date <- GetDateLundi(data$week)
data$week_year <- GetYearWeek(data$week)
data_fr$week_year <- GetYearWeek(data_fr$week)
data_fr$date <- GetDateLundi(data_fr$week)

# --- INC100 WITH LAG ---#
for(i in levels(as.data.frame(table(data[,"geo_insee"]))[,1])){
  data_int = as_data_frame(GetSubsetWithValue(data, "geo_insee", i))
  data_int[,"inc100lag"] <- NA
  data_int[1:nrow(data_int)-1,"inc100lag"] <- data_int[2:nrow(data_int),"inc100"]
  assign(paste("data",i,sep="_"), data_int)
  rm(data_int)
}
data_fr$inc100lag <- NA
data_fr[1:nrow(data_fr)-1,"inc100lag"] <- data_fr[2:nrow(data_fr),"inc100"]

# --- INSEE CODE OF EACH REGION --- #
code_reg <- levels(as.factor(data$geo_insee))[-1]
nvx_codes_reg <- read_tsv("correspondance_regions.tsv") # because of convention change, we need an equivalence table
#=======================================================================================



#=======================================================================================
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# MERGING EPIDEMICS AND WEATHER

path_weather = "Meteo/df_"
for(i in code_reg){
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  # FIRST STEP : IMPORT WEATHER DATA FOR EACH REGION
  df <- read.csv(paste(path_weather,i,".txt",sep=""), colClasses = c("Date", "numeric", "numeric"))
  df$week_year <- strftime(df[,"Jour"],"%Y-%W")
  df$week <- strftime(df[,"Jour"],"%W")
  df_int <- subset(df, select=c("Jour","week_year","week"))
  df_int <- df_int %>%
    group_by(week_year, week) %>%
    summarise(date=head(Jour,1))
  df <- as_data_frame(do.call(data.frame,aggregate(. ~ week_year, data = df[,c("TMin","TMax","week_year")], FUN = function(x) c(moy = mean(x), min = min(x), max = max(x) ), na.action = na.pass)))
  df$date <- df_int$date
  df$week <- df_int$week
  rm(df_int)
  rownames(df) <- NULL
  
  #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  # SECOND STEP : MERGING WEATHER DATA WITH EPIDEMICS DATA
  df <- merge(x = get(paste("data",i,sep="_")), y = df,by.x=c("week_year"), by.y=c("week_year"), all.x=TRUE, all.y = TRUE)
  df$week.x <- NULL
  df$date.x <- NULL
  df[,"geo_insee"] <- i
  colnames(df)[which(colnames(df)=="week.y")] <- "week"
  colnames(df)[which(colnames(df)=="date.y")] <- "date"
  df <- subset(df, df$week_year > "1984-43")
  rownames(df) <- NULL
  df <- as_data_frame(df)
  df <- df[,c("geo_insee","week_year","week","date","TMin.min","TMin.moy","TMin.max","TMax.min","TMax.moy","TMax.max","inc100lag","inc100")]
  assign(paste("merged", "df", i, sep="_"),df)
  rm(list=paste("data", i, sep="_"))
}
#=======================================================================================



#=======================================================================================
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# MERGING (EPIDEMICS, WEATHER) AND MOBILITY

for(i in code_reg){
  df <- get(paste("merged_df", i, sep="_"))
  df[,c("PopTot","Mobilite_Sc_MCm_1","Mobilite_Sc_MCm_2","Mobilite_Sc_MCm_3","Mobilite_Sc_MRg_1","Mobilite_Sc_MRg_2","Mobilite_Sc_MRg_3","Mobilite_Pr_MCm_1","Mobilite_Pr_MCm_2","Mobilite_Pr_MCm_3","Mobilite_Pr_MRg_1","Mobilite_Pr_MRg_2","Mobilite_Pr_MRg_3")] <- NA
  for(y in c(2006:2015)){
    sel=which(year(df$date)==y)
    if(nrow(subset(df_pca, df_pca$REGION1==i & df_pca$Annee == y))> 0){
      region = i
    }else{
      region = nvx_codes_reg[[which(nvx_codes_reg$former_code==i),"new_code"]]
    }
    if(nrow(subset(df_pca, df_pca$REGION1==region & df_pca$Annee == y))> 0){
      df[sel, "PopTot"] <- subset(df_pca, df_pca$REGION1==region & df_pca$Pr.Sc == "Sc" & df_pca$REGION2 == "MCm" & df_pca$Annee == y)[, "PopTot"]
      for(j in c(1:3)) {
        for (k in c("Sc", "Pr")) {
          for (l in c("MCm", "MRg")) {
            var_name = paste("Mobilite", k, l, j, sep = "_")
            df[sel, var_name] <- subset(df_pca, df_pca$REGION1==region & df_pca$Pr.Sc == k & df_pca$REGION2 == l & df_pca$Annee == y)[, paste("Mobilite", j, sep = "")]
          }
        }
      }
    }
  }
  df[which(year(df$date)<2006 |(year(df$date)==2006 & as.double(df$week)<26)),"PopTot"] <- df[[which(df$week_year=="2006-26"),"PopTot"]]
  df[which(year(df$date)>2015 |(year(df$date)==2015 & as.double(df$week)>26)),"PopTot"] <- df[[which(df$week_year=="2015-26"),"PopTot"]]
  for(idx in which(year(df$date)>=2006 & year(df$date)<=2015)){
    y = year(df$date[[idx]])
    w = as.double(df$week[[idx]])
    if(w!=26){
      rel_past = (sign(w-26)-1)/2
      past_idx = which(df$week_year==paste(y+rel_past,26,sep="-"))
      futr_idx = which(df$week_year==paste(y+rel_past+1,26,sep="-"))
      if(is.na(df$Mobilite_Pr_MCm_1[[past_idx]]) | is.na(df$Mobilite_Pr_MCm_1[[futr_idx]])){
        df[idx,c("Mobilite_Sc_MCm_1","Mobilite_Sc_MCm_2","Mobilite_Sc_MCm_3","Mobilite_Sc_MRg_1","Mobilite_Sc_MRg_2","Mobilite_Sc_MRg_3","Mobilite_Pr_MCm_1","Mobilite_Pr_MCm_2","Mobilite_Pr_MCm_3","Mobilite_Pr_MRg_1","Mobilite_Pr_MRg_2","Mobilite_Pr_MRg_3")] <- NA
      }else{
        df[[idx,"PopTot"]] <- df[[past_idx,"PopTot"]] + (((w-26)%%52)/52) * (df[[futr_idx,"PopTot"]] - df[[past_idx,"PopTot"]])
        for(j in c(1:3)){
          for(k in c("Sc","Pr")){
            for(l in c("MCm", "MRg")){
              var_name = paste("Mobilite",k,l,j, sep="_")
              df[[idx,var_name]] <- df[[past_idx,var_name]] + (((w-26)%%52)/52) * (df[[futr_idx,var_name]] - df[[past_idx,var_name]])
            }
          }
        }
      }
    }
  }
  df <- df[,c("geo_insee","week_year","week","date","TMin.min","TMin.moy","TMin.max","TMax.min","TMax.moy","TMax.max","PopTot","Mobilite_Sc_MCm_1","Mobilite_Sc_MRg_1","Mobilite_Pr_MCm_1","Mobilite_Pr_MRg_1","Mobilite_Sc_MCm_2","Mobilite_Sc_MRg_2","Mobilite_Pr_MCm_2","Mobilite_Pr_MRg_2","Mobilite_Sc_MCm_3","Mobilite_Sc_MRg_3","Mobilite_Pr_MCm_3","Mobilite_Pr_MRg_3","inc100lag","inc100")]
  assign(paste("merged_df", i, sep="_"), df)
}
rm(df,df_pca,nvx_codes_reg,futr_idx,past_idx,idx,rel_past,i,j,k,l,region,sel,var_name,w,y)
#=======================================================================================


#=======================================================================================
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# MERGING ALL DATABASES INTO ONE
merged_df <- as.data.frame(merged_df_11, stringsAsFactors = FALSE)
for(i in code_reg){
  if(i != "11"){
    merged_df <- rbind(merged_df, as.data.frame(eval(parse(text = paste("merged_df_",i,sep=""))), stringsAsFactors = FALSE))
  }
  rm(list=paste("merged_df_", i, sep=""))
}
rm(i)
#=======================================================================================

#=======================================================================================
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# FIXING AN ISSUE WITH DATE FORMAT

week_53 <- which(merged_df$week == "53" | is.na(merged_df$week))
week_53_00 <- c()
for(idx in week_53){
  week_53_00 = c(week_53_00, idx)
  if(merged_df$week[[idx-1]] == "52"){
    if(!is.na(merged_df$inc100[[idx]])){
      merged_df$inc100[[idx-1]] <- merged_df$inc100[[idx-1]]+merged_df$inc100[[idx]]
    }
    min_min = c(merged_df$TMin.min[[idx-1]])
    min_moy = c(merged_df$TMin.moy[[idx-1]])
    min_max = c(merged_df$TMin.max[[idx-1]])
    max_min = c(merged_df$TMax.min[[idx-1]])
    max_moy = c(merged_df$TMax.moy[[idx-1]])
    max_max = c(merged_df$TMax.max[[idx-1]])
    if(!is.na(merged_df$TMin.min[[idx]])){
      min_min = c(min_min,merged_df$TMin.min[[idx]])
      min_moy = c(min_moy,merged_df$TMin.moy[[idx]])
      min_max = c(min_max,merged_df$TMin.max[[idx]])
      max_min = c(max_min,merged_df$TMax.min[[idx]])
      max_moy = c(max_moy,merged_df$TMax.moy[[idx]])
      max_max = c(max_max,merged_df$TMax.max[[idx]])
    }
    if(idx < nrow(merged_df)){
      if(merged_df$week[[idx+1]] == "00"){
        if(!is.na(merged_df$inc100[[idx+1]])){
          merged_df$inc100[[idx-1]] <- merged_df$inc100[[idx-1]]+merged_df$inc100[[idx+1]]
        }
        week_53_00 = c(week_53_00, idx+1)
        week1 = 2
        if(!is.na(merged_df$TMin.min[[idx+1]])){
          min_min = c(min_min,merged_df$TMin.min[[idx+1]])
          min_moy = c(min_moy,merged_df$TMin.moy[[idx+1]])
          min_max = c(min_max,merged_df$TMin.max[[idx+1]])
          max_min = c(max_min,merged_df$TMax.min[[idx+1]])
          max_moy = c(max_moy,merged_df$TMax.moy[[idx+1]])
          max_max = c(max_max,merged_df$TMax.max[[idx+1]])
        }
      }else{
        week1 = 1
      }
      merged_df$inc100lag[[idx+week1]] <- merged_df$inc100[[idx-1]]
    }
    merged_df$TMin.min[[idx-1]] <- min(min_min)
    merged_df$TMin.moy[[idx-1]] <- mean(min_moy)
    merged_df$TMin.max[[idx-1]] <- max(min_max)
    merged_df$TMax.min[[idx-1]] <- min(max_min)
    merged_df$TMax.moy[[idx-1]] <- mean(max_moy)
    merged_df$TMax.max[[idx-1]] <- max(max_max)
  }else{
    print("ERROR")
  }
}
merged_df <- merged_df[-week_53_00,]
rm(min_min,min_moy,min_max,max_min,max_moy,max_max,week_53,week_53_00,week1)

week_00 <- which(merged_df$week=="00")
for(idx in week_00){
  if(merged_df$week[[idx-1]] == "52" & !is.na(merged_df$TMin.min[[idx]])){
    merged_df$TMin.min[[idx-1]] <- min(c(merged_df$TMin.min[[idx-1]],merged_df$TMin.min[[idx]]))
    merged_df$TMin.moy[[idx-1]] <- mean(c(merged_df$TMin.moy[[idx-1]],merged_df$TMin.moy[[idx]]))
    merged_df$TMin.max[[idx-1]] <- max(c(merged_df$TMin.max[[idx-1]],merged_df$TMin.max[[idx]]))
    merged_df$TMax.min[[idx-1]] <- min(c(merged_df$TMax.min[[idx-1]],merged_df$TMax.min[[idx]]))
    merged_df$TMax.moy[[idx-1]] <- mean(c(merged_df$TMax.moy[[idx-1]],merged_df$TMax.moy[[idx]]))
    merged_df$TMax.max[[idx-1]] <- max(c(merged_df$TMax.max[[idx-1]],merged_df$TMax.max[[idx]]))
  }
}
merged_df <- merged_df[-week_00,]
rm(idx,week_00)

inc100_NA = which(is.na(merged_df$inc100) | is.na(merged_df$inc100lag))
merged_df <- merged_df[-inc100_NA,]
rm(inc100_NA)

rows_with_nonMob_NAs_left = subset(merged_df, is.na(merged_df$geo_insee) | is.na(merged_df$week) | is.na(merged_df$week_year) | is.na(merged_df$date) | is.na(merged_df$TMin.min) | is.na(merged_df$TMin.moy) | is.na(merged_df$TMin.max) | is.na(merged_df$TMax.min) | is.na(merged_df$TMax.moy) | is.na(merged_df$TMax.max) | is.na(merged_df$PopTot))
#=======================================================================================


#=======================================================================================
#%%%%%%%%%%%
#SAVING FILE

write.csv(merged_df, file = "Merged_data.csv",row.names=FALSE)
#=======================================================================================

