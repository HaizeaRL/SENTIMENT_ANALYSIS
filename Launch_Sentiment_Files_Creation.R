#INITIALIZATIONS
if(Sys.getenv("R_ROOT") =="") Sys.setenv(R_ROOT=getwd())

#Create sentiment files

dir<-paste(Sys.getenv("R_ROOT"),"/1-Sentiment_Files/",sep="")
setwd(dir)
source("Sentiment_Files_Creation.R")



