#INITIALIZATIONS
if(Sys.getenv("R_ROOT") =="") Sys.setenv(R_ROOT=getwd())

#Create sentiment files

dir<-paste(Sys.getenv("R_ROOT"),"/1-Emotions_categorization_files/",sep="")
setwd(dir)
source("Sentiment_Files_Creation.R")
setwd(Sys.getenv("R_ROOT"))


# TODO: komentatu kodigoa