if(Sys.getenv("R_ROOT") =="") Sys.setenv(R_ROOT=getwd())
dir<-paste(Sys.getenv("R_ROOT"),"/2-Analysis_of_Tweets/",sep="")
setwd(dir)
source("GetTwitterData.R")
setwd(Sys.getenv("R_ROOT"))

