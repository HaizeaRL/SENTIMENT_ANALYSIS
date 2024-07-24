# TODO: komentatu kodigoa

dir<-paste(Sys.getenv("R_ROOT"),"/1-Emotions_categorization_files/",sep="")

setwd(dir)
source("Sentiment_Files_Functions.R")

language <-NULL

cat("Indica en que idioma quieres realizar el analisis de sentimiento\n")
language <- menu(c("ES","EN"))

if(!is.null(language))
{
  msg<-list()
  
  #Initializations
  createDefaultSentimentFiles()
  lang<-knowLanguage(language)
  Sys.setenv(IDIOMA=lang)
  msg<-getAllPromptMessages(lang)
  
  #visualize only corresponding files
  visualizeFiles(lang)
  cat(msg[[1]])
  add<-menu(msg[[2]])
  
  #------------------
  # cleanEnvironment 
  except <- c("add","msg", "lang","dir")
  varsToPurge <- as.list(ls())
  rmv <- varsToPurge[!(varsToPurge %in% except)]
  for(i in 1:length(rmv)) rm(list = rmv[[i]])
  rm(varsToPurge, rmv, i)
  
  #carga funciones
  setwd(dir)
  source("Sentiment_Files_Functions.R")
  #------------------
  
  if(add ==1) #YES
  {
    cat(msg[[3]])
    which.file<-menu(msg[[4]])
    addDataToCorrespondingFile(lang,which.file,msg)
    
  }
  else
  {
    varsToPurge <- as.list(ls())
    for(i in 1:length(varsToPurge)) rm(list = varsToPurge[[i]])
    rm(varsToPurge,  i)
  }
}
    

