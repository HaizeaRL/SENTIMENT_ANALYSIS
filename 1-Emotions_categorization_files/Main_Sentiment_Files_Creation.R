# Load necessary functions, call: Sentiment_Files_Functions.R
source(paste0(paste0(Sys.getenv("R_ROOT"),"/1-Emotions_categorization_files/"),"Sentiment_Files_Functions.R"))


# Display language selection form
repeat {
  cat("Indicate in which language you want to perform the sentiment analysis:\n")
  cat("1: ES\n")
  cat("2: EN\n")
  cat("0: Exit\n")
  
  # Read user input
  user_input <- as.integer(readline(prompt = "Enter your choice: "))
  
  # Handle the user's choice and determine whether to continue or exit
  if (!handle_main_menu_option(user_input)) {
    
    # remove all used variables
    varsToPurge <- as.list(ls())
    for(i in 1:length(varsToPurge)) rm(list = varsToPurge[[i]])
    rm(i,varsToPurge)
    
    break  # Exit the loop 
  }
}


# if(!is.null(language) && language %in% c(1,2)){
#   
#   cat("Bien seleccionado:\n")
#   break
#   
#   # msg<-list()
#   # 
#   # #Initializations
#   # createDefaultSentimentFiles()
#   # lang<-knowLanguage(language)
#   # Sys.setenv(IDIOMA=lang)
#   # msg<-getAllPromptMessages(lang)
#   # 
#   # #visualize only corresponding files
#   # visualizeFiles(lang)
#   # cat(msg[[1]])
#   # add<-menu(msg[[2]])
#   # 
#   # #------------------
#   # # cleanEnvironment 
#   # except <- c("add","msg", "lang","dir")
#   # varsToPurge <- as.list(ls())
#   # rmv <- varsToPurge[!(varsToPurge %in% except)]
#   # for(i in 1:length(rmv)) rm(list = rmv[[i]])
#   # rm(varsToPurge, rmv, i)
#   # 
#   # #carga funciones
#   # setwd(dir)
#   # source("Sentiment_Files_Functions.R")
#   # #------------------
#   # 
#   # if(add ==1){ 
#   #   #YES
#   #   cat(msg[[3]])
#   #   which.file<-menu(msg[[4]])
#   #   addDataToCorrespondingFile(lang,which.file,msg)
#   #   
#   # }
#   # else{
#   #   varsToPurge <- as.list(ls())
#   #   for(i in 1:length(varsToPurge)) rm(list = varsToPurge[[i]])
#   #   rm(varsToPurge,  i)
#   # }
# }else{
#   cat("Exiting:\n")
#   break
# }

