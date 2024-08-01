# Load necessary functions, call: Sentiment_analysis_functions.R
source(paste0(paste0(Sys.getenv("R_ROOT"),"/2-Message_analysis/"),"Sentiment_analysis_functions.R"))


# Display language selection form
repeat {
  cat("Indicate in which language you want to perform the sentiment analysis:\n")
  cat("1: ES\n")
  cat("2: EN\n")
  cat("0: Exit\n")

  # Read user input
  user_input <- as.integer(readline(prompt = "Enter your choice: "))

  # Handle the user's choice and determine whether to continue or exit
  if (!handle_language_options(user_input)) {

    # remove all used variables
    varsToPurge <- as.list(ls())
    for(i in 1:length(varsToPurge)) rm(list = varsToPurge[[i]])
    rm(i,varsToPurge)

    break  # Exit the loop
  }
}




# # -----------
# # FUNCTIONS
# #------------
# source("TwitterFunctions.R")
# 
# 
# if(Sys.getenv("R_ROOT") =="") Sys.setenv(R_ROOT=getwd())
# 
# setwd(Sys.getenv("R_ROOT"))
# # DATA 
# 
# consumer_key <- "oGbGZQk0l587t8HDtPol7xFjH"
# 
# consumer_secret<- "RMP26xOyAECi1TMmXMCW6UA4Cf8ZaTc37uV10uu439KuHrTZWM"
# 
# setup_twitter_oauth(consumer_key, consumer_secret, access_token=NULL, access_secret=NULL)
# 
# language <-NULL
# 
# cat("Indica en que idioma quieres buscar los datos\n")
# language <- menu(c("ES","EN"))
# 
# if(!is.null(language))
# {
#   if(language ==1) lang="ES"
#   if(language ==2) lang="EN"
#   
#   mainDir <-Sys.getenv("R_ROOT")
#   sub <-paste("TWEETS",lang,sep="_")
#   
#
#      if(!dir.exists(file.path(mainDir, sub)))
#   {
#     if(lang == "EN") ctext <-"NO DATA IN DATABASE.\n PLEASE LAUNCH SCRIPT: Launch_Searching.R\n\n IF THE ERROR PERSIST PLEASE CONSULT WITH THE TECHNICIAN."
#     if(lang == "ES") ctext <-"LA BASE DE DATOS NO ESTA CREADA.\n POR FAVOR EJECUTA EL SCRIPT: Launch_Searching.R\n\nSI TRAS EJECUTAR SIGUE HABIENDO ALGUN ERROR CONSULTASELO AL TECNICO."
#     button <- tkmessageBox(title='Message',message=ctext,type='ok')
#     button <- tclvalue(button)
#     quit("yes")
#   }
#   if(dir.exists(file.path(mainDir, sub)))
#   {
#     setwd(file.path(mainDir, sub))
#     
#     myFiles <- list.files(path=".",pattern="*_stack.csv")
#     
#     todo<-NULL
#     for(i in 1:length(myFiles))
#     {
#       todo<-rbind(todo,read.csv(file=myFiles[i]))
#     }
#     
#     #longitude / latitude ctrl
#     
#     n<-nrow(todo)
#     long_lat<-sum(is.na(todo[,2]))
#     
#     text <-paste("ONLY ",(n-long_lat),"FROM", n,"HAVE GEOLOCALIZATION DATA.\n LOADING DEFAULT GEOLOCALIZATION DATA AS DEMO")
#   
#     button <- tkmessageBox(title='Message',message=text,type='ok')
#     button <- tclvalue(button)
#     
#     
#     cty<-read.csv(paste(Sys.getenv("R_ROOT"),"countries.csv", sep="/"),sep=";",stringsAsFactors = F,dec=".")
#     
#   
#     todo[,"longitude"]<-sample(cty[,"longitude"],nrow(todo),replace=T)
#     todo[,"latitude"]<-sample(cty[,"latitude"],nrow(todo),replace=T)
#     todo[,"name"]<-sample(cty[,"name"],nrow(todo),replace=T)
#     
#     mainDir <-Sys.getenv("R_ROOT")
#     lang <-lang
#     
#     if(!dir.exists(file.path(mainDir, lang)))
#     {
#       if(lang == "EN") cattext <-"NO CATEGORIES CREATED.\n PLEASE LAUNCH SCRIPT: Launch_Sentiment_Files_Creation.R\n\n IF THE ERROR PERSIST PLEASE CONSULT WITH THE TECHNICIAN."
#       if(lang == "ES") cattext <-"NO EXISTEN CATEGORIAS.\n POR FAVOR EJECUTA EL SCRIPT: Launch_Sentiment_Files_Creation.R\n\nSI TRAS EJECUTAR SIGUE HABIENDO ALGUN ERROR CONSULTASELO AL TECNICO."
#       button <- tkmessageBox(title='Message',message=cattext,type='ok')
#       button <- tclvalue(button)
#       quit("yes")
#     }
#     
#     if(dir.exists(file.path(mainDir, lang)))
#     {
#       setwd(file.path(mainDir, lang))
#       
#       titP<-paste("Pos_",lang,".csv",sep="")
#       titN<-paste("Neg_",lang,".csv",sep="")
#       
#       pos <- read.csv(file=titP)
#       neg<- read.csv(file=titN)
#       
#       scores <- score.sentiment(todo, pos, neg)
#       
#       if(lang == "ES")
#       {
#         strp<-"POSITIVOS"
#         strn<-"NEGATIVOS"
#       
#       }
#       if(lang == "EN")
#       {
#         strp<-"POSITIVES"
#         strn<-"NEGATIVES"
#       }
#       
#       #NO DATA CTRL
#       if(length(scores)==4) #SOME DATA MISSING
#       {
#         mtext<-""
#         geol <-1
#         posi<-1
#         nega<-1
#         
#         for(i in 1:length(scores[[4]]))
#         {
#           if(scores[[4]][i] ==1)
#           {
#             if(lang == "EN") mtext <-paste(mtext,"NO GEOLOCALIZATION COINCIDENCIES\n")
#             if(lang == "ES") mtext <-paste(mtext,"NO EXISTEN COINDICENCIAS DE GEOLOCALIZACION\n")
#       
#             geol<-0
#           }
#           if(scores[[4]][i] ==2)
#           {
#             if(lang == "ES") mtext <-paste(mtext,"NO EXISTEN COINDICENCIAS DE CATEGORIAS POSITIVAS\n")
#             if(lang == "EN") mtext <-paste(mtext,"NO POSITIVE CATEGORIES COINCIDENCIES\n")
#             posi<-0
#           }
#           if(scores[[4]][i] ==3)
#           {
#             if(lang == "ES") mtext <-paste(mtext,"NO EXISTEN COINDICENCIAS DE CATEGORIAS NEGATIVAS\n")
#             if(lang == "EN") mtext <-paste(mtext,"NO NEGATIVE CATEGORIES COINCIDENCIES\n")
#             nega<-0
#           }
#           
#           button <- tkmessageBox(title='Message',message=mtext,type='ok')
#           button <- tclvalue(button)
#           
#           # SENTIMENT ANALYSIS RESULTS
#           if(posi == 1) plotTermPositiveResults(scores[[2]],strp,lang,"darkgreen")
#           if(nega == 1) plotTermNegativeResults(scores[[3]],strn,lang,"darkred")
#           
#           # GEOLOCALIZATION RESULTS
#           if(geol == 1)      geoResults(scores[[1]],lang)
#         }
#         
#       }
#       if(length(scores)!=4) #ALL DATA OK
#       {
#         # SENTIMENT ANALYSIS RESULTS
#         plotTermPositiveResults(scores[[2]],strp,lang,"darkgreen")
#         plotTermNegativeResults(scores[[3]],strn,lang,"darkred")
#         
#         # GEOLOCALIZATION RESULTS
#         geoResults(scores[[1]],lang)
#         
#       }
#     }
#   }
#   
# }


