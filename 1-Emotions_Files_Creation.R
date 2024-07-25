# Set root path as default environment
if(Sys.getenv("R_ROOT") =="") Sys.setenv(R_ROOT=getwd())

#' Call: Sentiment_Files_Creation.R 
#' Questions are provided to add additional emotion categories if necessary.
#' Default positive and negative categories are displayed in the selected language.
#' Option to add more categories is presented.
source(paste0(paste0(Sys.getenv("R_ROOT"),"/1-Emotions_categorization_files/"),"Main_Sentiment_Files_Creation.R"))


