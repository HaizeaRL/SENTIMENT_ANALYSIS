# Set root path as default environment
if(Sys.getenv("R_ROOT") =="") Sys.setenv(R_ROOT=getwd())

#' Call: Sentiment_files_creation.R 
#' Questions are provided to help determine in which language you want to perform the sentiment analysis.
#' Default positive and negative categories are displayed in the selected language.
#' Complementary questions are provided to add additional emotion categories if necessary.
source(paste0(paste0(Sys.getenv("R_ROOT"),"/1-Emotions_categorization_files/"),"Sentiment_files_creation.R"))


