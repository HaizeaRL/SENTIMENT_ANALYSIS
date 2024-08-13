# Set root path as default environment
if(Sys.getenv("R_ROOT")=="") Sys.setenv(R_ROOT=getwd())

#' Call: Sentiment_analysis_results.R 
#' The questions help determine the language in which you want to perform the sentiment analysis.
#' Based on the selected language, the script checks whether the messages to be analyzed and sentiment categories exist in that language.
#' Next, it performs and plots sentiment analysis in the specified language.
#' If errors occur during the process, guidelines are provided.
source(paste0(paste0(Sys.getenv("R_ROOT"),"/2-Message_analysis/"),"Sentiment_analysis_results.R"))