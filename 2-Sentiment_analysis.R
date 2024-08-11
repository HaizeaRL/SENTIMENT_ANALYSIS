# Set root path as default environment
if(Sys.getenv("R_ROOT")=="") Sys.setenv(R_ROOT=getwd())

#' Call: Sentiment_analisis_results.R 
#' Questions are provided to help determine in which language you want to perform the sentiment analysis.
#' Based on the selected language, checks whether the messages to analyze and sentiment categories exist in that language.
#' Later, perform and plot sentiment analysis in the specified language.
#' During the process, if errors occur, guidelines are provided.
source(paste0(paste0(Sys.getenv("R_ROOT"),"/2-Message_analysis/"),"Sentiment_analysis_results.R"))