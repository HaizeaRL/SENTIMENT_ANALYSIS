# Set root path as default environment
if(Sys.getenv("R_ROOT")=="") Sys.setenv(R_ROOT=getwd())

#' Call: Sentiment_analisis_results.R 
source(paste0(paste0(Sys.getenv("R_ROOT"),"/2-Message_analysis/"),"Sentiment_analysis_results.R"))



