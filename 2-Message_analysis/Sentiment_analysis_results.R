# Load necessary functions, call: Sentiment_analysis_functions.R
source(paste0(paste0(Sys.getenv("R_ROOT"),"/2-Message_analysis/"),"Sentiment_analysis_functions.R"))



#' Questions are provided to help determine in which language you want to perform the sentiment analysis.
#' Based on the selected language, checks whether the messages to analyze and sentiment categories exist in that language.
#' Later, perform and plot sentiment analysis in the specified language.
#' During the process, if errors occur, guidelines are provided to guide.
repeat {
  cat("Indicate in which language you want to perform the sentiment analysis:\n")
  cat("1: ES\n")
  cat("2: EN\n")
  cat("0: Exit\n")

  # Read user input
  user_input <- get_integer_input("Enter your choice: ")

  # Handle the user's choice and determine whether to continue or exit
  if (!handle_language_options(user_input)) {

    # remove all used variables
    varsToPurge <- as.list(ls())
    for(i in 1:length(varsToPurge)) rm(list = varsToPurge[[i]])
    rm(i,varsToPurge)

    break  # Exit the loop
  }
}


