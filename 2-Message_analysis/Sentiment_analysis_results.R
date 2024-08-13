# Load necessary functions, call: Sentiment_analysis_functions.R
source(paste0(paste0(Sys.getenv("R_ROOT"),"/2-Message_analysis/"),"Sentiment_analysis_functions.R"))


repeat {
  cat("Indicate in which language you want to perform the sentiment analysis:\n")
  cat("1: ES\n")
  cat("2: EN\n")
  cat("0: Exit\n")

  # Read user input
  user_input <- get_integer_input("Enter your choice: ")

  # Questionary is provided and actions are handled by the selected option
  # During the process, if errors occur, guidelines are provided.
  if (!handle_language_options(user_input)) {

    # remove all used variables
    varsToPurge <- as.list(ls())
    for(i in 1:length(varsToPurge)) rm(list = varsToPurge[[i]])
    rm(i,varsToPurge)

    break  # Exit the loop
  }
}


