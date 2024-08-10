# Load necessary functions, call: Sentiment_Files_Functions.R
source(paste0(paste0(Sys.getenv("R_ROOT"),"/1-Emotions_categorization_files/"),"Sentiment_files_functions.R"))


# Display language selection form
repeat {
  cat("Indicate in which language you want to perform the sentiment analysis:\n")
  cat("1: ES\n")
  cat("2: EN\n")
  cat("0: Exit\n")
  
  # Read user input
  user_input <- get_integer_input("Enter your choice: ")
  
  # Handle the user's choice and determine whether to continue or exit
  if (!handle_main_menu_option(user_input)) {
    
    # remove all used variables
    varsToPurge <- as.list(ls())
    for(i in 1:length(varsToPurge)) rm(list = varsToPurge[[i]])
    rm(i,varsToPurge)
    
    break  # Exit the loop 
  }
}


