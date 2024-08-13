# ------------------------
# default_sentiment_dict
#------------------------
default_sentiment_dict <- list(
  "Pos_ES" = c("Atractivo","Ideal","Precio especial","Robusto","Elegante","Cumple las necesidades","Portable",
               "Diferente","A la moda","Fantastico","Facil", "Reusable","Rapido","Agil","Unico","Manejable","Actual",
               "Beneficioso","Increible","Brillante","Practico","Impactante", "Divertido","Bonito","Pulcro","Nuevo",
               "Comodo","Maravilloso","Inteligente","Seguro","Satisfactorio","Contento","Alegre","Lujoso","Novedoso","Moderno"),
  "Neg_ES" = c("Feo","Sucio","Muy simple","Complejo","Dificil","Inseguro","Peligroso","Debil","Fragil",
               "Aburrido","Obsoleto", "Desordenado","Pobre","Horrible","Descontento","Insatisfecho",
               "Enfadado","Decepcionado","Frustrante","Sin sentido"),
  "Pos_EN" = c("Attractive","Ideal","Special Price","Robust","Elegant","Meet the needs","Highly portable",
               "Trendy","Fashionble","Fantastic","Portable", "Reusable","Quick","Agile","Unique","Luxury",
               "User-Friendly","Beneficial","Amazing","Billiant","Practical","Atonishing", "Awesome",
               "Beautiful","Sleek","Incredible","Gorgeous","Intelligent","Smart","Easy","Safe","Funny","The latest"),
  "Neg_EN" = c("Ugly","Dirty","Too Simple","Complex","Difficult","Unsafe","Dangerous","Risky","Weak",
               "Same","Messy", "Poor","Awful","WTF","Unpleased","Dissatisfied","Unhappy","Annoyed",
               "Angry","Boring")
)

# -----------------
# get_integer_input
# -----------------

get_integer_input <- function(option, lang = NULL) {
  
  #' Function that prompts the user to enter a numeric value and validates the input.
  #' If the input is not numeric, it provides feedback in the selected language or default language, prompting the user to enter a valid number.
  #' @param option A prompt message to guide the user.
  #' @param lang An optional parameter to specify the language for error messages.
  #' - "ES" for Spanish messages.
  #' - "EN" for English messages.
  #' @return integer
  #' This function returns the correctly entered integer.
  #' @examples
  #' # Example usage:
  #' get_integer_input(option = "Enter your choice: ")  # Prompts the user to enter a number, returning the valid input
  #' 
  
  while (TRUE) {
    user_input <- readline(prompt = option)
    
    # Check if the input is numeric and a whole number
    if (grepl("^-?\\d+$", user_input)) {
      return(as.integer(user_input))
    }else if (!is.null(lang)){
      if(lang=="ES"){
        cat("Valor introducido es incorrecto. Por favor introduzca un valor valido.\n")
      }else if(lang =="EN"){
        cat("Invalid input. Please enter a valid number.\n")
      }
    }else{
      cat("Invalid input. Please enter a valid number.\n")
    }
  }
}

# -----------------
# determine_language
# -----------------

determine_language <-function(option){
  
  #' Function that determines the language code based on the user’s selection.
  #' Returns "ES" for Spanish or "EN" for English depending on the selected option.
  #' 
  #' @param option 
  #'   - 1 for Spanish
  #'   - 2 for English
  #' @return character 
  #' This function returns "ES" or "EN" based on the selected option.
  #' @examples
  #' # Example usage:
  #' determine_language(option = 1)  # Returns "ES"
  #' determine_language(option = 2)  # Returns "EN"
  
  return(ifelse(option==1,"ES","EN"))
}


# --------------------
# get_all_prompt_msgs
#---------------------
get_all_prompt_msgs<-function(lang){
  
  #' Function that retrieves all prompt messages in the specified language.
  #' @param lang 
  #'   - "ES" for Spanish
  #'   - "EN" for English
  #' @return list 
  #' This function returns a list of prompt messages in the specified language.
  #' The list includes messages for various user prompts and actions.
  #' @examples
  #' # Example usage:
  #' get_all_prompt_msgs(lang = "ES")  # Retrieves a list of Spanish prompt messages
  #' get_all_prompt_msgs(lang = "EN")  # Retrieves a list of English prompt messages
  
  aux <-list()
  if(lang=="ES"){
    
    aux[[1]]<-"Seleccionaste: Español (ES)\n"
    aux[[2]]<-"Revisa las categorias de emociones positivas y negativas mostradas por defecto.\n"
    aux[[3]]<-"\nDeseas anadir nuevos atributos a la lista de por defecto:\n"
    aux[[4]]<-c("Si","No")
    aux[[5]]<-"Indique en qué tipo de emoción desea añadir mas categorias:\n"
    aux[[6]] <-c("Positivos","Negativos","Ambos")
    aux[[7]] <-"Empieza a introducir datos: (0 para terminar)"
    aux[[8]] <-"VALORES POSITIVOS:\n"
    aux[[9]] <-"VALORES NEGATIVOS:\n"
    aux[[10]] <-"Nuevo fichero(s) guardados\n"
    aux[[11]] <-"Valor incorrecto. Introduzca un valor correcto por favor."
    aux[[12]] <-"Cerrando el programa\n"
    aux[[13]] <-"Introduzca un valor:"
    aux[[14]] <-"INTRODUCIENDO "
  }
  else {
    aux[[1]]<-"You selected English (EN) \n"
    aux[[2]]<-"Check presented default positive and negative emotions categories.\n"
    aux[[3]]<-"\nWould you like to add new atributes to defaults ones:\n"
    aux[[4]]<-c("Yes","No")
    aux[[5]]<-"Specify in which emotion type you want to add new attributes:\n"
    aux[[6]] <-c("Positives","Negatives","Both")
    aux[[7]] <-"Add atributes: (0 to stop)"
    aux[[8]] <-"POSITIVE VALUES:\n"
    aux[[9]] <-"NEGATIVE VALUES:\n"
    aux[[10]] <-"New file(s) save :\n"
    aux[[11]] <-"Invalid option. Please try again."
    aux[[12]] <-"Closing the program.\n"
    aux[[13]] <-"Enter your choice:"
    aux[[14]] <-"ENTERING "
  }
  
  return(aux)
}



# ----------------------------------------
# create_and_display_default_sentiment_files
#-----------------------------------------
create_and_display_default_sentiment_files<-function(lang, msg){
  
  #' This function creates the default positive and negative sentiment files for the specified language 
  #' (Spanish or English) and displays their contents.
  #' 
  #' @param lang 
  #'   - "ES" for Spanish
  #'   - "EN" for English
  #' @param msg 
  #'   A list of prompt messages used to display information to the user.
  #' @return NULL 
  #' This function generates default sentiment files and prints their content to the console.
  #' @examples
  #' # Example usage:
  #' create_and_display_default_sentiment_files(lang = "ES", msg = get_all_prompt_msgs("ES"))  
  #' # Creates and displays Spanish default positive and negative sentiment files
  #' 
  #' create_and_display_default_sentiment_files(lang = "EN", msg = get_all_prompt_msgs("EN"))  
  #' # Creates and displays English default positive and negative sentiment files
  
  # create corresponding folder
  if(!dir.exists(file.path(Sys.getenv("R_ROOT"), lang)))
    dir.create(file.path(Sys.getenv("R_ROOT"), lang),recursive = TRUE,showWarnings = F)
  
  # create corresponding positive file
  titP<-paste(paste0(Sys.getenv("R_ROOT"),"/",lang),paste0("Pos_",lang,".csv"),sep="/")
  if(!file.exists(titP)) write.table(default_sentiment_dict[[paste0("Pos_",lang)]], titP, row.names=F,col.names=F) 
  
  # create corresponding negative file
  titN<-paste(paste0(Sys.getenv("R_ROOT"),"/",lang),paste0("Neg_",lang,".csv"),sep="/")
  if(!file.exists(titN)) write.table(default_sentiment_dict[[paste0("Neg_",lang)]], titN, row.names=F,col.names=F) 
  
  # visualize corresponding language files
  cat(msg[[8]])
  print(default_sentiment_dict[[paste0("Pos_",lang)]])  
  
  cat(msg[[9]])
  print(default_sentiment_dict[[paste0("Neg_",lang)]])  
}

# -------------
# loop_ask_data
#--------------

loop_ask_data<-function(msg){
  
  #' This function repeatedly prompts the user to enter data until the user enters '0' to stop.
  #' The entered values are collected into a list, with non-empty inputs being stored.
  #' 
  #' @param msg 
  #'   A list of messages used for prompting the user.
  #' @return vector 
  #'   A vector containing all entered values, excluding 'NA' values.
  #' @examples
  #' # Example usage:
  #' loop_ask_data(msg = get_all_prompt_msgs("ES"))  
  #' # Prompts the user to enter data and collects inputs until '0' is entered.
  #' 
  str <- -1
  lista <-vector()
  while(str != 0)
  {
    cat(msg[[7]]) 
    str <-readline()
    if(str!=0 && str!="") lista<-c(lista,str)
    # print(lista)
  }
  #deleting possible NA values
  lista<-lista[!is.na(lista)]
  return(as.vector(lista))
}


# -------------------
# MENU DISPLAYERS
# display_which_file_menu
#--------------------

display_which_file_menu <- function(lang, msg){
  
  #' Function to display menu options for modifying sentiment files.
  #' Prompts the user to select which files to modify: positive, negative, or both.
  #' 
  #' @param lang 
  #'   "ES" for Spanish prompt messages,
  #'   "EN" for English prompt messages.
  #' @param msg 
  #'   A list of messages for prompting the user in the selected language.
  #' @return NULL 
  #'   This function does not return a value but directs to the appropriate actions based on user choice.
  #' @examples
  #' # Example usage:
  #' display_which_file_menu(lang = "ES", msg = get_all_prompt_msgs("ES"))  
  #' # Displays file modification options in Spanish.
  
  repeat {
    cat(msg[[5]])
    cat(paste0("1:",paste0(msg[[6]][1],"\n")))
    cat(paste0("2:",paste0(msg[[6]][2],"\n")))
    cat(paste0("3:",paste0(msg[[6]][3],"\n")))
    
    # Read user input
    user_input <- get_integer_input(cat(msg[[13]]))
    
    # Handle the user's choice and determine whether to continue or exit
    if (!handle_file_option(user_input, lang, msg)) {
      break  # Exit the loop
    }
  }
}

# ----------------------
# display_adding_menu
#-----------------------
display_adding_menu <- function(lang, msg){
  
  #' Function to display options for adding new emotion categories.
  #' Prompts the user to choose whether to add new positive, negative, or both types of categories.
  #' The prompt messages are provided in the selected language.
  #' 
  #' @param lang 
  #'   "ES" for Spanish prompt messages,
  #'   "EN" for English prompt messages.
  #' @param msg 
  #'   A list of messages for prompting the user in the selected language.
  #' @return NULL 
  #'   This function does not return a value but directs to the appropriate actions based on user input.
  #' @examples
  #' # Example usage:
  #' display_adding_menu(lang = "ES", msg = get_all_prompt_msgs("ES"))  
  #' # Displays the menu for adding new emotion categories with Spanish messages and handles the user's response.
  
  repeat {
    cat(msg[[3]])
    cat(paste0("1:",paste0(msg[[4]][1],"\n")))
    cat(paste0("2:",paste0(msg[[4]][2],"\n")))
    
    # Read user input
    user_input <- get_integer_input(cat(msg[[13]]),lang)
    
    # Handle the user's choice and determine whether to continue or exit
    if (!handle_change_option(user_input, lang, msg)) {
      break  # Exit the loop 
    }
  }
}

# ----------------------
# ACTION HANDLERS
# handle_addings
#-----------------------

handle_addings <- function(msg,lang, text){
  
  #' Function to manage the addition of new emotion categories to sentiment files.
  #' Prompts the user to enter new values, merges them with existing values, and updates the relevant file.
  #' The prompt messages are provided in the selected language.
  #' 
  #' @param msg 
  #'   A list of prompt messages corresponding to the selected language.
  #' @param lang 
  #'   "ES" for Spanish prompt messages,
  #'   "EN" for English prompt messages.
  #' @param text 
  #'   "Pos_" for positive sentiment files,
  #'   "Neg_" for negative sentiment files.
  #' @return NULL 
  #'   This function does not return a value but performs actions to update the sentiment files.
  #' @examples
  #' # Example usage:
  #' handle_addings(msg = get_all_prompt_msgs("ES"), lang = "ES", text = "Pos_")  
  #' # Prompts for new positive values in Spanish, updates the positive sentiment file with new entries.
  
  # ask for data
  new_vals = loop_ask_data(msg)
  
  # recover corresponding original values
  orig_vals = default_sentiment_dict[[paste0(text,lang)]]
  
  # add new values to origin
  last_vals = unique(c(orig_vals,new_vals))
  
  # find corresponding file and update
  file_title<-paste(paste0(Sys.getenv("R_ROOT"),"/",lang),paste0(text,lang,".csv"),sep="/")
  if(file.exists(file_title)){
    
    # remove existed file
    file.remove(file_title)
    
    # save new values
    write.table(last_vals, file_title, row.names=F,col.names=F)
  }
}

# ----------------------
# handle_adding_actions
#-----------------------
handle_adding_actions <- function(option, lang, msg){
  
  #' Function to handle actions for adding new emotion categories.
  #' Prompts the user with the corresponding message and performs the updates on the specified files.
  #' 
  #' @param option 
  #'   1 for updates to the positive sentiment file only,
  #'   2 for updates to the negative sentiment file only,
  #'   3 for updates to both positive and negative sentiment files.
  #' @param lang 
  #'   "ES" for Spanish prompt messages,
  #'   "EN" for English prompt messages.
  #' @param msg 
  #'   A list of prompt messages in the selected language.
  #' @return NULL 
  #'   This function does not return a value but updates the sentiment files as specified.
  #' @examples
  #' # Example usage:
  #' handle_adding_actions(option = 1, lang = "ES", msg = get_all_prompt_msgs("ES"))  
  #' # Prompts for new values for the positive sentiment file in Spanish and updates it.
  
  if(option == 1){
    # Asking for positives values
    cat(paste0(msg[[14]],msg[[8]]))
    handle_addings(msg,lang, "Pos_")
    
  }else if(option == 2){
    # Asking for negatives values
    cat(paste0(msg[[14]],msg[[9]]))
    handle_addings(msg,lang, "Neg_")
    
  }else if(option == 3){
    
    # Asking for positives values
    cat(paste0(msg[[14]],msg[[8]]))
    handle_addings(msg,lang, "Pos_")
    
    # Asking for negatives values
    cat(paste0(msg[[14]],msg[[9]]))
    handle_addings(msg,lang, "Neg_")
    
  }
}

# ----------------------
# handle_file_option
#-----------------------
handle_file_option <- function(option, lang, msg) {
  
  #' Function to handle updates to sentiment files based on user selection.
  #' Prompts the user with messages in the specified language and performs updates 
  #' to the sentiment files according to the selected option.
  #' 
  #' @param option 
  #'   1 for updating the positive sentiment file only,
  #'   2 for updating the negative sentiment file only,
  #'   3 for updating both positive and negative sentiment files.
  #' @param lang 
  #'   "ES" for Spanish prompt messages,
  #'   "EN" for English prompt messages.
  #' @param msg 
  #'   A list of prompt messages in the selected language.
  #' @return NULL 
  #'   This function does not return a value but performs the specified file update actions.
  #' @examples
  #' # Example usage:
  #' handle_file_option(option = 1, lang = "ES", msg = get_all_prompt_msgs("ES"))
  #' # Redirects to update positive sentiment file in Spanish and exits the loop.
  
  if (option == 1) {
    handle_adding_actions(option,lang, msg)   # "POSITIVE"
    return(FALSE)  # exit the loop
    
  } else if (option == 2) {
    handle_adding_actions(option,lang, msg)   # "NEGATIVE"
    return(FALSE)  # exit the loop
    
  } else if (option == 3) {
    handle_adding_actions(option,lang, msg)   # "BOTH"
    return(FALSE)  # exit the loop
    
  } else {
    # re-ask for correct option
    cat(msg[[11]])
    return(TRUE)  # continue the loop
  }
}

# ----------------------
# handle_change_option
#-----------------------
handle_change_option <- function(option, lang, msg) {
  
  #' Function to handle user responses for adding new emotions.
  #' Redirects to appropriate actions based on user selection, or provides an error message 
  #' and prompts the user again for a valid option.
  #' 
  #' @param option 
  #'   1 to redirect to the menu for adding new emotions,
  #'   2 to close the program,
  #'   otherwise display an error message and prompt the user again.
  #' @param lang 
  #'   "ES" for Spanish prompt messages,
  #'   "EN" for English prompt messages.
  #' @param msg 
  #'   A list of prompt messages in the selected language.
  #' @return NULL 
  #'   This function does not return a value but performs the appropriate actions based 
  #'   on user input.
  #' @examples
  #' # Example usage:
  #' handle_change_option(option = 1, lang = "ES", msg = get_all_prompt_msgs("ES"))
  #' # Redirects to the menu for adding new categories in Spanish.
  
  if (option == 1) {
    
    # manage which file to update
    display_which_file_menu(lang, msg)
    return(FALSE)  # exit the loop
    
  } else if (option == 2) {
    
    # closing the program
    cat(msg[[12]])
    return(FALSE)  # exit the loop
    
  } else {
    
    # re-ask for correct option
    cat(msg[[11]])
    return(TRUE)  # continue the loop
  }
}


# ----------------------
# handle_main_actions
#-----------------------
handle_main_actions<-function(option){
  
  #' Function to handle language-specific actions.
  #' 
  #' Determines the language based on the selected option, retrieves all prompt messages 
  #' in that language, creates and displays default sentiment files, and allows the user 
  #' to make changes to those files. Handles the specified changes accordingly.
  #' 
  #' @param option 
  #'   1 for Spanish sentiment file creation,
  #'   2 for English sentiment file creation.
  #' @return NULL 
  #'   This function does not return a value but performs actions related to the chosen language.
  #' @examples
  #' # Example usage:
  #' handle_main_actions(option = 1)  # Directs to Spanish sentiment file creation and management.
  
  #' Determine corresponding language characters:
  #' Example: 1 - "ES"
  #' Example: 2 - "EN"
  lang<-determine_language(option)
  
  # Get all prompt messages in corresponding language
  msg<-get_all_prompt_msgs(lang)
  
  # Display selected language text
  cat(msg[[1]])
  
  # Create default sentiment files & visualize
  cat(msg[[2]])
  create_and_display_default_sentiment_files(lang, msg)
  
  # Ask if changes are needed
  display_adding_menu(lang, msg)
  
  return(FALSE)  # exit the loop
}

# ----------------------
# handle_main_menu_option
#-----------------------

# Function to handle the menu options
handle_main_menu_option <- function(option) {
  
  #' Function to handle the main menu's selection.
  #' 
  #' Directs to different actions based on the user's menu choice. Handles options for 
  #' creating sentiment files in Spanish or English, or exiting the program. Displays an 
  #' error message and re-prompts the user for valid input if the option is invalid.
  #' 
  #' @param option 
  #'   0 to exit the program,
  #'   1 to direct to Spanish sentiment files creation,
  #'   2 to direct to English sentiment files creation.
  #' @return NULL 
  #'   This function does not return a value but redirects to the corresponding language actions 
  #'   or exits the program based on user input.
  #' @examples
  #' # Example usage:
  #' handle_main_menu_option(option = 1)  # Redirects to Spanish sentiment file creation.
  
  if (option == 1) {
    # actions for "ES"
    handle_main_actions(option)
    
  } else if (option == 2) {
    # actions for "EN"
    handle_main_actions(option)
    
  } else if (option == 0) {
    cat("Closing the program.\n")
    return(FALSE)  # exit the loop
  } else {
    cat("Invalid option. Please try again.\n")
    return(TRUE)  # continue the loop
  }
}
