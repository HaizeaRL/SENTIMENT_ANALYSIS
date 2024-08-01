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
# determine_language
# -----------------

determine_language <-function(option){
  
  #' Function that determine corresponding language characters. 
  #' "ES" or "EN" depending of selected option.
  #' @param option 
  #'   1 for Spanish,
  #'   2 for English .
  #' @return NULL 
  #' This function returns "ES" or "EN" according to selected language.
  #' @examples
  #' # Example usage:
  #' determine_language(option =1)  # returns "ES"
  
  return(ifelse(option==1,"ES","EN"))
}


# --------------------
# get_all_prompt_msgs
#---------------------
get_all_prompt_msgs<-function(lang){
  
  #' Function that recover all prompt messages in corresponding language
  #' @param lang 
  #'   "ES" for Spanish,
  #'   "EN" for English .
  #' @return list 
  #' This function return a list with all prompt messages. These messages correspond to Spanish or English language.
  #' @examples
  #' # Example usage:
  #' get_all_prompt_msgs(lang ="ES")  # obtain a list of spanish messages to prompt
  #'
  
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
  
  #' This function creates the positive and negative sentiment files and displays their content
  #' corresponding to the chosen language (Spanish or English). 
  #'
  #' @param lang 
  #'   "ES" for Spanish,
  #'   "EN" for English .
  #' @return NULL 
  #' This function calls to corresponding English_Sentiment_Files.R or Spanish_Sentiment_Files.R
  #' scripts to create default sentiment files.
  #' @examples
  #' # Example usage:
  #' create_and_display_default_sentiment_files(lang ="ES")  # create Spanish default pos & neg files
  #'
  
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
# loopAskData
#--------------

loopAskData<-function(msg)
{
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
  
  #' Function display options to modify files
  #' Only positive or negative file or both.
  #' 
  #' @param lang 
  #'   "ES" for Spanish prompt messages
  #'   "EN" for English prompt messages
  #' @param msg 
  #'    Corresponding language prompt message list
  #' @return NULL 
  #' This function does not return a value but handle actions.
  #' @examples
  #' # Example usage:
  #' display_which_file_menu(lang = "ES" , msg = list()) # display file options in Spanish
  #'
  
  repeat {
    cat(msg[[5]])
    cat(paste0("1:",paste0(msg[[6]][1],"\n")))
    cat(paste0("2:",paste0(msg[[6]][2],"\n")))
    cat(paste0("3:",paste0(msg[[6]][3],"\n")))
    
    # Read user input
    user_input <- as.integer(readline(prompt = cat(msg[[13]])))
    
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
  
  #' Function that display new emotions adding option and handles the response
  #' Prompt message are in corresponding language
  #' 
  #' @param lang 
  #'   "ES" for Spanish prompt messages
  #'   "EN" for English prompt messages
  #' @param msg 
  #'    Corresponding language prompt message list
  #' @return NULL 
  #' This function does not return a value but handle actions.
  #' @examples
  #' # Example usage:
  #' display_adding_menu(lang = "ES" , msg = list())  # prompt Spanish messages and handle actions
  #'
  
  repeat {
    cat(msg[[3]])
    cat(paste0("1:",paste0(msg[[4]][1],"\n")))
    cat(paste0("2:",paste0(msg[[4]][2],"\n")))
    
    # Read user input
    user_input <- as.integer(readline(prompt = cat(msg[[13]])))
    
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
  
  #' Function that handles adding actions
  #' Ask for values
  #' Recover original values and add new ones removing duplicates
  #' Update corresponding file. Eliminate previous file and replace with new values
  #' 
  #' @param msg 
  #'    Corresponding language prompt message list
  #' @param lang 
  #'   "ES" for Spanish prompt messages
  #'   "EN" for English prompt messages
  #' @param text 
  #'   "Pos_" for positive files
  #'   "Neg_" for negative files
  #' @return NULL 
  #' This function does not return a value but handle file updating actions.
  #' @examples
  #' # Example usage:
  #' handle_addings(lang = "ES" , msg = list(), text = "Pos_")  # Ask for new values in Spanish and update positive file
  #'
  
  # ask for data
  new_vals = loopAskData(msg)
  
  # recover corresponding original values
  orig_vals = default_sentiment_dict[[paste0(text,lang)]]
  
  # add new values to origin
  last_vals = unique(c(orig_vals,new_vals))
  
  # find corresponding file and update
  file_title<-paste(paste0(Sys.getenv("R_ROOT"),"/",lang),paste0(text,lang,".csv"),sep="/")
  print(file_title)
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
  
  #' Function that handle adding actions
  #' Prompt corresponding message and redirect to corresponding actions
  #' 
  #' @param option 
  #'   1 changes only in positive file
  #'   2 changes only in negative file
  #'   3 changes in both files
  #' @param lang 
  #'   "ES" for Spanish prompt messages
  #'   "EN" for English prompt messages
  #' @param msg 
  #'   Corresponding language prompt message list
  #' @return NULL 
  #'   This function does not return a value but handle file updating actions.
  #' @examples
  #' # Example usage:
  #' handle_adding_actions(option = 1 , lang = "ES", msg = list()) # Ask for new values update positive file in Spanish.
  #'
  
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
  
  #' Function that handles file updating action
  #' 
  #' @param option 
  #'   1 changes only in positive file
  #'   2 changes only in negative file
  #'   3 changes in both files
  #' @param lang 
  #'   "ES" for Spanish prompt messages
  #'   "EN" for English prompt messages
  #' @param msg 
  #'    Corresponding language prompt message list
  #' @return NULL 
  #' This function does not return a value but handle actions.
  #' @examples
  #' # Example usage:
  #' handle_file_option(option = 1, lang = "ES" , msg = list()) # redirect to add new values in positive file in Spanish
  #'
  
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
  
  #' Function that handles new emotions adding menu responses
  #' 
  #' @param option 
  #'   1 redirect to adding actions
  #'   2 close the program
  #'   else display an error and re-ask for correct option
  #' @param lang 
  #'   "ES" for Spanish prompt messages
  #'   "EN" for English prompt messages
  #' @param msg 
  #'    Corresponding language prompt message list
  #' @return NULL 
  #' This function does not return a value but handle actions.
  #' @examples
  #' # Example usage:
  #' handle_change_option(option = 1, lang = "ES" , msg = list()) # redirect new category adding action in Spanish
  #'
  
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
  
  #' Function that handles languages based actions
  #' 
  #' Determine corresponding language characters from selected option
  #' Get corresponding language all prompt messages
  #' Create and visualize default positive and negative emotion files
  #' Ask for changes to those positive and negative emotion files
  #' Handle specified changes
  #' 
  #' @param option 
  #'   1 directs to Spanish sentiment files creation
  #'   2 directs to English sentiment files creation
  #' @return NULL 
  #' This function does not return a value but redirect to corresponding language actions.
  #' @examples
  #' # Example usage:
  #' handle_main_actions(option =1)  # redirect to Spanish sentiment file creation
  #'
  
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
  
  #' Function that handles first menu's selection
  #' @param option 
  #'   0 skip from program
  #'   1 directs to Spanish sentiment files creation
  #'   2 directs to English sentiment files creation
  #'   else display error message and re-ask for correct option
  #' @return NULL 
  #' This function does not return a value but redirect to corresponding language actions.
  #' @examples
  #' # Example usage:
  #' handle_main_menu_option(option =1)  # redirect to Spanish sentiment file creation
  #'
  
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
