options(warn=-1)

# -----------------------
#  package installation
#-----------------------

# library list
packages <- list("plyr", "dplyr", "stringr", "ggplot2", "wordcloud", "reshape2",
                 "png", "tcltk", "ggmap", "RColorBrewer")

# Install packages if not already installed and load them
for (pck in packages) {
  if (!require(pck, character.only = TRUE)) {
    install.packages(pck, dependencies = TRUE)
    suppressWarnings(library(pck, character.only = TRUE))
  }
}

# twitteR


# -----------------
# get_integer_input
# -----------------

get_integer_input <- function(prompt_message, lang =NULL) {
  
  #' Function that validates whether the entered option is numeric.
  #' If an incorrect input is entered, a message prompts the user to input a correct value.
  #' @return integer
  #' This function returns the correctly entered number.
  #' @examples
  #' # Example usage:
  #' get_integer_input(option =1)  # Prompts the user to enter a number, returning correct option
  #' 
  
  while (TRUE) {
    user_input <- readline(prompt = prompt_message)
    
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


# ------------------
# msg_data_checker
#-------------------
msg_data_checker <- function(lang){
  
  #' TODO
  #' 
  #' @param option 
  #'   1 directs to Spanish sentiment files creation
  #'   2 directs to English sentiment files creation
  #' @return NULL 
  #' This function does not return a value but redirect to corresponding language actions.
  #' @examples
  #' # Example usage:
  #' handle_main_actions(option =1)  # redirect to Spanish sentiment analysis actions
  #'
  
  ctext <- NULL
  file_path = paste0(Sys.getenv("R_ROOT"),paste("/MSG",lang,sep="_"))
  if(!dir.exists(file.path(file_path))){
    if(lang == "EN"){
      text = "English"
      ctext <- sprintf("Folder: `MSG_EN` missing, there are no %s messages to analyze.\nPlease perform a message search. You can use the script: `2.1- Message_searching.R` for this purpose.", text)
    }else if(lang == "ES"){
      text = "Español"
      ctext <- sprintf("Falta la carpeta: `MSG_ES`, no existen mensajes en idioma %s para analizar.\nPor favor realiza la búsqueda de mensajes. Puede valerse del script: `2.1- Message_searching.R` para ello.\n", text)
    } 
  } 
  if (!is.null(ctext)){
    cat(ctext)
  }
  
  return(ctext)
}


# -------------------------
# category_data_checker
#--------------------------
category_data_checker <- function(lang){
  
  #' TODO
  #' 
  #' 
  #' @param option 
  #'   1 directs to Spanish sentiment files creation
  #'   2 directs to English sentiment files creation
  #' @return NULL 
  #' This function does not return a value but redirect to corresponding language actions.
  #' @examples
  #' # Example usage:
  #' handle_main_actions(option =1)  # redirect to Spanish sentiment analysis actions
  #'
  
  ctext <- NULL
  file_path = paste(Sys.getenv("R_ROOT"),lang,sep="/")
  if(!dir.exists(file.path(file_path))){
    if(lang == "EN"){
      text = "English"
      ctext <-sprintf("No %s sentiment categories have been created.\nPlease run the script: `1- Emotions_files_creation.R`to create.",text)
    }else if(lang == "ES"){
      text = "Español"
      ctext <-sprintf("No se han creado categorías de sentimientos en %s.\nPor favor, ejecute el script: `1- Emotions_files_creation.R para crearlas.\n",text)
    }
  } 
  if (!is.null(ctext)){
    cat(ctext)
  }
  
  return(ctext)
}

# --------------------
# get_exception_msgs
#---------------------
get_exception_msgs<-function(lang, file_full_path ,e){
  
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
    
    aux[[1]]<-sprintf("Error leyendo fichero: %s",file_full_path)
    aux[[2]]<-sprintf("Mensaje de error: %s.Abriendo con encoding UTF-8-BOM ...",e$message)
    aux[[3]]<-sprintf("Error abriendo fichero: %s en el segundo intento",file_full_path)
    aux[[4]]<-sprintf("Mensaje de error: %s \n",e$message)
  }
  else {
    aux[[1]]<-sprintf("Error reading file: %s",file_full_path)
    aux[[2]]<-sprintf("Error message: %s.Reading with UTF-8-BOM encoding...",e$message)
    aux[[3]]<-sprintf("Second attempt failed for file: %s",file_full_path)
    aux[[4]]<-sprintf("Error message: %s \n",e$message)
  }
  
  return(aux)
}

# Function to print colored text
cat_colored <- function(text, color) {
  
  #' TODO
  #' 
  #' 
  
  colors <- list(
    red = "\033[31m",
    green = "\033[32m",
    reset = "\033[0m"
  )
  
  cat(colors[[color]], text, colors$reset, "\n", sep = "")
}

# -----------------
# join_all_files
#------------------
join_all_files <- function(file_path,lang){
  
  #' TODO
  #' 
  #' 
  #' @param option 
  #'   1 directs to Spanish sentiment files creation
  #'   2 directs to English sentiment files creation
  #' @return NULL 
  #' This function does not return a value but redirect to corresponding language actions.
  #' @examples
  #' # Example usage:
  #' handle_main_actions(option =1)  # redirect to Spanish sentiment analysis actions
  #'
  
  all<-NULL
  if(dir.exists(file_path)){
    
    if(lang=="ES"){
      cat("Leyendo y juntando todos los mensajes a analizar...\n")
    }else if(lang =="EN"){
      cat("Reading and gathering all the messages to be analyzed...\n")
    }
    
    # open all searched files
    myFiles <- list.files(path=file_path,pattern="*_stack.csv")

    # join all in a single df
    for(i in 1:length(myFiles)){
      
      # create full path
      file_full_path <- file.path(file_path, myFiles[i])
      
      # try to read df
      df <- tryCatch({
        suppressWarnings(read.csv(file=file_full_path))
      }, error = function(e) {
        # recover messages to prompt in corresponding language
        msg <- get_exception_msgs(lang, file_full_path, e )
        cat_colored(msg[[1]],"red")
        cat_colored(msg[[2]],"red")
        tryCatch({
          suppressWarnings(read.csv(file=file_full_path, fileEncoding="UTF-8-BOM"))
        }, error = function(e) {
          cat_colored(msg[[3]],"red")
          cat_colored(msg[[4]],"red")
          return(NULL)
        })
      })
      if (!is.null(df)) {
        all <- rbind(all, df)
      }
    }
  }
  return (all)
}

lat_long_check <- function(all,lang){
  
  #' TODO
  #' 
  #' 
  #' @param option 
  #'   1 directs to Spanish sentiment files creation
  #'   2 directs to English sentiment files creation
  #' @return NULL 
  #' This function does not return a value but redirect to corresponding language actions.
  #' @examples
  #' # Example usage:
  #' handle_main_actions(option =1)  # redirect to Spanish sentiment analysis actions
  #'
  #'
  # determine msg num. with lat, log data
  sum_lat_lon<-sum(is.na(all[,2]))
  text <- NULL
  if(lang == "EN" & (nrow(all)-sum_lat_lon) < nrow(all)){
    cat("Proceeding to analyze geolocation data...\n")
    text <-sprintf("Only %d from %d messages have geolocalization data!!\nLoading default geolocalization data as demo...\n",(nrow(all)-sum_lat_lon),nrow(all))
  }else if(lang == "EN" & (nrow(all)-sum_lat_lon) == nrow(all)){
    text <-sprintf("Geolocation data added.\n")
  }else if(lang == "ES" & (nrow(all)-sum_lat_lon) < nrow(all)){
    cat("Procediendo a analizar datos de geolocalización...\n")
    text <-sprintf("Solo %d mensajes de %d presentan datos de geolocalización!!\nCargando datos de geolocalización de por defecto como demo...\n",(nrow(all)-sum_lat_lon),nrow(all))
  }else if(lang == "ES" & (nrow(all)-sum_lat_lon) == nrow(all)){
    text <-sprintf("Datos de geolocalización añadidos.\n")
  }
  
  if (!is.null(text)){
    cat(text)
  }
  return (text)

}

# ------------------
#  update_matches 
#------------------
update_matches <- function(lang, matches, words, df, df_geo, aux, i, categoria) {
  
  #' TODO
  #' 
  
  # Check if there are any non-NA matches
  if (sum(!is.na(matches)) > 0) {
    
    # Extract the term(s) corresponding to the non-NA matches
    term <- words[!is.na(matches)]
    
    # Extract the term(s) corresponding to the non-NA matches
    terms <- words[!is.na(matches)]
    
    for (term in terms) {
      if (term %in% df[, 1]) {
        # Term exists in the data frame, update its count
        pos <- grep(term, df[, 1])
        df[pos, 2] <- df[pos, 2] + sum(!is.na(matches))
        
        # Update the geographic data frame if coordinates match
        if ((df_geo[, "lon"] == aux[i, "longitude"]) && (df_geo[, "lat"] == aux[i, "latitude"])) {
          pos_geo <- grep(term, df_geo[, 1])
          if (length(pos_geo) > 0) {
            df_geo[pos_geo, 2] <- df_geo[pos_geo, 2] + sum(!is.na(matches))
          }
        }
      } else {
        # Term does not exist in the data frame, add it
        df <- rbind(df, data.frame(word = term, cuantos = sum(!is.na(matches))))
        
        # Add to geographic data frame
        df_geo <- rbind(df_geo, data.frame(word = term, cuantos = sum(!is.na(matches)), 
                                           lon = aux[i, "longitude"], lat = aux[i, "latitude"], 
                                           name = aux[i, "name"], categoria = categoria))
      }
    }
  }
  
  # Return the updated data frames as a list
  return(list(df = df, df_geo = df_geo))
}

# ------------------
#  organize_data 
# ------------------

organize_data <- function(df_geo, df_p, df_n) {
  
  #' TODO
  #' 
  
  # Initialize the list and the vector for missing data indicators
  l <- list()
  noDatapos <- vector()
  
  # Check if df_geo is provided and not empty
  if (length(df_geo) != 0) {
    l[[1]] <- df_geo
  } else {
    noDatapos <- c(noDatapos, 1)  # Indicate that df_geo is missing
  }
  
  # Check if df_p is provided and not empty
  if (length(df_p) != 0) {
    l[[2]] <- df_p
  } else {
    noDatapos <- c(noDatapos, 2)  # Indicate that df_p is missing
  }
  
  # Check if df_n is provided and not empty
  if (length(df_n) != 0) {
    l[[3]] <- df_n
  } else {
    noDatapos <- c(noDatapos, 3)  # Indicate that df_n is missing
  }
  
  # Add missing values indicator to the list if there are any missing
  if (length(noDatapos) != 0) {
    l[[4]] <- noDatapos
  }
  
  return(l)
}

# ------------------
#  score_sentiment 
#------------------

#evaluation tweets function
score_sentiment <- function(sentences, pos_words, neg_words, lang){
  
  #' TODO
  #' 
  #' Determine corresponding language characters from selected option
  #' Check if needed data is presented. Ask for its creation if not.
  #' Realize sentiment analisis a display results in corresponding language.
  #' 
  #' @param option 
  #'   1 directs to Spanish sentiment analysis actions
  #'   2 directs to English sentiment analysis actions
  #'   0 close program
  #' @return NULL 
  #' This function does not return a value but redirect to corresponding actions.
  #' @examples
  #' # Example usage:
  #' handle_language_options(option =1)  # redirect to Spanish sentiment analysis
  #'
  
  df_p<-NULL
  df_n<-NULL
  df_geo <-NULL
  aux<-sentences
  sentences<-sentences$text
  for(i in 1:length(sentences)){
    
    #' Clean text:
    #'  - Remove punctuation
    #'  - Remove control characters
    #'  - Remove digits
    #'  - Replace multiple spaces with a single space
    #'  - Convert encoding to UTF-8
    #'  - Convert to lowercase
    
    if (lang =="ES"){
      print(paste("Quedan ", length(sentences)-i, " textos por analizar.", sep=""))
    }else if(lang =="EN"){
      print(paste(length(sentences)-i, " texts remain to be analyzed.", sep=""))
    }
    
    sentence <- sentences[i]
    sentence <- gsub('[[:punct:]]', "", sentence)
    sentence <- gsub('[[:cntrl:]]', "", sentence)
    sentence <- gsub('[[:digit:]]', "", sentence)
    sentence <- gsub('[[:space:]]+', " ", sentence)
    sentence <- iconv(sentence, from = "latin1", to = "UTF-8", sub = "")
    sentence <- tolower(sentence)
    
    # split sentence in words
    word_list <- str_split(sentence, ' ')
    words <- unlist(word_list)
    
    # check sentiments in words
    pos_matches<-match(words, tolower(unlist(pos_words)))
    neg_matches<-match(words, tolower(unlist(neg_words)))
    
    # check for possitive matches
    result_pos  =  update_matches(lang, pos_matches, words, df_p, df_geo, aux, i, "p") 
    df_p = result_pos$df
    df_geo = result_pos$df_geo
    
    # check for negative matches
    result_neg  =  update_matches(lang, neg_matches, words, df_n, df_geo, aux, i, "n") 
    df_n = result_neg$df
    df_geo = result_neg$df_geo
    
  }
  # organize data
  return (organize_data(df_geo, df_p, df_n))
}

# ----------------------
# ACTION HANDLERS
# handle_main_actions
#-----------------------

handle_main_actions<-function(option){
  
  #' Function that handles languages based actions
  #' 
  #' Determine corresponding language characters from selected option
  
  #' 
  #' @param option 
  #'   1 directs to Spanish sentiment files creation
  #'   2 directs to English sentiment files creation
  #' @return NULL 
  #' This function does not return a value but redirect to corresponding language actions.
  #' @examples
  #' # Example usage:
  #' handle_main_actions(option =1)  # redirect to Spanish sentiment analysis actions
  #'
  
  #' Determine corresponding language characters:
  #' Example: 1 - "ES"
  #' Example: 2 - "EN"
  lang<-determine_language(option)
  
  # Check for messages
  msgs = msg_data_checker(lang)
  
  # if no error 
  if (is.null(msgs)){
    
    # check for sentiment categories
    categories = category_data_checker(lang)
    
    # if no error 
    if (is.null(categories)){ 
      
      # Join all data in a single df
      file_path = paste0(Sys.getenv("R_ROOT"),paste("/MSG",lang,sep="_"))
      all <- join_all_files(file_path,lang)
      # if it is correctly joined
      if (!is.null(all)){

        # check for latitude and longitude data and added default values if corresponds
        lat_long_check(all, lang)
        
        # get default country file 
        cty<-read.csv(paste(Sys.getenv("R_ROOT"),"countries.csv", sep="/"),sep=";",stringsAsFactors = F,dec=".")
         
        # Add default longitude and latitude data to data  
        all[,"longitude"]<-sample(cty[,"longitude"],nrow(all),replace=T)
        all[,"latitude"]<-sample(cty[,"latitude"],nrow(all),replace=T)
        all[,"name"]<-sample(cty[,"name"],nrow(all),replace=T)
        lat_long_check(all, lang)
        
        # redirect to sentiment files
        terms_path = paste(Sys.getenv("R_ROOT"),lang,sep="/")
        pos_term_file = paste0(paste0("Pos_",lang),".csv")
        neg_term_file = paste0(paste0("Neg_",lang),".csv")
        
        # read corresponding files
        pos_terms = read.csv(paste(terms_path,pos_term_file,sep ="/"))
        neg_terms = read.csv(paste(terms_path,neg_term_file,sep ="/"))
        
        # organize, sentiments score by possitive and negative sentimens
        scores <- score_sentiment(all, pos_terms, neg_terms,lang)
       
        # handle scores results
        handle_scores(scores, lang)
      }
      
    }
  }
  return(FALSE)  # exit the loop
}

# ----------------------
# handle_language_options
#-----------------------
handle_language_options<-function(option){
  
  #' Function that handles languages based actions
  #' 
  #' Determine corresponding language characters from selected option
  #' Check if needed data is presented. Ask for its creation if not.
  #' Realize sentiment analisis a display results in corresponding language.
  #' 
  #' @param option 
  #'   1 directs to Spanish sentiment analysis actions
  #'   2 directs to English sentiment analysis actions
  #'   0 close program
  #' @return NULL 
  #' This function does not return a value but redirect to corresponding actions.
  #' @examples
  #' # Example usage:
  #' handle_language_options(option =1)  # redirect to Spanish sentiment analysis
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
  
  
  return(FALSE)  # exit the loop
}

# ----------------------
# handle_scores
#-----------------------

handle_scores <- function(scores, lang) {
  
  #' TODO
  #'
  
  # Define language-specific strings
  lang_strings <- list(
    ES = list(positive = "POSITIVOS", negative = "NEGATIVOS", 
              no_geoloc = "NO EXISTEN COINCIDENCIAS DE GEOLOCALIZACION\n",
              no_positive = "NO EXISTEN COINCIDENCIAS DE CATEGORIAS POSITIVAS\n",
              no_negative = "NO EXISTEN COINCIDENCIAS DE CATEGORIAS NEGATIVAS\n"),
    EN = list(positive = "POSITIVES", negative = "NEGATIVES", 
              no_geoloc = "NO GEOLOCALIZATION COINCIDENCIES\n",
              no_positive = "NO POSITIVE CATEGORIES COINCIDENCIES\n",
              no_negative = "NO NEGATIVE CATEGORIES COINCIDENCIES\n")
  )
  
  # Select the appropriate strings based on the language
  strings <- lang_strings[[lang]]
  
  # Initialize flags and message text
  geol <- posi <- nega <- 1
  mtext <- ""
  
  # Handle the case where some data is missing
  if (length(scores) == 4) {
    for (i in seq_along(scores[[4]])) {
      switch(scores[[4]][i],
             `1` = {
               mtext <- paste(mtext, strings$no_geoloc)
               geol <- 0
             },
             `2` = {
               mtext <- paste(mtext, strings$no_positive)
               posi <- 0
             },
             `3` = {
               mtext <- paste(mtext, strings$no_negative)
               nega <- 0
             }
      )
    }
    
    # Print the missing data message if any
    if (nchar(mtext) > 0) cat(mtext)
    
    # Plot results based on flags
    if (posi == 1) plotTermPositiveResults(scores[[2]], strings$positive, lang, "darkgreen")
    if (nega == 1) plotTermNegativeResults(scores[[3]], strings$negative, lang, "darkred")
    if (geol == 1) geoResults(scores[[1]], lang)
    
  } else {
    # Handle the case where all data is present
    plotTermPositiveResults(scores[[2]], strings$positive, lang, "darkgreen")
    plotTermNegativeResults(scores[[3]], strings$negative, lang, "darkred")
    geoResults(scores[[1]], lang)
  }
}




# # DATA 
# 
# consumer_key <- "oGbGZQk0l587t8HDtPol7xFjH"
# 
# consumer_secret<- "RMP26xOyAECi1TMmXMCW6UA4Cf8ZaTc37uV10uu439KuHrTZWM"
# 
# setup_twitter_oauth(consumer_key, consumer_secret, access_token=NULL, access_secret=NULL)
# 
# # ------------------
# #  search
# #------------------
# 
# #the function of tweets accessing and analyzing
# search <- function(searchterm,lang)
# {
#    respuesta<--1
#   
#     mainDir <-Sys.getenv("R_ROOT")
#     subcarpeta <-paste("TWEETS",lang,sep="_")
#     
#     if(!dir.exists(file.path(mainDir, subcarpeta)))
#       dir.create(file.path(mainDir, subcarpeta),recursive = TRUE,showWarnings = F)
#     
#     setwd(file.path(mainDir, subcarpeta))
# 
#     #access tweets and create cumulative file
#     list <- searchTwitter(searchterm, lang=lang, n=1500)
#     
#     if(length(list)!=0)
#     {
#       #convertir en data.frame
#       df <- twListToDF(list)
#       #quedarse con las colunmas correspondientes: Text, longitud,latitud
#       df <- df[,c(1,(ncol(df)-1),ncol(df))]
#       
#       #si faltan datos tambien queremos guardarlos
#       df[which(is.na(df[,"longitude"])),"longitude"]<-"NA"
#       df[which(is.na(df[,"latitude"])),"latitude"]<-"NA"
#       
#       if (file.exists(paste(searchterm, '_stack.csv'))==FALSE) write.csv(df, file=paste(searchterm, '_stack.csv'), row.names=F)
#       
#       #merge last access with cumulative file and remove duplicates
#       stack <- read.csv(file=paste(searchterm, '_stack.csv'))
#       stack <- rbind(stack, df)
#       stack <- subset(stack, !duplicated(stack$text))
#       write.csv(stack, file=paste(searchterm, '_stack.csv'), row.names=F)
#       respuesta = 0
#     }
#     
# 
#     setwd(Sys.getenv("R_ROOT"))
#     return(respuesta)
# 
# }


# ------------------
#  plotTermPositiveResults 
#------------------
plotTermPositiveResults <-function(terms,str,lang,color)
{
  dirGraph <- paste(Sys.getenv("R_ROOT"),"/Graphs/",sep="")
  if(!dir.exists(dirGraph))
    dir.create(dirGraph,recursive=T)
  setwd(dirGraph)
  
  if(lang =="EN")
  {
    t<-paste(str, " TERMS RESULTS")
    pdftitle <- paste(t,".pdf")
  }
  
  if(lang =="ES")
  {
    t<-paste("RESULTADOS DE TERMINOS ",str)
    pdftitle <- paste(t,".pdf")
  }
  
  while (TRUE)
  {
    if(file.exists(pdftitle))
      pdftitle <- paste(" - ", pdftitle)
    else
      break
  }
  
  #create png
  wordcloud(terms$word,terms$cuantos, random.order=FALSE,color=color)
  dev.copy(png,paste(str,".png",sep=""),width=3.25,height=3.25,units="in",res=1500)
  dev.off()
  dev.off()
  
  mypng = readPNG(paste(str,".png",sep=""))
  
  #PLOTTING
  par(mar=c(6,3,3,2.5), mfrow =c(1,1))
  p.example = qplot(terms[,1],terms[,2],main =t,xlab="",ylab="", colour = I(color),size=I(5.5),ylim=c(0,max(terms[,2])))+
    theme(text = element_text(size=20),axis.text.x = element_text(angle = 45, hjust = 1))+
    theme(axis.line.x = element_line(color="black"),
          axis.line.y = element_line(color="black"),
          panel.background = element_blank()) +
    annotation_raster(mypng, ymin = 90, ymax= 250, xmin = 16, xmax = 22)
  
  pdf(pdftitle ,width=14,height=12/2, onefile=TRUE, paper="a4r")
  print(p.example)
  dev.off()
  dev.off()
  
  file.show(pdftitle)
  
}

# ------------------------------
#  plotTermNegativeResults 
#------------------------------
plotTermNegativeResults <-function(terms,str,lang,color)
{
  dirGraph <- paste(Sys.getenv("R_ROOT"),"/Graphs/",sep="")
  if(!dir.exists(dirGraph))
    dir.create(dirGraph,recursive=T)
  setwd(dirGraph)
  
  if(lang =="EN")
  {
    t<-paste(str, " TERMS RESULTS")
    pdftitle <- paste(t,".pdf")
  }
  
  if(lang =="ES")
  {
    t<-paste("RESULTADOS DE TERMINOS ",str)
    pdftitle <- paste(t,".pdf")
  }
  
  while (TRUE)
  {
    if(file.exists(pdftitle))
      pdftitle <- paste(" - ", pdftitle)
    else
      break
  }
  
  #create png
  wordcloud(terms$word,terms$cuantos, random.order=FALSE,color=color)
  dev.copy(png,paste(str,".png",sep=""),width=3.25,height=3.25,units="in",res=1500)
  dev.off()
  dev.off()
  
  mypng = readPNG(paste(str,".png",sep=""))
  
  #PLOTTING
  par(mar=c(6,3,3,2.5), mfrow =c(1,1))
  p.example = qplot(terms[,1],terms[,2],main =t,xlab="",ylab="", colour = I(color),size=I(5.5),ylim=c(0,max(terms[,2])))+
    theme(text = element_text(size=20),axis.text.x = element_text(angle = 45, hjust = 1))+
    theme(axis.line.x = element_line(color="black"),
          axis.line.y = element_line(color="black"),
          panel.background = element_blank())+ 
    annotation_raster(mypng, ymin = max(terms[,2])-10, ymax= max(terms[,2]), xmin = length(terms[,1])-4, xmax = length(terms[,1]))
  pdf(pdftitle ,width=14,height=12/2, onefile=TRUE, paper="a4r")
  print(p.example)
  dev.off()
  dev.off()
  
  file.show(pdftitle)
  
}





# ------------------
#  geoResults
#------------------


geoResults<-function(data,lang)
{
  
  dirGraph <- paste(Sys.getenv("R_ROOT"),"/Graphs/",sep="")
  if(!dir.exists(dirGraph))
    dir.create(dirGraph,recursive=T)
  setwd(dirGraph)
  
  if(lang =="EN") 
  { 
    text ="SENTIMENT ANALYSIS GEOLOCALIZED"
    pdftitle <- paste(text,".pdf",sep="")
  } 
  if(lang =="ES")
  { 
    text ="ANALISIS DE SENTIMIENTO GEOLOCALIZADO"
    pdftitle <- paste(text,".pdf",sep="")
  } 
   
  while (TRUE)
  {
    if(file.exists(pdftitle))
      pdftitle <- paste(" - ", pdftitle)
    else
      break
  }
  
  ll.visited <- geocode(as.character(unlist(data["name"])))
  data["lon"]<-ll.visited$lon
  data["lat"]<-ll.visited$lat
  
  xValue<-data["lon"]
  yValue<-data["lat"]
  
  
  #Using GGPLOT, plot the Base World Map
  mp <- NULL
  mapWorld <- borders("world", colour="gray50", fill="gray50") # create a layer of borders
  mp <- ggplot() +   mapWorld
  
  #Now Layer the cities on top
  
  
  myColors <- brewer.pal(2,"Set1")
  
  names(myColors) <- levels(data[,"categoria"])
  colScale <- scale_colour_manual(name = data[,"categoria"],values = myColors)
  
  
  mp <- mp+ geom_point(aes(x=xValue, y=yValue,colour=data[,"categoria"]), size=3)+
        theme(legend.title=element_blank())+
        ggtitle(text) + theme(plot.title = element_text(size = 25, face = "bold"))
      
        
  pdf(pdftitle ,width=14,height=12/2, onefile=TRUE, paper="a4r")
  print(mp)
  dev.off()
  
  file.show(pdftitle)
  
}




