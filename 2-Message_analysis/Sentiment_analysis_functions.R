options(warn=-1)

# -----------------------
#  package installation
#-----------------------

# library list
packages <- list("plyr", "dplyr", "stringr", "ggplot2", "wordcloud", "reshape2",
                 "png", "grid","leaflet","htmlwidgets")

# Install packages if not already installed and load them
for (pck in packages) {
  if (!require(pck, character.only = TRUE)) {
    install.packages(pck, dependencies = TRUE)
    suppressWarnings(library(pck, character.only = TRUE))
  }
}


# -----------------
# get_integer_input
# -----------------

get_integer_input <- function(prompt_message, lang =NULL) {
  
  #' Function that validates whether the entered option is numeric.
  #' If an incorrect input is entered, a message prompts the user to input a correct value.
  #' 
  #' @param prompt_message 
  #' The message entered by the prompt.
  #' @param lang 
  #' Corresponds to the selected language. By default, it is NULL.
  #' - "ES" to print instruction messages in Spanish.
  #' - "EN" to print instruction messages in English.
  #' @return integer
  #' This function returns the correctly entered number.
  #' @examples
  #' # Example usage:
  #' get_integer_input(prompt_message = 1)  # Prompts the user to enter a number, returning the correct option.
  
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
  
  #' Function that determines the corresponding language characters.
  #' Returns "ES" or "EN" depending on the selected option.
  #' 
  #' @param option 
  #'   1 for Spanish,
  #'   2 for English.
  #' @return character
  #' This function returns "ES" or "EN" according to the selected language.
  #' @examples
  #' # Example usage:
  #' determine_language(option = 1)  # returns "ES"
  
  return(ifelse(option==1,"ES","EN"))
}


# ------------------
# msg_data_checker
#-------------------
msg_data_checker <- function(lang){
  
  #' Function to verify the existence of language-specific folders for text analysis.
  #' - If the selected language is English, the function checks for the presence of the `MSG_EN` folder.
  #' - If the selected language is Spanish, the function checks for the `MSG_ES` folder.
  #' If the corresponding folder is missing, the function provides instructions on how to create it.
  #' 
  #' @param lang 
  #' Corresponds to the selected language.
  #' - "ES" to print instruction messages in Spanish.
  #' - "EN" to print instruction messages in English.
  #' @return NULL 
  #' This function does not return a value but prompts actions in case of an error.
  #' @examples
  #' # Example usage:
  #' msg_data_checker(lang = "ES")  # redirects to Spanish message checking.
  
  ctext <- NULL
  file_path = paste0(Sys.getenv("R_ROOT"),paste("/MSG",lang,sep="_"))
  if(!dir.exists(file.path(file_path))){
    if(lang == "EN"){
      text = "English" 
      ctext <- sprintf("Folder `MSG_EN` is missing, there are no %s messages to analyze.\nPlease unzip the corresponding folder from `Folders_to_add_for_DEMO.rar` file into the project.", text)
    }else if(lang == "ES"){
      text = "Español"
      ctext <- sprintf("Falta la carpeta: `MSG_ES`, no existen mensajes en idioma %s para analizar.\nPor favor, descomprima la carpeta correspondiente del archivo `Folders_to_add_for_DEMO.rar`en el proyecto.", text)
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
  
  #' Function to verify the existence of language-specific categories for text analysis.
  #' - If the selected language is English, the function checks for the presence of the `EN` folder.
  #' - If the selected language is Spanish, the function checks for the `ES` folder.
  #' If the corresponding folder is missing, the function provides instructions on how to create it.
  #' 
  #' @param lang 
  #' Corresponds to the selected language.
  #' - "ES" to print instruction messages in Spanish.
  #' - "EN" to print instruction messages in English.
  #' @return NULL 
  #' This function does not return a value but prompts actions in case of an error.
  #' @examples
  #' # Example usage:
  #' category_data_checker(lang = "ES")  # redirects to Spanish category checking.
  
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
get_exception_msgs<-function(lang, file_full_path ,e = NULL){
  
  #' Function that creates exception messages to prompt if errors occur during the reading of message files.
  #' 
  #' @param lang 
  #' Corresponds to the selected language.
  #' - "ES" to print instruction messages in Spanish.
  #' - "EN" to print instruction messages in English.
  #' @param file_full_path 
  #' The full path of the file to be opened.
  #' @param e 
  #' The exception error, if it occurs.
  #' @return NULL 
  #' This function does not return a value but creates messages to prompt in each case.
  
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


# --------------------
# cat_colored
#--------------------- 
cat_colored <- function(text, color) {
  
  #' Function to print colored text. Called from the `join_all_files` function.
  #' 
  #' @param text 
  #' The text to print.
  #' @param color 
  #' The color to apply to the text.
  #' @return NULL 
  #' This function does not return a value but prints the passed text in the specified color.
  #' @examples
  #' # Example usage:
  #' cat_colored("Hello, World!", "red")  # Prints "Hello, World!" in red color.
  
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
  
  #' Function that joins all message files to analyze into a single file.
  #' 
  #' @param file_path 
  #' The path of the file to open.
  #' @param lang 
  #' The selected language.
  #' - "ES" to print instruction messages in Spanish.
  #' - "EN" to print instruction messages in English.
  #' @return all 
  #' This function returns the final file containing all texts. 
  #' If an exception occurs, a message is printed and the function tries with another encoding.
  #' @examples
  #' # Example usage:
  #' join_all_files(file_path = "C:/file_path",
  #'                lang = "ES")  # Reads and joins all Spanish texts
  
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
  
  #' Function that checks how many messages have geolocation latitude and longitude data.
  #' 
  #' @param all 
  #' All messages in a single file to analyze.
  #' @param lang 
  #' The selected language.
  #' - "ES" to print instruction messages in Spanish.
  #' - "EN" to print instruction messages in English.
  #' @return NULL 
  #' This function does not return a value but prints the results.
  #' @examples
  #' # Example usage:
  #' lat_long_check(all = messages_data,
  #'                lang = "EN")  # Checks and prints the number of messages with geolocation data in English.
  
  sum_lat_lon<-sum(is.na(all[,2]))
  text <- NULL
  if(lang == "EN" & (nrow(all)-sum_lat_lon) < nrow(all)){
    cat("Proceeding to analyze geolocation data.\n")
    text <-sprintf("Only %d from %d messages have geolocalization data!!\nLoading default geolocalization data as demo.\n",(nrow(all)-sum_lat_lon),nrow(all))
  }else if(lang == "EN" & (nrow(all)-sum_lat_lon) == nrow(all)){
    text <-sprintf("Geolocation data added.\n")
  }else if(lang == "ES" & (nrow(all)-sum_lat_lon) < nrow(all)){
    cat("Procediendo a analizar datos de geolocalización.\n")
    text <-sprintf("Solo %d mensajes de %d presentan datos de geolocalización!!\nCargando datos de geolocalización de por defecto como demo.\n",(nrow(all)-sum_lat_lon),nrow(all))
  }else if(lang == "ES" & (nrow(all)-sum_lat_lon) == nrow(all)){
    text <-sprintf("Datos de geolocalización añadidos.\n")
  }
  
  if (!is.null(text)){
    cat(text)
  }
  return (text)

}

# ------------------------
#  clean_and_convert 
#-------------------------
clean_and_convert <- function(x) {
  
  #' Cleans and converts text to numeric values.
  #' 
  #' This function performs the following operations:
  #' - Replaces non-standard dashes and commas with periods.
  #' - Converts the cleaned text to numeric values.
  #' 
  #' @param x 
  #' A character vector or string to be cleaned and converted.
  #' @return numeric 
  #' The cleaned and converted numeric value.
  #' @examples
  #' # Example usage:
  #' clean_and_convert("1,234.56")  # Returns 1234.56 as a numeric value.
  #' clean_and_convert("1–234.56")  # Returns 1234.56 as a numeric value after replacing the en-dash with a hyphen.
  
  # Replace non-standard dashes and commas with periods
  x <- gsub("[–]", "-", x)   # Replace en-dashes with hyphens
  x <- gsub(",", ".", x)     # Replace commas with periods
  x <- as.numeric(x)         # Convert to numeric
  return(x)
}

#------------------
#  update_matches
#------------------
update_matches <- function(lang, matches, words, df, df_geo, aux, i, categoria) {
  
  #' Updates the counts and geographic data for terms based on matches found.
  #' 
  #' This function performs the following actions:
  #' - Updates the count of terms in the data frame `df` based on non-NA matches.
  #' - Updates the geographic data frame `df_geo` with latitude and longitude information.
  #' - Adds new terms to both data frames if they are not already present.
  #' 
  #' @param lang 
  #' The selected language. This parameter is not used in the function implementation but may be intended for future use.
  #' @param matches 
  #' A vector of matches indicating the positions of found terms.
  #' @param words 
  #' A vector of terms corresponding to the matches.
  #' @param df 
  #' A data frame containing term counts.
  #' @param df_geo 
  #' A data frame containing geographic information associated with terms.
  #' @param aux 
  #' A data frame containing additional information, including latitude and longitude.
  #' @param i 
  #' The index for the current row in the `aux` data frame.
  #' @param categoria 
  #' The category associated with the terms being updated.
  #' @return NULL 
  #' This function does not return a value but updates the data frames `df` and `df_geo`.
  #' @examples
  #' # Example usage:
  #' update_matches(lang = "EN", matches = c(1, NA, 2), words = c("term1", "term2"), 
  #'                df = existing_df, df_geo = existing_df_geo, aux = aux_data, 
  #'                i = 1, categoria = "Category1")  # Updates term counts and geographic data for English terms
  #'
  
  # Check if there are any non-NA matches
  if (sum(!is.na(matches)) > 0) {
    
    # Extract the term(s) corresponding to the non-NA matches
    terms <- words[!is.na(matches)]
    
    for (term in terms) {
      match_count <- sum(!is.na(matches))  # Count the non-NA matches
      
      if (term %in% df[, 1]) {
        # Term exists in the data frame, update its count
        pos <- grep(term, df[, 1])
        df[pos, 2] <- df[pos, 2] + match_count
        
        # Update the geographic data frame
        pos_geo <- grep(term, df_geo[, 1])
        
        if (length(pos_geo) > 0) {
          # If the term exists in df_geo, update its values
          df_geo[pos_geo, 2] <- df_geo[pos_geo, 2] + match_count
          
          # Create new geo value
          lat <- clean_and_convert(aux[i, "latitude"])
          lon <- clean_and_convert(aux[i, "longitude"])
          new_value <- c(aux[i, "name"], lat, lon)
          
          # Append the new value to the existing list in the name_lat_lon column
          prev_value <- df_geo$name_lat_lon[[pos_geo]]
          updated_value <- c(prev_value, list(new_value))
          df_geo$name_lat_lon[[pos_geo]] <- updated_value
          
        } else {
          # If the term is somehow not found, we add it to df_geo
          lat <- clean_and_convert(aux[i, "latitude"])
          lon <- clean_and_convert(aux[i, "longitude"])
          new_value <- c(aux[i, "name"], lat, lon)
          
          # Add the new term to df_geo with the corresponding geographic information
          df_geo <- rbind(df_geo, data.frame(word = term, cuantos = match_count,
                                             name_lat_lon = I(list(list(new_value))),
                                             categoria = categoria))
        }
        
      } else {
        # Term does not exist in the data frame, add it
        df <- rbind(df, data.frame(word = term, cuantos = match_count))
        
        # Create new geo value and add to geographic data frame
        lat <- clean_and_convert(aux[i, "latitude"])
        lon <- clean_and_convert(aux[i, "longitude"])
        new_value <- c(aux[i, "name"], lat, lon)
        
        # Add the new term to df_geo with the corresponding geographic information
        df_geo <- rbind(df_geo, data.frame(word = term, cuantos = match_count,
                                           name_lat_lon = I(list(list(new_value))),
                                           categoria = categoria))
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
  
  #' Organizes data frames into a list and identifies which ones are missing.
  #' 
  #' This function:
  #' - Checks if each provided data frame (`df_geo`, `df_p`, `df_n`) is non-empty.
  #' - Adds non-empty data frames to a list.
  #' - Records which data frames are missing.
  #' - Returns a list containing the non-empty data frames and an indicator of missing data frames.
  #' 
  #' @param df_geo 
  #' A data frame containing geographic data.
  #' @param df_p 
  #' A data frame containing positive sentiment data.
  #' @param df_n 
  #' A data frame containing negative sentiment data.
  #' @return list 
  #' A list where:
  #' - The first element is `df_geo` if it is not empty.
  #' - The second element is `df_p` if it is not empty.
  #' - The third element is `df_n` if it is not empty.
  #' - The fourth element is a vector indicating which of the first three data frames are missing.
  #' @examples
  #' # Example usage:
  #' df_geo <- data.frame(word = c("term1", "term2"), cuantos = c(5, 10))
  #' df_p <- data.frame(word = c("term1"), cuantos = c(3))
  #' df_n <- data.frame()  # Empty data frame
  #' result <- organize_data(df_geo = df_geo, df_p = df_p, df_n = df_n)
  #' # result will be a list with df_geo, df_p, and an indicator that df_n is missing.
  
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
# ------------------

score_sentiment <- function(sentences, pos_words, neg_words, lang){
  
  #' Performs sentiment analysis on a set of sentences and organizes the results.
  #' 
  #' This function:
  #' - Cleans and processes each sentence.
  #' - Checks sentiment against positive and negative word lists.
  #' - Updates sentiment counts and geographic data.
  #' - Displays results in the specified language.
  #' 
  #' @param sentences 
  #' A data frame containing sentences to be analyzed, with a column named `text`.
  #' @param pos_words 
  #' A vector of positive sentiment words.
  #' @param neg_words 
  #' A vector of negative sentiment words.
  #' @param lang 
  #' The selected language for output.
  #' - "ES" for Spanish output.
  #' - "EN" for English output.
  #' @return list 
  #' A list containing:
  #' - `df_geo`: The combined geographic data frame.
  #' - `df_p`: The data frame with positive sentiment counts.
  #' - `df_n`: The data frame with negative sentiment counts.
  #' The list is organized by the `organize_data` function.
  #' @examples
  #' # Example usage:
  #' sentences <- data.frame(text = c("I love this product", "I hate waiting"))
  #' pos_words <- c("love", "enjoy", "happy")
  #' neg_words <- c("hate", "angry", "bad")
  #' result <- score_sentiment(sentences = sentences, pos_words = pos_words, neg_words = neg_words, lang = "EN")
  #' # result will be a list containing the analyzed geographic data and sentiment counts.
  
  df_p<-NULL
  df_n<-NULL
  df_geo_p <-NULL
  df_geo_n <-NULL
  aux<-sentences
  sentences<-sentences$text
  
  if (lang =="ES"){
    cat(sprintf("Analizando %d textos...", length(sentences)))
  }else if(lang =="EN"){
    cat(sprintf("Analyizing %d texts...", length(sentences)))
  }
  
  for(i in 1:length(sentences)){
    
    #' Clean text:
    #'  - Remove punctuation
    #'  - Remove control characters
    #'  - Remove digits
    #'  - Replace multiple spaces with a single space
    #'  - Convert encoding to UTF-8
    #'  - Convert to lowercase
    
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
    result_pos  =  update_matches(lang, pos_matches, words, df_p, df_geo_p, aux, i, "p") 
    df_p = result_pos$df
    df_geo_p = result_pos$df_geo
    
    # check for negative matches
    result_neg  =  update_matches(lang, neg_matches, words, df_n, df_geo_n, aux, i, "n") 
    df_n = result_neg$df
    df_geo_n <- result_neg$df_geo
    
  }
  # organize data
  if (lang =="ES"){
    cat("\nTextos analizados. Mostrando resultados.\n")
  }else if(lang =="EN"){
    cat("\nTexts analyzed. Showing results.\n")
  }
  
  # join both geo data in a single df
  df_geo = rbind(df_geo_p,df_geo_n)
  
  return (organize_data(df_geo, df_p, df_n))
}

# ------------------------
#  plot_term_results 
#-------------------------
plot_term_results <-function(terms,str,lang,color){
  
  #' Generates and saves a word cloud and a term count plot, then creates a PDF with the results.
  #' 
  #' This function:
  #' - Creates a results folder if it doesn't exist.
  #' - Sets titles and labels based on the selected language.
  #' - Generates a word cloud and saves it as a PNG file.
  #' - Creates a ggplot2 scatter plot of term counts.
  #' - Combines both plots into a PDF file.
  #' 
  #' @param terms 
  #' A data frame with terms and their corresponding counts. Should have columns `word` and `cuantos`.
  #' @param str 
  #' A string used to name the output files and titles.
  #' @param lang 
  #' The selected language for the output.
  #' - "EN" for English.
  #' - "ES" for Spanish.
  #' @param color 
  #' A vector of colors to use in the word cloud.
  #' @return NULL 
  #' This function does not return a value but creates a PDF file with the results and displays it.
  #' @examples
  #' # Example usage:
  #' terms <- data.frame(word = c("positive", "negative", "neutral"), cuantos = c(10, 5, 2))
  #' plot_term_results(terms = terms, str = "SentimentAnalysis", lang = "EN", color = c("blue", "red", "grey"))
  #' # This will generate a PDF with a word cloud and a scatter plot of term counts.
  
  # Create corresponding result folder if it doesn't exist
  results_folder <- file.path(Sys.getenv("R_ROOT"), "RESULTS")
  
  if (!dir.exists(results_folder)) {
    dir.create(results_folder, recursive = TRUE, showWarnings = FALSE)
  }
  
  # Set PDF title based on language
  if (lang == "EN") {
    pdf_title <- paste0(str, "_TERM_INSTANCES.pdf")
    plot_title <- paste0(str, "_TERM_INSTANCES")
  } else if (lang == "ES") {
    pdf_title <- paste0("INSTANCIAS_TERMINOS_", str, ".pdf")
    plot_title <- paste0("INSTANCIAS_TERMINOS_", str)
  }
  
  # Create wordcloud with the corresponding color 
  wordcloud(terms$word, terms$cuantos, random.order = FALSE, colors = color)
  
  # and save it as PNG
  wordcloud_img_path <- file.path(results_folder, paste0("WORDCLOUD_",str,".png"))
  dev.copy(png,wordcloud_img_path)
  dev.off()
  
  # Convert the word cloud image to a grob
  wordcloud_img <- readPNG(wordcloud_img_path)
  wordcloud_grob <- rasterGrob(wordcloud_img, width = unit(3, "inches"), height = unit(3, "inches")) 
  
  # Calculate the max value and default step size
  max_value <- max(terms[, 2], na.rm = TRUE)
  
  # Obtain default break points
  default_breaks <- scales::pretty_breaks(n = 5)(c(0, max_value))
  step_size <- default_breaks[2] - default_breaks[1]
  
  # Calculate the next step limit
  y_limit <- ceiling(max_value / step_size) * step_size
  
  # Create ggplot object for the term appearance plot
  p.example <- ggplot(terms, aes(x = terms[, 1], y = terms[, 2])) +
    geom_point(color = color, size = 3) +
    ggtitle(plot_title) +  # Add title 
    labs(y = "Count", x = "Terms") +  # Label axes
    ylim(0, y_limit) +  # Set y-axis limits dynamically
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 10),   # Center the title and set size to 10
      axis.title.y = element_text(size = 10),              # Set y-axis title text size to 10
      axis.text.x = element_text(angle = 45, hjust = 1, size = 10),  # Set x-axis text size to 10
      axis.text.y = element_text(size = 10),               # Set y-axis text size to 10
      axis.line.x = element_line(color = "black"),
      axis.line.y = element_line(color = "black")
    ) + 
    annotation_custom(
      wordcloud_grob,
      xmin = 0, xmax = 7, ymin = Inf, ymax = 0
    )
  
  pdf(file = file.path(results_folder, pdf_title), width = 14, height = 6, onefile = TRUE, paper = "a4r")
  print(p.example)
  dev.off()
  
  # Show the PDF file
  file.show(file.path(results_folder, pdf_title))

}



# ------------------------
#  geolocalizated_results 
#-------------------------
geolocalizated_results <- function(data, lang) {
  
  
  #' Generates a geolocalized sentiment analysis map and saves it as an HTML file.
  #' 
  #' This function:
  #' - Creates a results folder if it doesn't exist.
  #' - Sets the title based on the selected language.
  #' - Expands the input data to handle multiple geolocations per word.
  #' - Visualizes the data using a leaflet map with color-coded markers.
  #' - Saves the map as an HTML file and displays it.
  #' 
  #' @param data 
  #' A data frame containing sentiment analysis results with geolocated information. Should have columns:
  #' - `word`: The term associated with the sentiment.
  #' - `name_lat_lon`: A list containing name, latitude, and longitude for each term.
  #' - `categoria`: Sentiment category, e.g., "p" for positive, "n" for negative.
  #' @param lang 
  #' The selected language for the output.
  #' - "EN" for English.
  #' - "ES" for Spanish.
  #' @return NULL 
  #' This function does not return a value but creates an HTML file with a leaflet map and displays it.
  #' @examples
  #' # Example usage:
  #' data <- data.frame(
  #'   word = c("happy", "sad"),
  #'   name_lat_lon = I(list(
  #'     list("Location1", "40.7128", "-74.0060"),
  #'     list("Location2", "34.0522", "-118.2437")
  #'   )),
  #'   categoria = c("p", "n")
  #' )
  #' geolocalizated_results(data = data, lang = "EN")
  #' # This will generate and display a leaflet map with the geolocalized sentiment data.
  
  # Create the results folder if it doesn't exist
  results_folder <- file.path(Sys.getenv("R_ROOT"), "RESULTS")
  if (!dir.exists(results_folder)) {
    dir.create(results_folder, recursive = TRUE, showWarnings = FALSE)
  }
  
  # Set title based on language
  if (lang == "EN") {
    text <- "SENTIMENT ANALYSIS GEOLOCALIZED"
  } else if (lang == "ES") {
    text <- "ANÁLISIS DE SENTIMIENTO GEOLOCALIZADO"
  }
  
  # Expand data to account for multiple geolocations per word
  expanded_data <- data %>%
    rowwise() %>%
    do({
      # Extract the list of geolocations for each word
      locations <- .$name_lat_lon
      
      # Create a data frame with one row per location
      if (is.list(locations) && all(sapply(locations,length) == 3)){
        data.frame(
          word = .$word,
          cuantos = 1,  # Each row represents one occurrence
          categoria = .$categoria,
          name = sapply(locations,`[[`, 1),
          lat = as.numeric(sapply(locations,`[[`, 2)),
          lon = as.numeric(sapply(locations,`[[`, 3)),
          stringsAsFactors = FALSE
        )
      }
    }) %>%
    ungroup()
  
  # Apply color mapping
  expanded_data$marker_color <- ifelse(expanded_data$categoria=="p", "Green", "Red")
  
  # Combine name and word for popup and label
  expanded_data$popup_label <- paste(expanded_data$name, " (", expanded_data$word, ") ", sep = "")
  
  # Visualize using leaflet
  map <- leaflet(expanded_data) %>%
    addTiles(group = "OSM") %>%
    addCircleMarkers(
      lat = ~lat,
      lng = ~lon,
      radius = 3,    # Adjust the size of the circle markers
      color = ~marker_color, # Adjust the color
      popup = ~popup_label,   # Show custom values
      label = ~popup_label   
    )
  
  # Save the leaflet map as an HTML file
  html_title <- file.path(results_folder, paste(text, ".html", sep = ""))
  saveWidget(map, file = html_title, selfcontained = TRUE)
  
  # Show result
  file.show(html_title)
}



# ----------------------
# ACTION HANDLERS
# handle_main_actions
#-----------------------

handle_main_actions<-function(option){
  
  #' Handles the main actions based on the selected language option for sentiment analysis.
  #' 
  #' This function:
  #' - Determines the language based on the selected option.
  #' - Checks for necessary message files and sentiment categories.
  #' - Joins all message files into a single data frame.
  #' - Verifies and updates geolocation data.
  #' - Reads sentiment terms for positive and negative sentiment analysis.
  #' - Scores the sentiment based on the combined data.
  #' - Handles and processes the sentiment scores accordingly.
  #' 
  #' @param option 
  #'   1 directs to Spanish sentiment files creation.
  #'   2 directs to English sentiment files creation.
  #' @return NULL 
  #' This function does not return a value but performs various actions based on the selected language option.
  #' @examples
  #' # Example usage:
  #' handle_main_actions(option = 1)  # Redirect to Spanish sentiment analysis actions
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
        cty<-read.csv(paste(Sys.getenv("R_ROOT"),"countries.csv", sep="/"),sep=",",stringsAsFactors = F,dec=".")
        
        # Assign random latitude, longitude, and name from cty to all
        set.seed(123) # Set seed for reproducibility
        
        # Find rows in `all` with missing latitude or longitude
        missing_coords_indices <- which(is.na(all$latitude) | is.na(all$longitude))
        
        # Randomly select rows from `cty` to assign to these missing rows
        random_cty_rows <- cty[sample(nrow(cty), length(missing_coords_indices), replace = TRUE), ]
        
        # Assign random latitude, longitude, and name from `cty` to the missing rows in `all`
        all[missing_coords_indices, "latitude"] <- random_cty_rows$latitude
        all[missing_coords_indices, "longitude"] <- random_cty_rows$longitude
        all[missing_coords_indices, "name"] <- random_cty_rows$name
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
  
  #' Handles actions based on the selected language option for sentiment analysis.
  #' 
  #' This function:
  #' - Determines the language based on the selected option.
  #' - Directs to the appropriate main actions for sentiment analysis in the selected language.
  #' - Closes the program if the option is 0.
  #' - Prompts an error message for invalid options and continues the loop if needed.
  #' 
  #' @param option 
  #'   1 directs to Spanish sentiment analysis actions.
  #'   2 directs to English sentiment analysis actions.
  #'   0 closes the program.
  #' @return NULL 
  #' This function does not return a value but performs various actions based on the selected option.
  #' @examples
  #' # Example usage:
  #' handle_language_options(option = 1)  # Redirect to Spanish sentiment analysis
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
  
  #' Handles the results of sentiment analysis based on the provided scores.
  #' 
  #' This function:
  #' - Defines language-specific strings for messages and labels.
  #' - Checks for missing data and generates appropriate messages.
  #' - Calls plotting functions to visualize positive and negative term results.
  #' - Displays geolocalized results if data is available.
  #' 
  #' @param scores 
  #'   A list containing sentiment analysis results. It includes:
  #'   - Geolocalized data
  #'   - Positive term counts
  #'   - Negative term counts
  #'   - Flags indicating missing data categories (1: geolocation, 2: positive, 3: negative)
  #' @param lang 
  #'   Specifies the language for output messages.
  #'   - "ES" for Spanish
  #'   - "EN" for English
  #' @return NULL 
  #' This function does not return a value but generates messages and plots based on the analysis results.
  #' @examples
  #' # Example usage:
  #' handle_scores(scores = list(geo_data, pos_terms, neg_terms, missing_flags), lang = "EN")
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
  geo <- pos <- neg <- 1
  mtext <- ""
  
  # Handle the case where some data is missing
  if (length(scores) == 4) {
    for (i in seq_along(scores[[4]])) {
      switch(scores[[4]][i],
             `1` = {
               mtext <- paste(mtext, strings$no_geoloc)
               geo <- 0
             },
             `2` = {
               mtext <- paste(mtext, strings$no_positive)
               pos <- 0
             },
             `3` = {
               mtext <- paste(mtext, strings$no_negative)
               neg <- 0
             }
      )
    }
    
    # Print the missing data message if any
    if (nchar(mtext) > 0) cat(mtext)
    
    # Plot results based on flags
    if (pos == 1) plot_term_results(scores[[2]], strings$positive, lang, "darkgreen")
    if (neg == 1) plot_term_results(scores[[3]], strings$negative, lang, "darkred")
    if (geo == 1) geolocalizated_results(scores[[1]], lang)
    
  } else {
    # Handle the case where all data is present
    plot_term_results(scores[[2]], strings$positive, lang, "darkgreen")
    plot_term_results(scores[[3]], strings$negative, lang, "darkred")
    geolocalizated_results(scores[[1]], lang)
  }
}
