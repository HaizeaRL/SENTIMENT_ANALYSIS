
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
    library(pck, character.only = TRUE)
  }
}

# twitteR

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
      ctext <- sprintf("There are no %s messages to analyze!!\nPlease perform a message search. You can use the script: `2.1- Message_searching.R` for this purpose.", text)
    }else if(lang == "ES"){
      text = "Español"
      ctext <- sprintf("¡¡No existen mensajes en idioma %s para analizar!!\nPor favor realiza la búsqueda de mensajes. Puede valerse del script: `2.1- Message_searching.R` para ello.\n", text)
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
    if(lang == "EN") ctext <-"No sentiment categories have been created!!.\nPlease run the script: `1- Emotions_files_creation.R`to create."
    if(lang == "ES") ctext <-"¡¡No se han creado categorías de sentimientos!!\nPor favor, ejecute el script: `1- Emotions_files_creation.R para crearlas.\n"
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
    
    aux[[1]]<-sprintf("Error leyendo fichero: %s \n",file_full_path)
    aux[[2]]<-sprintf("Mensaje de error: %s \nIntentando abrir con encoding UTF-8-BOM ...\n",e$message)
    aux[[3]]<-sprintf("Error abriendo fichero: %s en el segundo intento\n",file_full_path)
    aux[[4]]<-sprintf("Mensaje de error: %s \n",e$message)
  }
  else {
    aux[[1]]<-sprintf("Error reading file: %s \n",file_full_path)
    aux[[2]]<-sprintf("Error message: %s \nTrying again with UTF-8-BOM encoding...\n",e$message)
    aux[[3]]<-sprintf("Second attempt failed for file: %s \n",file_full_path)
    aux[[4]]<-sprintf("Error message: %s \n",e$message)
  }
  
  return(aux)
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
        cat(msg[[1]])
        cat(msg[[2]])
        tryCatch({
          suppressWarnings(read.csv(file=file_full_path, fileEncoding="UTF-8-BOM"))
        }, error = function(e) {
          cat(msg[[3]])
          cat(msg[[4]])
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
  

  # determine msg num. with lat, log data
  sum_lat_lon<-sum(is.na(all[,2]))
  text <- NULL
  if(lang == "EN"){
    text <-sprintf("Only %d from %d messages have geolocalization data!!\n Loading default geolocalization data as demo.",(nrow(all)-sum_lat_lon),nrow(all))
  }
  else if(lang == "ES"){
    text <-sprintf("Solo %d mensajes de %d presentan datos de geolocalización!!\nCargando datos de geolocalización de por defecto como demo.",(nrow(all)-sum_lat_lon),nrow(all))
  }
  
  if (!is.null(text)){
    cat(text)
  }
  return (text)

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
        lat_lon <- lat_long_check(all, lang)
        
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
#  score.sentiment 
#------------------

#evaluation tweets function
score.sentiment <- function(sentences, pos.words, neg.words)
{
  df_p<-NULL
  df_n<-NULL
  df_geo <-NULL
  aux<-sentences
  sentences<-sentences$text
  for(i in 1:length(sentences))
  {
    print(paste("Quedan ", length(sentences)-i, " textos", sep=""))
    sentence <- gsub('[[:punct:]]', "", sentences[i])
    sentence <- gsub('[[:cntrl:]]', "", sentences[i])
    sentence <- gsub('[[:digit:]]', "", sentences[i])
    sentence <-gsub('[[:space:]]', " ", sentences[i])
    sentence <- tolower(sentences[i])
    word.list <- str_split(sentences[i], ' ')
    words <- unlist(word.list)
    pos.matches<-match(words, tolower(unlist(pos.words)))
    neg.matches<-match(words, tolower(unlist(neg.words)))
    
    if(sum(!is.na(pos.matches))>0)
    {
      term<-words[!is.na(pos.matches)]
      if((term %in% df_p[,1]))
      {
        pos<-grep(term,df_p[,1])
        df_p[pos,2]<-df_p[pos,2]+sum(!is.na(pos.matches))
        if((df_geo[,"lon"] == aux[i,"longitude"]) && (df_geo[,"lat"] == aux[i,"latitude"]))
        {
          pos<-grep(term,df_geo[,1])
          df_geo[pos,2]<-df_geo[pos,2]+sum(!is.na(pos.matches))
        }
      }
      else {
        df_p<- rbind(df_p,data.frame(word=term,cuantos=sum(!is.na(pos.matches))))
        df_geo<-rbind(df_geo,data.frame(word=term,cuantos=sum(!is.na(pos.matches)),lon=aux[i,"longitude"],lat=aux[i,"latitude"],name=aux[i,"name"],categoria="p"))            
      }
    }
    
    if(sum(!is.na(neg.matches))>0)
    {
      term<-words[!is.na(neg.matches)]
      if((term %in% df_n[,1]))
      {
        pos<-grep(term,df_n[,1])
        df_n[pos,2]<-df_n[pos,2]+sum(!is.na(neg.matches))
        if((df_geo[,"lon"] == aux[i,"longitude"]) && (df_geo[,"lat"] == aux[i,"latitude"]))
        {
          pos<-grep(term,df_geo[,1])
          df_geo[pos,2]<-df_geo[pos,2]+sum(!is.na(neg.matches))
        }
      }
      else {
        df_n<- rbind(df_n,data.frame(word=term,cuantos=sum(!is.na(neg.matches))))
        df_geo<- rbind(df_geo,data.frame(word=term,cuantos=sum(!is.na(neg.matches)),lon=aux[i,"longitude"],lat=aux[i,"latitude"],name=aux[i,"name"],categoria="n"))
      }
      
    }
    # # scan(n=1)
    # print(df_p)
    # print(df_n)
  }
  l<-list()
  noDatapos <-vector()
  #geo table
  if(length(df_geo)!=0)   l[[1]]<-df_geo
  if(length(df_geo)==0)   noDatapos <- c(noDatapos,1)
  
  #positive term table
  if(length(df_p)!=0)   l[[2]]<-df_p
  if(length(df_p)==0)   noDatapos <- c(noDatapos,2)
  
  #negative term table
  if(length(df_n)!=0)   l[[3]]<-df_n
  if(length(df_n)==0)   noDatapos <- c(noDatapos,3)
  
  #missing values
  if(length(noDatapos)!=0) l[[4]]<-noDatapos
  return(l)
  
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




