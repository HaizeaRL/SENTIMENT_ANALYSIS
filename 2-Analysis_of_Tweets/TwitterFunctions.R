# -----------------------
#  package installation
#-----------------------

options(warn=-1)

#connect all libraries
if(!require(twitteR)){
  install.packages("twitteR")
  library(twitteR)
}

if(!require(plyr)){
  install.packages("plyr")
  library(plyr)
}

if(!require(dplyr)){
  install.packages("dplyr")
  library(dplyr)
}

if(!require(stringr)){
  install.packages("stringr")
  library(stringr)
}

if(!require(ggplot2)){
  install.packages("ggplot2")
  library(ggplot2)
}

if(!require(ggmap)){
  install.packages("ggmap")
  library(ggmap)
}


if(!require(RColorBrewer)){
  install.packages("RColorBrewer")
  library(RColorBrewer)
}


if(!require(wordcloud)){
  install.packages("wordcloud")
  library(wordcloud)
}

if(!require(reshape2)){
  install.packages("reshape2")
  library(reshape2)
}

if(!require(png)){
  install.packages("png")
  library(png)
}

if(!require(tcltk)){
  install.packages("tcltk")
  library(tcltk)
}


# DATA 

consumer_key <- "oGbGZQk0l587t8HDtPol7xFjH"

consumer_secret<- "RMP26xOyAECi1TMmXMCW6UA4Cf8ZaTc37uV10uu439KuHrTZWM"

setup_twitter_oauth(consumer_key, consumer_secret, access_token=NULL, access_secret=NULL)

# ------------------
#  search
#------------------

#the function of tweets accessing and analyzing
search <- function(searchterm,lang)
{
   respuesta<--1
  
    mainDir <-Sys.getenv("R_ROOT")
    subcarpeta <-paste("TWEETS",lang,sep="_")
    
    if(!dir.exists(file.path(mainDir, subcarpeta)))
      dir.create(file.path(mainDir, subcarpeta),recursive = TRUE,showWarnings = F)
    
    setwd(file.path(mainDir, subcarpeta))

    #access tweets and create cumulative file
    list <- searchTwitter(searchterm, lang=lang, n=1500)
    
    if(length(list)!=0)
    {
      #convertir en data.frame
      df <- twListToDF(list)
      #quedarse con las colunmas correspondientes: Text, longitud,latitud
      df <- df[,c(1,(ncol(df)-1),ncol(df))]
      
      #si faltan datos tambien queremos guardarlos
      df[which(is.na(df[,"longitude"])),"longitude"]<-"NA"
      df[which(is.na(df[,"latitude"])),"latitude"]<-"NA"
      
      if (file.exists(paste(searchterm, '_stack.csv'))==FALSE) write.csv(df, file=paste(searchterm, '_stack.csv'), row.names=F)
      
      #merge last access with cumulative file and remove duplicates
      stack <- read.csv(file=paste(searchterm, '_stack.csv'))
      stack <- rbind(stack, df)
      stack <- subset(stack, !duplicated(stack$text))
      write.csv(stack, file=paste(searchterm, '_stack.csv'), row.names=F)
      respuesta = 0
    }
    

    setwd(Sys.getenv("R_ROOT"))
    return(respuesta)

}


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




