
# TODO: komentatu kodigoa

# -----------------------------
# createDefaultSentimentFiles
#-----------------------------
createDefaultSentimentFiles<-function()
{
  dir<-paste(Sys.getenv("R_ROOT"),"/1-Emotions_categorization_files/",sep="")
  #EN
  source("English_Sentiment_Files.R")
  #ES
  setwd(dir)
  source("Spanish_Sentiment_Files.R")
  
  # setwd(Sys.getenv("R_ROOT"))
}

# ------------
# knowLanguage
#------------

knowLanguage <-function(lang)
{
  return(ifelse(lang==1,"ES","EN"))
}

# ---------------
# visualize Files
#------------------

visualizeFiles <-function(lang)
{
  setwd(paste(Sys.getenv("R_ROOT"),"/",lang,sep=""))
  titP<-paste("Pos_",lang,".csv",sep="")
  titN<-paste("Neg_",lang,".csv",sep="")
  
  pos<- read.csv(file=titP)
  neg<- read.csv(file=titN)
  View(pos)
  View(neg)
}

# --------------------
# getAllPromptMessages
#---------------------
getAllPromptMessages<-function(lang)
{
  aux <-list()
  
  if(lang=="ES")
  {
    aux[[1]]<-"\nDeseas anadir nuevos atributos a la lista de por defecto :\n"
    aux[[2]]<-c("SI","NO")
    aux[[3]]<-"Indica la lista a complementar :\n"
    aux[[4]] <-c("POSITIVOS","NEGATIVOS","AMBOS")
    aux[[5]] <-"Empieza a introducir datos: (0 para terminar)"
    aux[[6]] <-"VALORES POSITIVOS:\n"
    aux[[7]] <-"VALORES NEGATIVOS:\n"
    aux[[8]] <-"Nuevo fichero(s) guardados\n"
  }
 else
  {
    aux[[1]]<-"\nWould you like to add new atributes to defaults ones:\n"
    aux[[2]]<-c("YES","NO")
    aux[[3]]<-"In which one :\n"
    aux[[4]] <-c("POSITIVES","NEGATIVES","BOTH")
    aux[[5]] <-"Add atributes: (0 to stop)"
    aux[[6]] <-"POSITIVE VALUES:\n"
    aux[[7]] <-"NEGATIVE VALUES:\n"
    aux[[8]] <-"New file(s) save :\n"
  }
  
  return(aux)
}

# ----------------------------
# addDataToCorrespondingFile
#----------------------------

addDataToCorrespondingFile <-function(lang,file,text)
{
  handleFiles(file,text,lang)
}

# --------------
# loopAskData
#---------------

loopAskData<-function(text)
{
  str <- -1
  lista <-vector()
  while(str != 0)
  {
    cat(text[[5]]) #MSG:INTRODUZCA DATOS
    str <-readline()
    if(str!=0 && str!="") lista<-c(lista,str)
     # print(lista)
  }
  #deleting possible NA values
  lista<-lista[!is.na(lista)]
  return(as.vector(lista))
}

# --------------
# loopAskDataBoth
#---------------
loopAskDataBoth<-function(text)
{
  str1 <- -1
  str2 <- -1
  l1 <-vector()
  l2 <-vector()
  
  cat(text[[6]]) #MSG:VALORES POSITIVOS
  while(str1 != 0)
  {
    cat(text[[5]]) #MSG:INTRODUZCA DATOS
    str1 <-readline()
    if(str1!=0 && str1!="") l1<-c(l1,str1)
    # print(l1)
  }
  cat(text[[7]]) #MSG:VALORES NEGATIVOS
  while(str2 != 0)
  {
    cat(text[[5]]) #MSG:INTRODUZCA DATOS
    str2 <-readline()
    if(str2!=0 && str2!="") l2<-c(l2,str2)
    # print(l2)
  }
  #deleting possible NA values
  l1<-l1[!is.na(l1)]
  l2<-l2[!is.na(l2)]
  
  max.len = max(length(l1), length(l2))
  x = c(l1, rep(NA, max.len - length(l1)))
  y = c(l2, rep(NA, max.len - length(l2)))
  
  df = list(x=x, y=y)
  attributes(df) = list(names = names(df),
                        row.names=1:max(length(x), length(y)), class='data.frame')
  names(df)<-c("Pos","Neg")
  return(df)
  
}


# -------------
# handleFiles
#--------------

handleFiles<-function(opt,text,lang)
{
  dir<-paste(Sys.getenv("R_ROOT"),"/",lang,"/",sep="")
  
  if(opt==1) # pos
  {
    #Ask for new features
    cat(text[[6]]) #MSG:VALORES POSITIVOS
    l<-loopAskData(text)
    
    #loading corresponding file
    titP<-paste("Pos_",lang,".csv",sep="")
    pos <- read.csv(file=paste(dir,titP,sep=""))
    
    #adding new values
    pos<- data.frame(lapply(pos, as.character), stringsAsFactors=FALSE)
    pos<-c(pos[,1],t(l))
    
    #removing duplicates
    pos <- pos[!duplicated(pos)]
    
    #saving changes
    setwd(dir)
    write.table(pos, file=titP, row.names=F,col.names=F)
    pos<-read.csv(file=titP,header=F)
    View(pos)
    #MSG:ARCHIVOS GUARDADOS
    cat(text[[8]])
    
  }
  if(opt==2)# neg
  {
    #Ask for new features
    cat(text[[7]]) #MSG:VALORES NEGATIVOS
    l<-loopAskData(text)
    
    #loading corresponding file
    titN<-paste("Neg_",lang,".csv",sep="")
    neg <- read.csv(file=paste(dir,titN,sep=""))

    #adding new values
    neg<- data.frame(lapply(neg, as.character), stringsAsFactors=FALSE)
    neg<-c(neg[,1],t(l))
    
    #removing duplicates
    neg <- neg[!duplicated(neg)]
    
    #saving changes
    setwd(dir)
    write.table(neg, file=titN, row.names=F,col.names=F)
    neg<-read.csv(file=titN,header=F)
    View(neg)
    #MSG:ARCHIVOS GUARDADOS
    cat(text[[8]])
  }
  
  if(opt==3) #both
  {
    #Ask for new features
    df<-loopAskDataBoth(text)
    
    #loading corresponding file
    titP<-paste("Pos_",lang,".csv",sep="")
    pos <- read.csv(file=paste(dir,titP,sep=""))
    #
    titN<-paste("Neg_",lang,".csv",sep="")
    neg <- read.csv(file=paste(dir,titN,sep=""))
    
    #adding new values
    pos<- data.frame(lapply(pos, as.character), stringsAsFactors=FALSE)
    pos<-c(pos[,1],t(df[,1]))
    #
    neg<- data.frame(lapply(neg, as.character), stringsAsFactors=FALSE)
    neg<-c(neg[,1],t(df[,2]))
    
    #removing duplicates
    pos <- pos[!duplicated(pos)]
    neg <- neg[!duplicated(neg)]
    
    #saving changes
    setwd(dir)
    write.table(pos, file=titP, row.names=F,col.names=F)
    write.table(neg, file=titN, row.names=F,col.names=F)
  
    pos<-read.csv(file=titP,header=F)
    View(pos)
    neg<-read.csv(file=titN,header=F)
    View(neg)
    #MSG:ARCHIVOS GUARDADOS
    cat(text[[8]])
  }
}



