dirRoot<-Sys.getenv("R_ROOT")

# ----------
# POSITIVO
#----------
pos<-c("Pos","Atractivo","Ideal","Precio especial","Robusto","Elegante","Cumple las necesidades","Portable",
            "Diferente","A la moda","Fantastico","Facil", "Reusable","Rapido","Agil","Unico","Manejable","Actual",
            "Beneficioso","Increible","Brillante","Practico","Impactante", "Divertido","Bonito","Pulcro","Nuevo",
            "Comodo","Maravilloso","Inteligente","Seguro","Satisfactorio","Contento","Alegre","Lujoso","Novedoso","Moderno")

#ES
mainDir <-dirRoot
lang <-"ES"

if(!dir.exists(file.path(mainDir, lang)))
  dir.create(file.path(mainDir, lang),recursive = TRUE,showWarnings = F)

setwd(file.path(mainDir, lang))

titP<-paste("Pos_",lang,".csv",sep="")
if(!file.exists(titP)) write.table(pos, titP, row.names=F,col.names=F) 

# ----------
# NEGATIVO
#----------
neg<-c("Neg","Feo","Sucio","Muy simple","Complejo","Dificil","Inseguro","Peligroso","Debil","Fragil",
            "Aburrido","Obsoleto", "Desordenado","Pobre","Horrible","Descontento","Insatisfecho",
            "Enfadado","Decepcionado","Frustrante","Sin sentido")


#ES
mainDir <-dirRoot
lang <-"ES"

if(!dir.exists(file.path(mainDir, lang)))
  dir.create(file.path(mainDir, lang),recursive = TRUE,showWarnings = F)

setwd(file.path(mainDir, lang))

titN<-paste("Neg_",lang,".csv",sep="")
if(!file.exists(titN)) write.table(neg, titN, row.names=F,col.names=F) 