#INITIALIZATIONS
source("TwitterFunctions.R")

language <-NULL

cat("Indica en que idioma quieres buscar los datos\n")
language <- menu(c("ES","EN"))

if(!is.null(language))
{
  resp<--1
  if(language ==1)
  {
     str<-"Indica la palabra a buscar:"
     lg<-"ES"
  }
  if(language ==2)
  {
    str<-"Add term to find:"
    lg<-"EN"
  }
  while(resp == -1)
  {
    searchterm<-readline(str)
    resp <-search(searchterm,lg)
    if(resp == -1)
    {
      if(lg=="ES")  text <-"NO SE HAN ENCONTRADO DATOS CON ESE TERMINO!! INTRODUZCA OTRO "
      if(lg=="EN")  text <-"NO DATA!! TRY WITH ANOTHER "
      
      button <- tkmessageBox(title='Message',message=text,type='ok')
      button <- tclvalue(button)
    }
  }
   
}

