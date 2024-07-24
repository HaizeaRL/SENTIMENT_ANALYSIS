dirRoot<-Sys.getenv("R_ROOT")

# ----------
# POSITIVE
#----------
pos<-c("Pos","Attractive","Ideal","Special Price","Robust","Elegant","Meet the needs","Highly portable",
            "Trendy","Fashionble","Fantastic","Portable", "Reusable","Quick","Agile","Unique","Luxury",
            "User-Friendly","Beneficial","Amazing","Billiant","Practical","Atonishing", "Awesome",
            "Beautiful","Sleek","Incredible","Gorgeous","Intelligent","Smart","Easy","Safe","Funny","The latest")

#ES
mainDir <-dirRoot
lang <-"EN"

if(!dir.exists(file.path(mainDir, lang)))
  dir.create(file.path(mainDir, lang),recursive = TRUE,showWarnings = F)

setwd(file.path(mainDir, lang))

titP<-paste("Pos_",lang,".csv",sep="")
if(!file.exists(titP)) write.table(pos, titP, row.names=F,col.names=F) 

# ----------
# NEGATIVE
#----------
neg<-c("Neg","Ugly","Dirty","Too Simple","Complex","Difficult","Unsafe","Dangerous","Risky","Weak",
            "Same","Messy", "Poor","Awful","WTF","Unpleased","Dissatisfied","Unhappy","Annoyed",
            "Angry","Boring")

#ES
mainDir <-dirRoot
lang <-"EN"

if(!dir.exists(file.path(mainDir, lang)))
  dir.create(file.path(mainDir, lang),recursive = TRUE,showWarnings = F)

setwd(file.path(mainDir, lang))

titN<-paste("Neg_",lang,".csv",sep="")
if(!file.exists(titN)) write.table(neg, titN, row.names=F,col.names=F) 