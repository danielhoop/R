### Shortcuts
browseURL("http://www.rstudio.com/ide/docs/using/keyboard_shortcuts")

#### Vorbereitungen ####
memory.limit(4095)
rm(list=ls())
rm(list=gdata::keep(list=c("a","b",as.character(lsf.str())))); # Alle Funktionen behalten!

#### Searching help ####
# Normale Hilfe
?mean # =
help(mean)
# erweiterte Hilfesuche
??mean #=
help.search("mean")

# Online Hilfe suchen:
browseURL("http://search.r-project.org/search.html")                     # Search functions, vignettes, tasks
browseURL("http://rseek.org/")                                           # R search
browseURL("http://r.789695.n4.nabble.com/")                              # R-Forum search
browseURL("http://www.r-project.org/search.html")                        # All R-searches
browseURL("http://cran.r-project.org/doc/manuals/R-lang.html")           # R Manual

# Suche auf R help site
RSiteSearch("benchmarking")
RSiteSearch("jump regression", restric='functions')

# Warum ist RStudio so langsam?!?!
# Post this on http://support.rstudio.org/help/discussions/problems/5991-slow-execution-of-multiple-lines-of-r-code-or-sourced-r-code
#I already read this thread
#http://support.rstudio.org/help/discussions/problems/1232-extremely-slow-execution-of-simple-commands
# Port anschauen, der gebraucht wird f?r die ?bermittlung von RStudio du Rsession
#browseURL("https://support.rstudio.com/hc/communities/public/questions/200658363-Slow-response-of-R-from-RStudio-Windows7-Desktop-")
tools:::httpdPort
sessionInfo()

# in eine Funktion schauen
getS3method("summary", "lm")
getAnywhere("rlm")

# In Paketen nach Funktion suchen (1)
library(sos); findFn("as.Formula")
# In Paketen nach Funktion suchen (2)
library(sos)
jr <- ???'jump regression'
summary(jr)
#
bs <- ???'Bayesian step'
bsw <- ???'Bayesian stepwise'
bs. <- bs|bsw
summary(bs.)

###### Packages ######
# Alle installierten Packages wieder installieren mit Funktionen save.packages() & recover.R.installation()
detach("package:gplots") # Package von Workspace entfernen.
install.packages("fortunes")
install.packages("psych") # Skewness, etc. psych::describe(1:100)
install.packages("nnls") # Positive Coefficient regressions
install.packages("mgcv") # Penalized Regressions  (e.g. greater than 0)
install.packages("rbenchmark") # See how much time a function needs
install.packages("quadprog") # Needed to solve quadratic prgramming minimization problems. Like e.g. linear regression with only positive coefficients.
install.packages("fpc") #, dependencies = TRUE) # Vorsicht mit dem depencencies Befehl!  # Clustering-Package (mit Validation)
install.packages("pvclust")   # P-Werte von Clustern errechnen (mittels Bootstrapping)
install.packages("mclust")    # Model-basiertes Clustering
install.packages("vegan",repos="http://r-forge.r-project.org")     # Allgemeines Statistik-Package
install.packages("clValid")   # Cluster statistisch ueberpruefen
install.packages("plyr")      # apply Paket
#install.packages(file.choose(), repos=NULL)# f?r local zip Files (e.g. Package FEAR)
# 'FEAR is not available for R 3.0.1' stimmt nicht! mit library(FEAR) laden, License Key mit ---FEAR-license-key--- kopieren, an pww@clemson.edu mailen.
install.packages('F:/Mobile Software/R/FEAR_2.0.zip', repos = NULL) # '', nicht ""!
install.packages("Benchmarking") # DEA
install.packges("frontier")
install.packages("agricolae"); install.packages("pgirmess") # Kruskal-Wallis-Test mit Unterschieden zwischen mehreren Gruppen.
install.packages("sos") # findFN("")
install.packages("ggplot2"); install.packages("gridExtra"); install.packages("reshape2") #
install.packages("scales"); # F?r Farbpaletten.
install.packages("gdata"); #install.packages("xlsReadWrite")
require(gdata); #require("xlsReadWrite") # to directly read in xls files. Nicht auf Bundesnetz installierbar!
install.packages("earth") # MARS - Regressionen. Sehr gut!
install.packages("extrafont"); font_import() # Schriften f?r Grafiken importieren
install.packages("mvoutlier") # Multivariate Outlier Detection.
install.packages("car") # Compagnion to applied Regresson
install.packages("Ryacas") # Package, um Funktionen abzuleiten
#  car::scatterplot(y~year|country,  boxplots=FALSE, smooth=TRUE,  reg.line=FALSE, data=Panel)
install.packages("rpanel") # Package, um mit Buttons R-Funktionen auszufuehren. Oder in Bilder zu zeichnen.

# Working with C++ Code... (Quelle u.a. Youtbe Video https://www.youtube.com/watch?v=UZkaZhsOfT4)
install.packages("Rcpp") # Easily interact between R and C++.
install.packages("inline") # Write code in another programming language inside R. Compile, etc. Magnificent!
install.packages("pryr") # Easily find C-code behind primitive R functions.
# Compiler for Windows: https://cran.r-project.org/bin/windows/Rtools/


# Fuer zip-files repos=NULL setzten. type nicht setzen!
install.packages('P:/Software/R/packages/earth_3.2-7.zip', repos=NULL) # '', nicht ""!
# Fuer tar.gz files repos=NULL, type='source' setzten!
install.packages("P:/Software/R/packages/bzip2-1.0.6.tar.gz", repos=NULL, type='source') # 'source', nicht "source"!
# Alternative, direkt ab Internet
install.packages("curl"); library(curl)
curl_download('http://www.systematicportfolio.com/SIT.tar.gz', 'sit',mode = 'wb',quiet=T)
install.packages('sit', repos = NULL, type='source')
# Installieren inoffizieller Packages von github
install.packages("devtools")
devtools::install_github("kassambara/r2excel") # Ergibt Fehler.


## Installing released version vs. development version
install.packages("roxygen2")
# install.packages("devtools")
devtools::install_github("klutometis/roxygen")



## Warum funktioniert die Installation von Packages nicht bei tar.gz?!
# *********************************************************
# Es wird Rtools gebraucht!
# Das kann man auf den Bundesrechnern selbst installieren (es braucht keine Administrator-Rechte).

# ODER
# Man installiert das Packeg devtools
#install.packages('P:/Software/R/packages/devtools_1.6.1.zip', repos = NULL) # '', nicht ""!
install.packages("devtools")
library(devtools)
# Dies ist fuer R 3.0.1 aber nicht verfuegbar!

# ODER
# It was the Panda antivirus installed in the main corporate server of my company.
# This antivirus program was corrupting the zip files containing the packages for Windows systems.
# The problem was solved by including CRAN repositories as trustable webpages.

# Fuer Rtools muss man die PATHS des Systems richtig setzen.
pathRtools <- paste(c("c:\\Rtools\\bin",
                      "c:\\Rtools\\gcc-4.6.3\\bin"
                      #"C:\\Program Files\\R\\R-3.0.1\\bin\\i386",
                      #"C:\\windows",
                      #"C:\\windows\\system32"
                      ), collapse=";")

Sys.setenv(PATH=paste(pathRtools,Sys.getenv("PATH"),sep=";"))
Sys.getenv()["PATH"]

# Fuer zip-files repos=NULL setzten. type nicht setzen!
install.packages('P:/Software/R/packages/zlib128.zip', repos = NULL) # '', nicht ""!
# Fuer tar.gz files repos=NULL, type='source' setzten!
install.packages("P:/Software/R/packages/bzip2-1.0.6.tar.gz", repos=NULL, type='source') # '', nicht ""!
install.packages("P:/Software/R/packages/zlib-1.2.8.tar.gz", repos=NULL, type='source') # '', nicht ""!


install.packages('http://www.bzip.org/1.0.6/bzip2-1.0.6.tar.gz', repos=NULL, type='source') # '', nicht ""!
install.packages("bzip2", repos='http://www.bzip.org/1.0.6/', type='source') # '', nicht ""!

install.packages("Rcompression", repos = "http://www.omegahat.org/R", type="source")



ls(pos = "package:packagename") # Alle Funktionen eines Pakets anschauen
lsp(packagename) # oder diese Funktion nutzen

# Ist ein Paket geladen resp. attached?
package.attached <- function(name) length(grep(paste0("package:",name), search())) > 0
package.attached("base")
# Konditionales detaching eines Paketes   unload
if(package.attached("Benchmarking")) detach("package:Benchmarking")


################## Zeilen / Spaltenauswahl ##################

### Zeilenauswahl

#Entfernen gewisser Zeilen
aaa <- aaa[-c(453,723),]
ausreisser <- c("218","528","1287","7","721","1945","1984","1429", "1700", "1964", "1028", "315")
aaa <- aaa[!rownames(aaa)%in%ausreisser,]
#Entfernen von NA Zeilen (aber nur mit Kondition in gewissen Spalten)
DF <- DF[!(is.na(rowSums(DF[,(1:2)]))),] # Heisst: nimm nicht die Zeilen, in dessen Zeilen der ersten 2 Spalten NA-Werte sind.

# Spalten-/Zeilenname fuer Spalte bekommen, in der das kleinste/groesste Element der Matrix ist
inds = which(mat == max(mat), arr.ind=TRUE)
  inds <- which.max(mat,arr.din=TRUE)
  inds <- which.min(mat,arr.din=TRUE)
rnames = rownames(mat)[inds[,1]]
cnames = colnames(mat)[inds[,2]]

#Eine Kondition:
aaa <- aaa[aaa[,'DREL_LN']>2,]
aaa <- subset(aaa, Jahr==2004)
#Mehrere Konditionen
aaa <- aaa[aaa[,'DREL_LN']>2&aaa[,'DREL_LN']<3&..&..&..,]

# Zufaellige Zeilenauswahl treffen
zufall <- data[sample(nrow(data), 2), ] # mit grÃ¶sse 2

# Alle Zeilen auswaehlen, die auch in einem andren Datensatz vorhanden sind
x %in% y
all(x %in% y) # Ueberpruefen, ob x ein Subset von y ist.

### Spaltenauswahl

spaltenauswahl <- c("DABS.LN","DABS.Aktiven")
  aaa <- aaa[,spaltenauswahl]
entfernen <- c("kgMilch_jeKuh", "DABS_Gebiet", "DABS_HuM")
  aaa <- aaa[,!colnames(aaa)%in%entfernen]

#Entfernen leerer Spalten; Entfernen leerer Zeilen
aaa <- aaa[,!is.na(colSums(aaa))]
aaa <- aaa[,!apply(aaa,2,function(x)all(is.na(x)))];   aaa<-aaa[!apply(aaa,1,function(x)all(is.na(x))),] # aaa <- matrix(nrow=6, ncol=4); aaa[2:5,2:3] <- seq(2)
  # Kombiniert entfernen leerer Spalten und Zeilen, sodass nur noch Rest Ã¼brig bleibt
  aaa[!apply(aaa,1,function(x)all(is.na(x))),!apply(aaa,2,function(x)all(is.na(x)))]
#Entfernen von duplizierten Spalten
testframe <- testframe[,!duplicated(lapply(testframe, summary))]


################## Erstellen ##################

### Zufallszahlen
rnorm(n, mean = 0, sd = 1)   # n=Anzahl Zufallszahlen
runif(n, min=0, max=1)

### Liste / Vektor
#Vektor
rep(0,50) # Vektor mit 50 Nullen erstellen
numeric() # leeren numeric Vektor erstellen
# Character Vektor (nicht CHARACTERS)
letters[1]     # Buchstaben sind in alphabetischer Reihenfolge schon vorgegeben
LETTERS[1]

# Farbvektor
colors()
vec <- c(1:10); plot(vec, col=colors()[vec], pch=20, cex=2)
rainbow(10)
vec <- c(1:10); plot(vec, col=rainbow(length(unique(vec)))[vec], pch=20, cex=2)

#Liste
vector("list", 20) # Leere Liste mit 20 Plaetzen machen.
list() # leere Liste erstellen

### Matrix
mat <- matrix(nrow=2, ncol= 5); # Leere Matrix
mat <- matrix(1:10, nrow=2, ncol= 5); # Geht auch nur mit einer Zahl als Fuellung. Bsp. 0.

# Wichtige Bemerkung:
# Man muss die l?nge eines Vectors oder einer Liste nicht vorgeben, wenn man z.B. einen Loop Output rein machen will einfach:
loopoutput <- list(); for(i in 1:5) loopoutput[[i]] <- i
loopoutput <- numeric(); for(i in 1:5) loopoutput[i] <- i

################## Umformungen ##################

### Objektformatierungen
# Aus einer Liste einen Vektor machen, die alle Bestandteile der Liste ununterteilt enthaelt.
unlist(liste)   # Try with liste <- list(c(1,2), c(3,4))
# Aus Vektor Charakter String machen
paste(x, collapse = "")

# Listennamen geben
names(list)[i] <- ""
  # Spalte aus Matrix abrufbar machen mit $$$. --> Geht nur fuer Listenaehnliche Objekte. Deshalb matrix zu data.frame umformen.
  # Listenelement auslesen mit Liste$Element oder Liste[[Element]], falls der Name Zahlen oder Sonderzeichen beinhaltet.
# Reihen neu benennen (statt urspruenglicher Reihennamen)
rownames(aaa) <- 1:nrow(aaa);   colnames(aaa) <- ...
rownames(aaa)[i] <- "rowname"


### Objektinhalte

# Vektor Ordnen
sort(a)
a[order(a)]
a[order(-a)] # wenn mans absteigend will
# Dataframe ordnen
dd <- data.frame(b = factor(c("Hi", "Med", "Hi", "Low"), levels = c("Low", "Med", "Hi"), ordered = TRUE), x = c("A", "D", "A", "C"), y = c(8, 3, 9, 9), z = c(1, 1, 1, 2))
dd[with(dd, order(-z, b)), ]
#Matrix ordnen
M <- matrix(c(1,2,2,2,3,6,4,5), 4, 2, byrow=FALSE, dimnames=list(NULL, c("a","b"))) #to create a matrix M, then use
M <- M[order(M[,"a"],-M[,"b"]),] #to order aufsteigend nach a und absteigend nach b
# Zahlenfolge neu benennen
as.numeric(as.factor(x)) # x <- c(1,1,1,1,1,4,4,4,4,7,7,1,1,1,1) --> 1 1 1 1 1 2 2 2 2 3 3 1 1 1
# andere MÃ¶glichkeit
clusternummer.neu <- matrix(c(1,3,4,6,7,1:5), nrow=5, ncol=2)
for(i in 1:nrow(aaa.alle.neuklassiert))
  aaa.alle.neuklassiert$Clusternummer[i] <- clusternummer.neu[which(clusternummer.neu[,1] == aaa.alle.neuklassiert$Clusternummer[i]),2]
# NA mit 0 fuellen
data[is.na(data)] <- 0
# bestimmte Zeichen durch andere ersetzen
sub(); gsub()
# Matrizen zusammenfuegen
rbind(), cbind()
cbind(x, Rtot = y) # Gibt der zugefuegten Spalte y den Namen "Rtot"

# Matching, Attention below
# 2 IDs matchen:
# id1 an id2 anfuegen.
id1 <- c(1,2,3,4,8,8,8,8); id1
id2 <- c(4,5,6,7,8,1,rep(9,3),rep(7,3), rep(NA,3) ); id2
match.1in2 <- match(id1, id2)
add_to_2 <- rep(NA, length(id2))
add_to_2[ match.1in2[!is.na(match.1in2)] ] <- id1[!is.na(match.1in2)] # alternativ statt a einen anderen Wert einfuegen
cbind(id2,add_to_2)

# Attention: If there are no duplicated rows in one data but in the other, your first have to delete the duplicated ones.
id <- id[!duplicated(id[,"IDneu"]),]
# Matching simple
c <- a %in% b
# Partial character matching
a <- c("Arb","Kap","Vorl","Fla"); b <- c("Arbeit","Kapital","Vorleistungen","Flaeche")
pmatch(a,b)
# --> Kann zum Beispiel in Funktionen verwendet werden, wenn f?r eine Methode nur der Anfangsbuchstabe verlangt wird

#### Rechenregeln Vektoren Matrizen ####

#Vektorprodukt bzw. cross product. Wenn dieses = 0 ist, sind zwei Vektoren Kollinear, d.h. wenn man sie richtig verschiebt, liegen sie genau aufeinander.
a <- c(1,2,3,4); b <- c(2,4,6,8)
pracma::cross(a,b)

################## Data Manipulation ##################

# Base R apply functions (from a presentation given by Hadley):

# ***********************************************************
# Output >   |  array        data frame    list        nothing
#............................................................
# Input \/    |**********************************************
# array       |  apply()      .             .             .
# data frame  |  .            aggregate()   by()          .
# list        |  sapply()     .             lapply()      .
# n replic.   |  replicate()  .             replicate()   .
# func. arg.  |  mapply()     .             mapply()      .

# ave() also seems to be interesting!

# rbind all places of a list.
do.call("rbind", list(c(1,2,3),c(2,3,4))) # -> superfast version implemented by data.table::rbindlist()

# mapply - Vektor apply over multiple lists.
xList <- list(matrix(1:10,ncol=2), matrix(1:3 , ncol=1))
yList <- list(matrix(1:3 ,ncol=1), matrix(1:10, ncol=2))
mapply(function(x,y)x*min(y),  x=xList,  y=yList)


################## Data / Import / Export  ##################

### Data
# Show the size of an object
format( object.size(obj), units="MB")

#Encoding - Scripting:
# Always use the same format: ISO-8859-1
# Do not open with UTF-8 in the one programm and ISO in the other. This produces ? instead of öäü.

# Alle Datasets aus dem Speicher entfernen
rm(list=ls())
# Bestimmte Daten behalten mit der keep function
library(gdata); rm(list=keep(object1,object2,object3,..)) #  #help(keep)
# Testen, ob ein Objekt exisitert
exists("nameOfObject")

# Alle Dateien in einem Ordner finden
list.files("P:/")
# Die einen bestimmten Textteil "_" enthalten
list.files("P:/", "_")

### Export
# Tabelle schreiben
  # nicht write.csv!
write.table(export, file=paste0("pfad.csv"), sep = ";", eol = "\n", quote=FALSE, col.names=TRUE, row.names=FALSE) # Nur COLnames
write.table(export, file=paste0("pfad.csv"), sep = ";", eol = "\n", quote=FALSE, col.names=FALSE, row.names=TRUE) # Nur ROWnames
write.table(export, file=paste0("pfad.csv"), sep = ";", eol = "\n", quote=FALSE, col.names=NA) # Colnames & Rownames
write.table(export, file=paste0("pfad.csv"), sep = ";", eol = "\n", quote=FALSE, col.names=FALSE, row.names=FALSE) # KEINE Colnames oder Rownames

# Image schreiben
save.image("pfad.RData"); load("...")
save(object1, object2, file="pfad.RData"); load("...")

saveRDS(object, file="dataFile.rds")
object <- readRDS("dataFile.rds")

### Import
# CSV einlesen
 # nicht read.csv!
read.table(paste0("pfad.csv"), sep=";", header=TRUE, stringsAsFactors=FALSE, quote = "\"", comment.char="", na.strings=c("","NA","na","NULL","null","#DIV/0","#DIV/0!","#WERT","#WERT!"))
 # zip Archive
read.table(unz(paste0(pfad, "filename.zip"), "nameOfCSVFile.csv"), sep=";", header=TRUE, stringsAsFactors=FALSE)

# 1. Spalte beinhaltet Rownames. Von 1. Spalte in Rownames transferieren.
dat <- read.table(paste0("pfad.csv"), sep=";", header=TRUE, stringsAsFactors=FALSE, quote = "\"", na.strings=c("","NA","na","NULL","null","#DIV/0","#DIV/0!","#WERT","#WERT!")); rownames(dat) <- dat[,1]; dat <- dat[,2:ncol(dat),drop=FALSE]
own.datname <- dat; rm(dat)
# 1. Reihe beinhaltet Colnames. Von 1. Reihe in colnames transferieren
#dat <- read.csv(paste0("pfad.csv"), sep=";", header=FALSE, stringsAsFactors=FALSE, quote = "\"", na.strings=c("","NA","na","NULL","null","#DIV/0","#DIV/0!","#WERT","#WERT!")); colnames(dat) <- dat[1,]; dat <- dat[2:nrow(dat),]
#own.datname <- dat; rm(dat)

# Excel xls oder xlsx Formate einlesen
# http://www.r-bloggers.com/read-excel-files-from-r/
# gdata (kein Support fuer xlsx)
source("https://gist.github.com/schaunwheeler/5825002/raw/3526a15b032c06392740e20b6c9a179add2cee49/xlsxToR.r")
dat = xlsxToR("myfile.xlsx", header = TRUE)

# Daten bereits beim Einlesen filtern
require(sqldf)
df <- read.csv.sql("sample.csv", "select * from file where age=23")



# Image einlesen
load("P:/_ZA/Statistik/Data/Grundlagenbericht/GB.RData")

# XLS-sheets einlesen - KEIN XLSX, daf?r wird Perl ben?tigt!
name c("14_Mai", "14_Juni")
dat <- list()
# Geht mit Sheet-Nummer oder Sheet name:
for(i in 1:length(name)) dat[[      i  ]] <- gdata::read.xls( xls=paste0(pfad,"ASDF.xls"), sheet=     i , stringsAsFactors=FALSE, header=TRUE, na.strings=c("NA","","#DIV/0","#DIV/0!","#WERT!"), quote='') # '' um Konversionsprobleme zu vermeiden.
for(i in 1:length(name)) dat[[ name[i] ]] <- gdata::read.xls( xls=paste0(pfad,"ASDF.xls"), sheet=name[i], stringsAsFactors=FALSE, header=TRUE, na.strings=c("NA","","#DIV/0","#DIV/0!","#WERT!"), quote='') # '' um Konversionsprobleme zu vermeiden.

#library(gdata); library(xlsReadWrite) # xls gibt es nicht mehr. Ist nun in gdata integriert.
#dat <- list()
#for(i in 1:...) dat[[i]] <- read.xls(sheet=i, paste0(laufwerk,"pfad.xls"), colNames=TRUE, naStrings=c("NA","","#DIV/0","#DIV/0!","#WERT!"))


################## Funktionen ##################
### Funktionen speichern
#Entweder in ein Textfile einfuegen un dann vor Funktionsaufruf Funktion mit source(Textfile-Pfad) einlesen
#Oder Funktionen in C:\Programme\R\R-2.15.0\etc\Rprofile.SITE reinschreiben. Dann werden sie jedes mal bei R Start geladen.

### STATISTIKFUNKTIONEN in R

# Regressionssyntax
fit <- lm(y~. ,data=...) # Schliesst alle Variablen ausser y in die independets ein
fit <- lm(y~. -x3 ,data=...) # Schliesst x3 aus den independet variablen aus.
fit <- lm(y ~ x -1 ,data=...) # Regression OHNE INTERCEPT
# Polynomiale Regression
fit <- lm(y~ x + I(x^2) + I(x^3),data=..)
  coef(fit) # statt fit$coefficients
  residuals(fit)
  fitted(fit)
# Robuste Regression
fit <- rlm(y~x,data=..)
# step-Verfahren zum verwerfen von nicht-signifikanten Independent Variables
fit <- lm(y^x,data=..)
step(fit)

# Regressionsmodelle Vergleichen mit F-Test
n <- 100
x <- 1:n
y1 <- x+rnorm(n,0,5)
y2 <- x+rnorm(n,0,10)
par(mfrow=c(1,2))
plot(x,y1); abline(0,1); plot(x,y2); abline(0,1)

mod1 <- lm(x~y1)
mod2 <- lm(x~y2)
var.test(mod1, mod2) # Varianz von Modell 1 ist kleiner, also besser!
var.test(mod2, mod1) # Varianz von Modell 2 ist gr?sser, also schlechter!

# nonparametric test of equal variance with the wilcoxon rank sum test
https://stat.ethz.ch/pipermail/r-help/2007-March/126831.html

### Funktionssyntax

# Aufbau einer Funktion

foo <- function(x, z=NULL, method=c("a","b","c"), ...) { # , ... to pass arguments to an embedded function
                                    # In der Klammer stehen beliebig viele weitere Argumente
  method <- match.arg(method)       # Ist die eingegebene Methode eine von c("a","b","c")? Falls nicht angegeben, wird "a" gew?hlt
  if(is.null(z)) stop("specify z")  # Eine Anfangskondition muss erf?llt sein
  if(sum(x)<1  ) warning("be careful...", call. = FALSE, immediate.=TRUE) # Such that the warning appears immediately.
  #
  s <- summary(x, ...)              # Ausf?hren der Berechnungen...
  m <- mean(x, ...)                 # ...
  #
  if(m<1) {                         # IF ELSE
    print(1)                        # Die Klammern und Zeilen muessen genau so sein. Sonst funktioniert es nicht!
  } else if (m==1) {
    print(2)
  } else { print("x must be smaller equal 3") }
  #
  result <- list(summary=s,mean=m)  # Resultate als Liste zusammenf?hren
  class(result) <- "foo"            # class = ... setzen fuer spaeteren "print.class"
  return(result)
}
# Hinweis: Mit der Einstellung
options(warn=2)
# Kann man Warnungen in Fehler umwandeln, sodass bspw. ein Loop abgebrochen wird.

# Generische Funktionen
# Print is a generic function which means that new printing methods can be easily added for new classes.
print.foo <- function(object){      # Der default print des Funktionsoutputs beinhaltet nur die summary.
  a <- list()
  a$summary <- object$summary
  print(a)
  invisible(a)                      # Der print output wird auch unsichbar ausgegeben (also nicht erneut geprinted) damit er einer Variable zugewiesen werden kann.
  # oder
  class(object) <- "list"
  object$mean <- NULL
  print(object)
  invisible(object)
}

foo(x=c(1,2,3,4,5,6,7,8,9,10,11,12,100),z="a", na.rm=TRUE, trim=0.4, digits=10) # Test

# Sehen, wie lange (welche Zeit) eine Funktion gebraucht hat, um ausgef?hrt zu werden. Performance Test f?r Funktionen
system.time(for(i in 1:100) foo(x=c(1,2,3,4,5,6,7,8,9,10,11,12,100),z="a", na.rm=TRUE, trim=0.4, digits=10))
# oder
ptm <- proc.time()
for(i in 1:100) foo(x=c(1,2,3,4,5,6,7,8,9,10,11,12,100),z="a", na.rm=TRUE, trim=0.4, digits=10)
proc.time() - ptm
# oder
library(rbenchmark)
x <- seq(1.0, 1.0e6); benchmark(sqrt(x), log(x))

# Schauen, warum ein Teil einer Funktion nicht funktioniert, Debugging
# http://www.stats.uwo.ca/faculty/murdoch/software/debuggingR/debug.shtml
# The   browser()   function in R's base package allows you to single step through the execution of an R function. You can view and change objects during execution. There is support for setting conditional breakpoints.
# The   debug()     function marks a function for debugging, so that browser() will be called on entry. Use ?debug in R to see the man page for debug().
debug(mean.default)
mean(1:10)
Q # to exit debugging!
undebug(fun)

fun <- function(){
  a <- 1
  browser() # Browser is like a break point
  b <- 2
  return(b)
}
fun()

# fix(fun)
# The   trace()     function modifies a function to allow debug code to be temporarily inserted. Use ?trace in R to see the man page for trace().
# The findLineNum() and setBreakpoint() functions in the utils package work with line number references to compute locations for calls to trace().
# Further details are given in the R Language manual in the Debugging chapter, and in Roger Peng's "An Introduction to the Interactive Debugging Tools in R".


# Outputstruktur einer Funktion anschauen
str(functionoutput)
str(summary(functionoutput)) # ist auch moeglich.



# Rekursive Funktionsdefinitionen sind auch moeglich. Wie z.B. zum Berechnen der Faktoriellen.
factorial <- function(x) {
  x <- abs(x)
  if(x==0) {
    return(1)
  } else {
    return(x*factorial(x-1))
  }
}

# Sonstiges

# Beliebige, auszufuehrende Funktion als Argument in andere Funktion eingeben.
# http://onertipaday.blogspot.de/2007/05/how-can-i-turn-string-into-variable.html
funname <- "mean"
f <- get(funname, mode="function") # OR: f <- match.fun(funname)
arglist <- c(1,2,3,4,5)
f(arglist)
funcall <- call(funname, arglist);funcall
eval(funcall)

# Variierende Variablennamen in der Funktion kreieren und abrufen
for (i in 1:5) assign( paste("Variablenname",i,sep=""), funktionswert)
outputmatrix <- rbind( get(paste("Variablenname",i-1,sep="")), get(paste("Variablenname",i,sep="")))
# f?r mehrere Matrizen in einem Loop dieselben Spalten neu definieren.
for(object in c("aaa","bbb","eee")){
  assign(object,  within( get(object), {
    DZoek_p_Umsatz   <- DZoek / (Landw+ DZoek+ DZrest+ Paralandw)
    DZoek_p_Umsatz2  <- DZoek2 / (Landw2+ DZoek2+ DZrest2+ Paralandw2)
    # etc.
  })
  )
}

# Geht fuer Funktionen auch mit eval(call())
eval(call((paste("Variablenname",i,sep="")))) # do.call(paste()) macht dasselbe
x <- 42
eval(parse(text = "x"))
x <- 42 #other way round
deparse(substitute(x))
# Ein Objekt als character string aufrufen
a <- 1:10
eval(parse(text="a"))
# geht auch komplizierter
a <- matrix(1:100,nrow=1)
x <- y <- 1
eval(parse(text=paste("a[", x,",",y,"]", sep = "")))
################## R 5 Reference Classes##################

r5ReferenceClass <- setRefClass(
  Class="className",
  
  fields=list(primitiveValue="numeric", objectOfClass="lm"),
  
  methods=list(
    # Methods to override.
    # Initialization of the class.
    initialize = function(primitiveValue, objectOfClass) {
      .self$primitiveValue <- primitiveValue
      .self$objectOfClass <- objectOfClass
    }
    # print() method.
    ,show = function() {
      print(primitiveValue)
    }
    # Own methods.
    ,getModel = function() return(primitiveValue)
  )
)
# R5 Class initialisieren
instance  <- r5ReferenceClass$new(primitiveValue=5, objectOfClass=lm(c(1,2)~c(1,3)))

# Test eines XML Parsers.
# xmlText <- c("<xmlRoot><Account>Lebensversicherung - Säule 3b (A)</Account>\n<Account>Lebensversicherung - Säule 3b (A)</Account>","<Account>Lebensversicherung - Säule 3b (A)</Account><Account_DE>Lebensversicherung - Säule 3b (A)</Account_DE><created>07.06.2017 10:48:51</created>", "<Account>Lebensversicherung - Säule 3b (A)</Account><Account_DE>Lebensversicherung - Säule 3b (A)</Account_DE></xmlRoot>")
xmlParser <- setRefClass(
  Class="xmlParser",
  
  fields=list(xmlText="character"),
  
  methods=list(
    # Methods to override.
    # Initialization of the class.
    initialize = function(xmlText) {
      xml <- strsplit(xmlText, ">[ \n\r]*<")
      xml <- unlist(lapply(xml, function(x){
        ind <- 2:length(x)
        x[ind] <- paste0("<",x[ind])
        ind <- 1:(length(x)-1)
        x[ind] <- paste0(x[ind],">")
        return(x)
      }))
      .self$xmlText <- xml
    }
    # print() method.
    ,show = function() {
      print(xmlText)
    }
    # Own methods.
    ,getAllElements = function(elementName){
      
      
    }
  )
)

################## Write code in other programming languages ##################
# Example to count number of lines in a file
# http://stackoverflow.com/questions/23456170/get-the-number-of-lines-in-a-text-file-using-r

install.packages("inline")
library(inline)

rtools <- "C:\\Rtools\\bin"
gcc <- "C:\\Rtools\\gcc-4.6.3\\bin"
path <- strsplit(Sys.getenv("PATH"), ";")[[1]]
new_path <- c(rtools, gcc, path)
new_path <- new_path[!duplicated(tolower(new_path))]
Sys.setenv(PATH = paste(new_path, collapse = ";"))

wc.code <- "
uintmax_t linect = 0;
uintmax_t tlinect = 0;

int fd, len;
u_char *p;

struct statfs fsb;

static off_t buf_size = SMALL_BUF_SIZE;
static u_char small_buf[SMALL_BUF_SIZE];
static u_char *buf = small_buf;

PROTECT(f = AS_CHARACTER(f));

if ((fd = open(CHAR(STRING_ELT(f, 0)), O_RDONLY, 0)) >= 0) {

if (fstatfs(fd, &fsb)) {
fsb.f_iosize = SMALL_BUF_SIZE;
}

if (fsb.f_iosize != buf_size) {
if (buf != small_buf) {
free(buf);
}
if (fsb.f_iosize == SMALL_BUF_SIZE || !(buf = malloc(fsb.f_iosize))) {
buf = small_buf;
buf_size = SMALL_BUF_SIZE;
} else {
buf_size = fsb.f_iosize;
}
}

while ((len = read(fd, buf, buf_size))) {

if (len == -1) {
(void)close(fd);
break;
}

for (p = buf; len--; ++p)
if (*p == '\\n')
++linect;
}

tlinect += linect;

(void)close(fd);

}
SEXP result;
PROTECT(result = NEW_INTEGER(1));
INTEGER(result)[0] = tlinect;
UNPROTECT(2);
return(result);
";

setCMethod("wc",
           signature(f="character"),
           wc.code,
           includes=c("#include <stdlib.h>",
                      "#include <stdio.h>",
                      "#include <sys/param.h>",
                      "#include <sys/mount.h>",
                      "#include <sys/stat.h>",
                      "#include <ctype.h>",
                      "#include <err.h>",
                      "#include <errno.h>",
                      "#include <fcntl.h>",
                      "#include <locale.h>",
                      "#include <stdint.h>",
                      "#include <string.h>",
                      "#include <unistd.h>",
                      "#include <wchar.h>",
                      "#include <wctype.h>",
                      "#define SMALL_BUF_SIZE (1024 * 8)"),
           language="C",
           convention=".Call")

wc("FULLPATHTOFILE")

################## Grafik ##################

# Gesch?tze Kurve in Scatterplot einzeichnen
scatter.smooth(1:100,rnorm(100,0,30)+1:100)

# Gesch?tzte Kurve in Pairsplot einzeichnen
data(iris)
pairs(~Sepal.Length+Sepal.Width+Petal.Length+Petal.Width, data=iris,
      lower.panel=panel.smooth, upper.panel=panel.smooth, pch=20, main="Iris Scatterplot Matrix")

# dasselbe mit Histogramm in der Mitte (in den eigenen Funktionen als pairs.smooth umgesetzt.)
data(iris)
panel.hist <- function(x, ...) {
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col="gray")
}
pairs(~Sepal.Length+Sepal.Width+Petal.Length+Petal.Width, data=iris,
      lower.panel=panel.smooth, upper.panel=panel.smooth, diag.panel=panel.hist, pch=20, main="Iris Scatterplot Matrix")

# Kurvenverlauf zeichnen # http://stackoverflow.com/questions/26091323/how-to-plot-a-function-curve-in-r
curve(expr=-2+x*2-x^2,from=-10,to=10)
# Kurvernverlauf von Weizenduengung zeichnen. Ertrag (dt/ha) in Abh?ngigkeit von N pro ha. Bei 200mm Niederschlag und 400m Irrigation pro m^2.
curveChar <- function(expr, ...) curve((function(x) eval(parse(text=expr)))(x), ...)
expr <- "100*(1-exp(-0.025*(40+x)))*(1-exp(-0.0045*(200+400)))"
curveChar(expr, from=1, to=160, main=expression(Yield[max] * (1-e^(-alpha*(N[0]+N))) * (1-e^(-beta*(W[0]+W))) ))

# pdf/png Graph ausgeben
pdf("D:/temp/graph4.pdf")
boxplot(write)
dev.off()

# Window in mehrere Plots unterteilen
par(mfrow=c(5,4))
par( mar=c(c(4.5, 4, 3, 2) + 0.1) ) # mar=c(5, 4, 4, 2) + 0.1  default
layout(layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE))) # so kann man einen grossen und zwei kleine Plots machen.

# Mehrere Plots in den gleichen Plot
plot(..., add=TRUE)
# & 2. Achsenbeschriftung (Y-Achse) rechts
par(mar=c(5, 4, 4, 4) + 0.1);
plot(x=rnorm(10),main="Entwicklung des ...",ylab="Arbeitstage",xlab="Jahr",type="b",pch=20);
par(new=TRUE); plot(rnorm(10),xaxt = "n",yaxt = "n",xlab="",ylab="",type="b",pch=20,cex=0.6,lty=2); axis(4); mtext("yaxis",side=4,line=2.5,cex=par()$cex)
# Histogramme in gleichen Plot (und Boxplot)
F1 <- rnorm(20,2); F2 <- rnorm(25,4);
par(mfrow=c(1,2));
boxplot(F1,F2,main="Boxplot", xlab="Groups", ylab="Efficiency");
dF1 <- density(F1);
dF2 <- density(F2);
plot(dF1,xlim=c(min(dF1$x,dF2$x),max(dF1$x,dF2$x)),ylim=c(min(dF1$y,dF2$y),max(dF1$y,dF2$y)),col="red",main="Dens. Plot",xlab="Efficiency");lines(dF2,col="darkgreen")

# Anzahl Tickmarks (Ticks) automatisch erhoehen.
plot(1:10, 1:10, lab=c(10,10,12))

#par(mar=c(5, 4, 4, 2.5) + 0.1, cex=1); plot(main="NE, FaAK & FrAK", xlab="Jahr", ylab="", x=years,y=verlne, type="l",lty=1, yaxt="n")
#axis(2,cex=par(cex=0.8)$cex); mtext("Veraenderung Arbeitstage",side=2,line=2.5); par(cex=1)
#par(new=T); plot(x=years, y=verlfr, type="l", lty=2, xaxt="n",yaxt="n",xlab="",ylab=""); axis(4,cex=par(cex=0.8)$cex); par(cex=1)
#par(new=T); plot(x=years, y=verlfa, type="b", lty=1,pch=20,cex=0.6, xaxt="n",yaxt="n",xlab="",ylab=""); xylim<-par("usr"); axis(2,pos=xylim[2],cex=par(cex=0.8)$cex); par(cex=1)
#par(new=T); plot(x=years, y=colMeans(pachtln[nep|nem|fap|fam|frp|frm,]), col="black", type="l", lty=3, xaxt="n",yaxt="n",xlab="",ylab=""); xylim<-par("usr"); axis(4,pos=xylim[1], col="black",cex=par(cex=0.8)$cex); par(cex=1)
#mtext(c("NE","LN (ha)","FaAK","FrAK"),at=c(xylim[1]-0.8, ceiling(xylim[1])+0.5, floor(xylim[2])-0.5, floor(xylim[2])+1),cex=0.8)
#legenxxxd(locator(1),legenxxxd=c("NE","LN","FrAK","FaAK"),lty=c(1,3,2,1), pch=c(NA,NA,NA,20),pt.cex=0.9, bty="n",cex=0.8)

# Spezielle Schriften verwenden
#   http://blog.revolutionanalytics.com/2012/09/how-to-use-your-favorite-fonts-in-r-charts.html
library(extrafont)
fonts()
fonttable()
plot(..., family"")

# Hoehe u Breite von window einstellen.
dev.new(width=30, height=18)  # Alternativ in pdf(width=.., height=..) einfuegen.

# Beschriftungen
  # Griechische Symbolde wie delta etc. in Graphenbeschriftung einfuegen (-> demo(plotmath) fuer Hilfesuche)
  # Mathematische Ausdr?cke Mathematical expressions in plots
  qqnorm(a<-rnorm(100),main=expression(paste(Delta, "FaAK[JAE]")), cex=1, pch=20);mtext("Normal Q-Q Plot", side=3, cex=0.5);qqline(a)
  mtext(expression(paste(Delta ~~  SAK[L], oder," ","SAK[L]")),side=4,line=1)
  # evt. muss man auch mit parse arbeiten:
  plot(1,1); pow <- 1.5
  title(parse(text=paste0("theta==1+alpha^",(pow))))
# Legende
  legend(locator(1),xjust=c(0,0.5,1)[2],legend=c("NE","FaAK","FrAK"),lty=c(1,2,1), pch=c(NA,NA,20),pt.cex=0.8, bty="n")
  Positionen: "bottomright", "bottom", "bottomleft", "left", "topleft", "top", "topright", "right", "center"
# Achsenbeschriftungen & Achsenposition
  # Eigenhaendig Achsenbeschriftung machen
  axis(2, at = c(1,2,3), labels = c("A","B","C"))
  # Scienfitic notation in allgemein & in Graphenbeschriftung verhindern
  options(scipen = 999) # vor dem Graphenbefehl eingeben
  options(scipen=0) # Wieder auf default setzten:
  # y axis on the left
  plot(1:5,axes=FALSE)
  axis(1); axis(2)
  xylim<-par("usr")                        # add a y axis one third of the way to the right
  axis(2,pos=xylim[1]+diff(xylim[1:2])/3, ,cex=par()$cex)
  axis(4,pos=xylim[2]-diff(xylim[1:2])/3)  # add another y axis two thirds of the way
  mtext()

# Scatterplot-Pairsplot
pairs(data)
scatterplotMatrix(data)
# oder ?hnlich: The diagonal shows the kernel density.
plotmatrix(with(iris, data.frame(Sepal.Length, Sepal.Width, Petal.Length, Petal.Width)))
plot(density(iris[,"Sepal.Length"]))


# Zusammengefasste Werte in ggplot2 plotten (z.B. resultat von mean.weights)
# Daten koennten in etwa so aussehn:
mw1 <- structure(c(70501.18728733, 79077.00623248, 55479.72870416, 59665.83888847,
            61760.4748543, 52834.84461067, 58902.64913118, 62835.58593476,
            51441.17820976, 68637.82798599, 78428.86472092, 57302.78946271,
            70605.16571968, 81331.37941756, 60441.80134763), .Dim = c(3L, 5L),
          .Dimnames = list(c("11_Ackb", "12_Spez", "21_VM"), c("2004", "2005", "2006", "2007", "2008")))

#print(mw1)
#            2004     2005     2006     2007     2008     2009     2010     2011     2012      2013     2014
#11_Ackb 70501.19 59665.84 58902.65 68637.83 70605.17 66307.57 58962.59 71257.07 60744.29  59815.13 64691.41
#12_Spez 79077.01 61760.47 62835.59 78428.86 81331.38 99246.96 79875.91 85920.25 69146.78  70727.35 82769.92
#21_VM   55479.73 52834.84 51441.18 57302.79 60441.80 55207.44 52847.33 57313.91 54619.06  58173.59 67058.73
library(ggplot2)
# Hinweis: wide.to.long.df ist function aus Vollkosten.
ggplot(data=wide.to.long.df(rownames(mw1), mw1), aes(x=index2,y=value,color=index1,linetype=index1,group=index1,size=1))) +
  geom_line() + geom_point() + scale_size(range=c(0.1, 2), guide=FALSE)


# Punkte in einem Scatterplot identifizieren
plot(x<-c(1,2,3,4,5), y<-c(1,2,3,4,5))
identify(x,y)
# Pairs bei dem die Punkte nach Gruppenzugehoerigkeit gefaerbt (und beschriftet) werden. Siehe help(pch) oder ... help(par) -> help(points) unter Punkt "pch" (=Pointscharacter)
# col=... wenn die Punkte gef?rbt werden sollen
# bg=... wenn der Inhalt der Punkte gef?rbt werden soll (geht nur bei manchen pch)
pairs(data[,columns], main="Pairs", pch=21, bg=c("red", "yellow", "blue")[grouping])
pairs(data[,columns], main="Pairs", pch=c(49:55)[grouping], col=c("red", "yellow", "blue", "green", "purple")[grouping]) # Ab 49 sind Zahlen.
#graphbeschriftung <- rep(0, max(aaa.alle.zugeteilt[,"Clusternummer"]));for (i in 1: length(graphbeschriftung)) graphbeschriftung[i] <- paste("Cluster",i,sep=" ")
  #library(lattice); windows(); splom(aaa.alle.zugeteilt[spaltenauswahl], groups=aaa.alle.zugeteilt$Clusternummer,  panel=panel.superpose, key=list(title="Geclusterte Betriebe", columns=max(aaa.alle.zugeteilt$Clusternummer), text=list(graphbeschriftung)))#points=list(pch=super.sym$pch[1:max(aaa.alle.zugeteilt$Clusternummer)], col=super.sym$col[1:max(aaa.alle.zugeteilt$Clusternummer)]),

# Farbplot mit nur einer Variable auf der X-Achse
colors <- c("red","orange","darkolivegreen4","dodgerblue2","darkviolet","violet","gray","gray32")
colors <- c("yellow", "cyan", "red", "green", "blue", "purple", "gray87", "yellowgreen", "steelblue1", "orchid1", "purple", "orange")
colvector <- character()
for (i in 1:length(colors)) colvector[grouping==i] <- colors[i]
windows(); par(mfrow = c(1,length(checkvariables)-1))
for (i in 2:length(checkvariables)) {
  plot(data[,checkvariables[1]], data[,checkvariables[i]], type="n", xlab=checkvariables[1], ylab=checkvariables[i])
  text(data[,checkvariables[1]], data[,checkvariables[i]], labels=as.character(grouping), col=colvector)
}

# QQ-Plot for alle benutzten Variablen
require(mvoutlier); windows(); par(mfrow = c(1,length(spaltenauswahl))); for (i in 1: length(spaltenauswahl)) {qqnorm(aaa[,spaltenauswahl[i]]); qqline(aaa[,spaltenauswahl[i]])}
# Multivariater QQ-Plot
x <- as.matrix(aaa.mat);      center <- colMeans(x);   n <- nrow(x);p <- ncol(x);cov <- cov(x);     d <- mahalanobis(x,center,cov)    ;    windows(); qqplot(qchisq(ppoints(n),df=p),d,main="QQ Plot Assessing Multivariate Normality",ylab="Mahalanobis D2") ; abline(a=0,b=1)

# Histogramme und Density Plots
hist(a<-rnorm(100000), xlim = c(-5, 5), main = "", xlab = "Normal Distribution",col = "gray", cex.lab = 1.25, freq=F, breaks=20)
lines(density(a,from=-5,to=5),lwd=2)

# Ueberlappende Histogramme
for (i in 1:20) hist(rnorm(100),main="",cex.axis=.8)
p1 <- hist(rnorm(500,4), plot=FALSE)                     # centered at 4
p2 <- hist(rnorm(500,6), plot=FALSE)                     # centered at 6
windows()
plot( p1, col=rgb(0,0,1,1/4), xlim=c(0,10))  # first histogram
plot( p2, col=rgb(1,0,0,1/4), xlim=c(0,10), add=T)  # second. The last number in rgb() makes the color transparent.

#Profilplot, welcher zeigt, wie hoch die Werte sind und wie sie Schwanken
makeProfilePlot <- function(mylist,names) {
  require(RColorBrewer)
  numvariables <- length(mylist)
  colours <- brewer.pal(numvariables,"Set1") # choose 'numvariables' random colours
  mymin <- 1e+20                             # find out the minimum and maximum values of the variables
  mymax <- 1e-20
  for (i in 1:numvariables)
  {
    vectori <- mylist[[i]]
    mini <- min(vectori)
    maxi <- max(vectori)
    if (mini < mymin) { mymin <- mini }
    if (maxi > mymax) { mymax <- maxi }
  }
  for (i in 1:numvariables)                  # plot the variables
  {
    vectori <- mylist[[i]]
    namei <- names[i]
    colouri <- colours[i]
    if (i == 1) { plot(vectori,col=colouri,type="l",ylim=c(mymin,mymax)) }
    else         { points(vectori, col=colouri,type="l")                                     }
    lastxval <- length(vectori)
    lastyval <- vectori[length(vectori)]
    text((lastxval-10),(lastyval),namei,col="black",cex=0.6)
  }
}

# pdf/png Graph ausgeben
pdf("D:/temp/graph4.pdf")
boxplot(write)
dev.off()

### ggplot2

# Plotting a boxplot against multiple factors in R with ggplot2 - Suche diesen Text in Google.
library(ggplot2); library(gridExtra)
angle=90
ycoords <- c(0.29, 1.01)
breakstep=0.1
crs1 <- ggplot(data=dea.efftable.sep, mapping=aes(x=interaction(grouping,year,sep=":"), y=TEcrs, col=grouping)) +
  geom_boxplot() +
  # F?r Boxplots m?ssen die AChsen mit coord_cartesian eingeschr?nkt werden, sonst ver?ndern sich die Boxplots!
  # F?r andere Plots kann man mit scale_x_continous arbeiten!
  coord_cartesian(ylim = ycoords) +
  scale_y_continuous(breaks=seq(min(ycoords)+0.01,max(ycoords)-0.01,breakstep)) +
  ylab("Technical Efficiency CRS") +
  scale_x_discrete("Country:Year") +
  labs(title = "Productivity of German\n& Swiss Organic Farms\nunder their respective frontiers") +
  theme(axis.text.x = element_text(angle=angle))
crs2 <- crs3 <- crs1
sidebysideplot <- grid.arrange(crs1, crs2, crs3, ncol=3)

# Stack Overflow: The right way to plot multiple y values as separate lines with ggplot2
library(ggplot2)
mtcars.long <- reshape2::melt(mtcars, id = "mpg", measure = c("disp", "hp", "wt"))
head(mtcars.long)
ggplot(mtcars.long, aes(mpg, value, colour = variable)) + geom_line() # Schauen, welche Namen die Spalten des Long-Dataframes haben.
# Selbst die Farben bestimmen:
ggplot(mtcars.long, aes(mpg, value, colour = variable)) + geom_line() + scale_colour_manual(values = c("red","darkolivegreen4","Blue","gray","orange","DeepSkyBlue","darkviolet","violet","gray32", "black"))
# Hintergrundfarbe ?ndern
ggplot(mtcars.long, aes(mpg, value, colour = variable)) + geom_line() + theme_bw()
# X-Achsenbereich w?hlen
ggplot(mtcars.long, aes(mpg, value, colour = variable)) + geom_line() + scale_x_continuous(breaks = seq(10,35,5), labels=seq(10,35,5))
# Beschriftungen ?ndern
#  Winkel der X-Achsenbeschriftung w?hlen
ggplot(mtcars.long, aes(mpg, value, colour = variable)) + geom_line() + theme(axis.text.x = element_text(angle=45))
#  Titel hinzuf?gen, Schrift fett, Zeilenabstand ?ndern
ggplot(mtcars.long, aes(mpg, value, colour = variable)) + geom_line() + ggtitle("Cars") + theme(plot.title = element_text(lineheight=1, face="bold"))
# Alles zusammen
colors <- c("red","darkolivegreen4","Blue","gray","orange","DeepSkyBlue","darkviolet","violet","gray32", "black")
breaks.x <- seq(10,35,5); labels.x <- seq(10,35,5)
angle.x <- 45
ggplot(mtcars.long, aes(mpg, value, colour = variable)) + geom_line()  +  scale_colour_manual(values = colors)  +  theme_bw()  +  scale_x_continuous(breaks=breaks.x, labels=labels.x)  +  theme(axis.text.x = element_text(angle=angle.x))


# Kumulative Wahrscheinlichkeitsverteilung macht man mit qplot(stat="ecdf", geom="step")
qplot(cost[i,], stat = "ecdf", geom = "step") +
  scale_x_continuous(breaks = round(seq(min(cost[i,]), max(cost[i,]), length.out=n.grids.x),-2)) +
  scale_y_continuous(breaks = round(seq(0, 1, length.out=n.grids.y),2))+
  ylab("Wahrscheinlichkeit") +
  xlab("Kosten (CHF)") +
  labs(title = plot.names[i])


# Monte-Carlo Bootstrapping of sample distribution
# Quelle: https://www.youtube.com/watch?v=UZkaZhsOfT4&feature=youtu.be
data(faithful)
fit <- density(faithful$eruptions)
plot(fit)

xx <- faithful$eruptions
fit1 <- density(xx)
fit2 <- replicate(1000, {
  x <- sample(xx, replace=TRUE)
  density(x, from=min(fit1$x), to=max(fit1$x))$y
})
fit3 <- apply(fit2, 1, function(x)quantile(x, c(0.025,0.975)))
plot(fit1, ylim=range(fit3))
polygon(c(fit1$x, rev(fit1$x)),
        c(fit3[1,], rev(fit3[2,])),
        col='grey', border=FALSE)
lines(fit1)



################## Verschiedenes ##################

# Siehe auch "R-Matlab quick reference.pdf" im Ordner "own functions" oder: http://www.math.umaine.edu/~hiebeler/comp/matlabR.html
# Alles ueber apply, mapply, tapply...
# http://stackoverflow.com/questions/3505701/r-grouping-functions-sapply-vs-lapply-vs-apply-vs-tapply-vs-by-vs-aggrega
tapply(aaa[,"fk_ArbeitDritte"],aaa[,"Region"],mean) #Mittelwerte Regionsweise berechnen (fuer eine Variable)
apply( results,2, function(x) tapply(x,results[,"Clusternummer"],sd) ) #Standardabweichung Clusterweise berechnen. (Fuer alle Variablen)
# Achtung: apply ?ber data.frames ist sehr langsam, weil erst zu Matrix umgewandelt werden muss
# Da ein data.frame eine Liste ist, ist lapply bzw. sapply viel schneller!
lapply( list, function(x)mean(x))
sapply( list, function(x)mean(x)) # Gibt einen Vektor statt einer Liste zur?ck.
#
zapsmall(c(0.00000001,0.004,0.5,5)) # Round values such that the small ones are displayed 0.
cbind();rbind()
data.frame(list(col1,col2,col3)) # So kann man characters u. numerics u. Zahlen in Dataframe geben ohne "" bei numerics. Aber Achtung: character werden zu factors.
any(TRUE,FALSE);all(TRUE,FALSE)
cumsum() # cumulative sum, kumulativ
rnorm(n,mean=0,sd=1) # bestimmte Anzahl normalverteilte Zufallsvariablen ausgeben.
head(aq) # Nur die ersten paar Spalten eines Datensatzes anzeigen
fitted() # Um Werte mit einer errechneten Regression vorauszusagen
relevel(as.factor(c(1,2,3,5,6)), ref=5)
abs()    # Betrag einer Zahl, negativ zu positv
table <- matrix(1:25,nrow=5,ncol=5) # Bereich ueber der Diagonale Matrix entfernen
  table[which( row(table)-col(table) <0)] <- 0
table <- matrix(1:25,nrow=5,ncol=5) # Bereich unter der Diagonale in den Bereich ?ber der Diagonale kopieren
  ttable <- t(table)
  table[which( row(table)-col(table) <0)] <- ttable[which( row(ttable)-col(ttable) <0)]

print(table, na="") # gibt NAs bei printen nicht aus
do.call(c,list)     # Alle Elemente einer Liste zusammenfuegen, geht auch mit anderen Operationen wie zb. rbind odr sum
Reduce("+",list)    # Operation ueber alle Elemente einer Liste sukzessive ausfuehren (nicht alles auf einmal).
1%%2                #gibt Rest wieder
c(1,2,3,4)%in%c(1,2) # Umkehrung von which in logical
all.equal(1:3,1:3)  # Sind alle Eintr?ge in zwei Vektoren/Matrizen gleich?
rev(matrix)         # Gibt Matrix in umgekehrter Spaltenreihenfolge wieder
combn(x=c(1,2,3), m=2) # Gibt alle moeglichen Kombinationen der Zahlen wieder #require(utils)
x <- replace(x,3,1.5) # replace x[3] with 1.5
# Werte, die NA oder Inf sind finden und mit etwas anderem ersetzen:
x <- c(1:3, Inf, -Inf, NA, NaN);
x; !is.finite(x); x[!is.finite(x)] <- 0; x
t(matrix(1:100, ncol=10)) # Transponieren
aperm(array(1:100, dim=c(2,5,10)), c(2,3,1)) # Array Transponieren (Array Permutation)

# Check difference between to groups, generally like this:
x <- rnorm(50); x2 <- rnorm(50, -1)
t.test(x, x2, alternative = "g")
wilcox.test(x, x2, alternative = "g") # non-parametric
ks.test(x, x2, alternative = "l")

# Wilcoxon Rank sums or Kruskal-Wallis for non-parametric significan differences in values of groups.
x <- sample(c(1,1,1,0),100,replace=TRUE)
y <- sample(c(1,1,0,0),100,replace=TRUE)
kruskal.test(x=c(x,y), g=as.factor(c(rep(1,100),rep(2,100))))
wilcox.test(x,y, alternative=c("two.sided", "less", "greater")[1], correct=FALSE)
pairwise.wilcox.test()

require(mvoutlier); outliers <- aq.plot(aaa); aaa <- aaa[outliers$outliers==FALSE,] # Multivariate Outliers entfernen/remove

# Funktionen ableiten und Punkt finden, wo die Steigung 0 ist.
expr <- expression( 1.0338e-01*x - 1.0941e-02*x^2 )
d_expr <- D(expr, name="x"); d_expr
f <- function(x)  eval(d_expr)
f(1)
d0 <- uniroot(f, c(0, 100))$root; d0
f(d0)
# alles zusammen wird in der Funktion find.deriv.0 gemacht. Hier ein Beispiel, wie man aus Koeffizienten
# were rauslesen und die L?sung gleich berechnen kann
coefs <- c(-0.00360093 ,2.939055e-05); names(coefs) <- c("Alter", "I(Alter^2)")
expr <- parse(text=paste0(coefs["Alter"],"*x + ", coefs["I(Alter^2)"], "*x^2"))
find.deriv.0(expr)

nchar("asdf")         # L?nge eines Strings
substr("asdf",1,2)    # Teil eines Strings auslesen
colnames.x <- c("landWeizen","landGerste","GVE_Milchkuehe","GVE_Mutterkuehe","roh_Weizen")
grep("land", colnames.x) # Alle colnames finden, die einen gewissen Textteil beinhalten
grep("land|GVE", colnames.x) # Mehrere Strings gleichzeitig suchen

file.choose()        # Pfad per Mausklick suchen, nicht eintippen.

attach(anova.data) # So koennen Variablen und Faktoren einfach ueber den Variablennamen genannt werden. kein anova.data[,...] oder anova.data$... noetig.
detach(...)

# Posting my data in forum
dput()

# With / Within
within() # innerhalb eines Datensatzes veraenderungen vornehmen, ohne Muehsam neue Spalten hinzuzufuegen.
    data(airquality)
    aq <- within(airquality, {     # mit {} koennen mehrere Variablen auf einmal geaendert werden
        lOzone<-log(Ozone)
        Month<-factor(month.abb[Month])
        cTemp <- round((Temp - 32) * 5/9, 1) # From Fahrenheit to Celsius
        rm(Day, Temp)  })
with()
    with(airquality, cbind(Month,log(Ozone)))
    with(data.frame(u = c(5,10,15,20,30,40,60,80,100),
                    lot1 = c(118,58,42,35,27,25,21,19,18),
                    lot2 = c(69,35,26,21,18,16,13,12,12)),
        list(summary(glm(lot1 ~ log(u), family = Gamma)),
             summary(glm(lot2 ~ log(u), family = Gamma))))

# Datum formatieren - help siehe auch help(DateTimeClasses)
# Handling von Uhrzeiten.
strptime ('12:00:00', format= '%H:%M:%S') # Hinweis: %OS steht fuer fractional seconds (also z.B. 0.5). %S steht fuer ganze Sekunden.
# - Einlesen ohne Zeit
strptime("21.4.2015", "%d.%m.%Y")
# - Einlesen mit Zeit
strptime("17.10.2014 17:00:00", "%d.%m.%Y %H:%M:%S")
# - Ausgeben mit Zeit
date <- strptime("17.10.2014 17:00:00", "%d.%m.%Y %H:%M:%S")
strftime(date, "%d.%m.%Y %H:%M:%S")

# Schauen, ob sich Elemente in einer Spalte als Expression evaluieren lassen.
# Wenn nicht, dann sind es character columns.
#x <- dat_colnames[,1]; y <- "100-2"
charcols <- apply(dat_colnames,2,function(x){
  apply(matrix(x),1,function(y){
    if(is.na(y)) {
      return(FALSE)
    } else {
      is( tryCatch(eval(parse(text=y)),error=function(e)e,warning=function(w)w), "error")
    }
  })
})

# Info zu tryCatch():
# http://adv-r.had.co.nz/Exceptions-Debugging.html

# Memory requirements fuer einen Datensatz berechnen (RAM)
# https://www.r-bloggers.com/calculating-memory-requirements/
# http://adv-r.had.co.nz/memory.html

# Moving average
moving.average <- function(x,n=5,dir=c("middle","retro","forward")){
  dir <- match.arg(dir)
  if(dir=="forward") return( rev(as.vector(filter(rev(x),rep(1/n,n), sides=1))) )
  return(as.vector(filter(x,rep(1/n,n), sides=if(dir=="middle") 2 else 1 )))
}

#### Parallel computing in R on Windows. Keyword: threads, processes
#install.packages("snow")
library(snow)

# Create data.
a <- "a"
z <- vector('list',4)
z <- 1:4

# 4 seconds = benchmark without parallel computing
system.time(lapply(z,function(x) Sys.sleep(1)))

# Make cluster. Important is the argument type="SOCK".
cl<-makeCluster(4,type="SOCK")
# 1 second
system.time(clusterApply(cl, z,function(x) {Sys.sleep(1)}))
# Accessing 'a' inside the cluster functoin causes an error because 'a' was not exported.
system.time(clusterApply(cl, z,function(x) {print(a); Sys.sleep(1)}))
# Thus, export all needed variables. Can also be done by using list=ls()
clusterExport(cl, list="a")
# Now run again without error.
# (However, printing does not work in clusters)
system.time(clusterApply(cl, z,function(x) {print(a); Sys.sleep(1)}))

# Stop cluster in the end.
stopCluster(cl)

#### Call by reference
# https://www.r-bloggers.com/call-by-reference-in-r/
assignRefValue <- function(obj, value){
  eval.parent(substitute( obj <- value ))
}
obj <- 0
assignRefValue(obj, 5)
print(obj) # 5

#### Special calls
# http://adv-r.had.co.nz/Functions.html#special-calls
# Create operators like
`%+%` <- function(a, b) paste0(a, b)
"new" %+% " string"

#### HTML Darstellung von Regressions-Modellen
x1 <- rnorm(100)+1:100; x2= x1/2; y <- rnorm(100)+1:100
model1 <- lm(y~x1); model2 <- lm(y~x2)
stargazer::stargazer("Model 1"=model1,
                     "Model 2"=model2,
                     type="html",
                     dep.var.labels=c("y"),
                     out="models.html")
