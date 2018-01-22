### Author: Daniel Hoop
###
### Insert the command: source("location of this file") into your Rprofile.SITE. You find the Rprofile.SITE in some location like:  C:\Programme\R\R-2.15.0\etc\Rprofile.SITE
### Or read it in via R console > source("filedirectory of this file")
### Or simply copy it into your R console.

# Most useful functions
# *************************
# Grouping
# group.by.quantiles / group.by.fix.scale
# group.by.wtd.quantiles (in combination with mean.weight -> to calculate weighted means of upper and lower income quartile)
#
# Calc weighted means / quantiles
# mean.weight (in combination with extract.I.vars & create.cols)
# median.weight (as wrapper for quantile.weight)
#
# Find columns / view data / clipboard copying
# find.col, find.gb.col, find.spa.col (quickly find column names with string pattern)
# read.cb, write.cb, view (read/write from clipboard. save as csv and view in Excel)
#
# Convenience functions
# slash  (convert back slashes to front slashes. Useful if copying file paths from windows to R scripts)
# categ.to.bin / categ.to.ordinal  (transform categorial variables to binary or ordinal)
# char.cols.to.num (check columns of data frame. If somethin is like " 23.3 " and interpreted as character -> Automatically convert to numeric )
#
# For Agroscoepe ZA-BH
# *************************
# Load data
# load.spa, load.gb, load.agis, load.cost (quickly load data, e.g. full costs)
# vergleichslohn, vergleichszins (quickly load data, e.g. opportunity costs)
#
# Other (translate coding and decrypt IDs)
# transl.reg, transl.typ, transl.ths, transl.lbf, transl.mm, transl.spb.330col.340row, transl.spb.320row, transl.ref.210row, transl.EB.MM
# id.entschluesseln (decrypt IDs of Referenzbetriebe)
# rekid.zaid (key between REK_ID[LINK] & ZA_ID[AGIS] )

# -- Source locally  --
# if(!exists("mean.weight")) tryCatch( suppressWarnings(source("//adb.intra.admin.ch/Agroscope$/Org/Sites/TA/Transfer/hpda/R/func.R")), error=function(e) tryCatch( suppressWarnings(source("//evdad.admin.ch/AGROSCOPE_OS/2/5/2/1/3/1/4269/B0000/func.R")), error=function(e) tryCatch( suppressWarnings(source("//evdad.admin.ch/AGROSCOPE_OS/2/5/2/2/3583/Resultate/00-00-00_Zusatzdaten/R_func/func.R")), error=function(e) source("https://raw.githubusercontent.com/danielhoop/R/master/func.R"))))
#
# -- GitHub --
# if(!exists("mean.weight")) source("https://raw.githubusercontent.com/danielhoop/R/master/func.R")
# browseURL("https://github.com/danielhoop/R/edit/master/func.R")
# browseURL("https://github.com/danielhoop/R/edit/master/Rhelp.R")


#### Options ####
.onHpdaPc <- function() return( isTRUE(file.info("C:/Users/U80823148/")$isdir) || isTRUE(file.info("C:/Users/A80823148/")$isdir) )
.dataFolder <- function() return( paste0("C:/Users/",Sys.info()["user"],"/_/Data/") )

# Optionen nur bei mir selbst einlesen. Nicht auf anderen Computern (falls Script-Ausfuehrung ueber Laufwerk W:)
if(FALSE && .onHpdaPc())  {
  options(scipen = 3) # mit scipen = 3 geht die Digits-Anzeige bis 0.000001 (also 1e-06). Ab 1e-07 in scientific notation.
  options(help.try.all.packages=TRUE)
  #options(prompt="    ")
  options(stringsAsFactors=FALSE)
  #options(max.print=1000)
  #options(na="")
  cat("**********************************************************************\n")
  cat("Options set.\n")
}

#### Automatisches Kopieren auf Laufwerke ####

file.copy.readOnly <- function(from, to, overwrite=TRUE, ...){
  # Function to copy a file and set to read only on Windows.
  # Arguments see file.copy()
  
  onWindows <- grepl("window",Sys.info()['sysname'], ignore.case=TRUE)
  if(onWindows){
    for(to1 in to) {
      to1 <- gsub("/","\\\\",to1)
      if(overwrite) system(paste0("attrib -R \"",to1,"\""), intern=FALSE)
      file.copy(from, to1, overwrite=overwrite, ...)
      system(paste0("attrib +R \"",to1,"\""), intern=FALSE)
    }
  } else warning("The function currently works only on Windows operating systems.")
}

if(!exists("dir.exists")){
  dir.exists <- function(x) isTRUE(file.info(x)$isdir)
}

# Nicht auf anderen Computern (falls Script-Ausfuehrung ueber Laufwerk W:)
.copyFuncs <- function(fromPath, toPath){
  
  pathFilesize <- paste0(fromPath, "funcSize.txt")
  filesize0 <- scan(pathFilesize, quiet=TRUE)
  filesize1 <- file.size(paste0(fromPath, "func.R")) + file.size(paste0(fromPath, "SqlUtilities.R"))
  
  if(filesize0!=filesize1){
    write(filesize1, file=pathFilesize)
    
    for(toPath1 in toPath){
      file.copy.readOnly(paste0(fromPath,"/func.R"), paste0(toPath1,"/func.R"), overwrite=TRUE)
      file.copy.readOnly(paste0(fromPath,"/Rhelp.R"), paste0(toPath1,"/Rhelp.R"), overwrite=TRUE)
      # SQLUtilities nur fuer ZA verfuegbar machen!
      if( grepl("//evdad.admin.ch/AGROSCOPE_OS/2/5/2/1/3/1",toPath1) ){
        file.copy.readOnly(paste0(fromPath,"/SqlUtilities.R"), paste0(toPath1,"/SqlUtilities.R"), overwrite=TRUE)
      }
      cat("func.R and Rhelp.R copied from P to W\n")
    }
  } else {
    cat("func.R did not change. Files not copied from P to W\n")
  }
}

.copyZaData <- function(){
  # This function copies za data from the network drives to the hard drive of a computer. If all files are up to date, nothing is done.
  # load.spe(), load.spb(), etc. will then access the files on the hard drive instead of the network drives.
  # This function is called each time when func.R is sourced. This way it will be guaranteed that all data files on the hard drive are always up to date!
  
  datFold <- .dataFolder()
  zamain <- "//art-settan-1000.evdad.admin.ch/ZAMAIN/ZADaten/"
  oslwParent <- "//evdad.admin.ch/AGROSCOPE_OS/2/5/2/1/3/3/"
  oslwCheck <- "//evdad.admin.ch/AGROSCOPE_OS/2/5/2/1/3/3/4276/alldata"
  
  if(dir.exists(zamain) && dir.exists(oslwCheck)){
    
    if(!dir.exists(datFold)){
      cat("Creating folder ", datFold, "\n", sep="")
      dir.create(datFold, recursive=TRUE)
    }
    if(!dir.exists(paste0(datFold,"AGIS/"))){
      cat("Creating folder ", paste0(datFold,"AGIS/"), "\n", sep="")
      dir.create(paste0(datFold,"AGIS/"), recursive=TRUE)
    }
    
    files <- matrix(c(
      paste0(datFold,"AGIS/AGIS_BFS_2014.RData"),paste0(zamain, "AGIS/2014/AGIS_BFS_2014.RData"),
      paste0(datFold,"AGIS/AGIS_BFS_2015.RData"),paste0(zamain, "AGIS/2015/AGIS_BFS_2015.RData"),
      paste0(datFold,"AGIS/AGIS_BFS_2016.RData"),paste0(zamain, "AGIS/2016/AGIS_BFS_2016.RData"),
      paste0(datFold,"GB.RData"),paste0(oslwParent,"4273/GB/GB.RData"),
      paste0(datFold,"SpE.RData"),paste0(oslwParent,"4276/alldata/SpE.RData"),
      #paste0(datFold,"Personen_Indexiert.RData"),paste0(oslwParent,"4276/Personen/Personen_Indexiert.RData"),
      paste0(datFold,"SpB.RData"),paste0(oslwParent,"4275/alldata/SpB.RData")
    ),ncol=2, byrow=TRUE)
    
    # Temporaer: Personen_Indexiert loeschen.
    if(file.exists(paste0(datFold,"Personen_Indexiert.RData"))){
      file.remove(paste0(datFold,"Personen_Indexiert.RData"))
    }
    if(file.exists(paste0(datFold,"Personen_Indexiert.csv"))){
      file.remove(paste0(datFold,"Personen_Indexiert.csv"))
    }
    
    invisible(apply(files,1,function(x){ # x <- files[1,]
      if(file.exists(x[2])){
        if( !file.exists(x[1]) || file.info(x[1])$mtime < file.info(x[2])$mtime ){
          cat("Copying file '",x[2], "' to '", x[1], "'. Success=", sep="")
          cat(file.copy(x[2],x[1],overwrite=TRUE))
          cat("\n")
        }
      }
    }))
    
  }
}

## Copying func & data here ##
if( .onHpdaPc() && file.exists("//evdad.admin.ch/AGROSCOPE_OS/2/5/2/1/3/9/4278/hpda/R/func/func.R") ) {
  .copyFuncs(fromPath="//evdad.admin.ch/AGROSCOPE_OS/2/5/2/1/3/9/4278/hpda/R/func/",
             toPath=c("O:/Sites/TA/Transfer/hpda/R/", "//evdad.admin.ch/AGROSCOPE_OS/2/5/2/1/3/1/4269/B0000/"))
}
.copyZaData()

## Removing functions that are not used. ##
rm(.copyFuncs, .copyZaData)


#### GRAPHICS ####

show.pch <- function(show=1:255,mfrow=c(5,5),mar=c(4,1,1,3)){
  # Show what the pch numbers mean (in a graph).
  mar.orig <- par()$mar; mfrow.orig <- par()$mfrow; #on.exit(par(mar=mar.orig, mfrow=mfrow.orig))
  par(mar=mar, mfrow=mfrow)
  for(i in show) suppressWarnings( plot(1,pch=i,xlab=i) )
  par(mar=mar.orig, mfrow=mfrow.orig)
}
####
color.check <- function(){
  cols <- c("#FF0000","#00FF00","#0000FF","#FFFF00","#FF00FF","#00FFFF","#FFFFFF")
  plot(1:length(cols),rep(1,length(cols)),col=cols, pch=20, cex=5,xaxt="n")
  points(1:length(cols),rep(1,length(cols)),col="black", pch=21, cex=4)
  axis(1, at=1:length(cols),labels=cols)
}
####

gg_color_hue <- function(n) {
  hues = seq(15, 375, length=n+1)
  hcl(h=hues, l=65, c=100)[1:n]
}
if(FALSE){
  #For example:
  n = 10
  cols = gg_color_hue(4)
  #dev.new(width=4, height=4)
  plot(1:n, pch=16, cex=3, col=cols)
}

####

cornerlegend <- function(v=c("o","m","u")[3],h=c("l","m","r")[3],ply=0,plx=0, ...){
  # This function places the legend in the specified corner of the graph. Give all agruments needed to produce the legend in the ,... argument.
  # v= vertical orientation, h= horizontal orientation. In german! "o","r" means for example: oben-rechts (upper right).
  # ply: plus y coord., plx: plus x coord.
  # Alternative: Try legend() and simply write: "bottomright", "bottom", "bottomleft", "left", "topleft", "top", "topright", "right", "center"
  
  yxlim <- par("usr")
  if(v=="o") {yjust <- 1; y <- yxlim[4];} else if(v=="m") {yjust <- 0.5; y <- mean(c(yxlim[3],yxlim[4]));} else if(v=="u") {yjust <- 0; y <- yxlim[3]+ply;}
  if(h=="l") {xjust <- 0; x <- yxlim[1];} else if(h=="m") {xjust <- 0.5; x <- mean(c(yxlim[1],yxlim[2]));} else if(h=="r") {xjust <- 1; x <- yxlim[2]+plx;}
  legend(x=x,y=y,xjust=xjust,yjust=yjust, ...)
}
####
curveChar <- function(expr, ...) {
  # curve() for character input instead of expression
  # Try this: expr <- "100*(1-exp(-0.025*(40+x)))*(1-exp(-0.0045*(200+400)))"; curveChar(expr, from=1, to=160)
  curve((function(x) eval(parse(text=expr)))(x), ...)
}

linreg.plot <- function(form,data,method=c("lm","rlm"),...){
  # see also curve()
  
  method <- match.arg(method)
  if(method=="lm") {
    reg <- lm(form, data=data)
    coefs <- reg$coefficient
  } else if(method=="rlm") {
    require.package(MASS)
    reg <- rlm(form, data=data)
    coefs <- reg$coefficients
  }
  abline(coefs[1],coefs[2],...)
  invisible(summary(reg)$coefficient)
}
####

parcoord.sorted <- function(data, orderByCorrelation=TRUE, colorGradient=TRUE, gradientColors=c("red","green","blue"), col=NULL, alpha=0.4, ...){
  # This function makes a parallel coordinates plot and sorts it according to the correlations between the variables.
  #
  # Arguments
  # orderByCorrelation: Logical indicating if the variables should ordered by correlation. Like this, patterns can be detected more easily.
  #                     Variables correlating negatively with the first variable (in data) will be placed left to this variable. Those with positive correlation will be right to the first variable.
  # colorGradient:      Logical indicating if a color gradient should be introduced.
  # gradientColors:     Character. The colors along which the gradient should be created.
  # col:                Character. Of length 1, or length(col)==nrow(data). Specifying the exact color for each observation.
  # alpha:              The transparency value as in rgb(..., alpha=...)
  
  if(orderByCorrelation){
    cors <- cor(data)[,1]
    negCors <- sort( cors[cors< 0], decreasing=TRUE)
    posCors <- sort( cors[cors>=0], decreasing=TRUE)
    #cors <- sort(cors, decreasing=TRUE)
    plotVars <- c(names(negCors),names(posCors))
  } else {
    plotVars <- colnames(data)
  }
  
  if(colorGradient & !is.null(col)) stop("Either specifiy colorGradient==TRUE or col. colorGradient==TRUE and !is.null(col) does not work.")
  
  if(colorGradient){
    if(length(gradientColors)==0) stop("length(gradientColors) must be greater than 0.")
    x <- data[,cvars[1]]
    col2 <- colorRampPalette(gradientColors) (100) [ findInterval(x, seq(min(x),max(x), length.out=100)) ] # color.gradient(data[,cvars[1]], colors)
  } else if(!is.null(col)) {
    if( !length(col)%in%c(1,nrow(data)) ) stop("length(col) must be equal to 1 or nrow(data).")
    col2 <- if(length(col)==1) "black" else col
  }
  col2 <- rgb( t(col2rgb(col2, alpha=FALSE)/255 ), alpha=alpha)
  
  MASS::parcoord(data[,plotVars], col=col2, ...)
}

####
radarchart.tutorial <- function() {
  # Radar chart / spider chart / Spinnendiagramm
  dat <- as.data.frame(matrix(sample(1:9,9), ncol=3))
  # Data must contain max and min values in the top 2 rows.
  radarData <- rbind(sapply(dat,range)[2:1,,drop=FALSE],
                     dat)
  # Line color
  pcol <- adjustcolor(rainbow(nrow(radarData)-2), alpha=0.5)
  # Filling collor
  pfcol <- adjustcolor(pcol, alpha=0.2)
  # Function
  fmsb::radarchart(
    radarData, title="Radarchart",
    # Settings for lines
    pcol=pcol, pfcol=pfcol, 
    plwd=1, plty=1,
    # Settings for axis
    cglcol="grey", cglty=1, cglwd=0.8, axislabcol="black", axistype=2,
    # Settings for points
    vlcex=0.8, palcex=0.8 # use pty=32 to disable points.
  )
}

####
if(FALSE){
  pairs.smooth(cor.data, main="Zusammenhang zw. SDB u. Sampling Rate AG(SO)")
  pch=20; cors=c("no","upper","lower")[2]; abline01=TRUE; pointscol="black"; smoothcol="red"; ablinecol="chartreuse4"; digits=2
  x <- cor.data
}
pairs.smooth <- function(x, pch=20, cors=c("no","upper","lower"), cor.method=c("pearson", "kendall", "spearman"), abline01=TRUE, pointscol="black", smoothcol="red", ablinecol="chartreuse4", digits=2, ...){
  cors <- match.arg(cors)
  cor.method <- match.arg(cor.method)
  panel.cor <- function(x, y, digits=2, prefix="", cex.cor, ...)  {
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(0, 1, 0, 1))
    r <- round( cor(x, y, method=cor.method), digits=digits) # abs(cor(x, y))
    txt <- format(c(r, 0.123456789), digits=digits)[1]
    txt <- paste(prefix, txt, sep="")
    if(missing(cex.cor)) cex.cor <- 2#0.5/strwidth(txt)
    text(0.5, 0.5, txt, cex = cex.cor)# * r)
  }
  panel.hist <- function(x, ...) {
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(usr[1:2], 0, 1.5) )
    h <- hist(x, plot = FALSE)
    breaks <- h$breaks; nB <- length(breaks)
    y <- h$counts; y <- y/max(y)
    rect(breaks[-nB], 0, breaks[-1], y, col="gray")
  }
  panel.smooth <- function (x, y, col = smoothcol, bg = NA, pch = par("pch"), cex = 1, col.smooth = "red", span = 2/3, iter = 3, ...) {
    points(x, y, pch = pch, col = pointscol, bg = bg, cex = cex)
    ok <- is.finite(x) & is.finite(y)
    if (any(ok)) {
      lines(stats::lowess(x[ok], y[ok], f = span, iter = iter),
            col = col.smooth, ...)
      if(abline01) {
        abline(0,1,col=ablinecol)
        mx <- mean(x)
        text(mx, mx, "y=x", col=ablinecol, adj=1.1)
      }
    }
  }
  if(cors=="no"){
    pairs(x,pch=pch, lower.panel=panel.smooth, upper.panel=panel.smooth, diag.panel=panel.hist, ...)
  } else if(cors=="upper"){
    pairs(x,pch=pch, lower.panel=panel.smooth, upper.panel=panel.cor, diag.panel=panel.hist, ...)
  } else {
    pairs(x,pch=pch, lower.panel=panel.cor, upper.panel=panel.smooth, diag.panel=panel.hist, ...)
  }
}
####

boxplot.multi <- function(data,grouping=NULL,meansd=FALSE,notch=TRUE,mark.notch=NULL,ovrl.median=TRUE,main=NULL,plotrows=3,split=1,mar=c(2,2.6,2,0.4),newwin=FALSE, info=FALSE, ...){
  # Make boxplots for several columns of a data.frame or matrix
  # Attention: The colnames of the data must not contain " ". Fill with "_"
  
  par.orig <- par()$mar; mfrow.orig <- par()$mfrow; on.exit(par(mar=par.orig, mfrow=mfrow.orig))
  
  if(is.null(dim(data))) {data <- matrix(data,ncol=1); colnames(data) <- "check_variable"}
  if(ncol(data)==1) {
    plotrows <- 1
    if(all(mar%in%c(2,2.6,2,0.4))) mar <- par()$mar
  }
  if(is.null(grouping)) grouping <- rep(1,nrow(data))
  if(!is.null(mark.notch)) if(!mark.notch%in%unique(grouping)) stop("mark.notch must be one of ",paste(sort(unique(grouping)),collapse=", "))
  if(is.null(colnames(data))) stop("the data must have colnames")
  data.orig <- data
  
  choose <- list()
  for(s in 1:split) {
    if(s==1) {choose[[s]] <- 1:floor(ncol(data.orig)/split)
    } else {
      if(s<split) { choose[[s]] <-  ((s-1)*ceiling(ncol(data.orig)/split)) : (s*floor(ncol(data.orig)/split))
      } else choose[[s]] <-  ((s-1)*ceiling(ncol(data.orig)/split)) : ncol(data.orig)
    }
    data <- data.orig[,choose[[s]],drop=FALSE]
    
    if(is.null(main)) mains <- colnames(data) else if(length(main)==1) mains <- rep(main,ncol(data)) else mains <- main[choose[[s]]]
    nvariables <- ncol(data)
    data <- cbind(data,grouping=grouping)
    if(newwin) windows()
    par(mar=mar, mfrow=c(plotrows,if(nvariables%%plotrows==0) nvariables/plotrows else ceiling(nvariables/plotrows) ))
    for(j in 1:nvariables) {
      if(!meansd) {
        box <- boxplot( formula=as.formula(paste(colnames(data)[j],"~ grouping")) , data=data , main=mains[j], notch=notch, ...)
        if(ovrl.median) {
          if(is.null(mark.notch)) abline(a=median(data[,j]), b=0)
          if(!is.null(mark.notch)) abline(a=median(data[,j]), b=0, lwd=2)
        }
        if(!is.null(mark.notch)) {
          abline(a=box$conf[1,mark.notch], b=0)
          abline(a=box$conf[2,mark.notch], b=0)
        }
      } else {
        box.meansd( data[,j],grouping, main=mains[j], ...)
        if(ovrl.median) abline(a=mean(data[,j],na.rm=TRUE), b=0)
      }
    }
  }
  if(info) cat("If the notches of different groups do not overlap this is 'strong evidence' that the two medians differ (Chambers et al., 1983, p. 62). From help(boxplot)\n")
}
#boxplot.multi(aaa,grouping=c(rep(1,floor(nrow(aaa)/2)),rep(2,ceiling(nrow(aaa)/2))))
####

boxplot.probabilities <- function(y=NULL, grouping=NULL, list=NULL, probs=c(0.025,0.25,0.5,0.75,0.975), outline=FALSE, ...) {
  # Boxplot with probability quantiles (not the original whisker definition).
  
  if(is.null(list)&any(c(is.null(y),is.null(grouping)))) stop("specify either list or y and grouping")
  if(!is.null(list)&any(c(!is.null(y),!is.null(grouping)))) stop("specify either list or y and grouping")
  if(length(probs)!=5) stop("length(probs) != 5")
  
  if(is.null(list)) {
    bp <- boxplot(as.formula("y~grouping"),plot=FALSE, ...)
    uniquegrouping <- sort(unique(grouping))
  } else {
    bp <- boxplot(list,plot=FALSE,outline=outline, ...)
    uniquegrouping <- 1:length(list)
  }
  
  for(i in uniquegrouping) {
    if(is.null(list)) { x <- y[grouping==i]
    } else { x <- list[[uniquegrouping[i]]] }
    quants <- quantile(x,probs=probs,na.rm=TRUE)
    bp$stats[,i] <- quants
  }
  bxp(bp,outline=outline, ...)
  invisible(bp)
}
####

boxplot.3.lines <- function(y, ...){
  # Draws a boxplot with only 3 horizontal lines.
  # Enter a 3 * (number of groups) matrix with upper, middle and lower value for boxplot
  
  grouping <- 1:ncol(y)
  y1 <- y[c(1),]
  bp <- boxplot(as.formula("y1~grouping"),plot=FALSE)#, ...)
  med <-  y[2,]
  up <-   y[1,]
  low <-  y[3,]
  bp$stats <- unname(rbind(low, med,med,med, up))
  bp$out <- NULL
  bxp(bp, ...)
}
boxplot.meansd <- function(y,grouping, ...){
  bp <- boxplot(as.formula("y~grouping"),plot=FALSE, ...)
  means <-  tapply(y,grouping,mean,na.rm=TRUE)
  sds <-    tapply(y,grouping,sd,na.rm=TRUE)
  bp$stats <- unname(rbind(means-sds, means,means,means,means+sds))
  bp$out <- NULL
  suppressWarnings( bxp(bp, ...))
}
####

histogram.overlap <- function(data, grouping, bins=10, transparency=1/4, ...){
  ### WORK IN PROGRESS. DOES NOT WORK!
  h <- mids <- counts <- breaks.list <- list()
  for(j in grp) {
    hi <- hist(data[grouping==j],plot=FALSE)
    breaks <- hi$breaks
    nbreaks <- seq(min(breaks), max(breaks), length.out=bins)
    breaks.list[[j]] <- nbreaks
  }
  
  h <- mids <- counts <- list()
  for(j in grp) {
    hi <- hist(data[grouping==j],breaks=nbreaks,plot=FALSE)
    h[[j]] <- hi
    mids[[j]] <- hi$mids
    counts[[j]] <- hi$counts
  }
  
  binwidth <- h[[grp[1]]]$mids[2] - h[[grp[1]]]$mids[1] # Die bins sind f?r alle Gruppen gleich gross.
  xlim <- c(min(mids.call<-do.call("c",mids))-binwidth, max(mids.call)+binwidth)
  ylim <- c(min(counts.call<-do.call("c",counts)), max(counts.call))
  col <- rgb(colsrgb[1,grp[1]],colsrgb[2,grp[1]],colsrgb[3,grp[1]],alpha=colsrgb[4,grp[1]])
  
  plot(h[[grp[1]]], col=col, xlim=xlim, ylim=ylim, main=mains[i],xlab=xlab[i])
  if(length(grp)>1) for(j in grp[2:length(grp)]){
    col <- rgb(colsrgb[1,j],colsrgb[2,j],colsrgb[3,j],alpha=colsrgb[4,j])
    plot(h[[j]], col=col, xlim=xlim, ylim=ylim, add=TRUE)
  }
}

####
histogram.overlap.multi <- function(data,grouping=NULL,plotrows=3,mar=c(2,2.6,2,0.4),bins=10,cols=NULL,transparency=1/4,main=NULL,xlab="",split=1,newwin=FALSE) {
  # Plot several overlapping histograms in one plot (possible for 1 or more variables)
  
  if(is.null(cols)) cols <- c("red","darkgreen","blue","cyan","green","gray87","yellowgreen","steelblue1","orchid1","purple","orange","yellow")
  colsrgb <- col2rgb(cols, alpha = 1)
  colsrgb <- colsrgb/255
  colsrgb[4,] <- transparency
  
  par.orig <- par()$mar; mfrow.orig <- par()$mfrow; on.exit(par(mar=par.orig, mfrow=mfrow.orig))
  
  if(is.null(dim(data))) {data <- matrix(data,ncol=1); colnames(data) <- "check variable"}
  if(ncol(data)==1) {
    plotrows <- 1
    if(all(mar%in%c(2,2.6,2,0.4))) mar <- par()$mar
  }
  if(is.null(colnames(data))) stop("the data must have colnames")
  if(is.null(grouping)) grouping <- rep(1,nrow(data))
  grp <- sort(unique(grouping))
  data.orig <- data
  
  choose <- list()
  for(s in 1:split) {
    if(s==1) {choose[[s]] <- 1:floor(ncol(data.orig)/split)
    } else {
      if(s<split) { choose[[s]] <-  ((s-1)*ceiling(ncol(data.orig)/split)) : (s*floor(ncol(data.orig)/split))
      } else choose[[s]] <- ((s-1)*ceiling(ncol(data.orig)/split)) : ncol(data.orig)
    }
    data <- data.orig[,choose[[s]],drop=FALSE]
    
    if(is.null(main)) mains <- colnames(data) else if(length(main)==1) mains <- rep(main,ncol(data)) else mains <- main[choose[[s]]]
    if(length(xlab)==1) xlab <- rep(xlab,ncol(data.orig))
    if(newwin) windows()
    nvariables <- ncol(data)
    par(mar=mar, mfrow=c(plotrows,if(nvariables%%plotrows==0) nvariables/plotrows else floor(nvariables/plotrows)+1 ))   # %% gibt Rest wieder
    
    h <- breaks <- mids <- counts <- list()
    for(i in 1:ncol(data)){
      #h <- list()
      for(j in grp){
        hi <- hist(data[grouping==j,i],plot=FALSE)
        h[[j]] <- hi
        breaks[[j]] <- hi$breaks
      }
      nbreaks <- seq(min(break.call<-do.call("c",breaks)), max(break.call), length.out=bins)
      for(j in grp) {
        hi <- hist(data[grouping==j,i],breaks=nbreaks,plot=FALSE)
        h[[j]] <- hi
        mids[[j]] <- hi$mids
        counts[[j]] <- hi$counts
      }
      
      binwidth <- h[[grp[1]]]$mids[2] - h[[grp[1]]]$mids[1] # Die bins sind f?r alle Gruppen gleich gross.
      xlim <- c(min(mids.call<-do.call("c",mids))-binwidth, max(mids.call)+binwidth)
      ylim <- c(min(counts.call<-do.call("c",counts)), max(counts.call))
      col <- rgb(colsrgb[1,grp[1]],colsrgb[2,grp[1]],colsrgb[3,grp[1]],alpha=colsrgb[4,grp[1]])
      
      plot(h[[grp[1]]], col=col, xlim=xlim, ylim=ylim, main=mains[i],xlab=xlab[i])
      if(length(grp)>1) for(j in grp[2:length(grp)]){
        col <- rgb(colsrgb[1,j],colsrgb[2,j],colsrgb[3,j],alpha=colsrgb[4,j])
        plot(h[[j]], col=col, xlim=xlim, ylim=ylim, add=TRUE)
      }
    }
  }
}
####
density.overlap.multi <- function(data,grouping=NULL,bw="nrd0",na.rm=TRUE,cols=NULL,xlab="",main=NULL,legends=TRUE,mar=c(2,2.6,2,0.4),plotrows=3,split=1,newwin=FALSE,...) {
  # plot several overlapping density plots in one plot (possible for 1 or more variables)
  if(is.null(cols)) cols <- c("red","darkgreen","blue","cyan","green","gray87","yellowgreen","steelblue1","orchid1","purple","orange","yellow")
  par.orig <- par()$mar; mfrow.orig <- par()$mfrow; on.exit(par(mar=par.orig, mfrow=mfrow.orig))
  
  if(is.null(dim(data))) {data <- matrix(data,ncol=1); colnames(data) <- "check variable"}
  if(ncol(data)==1) {
    plotrows <- 1
    if(all(mar%in%c(2,2.6,2,0.4))) mar <- par()$mar
  }
  if(is.null(colnames(data))) stop("the data must have colnames")
  if(is.null(grouping)) grouping <- rep(1,nrow(data))
  grp <- sort(unique(grouping))
  data.orig <- data
  
  choose <- list()
  for(s in 1:split) {
    if(s==1) {choose[[s]] <-  1:floor(ncol(data.orig)/split)
    } else {
      if(s<split) { choose[[s]] <-  ((s-1)*ceiling(ncol(data.orig)/split)) : (s*floor(ncol(data.orig)/split))
      } else choose[[s]] <-  ((s-1)*ceiling(ncol(data.orig)/split)) : ncol(data.orig)
    }
    data <- data.orig[,choose[[s]],drop=FALSE]
    
    if(is.null(main)) mains <- colnames(data) else if(length(main)==1) mains <- rep(main,ncol(data)) else mains <- main[choose[[s]]]
    if(length(xlab)==1) xlab <- rep(xlab,ncol(data.orig))
    if(newwin) windows()
    nvariables <- ncol(data)
    par(mar=mar, mfrow=c(plotrows,if(nvariables%%plotrows==0) nvariables/plotrows else floor(nvariables/plotrows)+1 ))   # %% gibt Rest wieder
    
    dens <- densx <- densy <- list()
    for(i in 1:ncol(data)) {
      for(j in grp) {
        de <- density(data[grouping==j,i],bw=bw,na.rm=na.rm)
        dens[[j]] <- de
        densx[[j]] <- de$x
        densy[[j]] <- de$y
      }
      
      xlim <- ylim <- numeric()
      xlim[1] <- min(calldensx <- do.call("c",densx))
      xlim[2] <- max(calldensx)
      ylim[1] <- min(calldensy <- do.call("c",densy))
      ylim[2] <- max(calldensy)
      
      plot(dens[[grp[1]]],xlim=xlim,ylim=ylim,col=cols[grp[1]],xlab=xlab[i],main=mains[i], ...)#,main="Dens. Plot",xlab="Efficiency");
      for(j in grp[2:length(grp)]) lines(dens[[j]],col=cols[j], ...)
      if(legends & length(grp)>1) legend("topright", legend=grp, col=cols[1:length(grp)], lty=1, bty="n", ...)
    }
  }
}


if(FALSE){
  # Debug color.gradient()
  x <- 1:10
  plot(rep(1, length(x)), y=x, col=color.gradient(x), pch=19,cex=3)
  x <- c(x, 10); length(unique(x))
  plot(rep(1, length(x)), y=x, col=color.gradient(x), pch=19,cex=3)
  x <- c(x, 10 , 20); length(unique(x))
  plot(rep(1, length(x)), y=x, col=color.gradient(x), pch=19,cex=3)
  x <- c(1,1,1,1,1,1.2,2,2.2,5,6.5,8,8.6,10,10); length(unique(x))
  plot(rep(1, length(x)), y=x, col=color.gradient(x), pch=19,cex=3)
  x <- c(1,1,1.1,1.2,1,1.3,2,2.2,5,5.1,5.2,5.3,5.4,6.5,8,100); length(unique(x))
  plot(rep(1, length(x)), y=x, col=color.gradient(x), pch=19,cex=3)
  x <- seq(1,3.7,length.out=200)
  x <- y
  plot(rep(1, length(x)), y=x, col=color.gradient(x), pch=19,cex=3)
  plot(1:length(x), y=x, col=color.gradient(x), pch=19,cex=3)
}

color.gradient <- function(x, colors=c("red","yellow","green"), colsteps=100) {
  # Create Color Gradient for a given vector x with given colors.
  #
  # The function creates a color function with colorRampPalette().
  # Then it hands over the number of unique elements of x into this function()().
  # From the result of the function()()[ ] only these elements are picked which are most similar to the values in the sequence min(x) to max(x)
  # If length(unique(x)) is relatively small (<15) it is done in a computation intensive matter in order to to achieve better results.
  # Else it is done with findInterval() which is much faster.
  # Example found in the internet: browseURL("http://stackoverflow.com/questions/18827214/one-colour-gradient-according-to-value-in-column-scatterplot-in-r")
  
  return( colorRampPalette(colors) (colsteps) [ findInterval(x, seq(min(x),max(x), length.out=colsteps)) ] )
}

#### CONVENIENCE FUNCTIONS ####

loadSqlUtils <- function(){
  # This function loads the SQL Utility functions to access the ZA database.
  # Because these functions containt confidential login information for the database, they are not part of this file (func.R).
  source("//evdad.admin.ch/AGROSCOPE_OS/2/5/2/1/3/1/4269/B0000/SqlUtilities.R")
}

slash <- function(reverse=FALSE){
  # This function changes \ to / in paths (or vice versa if reverse=TRUE)
  
  cb <- suppressWarnings(readLines("clipboard"))
  if(!any(grepl("\\\\",cb)) && grepl("/",cb)) reverse <- TRUE
  
  if(!reverse){
    txt <- gsub("\\\\","/",cb)
  } else {
    txt <- gsub("/","\\\\",cb) # Like this: "/{1,10}" you would replace several / with only one \
  }
  write.table(txt,'clipboard',quote=FALSE,col.names=FALSE,row.names=FALSE,eol=""); cat("Converted string is in clipboard. Use Ctrl+V:\n",txt,"\n",sep="")
}

winSlashes <- function(x){
  if(!grepl("window",Sys.info()["sysname"],ignore.case=TRUE))
    return(x)
  
  x <- gsub("/","\\\\",x)
  if(substr.rev(x,1,1)=="\\")
    x <- substr(x, 1, nchar(x)-1)
  return(x)
}


# a=1, b="A,B", d=c(1,1)
arg <- function(){
  txt <- suppressWarnings(readLines("clipboard"))
  qc <- bc <- 0 # quote counter, bracket counter
  for(i in 1:nchar(txt)){
    if(        qc==0 && substr(txt,i,i)%in%c("\"","'")) { qc <- qc+1
    } else if (qc==1 && substr(txt,i,i)%in%c("\"","'")) { qc <- qc-1 }
    if(        qc==0 && substr(txt,i,i)%in%c("(")) {      bc <- bc+1
    } else if (qc==0 && substr(txt,i,i)%in%c(")")) {      bc <- bc-1 }
    if(        qc==0 && substr(txt,i,i)%in%c("[")) {      bc <- bc+1
    } else if (qc==0 && substr(txt,i,i)%in%c("]")) {      bc <- bc-1 }
    
    if(qc==0 && bc==0 && substr(txt,i,i)==",") {
      substr(txt,i,i) <- ";"
    }
  }
  write.table(txt,'clipboard',quote=FALSE,col.names=FALSE,row.names=FALSE,eol=""); cat("Converted string is in clipboard. Use Ctrl+V:\n",txt,"\n",sep="")
}

save.packages <- function(){
  # This function saves all installed R-Packages to a file.
  # Use function recover.R.installation() to recover all packages.
  pkg_list <- rownames(installed.packages()) #installed.packages()[is.na(installed.packages()[ , "Priority"]), 1]
  save(pkg_list, file=paste0(Sys.getenv("TMP"),"/R_Migration_Package_List.Rdata") )
}

recover.R.installation <- function(){
  # This function loads the names of all previously installed packages (that were saved by function save.packages()) and installs those packages.
  # Afterwards the Rprofile.site is edited, such that my own functions are loaded automatically when starting R.
  load(paste0(Sys.getenv("TMP"),"/R_Migration_Package_List.Rdata"))
  install.packages(pkg_list)
  cat("Packages installed!\n")
  
  #txt <- scan(paste0(R.home("etc"),"/Rprofile.site"), what=character())
  if(.onHpdaPc()){
    txt <- readLines(paste0(R.home("etc"),"/Rprofile.site"))
    txt <- txt[txt!=""]
    addtxt <- "fortunes::fortune(); source('//evdad.admin.ch/AGROSCOPE_OS/2/5/2/1/3/9/4278/hpda/R/func/func.R')"
    if(txt[length(txt)]!=addtxt){
      write.table(c(txt,addtxt), paste0(R.home("etc"),"/Rprofile.site"), quote=FALSE, col.names=FALSE, row.names=FALSE, append=TRUE)
      cat("Rprofile.site updated!\n")
    } else {
      cat("Rprofile.site was already up to date!\n")
    }
  } else {
    cat("Rprofile.site *NOT* updated because not Daniel's computer!\n")
  }
  
  cat(paste0("Information: If you use RStudio and encounter a warning message like\n***\nIn dir.create(tempPath, recursive = TRUE) :\ncannot create dir '\\\\evdad.admin.ch\\AGROSCOPE_OS', reason 'Permission denied'\n***",
             "\nat every start of RStudio then you must edit the file  .../RStudio/R/modules/SessionProfiler.R  and delete delete the according line."))
}

list.all.package.functions <- function(package, all.names = FALSE, pattern) {
  # List all functions of a package
  package <- deparse(substitute(package))
  ls(
    pos = paste("package", package, sep = ":"),
    all.names = all.names,
    pattern = pattern
  )
}

require.package <- function(...){
  # This function checks if a package is already installed or loaded. If not, installation is done. Then package is required.
  pkgName <- deparse(substitute(...))
  if(substr(pkgName,1,1)=="\"" && substr(pkgName,nchar(pkgName),nchar(pkgName))=="\""){
    pkgName <- substr(pkgName,2,nchar(pkgName)-1)
  }
  
  if( !paste0("package:",pkgName)%in%search() ){
    if( !pkgName%in%rownames(installed.packages()) ){
      install.packages( pkgName )
    }
    require(..., quietly=TRUE)
  }
}


vergleichslohn <- function(region=NULL, jahr=NULL){
  
  # This function returns a matrix containing the Vergleichsloehne for the ZA. Optionally, regions and years can be chosen.
  # The following line is only needed to import data.
  # t1 <- read.cb("col"); t1 <- char.cols.to.num(sapply(t1,function(x)gsub("'","",x))); colnames(t1) <- substr.rev(colnames(t1),1,4); t1 <- as.matrix(t1); dput(t1)
  
  vgl <- structure(c(42301.656, 38300.148, 35066.52, 43789.056, 39646.848, 
                     36299.52, 44800.488, 40562.604, 37137.96, 46406.88, 42017.04, 
                     38469.6, 48132.264, 43579.212, 39899.88, 50988.072, 46164.876, 
                     42267.24, 54498.336, 49343.088, 45177.12, 57116.16, 51713.28, 
                     47347.2, 58663.056, 53113.848, 48629.52, 59496, 53868, 49320, 
                     60269.448, 54568.284, 49961.16, 61320, 56328, 51996, 61626.6, 
                     56609.64, 52255.98, 62055.84, 57003.936, 52619.952, 62864.028, 
                     56749.74, 53090.796, 63678.816, 57485.28, 53778.912, 65854.2, 
                     60885, 55128.6, 67010.664, 61954.2, 56096.712, 67629.744, 62434.008, 
                     56934.072, 68230.008, 62988.156, 57439.404, 68938.56, 63084.6, 
                     58188.12, 69689.376, 63771.66, 58821.852, 71091.552, 64520.064, 
                     60204.096, 72560.964, 65853.648, 61448.472, 73279.212, 66993.936, 
                     62387.184, 73853.388, 67518.864, 62876.016, 74198.64, 66963, 
                     62587.68, 74786.352, 67493.4, 63083.424, 73712.4, 69108.396, 
                     63839.772, 74298, 69657.42, 64346.94, 74010.864, 69035.04, 66239.904, 
                     74526.876, 69516.36, 66701.736), .Dim = c(3L, 32L), .Dimnames = list(
                       c("1", "2", "3"), c("1985", "1986", "1987", "1988", "1989", 
                                           "1990", "1991", "1992", "1993", "1994", "1995", "1996", "1997", 
                                           "1998", "1999", "2000", "2001", "2002", "2003", "2004", "2005", 
                                           "2006", "2007", "2008", "2009", "2010", "2011", "2012", "2013", 
                                           "2014", "2015", "2016")))
  if(is.null(region)) region <- 1:nrow(vgl)
  if(is.null(jahr)) jahr <- 1:ncol(vgl)
  if(min(jahr)>1000) jahr <- as.character(jahr)
  
  return(vgl[region,jahr])
}

vergleichszins <- function(jahr=NULL){
  # This function returns a matrix containing the Eigenkapital-Zinssaetze for the ZA. Optionally, regions and years can be chosen.
  # The following line is only needed to import data.
  # t1 <- read.cb("col"); colnames(t1) <- substr.rev(colnames(t1),1,4); t1 <- as.matrix(t1); dput(t1)
  
  structure(c(4.53, 4.71, 4.24, 4.04, 4, 5.13, 6.4, 6.23, 6.42, 
              4.58, 4.93, 4.57, 4, 3.4, 2.81, 3.02, 3.95, 3.36, 3.22, 2.63, 
              2.73, 2.11, 2.5, 2.91, 2.93, 2.22, 1.65, 1.48, 0.66, 0.94, 0.73, 
              0, 0), .Dim = c(1L, 33L), .Dimnames = list(NULL, c("1984", "1985", 
                                                                 "1986", "1987", "1988", "1989", "1990", "1991", "1992", "1993", 
                                                                 "1994", "1995", "1996", "1997", "1998", "1999", "2000", "2001", 
                                                                 "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", 
                                                                 "2010", "2011", "2012", "2013", "2014", "2015", "2016")))
  if(is.null(jahr)) jahr <- 1:ncol(vgl)
  return(vgl[,jahr,drop=FALSE])
}

find.fun <- function(pattern){
  # This function finds all functions in the workspace that contain a certain pattern.
  allfun <- as.character( lsf.str( envir=environment(find.fun) ) )
  choose <- which(grepl(paste0(pattern,collapse="|"), allfun, ignore.case=TRUE ))
  return(allfun[choose])
}

find.obj <- function(pattern, ignore.case=TRUE){
  # This function finds all objects in the workspace that contain a certain pattern.
  allobj <- as.character( ls( envir=environment(find.fun) ) )
  choose <- grep(paste0(pattern,collapse="|"), allobj , ignore.case=ignore.case)
  return(allobj[choose])
}

#x <- as.data.frame(matrix(1:100, ncol=10))
printzeros <- function(x, zero.sign=".") {
  x[x==0] <- NA
  print(x, na=zero.sign)
}

# http://adv-r.had.co.nz/Functions.html#special-calls
#`%+%` <- function(a, b) paste0(a, b)
#`% %` <- function(a, b) paste (a, b)
#pc <- function(...) paste0(..., collapse=", ") # Try this: # "a" % % "b" % % pc(1:10)

l <- match.fun(length)
# http://adv-r.had.co.nz/Functions.html#replacement-functions
cn <- match.fun(colnames);# `cn<-` <- `colnames<-`
rn <- match.fun(rownames);# `rn<-` <- `rownames<-`
dn <- match.fun(dimnames);# `dn<-` <- `dimnames<-`
nc <- match.fun(ncol)
nr <- match.fun(nrow)
su <- function(x) sort(unique(x))
# x <- data.frame(a=as.factor(c("b","c")),b=1:2,c=c("a","z"),stringsAsFactors=FALSE)

h <- function(x, n=6) {
  
  if(is.null(dim) || is.matrix(x))(return(head(x,n)))
  if(n>dim(x)[1]) n <- dim(x)[1]
  
  if(length(dim(x))==3) {
    return(x[1:n,,])
    
  } else {
    if(is.data.frame(x)) {
      cn1 <- colnames(x)
      rn1 <- rownames(x)
      x <- as.data.frame(rbind(paste0("<", substr( sapply(x[1,,drop=FALSE],function(x)class(x)), 1,3), ">"),
                               as.matrix(x[1:n,,drop=FALSE])
      ),
      stringsAsFactors=FALSE)
      colnames(x) <- cn1
      rownames(x) <- c("-",rn1[1:n])
      return(x)
    } else {
      return(head(x,n))
    }
  }
}

ch <- function(x){
  # Look shortly at the most important properties of a matrix / data.frame
  print(head(x))
  cat("\ncolnames\n")
  print(colnames(x)); cat("\n")
  cat(paste("nrow:\t",nrow(x),"\n"))
  cat(paste("ncol:\t",ncol(x)))
}
####

naF <- function(x){
  x[is.na(x)] <- FALSE
  return(x)
}
naT <- function(x){
  x[is.na(x)] <- TRUE
  return(x)
}
na0 <- function(x){
  x[is.na(x)] <- 0
  return(x)
}



#### Funktionen, mit denen einfach die Tabellennr. Spaltennr. Zeilennr. etc. aus den Spalten?berschriften des Merkmals
#### Katalogs rausgelesen werden k?nnen.

# REFERENZBETRIEBE
MKtab <- function(string) {if(is.null(dim(string))) substr(string,1,4) else substr(colnames(string),1,4)}
MKspalte <- function(string) {if(is.null(dim(string))) substr(string,6,9) else substr(colnames(string),6,9)}
MKzeile <- function(string) {if(is.null(dim(string))) substr(string,11,15) else substr(colnames(string),11,15)}

sort.MK <- function(data, order=c("zeile","spalte")){
  order <- match.arg(order)
  cn.data <- colnames(data)
  change_vec <- substr(cn.data,1,1)=="M" & nchar(cn.data)>=4 & !is.na(suppressWarnings(as.numeric(substr(cn.data,2,4))))
  data.keep <- data[ , !change_vec, drop=FALSE ]
  data.change <- data[ , change_vec, drop=FALSE ]
  cn.data.change <- colnames(data.change)
  if(order=="zeile") {
    return( cbind( data.keep, data.change[, order(MKtab(cn.data.change),MKzeile(cn.data.change),MKspalte(cn.data.change)) ]) )
  } else {
    return( cbind( data.keep, data.change[, order(MKtab(cn.data.change),MKspalte(cn.data.change),MKzeile(cn.data.change)) ] ) )
  }
}
sort.MK.colnames <- function(data, order=c("zeile","spalte")){
  order <- match.arg(order)
  cn.data <- colnames(data)
  change_vec <- substr(cn.data,1,1)=="M" & nchar(cn.data)>=4 & !is.na(suppressWarnings(as.numeric(substr(cn.data,2,4))))
  cn.keep <- cn.data[!change_vec]
  cn.change <- cn.data[change_vec]
  if(order=="zeile") {
    return( c(cn.keep, cn.change[ order(MKtab(cn.change), MKzeile(cn.change), MKspalte(cn.change)) ] ) )
  } else {
    return( c(cn.keep, cn.change[ order(MKtab(cn.change), MKspalte(cn.change),MKzeile(cn.change)) ] ) )
  }
}

# System ZA2015
MKtab <- function(string) {if(is.null(dim(string))) substr(string,1,4) else substr(colnames(string),1,4)}
nMKtab <- function(string){
  if(is.null(dim(string))) x <- substr(string,2,4) else x <- substr(colnames(string),2,4)
  x <- suppressWarnings(as.numeric(MKcol(string))); x[is.na(x)] <- -1; return(x)
}
MKcol <- function(string) {if(is.null(dim(string))) substr(string,6,9) else substr(colnames(string),6,9)}
nMKcol <- function(string){ x <- suppressWarnings(as.numeric(MKcol(string))); x[is.na(x)] <- -1; return(x) }
MKrow <- function(string) {if(is.null(dim(string))) substr(string,11,15) else substr(colnames(string),11,15)}
nMKrow <- function(string){ x <- suppressWarnings(as.numeric(MKrow(string))); x[is.na(x)] <- -1; return(x) }


MKsort <- function(data, order=c("zeile","spalte")){
  order <- match.arg(order)
  cn.data <- colnames(data)
  change_vec <- substr(cn.data,1,1)=="P" & nchar(cn.data)>=4 & !is.na(is.numeric(substr(cn.data,2,4)))
  data.keep <- data[ , !change_vec, drop=FALSE ]
  data.change <- data[ , change_vec, drop=FALSE ]
  cn.data.change <- colnames(data.change)
  if(order=="zeile") {
    return( cbind( data.keep, data.change[, order(MKtab(cn.data.change),MKrow(cn.data.change),MKcol(cn.data.change)) ]) )
  } else {
    return( cbind( data.keep, data.change[, order(MKtab(cn.data.change),MKcol(cn.data.change),MKrow(cn.data.change)) ] ) )
  }
}
MKsort.colnames <- function(data, order=c("zeile","spalte")){
  order <- match.arg(order)
  cn.data <- colnames(data)
  change_vec <- substr(cn.data,1,1)=="P" & nchar(cn.data)>=4 & !is.na(is.numeric(substr(cn.data,2,4)))
  cn.keep <- cn.data[!change_vec]
  cn.change <- cn.data[change_vec]
  if(order=="zeile") {
    return( c(cn.keep, cn.change[ order(MKtab(cn.change), MKrow(cn.change), MKcol(cn.change)) ] ) )
  } else {
    return( c(cn.keep, cn.change[ order(MKtab(cn.change), MKcol(cn.change),MKrow(cn.change)) ] ) )
  }
}

paste.cols <- function(dat, cols=NULL, sep="_") {
  # Paste Values of columns of a data frame
  if(is.null(dim(dat))) return(dat)
  if(is.matrix(dat)) dat <- as.data.frame(dat)
  if(is.null(cols)) cols <- colnames(dat)
  return( eval(parse(text= paste0( "paste(", paste( paste0("dat[,'",cols,"']"), collapse=", "), ", sep='",sep,"')") )) )
}

paste.IDJahr <- function(dat){
  return(paste.cols(dat=dat, cols=c("ID","Jahr"), sep="_"))
}

wait <- function(secs) {
  Sys.sleep(secs)
}

try <- function(...) {
  tryCatch(..., error=function(e)e,warning=function(w)w)
}

rm.gc.keep <- function(keep){
  # This function removes and garbage collects all objects in the current environment (can also be within a function)
  # Except for the variables defined in keep (charcter vector).
  
  rm1 <- ls(envir=parent.frame())
  rm1 <- rm1[!rm1%in%keep]
  rm(list=rm1, envir=parent.frame())
  invisible(gc())
}


#### CHANGE OBJECT STRUCUTRE ####

# array <- array(0, dim=c(3,4,5), dimnames=list(c("c","b","a"),c(4,3,2,1),c("z","y","x","v","u")))
# array <- matrix(0, ncol=3, nrow=3); colnames(array) <- c("a","b","c");
c.dimnames <- function(array, sep.sign="_"){
  # This function brings the dimnames of an array into the array itself
  # Example
  # , , x
  #   a       b       c
  # 1 "1_a_x" "1_b_x" "1_c_x"
  # 2 "2_a_x" "2_b_x" "2_c_x"
  # 3 "3_a_x" "3_b_x" "3_c_x"
  
  if(is.null(dim(array))) stop("Input has to be an array, not vector.")
  
  dn1 <- dimnames(array)
  dn1.1 <- dn1[[1]]; if(is.null(dn1.1)) dn1.1 <- rep("",dim(array)[[1]])
  dn1.2 <- dn1[[2]]; if(is.null(dn1.2)) dn1.2 <- rep("",dim(array)[[2]])
  
  res0 <- paste( rep(dn1.1, length(dn1.2)) , rep(dn1.2, each=length(dn1.1)) , sep=sep.sign)
  if(length(dn1)>2){
    for(i in 3:length(dn1)){
      dn1.1 <- res0
      dn1.2 <- dn1[[i]]; if(is.null(dn1.2)) dn1.2 <- rep("",dim(array)[[i]])
      res0 <- paste( rep(dn1.1, length(dn1.2)) , rep(dn1.2, each=length(dn1.1)) , sep=sep.sign)
    }
  }
  res1 <- array
  res1[] <- res0
  return(res1)
}

# x <- data.frame(testCol=1:10); y <- data.frame(testCol2=1:5); dims=c("rowcol","row","col")[3]; d1imnames.to.mat(x, dims=dims); m1erge.matrices(x,y,integrate.dimnames="col")
dimnames.to.mat <- function(x, dims=c("rowcol","row","col")){
  # Diese Funktion integriert die Spalten- und Reihennamen einer Matrix in die Matrix selbst. Also quasi als Werte.
  # WICHTIG: Dies Funktion wird gebraucht fuer Funktion merge.matrices()
  # Arguments
  # x     = The matrix into which the dimnames should be integrated.
  # dims  = Which dimnames should be integrated? row&colnames? only rownames? only colnames?
  
  dims <- match.arg(dims)
  #if(is.null(dim(x))) x <- as.matrix(x)
  x <- as.matrix(x)
  if(is.null(dimnames(x))) return(x)
  if(is.null(rownames(x))) rownames(x) <- rep("",nrow(x))
  if(is.null(colnames(x))) colnames(x) <- rep("",ncol(x))
  if(dims=="rowcol") res <- rbind( c("", colnames(x)), cbind(rownames(x),x) )
  if(dims=="row") res <- cbind(rownames(x),x)
  if(dims=="col") res <- rbind(colnames(x),x)
  dimnames(res) <- NULL
  return(res)
}

#x <- data.frame(testCol=1:10, testC=10:1); y <- data.frame(testCol2=1:5, testCol3=2:6, testcol4=11:15); z <- 1:3; func=c("rbind","cbind")[1]; fill=""; nbreak=1; aligned=c("left","right")[1]; integrate.dimnames=c("no","rowcol","row","col")[1]
#li <- list(x,y,z) ;m1erge.matrices(x,y,z, integrate.dimnames = "rowcol", nbreak=1, func="cbind")
merge.matrices <- function(..., fill="", nbreak=0, aligned=c("left","right"), integrate.dimnames=c("no","rowcol","row","col"), func=c("rbind","cbind")) {
  # Diese Funktion vergleicht die Anzahl Spalten aller gegebenen Matrizen, gleicht sie an und verbindet alle Matrizen in einer einzigen.
  #
  # Arguments
  # Mit fill kann gewaehlt werden, was fuer ein Zeichen fuer das Auffuellen der zusaetzlichen Spalten verwendet wird.
  # nbreak gibt an, wie viele Zeilen zwischen zwei Ursprungs-Matrizen eingefuegt werden.
  # Mit integrate.dimnames kann man die dimnames in die End-Matrix integrieren, was mittels dimnames.to.mat() geschieht.
  # func: Funktion mit welcher die Matrizen zuammengefuehrt werden sollen.
  
  func <- match.arg(func)
  aligned <- match.arg(aligned)
  integrate.dimnames <- match.arg(integrate.dimnames)
  
  li <- list(...)
  li <- lapply(li,function(x){
    if(is.null(x)) x <- fill
    if(is.null(dim(x))) {return(t(as.matrix(x)))} else return(x)
  })
  if(integrate.dimnames!="no") li <- lapply(li, function(x) dimnames.to.mat(x, dims=integrate.dimnames))
  ncolmax <- max(unlist(lapply(li,function(x) ncol(x) )))
  
  tryCatch(
    res <- do.call("rbind",
                   lapply( li,function(x) {
                     if(ncol(x)<ncolmax) {
                       if(aligned=="left") {
                         x <- cbind(x,array(fill,dim=c(nrow(x),ncolmax-ncol(x))))
                       } else {
                         x <- cbind(array(fill,dim=c(nrow(x),ncolmax-ncol(x))),x)
                       }
                     }
                     if(nbreak==0) x else rbind(x,matrix(fill,nrow=nbreak,ncol=ncol(x)))
                   }))
    ,error=function(e)stop("If different colnames are provided, you must choose integrate.names=\"col\" or \"rowcol\" "))
  if(nbreak>0) res <- res[ -c((nrow(res)-nbreak+1):nrow(res)), , drop=FALSE]
  if(func=="cbind") res <- t(res)
  return(res)
}

if(FALSE){
  x <- array(0, dim=c(5,5,2), dimnames=list(c("asdf1","asdf2","asdf3","asdf4","asdf5"),c("asdf1","asdf2","asdf3","asdf4","asdf5"),c("dim3.1", "dim3.2")))
  #dimnames(x)[[3]] <- NULL
  #dimnames(x) <- NULL
  sep.sign=NA; sep.line=FALSE; keep.colnames=FALSE; keep.dim3names=FALSE
  d1im3.to.mat(x, sep.line=TRUE, sep.sign=NA, keep.colnames=TRUE, keep.dim3names=TRUE)
}

dim3.to.mat <- function(x, sep.line=TRUE, sep.sign=NA, keep.colnames=TRUE, keep.dim3names=TRUE){
  # This function converts an array with 3 dimension to a matrix (with 2 dimensions).
  # This is especially useful if you want to export a 3 dimensional array into a csv file.
  # The matrix shows the third dimension of the original array row by row as 'sub-matrices'.
  #
  # x = array to be converted. (array with 3 dimensions)
  # sep.line = Should the sub-matrices be separated by the sep.sign? (logical)
  # sep.sign = The sign to separate the sub-matrices (character)
  # keep.colnames = Should the colnames be written into the result matrix as pseudo colnames above each sub-matrix? (logical)
  # keep.dim3names = Should the dimnames of the 3rd dimension be written into the rownames of the result matrix? (logical)
  
  if(length(dim(x))!=3) stop("x must be an array with 3 dimensions.")
  
  di <- dim(x)
  cnx <- colnames(x)
  ncx <- ncol(x)
  
  if(keep.dim3names & !keep.colnames) sep.line <- TRUE
  if(is.null(colnames(x))) keep.colnames <- FALSE
  if(is.null(rownames(x))) rownames(x) <- 1:nrow(x)
  if(is.null(dimnames(x)[[3]]) & keep.dim3names) dimnames(x)[[3]] <- 1:dim(x)[3]
  
  # Transform colnames to numeric if there are no letters. If there were letters, it would result in a warning "NAs introduced by coercion"
  if( !is( tryCatch(as.numeric(cnx),error=function(e)e,warning=function(w)w), "warning") ) cnx <- as.numeric(cnx)
  
  res <- NULL
  for(i in 1:dim(x)[3]){
    res <- rbind( res, if(sep.line)rep(sep.sign ,ncx), if(keep.colnames)cnx,  x[,,i])
  }
  if(sep.line & !(keep.dim3names&!keep.colnames)) res <- res[-1,]
  
  if(keep.dim3names){
    if(!sep.line &  keep.colnames) places <- seq(1, nrow(res), 1+dim(x)[1])
    if( sep.line &  keep.colnames) places <- seq(1, nrow(res), 2+dim(x)[1])
    if( sep.line & !keep.colnames) places <- seq(1, nrow(res), 1+dim(x)[1])
    rownames(res)[places] <- dimnames(x)[[3]]
  }
  
  return(res)
}

rep.1b1 <- function(vector,times){
  if(length(times)==1) {
    return(rep(vector,each=times))
    #return(as.vector(   apply(matrix(vector,ncol=1),1,function(x)rep(x,times))   ))
  } else {
    if(length(vector)!=length(times)) stop("If length(times)>1 then condition length(vector)==length(times) must hold.")
    return(rep(vector,times))
    #return(unlist(   apply(matrix(c(vector,times),ncol=2),1,function(x)rep(x[1],x[2])) ))
  }
}
# Performance-Vergleich zwischen mapply und apply. Apply ist deutlich schneller.
# vector <- 1:10e3
# times <- 1:10e3
# system.time(t1 <- mapply(function(vector,times)rep(vector,times), vector, times) )
# system.time(t1 <- apply(matrix(c(vector,times),ncol=2),1,function(x)rep(x[1],x[2])) )

if(FALSE){
  x <- data.frame(c("a","b","c"),1:3)
  times <- 6
  rep.rows(x,times)
  rep.rows.1b1(x,c(5,4,3))
}
rep.rows <- function(x, times){
  if(length(times)>1) stop("length(times) must be equal 1, else repetition is done one by one.")
  return(x[rep(1:nrow(x),times),,drop=FALSE])
}
rep.rows.1b1 <- function(x, times){
  return(x[rep.1b1(1:nrow(x),times),,drop=FALSE])
}


if(FALSE) rep.rows_OLD_DELETE <- function(x, times){
  return( eval(parse(text=paste0("rbind(",paste0(rep("x",times),collapse=","),")"))) )
}
if(FALSE) rep.rows.1b1_OLD_DELETE <- function(x, times) {
  res <- eval(parse(text=paste0("rbind(",paste0(rep("x",times),collapse=","),")")))
  if(nrow(x)>1) newo <- order( rep(1:nrow(x), times) ) else newo <- 1:times
  res <- res[newo,,drop=FALSE]
  rownames(res) <- NULL
  return(res)
}


c.1b1 <- function(..., add.names=c("char","num","obj.names","own.names","none"), own.names=NULL, names.at.front=FALSE, sep.sign="_") {
  # This function concenates given vectors ... element by element
  # See function description of rbind.1b1 for further information on arguments
  
  # a <- 1:10
  # names(a) <- LETTERS[1:10]
  # b <- c <- a
  # dat <- list(a,b,c)
  # c.1b1(a,b,c,add.names=c("char","num","obj.names","own.names","none")[5],own.names=c("x","y","z"),names.at.front=TRUE,sep.sign="_")
  
  add.names <- match.arg(add.names)
  
  # Create List and delete NULL elements.
  dat <- list(...)
  dat <- dat[ sapply(dat,function(x)!is.null(x)) ]
  
  # Test if all length is the same for all arguments.
  n.arg <- length(dat)
  length.arg.logical <- logical()
  for(i in 1:n.arg) length.arg.logical[[i]] <- length(dat[[1]]) != length(dat[[i]])
  if(any(length.arg.logical)) stop("length of all arguments must be equal!")
  length.dat <- length(dat[[1]])
  
  # Concenate in the wrong order
  res <- do.call("c", dat)
  # Now order correctly
  res <- res [order(rep(1:length.dat, n.arg))]
  
  if(add.names=="char") {
    if(!names.at.front) names(res) <- paste0(names(res), sep.sign, rep(letters[1:n.arg],length.dat))
    if( names.at.front) names(res) <- paste0(rep(letters[1:n.arg],length.dat), sep.sign, names(res))
  } else if(add.names=="num") {
    if(!names.at.front) names(res) <- paste0(names(res), sep.sign, rep(1:n.arg,length.dat))
    if( names.at.front) names(res) <- paste0(rep(1:n.arg,length.dat), sep.sign, names(res))
  } else if(add.names=="obj.names") {
    name <- as.list(substitute(list(a,b,c)))[-1L]
    obj.names <- NULL
    for(i in 1:length(name)) obj.names <- c(obj.names, as.character(name[[i]]))
    if(!names.at.front) names(res) <- paste0(names(res), sep.sign, rep(obj.names,length.dat))
    if( names.at.front) names(res) <- paste0(rep(obj.names,length.dat), sep.sign, names(res))
  } else if(add.names=="own.names"){
    if(is.null(own.names)) stop("specify own.names!")
    if(length(own.names)!=n.arg) stop("length(own.names) must be equal the number of objects to bind!")
    if(!names.at.front) names(res) <- paste0(names(res), sep.sign, rep(own.names,length.dat))
    if( names.at.front) names(res) <- paste0(rep(own.names,length.dat), sep.sign, names(res))
  }
  return(res)
}

####

rbind.1b1 <- function(..., add.names=c("char","num","obj.names","own.names","none"), own.names=NULL, names.at.front=FALSE, sep.sign="_", cbind=FALSE) {
  # This function binds all given matrices in ... row by row.
  # add.names adds endings to the rownames such that you can see which row comes from which object.
  # sep.sign separates the original rownames from the add.names-endings
  # if add.names="own.names" then own.names must be given.
  # cbind is only used for the cbind.1b1 function (below) which is some kind of wrapper function for rbind.1b1
  
  #a <- b <- c <- matrix(1:100,nrow=10,dimnames=list(letters[1:10],letters[11:20]) )
  #dat <- list(a,b,c)
  #add.names <- c("char","num","obj.names","own.names","none")[5]; sep.sign="_"; own.names <- c("x","y","z")
  #rbind.1b1(a,b,c,add.names=c("char","num","obj.names","own.names","none")[5],own.names=own.names,names.at.front=TRUE)
  #cbind.1b1(a,b,c,add.names=c("char","num","obj.names","own.names","none")[5],own.names=own.names,names.at.front=TRUE)
  
  add.names <- match.arg(add.names)
  # Create List and delete NULL elements.
  dat <- list(...)
  if(!is.data.frame(dat[[1]]) && is.list(dat[[1]]) && length(dat)==1) dat <- dat[[1]]
  dat <- dat[ sapply(dat,function(x)!is.null(x)) ]
  if(cbind) dat <- lapply(dat,function(x)t(x))
  
  # Test if all nrow is the same for all arguments.
  n.arg <- length(dat)
  nrow.arg.logical <- logical()
  for(i in 1:n.arg) nrow.arg.logical[[i]] <- nrow(dat[[1]]) != nrow(dat[[i]])
  if(any(nrow.arg.logical)) stop("nrow of all arguments must be equal!")
  nrow.dat <- nrow(dat[[1]])
  
  # Test if all ncol is the same for all arguments.
  ncol.arg.logical <- logical()
  for(i in 1:n.arg) ncol.arg.logical[[i]] <- ncol(dat[[1]]) != ncol(dat[[i]])
  if(any(ncol.arg.logical)) stop("ncol of all arguments must be equal!")
  ncol.dat <- ncol(dat[[1]])
  
  # Bind the rows with wrong order
  res <- do.call("rbind", dat)
  # Now order correctly
  res <- res[order(rep(1:nrow.dat, n.arg)),,drop=FALSE]
  # Get original rownames and order correctly.
  rn_res <- unlist(lapply(dat, function(x)rownames(x)))
  rn_res <- rn_res[order(rep(1:nrow.dat, n.arg))]
  
  if(add.names=="char") {
    if(!names.at.front) rownames(res) <- paste0(rn_res, sep.sign, rep(letters[1:n.arg],nrow.dat))
    if( names.at.front) rownames(res) <- paste0(rep(letters[1:n.arg],nrow.dat), sep.sign, rn_res)
  } else if(add.names=="num") {
    if(!names.at.front) rownames(res) <- paste0(rn_res, sep.sign, rep(1:n.arg,nrow.dat))
    if( names.at.front) rownames(res) <- paste0(rep(1:n.arg,nrow.dat), sep.sign, rn_res)
  } else if(add.names=="obj.names") {
    name <- as.list(substitute(list(a,b,c)))[-1L]
    obj.names <- NULL
    for(i in 1:length(name)) obj.names <- c(obj.names, as.character(name[[i]]))
    if(!names.at.front) rownames(res) <- paste0(rn_res, sep.sign, rep(obj.names,nrow.dat))
    if( names.at.front) rownames(res) <- paste0(rep(obj.names,nrow.dat), sep.sign, rn_res)
  } else if(add.names=="own.names"){
    if(is.null(own.names)) stop("specify own.names!")
    if(length(own.names)!=n.arg) stop("length(own.names) must be equal the number uf rbind arguments")
    if(!names.at.front) rownames(res) <- paste0(rn_res, sep.sign, rep(own.names,nrow.dat))
    if( names.at.front) rownames(res) <- paste0(rep(own.names,nrow.dat), sep.sign, rn_res)
  }
  return(res)
}

####

cbind.1b1 <- function(...){
  return(t( rbind.1b1(...,cbind=TRUE)) )
}

upside.down <- function(x){
  return(x[nrow(x):1,])
}
left.to.right <- function(x){
  return(x[,ncol(x):1])
}
right.to.left <- match.fun(left.to.right)

if(FALSE){
  what <- 1:10; inobject <- matrix(1:100,nrow=10); where <- 9:10; how <- "rbind"
  insert(what,inobject,where,how)
  what <- "X"; inobject <- 1:10; where <- 9:10; how <- "c"
  insert(what,inobject,where,how)
  what <- list("x","y","z"); inobject <- list(1,2,3,4,5,6,7,8,9,10); where <- 9:10; how <- "list"
  insert(what,inobject,where,how)
}
insert <- function(what, inobject, where, how=c("c","list","rbind","cbind")){
  how <- match.arg(how)
  where <- rev(sort(where))
  if(is.list(inobject) & !is.data.frame(inobject)) how <- "list" else if(is.null(dim(inobject))) how <- "c"
  if(how=="c") {
    for(ii in 1:length(where)){
      if(where[[ii]]==1) {
        inobject <- c(what, inobject[1:length(inobject)]      )
        #} else if(where[[ii]]==length(inobject)) {
        #  inobject <- c(      inobject[1:length(inobject)], what)
      } else {
        inobject <- c(inobject[1:(where[[ii]]-1)], what, inobject[where[[ii]]:length(inobject)])
      }
    }
    return(inobject)
    
  } else if(how=="list") {
    for(ii in 1:length(where)) {
      newlist <- list()
      #if(where[[ii]]%in%c(1,length(inobject))) stop("where cannot be 1 or length(inobject)")
      if(where[[ii]]==1){
        #if(is.list(what)) {
        #    for(i in 1:length(what)) newlist[[i]] <- what[[i]]
        #    for(i in 1:length(inobject)) newlist[[i+length(what)]] <- inobject[[i]]
        #  } else {
        newlist[[1]] <- what
        for(i in 1:length(inobject)) newlist[[i+1]] <- inobject[[i]]
        #  }
        inobject <- newlist
        
        #} else if(where[[ii]]==length(inobject)){
        #newlist <- inobject
        ##if(is.list(what)) {
        ##  for(i in 1:length(what)) newlist[[i+length(inobject)]] <- what[[i]]
        ##} else {
        #newlist[[length(inobject)+1]] <- what
        ##}
        #inobject <- newlist
        
      } else {
        for(i in 1:(where[ii]-1)) newlist[[i]] <- inobject[[i]]
        #if(is.list(what)){
        #  for(i in     1:length(what)    ) newlist[[i+(where[ii]-1)]]    <- what[[i]]
        #  for(i in where[ii]:length(inobject)) newlist[[i+length(what)]] <- inobject[[i]]
        #} else {
        newlist[[where[ii]]] <- what
        for(i in where[ii]:(length(inobject))) newlist[[i+1]] <- inobject[[i]]
        #}
        inobject <- newlist
      }
    }
    return(newlist)
    
  } else if(how=="rbind") {
    for(ii in 1:length(where)){
      if(where[[ii]]==1) {
        inobject <- rbind( what, inobject[1:nrow(inobject),,drop=FALSE] )
        rownames(inobject)[1] <- ""
        #} else if (where[[ii]]==nrow(inobject)) {
        #  inobject <- rbind(       inobject[1:nrow(inobject),,drop=FALSE], what )
      } else {
        inobject <- rbind( inobject[1:(where[ii]-1),,drop=FALSE], what, inobject[where[ii]:nrow(inobject),,drop=FALSE] )
        rownames(inobject)[where[[ii]]] <- ""
      }
    }
    return(inobject)
    
  } else if(how=="cbind") {
    for(ii in 1:length(where)){
      if(where[[ii]]==1) {
        inobject <- cbind( what, inobject[,1:nrow(inobject),drop=FALSE]        )
        colnames(inobject)[1] <- ""
        #} else if (where[[ii]]==ncol(inobject)) {
        #  inobject <- cbind(       inobject[,1:nrow(inobject),drop=FALSE], what  )
      } else {
        inobject <- cbind( inobject[,1:(where[ii]-1),drop=FALSE], what, inobject[,where[ii]:ncol(inobject),drop=FALSE] )
        colnames(inobject)[where[[ii]]] <- ""
      }
    }
    return(inobject)
    
  }
}

#### SUMMARIES & MEANS ####

trim <- function(x, probs=c(0.025,0.975)) {
  # This function trims a distribution and gives back only those values that are within the trimming limits.
  # Arguments:
  # x     = vector of values
  # probs = probabilities to exclude extreme values using the quantlie() function. Both quantile probabilities are included like: prob1[1] <= x <= probs[2]
  
  if(length(probs)!=2) stop("probs must be a vector of length 2")
  q1 <- quantile(x, sort(probs), na.rm=TRUE)
  return(x[ which(x>=q1[1] & x<=q1[2]) ]) # Use which to exclude NA values.
}

summarysd <- function(x,na.rm=TRUE,digits=2,...) {
  if(is.null(x)) {
    result <- rep(NA,7);  names(result) <- c("Min.", "1st Qu.","Media","Mean","3rd Qu.","Max.","SD")
    return(result)
  } else if(length(x)==0) {
    result <- rep(NA,7)
    names(result) <- c("Min.", "1st Qu.","Media","Mean","3rd Qu.","Max.","SD")
    return(result)
  } else if(all(is.na(x))) {
    result <- rep(NA,7)
    names(result) <- c("Min.", "1st Qu.","Media","Mean","3rd Qu.","Max.","SD")
    return(result)
  } else {
    return(round(c(summary(x,na.rm=na.rm,...),"SD"=sd(x,na.rm=na.rm,...))),digits=digits)
  }
}
####
summaryna <- function(x, ...) {
  # Recursive function definition in case of matrix or data.frame.
  if(is.matrix(x)) {
    return(apply(x,2,function(x)summaryna(x, ...)))
  } else if (is.data.frame(x)) {
    return(sapply(x,function(x)summaryna(x, ...)))
  }
  # This is the actual function.
  sum <- summary(x, ...)
  if(length(sum)<7)    sum <- c(sum,"NA's"=0)
  return(sum)
}

####
renumber <- function(x){
  # Integrated function for clarity
  replace.values <- function(search, replace, x){
    return(replace[ match(x, search) ])
  }
  # Replacement here
  ux <- unique(x)
  return(replace.values(ux, 1:length(ux), x))
}

####
if(FALSE){
  data <- cbind(1:1000, 100:1099)
  #data <- as.data.frame(data)
  colnames(data) <- c("asbasdf","asdfgawer")
  data <- data.frame(asbasdf=1:1000, asdfgawer=100:1099, rep("a", 1000))
  
  weights <- rnorm(1000,1,0); weights[weights<0] <- 0
  index <- list(sample(c(1,2),1000,replace=TRUE))
  #index <- list(sample(c(1,2),1000,replace=TRUE), sample(c(3,4),1000,replace=TRUE))
  #index <- NULL
  na.rm <- TRUE
  summary.long(data)
  summary.long(data[,1,drop=FALSE])
  
  x <- data; quant=10;digits=2;na.rm=TRUE;margin=2;reverse=FALSE
}
summary.long <- function(x,quant=10,digits=2,na.rm=TRUE,margin=2,reverse=FALSE,...) {
  # Gives a "long" summary with 11 quantiles from 0 to 1 (default).
  # margin is only used when a dataframe/matrix is given. Then apply() is used.
  # See also: describe()
  
  if(!is.null(dim(x))) {
    if(is.matrix(x))     return( apply(x,2,function(x)summary.long(x,quant=quant,digits=digits,na.rm=na.rm,margin=2,reverse=TRUE, ...))  )
    if(is.data.frame(x)) return( as.data.frame( lapply(x,function(x)summary.long(x,quant=quant,digits=digits,na.rm=na.rm,margin=2,reverse=TRUE, ...)) ,stringsAsFactors=FALSE) )
  }
  if(is.list(x)) return(lapply(x,function(x)summary.long(x,quant=quant,digits=digits,na.rm=na.rm,margin=2,reverse=TRUE, ...)))
  
  if(!is.numeric(x)) x <- rep(0, length(quant+1))
  
  result <- numeric()
  quants <- seq(0,1,length.out=quant+1)
  if(reverse) quants <- rev(quants)
  result <- round( quantile(x=x, probs=quants, na.rm=na.rm, ...), digits=digits)
  names(result) <- paste0(round(100*quants,digits=2), "%")
  return(result)
}

summary.quart <- function(x,digits=2,na.rm=TRUE,margin=2,reverse=FALSE,...){
  # Gives a specified summary with Quantiles: Min., 5%, 25%, Median, Mean, 75%, 95%, Max
  # margin is only used when a dataframe/matrix is given. Then apply() is used.
  
  if(!is.null(dim(x))) {
    if(is.matrix(x))     return( apply(x,2,function(x)summary.quart(x,digits=digits,na.rm=na.rm,margin=margin,reverse=TRUE,...))  )
    if(is.data.frame(x)) return( as.data.frame( lapply(x,function(x)summary.quart(x,digits=digits,na.rm=na.rm,margin=margin,reverse=TRUE,...)) ,stringsAsFactors=FALSE) )
  }
  if(is.list(x)) return(lapply(x,function(x)summary.quart(x,digits=digits,na.rm=na.rm,margin=margin,reverse=TRUE,...)))
  
  if(!is.numeric(x)) x <- rep(0, length(quant+1))
  
  result <- numeric()
  
  probs <- c(0, 0.05, 0.25, 0.5, 0.75, 0.95, 1)
  result <- round( quantile(x=x, probs=probs, na.rm=na.rm, ...), digits=digits)
  result <- c(result[1:4], "Mean"=mean(x, na.rm=na.rm), result[5:7])
  names(result) <- c("Min.","5%","25%","Median","Mean","75%","95%","Max.")
  #names(result) <- paste0(round(100*probs,digits=2), "%")
  
  if(reverse) result <- rev(result)
  
  return(result)
}

####
if(FALSE){
  # Beispieldaten vorbereiten
  data <- data.frame(1:1000, 100:1099, NA)
  colnames(data) <- c("Variable_1","Variable_2", "I(Variable_1/Variable_2)")
  data[,"Variable_1_pro_Variable_2"] <- data[,"Variable_1"]/ data[,"Variable_2"]
  na.rm=TRUE; digits=2
  weights <- rnorm(1000,40,20); weights[weights<0] <- 0
  
  # Rechnung mit keinem Index
  index <- NULL
  mean.weight(data,weights,index)
  
  # Rechnung mit 1-Dimensionalem Index (koennte z.B. das Jahr sein)
  index <- list(sample(c(2012,2013),1000,replace=TRUE))
  mean.weight(data,weights,index)
  
  # Rechnung mit 2-Dimensionalem Index z.B. Jahr x Region
  index <- list(sample(c("1_Tal","2_Huegel","3_Berg"),1000,replace=TRUE), sample(c(2012,2013),1000,replace=TRUE))
  mean.weight(data,weights,index)
  
  # Rechnung ohne Index
  index <- NULL
  mean.weight(data,weights,index)
  
  gb <- load.gb()
  gb[,c("I((LE-eigenZinsanspruch)/JAE_FamAK)","I(LE/LN)","_",".")] <- 0
  mean.weight(data=gb[,c("I((LE-eigenZinsanspruch)/JAE_FamAK)","I((LE-eigenZinsanspruch)/JAE_FamAK)","I(LE/LN)","LE","LN","eigenZinsanspruch","JAE_FamAK","_",".","I(LE/LN)")],
              weights=gb[,"Gewicht"],
              index=gb[,"Jahr"],
              edit.I.colnames=TRUE,del.I.help.columns=FALSE)
}

if(FALSE) slapply <- function(X, MARGIN=2, FUN, ...){
  if(is.matrix(X) | MARGIN!=2) {
    return(apply(X=X, MARGIN=MARGIN, FUN=FUN, ...))
  } else if (is.data.frame(X) & MARGIN==2) {
    return(sapply(X=X, FUN=FUN, ...))
  } else if (is.list(X)) {
    return(lapply(X=X, FUN=FUN, ...))
  }
}

.prepare.fixed.index.result <- function(data, index, names.result, index.sep="_", edit.I.colnames=FALSE){
  # This function is internally used in mean.weight and variance.estimate
  if(!is.null(dim(data))){
    rawResult <- tapply.fixed(X=data[,1], INDEX=index, FUN=sum, names.result=names.result, vector.result=TRUE)
    nam <- names(rawResult)
    rawResult <- matrix(NA, nrow=length(rawResult), ncol=ncol(data))
    rownames(rawResult) <- nam; 
    if(!edit.I.colnames) {
      colnames(rawResult) <- colnames(data)
    } else {
      colnames(rawResult) <- .rm.I.from.names(colnames(data))
    }
  } else {
    rawResult <- tapply.fixed(X=data, INDEX=index, FUN=sum, names.result=names.result, sep.sign=index.sep, vector.result=TRUE)
    rawResult[] <- NA 
  }
  return(rawResult)
}

#data <- as.data.frame(matrix(1:15, ncol=3)); colnames(data) <- c("I(a+b)","a","b"); weights <- 1:5; index <- as.data.frame(matrix(c(2014,2014,2014,2015,2016,   1,2,2,1,1,   11,11,12,13,13),ncol=3)); calc.sum=FALSE; digits=NULL; na.rm=TRUE; edit.I.colnames=TRUE; del.I.help.columns=FALSE; I.help.columns=NULL; fixed.index=TRUE; index.of.result=c("2014_2_11","2014_1_11","0000_0_00"); index.sep="_"
#m1ean.weight(data[,1],weights,index,fixed.index=TRUE, index.of.result=index.of.result)
mean.weight <- function(data, weights=NULL, index=NULL, fixed.index=FALSE, index.of.result=NULL, index.sep="_", calc.sum=FALSE, digits=NULL, na.rm=TRUE, edit.I.colnames=TRUE, del.I.help.columns=FALSE, I.help.columns=NULL){
  # This function calculates the weighted mean of all variables in a possibly indexed data.frame or matrix.
  
  # Arguments
  # data = data of which the weighted means should be calculated. Can be data.frame, matrix or vector
  #        If any colname of data contains an expression like I(Var_A/Var_B), then the the "weighted mean of the ratio" is calculated.
  #           This is done by building a model.matrix() of the result matrix.
  #           Use function extract.I.vars() to add all variables to your data frame that are used in the formula
  # weights =         weights for the weighted mean calculation
  # index =           index in the same structure as used in tapply(). Can be a vector or list of vectors.
  # calc.sum =        Should sum(data*weights) should be calculated, rather than weighted means?
  # digits =          digits for rounding the results
  # na.rm =           na action
  # edit.I.colnames = Should the colnames containing expressions with I() be edited, such that I() won't be there anymore? TRUE/FALSE
  
  # Wenn innerhalb eines Indexes mehrere Indexe als Listen abgelegt sind, wird die Berechnung fuer alle Indexe gemacht.
  #if(is.list(index)){
  #  if(any(sapply(index,function(x)is.list(x)))){
  #    return(do.call("rbind", lapply(index, function(x)mean.weight(data=data, weights=weights, index=x, digits=digits, na.rm=na.rm, edit.I.colnames=edit.I.colnames, del.I.help.columns=del.I.help.columns, I.help.columns=I.help.columns))))
  #  }
  #}
  
  if(is.list(data) && !is.data.frame(data)) stop("data must be matrix or data.frame but not a list.")
  
  # Fixed index ausschalten, wenn Index ein Vektor ist. Dann bringt es nichts.
  if(fixed.index && is.null(index.of.result) && !is.list(index)) stop("fixed.index & is.null(index.of.result) & !is.list(index)   -> fixed.index doesn't have any effect this way. Give index as a list!")
  
  # Im Falle, dass der index fixiert sein soll, hier die rohe Ergebnisstruktur erstellen.
  if(fixed.index){
    rawResult <- .prepare.fixed.index.result(data=data, index=index, names.result=index.of.result, edit.I.colnames=edit.I.colnames)
    index <- .paste.elements(index, sep="_", errorMsg="All indices must have same length!")
  }
  
  # Index muss eine List mit folgender Struktur sein:
  isNullIndex <- is.null(index)
  if(!is.list(index)) index <- list(index)
  
  
  
  # Im Falle, dass !is.null(dim(data)) folgt eine rekursive Funktionsdefinition!
  if(!is.null(dim(data))) {
    # Wenn !is.null(dim(data))
    # & es keinen oder nur einen Index gibt:
    if(is.null(index) || length(index)==1) {
      
      # Berechnung rekursiv fuer matrix / data.frame
      if(is.matrix(data)) {
        if(nrow(data)==0) stop("nrow of data is 0.")
        result <- apply(data, 2, function(x)mean.weight(data=x, weights=weights, index=index, fixed.index=FALSE, index.of.result=index.of.result, index.sep=index.sep, calc.sum=calc.sum, digits=digits, na.rm=na.rm, edit.I.colnames=edit.I.colnames, del.I.help.columns=del.I.help.columns, I.help.columns=I.help.columns))
      } else if(is.data.frame(data)) {
        if(nrow(data)==0) stop("nrow of data is 0.")
        #result <- sapply(data, function(x)  mean.weight(data=x, weights=weights, index=index, fixed.index=FALSE, index.of.result=index.of.result, index.sep=index.sep, calc.sum=calc.sum, digits=digits, na.rm=na.rm, edit.I.colnames=edit.I.colnames, del.I.help.columns=del.I.help.columns, I.help.columns=I.help.columns))
        result <- as.matrix(as.data.frame(lapply(data, function(x)  mean.weight(data=x, weights=weights, index=index, fixed.index=FALSE, index.of.result=index.of.result, index.sep=index.sep, calc.sum=calc.sum, digits=digits, na.rm=na.rm, edit.I.colnames=edit.I.colnames, del.I.help.columns=del.I.help.columns, I.help.columns=I.help.columns)),stringsAsFactors=FALSE))
      }
      # Wieder zu Marix machen, falls es ein Vektor ist
      if(is.null(dim(result))) result <- t(as.matrix(result))
      # Wieder die alten Colnames vergeben
      colnames(result) <- colnames(data)
      
      # Falls eine Expression mit I() in einem der colnames ist, werden diese Kennzahlen neu berechnet.
      # Konkret wird statt "weighted mean of ratio" das "ratio of weighted means" berechnet.
      cn.res <- colnames(result) # cn.res.orig
      icols <- substr(cn.res,1,2)=="I("
      if(any(icols)){
        if(!is.null(digits)) stop("When rounding (digts!=NULL) and using I() columns, the results might not be accurate")
        # Wert der I() columns berechnen
        result <- calc.I.cols(result, edit.I.colnames=edit.I.colnames, del.I.help.columns=del.I.help.columns, I.help.columns=I.help.columns)
      }
      
      # Ergebnis in fixierte Ergebnisstrutkur einfuegen.
      if(fixed.index){
        result <- result[rownames(result)%in%rownames(rawResult),,drop=FALSE]
        rawResult[match(rownames(result),rownames(rawResult)),] <- result
        result <- rawResult
      }
      
      # Resultat ausgeben.
      if(nrow(result)==1 && isNullIndex) result <- result[1,] #rownames(result) <- NULL
      return(result)
      
      
      # Wenn !is.null(dim(data))
      # & 2 Indexe eingegeben wurden:
    } else if(length(index)==2) {
      # res1 <- mean.weight(data=data[,1], weights=weights, index=index, calc.sum=calc.sum, digits=digits, na.rm=na.rm)
      
      # Hier keine Fallunterscheidung zwischen matrix und data.frame einfuegen, sonst funktioniert es nicht!!
      res.prov <- apply(data, 2, function(x) mean.weight(data=x, weights=weights, index=index, calc.sum=calc.sum, digits=digits, na.rm=na.rm) )
      if(class(res.prov)!="matrix") res.prov <- t(as.matrix(res.prov))
      
      res.list <- list()
      su.index1 <- sort(unique(index[[1]]))
      su.index2 <- sort(unique(index[[2]]))
      for(i in 1:ncol(res.prov)){
        res.list[[i]] <- matrix(res.prov[,i],nrow=length(su.index1), ncol=length(su.index2))
        dimnames(res.list[[i]]) <- list(su.index1, su.index2)
      }
      names(res.list) <- colnames(data)
      
      # Falls eine Expression mit I() in einem der colnames ist, werden diese Kennzahlen neu berechnet.
      # Konkret wird statt "weighted mean of ratio" das "ratio of weighted means" berechnet.
      cn.res <- names(res.list)
      icols <- grepl("I\\(", cn.res)
      if(any(icols)){
        if(!is.null(digits)) stop("When rounding (digts!=NULL) and using I() columns, the results might not be accurate")
        #if(any(cn.res%in%c("_","."))) stop("When using I() colnames _ and . are not allowed.")
        res.list <- calc.I.cols(res.list, edit.I.colnames=edit.I.colnames, del.I.help.columns=del.I.help.columns, I.help.columns=I.help.columns)
      }
      return(res.list)
      
    } else if(length(index)>2) {
      stop("more than 2 indexes not possible if data is a matrix/data.frame. Please enter data as vector.")
    }
  }
  
  
  # Tatsaechliche mean.weight() Funktion.
  # Falls es keine numerische Variable ist (weil z.B. ein durchmischter data.frame eingegeben wird),
  # wird daraus eine 0 gemacht, damit die Funktion trotzdem funktioniert.
  if(! (is.numeric(data)||is.logical(data)) ) data <- rep(0, length(data))
  
  if(is.null(weights)) weights <- rep(1,length(data))
  
  # Falls kein index gegeben wurde, einfache Berechnung (mit weighted.mean)
  if( is.null(index) | is.null(index[[1]]) ){
    if(calc.sum){
      result <- sum( data * weights ,na.rm=na.rm )
    } else {
      result <- weighted.mean(data,weights, na.rm=na.rm)
    }
    
    # Sonst muss mit index und tapply() gerechnet werden.
  } else {
    index <- lapply(index, function(x)if(length(x)==1) return(rep(x,length(weights))) else return(x))
    length.index <- sapply(index,function(x)length(x))
    if(any(length.index!=length.index[1])) stop("All vectors in the index have to have the same length!")
    #print(length(weights)); print(length.index)
    if(!all(length(weights)==length.index)) stop("length(weights)!=length(index)")
    
    # NA Werte in weights uebertragen. Muss so sein, nicht mit na.rm innerhalb der Funktionen, da sonst data und weights evtl. nicht korrespondieren!!
    dataweights <- data*weights
    weights[is.na(dataweights)] <- NA
    
    if(calc.sum){
      # Resultat = Summe ( Werte * Gewichte )
      result <-  tapply(dataweights,index,  sum,na.rm=na.rm)
    } else {
      # Resultat = Summe ( Werte * Gewichte )                             / Summe( Gewichte )
      result <-  tapply(dataweights,index,  sum,na.rm=na.rm) / tapply(weights,index,  sum,na.rm=na.rm)
    }
    
    # Resultat in vorgefertige fixierte Index-Struktur einfuegen
    if(fixed.index){
      result <- result[names(result)%in%names(rawResult)]
      rawResult[match(names(result),names(rawResult))] <- result
      result <- rawResult
    }
  }
  
  # Falls gewuenscht, runden, dann Ergebnis ausgeben.
  if(!is.null(digits)) result <- round(result, digits)
  return(result)
}

#index="Reg"; inclusProbs="pik_w0"; weights="Gew_Calib"; dat=db[filtX(),]; Xs=Xs[filtX(),]
#data=spa[filt_all,unique_form]; weights=spa[filt_all,"Gewicht"]; inclusProbs=spa[filt_all,"pik_w0"]; index=x[["vector"]][filt_all]; fixedIndex=TRUE; indexOfFixedResult=indexOfFixedResult; indexStrata=rep(1,sum(filt_all)); method="ht"; figure="halfLengthCI"; CIprob=0.025; na.rm=TRUE; edit.I.colnames=TRUE; CImultiplier=NULL; relativeToMean=TRUE
#filt=spa[,"JAHR"]==2016; data=spa[filt,c("P430_0100_94000","P430_0100_94000")]; weights=spa[filt,"Gewicht"]; inclusProbs=spa[filt,"pik_w0"]; index=spa[filt,"ZATYP"]; fixedIndex=TRUE; indexOfFixedResult=c(11,12,21); indexStrata=rep(1,sum(filt)); method="ht"; figure="halfLengthCI"; CIprob=0.025; na.rm=TRUE; edit.I.colnames=TRUE; CImultiplier=NULL; relativeToMean=TRUE
#filt=spa[,"JAHR"]==2016; variance.estimate(data=spa[filt,c("P430_0100_94000")], weights=spa[filt,"Gewicht"], inclusProbs=spa[filt,"pik_w0"], index=NULL, method="ht", figure="halfLengthCI", CIprob=0.025, na.rm=TRUE, edit.I.colnames=TRUE, CImultiplier=NULL, relativeToMean=TRUE) #spa[filt,"ZATYP"], fixedIndex=TRUE, indexOfFixedResult=c(11,12,21), indexStrata=rep(1,sum(filt)),
variance.estimate <- function(data, weights, inclusProbs, index=NULL, indexStrata=NULL, fixedIndex=FALSE, indexOfFixedResult=NULL, indexSep="_", 
                              method=c("ht","calib"), figure=c("var","SE","halfLengthCI"), relativeToMean=FALSE, CIprob=NULL, CImultiplier=NULL, DFestimator=c("simple","satterwaithe"), Xs=NULL, na.rm=TRUE,
                              edit.I.colnames=FALSE){
  
  # This function calculates the variance, standard error or confidence invertals for estimated mean values.
  #
  # Arguments
  # data =          the data.frame/matrix that contains the variables of interest.
  # weights =       the vector that contains the calibrated weightss.
  # inclusProbs =   the vector that contains the inclusion probabilities
  # index =         the vector that contains the index for the aggregation level to be estimated. E.g. something like "region". If NULL, the calculation is done for the whole data.frame.
  # indexStrata =   the vector that contains the statification according to the sample design. This can be different from the index.
  # fixedIndex, indexOfFixedResult, indexSep
  #                 special arguments if a fixed index should be contained in the result. E.g. if only region 1 and region 2 occur in the sample, but also region 3 should be displayed as NA.
  #                 in this case you could specify: index=region, fixedIndex=TRUE, indexOfFixedResult=c("1","2","3"). Or if you want to display all permutations of region and type, also if they don't occur in the sample.
  #                 then specify something like fixedIndex=TRUE, index=list(region, type)
  # method =        "ht" for Horvith-Thompson method using VE.HT.Total.NHT{samplingVarEst}. "calib" for the calibration method using varest{sampling}
  # figure =        the figure to be calculated. var=variance, stErr=standard error, CI=confidence interval
  # relativeToMean= if TRUE, then the calculated variance/SE/CI will be divided by the weighted mean. Use colnames like "I(a/b)" for ratio of mean figures. see also function mean.weight().
  # CIprob =        the relative accuracy of the confidence interval (CI). +- 0.025 is the default value and will yield CImultiplier =~ 1.96 for a large sample (student distribution).
  # CImultiplier =  the factor to calculate the confidence interval (CI). 1.96 is equivalent to +-2.5%, i.e. 95% CI in a large sample (student distribution).
  # DFestimator =   the estimator to calculate the degrees of freedom if is.null(CImultiplier). CImultiplier is calculated from CIprob with assumed t-distribution. "simple" will use  df=n-(number of strata). "satterwaithe" will use the Satterwaithe approximation
  # Xs =            the Xs matrix that was used to calibrate the weights with the function sampling::calib(). Use the function calcXsMultiLevel() to calculate Xs.
  # na.rm =         if TRUE, then for each column of data the NA values will be excluded from the calculation.
  # edit.I.colnames=if TRUE, then colnames like "I(a/b)" will be edited as "a/b" for the result. This is in accordance to the mean.weight() function.
  
  figure <- match.arg(figure)
  method <- match.arg(method)
  DFestimator <- match.arg(DFestimator)
  
  # Format data
  if(is.list(data) && !is.data.frame(data)) stop("data must be matrix or data.frame but not a list.")
  isNullDimData <- is.null(dim(data))
  if(isNullDimData || !is.data.frame(data)){
    namesOrig <- if(isNullDimData) names(data) else colnames(data)
    data <- as.data.frame(data, stringsAsFactors=FALSE)
  } else {
    namesOrig <- colnames(data)
  }
  
  # Check length of all vectors
  stopifnot(length(inclusProbs) == nrow(data))
  stopifnot(length(weights) == nrow(data))
  if(!is.null(index)) {
    if(!is.list(index)) {
      stopifnot(length(index) == nrow(data))
    } else {
      lapply(index, function(x)if(length(x)!=nrow(data))stop("If index is a list, for all list entries must hold: length(index[[?]])==nrow(data)"))
    }
  }
  
  # Assure no NA weights and inclusProbs.
  if(any(is.na(weights))) stop("There must be no NA values in weights.")
  if(any(is.na(inclusProbs))) stop("There must be no NA values in inclusProbs")
  
  # CI preparations
  if(figure=="halfLengthCI"){
    if( is.null(CImultiplier) &&  is.null(CIprob)) stop("Either CImultiplier or CIprob must be specified. E.g. CImultiplier=1.96 or CIprob=0.025")
    if(!is.null(CImultiplier) && !is.null(CIprob)) stop("Either specifiy CImultiplier or CIprob but not both.")
    if(!is.null(CImultiplier) && !is.null(index)) warning("If index is given, the CImultiplier should be calculated for each iteration separately based on CIprob. Thus, CIprob should be specified, not CImultiplier.")
    if(!is.null(CIprob) && is.null(indexStrata)) stop("If CIprob is given, then indexStrata must be specified, such that the degrees of freedom can be calculated. If it isn't a stratified sample, then use indexStrata=rep(1,nrow(data)).")
    
    if(!is.null(CIprob)){
      if(CIprob < 0 || CIprob > 1) stop("CIprob must lie between 0 and 1. For +-2.5%, i.e. 95% confidence, choose 0.025.")
    }
  }
  calcCImultiplierFlag <- figure=="halfLengthCI" && !is.null(CIprob)
  
  # Wenn kein Aggregationslevel angegeben wurde, dann einen kuenstlichen erzeugen
  isNullIndex <- is.null(index)
  if(isNullIndex) {
    index <- rep("", nrow(data))
  } else {
    if(any(is.na(index))) stop("There must be no NA values in index")
  }
  # Fixed index ausschalten, wenn Index ein Vektor ist. Dann bringt es nichts.
  if(fixedIndex && is.null(indexOfFixedResult) && !is.list(index)) stop("fixedIndex & is.null(indexOfFixedResult) & !is.list(index)   -> fixedIndex doesn't have any effect this way. Give index as a list!")
  # Im Falle, dass der index fixiert sein soll, hier die rohe Ergebnisstruktur erstellen.
  if(fixedIndex){
    rawResult <- .prepare.fixed.index.result(data=data, index=index, names.result=indexOfFixedResult, index.sep=indexSep)
    index <- .paste.elements(index, sep=indexSep, errorMsg="All indices must have same length!")
  }
  
  if(method=="calib" && (is.null(dim(Xs)) || nrow(data)!=nrow(Xs))) stop("nrow(data) must be equal nrow(Xs)")
  
  # Indices (0,1) fuer die verschiedenen Aggregationslevel machen.
  levelBin <- apply( categ.to.bin(index, varname="var", sep="_"),2,function(x)as.logical(x) )
  
  # Indices der Aggregationslevel mit den zugehoerigen Variablen multiplizieren.
  # Davon die Varianz-Total-Population berechnen
  var0 <- NULL
  for(i1 in 1:ncol(levelBin)) { # i1 <- 1
    # Wenn weniger als 2 Betriebe, dann NaN zurueckgeben
    if(sum(levelBin[,i1]) < 2){
      var1 <- rep(NA_integer_, ncol(data))
      names(var1) <- colnames(data)
      # Berechnung im fall dass mind. 2 Betriebe
    } else {
      # Calculate CI factor, simple case
      if(calcCImultiplierFlag && DFestimator=="simple")
        CImultiplier <- .estStudentTmultiplier(prob=1-CIprob, indStr=indexStrata[levelBin[,i1]], y=NULL, w=NULL, simple=TRUE, na.rm=na.rm)
      # Calculate variance...
      yCounter <<- 0
      var1 <- sapply(data, function(Ys) { # Ys <- data[,1]
        filt <- if(na.rm) which(levelBin[,i1] & !is.na(Ys)) else which(levelBin[,i1])
        if(length(filt) < 2) return(NA_integer_)
        # Calculate variance
        rawVar <- .varestSampling( Ys=Ys[filt], Xs=if(method=="calib") Xs[filt,,drop=FALSE] else NULL, pik=inclusProbs[filt], w=weights[filt] )
        # Calculate CI factor, with Satterwaithe
        if(calcCImultiplierFlag && DFestimator=="satterwaithe")
          CImultiplier <- .estStudentTmultiplier(prob=1-CIprob, indStr=indexStrata[filt], y=Ys[filt], w=weights[filt], simple=FALSE, na.rm=na.rm)
        #yCounter <<- yCounter+1; cat("CImultiplier=",CImultiplier, ", ", sep=""); cat(colnames(data)[yCounter],"\n",sep="")
        # Further process variance and return
        return( .calcVarStErrOrCI(rawVar=rawVar, w=weights[filt], figure=figure, CImultiplier=CImultiplier) )
      })
      # Calculate the variance relative to mean. Absoulte value, in case the mean is negative.
      # Use mean.weight here, because then you can use column names like "I(a/b)" for mean of ratio variables.
      if(relativeToMean) var1 <- abs( var1 / mean.weight(data=data[levelBin[,i1],,drop=FALSE], weights=weights[levelBin[,i1]], index=NULL, edit.I.colnames=FALSE, na.rm=na.rm) )
    }
    #
    var0 <- rbind(var0,var1)
  }
  
  # Schlussformatierung
  if(is.null(dim(var0))){
    var0 <- t(as.matrix(var0))
  }
  rownames(var0) <- gsub("var_","",colnames(levelBin))
  
  # Ergebnis in fixierte Ergebnisstrutkur einfuegen.
  if(fixedIndex){
    var0 <- var0[rownames(var0)%in%rownames(rawResult),,drop=FALSE]
    rawResult[match(rownames(var0),rownames(rawResult)),] <- var0
    var0 <- rawResult
  }
  
  # Urspruengliche Namen wiederherstellen. Diese wurden durch data.frame zerstoert.
  if(edit.I.colnames) namesOrig <- .rm.I.from.names(namesOrig)
  if(isNullIndex && !fixedIndex) rownames(var0) <- NULL
  if(isNullDimData){
    rn1 <- rownames(var0)
    var0 <- var0[,1]
    names(var0) <- rn1
  } else {
    colnames(var0) <- namesOrig
  }
  
  # Ausgabe des Ergebnisses
  return(var0)
}

.varestSampling <- function(Ys, Xs = NULL, pik, w = NULL) {
  # This function is a copy of the function sampling::varest(). It depends on the function MASS::ginv().
  # It is used inside the variance.estimate() function.
  
  if (any(is.na(pik)))  stop("there are missing values in pik")
  if (any(is.na(Ys)))  stop("there are missing values in Ys")
  if (length(Ys) != length(pik))  stop("Ys and pik have different sizes")
  if (!is.null(Xs)) {
    if (is.data.frame(Xs)) 
      Xs = as.matrix(Xs)
    if (is.vector(Xs) & (length(Ys) != length(Xs)))  stop("Ys and Xs have different sizes")
    if (is.matrix(Xs) & (length(Ys) != nrow(Xs))) stop("Ys and Xs have different sizes")
  }
  a = (1 - pik)/sum(1 - pik)
  if (is.null(Xs)) {
    A = sum(a * Ys/pik)
    var = sum((1 - pik) * (Ys/pik - A)^2)/(1 - sum(a^2))
  } else {
    B = t(Xs * w)
    beta = MASS::ginv(B %*% Xs) %*% B %*% Ys
    e = Ys - Xs %*% beta
    A = sum(a * e/pik)
    var = sum((1 - pik) * (e/pik - A)^2)/(1 - sum(a^2))
  }
  return(var)
}

#indStr=c(1,1,2,2,3,3,3,3); y <- c(1,2,3,4,5,6,7,8); w <- c(3,6,5,8,1,3,1,9)
.estStudentTmultiplier <- function(prob, indStr, y=NULL, w=NULL, simple=TRUE, na.rm=FALSE){
  # This function calculates the student t multiplier for a given probability. The degrees of freedom are calculated by simple method or Satterwaithe approximation.
  # Arguments
  # prob =   probability
  # indStr = an index defining the strata
  # y =      variable of interest. Needed if !simple.
  # w =      weights. Needed if !simple.
  # simple = simple calculation or more sophisticated calculation using the Satterwaithe approximation?
  
  df <- if(simple) length(indStr) - length(unique(indStr)) else .estSatterDf(y=y, w=w, indStr=indStr, na.rm=na.rm)
  #cat("df=",df, ", ", sep="")
  return(qt(prob, max(1,df)))  # suppress NaN warnings if df is 0 or -1
}

.estSatterDf <- function(y, w, indStr, na.rm=FALSE){
  # This function calculates the Satterwaithe approximation of degrees of freedom.
  # Arguments
  # y =      variable of interest
  # w =      weights
  # indStr = an index defining the strata
  
  s2 <- tapply(y,indStr,var) # s^2 = var
  Nh <- tapply(w,indStr,sum)
  nh <- tapply(w,indStr,length)
  a <- Nh*(Nh-nh)/nh
  
  if(length(Nh)==0) stop("indStr seems to contain only NA values.")
  
  if(any(is.na(s2))) {
    if(na.rm){
      Nh <- Nh[!is.na(s2)]
      nh <- nh[!is.na(s2)]
      a  <- a [!is.na(s2)]
      s2 <- s2[!is.na(s2)]
      warning("There are strata with only 1 non-NA observation. In this case the Satterwaithe approximation of degrees of freedom is impossible because var(Ys)==NA. Because na.rm==TRUE, these strata were dropped in order to estimate of the degrees of freedom.")
    } else {
      stop("There are strata with only 1 observation. In this case the Satterwaithe approximation of degrees of freedom is impossible.")
    }
  }
  
  if(sum(a)==0) stop("If all(Nh==nh), in other words, If all(weights==1), then the Satterwaithe approximation does not work.")
  return( sum(a*s2)^2 / sum((a*s2)^2 / (nh-1)) )
}

.calcVarStErrOrCI <- function(rawVar, w, figure=c("var","SE","halfLengthCI"), CImultiplier){
  # Function to calculate variance, standard error or confidence interval on the level of the mean (not total population)
  # Arguments
  # rawVar =       the "raw" variance that should be further processed. rawVar can be calculated e.g. using sampling::varest().
  # w =            vector of weights
  # figure =       the figure to be calculated. Either "var"=variance, "SE"=standard error, "halfLengthCI"=half length of confidence interval.
  # CImultiplier = multiplier to calculate the confidence interval (CI)
  
  figure <- match.arg(figure)
  if(figure=="halfLengthCI") { sqrt(rawVar)  /  sum(w) * CImultiplier
  } else if(figure=="SE")    { sqrt(rawVar)  /  sum(w)
  } else if(figure=="var")   {      rawVar   /  sum(w)^2 }
}

calcXsMultiLevel <- function(dat, optVarListMultiLevel){
  # This function calculates the Xs matrix that is needed for the calib{sampling} function if different variables should be calibrated for different aggregation levels.
  # E.g. you'd like to calibrate variable a on puplation level, but variable b only on regional level.
  # Arguments
  # dat =                  the data.frame/matrix that contains the variables that should be calibrated
  # optVarListMultiLevel = the variable list for different levels. The names of the list correspond to each level that should be calibrated.
  #                        inside each list place there must be a character vector that holds the variables to be calibrated. e.g. list(levelA=c("var1","var2"), levelB=c("var3"))
  
  if(!is.list(optVarListMultiLevel) || is.null(names(optVarListMultiLevel))) stop("optVarListMultiLevel must be a named list.")
  namesNotInColnames <- names(optVarListMultiLevel)[ !names(optVarListMultiLevel)%in%colnames(dat) ]
  if(length(namesNotInColnames) > 0) stop(paste0("the names of optVarListMultiLevel must represent columns in dat. Some are not contained in colnames(dat):\n",paste0(namesNotInColnames,collapse=", ")))
  lapply(optVarListMultiLevel, function(x)if(!is.null(dim(x))) stop("In each list place of optVarListMultiLevel there must be a character vector."))
  varsNotInColnames <- sort(unique( unlist(optVarListMultiLevel)[ !unlist(optVarListMultiLevel)%in%colnames(dat) ] ))
  if(length(varsNotInColnames) > 0) stop(paste0("the values in each list place of optVarListMultiLevel must represent columns in dat. Some are not contained in colnames(dat):\n",paste0(varsNotInColnames,collapse=", ")))
  
  # Optimierungs-Zielvariablen in Xs ablegen.
  ## --> In diesem Fall fuer Kalibierung auf Ebene Betriebsform!
  Xs <- as.list(rep(0, length(optVarListMultiLevel))); names(Xs) <- names(optVarListMultiLevel)
  specialCase <- FALSE
  
  # -> Multiply the variables to be optimized with the binary index for the stratification on each level.
  for(i in names(optVarListMultiLevel)){
    # Create binaries (0,1)
    if( suppressWarnings(all(sort(unique(dat[,i]))==c(0,1))) )  {
      binInd <- dat[,i,drop=FALSE]
      specialCase <- TRUE # Prepare warning for special case of pure binary variable.
    } else {
      binInd <- categ.to.bin(dat[,i])
    }
    # Multiply the binary indices with the corresponding variables that will be calibrated
    # First multiplication is done "manually"
    indTimesVar <- dat[,optVarListMultiLevel[[i]],drop=FALSE]*binInd[,1]
    # The latter will be added to the
    if(ncol(binInd)>1) for(i2 in 2:ncol(binInd)) indTimesVar <- cbind(indTimesVar, dat[,optVarListMultiLevel[[i]],drop=FALSE]*binInd[,i2])
    Xs[[i]] <- indTimesVar
  }
  # Show warning for special case that is not handled correctly in all cases.
  if(specialCase) warning("Binary indexes (0,1) are directly multiplied with the calibration variables to calculate Xs. If you want to avoid this, use a categorial variable that starts with 1, not 0. -> E.g. use values 1 and 2.")
  
  # Collapse Xs from list to a matrix/data.frame
  return( as.matrix(do.call("cbind",Xs)) )
}

calibevE <- function (Ys, Xs, total, pikl, d, g, q = rep(1, length(d)), with = FALSE,  EPS = 1e-06) {
  # This funciton is a clone of sampling::calibev(). Refer to the help files of that package.
  # Only difference is, that e (the residuals of the calibration model) is returned as well.
  
  if (any(is.na(g))) 
    stop("There are missing values in g")
  stopifnot((ns <- length(g)) >= 1)
  if (min(pikl) == 0) {
    ss = NULL
    warning("There are zero values in the 'pikl' matrix. The variance estimator can not be computed.\n")
  }
  piks = as.vector(diag(pikl))
  if (!checkcalibration(Xs, d, total, g, EPS)$result) 
    stop("The calibration is not possible. The calibration estimator is not computed.\n")
  if (is.data.frame(Xs)) 
    Xs = as.matrix(Xs)
  if (!is.vector(Ys)) 
    Ys = as.vector(Ys)
  if (is.matrix(Xs)) 
    n = nrow(Xs) else
      n = length(Xs)
  if (ns != length(Ys) | ns != length(piks) | ns != n | ns != 
      length(d)) 
    stop("The parameters have different sizes.\n")
  w = g * d
  wtilde = w * q
  B = t(Xs * wtilde)
  beta = ginv(B %*% Xs) %*% B %*% Ys
  e = Ys - Xs %*% beta
  if (!with) 
    e = e * w else
      e = e * d
  ss = 0
  for (k in 1:ns) {
    ss2 = 0
    for (l in 1:ns) ss2 = ss2 + (1 - piks[k] * piks[l]/pikl[k, 
                                                            l]) * e[l]
    ss = ss + e[k] * ss2
  }
  
  return( list(calest = sum(w * Ys), evar = as.numeric(ss), e=e) )
}

#vars <- c("asd","efe+p", "c-1", "f*c", "a/ b", "A^B", "c,d", "a==b", "ifelse(a==b, 1, 2)")
extract.I.vars <- function(vars, keep.original=FALSE, keep.only.necessary=TRUE){
  # This function extracts all Variables in a I(a+b*c) formula seperately. This is useful in combination with the function mean.weight()
  vars_all <- vars[grep("\\(|\\)|\\-|/|\\*|\\+|\\^|,|=|ifelse",vars)]
  vars_all <- unlist(strsplit(vars_all,"-|/|\\*|\\+|\\^|,|=|ifelse"))
  vars_all <- gsub("I\\(|\\(|\\)| ","",vars_all)
  vars_all <- unique(vars_all[!vars_all%in%c("")])
  vars_all <- vars_all[is.na(suppressWarnings(as.numeric(vars_all)))]
  if(keep.only.necessary) vars_all <- vars_all[!vars_all%in%vars]
  if(keep.original) vars_all <- unique(c(vars, vars_all))
  return(vars_all)
}

#gb <- load.gb(); data <- gb; cols <- c("I(ArbVerdFamilie/JAE_FamAK)", "I(LE+NE_tot)", "JAE_FamAK"); non.I.value=NA
create.cols <- function(data, cols, non.I.value=NA_integer_){
  # This function creates columns in a data.frame that do not exist yet.
  # If they are written as I(), their value is filled with the right value.
  # Arguments
  # data         data.frame to which columns should be added
  # cols         all new colums that should be created in data.frame. Can also be without I().
  # non.I.value  The value that is filled into columns that don't have colnames I().
  
  # Create new columns
  cols_new <- cols[!cols%in%colnames(data)]
  data[,cols_new] <- non.I.value
  
  # Calculate the value of the new columns with I()
  if(any(substr(cols,1,2)=="I(")){
    data <- calc.I.cols(data)
  }
  
  # Return result
  return(data)
}

#gb <- load.gb(); data <- gb; data[,c("I(ArbVerdFamilie/JAE_FamAK)", "I(LE+NE_tot)","NA")] <- 1
#h(calc.I.cols(data))
#data <- matrix(c(1:30),ncol=3); colnames(data) <- c("a","b","I(a+b)")
#data <- as.list(as.data.frame(data))
calc.I.cols <- function(data, edit.I.colnames=FALSE, del.I.help.columns=FALSE, I.help.columns=NULL) {
  # This function calculates the value of all columns with colnames looking like I(a+b) in a matrix/data.frame/list.
  # The whole matrix/data.frame/list is returned after having finished the caluclations.
  # Arguments
  # data               matrix/data.frame/list. Columns to be calculated must have names like I(a+b), I(a/b). Only "a+b" does not work.
  # edit.I.colnames    Should the brackets I() in the colnames be removed after the calculations?
  # del.I.help.colums  If e.g. I(a+b) is calculated. Should the columns "a" and "b" be removed after the calculation because they are of no interest?
  # I.help.columns     If del.I.help.colums=TRUE: The list of the columns to be deleted can be specified.
  #                    Otherwise the help columns are dectected automatically.
  
  ismat <- is.matrix(data)
  if(ismat) data <- as.data.frame(data)
  
  i_cols <- names(data)
  i_cols <- i_cols[substr(i_cols,1,2)=="I(" & substr.rev(i_cols,1,1)==")"]
  # If there are no i_cols then return the original data without calculations.
  if(length(i_cols)==0) return(data)
  
  # Calc only if there are elements. Otherwise this would yield an error.
  if(length(data[[1]])>0){
    for(i in 1:length(i_cols)){
      data[[i_cols[i]]] <- with(data, eval(parse(text=i_cols[i])))
      if(is.data.frame(data)) data[[i_cols[i]]] <- as.vector(data[[i_cols[i]]])
    }
  }
  
  if(del.I.help.columns){
    i_cols <- substr(names(data),1,2)=="I(" & substr.rev(names(data),1,1)==")"
    if(!is.null(I.help.columns)){
      delnames <- I.help.columns
    } else {
      delnames <- unlist(strsplit(names(data)[i_cols],"-|/|\\*|\\+"))
      delnames <- gsub("I\\(|\\(|)","",delnames)
    }
    data <- data[!names(data)%in%delnames] # ALT, geht nicht fuer Listen: #data <- data[,!names(data)%in%delnames,drop=FALSE]
  }
  if(edit.I.colnames){
    names(data) <- .rm.I.from.names(names(data))
  }
  
  if(ismat) data <- as.matrix(data)
  return(data)
}

.rm.I.from.names <- function(x){
  i_x <- substr(x,1,2)=="I(" & substr.rev(x,1,1)==")"
  x[which(i_x)] <- substr( x[which(i_x)], 3, nchar(x[which(i_x)])-1 )
  return(x)
}

median.weight <- function(...) return(quantile.weight(..., probs=0.5))
#x <- gb[,"ArbVerd_jeFJAE"]; weights <- gb[,"Gewicht"];index <- gb[,c("Jahr","Region","Betriebstyp_S3")]; probs=0.5; na.rm=TRUE
#quantile.weight(x=gb[,c("LE","ArbVerd_jeFJAE")], weights=gb[,"Gewicht"], index=gb[,c("Jahr")], probs=c(0.25,0.5))

quantile.weight <- function(x, weights=NULL, index=NULL, probs=0.5, na.rm=TRUE) {
  # This function calculates weighted quantiles.
  # Arguments:
  # x       = Vector of numbers of which quantiles should be calculated
  # weights = vector of weights
  # probs   = probabilities of quantiles
  
  # Original function was cwhmisc::w.median. Alternative function with same result is reldist::wtd.quantile
  # Differents result calculated by these functions: Hmisc::wtd.quantile, matrixStats::weightedMedian (test with RefB, Jahr 2012, gb[,"ArbVerd_jeFJAE"])
  
  # Recursive function definition if x is given as matrix, data.frame or list
  if(!is.null(dim(x))) {
    cn_x <- colnames(x)
    if(is.matrix(x)) res <-  apply(x,2,function(x)quantile.weight(x=x,weights=weights,index=index,probs=probs,na.rm=na.rm))
    if(is.data.frame(x)) res <- as.data.frame( lapply(x,function(x)quantile.weight(x=x,weights=weights,index=index,probs=probs,na.rm=na.rm)) ,stringsAsFactors=FALSE)
    colnames(res) <- cn_x
    return(res)
  }
  if(is.list(x)) return( lapply(x,function(x)quantile.weight(x=x,weights=weights,index=index,probs=probs,na.rm=na.rm)) )
  
  # Recursive function definition if index is given
  if(!is.null(index)){
    res <- by(cbind(x=x, weights=weights),index,function(x) quantile.weight(x=x[,"x"], weights=if(is.null(weights)) NULL else x[,"weights"], probs=probs, na.rm=na.rm))
    attr(res,"call") <- NULL
    if(length(res[[1]])>1) res <- do.call("rbind", res) else if(length(dim(res))==1) res <- c(res) else class(res) <- "array"
    return(res)
  }
  
  if(is.null(weights)) weights <- rep(1, length(x))
  w <- weights
  
  # Recursive function definition for more than 1 probs
  if(length(probs)>1){
    res <- apply(matrix(probs),1,function(probs)  quantile.weight(x=x, weights=w, probs=probs, na.rm=na.rm) )
    names(res) <- paste0(round(probs*100,2),"%")
    return(res)
    
    # Now follows the actual function to calculate weighted means
  } else {
    if(na.rm) {
      ok <- complete.cases(x, w)
      x <- x[ok]
      w <- w[ok]
    }
    w_not0 <- w!=0
    x <- x[w_not0]
    w <- w[w_not0]
    
    if(length(x)==0) return(NA)
    
    ind <- sort.list(x)
    x <- x[ind]
    w <- w[ind]
    ind1 <- min(which(cumsum(w)/sum(w) >= probs))
    ind2 <- if ((w[1]/sum(w)) > probs) {
      1
    } else { # Fueher war else erst auf der naechsten Zeile. Aber funktioniert der Code dann richtig??
      max(which(cumsum(w)/sum(w) <= probs))
    }
    max(x[ind1], x[ind2])
  }
}

####
quantile.inverse <- function(x, value){ # quantile.reverse inverse.quantile reverse.quantile
  # This funciton acts as a inverse quantile function.
  # However, it does not yield exactly the same results as you would get using the quantile() function because the distribution functions differ.
  # Arguments
  # x     = All values in the sample.
  # value = The value of which the probability in the cumulative distribution should be calculated.
  invProb <- ecdf(x)(value)
  invProbs <- c(invProb-0.01,invProb,min(1,invProb+0.01))
  values <- quantile(x,invProbs)
  return(approx(x=values,y=invProbs,xout=value)$y)
}

####
meansd <- function(x,na.rm=TRUE) {
  c(Mean=mean(x,na.rm=na.rm),SD=sd(x,na.rm=na.rm))
}
####

mean.geom <- function(x,na.rm=TRUE) {
  if(na.rm==TRUE) x <- x[!is.na(x)]
  result <- (prod(x))^(1/length(x))
  # Wenn das Ergebnis zu gross wird (Inf) m?ssen die Anfangswerte erst verkleinert werten.
  if(is.infinite(result)){
    for(i in seq(1,6,1)){
      result <- mean.geom(x=x*10^(-i),na.rm=na.rm)*10^(i)
      if(!is.infinite(result)) break
    }
  }
  names(result) <- "geom.Mean"
  return(result)
}
####

meansd.geom <- function(x,na.rm=TRUE){
  if(na.rm==TRUE) x <- x[!is.na(x)]
  m <- mean.geom(x=x,na.rm=na.rm)
  sd <- sd(x)
  return(c(Mean=m,SD=sd))
}
####
# data <- tab_ch0; margin=1; front.back=1; method="sum"; name="Total"; digits=NULL; na.rm=FALSE
add.means <- function(data, margin=c(1,2), front.back=c(1,2), method=c("arith","geom","sum","median"), name=NULL, digits=NULL, na.rm=FALSE, ...){
  
  if(is.list(data) & !is.data.frame(data)) return(lapply(data, function(data)add.means(data=data, margin=margin, front.back=front.back, method=method, name=name, digits=digits, ...)))
  
  if(na.rm) data[is.na(data)] <- 0
  
  margin <- margin[1]
  front.back <- front.back[1]
  matrix.opposite <- matrix(c(1,2,2,1),ncol=2)
  margin.opposite <- matrix.opposite[2 ,matrix.opposite[1,margin] ]
  method <- match.arg(method)
  if(is.null(name)){
    if(method=="arith") name <- "mean"
    if(method=="geom") name <- "geom"
    if(method=="sum") name <- "sum"
    if(method=="median") name <- "median"
  }
  name <- as.character(name)
  #if(is.null(name)) name <- as.character(substitute(func))
  mean.geom <- function(x,na.rm=TRUE) {
    if(na.rm==TRUE) x <- x[!is.na(x)]
    result <- (prod(x))^(1/length(x))
    names(result) <- "Mean"
    return(result)
  }
  if(method=="arith") mean.func <- mean else if(method=="geom") mean.func <- mean.geom else if(method=="sum") mean.func <- sum else if(method=="median") mean.func <- median
  if(is.null(digits)) digits <- max(do.call("c",lapply(diag(data),function(x)n.decimals(x))))
  
  if(margin==1) {
    bind.func <- cbind
    count.func <- ncol
  } else {
    bind.func <- rbind
    count.func <- nrow
  }
  means <- apply(data,margin,function(x)mean.func(x))
  means <- round(means,digits)
  
  if(front.back==1) {
    res <- bind.func(means, data)
    attributes(res)$dimnames[[margin.opposite]][1] <- name
  } else {
    res <- bind.func(data,means)
    attributes(res)$dimnames[[margin.opposite]][count.func(res)] <- name
  }
  return(res)
}
####

gb.diffs <- function(x,cols=list(c("2013","2014")), # cols=list(c("2013","2014"),c("2005","2014"))
                     short.names=list(c("13","14")), digits=2, percdigits=1) { # short.names=list(c("13","14"),c("05","14"))
  # This function calculates the Grundlagenberichts-differences that are mostly used in the
  # Medienmitteilung and the Hauptbericht
  # First, calculate weighted means with the function mean.weights()
  # Then use gb.diffs() to calculate the differences between years.
  if(is.list(x) & !is.data.frame(x)) return( lapply(x,function(x)gb.diffs(x=x, cols=cols, short.names=short.names, digits=digits)) )
  
  allcols <- unlist(cols)
  if(any(!allcols%in%colnames(x))){
    stop(paste0("The following columns are not available in the data: ", paste0(allcols[!allcols%in%colnames(x)],collapse="  ")))
  }
  add <- list()
  for(i in 1:length(cols)){
    tp <- cols[[i]][2]
    tm <- cols[[i]][1]
    temp <- cbind( x[,tp]-x[,tm], (x[,tp]/x[,tm]-1)*100 )
    temp[,2] <- round(temp[,2], percdigits)
    if(is.null(short.names)){
      colnames(temp) <- c(paste0(tp,"-",tm), paste0(tp,"/",tm))
    } else {
      tpn <- short.names[[i]][2]
      tmn <- short.names[[i]][1]
      colnames(temp) <- c(paste0(tpn,"-",tmn), paste0(tpn,"/",tmn))
    }
    add[[i]] <- temp
  }
  add <- do.call("cbind", add)
  res <- cbind(x,add)
  if(!is.null(digits)) res <- round(res,digits)
  return(res)
}

####

if(FALSE){
  X=round(runif(100,1,15)); INDEX=c(rep(1,50), rep(2,20), rep(3,30)); FUN=function(x)mean(x); missing.value=NA
  names.result=c(1:5, "1_a"); names.result=NULL
  INDEX <- list( c(rep(1,50), rep(2,20), rep(3,30)) ,
                 c(rep("a",30),rep("b",20), rep("c",50)),
                 c(rep("u",15),rep("v",60), rep("w",25)),
                 c(rep("x",10),rep("y",25), rep("z",65))
  )
  vector.result=FALSE
  
  X=round(runif(100,1,15));
  INDEX <- list( c(rep(1,50), rep(2,20), rep(3,30)) ,
                 c(rep("a",30),rep("b",20), rep("d",50)),
                 c(rep("u",15),rep("v",60), rep("w",25))
  )
  FUN=function(x)mean(x); missing.value=NA
  names.result <- list(c(1,2,3),c("a","c","d"),c("u","v","w"))
  vector.result=FALSE
  
  sep.sign="_"
  res <- t1apply.f1ixed(X, INDEX, FUN=function(x)mean(x), names.result=names.result, vector.result=vector.result)
  res
  res[!is.na(res)]
  pasteown <- function(...) paste(..., sep=sep.sign)
  cbind(names(res[!is.na(res)]), sort(unique(do.call(pasteown, INDEX))))
}


tapply.fixed <- function(X, INDEX, FUN, names.result=NULL, missing.value=NA, vector.result=FALSE, sep.sign="_", warn=FALSE){
  # This function puts the result of tapply(X,INDEX,FUN) into a fixed given vector with names=names.result.
  # This is especially useful if some entries are missing in INDEX but you want them to be displayed as well!
  # Otherwise they would be missing in the result of the tapply() function.
  #
  # Arguments:
  # X, INDEX, FUN: See help page for tapply()
  # names.result = The names of the resulting vector (including all possible 0/NA entries).
  # missing.value = Which value should be put into the resulting vector if there were no entries in INDEX?
  # sep.sign = The sign to separate the new vector index and names.result if INDEX is a list.
  # vector.result = Should the result be presentet in a vector (one dimension) or in a multidimensional array?
  
  if(!is.null(dim(X))) stop("This function only works for vectors! dim(X) must be NULL!")
  if(length(INDEX)==1) vector.result=TRUE
  if(is.matrix(INDEX)) INDEX <- as.data.frame(INDEX, stringsAsFactors=FALSE)
  
  # Falls names.result eine Liste ist, werden alle moeglichen Kombinationen der Listenplaetze
  # zusammengestelt und damit ein Vektor erstellt
  if(is.list(names.result) & vector.result){
    if(length(names.result)==1) {
      names.result <- sort(unique(names.result[[1]]))
    } else {
      su.index1 <- sort(unique(names.result[[1]]))
      su.index2 <- sort(unique(names.result[[2]]))
      nres1 <- paste( rep.1b1(su.index1, length(su.index2)) , rep(su.index2, length(su.index1)) , sep=sep.sign)
      if(length(names.result)>2){
        for(i in 3:length(names.result)){
          su.index1 <- nres1
          su.index2 <- sort(unique(names.result[[i]]))
          nres1 <- paste( rep.1b1(su.index1, length(su.index2)) , rep(su.index2, length(su.index1)) , sep=sep.sign)
        }
      }
    }
  }
  
  # Vorbereiten von INDEX und names.result
  # Wenn der Index eine Liste ist...
  if(is.list(INDEX)) {
    l.INDEX <- sapply(INDEX, function(x)if(!is.null(dim(x))) nrow(x) else length(x))
    if(any(l.INDEX!=l.INDEX[1])) stop(paste0("All indexes must have the same length! ", paste0(l.INDEX,collapse=" ") ))
    if(l.INDEX[1]==0 && length(X)>0) stop(paste0("INDEX(es) has/have length=0 or nrow=0."))
    
    # Wenn das Resultat 1 Dimension haben soll.
    if(vector.result){
      if(is.null(names.result)){
        if(length(l.INDEX)==1) {
          names.result <- sort(unique(INDEX[[1]]))
        } else {
          su.index1 <- sort(unique(INDEX[[1]]))
          su.index2 <- sort(unique(INDEX[[2]]))
          names.result <- paste( rep.1b1(su.index1, length(su.index2)) , rep(su.index2, length(su.index1)) , sep=sep.sign)
          if(length(INDEX)>2){
            for(i in 3:length(INDEX)){
              su.index1 <- names.result
              su.index2 <- sort(unique(INDEX[[i]]))
              names.result <- paste( rep.1b1(su.index1, length(su.index2)) , rep(su.index2, length(su.index1)) , sep=sep.sign)
            }
          }
        }
      }
      pasteown <- function(...) paste(..., sep=sep.sign)
      INDEX <- do.call(pasteown, INDEX)
      
      # Wenn das Resultat mehrere Dimensionen haben soll.
    } else {
      if(is.null(names.result)){
        names.result <- lapply(INDEX,function(x)sort(unique(x)))
      }
    }
    # Wenn der Index keine Liste ist.
  } else {
    if(is.null(names.result)) names.result <- sort(unique(INDEX))
  }
  
  
  # Resultate-Berechnung im Falle mehrdimensionaler Ergebnisstruktur.
  if(is.list(INDEX) & !vector.result){
    # Hier Ergebnis-Berechnung
    res0 <- tapply(X,INDEX,FUN)
    if(!is.na(missing.value)) res0[is.na(res0)] <- missing.value
    
    nres0 <- unlist(dimnames(res0))
    nres1 <- unlist(names.result)
    # Warnung ausgeben, falls nicht alle Ergebnisse ausgegeben werden wegen zu kurzem names.result
    if(warn) if(any(!nres0%in%nres1))
      warning(paste0("For some entries in X no corresponding entries in names.result were given. The resulting array is incomplete!\n", paste(nres0[!nres0%in%nres1], collapse=" ") ))
    # Aufwaendige Uebertragung nur machen, wenn es wirklich fehlende Eintraege in res0 gibt
    # ALTE Bedingung: if(length(res0)!=length(nres1) || names(res0)!=nres1){
    if(length(res0)!=length(nres1) || length(names(res0))==0 || any(names(res0)!=nres1)){
      # Ergebnis-Strukturen fuer Matching vorbereiten.
      su.index1 <- sort(unique(INDEX[[1]]))
      su.index2 <- sort(unique(INDEX[[2]]))
      nres0 <- paste( rep.1b1(su.index1, length(su.index2)) , rep(su.index2, length(su.index1)) , sep=sep.sign)
      if(length(INDEX)>2){
        for(i in 3:length(INDEX)){
          su.index1 <- nres0
          su.index2 <- sort(unique(INDEX[[i]]))
          nres0 <- paste( rep.1b1(su.index1, length(su.index2)) , rep(su.index2, length(su.index1)) , sep=sep.sign)
        }
      }
      su.index1 <- sort(unique(names.result[[1]]))
      su.index2 <- sort(unique(names.result[[2]]))
      nres1 <- paste( rep.1b1(su.index1, length(su.index2)) , rep(su.index2, length(su.index1)) , sep=sep.sign)
      if(length(names.result)>2){
        for(i in 3:length(names.result)){
          su.index1 <- nres1
          su.index2 <- sort(unique(names.result[[i]]))
          nres1 <- paste( rep.1b1(su.index1, length(su.index2)) , rep(su.index2, length(su.index1)) , sep=sep.sign)
        }
      }
      res1 <- array(missing.value, dim=lapply(names.result,function(x)length(x)), dimnames=names.result)
      res1[nres1%in%nres0] <- res0[nres0%in%nres1]
      if(is.null(dim(res1))) res1 <- as.array(res1); return(res1)
      # Sonst Original-Ergebnis ausgeben
    } else {
      if(is.null(dim(res0))) res0 <- as.array(res0); return(res0)
    }
    
    # Resultate-Berechnung im Falle von Vektor-Ergebnisstruktur
  } else{
    res0 <- tapply(X,INDEX,FUN)
    if(!is.na(missing.value)) res0[is.na(res0)] <- missing.value
    
    # Warnung ausgeben, falls nicht alle Ergebnisse ausgegeben werden wegen zu kurzem names.result
    if(warn) if(any(!names(res0)%in%names.result))
      warning(paste0("For some entries in X no corresponding entries in names.result were given. The resulting array is incomplete!\n", paste(names(res0)[ !names(res0)%in%names.result ], collapse=" ") ))
    # Aufwaendige Uebertragung nur machen, wenn es wirklich fehlende Eintraege in res0 gibt
    if(length(res0)!=length(names.result) || names(res0)!=names.result){
      res1 <- rep(missing.value, length(names.result))
      names(res1) <- names.result
      ind <- match(names(res0),names(res1))
      if(any(is.na(ind))){
        res0 <- res0[!is.na(ind)]
        ind <- ind[!is.na(ind)]
      }
      res1[ ind ] <- res0
      if(is.null(dim(res1))) res1 <- as.array(res1); return(res1)
      # Sonst Original-Ergebnis ausgeben
    } else {
      if(is.null(dim(res0))) res0 <- as.array(res0); return(res0)
    }
  }
}

#X=round(runif(100,1,15)); names.result=1:5;
#table.fixed(X, names.result=1:5)
#table.fixed(round(runif(100,1,15)), round(runif(100,1,15)), names.result=list(1:20, 5:15), vector.result=FALSE)
table.fixed <- function(..., names.result=NULL, vector.result=FALSE, sep.sign="_") {
  # This function puts the result of table(X) into a fixed given vector with names=names.result.
  # It is a wrapper function for tapply.fixed().
  # For information on the arguments see tapply.fixed()
  INDEX <- list(...)
  if(is.list(INDEX[[1]])) INDEX <- INDEX[[1]]
  # Kuenstlich Daten erzeugen, falls keine vorhanden sind. Ist noetig, damit es keinen Fehler in tapply.fixed gibt.
  #if(length(INDEX[[1]])==0){
  #  INDEX[[1]] <-
  #}
  tapply.fixed(X=rep(1,length(INDEX[[1]])), INDEX=INDEX, FUN=function(x)length(x), names.result=names.result, missing.value=0, vector.result=vector.result, sep.sign=sep.sign)
}

####
if(FALSE){
  # Debugging data for bxy.partially()
  cost <- load.cost(); sampl <- sample(1:200, 200)
  data <- cost[sampl,]
  data[,"orderTester"] <- 1:nrow(data)
  relevantColnames <- c("Gewinn","Groesse","orderTester")
  INDICES <- cost[sampl,c("ID","Jahr")]
  FUN <- function(x) {
    x[,"Gewinn_tot"] <- sum(with(x, Gewinn*Groesse), na.rm=TRUE)
    #return(x[,c("Gewinn_tot","Gewinn"),drop=FALSE])
    return(x[,c("Gewinn_tot"),drop=FALSE])
  }
  bxy.partially(data=data, relevantColnames=relevantColnames, INDICES=INDICES, FUN=FUN)
}

# by(data.frame(v1=1:10),INDICES=c(1,1,1,1,1,2,2,2,2,2),function(x)return(data.frame(result=x[,1,drop=FALSE])))
by.add.df.cols <- function(data, relevantColnames, INDICES, FUN, showWarnings=TRUE) {
  # This function uses by() over data[,relevantColnames] with INDICES and a defined function FUN. See also ?by
  # Is is designed to be faster than by() over the whole data.frame because is takes only these columns into by() that are really needed for the function.
  # After the calculation a data.frame is returned instead of the usual list that is returned by the by function().
  # The initial order of the rows in data is kept in the result. The additionally calculated columns from FUN are added to the original data.frame.
  
  # Arguments
  # data             = The data frame over which by() should be applied.
  # relevantColnames = The relevant colnames that are needed for the calculations in FUN.
  # INDICES          = The indices for the application of by(). This will be pasted to a signle string if !is.null(dim(INDICES)).
  # FUN              = The function to apply within by()
  
  # Combine INDICES to a single string if it is given as several columns of a data.frame/matrix
  if(is.matrix(data)) data <- as.data.frame(data)
  if(is.list(INDICES) & !is.data.frame(INDICES)) INDICES <- as.data.frame(INDICES)
  if(!is.null(dim(INDICES))) INDICES <- paste.cols(INDICES, colnames(INDICES))
  if(length(INDICES)!=nrow(data)) stop("INDICES must have the same number of elements as data has rows. Do not enter colnames here, but vectors instead.")
  # By...   Add column to restore the original order of the rows.
  # An additional function has to be definded that add will add the "order column" with content=1:nrow(x) and col number=ncol(x) to the result.
  res <- by( data[,relevantColnames,drop=FALSE], INDICES, FUN )
  if(!is.null(dim(res[[1]]))) res <- do.call("rbind",res) else {
    res <- matrix(do.call("c",res)); colnames(res) <- "byResult"
    if(showWarnings) cat("The resulting column was named 'byResult' because FUN returned a vector without dimensions.\n")
  }
  # Now remove the relevantColnames and reorder the data. Set rownames as of data.
  tapplyOrder <- unname(unlist( tapply(1:length(INDICES),INDICES,function(x)return(x))))
  if(length(tapplyOrder)!=nrow(res)) stop("The result given by FUN does not containt all rows of the initial data.frame. Please check FUN and correct it.")
  res <- res[order(tapplyOrder),
             !colnames(res)%in%relevantColnames,drop=FALSE]
  if(ncol(res)==0) stop("The colnames of the result must not be named like relevantColnames, otherwise they are deleted and not returned.")
  rownames(res) <- rownames(data)
  # Return original data.frame and ordered additional columns (without the column that was added to restore the initial row order.
  return(cbind(data, res))
}

#### OTHER ####
minmax <- function(x, na.rm=TRUE) {
  return(range(x, na.rm=na.rm))
}

file.move <- function(from, to) {
  todir <- dirname(to)
  if ( any(is.na(file.info(todir)$isdir)) || any(!file.info(todir)$isdir) ) {
    stop("The directory to which shall be copied does not exist.")
    dir.create(todir, recursive=TRUE)
  }
  if( length(to)==1 && length(from)>1 && isTRUE(file.info(to)$isdir) ){
    to <- paste0(to,"/",basename(from))
  }
  success <- file.rename(from = from,  to = to)
  if(any(!success)) stop("Some files were not moved. Files not accessible? Or they might already exist in the copy directory?")
}

#fromFile <- "Y:/ZADaten/SpE/Liste_Plausible/B2016/5_Termin/Plausible_B2016.csv" # //art-settan-1000-evdad.admin.ch
#fromFile="//adb.intra.admin.ch/Agroscope$/Org/Sites/TA/Transfer/hpda/MATLAB_goya/Matlab_Skripte/_Erklärungsfile.xlsx";                   toBaseDir="G:/_";                   urlAlias="O"
file.copy.with.dir <- function(fromFile, toBaseDir, urlAlias=NULL){
  
  if(length(fromFile)>1 && length(toBaseDir)==1){
    toBaseDir <- rep(toBaseDir, length(fromFile))
  }
  if(length(fromFile) != length(toBaseDir)) stop("length(form) must be equal length(toBaseDir).")
  if(!is.null(urlAlias) && length(fromFile) != length(urlAlias)) stop("length(fromFile) must be equal length(urlAlias).")
  
  # Recursive function definition for vector aruments
  if(length(fromFile)>1 && length(toBaseDir)>1) {
    return(apply(matrix(c(fromFile,toBaseDir,urlAlias),nrow=length(fromFile)),1,function(x)copy.dir.structure(fromFile=x[1], toBaseDir=x[2], urlAlias=x[3])))
  }
  
  if(!dir.exists(toBaseDir)) stop(paste0(dir.exists, " does not exist (argument 'toBaseDir')"))
  if(!file.exists(fromFile)) stop(paste0(fromFile, " does not exist (argument 'fromFile')"))
  
  if( !grepl("^[a-z]:",fromFile,ignore.case=TRUE) ) {
    if(is.null(urlAlias)) stop("'fromFile' must start with a letter for the drive. Like C:/")
    
    fromFileBaseDir <- sub("//[^/]*/",paste0(urlAlias,"/"), gsub("\\\\","/",dirname(fromFile)),ignore.case=TRUE)
  } else {
    fromFileBaseDir <- sub(":","",dirname(fromFile))
  }
  
  toDir <- paste0(toBaseDir,"/",fromFileBaseDir)
  if(!dir.exists(toDir)) dir.create(toDir, recursive=TRUE)
  
  file.copy(fromFile, paste0(toDir, "/", basename(fromFile)))
}

is.dir <- function(path) {
  return( file.info(path)$isdir )
}

#is.finite.data.frame <- function(x){
#  # Error in is.finite(df) : default method not implemented for type 'list'
#  return(as.matrix(as.data.frame(lapply(x,function(x)is.finite(x)))))
#}
#is.numeric.data.frame <- function(x) {
#  return(as.data.frame(lapply(x,function(x)is.numeric(x)),stringsAsFactors=FALSE))
#}

transl.typ <- function(x, short=FALSE, FAT99=FALSE, give.tab=FALSE){
  
  # Wenn Typennummern in Vektor groesser als 99, dann ist es in der Schreibweise 15..
  s3.numb <- c(   11,           12,           21,          22,               23,                   31,                41,                   51,                          52,                          53,                   54)
  
  if(!short){
    s3.name <- c("Ackerbau","Spezialkulturen","Milchkühe", "Mutterkühe", "Rindvieh gemischt", "Pferde/Schafe/Ziegen", "Veredelung", "Kombiniert Milchkühe/Ackerbau", "Kombiniert Mutterkühe", "Kombiniert Veredelung", "Kombiniert Andere")
    if(FAT99) s3.name[c(3,5,8)] <- c("Verkehrsmilch", "Anderes Rindvieh", "Kombiniert Verkehrsmilch/Ackerbau")
  } else {
    s3.name <- c("Ackb","Spez","Milk","MuKu","RiGe","PfSZ","Vere","MiAc","KoMu","KoVe","KoAn")
    if(FAT99) s3.name[c(3,5,8)] <- c("VMil","AnRi","VMAc")
  }
  if(give.tab){ tab <- matrix(s3.numb); dimnames(tab) <- list(s3.name, "code"); return(tab) }
  
  if(x[1]>99) s3.numb <- s3.numb + 1500
  return(replace.values(s3.numb, s3.name, x))
}

transl.reg <- function(x, give.tab=FALSE){
  name <- c("Tal","Hügel","Berg")
  numb <- c(  1,     2,      3)
  if(give.tab){ tab <- matrix(numb); dimnames(tab) <- list(name, "code"); return(tab) }
  return(replace.values(numb, name, x))
}

transl.kt <- function(x, give.tab=FALSE){
  name <- c("ZH", "BE", "LU", "UR", "SZ", "OW", "NW", "GL", "ZG", "FR", "SO", "BS", "BL", "SH", "AR", "AI", "SG", "GR", "AG", "TG", "TI", "VD", "VS", "NE", "GE", "JU", "FL")
  numb <- 1:27
  if(give.tab){ tab <- matrix(numb); dimnames(tab) <- list(name, "code"); return(tab) }
  return(replace.values(numb, name, x))
}

transl.lbf <- function(x, give.tab=FALSE){
  name <- c("konv.","?LN","Bio","Bio Umstell.")
  numb <- c(   1,     2,    3,       4)
  if(give.tab){ tab <- matrix(numb); dimnames(tab) <- list(name, "code"); return(tab) }
  return(replace.values(numb, name, x))
}

transl.ths <- function(x, give.tab=FALSE){
  # dat <- read.cb("no") # Quelle: \\evdad.admin.ch\AGROSCOPE_OS\2\5\2\1\1\1860\C_GB\B2014\A_Vers\Druckerei_Adr&Auflage_f_Versand
  ths.name <- c("Agro-Treuhand Rütti AG", "Agro-Treuhand Schwand", "Agro-Treuhand Berner-Oberland",  "Agro-Treuhand Emmental", "Agro-Treuhand Aargau", "Agro-Treuhand Thurgau AG",
                "Agro-Treuhand Waldhof", "BBV Treuhand", "Agro-Treuhand Region Zürich AG",  "BBV Treuhand", "BBV Treuhand", "Agro-Treuhand Schwyz GmbH",
                "SBV Treuhand und Schätzungen", "Fidasol S.A.", "AgriGenève",  "Cofida S.A.", "Service des comptabilités agricoles", "Service des comptabilités agricoles",
                "Service de l'Agriculture", "Fiduciaire SEGECA", "Fiduciaire SEGECA",  "Fondation Rurale Interjurassienne", "Landwirtschaftszentrum Visp",
                "Agro-Treuhand Seeland AG", "Agro-Treuhand Sursee", "Agro-Treuhand Uri, Nid- und Obwalden GmbH",  "Agro-Treuhand Uri, Nid- und Obwalden GmbH", "Agro-Treuhand Glarus",
                "B?ndner Bauernverband", "Agro-Treuhand Solothurn-Baselland",  "Fessler Treuhand GmbH", "Studer-Korner Treuhand")
  ths.numb <- c(101L, 102L, 103L, 104L, 105L, 110L, 111L, 112L, 113L, 115L, 116L, 117L, 121L, 201L, 202L, 203L, 204L, 224L, 205L, 206L, 226L, 207L, 225L, 336L, 338L, 340L, 342L, 362L, 401L, 402L, 403L, 404L)
  
  if(give.tab){ tab <- matrix(ths.numb); dimnames(tab) <- list(ths.name, "code"); return(tab) }
  return(replace.values(ths.numb, ths.name, x))
}

transl.mm <- function(x, gsub=FALSE, excel.format=FALSE){
  # Diese Funktion uebersetzt die Merkmalsnummern der Merkmalsliste ZA2015 in die Zeilen-Bezeichnungen laut REX.
  # Wenn gsub=TRUE findet suche in Strings statt, statt ganze Character-Vektorplaetze zu uebersetzen.
  # Wenn excel.forma=TRUE wird davor noch ein Abstand gemacht, falls am Anfang ein Rechenoperations-Zeichen steht.
  
  if(!exists("transmm_list", envir=globalenv())) {
    cat("Reading in translation...\n")
    transmm_list <<- as.matrix(read.table(paste0("//evdad.admin.ch/AGROSCOPE_OS/2/5/2/1/4/3/4285/MML/Berechn_Uebersetz_in_R/Data/Data_out/MML_Namen_Nummern_full.txt"), sep="\t", header=TRUE, stringsAsFactors=FALSE, quote = "\"", na.strings=c("","NA")))
    # Check if there are non duplicated tranlsations. If not, then delete the duplicated ones.
    if(any(duplicated(transmm_list[,"number"]))){
      transmm_list_dupl <- transmm_list[ duplicated( transmm_list[,"number"] ) | duplicated( transmm_list[,"number"], fromLast=TRUE ) ,];
      transmm_list_dupl <- transmm_list_dupl[order(transmm_list_dupl[,"number"]),]
      if( any(duplicated(transmm_list_dupl) & !duplicated(transmm_list_dupl)) ) {
        stop("Non duplicated entries in translation matrix.")
      } else {
        transmm_list <<- transmm_list[ !duplicated(transmm_list[,"number"]) ,]
      }
    }
  }
  
  if(gsub){
    transmm_list_tmp <- transmm_list[ grepl(paste(x,collapse="|") ,transmm_list[,"number"]) ,]
  }
  x <- replace.values(transmm_list[,"number"], transmm_list[,"name"], x, gsub=gsub)
  
  if(excel.format){
    filt <- substr(x,1,1)%in%c("=","+","-","/","*")
    x[filt] <- paste0(" ",x[filt])
  }
  return(x)
}

transl.ref.210row <- function(x, give.tab=FALSE){
  t1 <- matrix(c(
    "Weizen","00200"     ,"Roggen","00300"    ,"Korn_Dinkel","00400"     ,"Mischel_ua_Brot","00500"
    ,"Gerste","00600"     ,"Hafer","00700"    ,"Triticale","00800"     ,"Mischel_ua_Futt","00900"
    ,"Koernermais","01000"     ,"Koernermais_uebr","01100"    ,"Silomais","01200"     ,"Silomais_alt","01201"
    ,"Kartoffeln","01300"     ,"Zueckerrueben","01400"    ,"Futterrueben","01500"     ,"Futterrueben_alt","01501"
    ,"Raps","01600"     ,"Industrieraps","01700"    ,"Sojabohnen","01800"     ,"Sonnenblumen","01900"
    ,"Oelsaaten_uebr","02000"     ,"einj_nachw_Rohst","02100"    ,"Hanf","02200"     ,"Ackerbohnen","02300"
    ,"Eiweisserbsen","02400"     ,"Koernerlegum_uebr", "02500"    ,"Tabak","02600"     ,"Maschinenbohnen","02700"
    ,"Drescherbsen","02800"     ,"Maschinenkarotten","02900"    ,"Maschinenspinat","03000"     ,"Konservengemuese_uebr","03100"
    ,"Karotten_Freiland","03200"     ,"Zwiebeln_Freiland","03300"    ,"Kabis_Freiland","03400"     ,"Randen_Freiland","03500"
    ,"Blumenkohl_Freiland","03600"     ,"Freilandgemuese_uebr","03700"    ,"einj_Beeren","03800"     ,"einj_Gewuerz_u_Medizin","03900"
    ,"einj_gaertn_Freilandkult","04000"    ,"Spezkult_in_GH","04100"    ,"gaertn_Kult_in_GH","04200"     ,"Buntbrache","04400"
    ,"Rotationsbrache","04500"    ,"Rotationsbrache_alt","04501"    ,"Saum_auf_Ackerfl","04520"     ,"Bluehstreifen_ab2015","04530"
    ,"Ackerschonstreifen","04600"     ,"einj_Ackerkult_uebr","04700"    ,"Gruenland_LeistKost","06000"     ,"Futterbau_LeistKost_alt","06001"
    ,"Kunstwiesen","06100"     ,"oekolAusgl_Kunstwiesen_alt","06101"    ,"oekolAusgl_Naturwiesen_alt","06102"     ,"Wiesen_ext_auf_Aeck","06200"
    ,"Wiesen_ext","06300"     ,"Wiesen_wenig_intens","06400"    ,"Dauerwiesen_andere","06500"     ,"Uferwiesen","06550"
    ,"Weiden_ext","06600"     ,"Waldweiden","06700"    ,"Weiden","06800"     ,"Weiden_f_Schw","06900"
    ,"Heuwiesen_Alp1","07000"     ,"Heuwiesen_Alp2","07010"    ,"Heuwiesen_Alp3","07020"     ,"Gruenfl_uebr","07100"
    ,"Alpweiden_alt","07101"     ,"Wiesen_ZwiFu","07200"    ,"Reben","08000"     ,"Obst_ohne_Flaeche","08100"
    ,"Obst_mit_flaeche","08200"     ,"mehrj_Erdbeeren","08300"    ,"Himbeeren","08400"     ,"Strauchbeeren_uebr","08500"
    ,"mehrj_Gewuerz_Medizin","08600"     ,"Chinaschilf","08700"    ,"mehrj_nachw_Rohst","08800"    ,"Hopfen","08900"
    ,"mehrj_Spezkult_uebr","09000"    ,"Weihnachtsbaeume","09100"    ,"mehrj_gaertn_Freilandkult","09200"     ,"Dauerkult_uebr","09300"
    ,"Streue_Torfland","10000"     ,"HeckFeldUfer_Gehoelz","10100"    ,"uebr_Flaechen_LN","10200"     ,"Wald","10300"
    ,"Strohverkauf","10400"     ,"Pflanzenbau_nicht_zuteilbar","10500"  ), ncol=2, byrow=TRUE)
  rownames(t1) <- t1[,1]
  t1 <- t1[,2,drop=FALSE]
  colnames(t1) <- "code"
  
  if(give.tab) {
    return(t1)
  } else {
    return(replace.values(t1[,1], rownames(t1), x))
  }
}

# transl1.spb.330col.340row(c(2011,2012))
transl.spb.330col.340row <- function(x, reverse=FALSE, give.tab=FALSE, nice.names=FALSE){
  # This function translates the numbers from MML Tab 330 columns  to  Tab 340 rows (and vice versa if reverse=TRUE).
  # Arguments
  # x        = The number to be translated from one tab to another
  # reverse  = Translate from 340 row -> 330 col
  # give.tab = Return translation table without translating anything.
  
  #dput(as.numeric(unname(unlist(read.cb("no")))))
  #dput(unname(as.matrix(read.cb("no"))))
  t1 <- t(structure(c(2011L, 11000L, 2012L, 13000L, 2013L, 15000L, 2014L, 16000L, 2015L, 17000L, 2018L, 19000L, 2019L, 18000L, 2021L, 61000L,
                      2022L, 62000L, 2023L, 63000L, 2024L, 64000L, 2028L, 69000L, 2031L, 71000L, 2032L, 72000L, 2033L, 73000L, 2034L, 75000L, 2038L, 79000L,
                      2075L, 21000L, 2080L, 33000L, 2085L, 34000L, 2090L, 35000L, 2095L, 81000L, 2100L, 83000L), .Dim = c(2L, 23L)))
  colnames(t1) <- c("Tab330col", "Tab340row")
  rownames(t1) <- c("Milch","Muku","Kaelberm","Rindviehm","fremdesRindvieh","Rind/Kaelberm-3","uebrigesRindvieh","SchweineAllg","Schweinezucht","Schweinemast","Ferkelprod","Schweinemast-3","Konsumeier","Bruteier","Pouletmast","Truten","Gefluegel-3","Pferde","Schafe","Ziegen","sonstigeRaufu","Kaninchen","uebrigesGeflu")
  if(nice.names) rownames(t1) <- c("Milchkühe","Mutterkühe","Kälbermast","Rindviehmast","Haltung fremdes Rindvieh","Rinder- & Kälbermast für Dritte","übriges Rindvieh",
                                   "Schweine (Zucht & Mast)","Schweinezucht","Schweinemast","Arbeitsteilige Ferkelproduktion","Schweinemast f?r Dritte","Konsumeierproduktion","Bruteierproduktion","Pouletmast","Truten","Gefluegelmast für Dritte","Pferde","Schafe","Ziegen","Andere Raufutter verzehrende Tiere","Kaninchen","Übriges Geflügel")
  t1 <- t1[order(t1[,"Tab340row"]),]
  
  if(give.tab) {
    return(t1)
  }
  
  if(!reverse) col_in <- 1 else col_in <- 2
  if(!reverse) col_ou <- 2 else col_ou <- 1
  row_ou <- match(x,t1[,col_in])
  res <- t1[row_ou,col_ou]
  names(res) <- rownames(t1)[row_ou]
  return(res)
}

transl.spb.320row <- function(x, give.tab=FALSE){
  # This function translates the numbers from MML Tab 320 rows  to  the german names of the enterprises.
  # Arguments
  # x        = The number to be translated to enterprise name.
  # give.tab = Return translation vector without translating anything.
  
  # Import the data from MML: Copy the following columns of tab 320 side by side: 1) Zeile [UID, 2nd col], 2) Total Leistung [col no 4000], 3) all cols containing the culture names (no matter how many). They will be pasted together.
  #t1 <- read.cb("no"); t1[is.na(t1)] <- ""; t1 <- t1[t1[,2]!="",]; t1 <- cbind(t1[,1:2], apply(t1[,3:ncol(t1)],1,function(x)paste0(x,collapse="")), stringsAsFactors=FALSE); dput(as.numeric(t1[,1])); dput(t1[,3])
  
  t1 <- matrix(c(11110, 11120, 11130, 11140, 11180, 11310, 11320, 11330, 11340, 11380, 12100, 12200, 12500, 12600, 13000, 14100, 14300, 15100, 15200, 15300, 15900, 16100, 16200, 16900, 18100, 18200, 18300,
                 18400, 19000, 20000, 20100, 20200, 21100, 21200, 21300, 21400, 30100, 35000, 41000, 42100, 42200, 43000, 43500, 44000, 44500, 46000, 48000, 49000, 61000, 65000, 70000, 81000, 85000))
  colnames(t1) <- "code"
  rownames(t1) <- c("Weizen (Brotgetreide)", "Roggen (Brotgetreide)", "Dinkel (Brotgetreide)", "Emmer, Einkorn", "Mischel Brotgetreide", "Gerste (Futtergetreide)", "Hafer (Futtergetreide)", "Triticale (Futtergetreide)", "Futterweizen",
                    "Mischel Futtergetreide", "K?rnermais", "Silo- & Gr?nmais", "Saatmais", "Hirse", "Kartoffeln", "Zuckerr?ben", "Futterr?ben", "Raps zur Speise?lgewinnung", "Soja", "Sonnenblumen zur Speise?lgewinnung", "?brige ?lsaaten",
                    "Ackerbohnen zu Futterzwecken", "Eiweisserbsen zu Futterzwecken", "?brige K?rnerleguminosen", "Tabak", "Einj?hrige g?rtnerische Freilandkulturen", "Einj?hrige nachwachsende Rohstoffe", "Freiland Frischgem?se",
                    "Freiland Konservengem?se", "Einj?hrige Beeren (z.B Erdbeeren)", "Einj?hrige Gew?rz- & Medizinalpflanzen", "?brige Ackerkulturen", "Buntbrache", "Rotationsbrache", "Saum auf Ackerfl?che", "Ackerschonstreifen",
                    "Futterbau (ohne Silomais, Futterr?ben und Samenproduktion)", "Samenproduktion Futterbau", "Reben", "Obst, Streuobst ohne Fl?che", "Obstanlagen", "Mehrj?hrige Beeren", "Mehrj?hrige nachwachsende Rohstoffe",
                    "Hopfen", "Mehrj?hrige Gew?rz- & Medizinalpflanzen", "Gem?se-Dauerkulturen", "Christb?ume, Baumschulen und ?hnliches", "?brige Dauerkulturen", "Gew?chshaus- und Tunnel-Frischgem?se", "G?rtnerische Kulturen in gesch?tztem Anbau",
                    "Weitere Fl?chen innerhalb der LN", "Wald", "Weitere Fl?chen ausserhalb der LN")
  
  if(give.tab) {
    return(t1)
  } else {
    return(replace.values(t1[,1], rownames(t1), x))
  }
}

transl.EB.MML <- function(x, inverse=FALSE, give.tab=FALSE) {
  # Diese Funktion ?bersetzt die Merkmalsbezeichnungen zwischen Online-Erhebungsbogen und Merkmalsliste-ZA2015
  # if inverse=TRUE in andere Richtung: MML2015 -> EB
  
  if(!exists("uebers_tab", envir=globalenv())){
    #pfad_uebers <- "//evdad.admin.ch/AGROSCOPE_OS/2/5/2/1/4/3/4285/MML/transcoding_fuer_R/Umbenennung_UID_Reglen_Alle_Merkmale_uebersetzt.csv"
    pfad_uebers <- "//evdad.admin.ch/AGROSCOPE_OS/2/5/2/1/4/3/4285/MML/transcoding_fuer_R/transcoding_active.csv"
    uebers_tab <<- read.table(pfad_uebers, sep=";", header=TRUE, stringsAsFactors=FALSE, quote = "\"", na.strings=c("","NA","na","NULL","null","#DIV/0","#DIV/0!","#WERT","#WERT!"))
  }
  if(give.tab) return(uebers_tab)
  
  if(!inverse) col_in <- 1 else col_in <- 2
  if(!inverse) col_ou <- 2 else col_ou <- 1
  return(uebers_tab[match(x,uebers_tab[,col_in]),col_ou])
}

gsub.multi <- function(pattern, replacement, x, ...){
  # gsub implemented for pattern and replacement as vector arguments (recursive implementation).
  # Use: gsub.multiple(c("a","b","c"),c(1,2,3),letters)
  # Agruments: Please consult help page of gsub.
  if(length(replacement)==1) replacement <- rep(replacement, length(pattern))
  if(length(pattern)!=length(replacement)) stop("length(pattern) must be equal length(replacement).")
  
  if(length(pattern)>1) {
    newo <- order(nchar(pattern))
    pattern <- pattern[newo]
    replacement <- replacement[newo]
    return(gsub(pattern[1], replacement[1], gsub.multiple(pattern[-1], replacement[-1], x, ...), ...))
  } else {
    return(gsub(pattern, replacement, x, ...))
  }
}
gsub.multiple <- function(...){
  stop("gsub.multiple is now called gsub.multi! Please rename the function in script.")
}

repl.aou <- function(x){
  # Funktion, um ??? in Text wiederherzustellen. Wird bei read.table umgeschrieben.
  x <- gsub("Ã¤","?",x)
  x <- gsub("Ã¶","?",x)
  return( gsub("Ã¼","?",x) )
}

#x <- "Differenz = PrivatbezÃ¼ge fÃ¼r Privatkosten"
repl.utf8 <- function(x) {
  # This function recodes UTF8 (e.g. Ã¶ to ?) by using a translation matrix & gsub()
  
  pfad_utf8 <- "//evdad.admin.ch/AGROSCOPE_OS/2/5/2/1/3/3/4272/UTF8-ANSI/utf8-ansi.csv"
  if(!exists("utf8ansi", envir=globalenv())){
    # Uebersetzungstabelle utf8 & ANSI laden und vorbereiten
    utf8ansi <- as.matrix(read.table(pfad_utf8, sep=";", header=TRUE))
    # Leerzeichen entfernen.
    utf8ansi <- apply(utf8ansi,2,function(x)gsub(" ","",x))
    # Nach Anzahl Zeichen in UTF-8 Codierung sortieren, sonst gibt's bei gsub im Loop probleme
    utf8ansi <- utf8ansi[order(nchar(utf8ansi[,"utf8"]),decreasing=TRUE),]
    # Fuer Debugging hier x definieren.
    # x <- utf8ansi[,"utf8"]
    #? muss speziell codiert werden, damit es mit gsub funktioniert.
    utf8ansi <- apply(utf8ansi,2,function(x)gsub("\\?","\\\\\\?",x))
    # Diejenigen Codiereungen, die aus nur 1 Buchstaben bestehen, separat behandeln. Nicht verdichten
    utf8ansi_singlechar <- utf8ansi[nchar(utf8ansi[,3])==1,]
    utf8ansi <- utf8ansi[nchar(utf8ansi[,3])>1,]
    # Matrix verdichten, damit weniger Loops durchlaufen werden muessen
    utf8ansi.orig <- utf8ansi
    un1 <- sort(unique(utf8ansi[,"sign"]))
    utf8ansi <- utf8ansi[numeric(),c("sign","utf8")]
    
    for(i in 1:length(un1)){
      utf8ansi <- rbind(utf8ansi, c(un1[i], paste0(utf8ansi.orig[utf8ansi.orig[,"sign"]==un1[i],"utf8"],collapse="|")) )
    }
    utf8ansi <- rbind(utf8ansi,utf8ansi_singlechar[,c("sign","utf8")])
  }
  
  # Hier Uebersetzung
  for(i in 1:nrow(utf8ansi)){
    x <- gsub(utf8ansi[i,"utf8"],utf8ansi[i,"sign"], x)
  }
  # Ergebnis ausgeben
  #utf8ansi <<- rbind(utf8ansi,utf8ansi_singlechar[,c("sign","utf8")])
  return(x)
}

random.string <- function(n=1, length=10, capitals=2, numbers=3){
  # Create n random strings of specified number of characters (length), capital letters and numbers.
  if(n>1) return(replicate(n, random.string(n=1, length=10, capitals=2, numbers=3)))
  
  nlower <- length-capitals-numbers
  if(nlower<0) stop("capitals+numbers must be smaller equal length")
  
  if(nlower==0)   lower <- character() else  lower <- sample(letters,nlower,replace=TRUE)
  if(capitals==0) upper <- character() else  upper <- sample(LETTERS,capitals,replace=TRUE)
  if(numbers==0)  numbe <- character() else  numbe <- sample(0:9,numbers,replace=TRUE)
  return( paste(sample(c(lower,upper,numbe), length), collapse="") )
}

set.args <- function(string){
  # This function reads a string of arguments looking like:
  # y=y, trt=grouping, sig=sig, sig.level=sig.level ,digits=digits, median.mean="mean", ranked=ranked, print.result=FALSE
  # and sets the variables accordingly in the global environment. For function debugging.
  
  string <- gsub("\\=","<<-",string)
  string <- gsub(",",";",string)
  eval(parse(text=string))
}

#hms_to_sec(c("22:06:02","22:06:02"))
#sec_to_hms(hms_to_sec(c("22:06:02","22:06:02")))
hms.to.sec <- function(hms){
  # Convert time format hh:mm:ss to seconds
  return(as.numeric(substr(hms,1,2))*3600 +
           as.numeric(substr(hms,4,5))*60 +
           as.numeric(substr(hms,7,nchar(hms))))
}

#sec.to.hms(c(119000.5,4,3,2,2342234.4),1)
sec.to.hms <- function(sec, digits=0){
  # Convert seconds to time format hh:mm:ss (Digits are possible for seconds)
  h <- floor( sec / 3600 )
  m <- floor( sec%%3600 / 60 )
  s <- as.matrix(round( sec%%60 , digits))
  
  ind <- nchar(h )==1; h[ind] <- paste0("0",h[ind])
  ind <- nchar(m )==1; m[ind] <- paste0("0",m[ind])
  s1 <- sapply(strsplit(as.character(s),"\\."),function(x)x[[1]])
  ind <- nchar(s1)==1; s[ind] <- paste0("0",s[ind])
  
  return(paste(h,m,s,sep=":"))
}

covar <- coef.of.variance <- function(x,na.rm=TRUE) {
  sd(x,na.rm=na.rm)/mean(x,na.rm=na.rm)
}

####
find.deriv.0 <- function(expr, dat=c(-1e20, 1e20), variablename="x") {
  # This function derivates a given expression in form of a string (with 1 unknown variable x) and
  # returns the point where the slope equals zero.
  
  # If values of variables are passed into the function just use paste0()
  # coefs <- c(2, -0.5)
  # expr <- paste0(coefs[1],"*x + ", coefs[2], "*x^2")
  # find.deriv.0(expr)
  # curve(coefs[1]*x +  coefs[2]*x^2, 0, 5)
  # abline(v=find.deriv.0(expr))
  
  # dat can be given (optional), in order to determin the interval where the solution is searched.
  
  d_expr <- D( parse(text=expr) , name=variablename)
  f <- function(x)  eval(d_expr)
  res <- uniroot(f, c(min(dat), max(dat)) )$root
  names(res) <- "deriv0"
  return(res)
}

if(FALSE){
  undebug(find.deriv.0)
  
  # Beide Varianten funktionieren:
  expr <- "1.0338e-01*x - 1.0941e-02*x^2"
  find.deriv.0(expr)
  expr <- expression( 1.0338e-01*x - 1.0941e-02*x^2 )
  find.deriv.0(expr)
  # Diese nicht:
  find.deriv.0( 1.0338e-01*x - 1.0941e-02*x^2 )
}

####
deriv.value <- function(expr, at, variablename="x", times=1) {
  # This function calculates the value of the derivative of a given expression (expr)
  # at a given point "at"
  # expression must be a String!
  # times = number of derivations
  
  # Falls times <= 0, wird nichts gemacht. Dann muesse man die Funktion eigentlich auch gar nicht verwenden...
  if(times<=0) {
    d_expr <-  parse(text=expr)
    
    # Sonst wird abgeleitet
  } else {
    d_expr <- D( parse(text=expr) , name=variablename)
    if(times>1) {
      for(i in 1:(times-1)) {
        d_expr <- D( d_expr, name=variablename)
      }
    }
  }
  assign( variablename,  at)
  
  return( eval(d_expr) )
}
if(FALSE){
  expr <- "x+x^2"
  deriv.value(expr, at=3, times=2)
  expr <- "x+x^2+x^3"
  deriv.value(expr, at=10, times=2)
}

approx.own <- function(x,y,xout) {
  if(length(xout)>1) return(sapply(xout,function(xout1)approx.own(x,y,xout1)))
  if(all(is.na(x)) || all(is.na(y))) return(rep(NA, length(xout)))
  
  isnaxy <- is.na(x)|is.na(y)
  if(any(isnaxy)){
    x <- x[!isnaxy]
    y <- y[!isnaxy]
  }
  
  if(length(x)==1){
    return(y)
  } else if(all(xout<x)) {
    return(y[which.min(x)])
  } else if(all(xout>x)) {
    return(y[which.max(x)])
  } else {
    return(approx(x=x,y=y,xout=xout)$y)
  }
}

lpsolve <- function(obj_coef, A, LHS_ge=NULL, LHS_le=NULL, opt_val_ge=NULL, opt_val_le=NULL,  maximize=FALSE) {
  # This function transforms a given optimization model in a not very intuitive and rather special form and solves with the
  # functions initProbCLP(), setObjDirCLP(), loadProblemCLP() and solveInitialCLP() of the package clpAPI.
  # Keywords: Linear Programming, LP
  
  # Arguments:
  # obj_coef = coefficients of the objective function
  
  # A = matrix containing the restriction coefficients
  #       nrow(A) = no. of restrictions
  #       ncol(A) = no. of coefficients
  
  # LHS_ge = "Left hand side greater equal ..."
  #       vector of values of the Right Hand Side that should be exceeded
  # LHS_le = "Left hand side lower equal ..."
  #       vector of values of the Left Hand Side that should NOT be exceeded
  # One of LHS_ge or LHS_le must be given!
  
  # opt_val_ge = "optimal value greater equal"
  #       value of objective function that should be exceeded
  # opt_val_le = "optimal value lower equal"
  #       value of objective function that should NOT be exceeded
  # opt_val_ge and opt_val_le are optional arguments!
  
  # maximize = TRUE  -> the objective function is maximized
  # maximize = FALSE -> the objective function is minimized
  
  if(!is.null(dim(obj_coef))) obj_coef <- as.vector(obj_coef)
  if(ncol(A)!=length(obj_coef)) stop("ncol(A) != length(obj_coef)")
  is.null.LHS_ge <- is.null(LHS_ge)
  is.null.LHS_le <- is.null(LHS_le)
  is.null.opt_val_ge <- is.null(opt_val_ge)
  is.null.opt_val_le <- is.null(opt_val_le)
  if(is.null.LHS_ge & is.null.LHS_le) stop("Specify either LHS_ge (left hand side >= b) or LHS_le (left hand side <= b)")
  if(!is.null.LHS_ge){
    if(!is.null(dim(LHS_ge))) LHS_ge <- as.vetor(LHS_ge)
    if(length(LHS_ge)!=nrow(A)) stop("length(LHS_ge) != nrow(A)")
  }
  if(!is.null.LHS_le) {
    if(!is.null(dim(LHS_le))) LHS_le <- as.vetor(LHS_le)
    if(length(LHS_le)!=nrow(A)) stop("length(LHS_le) != nrow(A)")
  }
  if(!is.null.opt_val_ge) {
    if(length(opt_val_ge)==1) opt_val_ge <- rep(opt_val_ge, ncol(A))
    if(length(opt_val_ge)!=ncol(A)) stop("length(opt_val_ge) != ncol(A)")
  }
  if(!is.null.opt_val_le) {
    if(length(opt_val_le)==1) opt_val_le <- rep(opt_val_le, ncol(A))
    if(length(opt_val_le)!=ncol(A)) stop("length(opt_val_le) != ncol(A)")
  }
  
  library(clpAPI)
  if(maximize) minmax <- (-1) else minmax <- 1
  lp <- initProbCLP() # Create LP object
  setObjDirCLP(lp, minmax) # 1 for minimization. -1 for maximization.
  nrows  <- nrow(A); ncols  <- ncol(A)
  rlower <- LHS_ge      # Lower row bound. Use for >= restriction on LHS
  rupper <- LHS_le      # Upper row bound. Use for <= restriction on LHS
  clower <- opt_val_ge  # Upper column bound. Use for >= restriction on optimization coefficients.
  cupper <- opt_val_le  # Lower column bound. Use for <= restriction on optimization coefficients.
  
  # constraint matrix (left hand side). Convert matrix into vector. Give row and column indices for every vector place.
  ia <- c(row(A))-1 # Has to start with row 0, not row1. Otherwise you will get an error!
  ja <- seq(0,length(ia)+nrow(A),nrow(A))
  ar <- c(A) # Convert A matrix to a vector.
  if(FALSE){
    nc <- c( apply(A,2,function(x){result<-rep(FALSE,length(x)); if(any(xn0<-x!=0)) result[min(which(xn0))] <- TRUE; return(result)}) )
    ar0 <- ar==0
    ar <- ar[!ar0]
    ia <- ia[!ar0]
    nc <- nc[!ar0]
    nc <- which(nc)-1; nc <- c(nc,length(ar))
    ja <- nc
  }
  
  # load problem data
  loadProblemCLP(lp=lp, ncols=ncols, nrows=nrows, ia=ia, ja=ja, ra=ar, lb=clower, ub=cupper, obj_coef=obj_coef, rlb=rlower, rub=rupper)
  solveInitialCLP(lp) # Solve the LP.
  
  result <- list()
  result$lp <- lp
  result$opt_val <- getColPrimCLP(lp)
  result$opt_sol <- getObjValCLP(lp)
  result$info <-  paste("getSolStatusCLP(lp)   # Retrieve solve status of LP",
                        "getObjValCLP(lp)      # Retrieve optimal (minimal/maximal) value of objective function.",
                        "getColPrimCLP(lp)     # Retrieve the primal values of the structural variables (columns) after optimization.",
                        "getColDualCLP(lp)     # Retrieve the dual values of the structural variables (columns) after optimization (reduced costs).",
                        "delProbCLP(lp)        # remove problem object",
                        sep="\n")
  
  # lp am Schluss loeschen. Wenn man mehrere Optimierungen nacheinander ausfuehrt, kommt es sonst zu Fehlern im solver.
  delProbCLP(lp)
  return(result)
}


## Dealing with Swisstopo coordinates in the AGIS-Data

# From International N,E  to  Swiss y,x
wgs84.to.ch1903 <- function(N, E){
  # This function convert the he international WGS84 coordinates (N, E) to
  # CH1903 coordinates (that are e.g. used in the AGIS-data)
  
  # N = Vektor mit c(Grad, Minuten, Sekunden)   Nord      oder Grad mit Dezimalstellen
  # E = Vektor mit c(Grad, Minuten, Sekunden)   Ost       oder Grad mit Dezimalstellen
  #
  
  # Swisstopo Beispiel
  # N <- c(46,  2, 38.87)
  # E <- c( 8, 43, 49.79)
  # Resultat
  # y = 700000 , x = 100000
  
  if(length(N)==1){   x0 <- N
  } else {            x0 <- N[1]*3600 + N[2]*60  + N[3]   }
  x1 <- (x0 - 169028.66) / 10000
  
  if(length(N)==1){   y0 <- E
  } else {            y0 <- E[1]*3600 + E[2]*60  + E[3]   }
  y1 <- (y0 - 26782.5) / 10000
  
  x <- 200147.07 + 308807.95 * x1 + 3745.25 * y1^2 + 76.63 * x1^2 + 119.79 * x1^3 - 194.56 * y1^2 * x1  # x0; x1;  x
  y <- 600072.37 + 211455.93 * y1 - 10938.51 * y1 * x1 - 0.36 * y1 * x1^2 - 44.54 * y1^3  # y0;  y1;  y
  
  x <- round(x)
  y <- round(y)
  
  res <- c(y=y, x=x)
  return(res)
}

# From Swiss y,x  to  International N,E
ch1903.to.wgs84 <- function(y, x,  output=c("decimal", "minsec")) {
  # This function converts the CH1903 coordinates (that are e.g. used in the AGIS-data)
  # to the international WGS84 coordinates (N, E).
  # y = y coordinates
  # x = x coordinates
  # Note that the coordinates METER_X and METER_Y in the AGIS data are probably interchanged.
  
  # Swisstopo Beispiel
  # x <- 100000
  # y <- 700000
  # Resultat
  # N = 46? 02' 38.86"
  # E =  8? 43' 49.80"
  # Ist korrekt.
  
  output <- match.arg(output)
  
  y0 <- (y-600000)/1000000
  x0 <- (x-200000)/1000000
  
  y1 <-  2.6779094 + 4.728982 * y0 + 0.791484 * y0 * x0 + 0.1306 * y0 * x0^2 - 0.0436 * y0^3
  x1 <- 16.9023892 + 3.238272 * x0 - 0.270978 * y0^2 - 0.002528 * x0^2 - 0.0447 * y0^2 * x0 - 0.0140 * x0^3
  
  E <- y1*100/36
  N <- x1*100/36
  
  if(output=="decimal") {
    res <- list()
    res$coord <- c(N=N, E=E)
    res$full <- paste0(round(N, 10), " N,   ", round(E, 10), " E")
    return(res)
  }
  
  Eg <- floor(E)
  Em <- floor( (E-Eg) * 60 )
  Es <- round( ( (E-Eg) * 60 - Em ) * 60  ,2)
  
  Ng <- floor(N)
  Nm <- floor( (N-Ng) * 60 )
  Ns <- round( ( (N-Ng) * 60 - Nm ) * 60  ,2)
  
  res <- list()
  res$N <- c(deg=Ng, min=Nm, sec=Ns)
  res$E <- c(deg=Eg, min=Em, sec=Es)
  res$full <- paste0(
    Ng, "?", Nm, "'", Ns, "'' N,   ",
    Eg, "?", Em, "'", Es, "'' E"
  )
  return(res)
}

# Veraltet und primitiv! Funktion eins weiter unten verwenden!
googlemaps.url <- function(coord, zoom=8, browse=FALSE){
  # Create googlemaps URL and open it in broswer if browse=FALSE
  # zoom is not implementet at the moment.
  
  url <- paste0("http://maps.google.com/?q=",coord[1],",",coord[2]) #, ",", zoom, "z")
  if(!browse)  return(url)
  browseURL(url)
}

# Frauenfeld auf googlemaps:
# googlemaps_url( ch1903_to_wgs84(709726, 268273)$coord , browse=TRUE)

if(FALSE){
  # N <- gb2[1:10,"N_KOORD"]
  # E <- gb2[1:10,"E_KOORD"]
  # create.googlemaps.markers(N=gb2[,"N_KOORD"], E=gb2[,"E_KOORD"], labels=gb2[,"ArbVerd_jeFJAE"])
  n <- 1000
  N <- rnorm(n, 47.07343  ,0.2); E <- rnorm(n, 7.68218  ,0.5);
  #N <- 47.06; E <- 8.00
  labels="";
  N.center=NULL; E.center=NULL
  zoom=9; map.type=c("HYBRID","ROADMAP","SATELLITE","TERRAIN"); angle=0;
  point.symbol <- c("CIRCLE","BACKWARD_CLOSED_ARROW","BACKWARD_OPEN_ARROW", "FORWARD_CLOSED_ARROW", "FORWARD_OPEN_ARROW")[1]; point.size=2.5; point.lwd=0.5; point.col.in="red"; point.col.out="black"; point.opacity=0.7; del.tmp.file=TRUE
  
  create.googlemaps.markers(N, E, map.type="HYBRID", point.opacity=1, #point.symbol=sample(c("CIRCLE","BACKWARD_CLOSED_ARROW","BACKWARD_OPEN_ARROW", "FORWARD_CLOSED_ARROW", "FORWARD_OPEN_ARROW"), length(N), replace=TRUE),
                            point.size= 2+2*scale.extreme(1:n), point.col.in=color.gradient(1:n), cat.html=FALSE , del.tmp.file=TRUE)
}

create.googlemaps.markers <- function(N, E, labels="", N.center=NULL, E.center=NULL, zoom=9, map.type=c("HYBRID","ROADMAP","SATELLITE","TERRAIN"), angle=0,
                                      point.symbol=c("CIRCLE","BACKWARD_CLOSED_ARROW","BACKWARD_OPEN_ARROW", "FORWARD_CLOSED_ARROW", "FORWARD_OPEN_ARROW")[1], point.size=2.5, point.lwd=0.5, point.col.in="red", point.col.out="black", point.opacity=0.7, del.tmp.file=TRUE, cat.html=FALSE){
  # N = N coords of points
  # N = E coords of points
  # labels = optional labels of points
  # N.center = N coords of map center
  # E.center = E coords of map center
  #
  # Choose
  # N.center = 47.06, E.center=8.00
  # For the approximated middle of Switzerland.
  #
  # zoom = zoom factor
  # and other options to costumize the map. Just try.
  # Hint: use point.size   = 2+2*scale.extreme(...) and
  #           point.col.in = color.gradient(...)   for nice results.
  
  
  if(is.null(N.center)) N.center <- (min(N, na.rm=TRUE) + max(N, na.rm=TRUE))/2
  if(is.null(E.center)) E.center <- (min(E, na.rm=TRUE) + max(E, na.rm=TRUE))/2
  
  map.type <- match.arg(map.type)
  if(zoom<18 & angle!=0) warning("angle!=0 only works if zoom>18. Even then it's not available for all regions of the world.", immediate.=TRUE)
  
  point.symbol.choice <- c("CIRCLE","BACKWARD_CLOSED_ARROW","BACKWARD_OPEN_ARROW", "FORWARD_CLOSED_ARROW", "FORWARD_OPEN_ARROW")
  if(any(!point.symbol%in%point.symbol.choice)) stop(paste0("point.symbol must be one of ", paste(point.symbol.choice, collapse=", ")))
  
  LN <- length(N)
  stopifnot( length(labels)==1 | length(labels)==LN )
  stopifnot( length(point.symbol)==1 | length(point.symbol)==LN )
  stopifnot( length(point.size)==1 | length(point.size)==LN )
  stopifnot( length(point.lwd)==1 | length(point.lwd)==LN )
  stopifnot( length(point.col.in)==1 | length(point.col.in)==LN )
  stopifnot( length(point.col.out)==1 | length(point.col.out)==LN )
  stopifnot( length(point.opacity)==1 | length(point.opacity)==LN )
  # Pferformance Verlgeich:
  #system.time(for(i in 1:1000000) LN)
  #system.time(for(i in 1:1000000) length(N))
  
  variable.arguments <- c("labels","point.symbol","point.size","point.lwd","point.col.in","point.col.out","point.opacity")
  length1 <- logical(length=length(variable.arguments)); names(length1) <- variable.arguments
  for(i in 1:length(variable.arguments)){
    length1[i] <- length(get(variable.arguments[i]))==1
  }
  char.vars <- c("labels","point.col.in","point.col.out")
  
  point.symbol <- paste0("google.maps.SymbolPath.", point.symbol)
  
  #### Erst wird das Skript erstellt
  
  text <- paste0(
    "
    <!--
    Herkunft des Beispiels:
    https://developers.google.com/maps/documentation/javascript/examples/icon-complex
    Alles was mit
    shape: shape,
    zu tun hat, einfach aus dem Code l?schen.
    -->
    
    <!DOCTYPE html>
    <html>
    <head>
    <meta name='viewport' content='initial-scale=1.0, user-scalable=no'>
    <meta charset='utf-8'>
    <title>Koordinaten Uebersicht</title>
    <style>
    html, body, #map-canvas {
    height: 100%;
    margin: 0px;
    padding: 0px
}
</style>
<script src='https://maps.googleapis.com/maps/api/js?v=3.exp'></script>
<script>

function initialize() {
var mapOptions = {
zoom: ",
zoom, ",\n",
"center: new google.maps.LatLng(",N.center,", ", E.center, "),\n",
"mapTypeId: google.maps.MapTypeId.", map.type, "\n",
"}
var map = new google.maps.Map(document.getElementById('map-canvas'),  mapOptions);
map.setTilt(", angle ,");

setMarkers(map, locationProperties);
}
"
  )
  # Debugging
  # cat(text)
  
  # Nun werden die Koordinaten der Punkte ins Skript eingefuegt:
  orders <- 1:length(N)
  # Matrix erzeugen, die alle Kennzahlen enth?lt
  info <- cbind(orders, N, E, labels, point.symbol, point.size, point.lwd, point.col.in, point.col.out, point.opacity)
  # NAs entfernen
  info <- info[!is.na(info[,"N"]),,drop=FALSE]
  # Fehler ausgeben und matrix printen, wenn es noch andere NAs drin hat:
  errors <- apply(info,1,function(x)any(x%in%c("NA", "NaN")))
  if(any(errors)){
    print(info[errors,], quote=FALSE)
    warning("There are NA values in one or more of the arguments. Check the above printed matrix or invisible function output.")
    return(invisible(info[errors,]))
  }
  # Alle String arguments fuer JavaScript mit '' versehen
  info[,char.vars] <- paste0("'", info[,char.vars],"'")
  # Alle Spalten entfernen, die immer dieselben sind.
  info1 <- info[,!colnames(info)%in%names(length1)[length1] ,drop=FALSE]
  
  js.matrix <- function(x){
    mat <- apply(x,1,function(x)paste("[", paste(x,collapse=", "), "],"))
    mat <- paste0(mat, collapse="\n")
    mat <- substr(mat, 1, nchar(mat)-1)
    mat <- paste0("[\n",mat,"\n];\n")
    return(mat)
  }
  
  text2 <- paste0( text, "\nvar locationProperties = ", js.matrix(info1))
  # Debugging
  # cat(text2)
  
  # Rest des Sktips schreiben (inkl. Aussehen der Punkte.)
  text4 <- paste0(
    text2,
    "
    function setMarkers(map, locations) {
    // Add markers to the map
    
    // Marker sizes are expressed as a Size of X,Y
    // where the origin of the image (0,0) is located
    // in the top left of the image.
    
    for (var i = 0; i < locations.length; i++) {
    var locationProperty = locations[i];
    var myLatLng = new google.maps.LatLng(locationProperty[1], locationProperty[2]);
    
    var image = {
    path: ",if(length1["point.symbol"]) info[1,"point.symbol"] else paste0("locationProperty[",which(colnames(info1)%in%"point.symbol")-1,"]"), ",
    scale: ",if(length1["point.size"]) info[1,"point.size"] else paste0("locationProperty[",which(colnames(info1)%in%"point.size")-1,"]"), ",
    strokeWeight: ",if(length1["point.lwd"]) info[1,"point.lwd"] else paste0("locationProperty[",which(colnames(info1)%in%"point.lwd")-1,"]"), ",
    fillColor: ",if(length1["point.col.in"]) info[1,"point.col.in"] else paste0("locationProperty[",which(colnames(info1)%in%"point.col.in")-1,"]"), ",
    strokeColor: ",if(length1["point.col.out"]) info[1,"point.col.out"] else paste0("locationProperty[",which(colnames(info1)%in%"point.col.out")-1,"]"), ",
    fillOpacity: ",if(length1["point.opacity"]) info[1,"point.opacity"] else paste0("locationProperty[",which(colnames(info1)%in%"point.opacity")-1,"]"), "
    };
    
    var marker = new google.maps.Marker({
    position: myLatLng,
    map: map,
    icon: image,
    title: ",if(length1["labels"]) info[1,"labels"] else paste0("locationProperty[",which(colnames(info1)%in%"labels")-1,"]"), ",
    zIndex: locationProperty[0]
    });
    }
    }
    
    google.maps.event.addDomListener(window, 'load', initialize);
    
    </script>
    </head>
    <body>
    <div id='map-canvas'></div>
    </body>
    </html>
    "
  )
  # Debugging:
  if(cat.html) cat(text4)
  
  
  
  #### Skript ist fertig
  
  # Tempor?re Datei erstellen
  write.table(text4, paste0(tempdir(),"/googlemaps_tmp.html"), col.names=FALSE, row.names=FALSE, quote=FALSE)
  # ?ffnen
  browseURL(paste0(tempdir(),"/googlemaps_tmp.html"))
  
  if(!del.tmp.file){ # length(unique(labels))>1 & labels[1]!="" |
    cat("File is stored in the folder:   ", tempdir(),"\\\n", sep="")
    cat("                    filename:   googlemaps_tmp.html\n" )
    cat("Use\nfile.remove(paste0(Sys.getenv('TMP'),'\\\\googlemaps_tmp.html'))\nto delete the file when you don't need it anymore\n")
    
    # Wenn keine Labels gesetzt wurden, macht es keinen Sinn, die Datei l?nger zu behalten.
    # Es wird 8 Sekunden gewartet, bis der Browser die Datei sicher ge?ffnet hat. Dann wird die tempor?re Datei wieder gel?scht.
  } else {
    Sys.sleep(8)
    if( file.remove(paste0(Sys.getenv('TMP'),'\\googlemaps_tmp.html')) ) cat("File was removed. If you want to use it, set argument del.tmp.file=TRUE")
  }
  }

if(FALSE){
  N <- 47.07343; E <- 7.68218; language="de"
  get.googlemaps.adress(N=47.07343, E=7.68218)
  get.googlemaps.adress(N=47.07343, E=7.68218, result="splitted")
}

get.googlemaps.adress <- function(N, E, result=c("full", "splitted"), language="de"){
  # This function finds the approximate adress of a coordinate.
  adr <- unlist( read.table( paste0("http://maps.googleapis.com/maps/api/geocode/json?latlng=", N, "," ,E,"&language=",language), sep=";", stringsAsFactors=FALSE) )
  
  result <- match.arg(result)
  extract.adr <- function(x)  return(unname(substr(x, 2+gregexpr(":", x)[[1]][1], nchar(x)-1)))
  
  if(result=="full"){
    return(   extract.adr( adr[grep("formatted_address",adr)[1]] )   )
  } else {
    res <- character()
    res["country"] <- extract.adr( adr[grep("country",adr)[1]-2] )
    res["admin_area_lvl1"] <-  extract.adr( adr[grep("administrative_area_level_1",adr)[1]-2] )
    res["admin_area_lvl2"] <-  extract.adr( adr[grep("administrative_area_level_2",adr)[1]-2] )
    res["postal_code"] <- extract.adr( adr[grep("postal_code",adr)[1]-2] )
    res["locality"] <-  extract.adr( adr[grep("locality",adr)[1]-2] )
    res["route"] <-  extract.adr( adr[grep("route",adr)[1]-2] )
    res["street_number"] <-  extract.adr( adr[grep("street_number",adr)[1]-2] )
    return(res)
  }
}

if(FALSE){
  N <- 47; E <- 8
  get.googlemaps.elevation(N=N, E=E)
}
get.googlemaps.elevation <- function(N, E){
  adr <- unlist( read.table( paste0("http://maps.googleapis.com/maps/api/elevation/json?locations=", N, "," ,E), sep=";", stringsAsFactors=FALSE) )
  extract.adr <- function(x)  return(unname(substr(x, 2+gregexpr(":", x)[[1]][1], nchar(x)-1)))
  return( round(as.numeric(extract.adr( adr[grep("elevation",adr)[1]] ))) )
}

if(FALSE){
  N <- 47.07343; E <- 7.68218
  N <- rep(N, 2); E <- rep(E, 2)
  get.googlemaps.slopes(N=47.07343, E=7.68218)
}
get.googlemaps.slopes <- function(N, E){
  # In dieser Funktion mehrere Positionen in der Naehe des Orts abfragen (gleichzeitig)
  # Dann die absolute Hoehendifferenz zwischen dem Mittelpunkt und allen aeusseren Punkten aufsummieren.
  # z.B. Kreisform, oder Sternform mit bestimmem Radius.
  
  # google search:   polygonal city boundaries
  #                  worldwide polygonal city boundaries
  #                  gis database locality boundaries
  # http://wiki.openstreetmap.org/wiki/AT/Gemeinden
  # http://www.openstreetmap.org/relation/16239
  # http://www.gadm.org/                             GADM database of Global Administrative Areas
  # http://www.naturalearthdata.com/
  # SRTM-Daten ( http://de.wikipedia.org/wiki/SRTM-Daten )
  # GRASS GIS - http://grass.osgeo.org/
  # Using GRASS with R - http://grasswiki.osgeo.org/wiki/R_statistics
  
  # http://stackoverflow.com/questions/8135243/finding-towns-within-a-10-mile-radius-of-postcode-google-maps-api
  # http://www.mullie.eu/geographic-searches/
  #  // convert latitude/longitude degrees for both coordinates
  #  // to radians: radian = degree * pi / 180
  #  $lat1 = deg2rad($lat1);
  #  $lng1 = deg2rad($lng1);
  #  $lat2 = deg2rad($lat2);
  #  $lng2 = deg2rad($lng2);
  #
  #  // calculate great-circle distance
  #  $distance = acos(
  #    sin($lat1) * sin($lat2) +
  #      cos($lat1) * cos($lat2) *
  #      cos($lng1 - $lng2)
  #  );
  #
  #  // distance in human-readable format:
  #    // earth's radius in km = ~6371
  # $distance = 6371 * $distance;
  
  
  # Runden um Zeichen zu sparen
  N <- round(N, 5)
  E <- round(E, 5)
  # Erst Adresse zusammenstellen. Darf maximal 2048 Zeichen haben. mit nchar pruefen.
  coord <- paste0(N, ",", E)
  coord <- paste(coord, collapse="|")
  
  # URL Vorlage:
  # http://maps.googleapis.com/maps/api/elevation/json?locations=39.7391536,-104.9847034|36.455556,-116.866667
  
  url <- paste0("http://maps.googleapis.com/maps/api/elevation/json?locations=", coord)
  if(nchar(url)>2048) {
    print(url, quote=FALSE)
    stop("URL darf maximal 2048 Zeilen lang sein.")
  }
  
  adr <- unlist( read.table( url , sep=";", stringsAsFactors=FALSE) )
  # extract.adr <- function(x)  return(unname(substr(x, 2+gregexpr(":", x)[[1]][1], nchar(x)-1)))
}

####
# pattern="B20"; replace="BH20"; recursive=TRUE;
# gsub.only.dirs("//evdad.admin.ch/AGROSCOPE_OS/2/5/2/1/2/4278/StatSkripte/", pattern="BH20", replace="B20", recursive=TRUE)
gsub.only.dirs <- function(path=".", pattern, replace, recursive=FALSE) {
  # This function renames all directories and if recursive=TRUE all subdirectories of a given path.
  # pattern & replace shall be used as is used in the gsub() function.
  
  # Find all files
  allfiles <- list.files(path, full.names=TRUE, recursive=recursive)
  # Count maximal number of subdirectories by replacing all slashes and counting the difference.
  nsubdirs <- max(nchar(allfiles)-nchar(gsub("/", "", allfiles)))
  
  # Prepare vector to indicate if the renaming worked.
  worked.fil <- worked.log <- l.name1 <- NULL
  
  # Loop over all subdirectories.
  i <- 1
  for(i in nsubdirs:1){
    
    # Rename the highest subdirectory level, then go one step downwards and rename the next subdirectory level, again and again
    # until all directories are renamed.
    remove.downwards.dirs <- function(char){
      splitchar <- unlist(strsplit(char, "/"))
      until <- max( length(splitchar)-i , 1)
      return( paste(splitchar[ 1:until ], collapse="/") )
    }
    
    name1 <- unique( apply(matrix(allfiles),1,function(x)remove.downwards.dirs(x)) )
    # Shorten directoy vector such that only dirs are renamed that containt pattern.
    name1 <- name1[grepl(pattern, name1)]
    l.name1 <- c(l.name1, length(name1) )
    
    # Replace pattern.
    name2 <- gsub(pattern, replace, name1)
    
    # Rename directories and store information about wheter file.rename() was successful.
    worked.fil <- c(worked.fil, name1)
    worked.log <- c(worked.log, file.rename(name1, name2) )
  }
  
  # Display warning if pattern was not found in any directory and end function
  if(all(l.name1)==0) {
    warning("Pattern was not found. No directories renamed.", call.=FALSE, immediate.=TRUE)
  } else {
    
    # Look for files where it didn't work.
    worked.fil <- unique(worked.fil[!worked.log])
    # See if in any of them there was the pattern to search an replace
    # worked.fil <- worked.fil[grepl(pattern, worked.fil)]
    # If there is one, display a warning.
    if(length(worked.fil)>0) {
      warning("Not all directories could be renamed. See function output.", call.=FALSE)
      return(worked.fil)
    } else {
      cat("All directories with occurencies successfully renamed.\n")
    }
  }
}

# gsub.only.files("//evdad.admin.ch/AGROSCOPE_OS/2/5/2/1/2/4278/StatSkripte/", pattern="B20", replace="BH20", recursive=TRUE)
gsub.only.files <- function(path=".", pattern, replace, recursive=FALSE) {
  # This function renames all files within a directory (and all files in subdirectories if recursive=TRUE).
  # pattern & replace shall be used as is used in the gsub() function.
  
  allfiles <- list.files(path, full.names=TRUE, recursive=recursive)
  name1 <- allfiles[ !file.info(allfiles)$isdir ]
  # Shorten file vector such that only dirs are renamed that containt pattern.
  name1 <- name1[grepl(pattern, name1)]
  
  # Display warning if pattern wasnt found and end function without doing anything.
  if(length(name1)==0) {
    warning("Pattern was not found. No files renamed.", call.=FALSE, immediate.=TRUE)
  } else {
    
    # Replace pattern.
    name2 <- gsub(pattern, replace, name1)
    
    # Prepare vector to indicate if the renaming worked & rename files.
    # Rename directories and store information about wheter file.rename() was successful.
    worked.fil <- name1
    worked.log <- file.rename(name1, name2)
    
    # Look for files where it didn't work.
    worked.fil <- unique(worked.fil[!worked.log])
    
    # If there is one, display a warning.
    if(length(worked.fil)>0) {
      warning("Not all files could be renamed. See function output.", call.=FALSE)
      return(worked.fil)
    } else {
      cat("All files with occurencies successfully renamed.\n")
    }
    
  }
}
####

# gsub.dirs.and.files("//evdad.admin.ch/AGROSCOPE_OS/2/5/2/1/2/4278/", pattern="B20", replace="BH20", recursive=TRUE)
# gsub.dirs.and.files("//evdad.admin.ch/AGROSCOPE_OS/2/5/2/1/2/4278/", pattern="BH20", replace="B20", recursive=TRUE)
# gsub.dirs.and.files("//evdad.admin.ch/AGROSCOPE_OS/2/5/2/1/2/4278/StatSkripte", pattern="BH20", replace="B20", recursive=TRUE)
# gsub.dirs.and.files("//evdad.admin.ch/AGROSCOPE_OS/2/5/2/1/2/4278/StatSkripte", pattern="B20", replace="BH20", recursive=TRUE)

gsub.dirs.and.files <- function(path=".", pattern, replace, recursive=FALSE) {
  # This function renames all directories and all files within a directory (and all files in subdirectories if recursive=TRUE).
  # pattern & replace shall be used as is used in the gsub() function.
  gsub.only.dirs(path, pattern, replace, recursive)
  gsub.only.files(path, pattern, replace, recursive)
}

#
rename.files <- function(path="."){
  name1 <- list.files("//evdad.admin.ch/AGROSCOPE_OS/2/5/2/1/2/4278/", pattern="BH20", full.names=TRUE, recursive=TRUE)
  name2 <- gsub("BH20", "B20", name1)
  file.rename(from=name1, to=name2)
  
  file.rename(from="//evdad.admin.ch/AGROSCOPE_OS/2/5/2/1/2/4278//SekDaten/Betr_B/AWP/BH2014/", to="//evdad.admin.ch/AGROSCOPE_OS/2/5/2/1/2/4278//SekDaten/Betr_B/AWP/B2014/")
  
  file.rename(from="./Betr_B/AWP/BH2014/001_AuswahlplanRef_Formel_Seite2.pdf", to="./Betr_B/AWP/B2014/001_AuswahlplanRef_Formel_Seite2.pdf")
}
####

# file <- "//evdad.admin.ch/AGROSCOPE_OS/2/5/2/2/3583/Resultate/16-06-06/2014/allcosts_info.RData";  setwd.to.file(file)
# file <- "C:/Users/U80823148/_/ME/ME_data_out/data_final/allcosts_info.RData"; setwd.to.file(file)
setwd.to.file <- function(file){
  if(length(file)>1) stop("file must containt only one filename.")
  fil <- strsplit(file, "/|\\\\")[[1]]
  fil <- fil[-length(fil)]
  setwd(paste(fil,collapse="/"))
}

####

# Beispiel und Parametereinstellung unter der Funktion!
stratif.sqrt.f.rule <- function(x, ngrp, interval){
  # Diese Funktion stratifiziert eine Population (teilt eine Population in Gruppen auf),
  # sodass moeglichst wenige Beobachtungen fuer eine Genaue Schaetzung gebraucht werden(?).
  # Siehe Cochran (1977): Sampling Techniques. 3rd edition. Wiley. p127-130
  #
  # Argumente
  #
  # x = variable of interest according to which the sample should be stratified
  # ngrp =     Anzahl Schichten/Gruppen
  # interval = Genauigkeit, mit welcher die Grenze gebildet werden soll bzw.
  #            Intervalle, mit denen die Daten geteilt werden.
  #
  # Value (Ergebnis) = Vector of values of x that should be used for stratifying.
  # Replace first/last value of vecor with -Inf/Inf and stratify by using the function g1roup.by.fix.scale()
  
  # Fehlende Werte enfternen
  x <- x[!is.na(x)]
  
  # Anzahl Betriebe in den einzelnen Intervallen berechnen
  tab <- table(ceiling(x/interval)*interval)
  names_tab <- as.numeric(names(tab))
  # Cumulative Summe der Wurzel berechnen
  cusu <- cumsum(sqrt(tab))
  # Division points berechnen
  maxcusu <- max(cusu)
  divpoint <-  maxcusu/ngrp
  divpoint <- seq(divpoint, maxcusu, length.out=ngrp)
  divpoint <- divpoint[-length(divpoint)]  # letzten Punkt weglassen, da dieser keine Grenze sein kann
  
  # Naechste Intervall-Punkte zu den Division points finden
  res <- apply(as.matrix(divpoint),1,function(x) names_tab[which.min(abs(x-cusu))] )
  res <- c(min(x), res, max(x))
  # Ergebnis ausgeben
  return(res)
}
if(FALSE){
  ## Beispiel aus Buch nachbilden und ausprobieren.
  # Siehe Cochran (1977): Sampling Techniques. 3rd edition. Wiley. p127-130
  x <- c(rep(2.5,3464), rep(7.5,2516), rep(12.5,2157), rep(17.5,1581), rep(22.5,1142), rep(27.5,746),
         rep(32.5,512), rep(37.5,376), rep(42.5,265), rep(47.5,207), rep(52.5,126), rep(57.5,107),
         rep(62.5,82), rep(67.5,50), rep(72.5,39), rep(77.5,25), rep(82.5,16), rep(87.5,19), rep(92.5,2), rep(97.5,3))
  ngrp <- 5; interval <- 5
  borders <- stratif.sqrt.f.rule(x,5,5)
  borders[1] <- -Inf; borders[length(borders)] <- Inf
  grouping <- g1roup.by.fix.scale(x=x, selection.levels=borders)
  table(grouping)
  
  ## Daten erzeugen
  x0 <- 1:1300
  x <- x0 + rnorm(length(x0),0,seq(1,20,length.out=length(x0)))
  x <- round(x, 1)
  interval=100; ngrp=5
  stratif.sqrt.f.rule(x,5,20)
}

#mclapply.own(1:100, function(x)return(x*2), type="PSOCK")
#mclapply.own(1:100, function(x)return(x*y+1), type="PSOCK")
#type <- "PSOCK"; mc.cores=8; X <- as.list(1:100); values <- X; FUN <- function(x)return(x*2)
mclapply.own <- function(X, FUN, mc.cores=parallel::detectCores(), type=c("PSOCK", "FORK", "MPI")){
  # This function does multi core processing of lapply (parLapply)
  # The function depends on following packages
  # parallel, snow, Rmpi
  # mc.cores = getOption("mc.cores", 8)
  if(!grepl("return",paste0(deparse(FUN),collapse="")))
    stop(paste0("FUN must explicitly return the result by using return(). The entered function looks like this\n",
                "    ",paste0(deparse(FUN),collapse=""), "\n",
                "  But it should look like this:\n",
                "    function(x)return(x*2)"))
  
  type <- match.arg(type)
  
  require.package(parallel)
  cl <- makeCluster(min(length(X), mc.cores), type=type)
  
  #res <- tryCatch({
  #  parLapply(cl, X=X, fun=FUN)
  #}, finally = {
  #  stopCluster(cl)
  #  if(type=="MPI") mpi.exit()
  #})
  
  res <- parLapply(cl, X=X, fun=FUN)
  stopCluster(cl)
  if(type=="MPI") mpi.exit()
  return(res)
}

## Define the hack
mclapply.hack <- function(...) {
  ## A script to implement a hackish version of
  ## parallel:mclapply() on Windows machines.
  ## On Linux or Mac, the script has no effect
  ## beyond loading the parallel library.
  
  # http://www.stat.cmu.edu/~nmv/2014/07/14/implementing-mclapply-on-windows
  # http://www.stat.cmu.edu/~nmv/setup/mclapply.hack.R
  require.package(parallel)
  
  ## Create a cluster
  size.of.list <- length(list(...)[[1]])
  cl <- makeCluster( min(size.of.list, detectCores()) )
  
  ## Find out the names of the loaded packages
  loaded.package.names <- c(
    ## Base packages
    sessionInfo()$basePkgs,
    ## Additional packages
    names( sessionInfo()$otherPkgs ))
  
  tryCatch( {
    
    ## Copy over all of the objects within scope to
    ## all clusters.
    this.env <- environment()
    while( identical( this.env, globalenv() ) == FALSE ) {
      clusterExport(cl,
                    ls(all.names=TRUE, env=this.env),
                    envir=this.env)
      this.env <- parent.env(environment())
    }
    clusterExport(cl,
                  ls(all.names=TRUE, env=globalenv()),
                  envir=globalenv())
    
    ## Load the libraries on all the clusters
    ## N.B. length(cl) returns the number of clusters
    parLapply( cl, 1:length(cl), function(xx){
      lapply(loaded.package.names, function(yy) {
        require.package(yy , character.only=TRUE)})
    })
    
    ## Run the lapply in parallel
    return( parLapply( cl, ...) )
  }, finally = {
    ## Stop the cluster
    stopCluster(cl)
  })
}

## If the OS is Windows, set mclapply to the
## the hackish version. Otherwise, leave the
## definition alone.
if(FALSE) mclapply <- switch( Sys.info()[['sysname']],
                              Windows = {mclapply.hack},
                              Linux   = {mclapply},
                              Darwin  = {mclapply})

## end mclapply.hack.R



#### OUTLIER DETECION ####
#x <- c(1,2,3,6,5,7,4,2,4,5,6,7,22); p <- 0.05
#remove.outliers(x,p)
normal.outliers <- function(x, p=0.01) {
  # The outliers of a normal distribution are displayed (TRUE, FALSE).
  pvals <- pnorm(x,mean(x,na.rm=TRUE),sd(x,na.rm=TRUE))
  outliers <- pvals < p/2   |   pvals > 1-p/2
  return(outliers)
}
quantile.outliers <- function(x, p=0.01) {
  # The outliers that are defined by the given quantile (p/2) are displayed.
  outliers <- x < quantile(x,p/2)   |   x > quantile(x,1-p/2)
  return(outliers)
}
sd.outliers <- function(x, no.sd=3){
  # The outliers that are above/below  mean(x)+sd(x) / mean(x)-sd(x)  are displayed
  mean.x <- mean(x,na.rm=TRUE)
  sd.x <- sd(x,na.rm=TRUE)
  outliers <- x < mean.x-sd.x | x>mean.x+sd.x
  return(outliers)
}
#which(normal.outliers(1:1000))
#which(quantile.outliers(1:1000))
#which(sd.outliers(1:1000))

if(FALSE) { data=prepareMvOutlData(cost[cost[,"Leist_Saat_Fl"]==0 & index==bz,checkCols]); p.val=0.025; method=c("absMahaDistIncrease","quantile","chisq")[1]; max.p.outl=0.15; na.action=c("median","mean","remove")[1]; make.plot=TRUE }
mahalanobis.outliers <- function(data, p.val=0.025, method=c("absMahaDistIncrease","quantile","chisq"), max.p.outl=0.15,
                                 na.action=c("median","mean","remove"), make.plot=FALSE, plotText=NULL){
  # Define multivariate outliers by mahalanobis distance
  method <- match.arg(method)
  if(is.data.frame(data)) data <- as.matrix(data)
  na.action <- match.arg(na.action)
  is.na.data <- is.na(data)
  if(any(is.na.data)){
    if(na.action=="remove"){
      data <- na.omit(data)
    } else {
      fillnas <- function(x){
        if (na.action=="mean") mean.x <- mean(x,na.rm=TRUE)
        if (na.action=="median") mean.x <- median(x,na.rm=TRUE)
        x[is.na(x)] <- mean.x
        return(x)
      }
      data <- apply(data,2,function(x)fillnas(x))
    }
  }
  # Prepare legend for plot
  if(make.plot){
    legMat <- as.data.frame(matrix(NA, nrow=5, ncol=4)); colnames(legMat) <- c("legend","lty","pch","col")
    legMat[,"legend"] <- c("normal","outlier","regrCrit","diffCrit","plotText")
    legMat[,"lty"] <- c(0,0,2,1,0)
    legMat[,"pch"] <- c(21,20,NA,NA,NA)
    legMat[,"col"] <- c("black","black","red","black",NA)
    if(method!="absMahaDistIncrease") legMat <- legMat[-c(3,4),]
    if(!is.null(plotText)) legMat["5","legend"] <- plotText else legMat <- legMat[-nrow(legMat),]
    drawLegend <- function() legend("topleft", legend=legMat[,"legend"], lty=legMat[,"lty"], pch=legMat[,"pch"], col=legMat[,"col"], bty="n");
  }
  
  maha <- mahalanobis(data,colMeans(data),cov(data))
  if(method=="quantile") {
    q1 <- quantile(maha,1-p.val)
    outliers <- maha > q1
    if(make.plot) {plot(sort(maha), main=paste0("Mahalanobis outliers,\nmethod=\"quantile\", p.val=",p.val) ); drawLegend(); abline(h=q1)}
  } else if(method=="chisq") {
    p.chisq <- pchisq(q=maha,df=ncol(data)) # df ist richtig. Laut ETH Folien zu "mutivariate outliers"
    outliers <- p.chisq > 1-p.val
    if(make.plot) {plot(sort(p.chisq), main=paste0("Mahalanobis outliers,\nmethod=\"chisq\", p.val=",p.val) ); drawLegend(); abline(h=1-p.val)}
    warning("Attention! By choosing method='chisq' you assume that all variables follow a normal distribution.")
  } else if(method=="absMahaDistIncrease"){
    # Order data according to mahalanobis distance
    #Sort & resort works like this: maha1 <- c(1,3,2,10,5,0); om <- order(maha1); oMaha <- maha1[om]; dput(oMaha[order(om)])
    om <- order(maha)
    oMaha <- unname( maha[om] )
    # Calculate the differences between ordered mahalanobis distances. Where the difference is larger than a certain quantile, it's a potential outlier.
    dMaha <- c(0,diff(oMaha))
    qdMaha <- quantile(dMaha, 1-p.val)
    diffFilt <- (dMaha > qdMaha)
    if(any(diffFilt)){
      diffFilt[which(diffFilt)[1]:length(diffFilt) ] <- TRUE
      # Calculate a regression for the left part of the distribution until the quantile <regr.quant>
      rMaha <- rank(oMaha)
      regr.quant <- 0.75
      lmFilt <- oMaha < quantile(oMaha, regr.quant)
      coef <- lm(oMaha[lmFilt]~rMaha[lmFilt])$coefficient[2]
      # If a value is <regr.diff.fact> times higher than it would be estimated with the regression, and is in the right side of the distribution it is a potential outlier.
      regr.diff.fact <- 2.3
      regrFilt <- (oMaha / (rMaha * coef)) > regr.diff.fact & rMaha>floor(length(rMaha)/2)
    }
    
    # Combine the criteria from 'estimated mahalanobis from regression' and 'increasing differences of mahalanobis distance'.
    outliers <- (diffFilt & regrFilt)[order(om)]
    names(outliers) <- names(maha)
    
    col <- rep("white",length(maha)); col[outliers] <- "black"
    if(make.plot) {
      plot(oMaha, main=paste0("Mahalanobis outliers,\nmethod=\"absMahaDistIncrease\", p.val=",p.val), ylab="Mahalanobis distance" )
      lines(oMaha, col=col[om], type="p", pch=20)
      drawLegend()
      if(any(diffFilt)) abline( h=min(oMaha[diffFilt]), lty=1)
      if(any(regrFilt)) abline( h=min(oMaha[regrFilt]), lty=2, col="red")
    }
  }
  return(outliers)
}
#rev(ma( rev(dMaha), n=floor(length(dMaha)/20) ))
#ma <- function(x,n=5){filter(x,rep(1/n,n), sides=1)}

####
remove.minmax.outliers <- function(x,remove=1,lower=NULL,upper=NULL) {
  if(is.null(upper)&is.null(lower)) {
    if(remove==0) return(x) else {
      for(i in 1:remove) {
        x[c(which(x==max(x,na.rm=TRUE))[1],which(x==min(x,na.rm=TRUE))[1])] <- NA
      }
      return(x)
    }
  } else if(!is.null(upper)&!is.null(lower)&is.null(remove)) {
    for(i in 1:lower) {
      x[which(x==min(x,na.rm=TRUE))[1]] <- NA
    }
    for(i in 1:upper) {
      x[which(x==max(x,na.rm=TRUE))[1]] <- NA
    }
    return(x)
  } else {stop("set either remove or lower and upper but not the three arguments together")}
}

#### DATA CHECK ####

#data <- matrix(c(rep(1,20),sample(1:20,10),sample(1:20,10),sample(1:20,10),sample(1:20,10)),nrow=10); data[10,] <- 2;
#highly.correlated(data,0.5,result="colnames"); highly.correlated(data,0.5,result="colnumbers");
highly.correlated1 <- function(data,level=0.8,digits=2,result=c("colnames","colnumbers","kick.out.logical")) {
  # Explanation: This function returns a table of variables which correlate more than the specified level.
  # use result= to specify if colnames or colnumbers are returned. Use data[,-unique(function.output[,2])] to kick out the highly correlated columns (attention. that doesn't work properly)
  result <- result[1]
  if(is.null(colnames(data))) colnames(data) <- 1:ncol(data)
  corrs <- cor(data)
  diag(corrs) <- 0
  select.corrs <- row(corrs)-col(corrs)
  highly.corr <- which(abs(corrs[])>level&select.corrs>0,arr.ind=TRUE)
  Variable1 <- Variable2 <- character(0)
  Correlation <- numeric(0)
  if(result=="colnames") {
    Variable1 <- colnames(data)[highly.corr[,2]]
    Variable2 <- colnames(data)[highly.corr[,1]]
    Correlation <- round(corrs[highly.corr],digits)
  } else {
    Variable1 <- highly.corr[,2]
    Variable2 <- highly.corr[,1]
    Correlation <- round(corrs[highly.corr],digits)
    kick.out.variable1 <- 1:ncol(data)%in%unique(Variable1)
    kick.out.variable2 <- 1:ncol(data)%in%unique(Variable2)
  }
  if(result!="kick.out.logical") {
    results <- cbind(Variable1,Variable2,Correlation);  colnames(results) <- c("Variable 1","Variable 2","Correlation")
    if(nrow(results)<1) results[1,] <- NA
  } else {results <- rbind(kick.out.variable1,kick.out.variable2); rownames(results) <- c("few","many")}
  return(results)
}

####
highly.correlated2 <- function(d, cutoff) {
  # r2test
  if (cutoff > 1 || cutoff <= 0) {
    stop(" 0 <= cutoff < 1")
  }
  if (!is.matrix(d) && !is.data.frame(d)) {
    stop("Must supply a data.frame or matrix")
  }
  r2cut = sqrt(cutoff);
  cormat <- cor(d);
  bad.idx <- which(abs(cormat)>r2cut,arr.ind=T);
  bad.idx <- matrix( bad.idx[bad.idx[,1] > bad.idx[,2]], ncol=2);
  drop.idx <- ifelse(runif(nrow(bad.idx)) > .5, bad.idx[,1], bad.idx [,2]);
  if (length(drop.idx) == 0) {
    1:ncol(d)
  } else {
    (1:ncol(d))[-unique(drop.idx)]
  }
}

####

####
check.variable.constance <- function(data,grouping=NULL,test=c("any","all"),level=0.5,nvalues=NULL,result=c("logical","colnumbers","colnames"), matrix.output=c(FALSE,"full","relevant")) {
  # Explanation: Function to check if more than level*nrow(data) values are the same in one variable. Can also be applied
  # group-wise to check if more than level*length(group) variables are constant. If you chose nvalues=NULL,  all values which occur more than 2 times in the variable
  # are counted. If you specify nvalues=1, e.g. only 1 constant value is counted. If matrix.output=TRUE only logical output is possible.
  test <- test[1]; result <- result[1]; matrix.output <- matrix.output[1]
  if(is.null(dim(data))) { data <- matrix(data); colnames(data) <- "checkvariable" }
  if(is.null(grouping))  grouping <- rep(1,nrow(data))
  if(is.null(nvalues))
    constance <- function(x,level) sum(table(x)[table(x)>1]) > length(x)*level
  if(!is.null(nvalues))
    constance <- function(x,level) {
      tablex <- table(x); lengthtablex <- length(tablex)
      sum(tablex[1:min(nvalues,lengthtablex)] [ tablex[1:min(nvalues,lengthtablex)]>1 ]) > length(x)*level
    }
  kick.outs <- apply(data,2,function(y)tapply(y,grouping,function(y)constance(y,level)))
  
  if(matrix.output==FALSE) {
    if(all(grouping==1)) kick.out <- rbind(kick.outs,kick.outs)
    if(test=="any") kick.out <- apply(kick.outs,2,function(x) any(x))
    if(test=="all") kick.out <- apply(kick.outs,2,function(x) all(x))
    if(result=="logical") {     results <- kick.out
    } else if(any(kick.out)) {
      if(result=="colnumbers")  results <- which(kick.out)
      if(result=="colnames")    results <- names(kick.out[kick.out])
    } else results <- NA
  } else if(matrix.output=="full") { results <- kick.outs
  } else if(matrix.output=="relevant") {
    results <- kick.outs[, apply(kick.outs,2,function(x) any(x)) ]
    if(ncol(results)<1) results <- NA
  }
  return(results)
}
####

#data <- matrix(1:100,nrow=10); data[5:10,] <- 1; grouping <- c(rep(1,5),rep(2,5)); #sds <- matrix(sample(c(rep(0,50),rep(1,50)),100),nrow=10)
#check.group.sd(data,grouping,matrix.output=TRUE)
check.group.sd <- function(data,grouping=NULL,test=c("all","any",factor=10)[1],result=c("logical","colnumbers","colnames"),matrix.output=c(FALSE,"full","relevant")) {
  # Explanation: Use this function to check if there are any variations in your groups.
  # test=all: report, if all standard deviations are 0, test=any: report if any sd of a group is 0, test=10 report if the sd of a group is more than 10 times the sd of another.
  # matrix.output=TRUE : report, if standard deviation in group is 0 (only logical output possible)
  result <- match.arg(result);
  matrix.output <- match.arg(matrix.output);
  if(is.null(grouping)) grouping <- rep(1,nrow(data))
  sds <- apply(data,2,function(x)tapply(x,grouping,function(y)sd(y,na.rm=TRUE)))
  sds[is.na(sds)] <- 0
  if(matrix.output==FALSE){
    if(test=="any") { kick.out <- apply(sds,2,function(x)  any(x==0) )
    } else if(test=="all") { kick.out <- apply(sds,2,function(x) all(x==0) )
    } else { test <- as.numeric(test); kick.out <- apply(sds,2,function(x) max(x)>test*min(x) ) }
    if(result=="logical") { results <- kick.out
    } else if(any(kick.out)) {
      if(result=="colnumbers") results <- which(kick.out)
      if(result=="colnames") results <- names(kick.out[kick.out])
    } else results <- NA
    
  } else if(matrix.output=="full")     { results <- sds==0
  } else if(matrix.output=="relevant") {
    semi.result <- sds==0
    results <- semi.result [, apply(sds,2,function(x) any(x==0) )]
    if(ncol(results)<1) results <- NA
  }
  return(results)
}

#### DATA MANIPULATION ####
#
# DOESN'T WORK!
#rmall <- function(){
#  rm(list=ls());
#  source("F:/Mobile Software/R/own functions/func.R")
#}
#
# DOESN't WORK!
#keep <- function(keepnames){
#  #keepnames <- "a"
#  if(!is.character(keepnames)) stop("Names to keep must be character")
#  allobj <- ls()
#  retrn <- allobj[!allobj%in%keepnames]
#  retrn <- retrn[!retrn%in%c("keepnames","allobj")]
#  return(retrn)
#}
#
#


clean.number.format <- function(dat, to.numeric=TRUE) {
  # This funciton cleans wrongly formatted data. E.g. from 1.000,00 to 1000.00
  # dat = the data.frame/matrix to be formatted
  # to.numeric = should numeric columns that where originally interpreted as characters be coerced to numeric()?
  
  # Schauen, in welchen Spalten keine Buchstaben drinstehen, aber ' oder,
  # Debugging
  #x <- c("bxfbs","1e+01","10'00","10,0",1)
  #!grepl("[a-zA-Z]",x) & grepl("'|,",x)
  col <- !sapply(dat,function(x)any(grepl( "[a-zA-Z]" , x ))) & sapply(dat,function(x)any(grepl( "'|," , x )))
  
  
  # Falsche , und Tausender-Trennzeichen korrigieren
  dat[,col] <- lapply(dat[,col],function(x){
    if( any(grepl(".",x)&grepl(",",x)) )  # Schritt 1: Format 1.000,00 -> 1000,00
      x <- gsub("\\.","",x)
    x <- gsub(" |'","", # Schritt 2: Format 1 000.00 oder 1'000.00 -> 1000.00
              gsub(",",".",x) ) # Schritt 3: Format 1000,00 -> 1000.00
  })
  
  return(dat)
}

clean.data.columns <- function(dat){
  # This function deletes duplicated columns and removes some part of the colnames that result from SQL execution with Java (instead of BO).
  
  # Leere Spalten entfernen
  dat <- dat[,!substr(colnames(dat),1,3)%in%paste0("X.",0:9)]
  # Doppelte Spalten entfernen
  #dat <- dat[,!conames(dat)%in%paste0(rep(colnames(dat),each=10),".",0:9)]
  # Teile von datltennahmen entfernen
  colnames(dat) <- gsub(pattern=paste0(
    c(paste0("SUM\\.BM_BE_WERT_",c("01","02","03","04","05","06","07","08","09",10:99),"\\."),
      paste0("SUM\\.RM_BE_WERT_",c("01","02","03","04","05","06","07","08","09",10:99),"\\."),
      "\\.GEWICHT\\.GEWICHT\\."),
    collapse="|"),replacement="",
    x=colnames(dat))
  # Fertige Datei ausgeben.
  return(dat)
}

if(FALSE){
  x <- cbind(ID=as.character(1:25), BZ=LETTERS[1:25], 1:25)
  char.cols.to.num(x)
  summary(char.cols.to.num(x))
}
char.cols.to.num <- function(x, checkrowsForInteger=NULL, stringsAsFactors=FALSE){
  # This function checks in all cols of a data.frame if they can be coerced to numeric without producing NA values.
  # If it's possible the col is coerced to numeric with as.numeric()
  
  if(is.matrix(x)) {
    rownames(x) <- NULL
    x <- as.data.frame(x, stringsAsFactors=stringsAsFactors)
  }
  if(is.vector(x)) {
    names(x) <- NULL
    x <- as.data.frame(as.list(x))
  }
  rn <- rownames(x)
  cn <- colnames(x)
  
  naT <- function(x){x[is.na(x)] <- TRUE; return(x)}
  if(!is.null(checkrowsForInteger) && checkrowsForInteger>nrow(x)) checkrowsForInteger <- nrow(x)
  res <- as.data.frame(lapply(x, function(x) if(is.character(x) || (!is.null(checkrowsForInteger) && all(naT(round(x[1:checkrowsForInteger])==x[1:checkrowsForInteger]))) ) type.convert(as.character(x),as.is=!stringsAsFactors) else x), stringsAsFactors=stringsAsFactors)
  #res <- as.data.frame(lapply(x,function(x)if(is.character(x)) type.convert(x,as.is=!stringsAsFactors) else x), stringsAsFactors=stringsAsFactors)
  #res <- as.data.frame(lapply(x, function(x) if( is( tryCatch(as.numeric(x[1:checkrowsForInteger]),error=function(e)e,warning=function(w)w), "warning") ) return(x) else return(as.numeric(x))    ), stringsAsFactors=stringsAsFactors)
  rownames(res) <- rn
  colnames(res) <- cn
  return(res)
}

numeric.cols <- function(x, checkrows=100) {
  # Identify numeric columns
  checkrows <- min(nrow(x),checkrows)
  return(unname(
    sapply(x,function(x) !is( tryCatch(as.numeric(x[1:checkrows]),error=function(e)e,warning=function(w)w), "warning") )
  ))
}



#categ.to.bin <- function(categ, names,  unique.categ=sort(unique(categ)), output.cont=c("binary","logical"), output.str=c("matrix","df")){
#  # s3.names <- c("Ackb","Spez","VM","MuKu","AnRi","PfSZ","Vere","VMAc","KoMu","KoVe","KoAn")
#  # s3.numbers <- c(11, 12, 21, 22, 23, 31, 41, 51, 52, 53, 54)
#  output.cont <- match.arg(output.cont)
#  output.str <- match.arg(output.str)
#  if(length(unique.categ)!=length(names)) stop("each element of 'unique.categ' must have a respective element in 'names'")
#  result <- matrix(NA,nrow=length(categ),ncol=length(names))
#  for(i in 1:length(unique.categ)){
#    result[,i] <- categ==unique.categ[i]
#  }
#  if(output.cont=="binary") result <- matrix(as.numeric(result),nrow=length(categ))
#  if(output.str=="dr") result <- as.data.frame(result)
#  colnames(result) <- names
#  return(result)
#}
####
if(FALSE){
  x <- sample(1:5, 100, replace=TRUE)
  varname="var"; sep.sign="_"
  categ.to.bin(x)
}
categ.to.bin <- function(x, varname="var", sep.sign="_", allnames=NULL){
  
  if(length(dim(x))>1 || is.list(x)) stop("x must be a vector or array with dim(x)<=1")
  
  sux <- sort(unique(x))
  lux <- length(sux)
  ord <- array(0, dim=c(length(x), ncol=lux));
  if(is.null(allnames)){
    colnames(ord) <- paste(varname, sux, sep=sep.sign)
  } else {
    if(length(allnames)!=lux) stop("length(allnames) != length(unique(x))")
    colnames(ord) <- allnames
  }
  
  for(i in 1:lux){
    ord[x==sux[i],i] <- 1
  }
  return(ord)
}
####
if(FALSE){
  x <- sample(1:5, 100, replace=TRUE)
  zone.numbers <- c(11,     21,       22,    31,    41,       51,      52,      53,      54)
  x <- sample(zone.numbers, 100, replace=TRUE)
  varname="var"; sep.sign="_"
  cbind(categ.to.ordinal(x, varname="Zone"), Zone=x)
}
categ.to.ordinal <- function(x, varname="var", sep.sign="_", allnames=NULL){
  sux <- sort(unique(x))
  lux <- length(sux)
  ord <- array(0, dim=c(length(x), ncol=lux));
  if(is.null(allnames)){
    colnames(ord) <- paste(varname, sux, sep=sep.sign)
  } else {
    if(length(allnames)!=lux) stop("length(allnames) != length(unique(x))")
  }
  
  for(i in 1:lux){
    ord[x>=sux[i],i] <- 1
  }
  return(ord)
}

####
wide.to.long.df <- function(fixed=NULL, transform, rawIndex=colnames(transform), colnameIndex="index", colnameValue="value"){
  # Diese Funktion erstellt aus einem Breiten data.frame, wo die Betriebszweige in verschiedenen Spalten stehen einen
  # langen, wo alle rawIndex untereinander stehen und indexiert sind.
  # Das ist vor allem noetig fuer die Konversion von Datensatzen im Skript 020_aggreg_ME_param.R
  #
  # fixed = Datensatz welcher die Betriebsinformationen enthaelt (ID und Jahr)
  #         Kann auch NULL sein. Dann werden keine Zusatzinfos angehaengt.
  # transform = Datensatz, in welchem alle rawIndex nebeneinander in Spalten stehen
  # rawIndex = Betriebszweigs-Index-Rohling
  # colnameIndex = Name der Betriebszweigs-Index-Spalte im neuen Datensatz
  # colnameValue = Name der neuen Spalte, in die die Werte reingeschrieben werden.
  
  # Verbesserungsvorschlag:
  # Die table class bietet einen deutlich einfacheren weg. Evtl. das umsetzen.
  #x <- array(NA, dim=c(3,5,2), dimnames=list(letters[1:3],letters[11:15],c("y","z")))
  #z <- x[,,1]; class(z) <- "table"
  #data.frame( z )
  
  # Tests
  if(!is.null(fixed) && is.null(dim(fixed))) stop("The argument 'fixed' must be a data.frame or matrix. No vector.")
  if(is.null(dim(transform))) stop("The argument 'fixed' must be a data.frame or matrix. No vector.")
  if(length(colnameIndex)!=1) stop("length(colnameIndex) must be equal 1.")
  if(length(colnameValue)!=1) stop("length(colnameValue) must be equal 1.")
  if(colnameIndex==colnameValue) stop("colnameIndex==colnameValue is not allowed. Please choose another colname.")
  
  # End-Datensatz erstellen
  if(!is.null(fixed)) {
    res <- suppressWarnings(data.frame(rep.rows.1b1(fixed, length(rawIndex)), rep(rawIndex,nrow(transform)), 0, stringsAsFactors=FALSE))
  } else {
    res <- data.frame(rep(rawIndex,nrow(transform)), 0, stringsAsFactors=FALSE)
  }
  colnames(res)[(ncol(res)-1):ncol(res)] <- c(colnameIndex, colnameValue)
  
  # Umwandlung des Datensatzes
  choose <- (0:(nrow(transform)-1))*ncol(transform)
  for(i in 1:ncol(transform)){
    res[choose+i,colnameValue] <- transform[,i]
  }
  rownames(res) <- NULL
  return(res)
}

long.to.wide.df <- function(dat, colnameFixed=c("ID","Jahr"), colnameIndex="index", colnameValue="Franken"){
  # Diese Funktion erstellt aus einem langen Datensatz (mit vielen Zeilen), wo alle rawIndex untereinander stehen
  # und indexiert sind einen breiten data.frame, wo die Betriebszweige in verschiedenen Spalten.
  # Das ist vor allem noetig fuer die Konversion von Datensatzen im Skript 020_aggreg_ME_param.R
  #
  # dat = langer Datensatz in indexierter Form
  # colnameFixed = Spalten welche die Betriebsinformationen enthalten (ID und Jahr)
  #                Kann auch NULL sein. Dann werden keine Zusatzinfos angehaengt.
  # colnameIndex = Name der Betriebszweigs-Index-Spalte in dat
  # colnameValue = Name der Spalte in dat, welche die Werte enthaelt 
  
  urawIndex <- unique(dat[,colnameIndex])
  lurawIndex <- length(urawIndex)
  
  # Zu extrahierende Zeilen auswaehlen und End-Datensatz erstellen
  choose <- which(dat[,colnameIndex]==dat[1,colnameIndex])-1
  transform <- matrix(0, ncol=lurawIndex, nrow=length(choose))
  colnames(transform) <- urawIndex
  # Fehlerpruefung
  check <- c(choose,NA)-c(NA,choose); check <- check[!is.na(check)]
  if(any(check!=check[1])) stop("The entries in colnameIndex are not in uniform order for all observertions of colnameFixed.")
  rm(check)
  
  # Umwandlung des Datensatzes
  for(i in 1:ncol(transform)){
    transform[,i] <- dat[choose+i,colnameValue]
  }
  res <- data.frame(dat[ seq(1,nrow(dat)-lurawIndex+1,lurawIndex) , colnameFixed ], transform)
  rownames(res) <- NULL
  return(res)
}
####

perc <- function(x,digits=1){
  round(x*100,digits)
}

perc.change <- function(x,digits=1){
  round(x*100-100,digits)
}

if(FALSE){
  x <- data.frame(a=seq(0.1, 1, 0.1) , b=c(1:10))
  x <- data.frame(a=seq(0.1, 1, 0.1) , b=c(1:9, 10.001))
  x <- as.matrix(x)
  x <- as.list(x)
  x <- data.frame(a=seq(0.1, 1, 0.1) , b=rep("a", 10),stringsAsFactors=FALSE)
  #x <- x[,2]
  perc.selective(x)
  digits=1; limits=c(0,1); margin=2
}
###
perc.selective <- function(x, digits=1, limits=c(0,1), add.name="(%)", margin=2){
  # Transform all decimal columns/rows in a matrix/data.frame into percentages
  # But only if all elements of the columns/rows are between the limits. This allows distinction between
  # Decimal numbers and other numbers that vary in a greater range.
  if(length(limits)==1) {
    limits.orig <- abs(limits)
    limits <- numeric()
    limits[1] <- -limits.orig;  limits[2] <- limits.orig
  }
  
  # Rekursive Funktionsdefinition im Fall, dass !is.null(dim(x))
  if(!is.null(dim(x))) {
    if(is.matrix(x)) {
      add.perc.colname <- apply(x,margin,function(x) is.numeric(x) && all( x[!is.na(x)]<=limits[2] & x[!is.na(x)]>=limits[1] ))
      res <- apply(x,margin,function(x)perc.selective(x=x,digits=digits,limits=limits,margin=margin))
      colnames(res)[ add.perc.colname ] <- paste0(colnames(res)[ add.perc.colname ], add.name)
      return(res)
    }
    if(is.data.frame(x)) {
      add.perc.colname <- unlist(lapply(x,function(x) is.numeric(x) && all( x[!is.na(x)]<=limits[2] & x[!is.na(x)]>=limits[1] )))
      res <- as.data.frame( lapply(x,function(x)perc.selective(x=x,digits=digits,limits=limits,margin=margin)) ,stringsAsFactors=FALSE)
      colnames(res)[ add.perc.colname ] <- paste0(colnames(res)[ add.perc.colname ], add.name)
      return(res)
    }
  }
  if(is.list(x)) {
    add.perc.colname <- unlist(lapply(x,function(x) is.numeric(x) && all( x[!is.na(x)]<=limits[2] & x[!is.na(x)]>=limits[1] )))
    res <- lapply(x,function(x)perc.selective(x=x,digits=digits,limits=limits,margin=margin))
    names(res)[ add.perc.colname ] <- paste0(names(res)[ add.perc.colname ], add.name)
    return(res)
  }
  
  if(!is.numeric(x)) return(x)
  if(all(is.na(x))) return(x)
  
  
  if(all( x[!is.na(x)]<=limits[2] & x[!is.na(x)]>=limits[1] )) {
    return( round(100*x,digits=digits) )
  } else  {
    return(x)
  }
}

signif.equally <- function(x,digits=3,max=1,margin=2) {
  # Round all elements of vector x to the same number of significant digits
  # margin is only used when a dataframe/matrix is given. Then apply() is used.
  if(!is.null(dim(x))) {
    if(is.matrix(x)) {
      res <- apply(x,margin,function(x)signif.equally(x=x,digits=digits,max=max,margin=margin))
      rownames(res) <- rownames(x)
      return( res )
    }
    if(is.data.frame(x)) {
      res <- as.data.frame( lapply(x,function(x)signif.equally(x=x,digits=digits,max=max,margin=margin)) ,stringsAsFactors=FALSE)
      rownames(res) <- rownames(x)
      return( res )
    }
  }
  if(is.list(x)) return(lapply(x,function(x)signif.equally(x=x,digits=digits,max=max,margin=margin)))
  
  if(!is.numeric(x)) return(x)
  
  x.orig <- x
  x <- x[!is.na(x)]
  if(length(x)==0) return(x.orig) # Wenn alles NA Werte waren, sollen NA Werte zur?ckgegeben werden.
  digits.min <- floor(log10(abs(min(x))))+1
  if( digits.min <= 1 ) {
    digits2 <- max
  } else {
    digits2 <- min(1, -(digits.min-digits) )
  }
  return( round(x.orig,digits2) )
}
####

n.decimals <- function(x){
  stopifnot(class(x)%in%c("numeric","integer"))
  if(length(x)>1) return(  unlist( lapply(as.list(x),function(x)n.decimals(x)) )  )
  
  x <- abs(x)
  if(x%%1==0) {
    return(0)
  } else {
    return(nchar(strsplit(as.character(x), "\\.")[[1]][2]))
  }
}
if(FALSE){
  x <- data.frame(a=seq(0.1, 1, 0.1) , b=c(1:10))
  x <- data.frame(a=seq(0.1, 1, 0.1) , b=c(1:9, 10.001))
  x <- as.matrix(x)
  x <- as.list(x)
  x <- data.frame(a=seq(0.1, 1, 0.1) , b=rep("a", 10))
  #x <- x[,2]
  equal.n.decimals(x)
  add=0; add.dot=TRUE; margin=2
}
###
equal.n.decimals <- function(x, digits=2){
  if(is.data.frame(x)) {
    res <- equal.n.decimals(as.matrix(as.data.frame(x,stringsAsFactors=FALSE)), digits=digits)
    return(apply(res,2,function(x)gsub(" ","",x)))
  }
  return( formatC(x, format="f", digits=digits) )
}
####
if(FALSE){
  x <- data.frame(a=seq(0.1, 1, 0.1) , b=c(1:10))
  x <- data.frame(a=seq(0.1, 1, 0.1) , b=c(1:9, 10.001))
  x <- as.matrix(x)
  x <- as.list(x)
  x <- data.frame(a=seq(0.1, 1, 0.1) , b=rep("a", 10))
  #x <- x[,2]
  equal.length(x)
  equal.length(equal.n.decimals(x))
  add=0; add.dot=TRUE; margin=2
}
equal.length <- function(x, add=0, where=c("beginning","end"), minlength=0, margin=2) {
  if(!is.null(dim(x))) {
    if(is.matrix(x)) return(apply(x,margin,function(x)equal.length(x=x, add=add, where=where, minlength=minlength, margin=margin)))
    if(is.data.frame(x)) return( as.data.frame( lapply(x,function(x)equal.length(x=x, add=add, where=where, minlength=minlength, margin=margin)) ,stringsAsFactors=FALSE) )
  }
  if(is.list(x)) return(lapply(x,function(x)equal.length(x=x, add=add, where=where, margin=margin)))
  
  where <- match.arg(where)
  nchar.x <- nchar(x)
  nchar.x[is.na(nchar.x)] <- 0
  n.add <- max(minlength, nchar.x)  - nchar.x
  x.new <- character()
  if(where=="beginning") {
    for(i in sort(unique(n.add))) x.new[n.add==i] <- paste0(paste0(rep(add,i),collapse=""), x[n.add==i])
  } else if(where=="end") {
    for(i in sort(unique(n.add))) x.new[n.add==i] <- paste0(x[n.add==i], paste0(rep(add,i),collapse="") )
  }
  return(x.new)
}
####
if(FALSE) {
  sign="'"; digits=2; signifequally=FALSE; power=9
  x <- seq(1.5, 45000000.345, 900000.5)
}
trennzeichen1000 <- function(x, sign="'", digits=2, signifequally=FALSE, power=9){
  # Diese Funktion fuegt Tausender-Trennzeichen in Zahlen ein.
  
  # Rekursive Funktionsdefinitionen, falls es sich um eine Matrix, Data.Frame oder Liste handelt.
  if(!is.null(dim(x))) {
    if(is.matrix(x))     return( apply(x,2,function(x)  trennzeichen1000(x=x, sign=sign, digits=digits, signifequally=signifequally, power=power) )  )
    if(is.data.frame(x)) return( as.data.frame( lapply(x,function(x)  trennzeichen1000(x=x, sign=sign, digits=digits, signifequally=signifequally, power=power) ) ,stringsAsFactors=FALSE) )
  }
  if(is.list(x)) return(lapply(x,function(x)  trennzeichen1000(x=x, sign=sign, digits=digits, signifequally=signifequally, power=power) ))
  
  
  if(is.character(x)) x <- as.numeric(x)
  
  if(signifequally) {
    x <- signif.equally(x)
  } else {
    x <- round(x, 2)
  }
  
  chars <- as.character(x)
  if(length(chars)%%2==0) add_1 <- 0 else add_1 <- 1
  find_comma <- (1:length(chars)) %in% grep("\\.", chars)
  if(sum(find_comma)==0) comma_sign <- "" else comma_sign <- "."
  sep_comma <- unlist(strsplit(chars[find_comma], split="\\."))
  
  after_comma <- rep("", length(chars))
  after_comma[find_comma] <- sep_comma[(1:(length(find_comma)+add_1))%%2==0]
  
  before_comma <- rep("", length(chars))
  before_comma[find_comma] <- sep_comma[(1:(length(find_comma)+add_1))%%2!=0]
  before_comma[!find_comma] <- chars[!find_comma]
  
  power_added <- 0
  i <- 1
  for(i in 1:power){
    select <- nchar(before_comma)>i*3+power_added
    if(sum(select)==0) break
    
    tmp <- before_comma[select]
    tmp <- paste0(substr.rev(tmp,i*3+power_added+1, nchar(tmp)+power_added+1),
                  sign,
                  substr.rev(tmp,1,i*3+power_added) )
    
    before_comma[select] <- tmp
    power_added <- power_added + 1
  }
  return(paste0(before_comma, comma_sign, after_comma))
}
####

#scale.extreme(c(1,2,3,4,5,6,7,8,9,10))
#scale.extreme(c(1,2,3,4,5,6,7,8,9,10), range=1, center=0)
#x <- scale.extreme(c(1,2,3,4,5,6,7,8,9,10,22,50,100,23),range=1,center=0); x; mean(x)
scale.extreme <- function(x,na.rm=TRUE,range=1,center=NULL){
  # In contrast du the scale() function this function performs a "Extremwertstandardisierung" (Bacher et al., 2010: Clusteranalyse)
  # The default is a standardization between 0 and 1. This can be altered with range=... and center=...
  minx <- min(x,na.rm=na.rm)
  maxx <- max(x,na.rm=na.rm)
  mm <- maxx-minx
  if(mm==0) {
    if(is.null(center)) return(range/2) else return(center)
  }
  res <- (x-minx)/mm * range
  if(!is.null(center)) res <- res + (center - mean(res))
  return(res)
}
####
# x <- 1:10; rescale(x)
rescale <- function(x, from=0, to=1){
  # Almost the same like scale.extreme but a little simpler and faster
  minx <- min(x, na.rm=TRUE)
  maxx <- max(x, na.rm=TRUE)
  return( (x-minx) * abs(to-from)/(maxx-minx) )
}
####
scale.sd <- function(x,na.rm=TRUE){
  # This is the same like the original scale() function
  y <- x-mean(x,na.rm=na.rm)
  y <- y/sd(x,na.rm=na.rm)
  return(y)
}

####
scale.reverse <- function(x,mean,sd){
  # "unscale" data.
  # Also possible for matrix and data.frame
  
  attributes(x)$`scaled:center` <- attributes(x)$`scaled:scale` <- NULL
  is.null.dim.x <- is.null(dim(x))
  is.data.frame.x <- is.data.frame(x)
  if(!is.null.dim.x) {
    mean <- c(mean)
    sd <- c(sd)
    if(ncol(x)!=length(mean) | ncol(x)!=length(sd)) stop("You must fulfill the condition: ncol(x) == length(mean) == length(sd)")
    if(!is.data.frame.x) { # If the data is a matrix we have to transform if temporarily into a data.frame such that the multiplication is done correctly
      dimnames.x <- dimnames(x)
      x <- as.data.frame(x)
      y <- as.matrix(  x*as.list(sd)+as.list(mean)  )
      dimnames(y) <- dimnames.x
    } else {
      y <- x*as.list(sd)+as.list(mean)
    }
  } else {
    if(length(x)!=length(mean) | length(x)!=length(sd)) stop("You must fulfill the condition: length(x) == length(mean) == length(sd)")
    y <- x*sd+mean
  }
  return(y)
}
#scale.rev(scale(1:50),mean=mean(1:50),sd=sd(1:50))
#sc1 <- scale(scmat <- matrix(1:100,ncol=10)); mean1 <- apply(scmat,2,function(x)mean(x)); sd1 <- apply(scmat,2,function(x)sd(x)); scale.rev(sc1, mean1, sd1)

scaled.value <- function(value,mean,sd){
  y <- (value - mean)/sd
  return(y)
}
#scale(c(0:5)); scaled.value(0,mean(0:5),sd(0:5))

if(FALSE){
  df1=matrix( 1:100, ncol=2); id1=df1[,1]; colnames(df1) <- c("ID1","Value1")
  df2=matrix(21:120, ncol=2); id2=df2[,1]; colnames(df2) <- c("ID2","Value2")
  match.df.by.id(df1=df1, df2=df2, id1=id1, id2=id2, keep.no.matches=TRUE)
  match.df.by.id(df1=df1, df2=df2, id1=id1, id2=id2, keep.no.matches=FALSE)
  merge(df1, df2, by.x="ID1", by.y="ID2")
}

# Moving average
moving.average <- function(x, n=5, dir=c("middle","retro","forward")){
  dir <- match.arg(dir)
  if(dir=="forward") return( rev(as.vector(filter(rev(x),rep(1/n,n), sides=1))) )
  return(as.vector(filter(x,rep(1/n,n), sides=if(dir=="middle") 2 else 1 )))
}
#moving.average(c(1,2,3,4,5,6,7,8,9,10), n=3, dir=c("middle","retro","forward")[3])

####
#' Documentation for automatic .Rd creation with roxygen2 package.
#' \code{match.df.by.id} matches two data.frames by id and returnes a merged data.frame.
#' @param df1 The first data.frame
#' @param df2 The second data.frame
#' @param id1 A vector of ids for the first data frame
#' @param id2 A vector of ids for the second data frame
#' @param keep.no.matches Logical determining whether rows that could not. Default TRUE.
#' @param check.duplicated Logical determining whether the ids should be checked for duplicated entries in each vector before matching. Default TRUE.
#' @param stringsAsFactors Logical determining wheter character vectors be converted to factors. Default FALSE.
#' @return A data.frame containing i1 and id2 in the first two columns followed by the columns of df1 and df2.
match.df.by.id <- function(df1,df2,id1,id2,keep.no.matches=TRUE, check.duplicated=TRUE, stringsAsFactors=FALSE){
  # This function matches two data frames by id.
  # If wished (by default) also no matches are kept.
  # Alternative: merge(df1, df2, by.x="ID1", by.y="ID2"), see also package data.table.
  
  if(any( colnames(df1)%in%c("id1","id2") )) stop("There must be no colnames(df1) equal 'id1' or 'id2'")
  if(any( colnames(df2)%in%c("id1","id2") )) stop("There must be no colnames(df2) equal 'id1' or 'id2'")
  
  if(is.null(dim(df1))|is.null(dim(df2))) stop("df1 and df2 must be data.frame or matrix")
  if(nrow(df1)!=length(id1)) stop("length(id1) must be equal nrow(df1)")
  if(nrow(df2)!=length(id2)) stop("length(id2) must be equal nrow(df2)")
  
  # Check for NA values in the IDs
  is.na.id1 <- is.na(id1)
  is.na.id2 <- is.na(id2)
  
  # Save original Colnames and restore them in the end of the function
  cn_orig <- c(colnames(df1),colnames(df2))
  cn_new <- colnames(data.frame(df1[1,,drop=FALSE], df2[1,,drop=FALSE]))
  
  # If there are NA values, store them seperately in order to add them to the result at the end of the function.
  # But only if keep.no.matches=TRUE. Anyway they are dropped for the matching process.
  if(any(is.na.id1)){
    if(keep.no.matches){
      df1.na <- df1[is.na.id1,,drop=FALSE]
    } else {
      warning("NA values in id1 where removed")
    }
    id1 <- id1[!is.na.id1]
    df1 <- df1[!is.na.id1,,drop=FALSE]
  }
  # Same for id2
  if(any(is.na.id2)){
    if(keep.no.matches){
      df2.na <- df2[is.na.id2,,drop=FALSE]
    } else {
      warning("NA values in id2 where removed")
    }
    id2 <- id2[!is.na.id2]
    df2 <- df2[!is.na.id2,,drop=FALSE]
  }
  
  id1.double <- duplicated(id1)
  id2.double <- duplicated(id2)
  if(any(id1.double)|any(id2.double)){
    
    prov.return <- list()
    if(check.duplicated & any(id1.double)){
      stop("There are duplicated IDs in id1")
      # Veraltet. Wird nicht mehr zurueckgegeben.
      cat("There are duplicated IDs in id1. See function output and return one of each pair.\n")
      prov.return$which.id1.duplicated <- which(id1%in%id1[id1.double])
      prov.return$id1 <- id1[ prov.return$which.id1.duplicated ]
      prov.return$df1 <- data.frame(id1=id1[prov.return$which.id1.duplicated],df1[prov.return$which.id1.duplicated,,drop=FALSE], stringsAsFactors=stringsAsFactors)
    }
    if(check.duplicated & any(id2.double)){
      stop("There are duplicated IDs in id2")
      # Veraltet. Wird nicht mehr zurueckgegeben.
      cat("There are duplicated IDs in id2. See function output and return one of each pair.\n")
      prov.return$which.id2.duplicated <- which(id2%in%id2[id2.double])
      prov.return$id2 <- id2[ prov.return$which.id2.duplicated ]
      prov.return$df2 <- data.frame(id2=id2[prov.return$which.id2.duplicated],df2[prov.return$which.id2.duplicated,,drop=FALSE], stringsAsFactors=stringsAsFactors)
    }
    class(prov.return) <- "match.df.by.id.prov"
    rm.gc.keep("prov.return"); return(prov.return)
  }
  
  # Wenn no-matches nicht behalten werden sollen, ist die einfach.
  if(!keep.no.matches){
    df1 <- df1[id1%in%id2,,drop=FALSE]
    df2 <- df2[id2%in%id1,,drop=FALSE]
    id1 <- id1[id1%in%id2]
    id2 <- id2[id2%in%id1]
    df1 <- data.frame(id1=id1, id2=id2[match(id1,id2)], df1, df2[match(id1,id2),,drop=FALSE], stringsAsFactors=stringsAsFactors)
    rm.gc.keep("df1"); return(df1) # Do not create not object "res" to save memory.
    #return(data.frame(id1=id1, id2=id2[match(id1,id2)], df1, df2[match(id1,id2),,drop=FALSE], stringsAsFactors=stringsAsFactors))
    
    
    # Kompliziert, wenn no-matches behalten werden sollen.
  } else {
    ncol_df1 <- ncol(df1)
    ncol_df2 <- ncol(df2)
    colnames_df2 <- colnames(df2)
    colnames_df1 <- colnames(df1)
    
    df1.gt.df2 <- nrow(df1)>nrow(df2)
    if(!df1.gt.df2){
      
      newo <- match(id1,id2)
      result <- data.frame(id1, df1, id2=id2[newo], df2[newo,,drop=FALSE], stringsAsFactors=stringsAsFactors)
      
      #if(keep.no.matches){
      id2.in.id2.new <- id2%in%result[,"id2"]
      if(any(!id2.in.id2.new)){
        #add.df2 <- data.frame(id2=id2[!id2.in.id2.new],df2[!id2.in.id2.new,,drop=FALSE], stringsAsFactors=stringsAsFactors)
        #add.df1 <- matrix(NA,nrow=nrow(add.df2),ncol=ncol_df1+1)
        add.df <- data.frame(matrix(NA,nrow=sum(!id2.in.id2.new),ncol=ncol_df1+1),
                             data.frame(id2=id2[!id2.in.id2.new],df2[!id2.in.id2.new,,drop=FALSE], stringsAsFactors=stringsAsFactors),
                             stringsAsFactors=stringsAsFactors)
        colnames(add.df) <- colnames(result)
        result <- rbind(result,add.df)
      }
      suppressWarnings(rm(add.df, id1, id2, df1, df2)); invisible(gc())
      
      #} else {
      #  result <- result[!is.na(result[,"id2"]),,drop=FALSE]
      #}
      
      
    } else { #if(df1.gt.df2)
      
      # Hinweis: Den Code von oben kopieren. Dann folgende Ersetzungen vornehmen:
      # df1 -> df3
      # df2 -> df4
      # id1 -> id3
      # id2 -> id4
      # Ruecktauschen:
      # df4 -> df1
      # df3 -> df2
      # id4 -> id1
      # id3 -> id2
      
      ### Block reinkopieren - Anfang ###
      newo <- match(id2,id1)
      result <- data.frame(id2, df2, id1=id1[newo], df1[newo,,drop=FALSE])
      
      #if(keep.no.matches){
      id1.in.id1.new <- id1%in%result[,"id1"]
      if(any(!id1.in.id1.new)){
        #add.df1 <- data.frame(id1=id1[!id1.in.id1.new],df1[!id1.in.id1.new,,drop=FALSE], stringsAsFactors=stringsAsFactors)
        #add.df2 <- matrix(NA,nrow=nrow(add.df1),ncol=ncol_df2+1)
        add.df <- data.frame(matrix(NA,nrow=sum(!id1.in.id1.new),ncol=ncol_df2+1) ,
                             data.frame(id1=id1[!id1.in.id1.new],df1[!id1.in.id1.new,,drop=FALSE], stringsAsFactors=stringsAsFactors) ,
                             stringsAsFactors=stringsAsFactors)
        colnames(add.df) <- colnames(result)
        result <- rbind(result,add.df)
      }
      suppressWarnings(rm(add.df, id1, id2, df1, df2)); invisible(gc())
      #} else {
      #  result <- result[!is.na(result[,"id1"]),,drop=FALSE]
      #}
      ### Block reinkopieren - Ende ###
      
      # Schliesslich Reihenfolge zuruecktauschen:
      result <- result[,c( (1+ncol_df2+1)  #id1
                           ,(1+ncol_df2+1+1):ncol(result) #df1
                           ,1 #id2
                           ,2:(1+ncol_df2)) #df2
                       ]
    }
    
    
    
    # Jetzt id1 und id2 an den Anfang stellen, Rest lassen:
    result <- result[,c( which(colnames(result)=="id1"),
                         which(colnames(result)=="id2"),
                         which(!colnames(result)%in%c("id1","id2")) )
                     ]
    
    # Am Schluss wieder diejenigen Betriebe einfuegen, die bei ihrer eigenen ID NA Values drinstehen hatten:
    if(any(is.na.id1) & keep.no.matches){
      pseudo.df2 <- as.data.frame(matrix(NA, nrow=nrow(df1.na), ncol=ncol(result)-2-ncol(df1.na)))
      colnames(pseudo.df2) <- colnames_df2
      result <- rbind(result,  data.frame(id1=NA, id2=NA, df1.na, pseudo.df2, stringsAsFactors=stringsAsFactors) )
    }
    if(any(is.na.id2) & keep.no.matches){
      pseudo.df1 <- as.data.frame(matrix(NA, nrow=nrow(df2.na), ncol=ncol(result)-2-ncol(df2.na)))
      colnames(pseudo.df1) <- colnames_df1
      result <- rbind(result,  data.frame(id1=NA, id2=NA, pseudo.df1, df2.na, stringsAsFactors=stringsAsFactors) )
    }
    
    colnames(result) <- replace.values(cn_new, cn_orig, colnames(result))
  }
  
  rm.gc.keep("result"); return(result)
}
####
print.match.df.by.id.prov <- function(object){
  object$df1 <- NULL
  object$df2 <- NULL
  class(object) <- "list"
  print(object)
  invisible(object)
}

match.multiple.id.left <- function(id_left, id_right) {
  # This function matches unique IDs in the right vector (the vector that provides values) to non-unique IDs in the left vector (the vector that receives values)
  # Ouput is a matrix. First column serves as index for left vector. Second Column serves as index for right vector.
  
  if(any(duplicated(id_right))) stop("Duplicated IDs in id_right are *not* allowed.")
  m_right <- match(id_left,id_right);
  m_right <- m_right[!is.na(m_right)]; #m_right
  m_left <- which(id_left%in%id_right)
  return(cbind(left=m_left,right=m_right))
}

.paste.elements <- function(l, sep="_", errorMsg="All list places must have same length!"){
  if(!is.list(l) && !is.matrix(l)){
    return(l)
  }
  if(is.matrix(l) | is.data.frame(l)) {
    return(paste.cols(l, sep=sep))
  }
  if(length(unique(unlist(lapply(l,function(x)length(x)))))>1) {
    stop(errorMsg)
  }
  paste_own <- function(...) paste(..., sep=sep)
  return( do.call("paste_own", l) )
}

#index <- as.matrix(spa[,c("REGION","ZA15TYPS3_AVG")]); id <- spa[,"BETRIEB"]; year <- spa[,"JAHR"]; YEAR <- 2014:2015; nYEARmin=length(unique(YEAR)); output=c("logical","ID")
#indexvars <- c("REGION"); table(spa[ balanced.panel(id=spa[,"BETRIEB"],year=spa[,"JAHR"],YEAR=2014:2015,index=spa[,indexvars])  , c("JAHR",indexvars)])
balanced.panel <- function(id, year, YEAR=sort(unique(year)), index=NULL, nYEARmin=length(unique(YEAR)), output=c("logical","ID")){
  # This function creates a vector to filter observations to a balanced panel.
  # id:       Vector of IDs
  # year:     Vector of year (same length as ID)
  # YEAR:     Years that should be selected
  # nYearmin: Minimum number of years to be selected for the pseudo balanced panel
  # index:    Index for which the balancing should be done.
  # output:   Logical vector or IDs?
  
  output <- match.arg(output)
  YEAR <- unique(YEAR)
  if(length(id)!=length(year)) stop("Length id must be equal length year")
  
  if(!is.null(index)) {
    if(output!="logical") stop("If !is.null(index) only output=logical is possible!")
    # Converting Index to string vector
    index <- .paste.elements(index, sep="_", errorMsg="All indices must have same length!")
    
    # Recursive function definition if !is.null(index)
    # balanced.panel() for each entry in index separately.
    #x <- cbind(counter=1:length(id),id=id,year=year)[index==index[1],]
    res <- do.call("rbind", by(cbind(counter=1:length(id), id=id, year=year), index, function(x){
      data.frame(x, filter=balanced.panel(id=x[,"id"], year=x[,"year"], YEAR=YEAR, index=NULL, nYEARmin=nYEARmin, output=output),stringsAsFactors=FALSE)
    }))
    return( res[order(res[,"counter"]),"filter"] )
  }
  
  mode.id <- mode(id) # Save mode of id for later output
  IDs <- list()
  for(i in 1:length(YEAR)){
    IDs[[i]] <- id[ year%in%YEAR[i] ]
  }
  IDs <- do.call("c",IDs)
  table.IDs <- table(IDs)
  if(any(table.IDs>length(YEAR))) {
    return.error <- names(table.IDs)[table.IDs>length(YEAR)]
    mode(return.error) <- mode.id
    stop(paste0("The following observations occur several times in several years! Not able to create balanced panel.\n", paste0(return.error, collapse=", ")))
  }
  IDs.final <- names(table.IDs)[table.IDs>=nYEARmin]
  mode(IDs.final) <- mode.id  # Set back the mode to original value (instead of character from names(table())... )
  if(output=="logical") {
    return( id%in%IDs.final & year%in%YEAR )
  } else {
    warning("This output only serves to show which IDs are available in all years. However, it is possible that they are in other years too. E.g. if you choose YEAR=c(0,1,2), some IDs could be in all years c(0,1,2) but also in year 3. If you want to filter only the relevant IDs AND years choose output='logical'")
    return(IDs.final)
  }
}
#id <- sample(1:10,100,TRUE); year <- sample(2000:2002,100,TRUE); YEAR <- c(2000,2001);
#balanced.panel(id,year,YEAR,"ID")

####
balanced.panel.filtering <- function(criterium, data, id.vec, year.vec, output=c("logical","ID"), NAasFALSE=TRUE) {
  # This function filters a balanced panel data set such that the criterium
  # is fulfilled in every year for each observation.
  #
  # criterium: expression, that formulates which filtering-criterium must be fulfilled. Colnames of the data must be used!
  # data:      matrix or data.frame that contains all colnames which are used in the criterium expression.
  # id.vec:    vector of IDs (preferrably something like data[,"ID"] )
  # year.vec:  vector of years (preferrably something like data[,"year"] )
  # output:    Should the output be in form of a logical vector or IDs? --> Better choose "logical" in order not to have problems!
  
  output <- match.arg(output)
  if(!is.data.frame(data)) data <- as.data.frame(data)
  mf <- match.call() # http://stackoverflow.com/questions/4682709/how-to-write-an-r-function-that-evaluates-an-expression-within-a-data-frame
  # See also http://stackoverflow.com/questions/4692231/r-passing-expression-to-an-inner-function
  # or http://adv-r.had.co.nz/
  criterium <- eval(mf$criterium, envir=data) # It would also work with eval(substitute(criterium), data) without match.call()
  criterium[!is.finite(criterium)] <- !NAasFALSE
  
  table.id.vec.criterium <- table(id.vec[criterium])
  ID <- as.numeric( names( table.id.vec.criterium )[ table.id.vec.criterium==length(unique(year.vec)) ] )
  if(output=="logical") {
    return( id.vec %in% ID  )
  } else {
    return(ID)
  }
}
#data <- matrix(1:100,ncol=2); colnames(data) <- c("a","b"); data <- as.data.frame(data); id.vec <- rep(1:25,2); year.vec <- rep.1b1(c(2003,2004),25)
#cbind(data, sum=rowSums(data), id=id.vec, year=year.vec, filter=balanced.panel.filtering(a+b<110, data, id.vec, year.vec))

if(FALSE){
  #x <- round( runif(100)*100 )
  #id <- rep(1:50, 2)
  #year <- rep.1b1(c(0,1),50)
  gb <- load.gb()
  x <- gb[,"rohPara_tot"]
  id <- gb[,"ID"]
  year <- gb[,"Jahr"]
  weights <- gb[,"Gewicht"]
  YEAR=sort(unique(year));
  baseyear=min(YEAR); geometric=FALSE; absolute.diff=TRUE; filter=FALSE; filter.level=c(1/3,3); return.N=FALSE;
  
  # Debugging
  mean(x[year==1]) - mean(x[year==0])
  mean(x[year==1]) / mean(x[year==0])
  balanced.panel.development(x,id,year,YEAR=sort(unique(year)),baseyear=min(YEAR),geometric=FALSE,absolute.diff=FALSE,filter=FALSE,filter.level=c(1/3,3),return.N=FALSE)
  mean.geom(x[year==1]) - mean.geom(x[year==0])
  mean.geom(x[year==1]) / mean.geom(x[year==0])
  balanced.panel.development(x,id,year,YEAR=sort(unique(year)),baseyear=min(YEAR),geometric=TRUE,absolute.diff=TRUE,filter=FALSE,filter.level=c(1/3,3),return.N=FALSE)
  
  # Weighted
  w <- c(1:10)
  x <- sample(1:100,10,replace=TRUE)
  mean( x[1:5]-x[6:10] )
  mean( (x*w)[1:5]-(x*w)[6:10] )
  weighted.mean( x[1:5]-x[6:10], w[6:10] )
}
####
# Daten zur Kontrolle der Funktion:
# LE Typ 54, vergleichbare 2013/2014, UNGEWICHTET: 2013:61433, 2014:70187, Diff:8754   Werte nicht vergleichbar, gewichtet, 2013:58133, 2014:66865, Diff:8732
balanced.panel.development <- function(x,id,year,YEAR=sort(unique(year)),baseyear=min(YEAR),weights=NULL,geometric=TRUE,absolute.diff=FALSE,absolute.number=c("no","comparable_baseyear","all_baseyear"),filter=TRUE,filter.level=c(1/3,3),return.N=FALSE){
  # Note that there is a safety copy of this function just below.
  # weights are not implemented for geometric means!!!
  
  # This function calculates an index of an unbalanced time series.
  # This can be useful if you want to follow the delevoptment of yields but the time horizon
  # is so long that a balanced panel over the whole period has 0 observations.
  # The function calculates relative changes of every pair of year (which builds some kind of 2-year balanced panel).
  # The changes from year to year are then multiplied over the hole period ( geometric=TRUE ) or summed up ( geometric=FALSE ).
  # x:               data like e.g. yield
  # id:              Vector of IDs
  # year:            Vector of year (same length as ID)
  # YEAR:            Years that should be selected
  # baseyear:        The baseyear in which the index has value 1
  # weights:         optional weights for the calculations
  # geometric:       See description above
  # absolute.diff:   Should absolute differences instead of relative differences be calculated?
  # absolute.number: Should absolute numbers instead of relative changes be calculated? If yes, which mean should be calculated in the baseyear?
  #                  The mean of the comparable observations in baseyear and baseyear+1 (comparable_baseyear) or the mean of all observations in the baseyear (all_baseyear)?
  # filter:          Should extreme changes (that are probalby not realistic) be filtered out?
  # filter.level:    If yes, which change-factor is the threshold to filter out c(upper,lower)
  # return.N:        Should the number of observations in each year be calculated instead of the index?
  
  if(is.data.frame(x) | is.list(x)) return( sapply(x,function(x)balanced.panel.development(x=x,id=id,year=year,YEAR=YEAR,baseyear=baseyear,weights=weights, geometric=geometric, absolute.diff=absolute.diff, absolute.number=absolute.number, filter=filter,filter.level=c(1/3,3),return.N=return.N)) )
  if(is.matrix(x)) return( apply(x,2,                function(x)balanced.panel.development(x=x,id=id,year=year,YEAR=YEAR,baseyear=baseyear,weights=weights, geometric=geometric, absolute.diff=absolute.diff, absolute.number=absolute.number, filter=filter,filter.level=c(1/3,3),return.N=return.N)) )
  
  # weights wird w genannt, damit es im Code einfacher einzubauen ist
  weighted <- !is.null(weights)
  if(weighted){
    w <- weights
    if(length(x)!=length(w)) stop("length(x)!=length(weights)")
  }
  rm(weights)
  
  absolute.number <- match.arg(absolute.number)
  
  if(filter) warning("You have chosen filter=TRUE which excludes extreme values from analysis.")
  if(geometric & absolute.diff) warning("The combination of mean.geom and absoulte difference is rather unusual!")
  if(geometric & weighted) warning("Weighting is not implemented for geometric means.")
  if(absolute.number%in%c("comparable_baseyear","all_baseyear")) absolute.diff <- TRUE
  if(length(x)!=length(id))   stop("length(x)!=length(id)")
  if(length(x)!=length(year)) stop("length(x)!=length(year)")
  if(!baseyear%in%YEAR) stop("baseyear must be in the range of YEAR")
  if(any(!YEAR%in%year)) warning("the years ", YEAR[!YEAR%in%year]," are used in YEAR but do not exist in year")
  
  YEAR <- sort(YEAR)
  d.means <- numeric()
  N <- numeric(length(YEAR)); names(N) <- YEAR; N[N==0] <- NA;
  
  i <- 1
  # Start loop over all YEAR
  for(i in 1:(length(YEAR)-1)) {
    # Making a balanced panel for 2 years
    ids <- id[year%in%c(YEAR[i],YEAR[i+1])]
    table.ids <- table(ids)
    if(any(table.ids>2)) {
      output <- names(table.ids)[table.ids>2]
      print(output)
      invisible(output)
      stop("Some ids occur several times in the same year.")
    }
    ids.final <- names(table.ids)[table.ids==2]
    year12 <- id%in%ids.final & year%in%c(YEAR[i],YEAR[i+1])
    # Choosing the first and the second year of the balanced panel seperately
    year1 <- year12 & year%in%YEAR[i]
    year2 <- year12 & year%in%YEAR[i+1]
    if(FALSE){ # Test, if it worked
      print(table( year[year12] ))
      print(all(table(id[year12])==2))
    }
    
    # Calculate the number of observations for the first year.
    if(return.N){
      if(i==1){
        year1.temp <- year1
        is.na.year1 <- is.na(x[year1.temp])
        year1.temp[is.na.year1] <- FALSE
        N[1] <- sum(year1.temp)
      }
    }
    
    # Filtering of extreme Values is always done with relative changes.
    if(filter) {
      d <- x[year2][order(id[year2])]   /   x[year1][order(id[year1])]
      keep <- d<filter.level[1] | d>filter.level[2]
      keep[is.na(keep)] <- TRUE
    } else {
      keep <- rep(TRUE, length(x))
    }
    
    if(geometric){
      # Calculate the relative differences between the years.
      # Important: The values have to be ordered such that always the same observations are compared
      d.means[i] <- mean.geom(x[year2][order(id[year2])][keep]) / mean.geom(x[year1][order(id[year1])][keep])
      
    } else {
      # Calculate the absolute differences between the years.
      if(weighted) {
        d.means[i] <- weighted.mean(x[year2][order(id[year2])][keep], w[year2][order(id[year2])][keep],na.rm=TRUE) -
          weighted.mean(x[year1][order(id[year1])][keep], w[year1][order(id[year1])][keep],na.rm=TRUE)
      } else {
        d.means[i] <- mean(x[year2][order(id[year2])][keep],na.rm=TRUE) - mean(x[year1][order(id[year1])][keep],na.rm=TRUE)
      }
      
      # If the absolute number is whised (absolute.number==TRUE), the mean of the according year has to be calculated with the comparable observations
      # Falls so gewaehlt, den Mittelwert der Vergleichbaren im Baseyear berechnen.
      if(absolute.number=="comparable_baseyear"){
        if(YEAR[i]==baseyear){
          if(weighted){
            mean.baseyear <- weighted.mean(x[year1][order(id[year1])][keep], w[year1][order(id[year1])][keep],na.rm=TRUE)
          } else {
            mean.baseyear <- mean(x[year1][order(id[year1])][keep],na.rm=TRUE)
          }
        }
      }
    }
    
    # Calculate the number of observations that were used to build the difference
    if(return.N) N[i+1] <- sum(!is.na(d[[i]]))
  }
  # END OF THE LOOP
  
  # Bei geometric ist der Wert im 1. Jahr ist der Ausgangswert. Also 1.
  # Bei arithmecid 0
  if(geometric){
    d.means <- c(1,d.means)
  } else {
    d.means <- c(0,d.means)
  }
  
  if(return.N) return(N)
  
  if(geometric){
    # Bisher wird jeweils die Ver?nderung zum Vorjahr wiedergegeben.
    # Nun m?ssen die Werte noch miteinander multipliziert werden, damit sich der Verlauf von Anfang an ergibt.
    index <- numeric(length(d.means)); names(index) <- YEAR
    for(i in 1:length(d.means)) index[i] <- prod(d.means[1:i])
    
    # Alt
    # Alle Zahlen durch den Index im Index-Jahr dividieren, sodass dort der 1-Punkt ensteht.
    #index <- index/index[names(index)==baseyear]
    # Debugging
    # mean.geom(x[year==2012])
    # mean.geom(x[year==2013])
    
    # Der Inex wird im Jahr 1 = 0 gesetzt. Dann Der Mittelwert im Index-Jahr subtrahiert, sodass dort der 0-Punkt ensteht.
    if(absolute.diff){
      mean.year1 <- mean.geom(x[year1],na.rm=TRUE)
      index <- index*mean.year1 - mean.year1
      index <- index-index[names(index)==baseyear]
      # Der Inex wird im Jahr 1 = 1 gesetzt. Dann alle Zahlen durch den Index im Index-Jahr dividiert, sodass dort der 1-Punkt ensteht.
    } else {
      index <- index/index[names(index)==baseyear]
    }
    
    
  } else { # if(!geometric)
    
    # Bisher wird jeweils die Ver?nderung zum Vorjahr wiedergegeben.
    # Nun m?ssen die Werte noch addiert werden, damit sich der Verlauf von Anfang an ergibt.
    index <- numeric(length(d.means)); names(index) <- YEAR
    for(i in 1:length(d.means)) index[i] <- sum(d.means[1:i])
    
    # Choose again the balanced Panel of the first year. Then put all numbers in to relation to that mean.
    ids <- id[year%in%c(YEAR[1],YEAR[1+1])]
    table.ids <- table(ids)
    ids.final <- names(table.ids)[table.ids==2]
    year12 <- id%in%ids.final & year%in%c(YEAR[1],YEAR[1+1])
    # Choosing the first and the second year of the balanced panel seperately
    year1 <- year12 & year%in%YEAR[1]
    year2 <- year12 & year%in%YEAR[1+1]
    if(FALSE){
      # So bildet man am Anfang nur den Mean, der Vergleichbaren Betriebe von Jahr 1 und Jahr 2, von denen auch die Differenz berechnet wird.
      is.na.year2 <- is.na(x[year2])
      year2[is.na.year2] <- FALSE
      year1.new <- year1 & year2
      #length(year1); length(year2) # Kontrolle
      mean.year1 <- mean(x[year1.new],na.rm=TRUE)
    }
    # So bildet man am Anfang den Mean, aller Vergleichbaren Betriebe von Jahr 1 und Jahr 2
    if(weighted) {
      mean.year1 <- weighted.mean(x[year1],w[year1],na.rm=TRUE)
    } else {
      mean.year1 <- mean(x[year1],na.rm=TRUE)
    }
    
    # Der Index wird relativiert durch den Mean des ersten Jahres.
    index <- (index+mean.year1)
    
    # Debugging
    # mean(x[year==2012])
    # mean(x[year==2013])
    
    # Der Inex wird im Jahr 1 = 0 gesetzt. Dann Der Mittelwert im Index-Jahr subtrahiert, sodass dort der 0-Punkt ensteht.
    if(absolute.diff){
      index <- index-mean.year1
      index <- index-index[names(index)==baseyear]
      # Wenn absolute Werte, nicht differenzen gegeben werden sollen, dann vom baseyear den mean addieren
      # Falls so gewaehlt, den Mittelwert der Vergleichbaren im Baseyear addieren.
      if(absolute.number=="comparable_baseyear") {
        index <- index + mean.baseyear
        # Falls so gewaehlt, den Mittelwert der aller Beobachtungen im Baseyear addieren.
      } else if(absolute.number=="all_baseyear") {
        choose_baseyear <- year==baseyear
        if(weighted) {
          index <- index + weighted.mean(x[choose_baseyear],w[choose_baseyear],na.rm=TRUE)
        } else {
          index <- index + mean(x[choose_baseyear],na.rm=TRUE)
        }
      }
      # Der Inex wird im Jahr 1 = 1 gesetzt. Dann alle Zahlen durch den Index im Index-Jahr dividiert, sodass dort der 1-Punkt ensteht.
    } else {
      index <- index/mean.year1
      index <- index/index[names(index)==baseyear]
    }
  } # End if
  
  if(any(is.na(index)) & geometric) cat("Note that the geometric mean can only be calculated if all numbers are positive.\n")
  return(index)
}

####
#x <- rnorm(100); selection.levels <- 0.1; method=c("<= x <", "< x <=")[1]; include.min.max=TRUE; give.names=TRUE
#g1roup.by.fix.scal1e(x,c(0),c("<= x <", "< x <="),FALSE)
group.by.fix.scale <- function(x, selection.levels, method=c("< x <=", "<= x <"), include.min.max=FALSE, give.names=FALSE, names.digits=2, names.sep="-"){
  # This function returns a grouping according to the chosen absolute selection.levels.
  # If include.min.max=TRUE depending on the method the minimum (maximum) point is included with the <= sign instead of <
  # Otherwise if you give exactely the min(x) and max(x) one of both would be excluded.
  # If selection.levels is only one value then all values below are numbered 1 and all above 2
  method <- match.arg(method)
  
  selection.levels <- sort(selection.levels)
  is.na.x <- is.na(x)
  if(length(selection.levels)==1) {
    include.min.max <- TRUE
    selection.levels <- c(min(x[!is.na.x]), selection.levels, max(x[!is.na.x]))
  }
  length.selection.levels <- length(selection.levels)
  
  grouping <- rep(NA,length(x))
  if(method=="<= x <")  {
    for(i in 1:(length.selection.levels-2)) {
      grouping[ x >= selection.levels[i] & x < selection.levels[i+1] ] <- i
      if(give.names) names(grouping)[ x >= selection.levels[i] & x < selection.levels[i+1] ] <- paste(zapsmall(round(c(selection.levels[i],selection.levels[i+1]),names.digits) ),collapse=names.sep)
    }
    if(include.min.max){
      grouping[ x >= selection.levels[length.selection.levels-1] & x <= selection.levels[length.selection.levels] ] <- length.selection.levels-1
      if(give.names) names(grouping)[ x >= selection.levels[length.selection.levels-1] & x <= selection.levels[length.selection.levels] ] <- paste(zapsmall(round(c(selection.levels[length.selection.levels-1],selection.levels[length.selection.levels]),names.digits) ),collapse=names.sep)
    } else {
      grouping[ x >= selection.levels[length.selection.levels-1] & x < selection.levels[length.selection.levels] ] <- length.selection.levels-1
      if(give.names) names(grouping)[ x >= selection.levels[length.selection.levels-1] & x < selection.levels[length.selection.levels] ] <- paste(zapsmall(round(c(selection.levels[length.selection.levels-1],selection.levels[length.selection.levels]),names.digits) ),collapse=names.sep)
    }
  } else  if(method=="< x <=")  {
    if(include.min.max){
      grouping[ x >= selection.levels[1] & x <= selection.levels[2] ] <- 1
      if(give.names) names(grouping)[ x >= selection.levels[1] & x <= selection.levels[2] ] <- paste(zapsmall(round(c(selection.levels[1],selection.levels[2]),names.digits) ),collapse=names.sep)
    } else {
      grouping[ x > selection.levels[1] & x <= selection.levels[2] ] <- 1
      if(give.names) names(grouping)[ x > selection.levels[1] & x <= selection.levels[2] ] <- paste(zapsmall(round(c(selection.levels[1],selection.levels[2]),names.digits) ),collapse=names.sep)
    }
    for(i in 2:(length(selection.levels-1)))  {
      grouping[ x > selection.levels[i] & x <= selection.levels[i+1] ] <- i
      if(give.names) names(grouping)[  x > selection.levels[i] & x <= selection.levels[i+1] ] <- paste(zapsmall(round(c(selection.levels[i],selection.levels[i+1]),names.digits) ),collapse=names.sep)
    }
  }
  
  if(any(is.na(grouping))) {
    #print(grouping)
    warning("NAs produced")
  }
  return(grouping)
}

####

group.by.quantiles <- function(x, probs=seq(0,1,0.1), method=c("< x <=", "<= x <"), index=NULL, include.min.max=TRUE, give.names=FALSE, names.digits=2, names.sep="-", weights=NULL, na.rm=FALSE){
  # Groupy data by quantiles.
  # This is a wrapper for group.by.fix.scale with slightly altered interface.
  
  #x <- rnorm(1000);  selection.levels <- c(0, 0.25, 0.5, 0.75, 1); method=c("<= x <"); include.min.max=TRUE; weights=rnorm(1000)
  #group.by.quartiles(x=x, weights=weights);
  method <- match.arg(method)
  
  if(!is.null(index)) {
    index <- .paste.elements(index, sep="_", errorMsg="All indices must have same length!")
    # Recursive function definition if !is.null(index). --> group.by.fix.scale() for each entry in index separately.
    res <- do.call("rbind", by(cbind(counter=1:length(x), x=x), index, function(x){
      data.frame(x, grouping=group.by.quantiles(x=x[,"x"], probs=probs, method=method, index=NULL, include.min.max=include.min.max, give.names=give.names, names.digits=names.digits, names.sep=names.sep, weights=weights, na.rm=na.rm))
    }))
    return( res[order(res[,"counter"]),"grouping"] )
  }
  
  probs <- sort(probs)
  probs.rel <- probs; rm(probs)
  if(any(probs.rel<0) | any(probs.rel>1)) stop("choose 0 >= probs >= 1")
  if(length(probs.rel)==1) {
    include.min.max <- TRUE
    probs.rel <- c(0, probs.rel, 1)
  }
  
  if(is.null(weights))  {
    probs.abs <- quantile(x,probs.rel)
  } else {
    if(length(weights)!=length(x)) stop("length(x) must be equal length(weights)")
    #require.package(Hmisc)
    #probs.abs <- wtd.quantile(x=x, weights=weights, probs=probs.rel)
    probs.abs <- quantile.weight(x=x, weights=weights, index=index, probs=probs.rel, na.rm=na.rm)
    # Vergleich: quantile(x=x, probs=probs.rel)
    if(min(probs.rel)==0) probs.abs[which.min(probs.rel)] <- min(x)-1
    if(max(probs.rel)==1) probs.abs[which.max(probs.rel)] <- max(x)+1
  }
  
  grouping <- group.by.fix.scale(x=x, selection.levels=probs.abs, method=method, include.min.max=include.min.max, give.names=give.names, names.digits=names.digits, names.sep=names.sep)
  return(grouping)
}

group.by.quartiles <- function(...){
  group.by.quantiles(..., probs=c(0, 0.25, 0.5, 0.75, 1))
}

#x <- gb[,"ArbVerd_jeFJAE"]; weights <- gb[,"Gewicht"]; index <- gb[,c("Jahr","Region","Betriebstyp_S3")]
#x=spa[,"P430_0100_95300"]; weights=spa[,weightCol]; index=spa[,"SchichtJahr"]
#x=c(1,2,3,4,5,6,7,8,9,10); index=c(1,1,1,1,1,2,2,2,2,2); weights=NULL; probs=c(0, 0.25, 0.5, 0.75, 1); method=c("< x <=", "<= x <")[1]; na.rm=TRUE
group.by.wtd.quantiles <- function(x, weights=NULL, index=NULL, probs=c(0, 0.25, 0.5, 0.75, 1), method=c("< x <=", "<= x <"), na.rm=TRUE) {
  # This function groups the elements of a vector by weighted quantiles.
  # Weights and index can be given.
  
  method <- match.arg(method)
  if(!is.null(dim(x))) stop("x must be a vector")
  
  # If an index is given, the quantiles are grouped for each index
  if(!is.null(index)) {
    index <- .paste.elements(index, sep="_", errorMsg="All indices must have same length!")
    if(any(weights==0) & na.rm==TRUE) warning("There were weights==0. Wbservations with weight==0 were not allocated to a group.")
    
    res <- do.call("rbind",as.list(by( cbind(counter=1:length(x), x=x, weights=weights), index,function(x){
      x[,"q"] <- suppressWarnings( group.by.wtd.quantiles(x=x[,"x"], weights=if(is.null(weights)) NULL else x[,"weights"], index=NULL, probs=probs, na.rm=na.rm, method=method) )
      return(x)
    })))
    return(res[order(res[,"counter"]),"q"])
  }
  
  # This is the actual grouping function
  selection.levels <- quantile.weight(x=x, weights=weights, index=NULL, probs=probs, na.rm=na.rm)
  return( group.by.fix.scale(x=x, selection.levels=selection.levels, method=method, include.min.max=TRUE) )
}

####
group.to.ngroups <- function(data,variable,ngroups){
  data <- data[order(data[,variable]),,drop=FALSE]
  nrow.data <- nrow(data)
  size <- nrow.data/ngroups
  grouping <- rep(NA,nrow.data)
  grouping[1:round(size)] <- 1
  for(i in 1:(ngroups-1)) grouping[round(size*i+1):round(size*(i+1))] <- i+1
  result <- cbind(data,grouping=grouping)
  return(result)
}
####

randomize.data <- function(x,index=NULL,times.sd=1,greater.zero=FALSE){
  # Randomize data (e.g. Grundlagenbericht) such that the mean of the numbers stays more or less the same
  # But the data is not "real" anymore
  
  #x <- 1:100; index=NULL; times.sd=1; greater.zero=FALSE
  #randomize.data(1:10,greater.zero=TRUE)
  if(is.matrix(x)) {
    return( apply(x,2,function(x)randomize.data(x=x,index=index,times.sd=times.sd,greater.zero=greater.zero)) )
  } else if(is.data.frame(x)){
    return(  sapply(x,function(x)randomize.data(x=x,index=index,times.sd=times.sd,greater.zero=greater.zero))  )
  }
  if(is.null(index)) index <- rep("a",length(x))
  randomize.inner <- function(x, times.sd, greater.zero){
    #x.new <- x + rnorm(length(x),0,times.sd*sd(x))
    add <- times.sd*sd(x)
    x.new <- x + sample(add*seq(-1,1,0.1),length(x),replace=TRUE)
    if(greater.zero) {
      x.new[x.new<0 & x>=0] <- 0
    }
    return(x.new)
  }
  result <- tapply(x,index,function(x)randomize.inner(x=x, times.sd=times.sd, greater.zero=greater.zero) )
  result <- unlist(result)
  return(result)
}

####
na.replace <- function(x,grouping=NULL,sd=1,warnings=0.5) {
  # Explanation: The NA values are replaced by a random normal variable with the mean and sd of the observations.
  # If grouping is given, the means&st'devs of the groups are used for group-wise NA replacement
  # Note: NAs of variables and groups which only have NAs are kept! If there is one value this one replaces all the NAs with a warning.
  # warnings=... specifies when a warning should be printed. The default warning is printed when 50% of the values in a group are replaced.
  
  # Definition of na.replace function
  if(any(is.na(x))) {
    replace.na.inner <- function(x,sd) {
      if(any(is.na(x))) {
        nas <- rep(NA,length(which(is.na(x))))
        meanx <- mean(x,na.rm=TRUE)
        sdix <- sd(x,na.rm=TRUE)
        if(is.na(sdix)) sdix <- 0
        if(length(nas)>1) {
          nas <- rnorm(n=length(nas),mean=meanx,sd=sdix*sd)
        } else nas <- meanx
        x[is.na(x)] <- nas
        return(x)
      } else return(x)
    }
    
    # Report if there are to many NA values or var(x) could not be calculated..
    if(!is.null(dim(x))) {
      if (is.null(grouping)) grouping.1 <- rep(1,nrow(x)) else grouping.1 <- grouping
      n.na <-                  apply(x,2,function(x)tapply(x,grouping.1,function(y)length(which(is.na(y)))>(1-warnings)*length(y)))
      all.na <-                apply(x,2,function(x)tapply(x,grouping.1,function(y)length(which(is.na(y)))==length(y)))
      if(is.null(dim(n.na)))   n.na <- rbind(n.na,n.na)
      if(is.null(dim(all.na))) all.na <- rbind(all.na,all.na)
      n.na.much <-             apply(n.na,2,function(x)any(x))
      all.n.na.much <-         apply(all.na,2,function(x)any(x))
      
      sds <- apply(x,2,function(x)tapply(x,grouping.1,function(y)sd(y,na.rm=TRUE)))
      sds[is.na(sds)] <- 0
      if(is.null(dim(sds))) sds <- rbind(sds,sds)
      var0.matrix <- apply(sds,2,function(x) x==0)
      var0 <- apply(var0.matrix,2,function(x)any(x))
      if(any(n.na.much&!all.n.na.much)) warning("in the following variables more than ",round(warnings*100)," % of the values were replaced (at least in some groups)\n",paste(names(n.na.much[n.na.much&!all.n.na.much]),collapse=" , "))
      if(any(var0&!all.n.na.much)) warning("in the following variables one value replaced all the others (at least in some groups)\n",paste(names(var0[var0&!all.n.na.much]),collapse=" , "))
      if(any(all.n.na.much)) warning("in the following variables NAs where kept because there wasn't even 1 value to replace the other NA values (at least in some groups)\n",paste(names(all.n.na.much[all.n.na.much]),collapse=" , "))
    }
    
    # Replace NA.values
    if(is.null(grouping)) {
      if(is.null(dim(x))) return(replace.na.inner(x,sd))
      if(!is.null(dim(x))) return(apply(x,2,function(y)replace.na.inner(y,sd)))
    } else if(!is.null(grouping)) {
      replace.na.inner.grouping <- function(x,grouping,sd) {
        for(i in unique(grouping))  x[grouping==i] <- replace.na.inner( x[grouping==i], sd)
        return(x) }
      if(is.null(dim(x))) return(replace.na.inner.grouping(x,grouping,sd))
      if(!is.null(dim(x))) return(apply(x,2,function(y)replace.na.inner.grouping(y,grouping,sd)))
    }
  } else return(x)
}

#search <- c(1,2,3); replace <- c("a","b","c"); x <- c(NA, 0, 1, 1, 2, 3)
#search <- c(9,10,11); replace <- c("a","b","c"); x <- c(NA, 0, 1, 1, 2, 3)
#replace.values(search,replace,x)
replace.values <- function(search, replace, x, no.match.NA=FALSE, gsub=FALSE, fixed=FALSE){
  # This function replaces all elements in argument search with the corresponding elements in argument replace.
  # Replacement is done in x.
  # if no.match.NA=TRUE, values that could not be replaced will be NA instead of the old value.
  
  if(length(search)!=length(replace)) stop("length(search) must be equal length(replace)")
  if(any(duplicated(search))) stop("There must be no duplicated entries in search.")
  
  if(is.matrix(x)){
    return(apply(x,2,function(x)replace.values(search=search,replace=replace,x=x)))
  } else if(is.data.frame(x)){
    return(as.data.frame(lapply(x,function(x)replace.values(search=search,replace=replace,x=x)),stringsAsFactors=FALSE))
  }
  
  xnew <- x
  if(!gsub) {
    m1 <- match(x, search)
    xnew <- replace[ m1 ]
    if(!no.match.NA){
      isna <- is.na(m1)
      xnew[isna] <- x[isna]
    }
  } else {
    # Hier erst nach Laenge der Strings ordnen, damit lange Teilstrings vor kurzen ersetzt werden.
    ord <- order(nchar(search),decreasing=TRUE)
    search <- search[ord]
    replace <- replace[ord]
    for(i in 1:length(search)){
      xnew <- gsub(search[i],replace[i],xnew, fixed=fixed)
    }
  }
  return(xnew)
}

recover.distorted.string <- function(original, distorted, max.rel.dist=0.7, no.match.NA=TRUE, ...){
  # This function compares the strings in argument "original" and "distorted".
  # It replaces all occurences in "distorted" with matches in "orgininal" if the relative distance is smaller than "max.rel.dist".
  # If no.match.NA==TRUE, then no matches are NA in the output vector. Otherwise no matches are the same as inputed in "distorted".
  # ... further arguments to pass into stringist::stringdistmatrix
  
  require.package(stringdist)
  distmat <- stringdist::stringdistmatrix(original, distorted)
  corrected <- if(no.match.NA) rep(NA_character_, length(distorted)) else distorted
  
  for(i in 1:ncol(distmat)){ # i <- 1
    d1 <- distmat[,i] / nchar(distorted[i])
    whichMin <- which(d1==min(d1) & d1<1)
    if(length(whichMin)>0){
      if(length(whichMin)>1) {
        warning(paste0("Several matches found for ", distorted[i], ": ", paste0(original[whichMin],collapse=", "), ". First was taken."), immediate.=TRUE)
        whichMin <- whichMin[1]
      }
      corrected[i] <- original[whichMin]
    }
  }
  return(corrected)
}

#### CSV/RData/XLS - READ/WRITE & MANIPULATION ####

#cost <- load2("C:/Users/U80823148/_/ME/ME_data_out/data_final/allcosts_info.RData")
#file <- "\\\\evdad.admin.ch/AGROSCOPE_OS/2/5/2/1/2/4278/hpda/R/Output/Test_write.table.fast/cost.csv"
#system.time( wr1ite.table(cost[1:1000,],file) ); system.time( utils::wr1ite.table(cost[1:1000,],file) )
write.table <- function(x, file, ...) {
  # This function writes tables much faster if they should be written onto network drives.
  # If a network drive is detected and the file will be larger than approx. 300kb, it first creates a temporary file on the local hard drive.
  # Then it moves the file from local to network drive.
  
  if( ! .isNecessaryToUseTmpDir(x, file) ){
    utils::write.table(x=x, file=file, ...)
  } else {
    utils::write.table(x=1, file=file, ...) # First try to write a file. If not possible (e.g. because directory does not exist) this will return a error message.
    tryCatch({
      folder <- paste0(tempdir(),"/RFastWrite") # paste0(Sys.getenv("TMP"), "\\R\\RFastWrite\\")
      dir.create(folder, recursive=TRUE, showWarnings=FALSE)
      file.remove(list.files(folder,full.names=TRUE))
      file2 <- paste0(folder, paste0(sample(letters,4,replace=TRUE),collapse=""), ".csv" )
      utils::write.table(x=x, file=file2, ...)
      file.copy(file2, file, overwrite=TRUE)
      suppressWarnings( file.remove(list.files(folder,full.names=TRUE)) )
    }, error = function(e) {
      stop(paste0(e$message, "write.table() has encountered an error. This is not the original write.table() function but an edited version by Daniel Hoop. It was optimized to save files on network drives.\nTry utils::write.table() to use the default function."))
    })
    unlink(folder)
  }
}

# Performance-Vergleich zwischen eigener zip-Loesung und csv.gz Loesung. Ist gleich schnell.
# Es scheint ausserdem, dass das Einlesen von csv File von Laufwerk gleich schnell ist wie das Einlesen & Entpacken von zip file von Laufwerk.
# Beide Male 10 Sekunden f?r AGIS-Daten 2015.
if(FALSE){
  dir.create("C:\\Users\\U80823~2\\AppData\\Local\\Temp\\R\\", showWarnings=FALSE, recursive=TRUE)
  df <- as.data.frame( matrix(1:1000000, ncol=100) )
  system.time(for(i in 1:1){ # Performanc with buil-tin function. 20 sec
    wr1ite.table(df, file=gzfile("C:\\Users\\U80823~2\\AppData\\Local\\Temp\\R\\filename.csv.gz"), sep=";", quote=FALSE, col.names=TRUE, row.names=FALSE) # Nur COLnames
    dat <- utils::read.table(gzfile("C:\\Users\\U80823~2\\AppData\\Local\\Temp\\R\\filename.csv.gz"), sep=";", header=TRUE)
  })
  system.time(for(i in 1:1){ # Performance with own function. 25 sec
    wr1ite.table.zipped(df, file="C:\\Users\\U80823~2\\AppData\\Local\\Temp\\R\\filename2.zip", sep=";", quote=FALSE, col.names=TRUE, row.names=FALSE) # Nur COLnames
    dat2 <- read.table("C:\\Users\\U80823~2\\AppData\\Local\\Temp\\R\\filename2.zip", sep=";", header=TRUE)
  })
}


# Function is e.g. necessary for write.table.zipped & write.table (fast version)
.isNecessaryToUseTmpDir <- function(x, file, sizeThreshold=1400000) {
  # Gives the answer to the question if it is necessary to write x to a temporary dir and move it afterwards.
  # Arguments
  # x             = The object to be written to disk. E.g. a matrix or data.frame.
  # file          = The file to which the object should be written.
  # sizeThreshold = The threshold of the object.size(x) in bits. If the object is greater than that amount & file is on network drive, temp dir will be used first.
  
  return( substr( getFullyQualifiedFileName(file) ,1,1)%in%c("/","\\") && object.size(x)>sizeThreshold )
}

write.table.zipped <- function(x, file, ...){
  # This file creates a zip file that contains a csv file.
  # zip files created like this can
  # Arguments
  # x    = data.frame to be saved
  # file = file path and name
  # For futher arguments see write.table
  # Alternative:   write.table(x, file=gzfile("filename.csv.gz")))
  # combined with  read.table(gzfile("filename.csv.gz"))
  
  # Get filename and folder
  fil <- strsplit(file, "/|\\\\")[[1]]
  filename <- fil[length(fil)]
  parentdir <- ifelse(length(fil)==1, "", paste0( paste(fil[-length(fil)],collapse="/"), "/"))
  # Remove ending of filename. This is necessary to store csv and zip correctly. Speical paste0(,collapse=".") in case the filename contains several dots.
  fileNameSplitted <- strsplit(filename,"\\.")[[1]]
  if(length(fileNameSplitted)>1) {
    filenameWithoutEnding <- paste0( fileNameSplitted[-length(fileNameSplitted)], collapse=".")
    ending <- fileNameSplitted[length(fileNameSplitted)]
  } else {
    filenameWithoutEnding <- fileNameSplitted
    ending <- ""
  }
  if(ending=="zip") stop("Please use the ending of the data file (e.g. csv), not zip!")
  
  # Create temporary file
  if(.isNecessaryToUseTmpDir(x,file)){
    folder <- paste0(tempdir(), "/Rzipped")
    dir.create(folder, recursive=TRUE, showWarnings=FALSE)
  } else {
    folder <- parentdir
  }
  
  tmpCSVfile <- paste0(folder, "/", filename )
  write.table(x, tmpCSVfile, ...)
  
  # Create zip file containing csv. Remove temporary folder.
  zip.nodirs(zipfile=paste0(parentdir,filenameWithoutEnding, ".zip"), files=tmpCSVfile, remove.original=TRUE, showWarnings=FALSE, invoked.internally=TRUE)
  unlink(folder)
}





#df=as.data.frame(matrix(1:12,ncol=3)); colnames(df) <- c(letters[1:3]); write.table(df,"table.csv",sep=";",col.names=TRUE, row.names=FALSE); read.table("table.csv", header=TRUE, sep=";", choose.columns=c("b")); unlink("table.csv")
read.table <- function(file, header=FALSE, sep="", ..., choose.columns=NULL, Rtools="C:/Tools/Rtools"){
  # This edited versin of read.table detects compressed files from their file endings and directly reads them without unpacking.
  # It is assumed that only one file is within the zip archive. Otherwise the function will give an error message.
  # All arguments are used like in read.table.
  #
  # Special arguments:
  # choose.columns: character(), numeric() or logical() indicating which columns of the file should be read in. This saves RAM compared to dropping them afterwards. No NA values allowed.
  # Rtools:         if !is.null(choose.columns), then Rtools must be installed because the Rtools/bin/cut.exe and some dll libraries are needed.
  #                 The argument <Rtools> has to indicate the folder in which Rtools was installed.
  
  # If it is not a compressed file, then use the normal read.table() function.
  # If the file does not exist, try to read with read.table() so you will get exact the same error message.
  if(class(file)!="character" || !grepl("^.*(.gz|.bz2|.tar|.zip|.tgz|.gzip|.7z)[[:space:]]*$", file) || !file.exists(file) || !is.null(choose.columns) ) {
    # Simple reading in without choosing columns
    if(!file.exists(file) || is.null(choose.columns)){
      return( utils::read.table(file=file, header=header, sep=sep, ...) )
      # Special reading in when certain columns are chosen...
    } else {
      if(!file.exists(Rtools)) stop(paste0("If !is.null(choose.columns), then the R toolset and Cygwin DLLs of Rtools must be available. Rtools not found at location: ", Rtools))
      Rtools <- paste0(Rtools, "/bin/cut.exe")
      if(is.character(choose.columns)){
        colNames <- unname(unlist(read.table(file, sep=sep, nrow=1, header=FALSE, stringsAsFactors=FALSE)))
        choose.columns <- which(colNames%in%choose.columns)
      }
      if(is.logical(choose.columns)) choose.columns <- which(choose.columns)
      if(any(is.na(choose.columns))) stop("No NA-values allowed in choose.columns.")
      return( utils::read.table(pipe(paste0(Rtools, " -f",paste0(choose.columns,collapse=","), " -d", sep," ", file) ), header=header, sep=sep, ...))
    }
    
    # In case of archive ...
  } else {
    tryCatch({
      # List files within the archive and read directly from archive without unpacking.
      withinFile <- unzip(file, list=TRUE)[,"Name"]
      if(length(withinFile)>1) stop("Only 1 file inside archive is allowed. Otherwise try utils::read.table(unz(pathOfZipFile, nameOfCSVwithinZipFile)).")
      return( utils::read.table(unz(file, withinFile), header=header, sep=sep, ...) )
    }, error = function(e) {
      stop(paste0(e$message, "\nread.table() has encountered an error. This is not the original read.table() function but an edited version by Daniel Hoop in order to read data from compressed files without unpacking them.\nTry utils::read.table() to use the default function."))
    })
  }
}

# file <- "C:/Users/U80823148/_/ME/ME_data_out/data_final/2014/allcosts_info.zip"
if(FALSE) read.table_OLD_DELETE <- function(file, ..., choose.columns=NULL){
  # This edited versin of read.table detects compressed files from their file endings and directly reads them without unpacking.
  # It is assumed that only one file is within the zip archive. Otherwise the function will give an error message.
  # All arguments are used like in read.table.
  
  # If it is not a compressed file, then use the normal read.table() function.
  # If the file does not exist, try to read with read.table() so you will get exact the same error message.
  if(class(file)!="character" || !grepl("^.*(.gz|.bz2|.tar|.zip|.tgz|.gzip|.7z)[[:space:]]*$", file) || !file.exists(file) ) {
    return( utils::read.table(file, ...) )
  } else {
    tryCatch({
      # List files within the archive and read directly from archive without unpacking.
      withinFile <- unzip(file, list=TRUE)[,"Name"]
      if(length(withinFile)>1) stop("Only 1 file inside archive is allowed. Otherwise try utils::read.table(unz(pathOfZIPFile, nameOfCSVwithinZipFile)).")
      return( utils::read.table(unz(file, withinFile), ...) )
    }, error = function(e) {
      stop(paste0(e$message, "\nread.table() has encountered an error. This is not the original read.table() function but an edited version by Daniel Hoop in order to read data from compressed files without unpacking them.\nTry utils::read.table() to use the default function."))
    })
  }
}

read.table2 <- function(file, header=TRUE, sep=";", fileEncoding=""){
  # This read.table() function works in some special cases of wrongly encoded data.
  
  dat <- readLines(file, encoding=fileEncoding)
  dat <- do.call("rbind", strsplit(dat,sep))
  if(header) {
    cn1 <- dat[1,]
    dat <- dat[-1,]
    colnames(dat) <- cn1
  }
  dat <- char.cols.to.num(dat)
  return(dat)
}

# file <- "C:/Users/U80823148/_/Data/testfile.csv"
# write.table(1:(1e7+351981), file=file, col.names=FALSE, row.names=FALSE); system.time( print(c1ount.lines(file)) ); file.remove(file)
count.lines <- function(file) {
  # count lines of file.
  if( grepl("window",Sys.info()["sysname"],ignore.case=TRUE) ){
    file <- gsub("/","\\\\",file) # Replace all forward-slashes with backward-slashes
    file <- gsub("(\\\\){2,10}","\\\\",file) # Replace all multiple slashes with unique slashes.
    str <- paste0(system(paste0("find /c /v \"\" \"",file,"\" "), intern=TRUE), collapse="") # Looks for "nonblank" lines but actually also "blank" lines contain "\n" and are therefore counted.
    str <- unlist(strsplit(str, ":"))
    return(as.numeric(str[length(str)]))
  } else if( grepl("linux",Sys.info()["sysname"],ignore.case=TRUE) ){
    file <- gsub("\\\\","/",file) # Replace all backward-slashes with forward-slashes
    file <- gsub("(/){2,10}","/",file) # Replace all multiple slashes with unique slashes.
    str <- paste0(system(paste0("wc -l \"",file,"\""), intern=TRUE), collapse="")
    stop("Linux version not yet fully implemented.")
  }
}

# x <- matrix(1:10); nrowmax=10000; ncolmax=10000; folder=NULL; view(x)
view <- function(x, names=c("col","rowcol","row","no"), nrows=-1, ncols=-1, fastViewOfSubset=TRUE, folder=NULL, quote=FALSE, na="NA", sep=";", openFolder=FALSE, ...){
  # This function creates a CSV file from a data.frame/matrix and opens it with the default CSV-opening-program
  # of the computer.
  #
  # x      = data.frame/matrix
  # names  = dimnames to be saved in the file. "col"=colnames, "rowcol"=rownames&colnames, "row"=rownames, "no"=no dimnames
  # nrows  = maximum number of rows    to be saved (for higher speed with large datasets)
  #          if n=-1, all rows will be displayed.-> see also the help for read.table()
  # ncols  = maximum number of columns to be saved (for higher speed with large datasets)
  # folder = directory, where the temporary file should be saved.
  #          If NULL an accessible folder in C:/Users/.../Documents will be created automatically.
  # quote  = should quotes be written into the csv File? -> see also the help for write.table()
  # na     = how should NA values be displayed in the csv File? -> see also the help for write.table()
  # openFolder = Should the folder with all temporary files be opened after having created the file?
  
  txtFile <- FALSE
  if(is.null(dim(x))) {
    if(length(x)==1) txtFile <- TRUE
    x <- as.matrix(x)
  }
  if(is.null(colnames(x))) colnames(x) <- paste0("V",1:ncol(x))
  
  # Replace NaN by NA, because otherwise the argument na=... in write.table() will not work.
  if(!na%in%c("NA","NaN")){
    naReplace <- function(x){ x[grep("^nan?$",x,ignore.case=TRUE)] <- NA ;x} #x[is.na(x)] <- NA; 
    if(is.matrix(x)) x <- apply(x,2,function(x)naReplace(x)) else x <- do.call("rbind",lapply(x,function(x)naReplace(x)))
  }
  
  if(txtFile) names <- "no"
  names <- match.arg(names)
  
  # Check if data.frame should be shrinked for faster view
  if(nrows<0 && ncols<0){
    maxSize <- 40000000 # On Intel i7-4770 CPU with 3.40 GHz, approx 12 seconds are necessary to save file as csv and open in Excel.
    objSize <- object.size(x)
    if(fastViewOfSubset && objSize>maxSize){
      nrows <- (floor(nrow(x)*maxSize/objSize))
    }
  }
  if(nrows<0) nrows <- nrow(x)
  if(ncols<0) ncols <- ncol(x)
  
  # Shrink data.frame such that it can be saved & viewed faster.
  nrows <- min(nrow(x), nrows); if(nrows<nrow(x)) warning(paste0("data.frame displays only ",nrows," of ",nrow(x)," rows. Either change nrows or set fastViewOfSubset=FALSE."))
  if(nrows!=nrow(x)) x <- x[1:nrows,,drop=FALSE]
  ncols <- min(ncol(x), ncols); if(ncols<ncol(x)) warning(paste0("data.frame displays only ",ncols," of ",ncol(x)," cols. Change ncols to view the full data.frame."))
  if(ncols!=ncol(x)) x <- x[,1:ncols,drop=FALSE]
  
  
  # Define paths
  # If is.null(folder), wird ein temporaerer Ordner im Windows-Dateisystem angelegt.
  if(is.null(folder)) {
    folder <- paste0(tempdir(),"/Rview") # paste0(Sys.getenv("TMP"), "\\R\\Rview")
    dir.create(folder, recursive=TRUE, showWarnings=FALSE)
  }
  
  # Wenn am Schluss des Pfades kein "/" angefuegt wurde, wird dies gemacht:
  if( !substr(folder,nchar(folder),nchar(folder))%in%c("/","\\") ) folder <- paste0(folder, "\\")
  pfad0 <- folder
  name <- "Rview_tmp"
  nr <- "01"
  csv <- ifelse(txtFile, ".txt", ".csv")
  
  # Check if there are existing files in the folder
  fil <- list.files(pfad0)
  fil <- fil[ substr(fil,nchar(fil)-3,nchar(fil))==csv ]
  # If there are no files in the folder, use the default save path.
  if(length(fil)==0){
    pfad1 <- paste0(pfad0, name, nr, csv)
  } else {
    # Remove all files in the folder (if possible)
    fil <- paste0(pfad0, fil)
    # Only remove old txt files. Because they are not protected from being deleted.
    if(txtFile) fil <- fil[  difftime(Sys.time(), file.info(c(fil))$mtime, units="hours")>1  ]
    suppressWarnings( try( file.remove( fil )  , silent=TRUE) )
    fil <- list.files(pfad0)
    fil <- fil[ substr(fil,nchar(fil)-3,nchar(fil))==csv ]
    # If there are no files anymore use the default save path.
    if( length(fil)==0 ) {
      pfad1 <- paste0(pfad0, name, nr, csv)
    } else {
      # If there are sill files, read out the number of the newest file (with the highest number)
      mx <- max( as.numeric( substr(fil,nchar(fil)-5,nchar(fil)-4) ) )
      # Add 1 to the number of the file
      mxpl1 <- as.character( mx+1 )
      if(nchar(mxpl1)==1) mxpl1 <- paste0("0",mxpl1)
      # Create a new path
      pfad1 <- paste0(pfad0, name, mxpl1, csv)
    }
  }
  
  # Rownames und colnames, die mit +, - oder = anfangen, mit ' am Anfang versehen, dass es von Excel richtig dargestellt wird
  rn1 <- rownames(x)
  cn1 <- colnames(x)
  ind <- substr(rn1,1,1)%in%c("+","-","=")
  if(any(ind)) rownames(x)[ind] <- paste0(" ",rn1[ind])
  ind <- substr(cn1,1,1)%in%c("+","-","=")
  if(any(ind)) colnames(x)[ind] <- paste0(" ",cn1[ind])
  
  # Write CSV file & open.
  if(names=="row") {
    # If the first cell of the file is named "ID" Microsoft Excel warns that a SYLK file is opened. Therefore it is renamed.
    if(!is.null(rownames(x)[1]) & !is.na(rownames(x)[1])) if(substr(rownames(x)[1],1,2)=="ID") rownames(x)[1] <- paste0("lD", substring(rownames(x)[1],3,nchar(rownames(x)[1])))
    write.table(x, file=pfad1, sep=sep, col.names=FALSE, row.names=TRUE, quote=quote, na=na, ...)
  } else if (names=="col") {
    # If the first cell of the file is named "ID" Microsoft Excel warns that a SYLK file is opened. Therefore it is renamed.
    if(!is.null(colnames(x)[1]) & !is.na(colnames(x)[1])) if(substr(colnames(x)[1],1,2)=="ID") colnames(x)[1] <- paste0("lD", substring(colnames(x)[1],3,nchar(colnames(x)[1])))
    write.table(x, file=pfad1, sep=sep, col.names=TRUE, row.names=FALSE, quote=quote, na=na, ...)
  } else if (names=="rowcol") {
    write.table(x, file=pfad1, sep=sep, col.names=NA, quote=quote, na=na, ...)
  } else {
    write.table(x, file=pfad1, sep=sep, col.names=FALSE, row.names=FALSE, quote=quote, na=na, ...)
  }
  
  browseURL(pfad1)
  if(openFolder) {
    Sys.sleep(1)
    browseURL(folder)
  }
}

view.folder <- function() {
  browseURL(paste0(tempdir(),"/Rview"))# (paste0(Sys.getenv("TMP"), "\\R\\Rview"))
}

load2 <- function(file){
  # This function loads an object and returns it, so you can assign it to an variable of choice.
  # The name of the originally stored variable is not relevant anymore.
  
  load(file)
  ret <- ls()[ls()!="file"]
  if(length(ret)>1) stop(paste0("More than one object was loaded. The function only works with one object.\n",paste0(ret, collapse=", ")) )
  eval(parse(text=paste0("return(",ret,")")))
}

format.colnames <- function(x) {
  # This function converts all characters to lower case if they don't look like P100..., T100... or K100...
  # Argument x can be a vector of characters or a matrix/data.frame.
  # The converted character vector is returned.
  
  if(!is.null(dim(x))) x <- colnames(x)
  filtMM <- which( grepl("^([PTK][0-9]{3})",x) )
  x[-filtMM] <- tolower(x[-filtMM]) # To lower for all which are NOT like P100
  x[filtMM] <- gsub("\\.","_",x[filtMM]) # Replace . with _ for all which ARE like P100
  return(x)
}

load.spa <- function() {
  pfad1 <- paste0(.dataFolder(),"SpE.RData")
  pfad2 <- "//evdad.admin.ch/AGROSCOPE_OS/2/5/2/1/3/3/4276/alldata/SpE.RData"
  pfad <- if(file.exists(pfad1)) pfad1 else pfad2
  cat("Tabellen werden aus folgendem Verzeichnis geladen:\n")
  cat(pfad, "\n", sep="")
  spa <- load2(pfad)
  
  # Testen ob alle plausiblen Betriebe im Datensatz sind
  if(FALSE){
    BHJ <- 2016
    pfad_CRM_plaus_t0 <- paste0("//art-settan-1000.evdad.admin.ch/ZAMAIN/ZADaten/SpE/Liste_Plausible/B",BHJ,"/")
    fold <- list.files(pfad_CRM_plaus_t0)
    fold <- fold[grepl("Termin",fold)]
    fold <- sort(fold[file.info(paste0(pfad_CRM_plaus_t0, fold))$isdir], decreasing=TRUE)[1]
    plauslist <- read.table(paste0("//art-settan-1000.evdad.admin.ch/ZAMAIN/ZADaten/SpE/Liste_Plausible/B",BHJ,"/",fold,"/Plausible_B",BHJ,".csv"),  sep=";", skip=2, header=TRUE, stringsAsFactors=FALSE, quote = "\"")
    ausschlussList <- read.table(paste0("//evdad.admin.ch/AGROSCOPE_OS/2/5/2/1/3/2/4271/B",BHJ,"/IDBetriebeAusschliessenDB/Ausschliessen_B",BHJ,".csv"),  sep=";", skip=0, header=TRUE, stringsAsFactors=FALSE, quote = "\"")
    id_ok <- as.numeric( plauslist[ plauslist[,"DB_einlesen"]=="Ja" & !plauslist[,"Betriebsnummer"]%in%ausschlussList[,"REK_ID"] ,"Betriebsnummer"] )
    id_not_in_DB <- sort(id_ok[ !id_ok%in%spa[spa[,"JAHR"]==BHJ,"REK_ID"] ])
    # Pruefen, ob alle plausiblen Betriebe im Datensatz sind.
    if(length(id_not_in_DB)>0) {
      dat <- read.table(paste0("//art-settan-1000.evdad.admin.ch/ZAMAIN/ZADaten/SpE/ErhbogTxtExport/nichtVerkn/B",BHJ,"/ID_nVerkn_B",BHJ,".csv"), sep=";", header=TRUE, stringsAsFactors=FALSE, quote = "\"", na.strings=c("","NA","na","NULL","null","#DIV/0","#DIV/0!","#WERT","#WERT!"))
      id_not_verkn <- sort(dat[dat[,1]%in%id_not_in_DB,1])
      if(length(id_not_in_DB)!=length(id_not_verkn) || any(id_not_in_DB!=id_not_verkn)){
        warning(paste0("Es gibt Betriebe, die laut OTRS-Liste eigentlich in die Datenbank gehoeren. Sie sind aber nicht im Datensatz drin! Anbei die REK_IDs:\n",
                       paste0(id_not_in_DB,collapse=", "),
                       "\nVon den oben genannten REK_ID, konnten die folgenden nicht mit AGIS verknuepft werden:\n",
                       paste0(id_not_verkn,collapse=", ")))
      }
      if(FALSE){
        files <- list.files(paste0("//art-settan-1000.evdad.admin.ch/ZAMAIN/ZADaten/SpE/ErhbogTxtExport/Rohdat/B",BHJ), recursive=TRUE)
        files <- unique(files[substr.rev(files,1,4)==".txt"])
        files <- lapply(strsplit(files,"/"),function(x)x[length(x)])
        files <- substr(files,13,19)
        cat("Diese IDs kommen nicht in den Rohdaten-Files vor:\n")
        print(id_not_in_DB[!id_not_in_DB%in%files])
      }
    }
    
    # Pruefen, ob es Betriebe in DB gibt, die eigentlich nicht plausibel sind.
    spaID <- spa[spa[,"JAHR"]==BHJ,"REK_ID"]
    id_not_in_plauslist <- sort(spaID[!spaID%in%id_ok])
    if(length(id_not_in_plauslist)>0) {
      warning(paste0("Einige Betriebe, die in der Datenbank sind, sind laut CRM-OTRS-Liste nicht plausibel! Anbei die REK_IDs:\n",
                     paste0(id_not_in_plauslist,collapse=", ")))
    }
  }
  
  # Datensatz ausgeben.
  return(spa)
}
# Alias erzeugen
load.spe <- function(...) load.spa(...)
load.spe.pers <- function(){
  pfad <- "//evdad.admin.ch/AGROSCOPE_OS/2/5/2/1/3/3/4276/Personen/SpE_Personen_Indexiert.RData"
  return(load2(pfad))
}

# Fuer SpB
load.spb <- function() {
  pfad1 <- paste0(.dataFolder(),"SpB.RData")
  pfad2 <- "//evdad.admin.ch/AGROSCOPE_OS/2/5/2/1/3/3/4275/alldata/SpB.RData"
  pfad <- if(file.exists(pfad1)) pfad1 else pfad2
  cat("Tabellen werden aus folgendem Verzeichnis geladen:\n")
  cat(pfad, "\n", sep="")
  return(load2(pfad))
}
load.spb.pers <- function(){
  pfad <- "//evdad.admin.ch/AGROSCOPE_OS/2/5/2/1/3/3/4275/Personen/SpB_Personen_Indexiert.RData"
  return(load2(pfad))
}


load.gb <- function() {
  pfad1 <- paste0(.dataFolder(),"GB.RData")
  pfad2 <- "//evdad.admin.ch/AGROSCOPE_OS/2/5/2/1/3/3/4273/GB/GB.RData"
  pfad <- if(file.exists(pfad1)) pfad1 else pfad2
  cat("Tabellen werden aus folgendem Verzeichnis geladen:\n")
  cat(pfad, "\n", sep="")
  load(pfad)
  cat("**********\nGewichte in Jahr 2015 sind immer 1, da ab dann SpE die offizielle Stichprobe ist.\n**********\n")
  return(gb)
}

load.agis <- function(year=2015){
  pfad1 <- paste0(.dataFolder(),"AGIS/AGIS_BFS_",year,".RData")
  pfad2 <- paste0("//art-settan-1000.evdad.admin.ch/ZAMAIN/ZADaten/AGIS/",year,"/AGIS_BFS_",year,".RData")
  pfad <- if(file.exists(pfad1)) pfad1 else pfad2
  cat("Tabellen werden aus folgendem Verzeichnis geladen:\n")
  cat(pfad, "\n", sep="")
  return(load2(pfad))
}

load.cost <- function(years=2014, ignore_P_cols=TRUE, non_aggr=FALSE, filter_expression=NULL, parentDir=NULL){
  # This function loads full cost data from several years and combines them.
  #
  # Arguments
  # years         = Numerical vector containing the years to be loaded.
  # igonre_P_cols = Logical indicating if all columns with the ending "_P" (proportional allocation) should be ignored when reading in the data
  # non_aggr      = Logical indicating if the non aggregated joint costs should be loaded (i.e. animal categories like miku instead of entersprises like Milch)
  # filter_expression = An expressoin() containing a filter expression that is applied to each year while reading in the data.
  #                     this could look like expression(cost[ cost[,"BZ"]=="Milch" ,c("ID","Jahr","BZ","ArbeitNAT","Maschinen")])
  # parentDir     = The parent directory in which the folders of all years are located.
  
  if(!is.null(filter_expression) && !is.expression(filter_expression)) stop("filter_expression must be an expression! Use e.g. expression(cost[c(1,2),])")
  
  # Automatically choose the newest folder for the directories of Daniel.
  if(is.null(parentDir)){
    parentDir <- "C:/Tools/ME/ME_data_out/data_final"
    allFolders <- list.files(parentDir,full.names=TRUE); allFolders <- allFolders[file.info(allFolders)$isdir]; allFolders <- allFolders[ order(file.info(allFolders)[,"ctime"], decreasing=TRUE) ]
    take <- 1
    
    while(TRUE){
      if( length(list.dirs(allFolders[take],recursive=FALSE))==0 && take<=length(allFolders) ){
        take <- take + 1
      } else {
        parentDir <- allFolders[take]
        break
      }
    }
  }
  
  if(non_aggr) fileName <- "/allcosts_nonaggr.RData" else fileName <- "/allcosts_info.RData"
  
  # Loop over all years. Read in the data.
  cost1 <- NULL
  for(i in 1:length(years)) {
    pfad1 <- paste0(parentDir,"/",years[i],fileName)
    if(!file.exists(pfad1)) stop(paste("The file does not exist. You might have chosen the wrong parentDir.",
                                       "The following folders/files are available:", paste0(list.files(parentDir,full.names=TRUE),collapse="\n") ,sep="\n"))
    if(is.null(cost1)) cat("Tabellen werden aus folgendem Verzeichnis geladen:\n")
    cat(pfad1, "\n", sep="")
    cost <- load2(pfad1)
    # Filter data according to special expression
    if(!is.null(filter_expression)){
      cost <- eval(filter_expression)
    }
    # Berechnungen mit propoertionaler Zuteilung entfernen.
    if(ignore_P_cols) cost <- cost[,substr.rev(colnames(cost),1,2)!="_P"]
    tryCatch({
      cost1 <- rbind(cost1,cost)
    },
    error=function(e){
      cn1 <- colnames(cost1)
      cn2 <- colnames(cost)
      cn1_noMatch <- cn1[!cn1%in%cn2]
      cn2_noMatch <- cn2[!cn2%in%cn1]
      cat("*** FEHLER! ***\n")
      cat("Die Spalten der verschiedenen Jahre passen nicht zueinander!\n")
      if(length(cn1_noMatch)>0) cat("Spalten in ",years[i-1],", die in ",years[i  ]," nicht vorkommen: ", paste0(cn1_noMatch,collapse=", "),"\n",sep="")
      if(length(cn2_noMatch)>0) cat("Spalten in ",years[i  ],", die in ",years[i-1]," nicht vorkommen: ", paste0(cn2_noMatch,collapse=", "),"\n",sep="")
      cat("*** ******* ***\n")
      stop(e)
    })
    
    rm(cost); invisible(gc())
  }
  return(cost1);
}

rekid.zaid <- function(id, reverse=FALSE, BHJ=NULL, no.match.NA=TRUE){
  if(!exists("spa")) spa <- load.spa()
  
  res <- spa[,c("JAHR","REK_ID", colnames(spa)[colnames(spa)%in%c("BETRIEB","ZA_ID")][1] )]
  colnames(res)[colnames(res)=="BETRIEB"] <- "ZA_ID"
  if(!is.null(BHJ)) {
    res <- res[res[,"JAHR"]%in%BHJ,]
  } else {
    res <- res[order(res[,"JAHR"],decreasing=TRUE),]
  }
  
  if(!no.match.NA) {
    if(!reverse) return(res[res[,"REK_ID"]%in%id,])
    if( reverse) return(res[res[,"ZA_ID" ]%in%id,])
  } else {
    if(!reverse) matchCol <- "REK_ID" else matchCol <- "ZA_ID"
    res <- res[ match(id, res[,matchCol]) ,]
    res[,matchCol] <- id
    return(res)
  }
}

# id <- 72010409
id.entschluesseln <- function(...){
  pfad1 <- "C:/Users/U80823148/_/Data/"
  pfad2 <- "//evdad.admin.ch/AGROSCOPE_OS/2/5/2/1/2/4278/hpda/_ZA/Ref/Data/Grundlagenbericht/"
  
  pfad3 <- "GB__allg_Einzel"
  if(any(file.exists(c(paste0(pfad1,pfad3,".csv"), paste0(pfad1,pfad3,".RData"))))) pfad <- pfad1 else pfad <- pfad2
  
  # csv.to.rdata(paste0(pfad1,pfad2))
  if(!file.exists(paste0(pfad,pfad3,".RData"))) {
    dat <- read.csv(paste0(pfad,pfad3,".csv"), sep=";", header=TRUE, stringsAsFactors=FALSE, quote = "\"", na.strings=c("NA","","#DIV/0","#DIV/0!", "#WERT", "#WERT!"))
    save(dat, file=paste0(pfad,pfad3,".RData") )
  }
  load( paste0(pfad,pfad3,".RData") )
  
  id <- c(...)
  res <- dat[match(id, dat[,"ID"]),"ID_unverschluesselt"]
  return(res)
  
  #id <- id[!duplicated(id)]
  #if(length(id)==1){
  #  cbind(id,gb[,"ID_unverschluesselt"])
  
  #res <- dat[dat[,"ID"]%in%id,"ID_unverschluesselt"]
  #return(res[!duplicated(res)])
  #} else {
  #res <- dat[dat[,"ID"]%in%id,c("ID","ID_unverschluesselt")]
  #return(res[!duplicated(res),])
  #}
}

#folder <- "P:/_ZA/Ref/Data/Grundlagenbericht/"; filenames=NULL;extra_filename=NULL; update.files=FALSE; save.file=TRUE; save.name="GB"; filetype=c("csv"); NAto0=TRUE; header=TRUE; colnamesrow=1; skiprows=colnamesrow+1
merge.gb <- function(folder, filenames=NULL,extra_filename=NULL, update.files=FALSE, save.file=TRUE, save.name="GB", filetype=c("csv"), NAto0=TRUE, header=TRUE, colnamesrow=1, skiprows=colnamesrow+1, ...){
  # Grundlagenbericht importieren, indem die 6 csv Files nach der Vorlage der BO-Extraktionen "GB_A_Einzel, ..." eingelesen und aneinander gebunden werden.
  # F?r aus BO exportierte csv die Einstellung colnamesrow=5 und skiprows=6 beibehalten.
  # Wenn die csv-Datei schon mit colname ist, colnamesrow=1 und skiprows=1 einstellen.
  filetype <- match.arg(filetype)
  if(is.null(filenames))  filenames <- c("GB__allg_Einzel","GB_A_Einzel","GB_B_Einzel","GB_C_Einzel","GB_D_Einzel","GB_E_Einzel","GB_F_Einzel")
  filenames <- c(filenames, extra_filename)
  if(substr(folder,nchar(folder),nchar(folder))!="/") folder <- paste(folder,"/",sep="")
  fullnames <- paste(folder,filenames,sep="")
  gb.list <- list()
  if(filetype=="csv"){
    cat("Reading in files...\n")
    for(i in 1:length(fullnames)) {
      if(header){
        gb <- read.csv(paste(fullnames[i],".csv",sep=""), sep=";", stringsAsFactors=FALSE, header=TRUE, na.strings=c("NA","","#DIV/0","#DIV/0!", "#WERT", "#WERT!"))
        xvector <- c("X",paste("X.",1:30,sep=""))
        na.cols <- colnames(gb)%in%xvector
        gb <- gb[,!na.cols]
        if(colnames(gb)[1]!="ID")
          if(colnames(gb)[1]=="X.ID") colnames(gb)[1] <- "ID" else stop("The first colname of the imported file is not ID or ?ID.")
      } else {
        headers <- read.csv(paste(fullnames[i],".csv",sep=""), sep=";", nrows=colnamesrow, stringsAsFactors=FALSE, header=FALSE, na.strings=c("NA","","#DIV/0","#DIV/0!", "#WERT", "#WERT!"))
        headers <- as.character(headers[colnamesrow,])
        na.cols <- headers=="NA"
        headers <- headers[!na.cols]
        gb <- read.csv(paste(fullnames[i],".csv",sep=""), sep=";", skip=skiprows, stringsAsFactors=FALSE, header=FALSE, na.strings=c("NA","","#DIV/0","#DIV/0!", "#WERT", "#WERT!"))
        gb <- gb[,!na.cols]
        colnames(gb) <- headers
        if(colnames(gb)[1]!="ID")
          if(colnames(gb)[1]=="X.ID") colnames(gb)[1] <- "ID" else stop("The first colname of the imported file is not ID or ?ID.")
      }
      gb <- gb[!is.na(gb[,1]),]
      # Falls manche Spalten Strings enthalten, werden diese in numeric umgewandelt.
      char.cols <- !sapply(gb,function(x)is.numeric(x))
      gb[,char.cols] <-   suppressWarnings( sapply(gb[,char.cols],function(x)as.numeric(x)) )
      # Falls gew?nscht, werden die NA in 0 umgewandelt (nur bestimmte, bei denen 0 besser passt als NA)
      if(NAto0) {
        if(i%in%c(1,2,4,5,6,7)){
          gb[is.na(gb)] <- 0
        }
        if(i==3) {
          # Funktion definieren, um NAs zu ersetzen (mit lapply-Befehl)
          fillNA <- function(x){
            x[is.na(x)] <- 0
            return(x)
          }
          # Gewisse Spalten sollen nicht in 0 umgewandelt werden, der Rest wird zu 0 gemacht:
          convert.cols.names <- c("HFF_jeRGVE", "Flaeche_jeJAE", "kgMilch_jeKuh", "dtWeizen_jeha")
          convert.cols <- !colnames(gb)%in%convert.cols.names
          if( sum(!convert.cols)!= 4 ) {
            warning("Colnames of any of 'HFF_jeRGVE', 'Flaeche_jeJAE', 'kgMilch_jeKuh', 'dtWeizen_jeha' have changed and therefore the conversion of NA to 0 is not done correctly!", immediate.=TRUE)
            cat("These colnames are not available anymore in the data:\n")
            print(convert.cols.names[!convert.cols.names%in%colnames(gb)])
          }
          
          gb[, convert.cols ] <- lapply(gb[, convert.cols ],function(x)fillNA(x))
          # Bei den letzten 4 ist es jedoch umgekehrt. Hier geh?ren NAs rein!
          # gb[, !convert.cols ] <- lapply(gb[, !convert.cols ],function(x)fillNA(x,invert=TRUE))
        }
      }
      
      gb.list[[i]] <- gb
      cat(filenames[i], "complete\n")
    }
  }
  # Pr?fen, ob in allen Tabellen gleich viele Beobachtungen sind
  nrows <- do.call("c",lapply(gb.list,function(x)nrow(x)))
  if(any(nrows!=nrows[1]))  stop(paste0("Not the same year-filters were selected in the different csv files. You created an unbalanced panel.\n ",
                                        paste(paste(filenames, nrows, sep=": "),collapse="\n ")))
  # Pr?fen, ob ID und Jahr immer in der ersten Spalte stehen
  gb.names <- do.call("rbind",lapply(gb.list,function(x)colnames(x)[1:2]))
  if(any(c(gb.names[,1]!="ID", gb.names[,2]!="Jahr"))) stop("The first two columns of each Excel file must contain the ID and the accounting year. Names must be 'ID' and 'Jahr'")
  
  # Pr?fen, ob die Reihenfolge der Beobachtungen in allen Tabellen dieselbe ist
  orders <- list()
  for(i in 1:length(gb.list)){
    orders[[i]] <- match(gb.list[[i]][,"ID"],gb.list[[1]][,"ID"])
  }
  orders <- do.call("rbind",orders)
  # Falls die Reihenfolge unterschiedlich ist, wird sie gleich gemacht.
  if( any(!apply(orders,2,function(x)all(x==x[i]))) ) {
    for(i in 1:length(gb.list)) {
      gb.list[[i]] <- gb.list[[i]][orders[1,],]
    }
  }
  cat("IDs checked...\n")
  
  # "ID" Spalte wird von allen, ausser der ersten Tabelle entfernt.
  for(i in 2:length(gb.list)) {
    gb.list[[i]] <- gb.list[[i]][,!colnames(gb.list[[i]])%in%c("ID","Jahr")]
  }
  if(update.files){
    cat("Updating files (0 --> NA)...\n")
    for(i in 1:length(gb.list)){
      if(i==1) {
        write.table(gb.list[[i]], paste(fullnames[i],".csv",sep=""), sep = ";", col.names=TRUE, row.names=FALSE, ...)
      } else {
        write.table(cbind(gb.list[[1]][,c(1,2)], gb.list[[i]]), paste(fullnames[i],".csv",sep=""), sep = ";", col.names=TRUE, row.names=FALSE, ...)
      }
      cat(filenames[i], "complete\n")
    }
  }
  # Alle Listenelemente werden zu einer Matrix zusammengefasst
  gb.list <- do.call("cbind",gb.list)
  
  # Nun werden die SAK berechnet
  SAK_variables <- c("GVE_Milchkuehe","GVE_Schafe","GVE_Ziegen"
                     ,"Stk_Mutterkuehe","GVE_Aufzucht_Miku","GVE_Mastvieh_gross","GVE_MuKuhKalb_1Jminus","GVE_AndereKaelber","GVE_Pferde","GVE_UebrigeRaufuTiere","GVE_Gefluegel_tot","GVE_UebrigeTiere"
                     ,"GVE_Zuchtschweine","GVE_Mastsschweine","GVE_Ferkel"
                     ,"LN","SpezialkulturFlaeche", "rohHangbeitr")
  if(all(SAK_variables%in%colnames(gb.list))) {
    gb.list[,"SAK"] <- with(gb.list,
                            0.043*(GVE_Milchkuehe+GVE_Schafe+GVE_Ziegen) +
                              0.03*(Stk_Mutterkuehe + GVE_Aufzucht_Miku + GVE_Mastvieh_gross + GVE_MuKuhKalb_1Jminus + GVE_AndereKaelber + GVE_Pferde + GVE_UebrigeRaufuTiere + GVE_Gefluegel_tot + GVE_UebrigeTiere) +
                              0.04*GVE_Zuchtschweine + 0.007*(GVE_Mastsschweine + GVE_Ferkel) +
                              0.028*(LN-SpezialkulturFlaeche) + 0.3*SpezialkulturFlaeche + 0.015*rohHangbeitr/370
    )
    bio <- as.numeric(gb.list[,"Landbauform"]%in%c(3,4))
    gb.list[,"SAK"] <- with(gb.list, SAK + bio*0.2* ( 0.028*(LN-SpezialkulturFlaeche) + 0.3*SpezialkulturFlaeche )  )
    rm(bio)
  } else {
    warning("SAK wurden nicht berechnet, da folgende Variablen fehlen:")
    print(SAK_variables[!SAK_variables%in%colnames(gb.list)])
  }
  rm(SAK_variables)
  
  # Nun noch fuer jeden Kanton das K?rzen anfuegen
  Kanton_col <- colnames(gb.list)%in%"Kanton"
  if(!any(Kanton_col)) {
    warning("Kanton Nr. cannot be converted to name because there is no column name 'Kanton' in the data.", immediate.=TRUE)
  } else {
    kantons <- matrix(c(1:27, "ZH", "BE", "LU", "UR", "SZ", "OW", "NW", "GL", "ZG", "FR", "SO", "BS", "BL", "SH", "AR", "AI", "SG", "GR", "AG", "TG", "TI", "VD", "VS", "NE", "GE", "JU", "FL"), ncol=2)
    kanton_nr <- gb.list[,"Kanton"]
    kanton_names <- character()
    for(i in 1:nrow(kantons)){
      kanton_names[kanton_nr==kantons[i,1]] <- kantons[i,2]
    }
    gb.list <- cbind(gb.list[, 1:which(colnames(gb.list)%in%"Kanton") ],
                     "Kanton2"=kanton_names,
                     gb.list[, (which(colnames(gb.list)%in%"Kanton")+1):ncol(gb.list) ], stringsAsFactors=FALSE)
  }
  
  # Ersetzen von Umlauten
  uml0 <- c("?", "?", "?", "?", "?", "?")
  uml1 <- c("Ae","Oe","Ue","ae","oe","ue")
  for(i in 1:length(uml0)){
    colnames(gb.list) <- gsub(uml0[i],uml1[i],colnames(gb.list))
  }
  
  # Speichern
  if(save.file) {
    cat("Saving CSV-file...\n")
    if(any(filenames%in%save.name)) save.path <- paste(folder,save.name, "2.csv",sep="") else save.path <- paste(folder,save.name,".csv",sep="")
    write.table(gb.list, save.path, sep = ";", col.names=TRUE, row.names=FALSE, ...)
    cat("Saving RData-file...\n")
    gb <- gb.list; rm(gb.list)
    if(any(filenames%in%save.name)) save.path <- paste(folder,save.name, "2.RData",sep="") else save.path <- paste(folder,save.name,".RData",sep="")
    save(gb, file=save.path)
  }
  invisible(gb)
  cat("Job done!")
}

####

#filepath <- paste0(pfad,"Auswertung_Abschr_lang.csv")
#                                 header=TRUE; remove.middle.rows.cols=TRUE;  save.file=TRUE; print.info=TRUE
repair.csv <- function(filepath, header=TRUE, remove.middle.rows.cols=FALSE, save.file=TRUE, print.info=TRUE, ...){
  # If Excel cannot read a csv file anymore apply this function to the file.
  # By default only the NA rows (and cols) at the end below (right) of the table are removed.
  # The function can also be use as a read.table function that automatically removes NA rows and cols.
  # For that purpose use:
  #    repair.csv("filepath", header=TRUE, remove.middle.rows.cols=TRUE, save.file=FALSE)
  data.in <- read.csv(filepath, sep=";", header=header, stringsAsFactors=FALSE, na.strings=c("NA","","#DIV/0","#DIV/0!", "#WERT", "#WERT!"), ...)
  
  # Exclue NA cols
  if(is.data.frame(data.in)) { colNAs <- sapply(data.in,function(x)all(is.na(x)))
  } else { colNAs <- apply(data.in,2,function(x)all(is.na(x))) }
  
  colNAs3 <- integer(0)
  last.col.NA <- FALSE
  if (length(which(colNAs))>0) if(max(which(colNAs))==length(colNAs)) last.col.NA <- TRUE
  if(!remove.middle.rows.cols & last.col.NA){ # Wenn die mittleren Spalten behalten werden sollen, aber die letzte leer ist...
    colNAs1 <- rev(colNAs)
    colNAs2 <- numeric()
    setNA <- 1
    while(setNA!=0){
      if(colNAs1[setNA]) {
        colNAs2[setNA] <- setNA
        setNA <- setNA+1
      } else { setNA <- 0 }
    }
    colNAs3 <- (length(colNAs)-length(colNAs2)+1):length(colNAs)
  } else if(remove.middle.rows.cols) {
    colNAs3 <- which(colNAs)
  }
  if(length(colNAs3)>0) {
    if(print.info){
      cat("The following cols where removed\n")
      print(colnames(data.in)[colNAs3])
    }
    data.in <- data.in[,-colNAs3]
  }
  
  # Exclue NA rows
  rowNAs <- apply(data.in,1,function(x)all(is.na(x)))
  
  rowNAs3 <- integer(0)
  last.row.NA <- FALSE
  if (length(which(rowNAs))>0) if(max(which(rowNAs))==length(rowNAs)) last.row.NA <- TRUE
  if(!remove.middle.rows.cols & last.row.NA){ # Wenn die mittleren Spalten behalten werden sollen, aber die letzte leer ist...
    rowNAs1 <- rev(rowNAs)
    rowNAs2 <- numeric()
    setNA <- 1
    while(setNA!=0){
      if(rowNAs1[setNA]) {
        rowNAs2[setNA] <- setNA
        setNA <- setNA+1
      } else { setNA <- 0 }
    }
    rowNAs3 <- (length(rowNAs)-length(rowNAs2)+1):length(rowNAs)
  } else if(remove.middle.rows.cols) {
    rowNAs3 <- which(rowNAs)
  }
  if(length(rowNAs3)>0) {
    if(print.info){
      cat("The following rows where removed\n")
      print(rowNAs3)
    }
    data.in <- data.in[-rowNAs3,]
  }
  
  if(save.file) write.table(data.in, file=filepath, sep = ";", col.names=header, row.names=FALSE)
  invisible(data.in)
}
#data.in <- matrix(c(
#  NA,NA,NA,NA,NA,NA,
# 10,10,NA,10,NA,NA,
#  10,10,NA,10,NA,NA,
#  10,10,NA,10,NA,NA,
#  NA,NA,NA,NA,NA,NA,
#  10,10,NA,10,NA,NA,
#  NA,NA,NA,NA,NA,NA,
#  NA,NA,NA,NA,NA,NA),
#                  byrow=TRUE,ncol=6)
#remove.middle.rows.cols <- TRUE
####

remove.na.rowcols.of.csv <- function(filepath, print.info=FALSE){
  repair.csv(filepath=filepath, header=TRUE, remove.middle.rows.cols=TRUE, save.file=TRUE, print.info=print.info)
}



csv.to.rdata <- function(path, dat=NULL, name="dat", clean.numb.format=FALSE, clean.dat.columns=FALSE, update.csv=FALSE, ...){
  # This function converts csv data to RData (which can be read in much faster)
  
  # Remove .csv or .RData from string.
  if( tolower(substr.rev(path,1,4))==".csv" ) path <- substr(path,1,nchar(path)-4)
  if( tolower(substr.rev(path,1,6))==".rdata" ) path <- substr(path,1,nchar(path)-6)
  
  # If a path is given, read in new data. Else the data.frame 'dat' is used for further processing.
  if(is.null(dat)) {
    dat <- read.csv(paste0(path, ".csv"), sep=";", header=TRUE, stringsAsFactors=FALSE, quote = "\"", na.strings=c("","NA","na","NULL","null","#DIV/0","#DIV/0!","#WERT","#WERT!"))
  } else {
    cat("Argument 'dat=' was given. Reading in no new data but directly using 'dat' from workspace!\n")
  }
  if(colnames(dat)[1]=="X.ID") colnames(dat)[1] <- "ID"
  
  # Clean columns, if wished
  if(clean.dat.columns) dat <- clean.data.columns(dat, ...)
  # Clean number formats if there are 1'000 formats, if whished
  if(clean.numb.format) dat <- clean.number.format(dat, ...)
  
  # Save RData
  assign(name, dat)
  eval(parse(text= paste0("save(",name,", file=",paste0("'", path, ".RData'") ,")")  ))
  # Update CSV, if wished
  if(update.csv) write.table(dat, file=paste0(path, ".csv"), sep = ";", eol = "\n", quote=FALSE, col.names=TRUE, row.names=FALSE) # Nur COLnames
}

rdata.to.csv <- function(path){
  # This function reads in an RData file and saves it as csv.
  # Remove .csv or .RData from string.
  if( tolower(substr.rev(path,1,4))==".csv" ) path <- substr(path,1,nchar(path)-4)
  if( tolower(substr.rev(path,1,6))==".rdata" ) path <- substr(path,1,nchar(path)-6)
  write.table(load2(paste0(path,".RData")), file=paste0(path,".csv"), sep = ";", eol = "\n", quote=FALSE, col.names=TRUE, row.names=FALSE) # Nur COLnames
}

read.cb <- function(names=c("col","rowcol","row","no"), no.data.frame=FALSE, ...) {
  # Read tables that are stored in the clipboard (e.g. copied in excel)
  
  if(no.data.frame){
    # Suppress the warning incomplete final line found on 'clipboard'
    return(suppressWarnings(readLines('clipboard')))
  }
  
  names <- match.arg(names)
  if(names=="row") {
    dat <- read.table("clipboard", sep="\t", header=FALSE, stringsAsFactors=FALSE, ...)
    rownames(dat) <- dat[,1]; dat <- dat[,-1]
    return(dat)
  } else if (names=="col") {
    return( read.table("clipboard", sep="\t", quote="", header=TRUE, stringsAsFactors=FALSE, ...) )
  } else if (names=="rowcol") {
    dat <- read.table("clipboard", sep="\t", quote="", header=TRUE, stringsAsFactors=FALSE, ...)
    rownames(dat) <- dat[,1]; dat <- dat[,-1]
    return(dat)
  } else {
    return( read.table("clipboard", sep="\t", quote="", header=FALSE, stringsAsFactors=FALSE, ...) )
  }
}

write.cb <- function(data, names=c("col","rowcol","row","no"), ...){
  # Save a matrix into the clipoard.
  
  # If a vector is given as argument it is convertet to a matrix and rownames are given.
  if(is.null(dim(data))) {
    if(length(names)>1) names <- "row"
    names_data <- names(data)
    data <- as.matrix(data)
    names(data) <- names_data
  }
  
  names <- match.arg(names)
  
  if(names=="row") {
    write.table(data, 'clipboard', sep='\t', quote=FALSE, col.names=FALSE, row.names=TRUE, ...)
  } else if (names=="col") {
    write.table(data, 'clipboard', sep='\t', quote=FALSE, col.names=TRUE, row.names=FALSE, ...) # Nur colnames
  } else if (names=="rowcol") {
    write.table(data, 'clipboard', sep='\t', quote=FALSE, col.names=NA, ...)                    # Colnames & Rownames
  } else {
    write.table(data, 'clipboard', sep='\t', quote=FALSE, col.names=FALSE, row.names=FALSE, ...)
  }
}
read.xlsx <- function(filename, sheet=1, ...){
  # This function reads xls and xlsx files into a data.frame. Package XLConnect must be installed.
  # Sheet can also be given as character. If all sheets of the workbook should be imported, rather use loadWorkbook{XLConnect}
  # http://www.r-bloggers.com/read-excel-files-from-r/
  require.package(XLConnect) # #options ( java.parameters = "-Xmx1024m ")
  
  wb = loadWorkbook(filename, create=FALSE)
  return( readWorksheet(wb, sheet=sheet, ...) )
}

#mat <- matrix(rep(c("\"b\"&char(10)&\"b\""),10), ncol=2); colnames(mat) <- c("test1","test2"); rownames(mat) <- 1:nrow(mat); data <- list(mat,mat); names(data) <- c("sheet1","sheet2"); 
#file="testfile.xlsx"; sheetName=NULL; row.names=TRUE; col.names=TRUE; append=FALSE; asFormula=TRUE; wrapText=FALSE; columnWidth=100; convertColumnWith="default"
#write.xlsx(file=file, data=data, wrapText=TRUE, columnWidth=list(50, c(100,400)), asFormula=list(c(TRUE,FALSE),FALSE), convertColumnWith="pixel")
write.xlsx <- function(file, data, sheetName=NULL, row.names=FALSE, col.names=TRUE, append=FALSE, asFormula=FALSE, wrapText=FALSE, columnWidth=-1, convertColumnWith=c("default","pixel","no")){
  # This function writes a xls file from either a matrix/data.frame or a list conaining several matrices/data.frames.
  # It is a convenient version of writeWorksheetToFile{XLConnect}
  # Arguments
  # file:        The file of the xls file
  # data:        A matrix/data.frame or list containing matrices/data.frames
  # sheetName:   Optional sheet names in the Excel file. If NULL, then names Sheet1, Sheet2, etc. are given.
  # row.names:   logical indicating if rownames should be written or character giving the header for rowname colum. Single value or list.
  # col.names:   logical indicating if a header should be written. Single value or list.
  # asFormula:   logical indicating if formulas should be written rather than values. Note that no "=" sign at the beginning of the cell is required.
  # append:      logical indicating if the excel file should be appended rather than newly created
  # wrapText:    logical indicating if the text in the cells should be wrapped (Zeilenumbruch)
  # columnWidth: The column width that will be applied to all columns. If -1, then auto-width is applied.
  # convertColumnWith: If no, then the standard of XLConnect is used. It is the 1/256 part of a character. "default" will convert in 1 character units. "pixel" will convert to number of pixel.
  
  require.package(XLConnect) # options ( java.parameters = "-Xmx1024m ")
  
  convertColumnWith <- match.arg(convertColumnWith)
  if(is.matrix(data) || is.data.frame(data)) data <- list(data)
  if(is.null(names(data))) names(data) <- paste0("Sheet",1:length(data))
  if(is.null(sheetName) && any(duplicated(names(data)))) stop("Each list place in data must contain a unique name. Otherwise speccify the sheetName argument.")
  
  if(!is.null(sheetName)){
    if(is.list(sheetName)) sheetName <- unlist(sheetName)
    if(length(data)!=length(sheetName)) stop("length(data) has to be equal length(sheetName)")
    if(any(duplicated(unlist(sheetName)))) stop("There must be no duplicated sheet names. Otherwise the sheets will be overwritten.")
  } else {
    sheetName <- names(data)
  }
  
  convertRowColNames <- function(nam, errorNam="names", expand=FALSE, allowCharacters=FALSE, checkDimFunc=NULL) { # trueVal=TRUE, falseVal=FALSE, 
    if(!is.list(nam)) nam <- list(nam)
    if(!allowCharacters) if(any(sapply(nam,function(x)!is.logical(x)))) stop("row.names must be logical.")
    if(length(nam)==1) nam <- lapply(sheetName, function(x)nam[[1]])
    if(expand) nam <- expandListPlaces(nam)
    if(length(nam)!=length(sheetName)) stop(paste0("length(",errorNam,") must be either 1 or length(data). If you want to specify for each column try different things like c(2,10,15) or list(c(5,10,15)) or list(TRUE,FALSE,TRUE)"))
    if(!is.null(checkDimFunc)){
      errorFlag <- FALSE;
      for(i in 1:length(nam)) errorFlag <- errorFlag | ( !is.null(data[[i]]) && length(nam[[i]])>1 && length(nam[[i]])!=ncol(data[[i]]) )
      if(errorFlag) stop(paste0("if ", errorNam, " are specified separately in a list place for each file, then they must be either of length 1 or equal to ncol of data."))
    }
    return(nam)
  }
  expandListPlaces <- function(x){
    for(i in 1:length(x))
      if(!is.null(data[[i]]) && length(x[[i]])==1) x[[i]] <- rep(x[[i]], ncol(data[[i]]))
    return(x)
  }
  rownames <- convertRowColNames(row.names, errorNam="row.names", expand=FALSE, allowCharacters=TRUE, checkDimFunc=NULL)#, trueVal="", falseVal=NULL)
  header <- convertRowColNames(col.names, errorNam="col.names", expand=FALSE, allowCharacters=FALSE, checkDimFunc=NULL)#, trueVal=TRUE, falseVal=FALSE)
  asFormula <- convertRowColNames(asFormula, errorNam="asFormula", expand=TRUE, allowCharacters=FALSE, checkDimFunc=ncol)#, trueVal=TRUE, falseVal=FALSE)
  columnWidth <- convertRowColNames(columnWidth, errorNam="columnWidth", expand=FALSE, allowCharacters=TRUE, checkDimFunc=ncol)#, trueVal="", falseVal="",  )
  columnWidth <- lapply(columnWidth, function(x){
    if(convertColumnWith=="default") x * 256 else
      if(convertColumnWith=="pixel") x * 256 / 7 else
        x
  })

  
  if(substr.rev(file,1,4)==".xls") {
    warning("file must end with xlsx. The file was appended.")
    file <- paste0(file,"x")
  }
  if(file.exists(file) && !append) file.remove(file)
  
  # Create the file and write...
  if(!wrapText && sum(unlist(asFormula))==0 && all(unlist(columnWidth)==-1)){
    writeWorksheetToFile(file=file, data=data, sheet=sheetName, rownames=rownames, header=col.names)
  } else {
    wb <- loadWorkbook(file=file, create=!append)
    if(wrapText){
      cs <- wb$createCellStyle()
      cs$setWrapText(wrap=TRUE)
    }
    for(i in 1:length(data)){ # i <- 1
      wb$createSheet(name=names(data)[i])
      if(!is.null(data[[i]])){
        
        wb$writeWorksheet(data=data[[i]], sheet=names(data)[i], header=header[[i]], rownames=rownames[[i]])
        addR <- as.numeric(header[[i]])
        if(any(asFormula[[i]])) {
          for(c1 in 1:ncol(data[[i]])) if(asFormula[[i]][c1]) for(r1 in 1:nrow(data[[i]])) wb$setCellFormula(sheet=names(data)[i], formula=data[[i]][r1,c1], row=r1+addR, col=c1)
        }
        
        if(wrapText)
          for(c1 in 1:ncol(data[[i]])) for(r1 in 1:nrow(data[[i]])) wb$setCellStyle(sheet=names(data)[i], cellstyle=cs, row=r1+addR, col=c1)
        
        if(any(columnWidth[[i]] != -1)){
          if(length(unique(columnWidth[[i]]))==1) {
            wb$setColumnWidth(sheet=names(data)[i], column=1:ncol(data[[i]]), width=columnWidth[[i]])
          } else {
            for(c1 in 1:ncol(data[[i]])) if(columnWidth[[i]][c1] != -1) wb$setColumnWidth(sheet=names(data)[i], column=c1, width=columnWidth[[i]][c1])
          }
        }
          
      }
    }
    saveWorkbook(wb)
  }
}


#path <- "C:/Users/U80823148/_/ME/ME_data_out/data_final"
list.nodirs <- function(path = ".", ...){
  # List all files but no directories.
  fil <- list.files(path, ...) # ...
  return( fil[!file.info(fil)$isdir] )
}

#zipfile <- "C:/Users/U80823148/_/ME/ME_data_out/data_final/2014/allcosts_info.zip"
#remove.original=TRUE; showWarnings=TRUE
#z1ip.nodirs(zipfile, files, remove.original=TRUE, showWarnings=FALSE)
zip.nodirs <- function(zipfile, files, remove.original=FALSE, showWarnings=TRUE, invoked.internally=FALSE, ...){
  # This function packs all files with full path names into a zip file that will not contain the subfolders.
  # Arguments
  # zipfile         = The zipfile containing all compressed files.
  # files           = All files to be packed into the zip file.
  # remove.original = Should original files be removed after they were packed into the zip file? If there was an error while zipping, the original files will not be removed.
  # showWarnings    = Should warnings be showed if the zipfile already exists?
  
  # Check if zipfile already exists.
  if(file.exists(zipfile)){
    if(file.info(zipfile)$isdir) stop("zipfile must not be a directory.")
    if(showWarnings) {
      if(remove.original) stop("zipfile already exists. Because remove.original==TRUE, for safety reasons, the zipping was stopped. Set showWarnings=FALSE to ignore this message.")
      warning("zipfile already existed. Content was updated.")
    }
  }
  
  # Assure fully qualified filenames.
  zipfile <- winSlashes(getFullyQualifiedFileName(zipfile))
  files <- getFullyQualifiedFileName(files)
  
  # Save original working directory.
  wd <- getwd()
  # Split filenames by / or \
  fil <- strsplit(files, "/|\\\\")
  
  # Safetycheck for duplicated filenames that would be overwritten within the zipfile.
  # x <- fil[[1]]
  filcheck <- unlist(lapply(fil,function(x)x[length(x)]))
  if(any(duplicated(filcheck))){
    stop(paste0("Some filenames are identical. Duplicated filenames would be overwritten within the zip. Please choose unique filenames of the following files:\n", paste0(unique(filcheck[duplicated(filcheck)]),collapse=", ")))
  }
  
  # Prepare function to call in case of error or warning
  errorFunc <- function(filename){
    setwd(wd)
    if(invoked.internally && remove.original) suppressWarnings(file.remove(files))
    if( isTRUE(!file.info(zipfile)$isdir) ) suppressWarnings(file.remove(zipfile)) # Remove zip anyway.
    stop(paste0(filename, " was not zipped!", if(!invoked.internally && remove.original) " Original files were *NOT* removed!"))
  }
  
  
  # Pack all files into zipfile.
  # x <- fil[[1]]
  lapply(fil, function(x){
    filename <- x[length(x)]
    if(length(x)>1){ # If there is more then 1 part in path, then it's the full path, not only filename.
      parentdir <- paste(x[-length(x)],collapse="/")
      setwd(parentdir)
    }
    tryCatch( zip(zipfile, filename),
              error=function(e)errorFunc(filename),
              warning=function(w)errorFunc(filename))
  })
  
  # Set original working directory.
  setwd(wd);
  # Remove original files if wished.
  if(remove.original) invisible(file.remove(files))
}

#filename <- "Eink_A/delete"
getFullyQualifiedFileName <- function(filename){
  # This function checks if a fully qualified filename was given.
  # If not, then the working directory is pasted to the filename.
  # Arguments
  # filename = Charactor vector containing the filename(s) to be checked.
  
  # Recursive definition in case several filenames where given
  if(length(filename)>1) return( apply(matrix(filename),1,getFullyQualifiedFileName) )
  
  # If the splitted filename with / only is of length one, then it can only be a single filename without preceding folder.
  if(length( strsplit(filename, "/|\\\\")[[1]]  )==1){
    return( paste(getwd(),filename,sep="/") )
  } else {
    # Check if the filename is valid only together with the current wd. If so, then expand the filename.
    wd <- getwd() # Store working directory.
    setwd(tempdir()) # Set wd to tmp.
    if(file.exists(filename)){  # If file exists, then it is an absolute path
      setwd(wd)
      return( filename )
    }
    # Now try to write a file. If filename is not fully qualified, then this should give an error.
    newFilename <- NULL
    newFilename <- tryCatch(suppressWarnings( write(1,filename) ),
                            error=function(e) return(paste(wd,filename,sep="/")) )
    suppressWarnings(file.remove(filename))
    if(!is.null(newFilename)) filename <- newFilename
    setwd(wd)
    # Alternative but slower on network drives. Original takes 1.54 secs for 1000 files. This one takes 6.86 secs for 1000 files.
    #appendWd <- tryCatch({ suppressWarnings(write(1,paste0(getwd(),"/",filename))); suppressWarnings(file.remove(filename)); return(FALSE) },  error=function(e) return(TRUE) )
    #if(appendWd) filename <- paste0(getwd(),"/",filename)
  }
  
  return(filename)
}

splitFileNameIntoParts <- function(filename){
  # This funciton splits a filename into the directory, filename and extension.
  # It returns data.frame(dir=..., file=..., extension=...)
  
  if(length(filename)>1) return( as.data.frame(do.call("rbind", lapply(filename, splitFileNameIntoParts)), stringsAsFactors=FALSE) )
  
  # Get the parent directory.
  dir <- dirname(filename)
  
  # Grasp filename and extension
  fileName1 <- basename(filename)
  fileName2 <- strsplit(fileName1,"\\.")[[1]]
  if(length(fileName2)>1){
    fileName <- paste(fileName2[1:(length(fileName2)-1)],collapse=".")
    extension <- fileName2[length(fileName2)]
  } else {
    fileName <- fileName1
    extension <- ""
  }
  # Grasp filename and extension. ALTERNATIVE. This is not as reliable. Does not work for filenames that start with a dot. Like ".hiddenLinuxFile"
  #filename <- basename(filename) # Without directory
  #file <- tools::file_path_sans_ext(filename)
  #extension <- tools::file_ext(filename)
  
  # Return result
  return(data.frame(dir=dir, file=fileName, extension=extension, stringsAsFactors=FALSE))
}


makeBackupOfFile <- function(file, backupSubFolder="backup", timeAccuracy=c("s","m","h","d"), removeOriginal=FALSE){
  # This function makes a copy of a file in the backupSubFolder. The copy is appended with the date and time of the last changed file info.
  # E.g. C:/test/file.txt will be moved to C:/test/old/file_2017-01-01_10h.txt
  #
  # Arguments
  # file =         The filename of the file to make a backup of.
  # backupSubFolder = The backup sub directory where the backup file should be put into.
  # tmieAccuracy = The time accuray to put after the backuped filename. s="date-hour-minute-second", m="date-hour-minute", h="date-hour", d="date"
  
  # Recursive definition in case several arguments are given.
  if(length(file)>1)
    return(apply(matrix(file),1,function(x)makeBackupOfFile(file=x, backupSubFolder=backupSubFolder, timeAccuracy=timeAccuracy, removeOriginal=removeOriginal)))
  
  if(file.exists(file)) {
    # Time accuracy pre calculations
    timeAccuracy <- match.arg(timeAccuracy)
    timeSubstr <- if(timeAccuracy=="s") 19 else if(timeAccuracy=="m") 16 else if(timeAccuracy=="h") 13 else if(timeAccuracy=="d") 10
    timeExt <- if(timeAccuracy=="s") "" else if(timeAccuracy=="m") "m" else if(timeAccuracy=="h") "h" else if(timeAccuracy=="d") ""
    # Split filename into Parts
    path <- splitFileNameIntoParts(file)
    path[path[,"extension"]!="","extension"] <- paste0(".",path[path[,"extension"]!="","extension"])
    # Create backupSubFolder
    pathBak <- paste0(path[,"dir"],"/",backupSubFolder,"/")
    suppressWarnings(dir.create(pathBak))
    # Append the time
    mtime <- gsub(" ","_",substring(as.character(file.info(file)$mtime),1,timeSubstr) )
    mtime <- gsub(":","-",mtime)
    fileNew <- paste0(pathBak,path[,"file"],"_",mtime,timeExt,path[,"extension"])
    # If there already was an old backup then remove it. Afterwards move/copy the new one.
    suppressWarnings(file.remove(fileNew))
    copyHasWorked <- if(removeOriginal) file.rename(file, fileNew) else file.copy(file, fileNew)
    return(copyHasWorked)
  } else {
    return(FALSE)
  }
}


#### CORRELATIONS, REGRESSIONS ####
####
#data <- gb; sig.level=c(0.1,0.05,0.01,0.001)[2]; method=c("pearson", "kendall", "spearman"); conf.level=NULL; conf.middle=TRUE; conf.middle.sign=TRUE; digits=2; triangle=FALSE; del.diag=TRUE; ignore.nonsig=FALSE; suppressWarnings=FALSE;
#colnames(gb)[183]
#gb[1:10,183]
cor.table <- function(data, sig.level=c(0.1,0.05,0.01,0.001)[2], method=c("pearson", "kendall", "spearman"), conf.level=NULL, conf.middle=TRUE, conf.middle.sign=TRUE, digits=2, triangle=FALSE, del.diag=TRUE, ignore.nonsig=FALSE, suppressWarnings=FALSE, count=FALSE, special.sign.before.nonsig.for.excel=FALSE, ...){
  method <- match.arg(method)
  if(!is.null(conf.level)) if(method!="pearson") stop("confidence intervals can only be calculated with the pearson method")
  
  if(is.matrix(data)) if(!is.numeric(data))     stop("The matrix isn't numeric")
  if(is.data.frame(data)) {
    char.cols <- !sapply(data,function(x)is.numeric(x))
    if(any(char.cols)) {
      invisible(colnames(data)[char.cols])
      cat("\n")
      dput(colnames(data)[char.cols])
      stop("Correlations for characters or factors can't be calculated", call.=FALSE)
    }
  }
  p <- matrix(NA,nrow=ncol(data),ncol=ncol(data)); rownames(p) <- colnames(p) <- colnames(data)
  cors <- sign <- p
  # Calculating the correlations and the p values of the correlations
  for(i in 1:ncol(data)){
    for(j in 1:i){
      if(count) print(paste("i=",i,",   j=",j))
      if(!suppressWarnings) { ct <- cor.test(data[,i], data[,j], method=method)#, ...)
      } else { ct <- suppressWarnings(cor.test(data[,i], data[,j], method=method))}#, ...)) }
      p[i,j] <- ct$p.value
      cors[i,j] <- ct$estimate
    }
  }
  # Der Bereich ?ber der Matrix-Diagonale wird mit dem selben unterhalb der Diagonale gef?llt, weil diese Teile der Matrix gleich sind
  tp <- t(p);        p[which( row(p)-col(p) <0)] <- tp[which( row(tp)-col(tp) <0)]
  tcors <- t(cors);  cors[which( row(cors)-col(cors) <0)] <- tcors[which( row(tcors)-col(tcors) <0)]
  
  # Alte langsame Version
  #for(i in 1:ncol(data)){
  #  for(j in 1:ncol(data)){
  #    if(!suppressWarnings) { ct <- cor.test(data[,i], data[,j], method=method)#, ...)
  #    } else { ct <- suppressWarnings(cor.test(data[,i], data[,j], method=method))}#, ...)) }
  #    p[i,j] <- ct$p.value
  #    cors[i,j] <- ct$estimate
  #    if(count) print(paste("i=",i,"   ,j=",j))
  #
  #  }
  #}
  cors <- round(cors,digits)
  # setting the signs for p-levels
  if(sig.level>=0.1) {
    sign[p<0.1 & cors>=0] <- "  . "
    sign[p<0.1 & cors<0]  <- "  ."
  }
  if(sig.level>=0.05) {
    sign[p<0.05 & cors>=0] <- "  * "
    sign[p<0.05 & cors<0]  <- "  *"
  }
  if(sig.level>=0.01) {
    sign[p<0.01 & cors>=0] <- " ** "
    sign[p<0.01 & cors<0]  <- " **"
  }
  if(sig.level>=0.001) {
    sign[p<0.001 & cors>=0] <- "*** "
    sign[p<0.001 & cors<0]  <- "***"
  }
  if(!special.sign.before.nonsig.for.excel){
    sign[is.na(sign) & cors>=0] <- "    "
    sign[is.na(sign)& cors<0]   <-  "   "
  } else {
    sign[is.na(sign) & cors>=0] <- "????"
    sign[is.na(sign)& cors<0]   <-  "???"
    cat("Export the table as .csv to Excel, then replace ? by Alt+255\n")
  }
  
  # combination of the signs with the correlation values
  comb <- paste(sign,cors)#, ...)
  print.table <- matrix(comb, nrow=ncol(data), ncol=ncol(data), dimnames=list(colnames(cors),rownames(cors)))
  # diag(print.table) <- "     1"
  
  # Calculating the conficence intervals
  if(!is.null(conf.level)){
    if(method!="pearson") stop("confidence intervals can only be calculated with the pearson method")
    uintv <- matrix(NA, nrow=ncol(data), ncol=ncol(data))
    colnames(uintv) <- colnames(data); rownames(uintv) <- colnames(uintv)
    lintv <- uintv
    for(i in 1:ncol(data)){
      for(j in 1:ncol(data)){
        if(count) print(paste("i=",i,",   j=",j))
        if(!suppressWarnings) {    ct <- cor.test(data[,i], data[,j], method=method, conf.level=conf.level)  #, ...)
        } else {  ct <- suppressWarnings(cor.test(data[,i], data[,j], method=method, conf.level=conf.level))}#, ...)) }
        lintv[i,j] <- ct$conf.int[1]
        uintv[i,j] <- ct$conf.int[2]
      }
    }
    # Der Bereich ?ber der Matrix-Diagonale wird mit dem selben unterhalb der Diagonale gef?llt, weil diese Teile der Matrix gleich sind
    tlintv <- t(lintv);  lintv[which( row(lintv)-col(lintv) <0)] <- tlintv[which( row(tlintv)-col(tlintv) <0)]
    tuintv <- t(uintv);  uintv[which( row(uintv)-col(uintv) <0)] <- tuintv[which( row(tuintv)-col(tuintv) <0)]
    
    # alte langsame Version
    #for(i in 1:ncol(data)){
    #  for(j in 1:ncol(data)){
    #    if(!suppressWarnings) {    ct <- cor.test(data[,i], data[,j], method=method, conf.level=conf.level)  #, ...)
    #    } else {  ct <- suppressWarnings(cor.test(data[,i], data[,j], method=method, conf.level=conf.level))}#, ...)) }
    #    lintv[i,j] <- ct$conf.int[1]
    #    uintv[i,j] <- ct$conf.int[2]
    #  }
    #}
    
    uintv <- round(uintv,digits)
    lintv <- round(lintv,digits)
    conf.int <- NULL
    # Adding the estimate in the middle of the lower and upper interval value
    if(conf.middle) {
      # ... with *** signs
      if(conf.middle.sign) {
        uintv <- matrix(paste("     ",uintv,sep=""),ncol=ncol(data))
        lintv <- matrix(paste("     ",lintv,sep=""),ncol=ncol(data))
        for(i in 1:ncol(uintv)) conf.int <- rbind(conf.int,uintv[i,],print.table[i,],lintv[i,],rep(NA,ncol(data)))
        # ... without *** signs
      } else if (!conf.middle.sign) {
        for(i in 1:ncol(uintv)) conf.int <- rbind(conf.int,uintv[i,],cors[i,],lintv[i,],rep(NA,ncol(data)))
      }
      rnames <- NULL
      for(i in 1:ncol(uintv)) rnames <- c(rnames,rep(colnames(data)[i],3),NA)
      rownames(conf.int) <- rnames
      # Setting values NA which should not be printed
      if(ignore.nonsig) {
        p.long <- NULL
        for(i in 1:ncol(data)) p.long <- rbind(p.long,matrix(rep(p[i,],3),nrow=3,byrow=TRUE),rep(1,ncol(data)))
        conf.int[p.long>=sig.level] <- NA
      }
      if(triangle) {
        del <- NULL
        for(i in 2:ncol(data)) del <- c(del,  ( (i-1)*nrow(conf.int)+1 )  : ( (i-1)*nrow(conf.int) +(i-1)*4 )  )
        conf.int[del] <- NA
      }
      if(del.diag) {
        del <- NULL
        for(i in 1:ncol(data)) del <- c(del,  ( (i-1)*nrow(conf.int) +(i)*4 -3 )  : ( (i-1)*nrow(conf.int) +(i)*4 )  )
        conf.int[del] <- NA
      }
      # Table Intervals without the estimates in the middle
    } else if (!conf.middle) {
      rnames <- NULL
      for(i in 1:ncol(uintv)) {conf.int <- rbind(conf.int,uintv[i,],lintv[i,],NA)
      rnames <- c(rnames,rep(colnames(data)[i],2),NA) }
      rownames(conf.int) <- rnames
      
      # Setting values NA which should not be printed (in the case without estimate in the middle)
      if(ignore.nonsig) {
        p.long <- NULL
        for(i in 1:ncol(data)) p.long <- rbind(p.long,matrix(rep(p[i,],2),nrow=2,byrow=TRUE),rep(1,ncol(data)))
        conf.int[p.long>=sig.level] <- NA
      }
      if(triangle) {
        del <- NULL
        for(i in 2:ncol(data)) del <- c(del,  ( (i-1)*nrow(conf.int)+1 )  : ( (i-1)*nrow(conf.int) +(i-1)*3 )  )
        conf.int[del] <- NA
      }
      if(del.diag) {
        del <- NULL
        for(i in 1:ncol(data)) del <- c(del,  ( (i-1)*nrow(conf.int) +(i)*3 -2 )  : ( (i-1)*nrow(conf.int) +(i)*3 )  )
        conf.int[del] <- NA
      }
    }
    conf.int <- conf.int[1:(nrow(conf.int)-1),]
  }
  
  # Im Fall, dass keine P-Werte berechnet werden konnten, werden diese Pl?tze in der Matrix als NA ausgegeben
  print.table[is.na(p)|is.na(cors)] <- NA
  # Sonstige konditionale NA-Belegunten
  if(ignore.nonsig)  print.table[p>sig.level] <- NA
  if(triangle) print.table[which( row(print.table)-col(print.table) < 0)] <- NA
  if(del.diag) diag(print.table) <- NA
  
  if(is.null(conf.level)) {
    result <- list(cor=cors,p.val=p,sign=sign,print.table=print.table,conf.int=NULL)
  } else  if(!is.null(conf.level)){
    result <- list(cor=cors,p.val=p,sign=sign,print.table=print.table,conf.int=conf.int)
  }
  class(result) <- "cor.table"
  return(result)
}
print.cor.table <- function(x,quote=FALSE,na.print="", ...){
  print(x$print.table, quote=quote, na.print=na.print, ...)
}

####
plm.within.between <- function(data, index, Y, timeVariantX, timeInvariantX=NULL, timeDummies=NULL, ...){
  # This function calculates random, fixed, mundlak and within-between models for panel data.
  # see ANDREW BELL AND KELVYN JONES (2015): Explaining Fixed Effects: Random Effects Modeling of Time-Series Cross-Sectional and Panel Data. p. 141, eq. 11 & 12.
  #
  # Arguments
  # data =           The data.frame/matrix containing all relevant variables.
  # index =          The colnames of the index columns in data. length(index)==2 is required. First: individual, second: time. E.g. c("id","year")
  # Y =              Colname of the dependent (y) variable.
  # timeVariantX =   Colname(s) of the indepentent variables that vary over time within each observation.
  # timeInvariantX = Colname(s) of the indepentent variables that don't vary over time within each observation. Can be NULL.
  # timeDummies =    Colname(s) of the time dummies. Can be NULL.
  #
  # Value
  # A list with class "plm.within.between".
  # ...[["overview"]] The overview of all models in one table.
  # ...[["summary"]] The summaries for all models. List places are named: rand, fix, mund, wibe.
  # ...[["model"]] The models. List places are named: rand, fix, mund, wibe.
  # ...[["colnames"]] This information can be used to restore the original I() colnames in the model coefficients. E.g.
  #                    rownames(mod$summary$fix$coefficients) <- replace.values(mod$colnames$adapted, mod$colnames$original, rownames(mod$summary$fix$coefficients))
  #
  # For Hausman-Taylor Check also
  # Source: http://www.econ.uiuc.edu/~econ472/panel.R.txt
  # Tutorial (not useful): http://www.econ.uiuc.edu/~econ472/tutorial13.html [at bottom of page]
  # And plm::pht()
  
  # Require the plm package
  require.package(plm)
  
  # Error checks
  if(!is.data.frame(data) || is.matrix(data)) stop("data must be a data.frame/matrix. The conversion to pdata.frame is done automatically within the function.")
  if(length(Y)>1) stop("Y must be of length 1.")
  timeNotFound <- timeDummies[!timeDummies%in%colnames(data)]
  if(length(timeNotFound)>0) stop(paste0("Time dummies not found: ",paste0(timeNotFound,collapse=", ")))
  
  # Convert data if necessary
  if(!is.data.frame(data)) data <- as.data.frame(data)
  
  # Check if more than 1 year was given for all IDs
  indexNotAvail <- index[!index%in%colnames(data)]
  if(length(indexNotAvail)>0)
    stop(paste0("Some index columns are not available in the data: ", paste0(indexNotAvail,collapse=", ")))
  tabId <- table(data[,index[1]]) # The first index must be the id of the individuum
  if(all(tabId==1)) stop("For all individuums only 1 observation was given. This is not a panel.")
  
  # Check for duplicated entries
  checkVars <- c(timeVariantX, timeInvariantX, timeDummies)
  duplVars <- unique(checkVars[duplicated(checkVars)])
  if(length(duplVars)>0) stop(paste0("There must be no duplicated variables in timeVariantX, timeInvariantX or timeDummies.\n",paste0(duplVars,collapse=", ")))
  
  
  # Make inner function to create the deMean data etc.
  pre.plm.data.transformation <- function(data, timeVariantX){
    # Define names of columns to be calculated
    varName <- timeVariantX
    mVarName <- paste0("m_",varName)
    dVarName <- paste0("d_",varName)
    fobidden <- colnames(data)[colnames(data)%in%c(mVarName,dVarName)]
    if(length(fobidden)>0) stop(paste0("Data columns that are named as follows are forbidden: ", paste0(fobidden,collapse=", ") ))
    
    # Create columns in data, if specified as I()...
    allVars <- c(Y, timeVariantX, timeInvariantX)
    newVars <- allVars[!allVars%in%colnames(data)]
    data[,newVars] <- NA
    data <- calc.I.cols(data)
    alwaysNA <- sapply(data[,allVars,drop=FALSE],function(x)all(is.na(x)))
    if(any(alwaysNA)) stop(paste0("The following variables are always NA and can't be used in the model: ", paste0(names(alwaysNA[alwaysNA]),collapse=", ") ))
    
    # Create "mean" and "deMean" columns. Select time invariant value 
    data <- by.add.df.cols(data, relevantColnames=varName, INDICES=data[,index[1]], FUN=function(x){
      x[,mVarName] <- as.list(colMeans(x[,varName]))
      return(x)
    })
    data[,dVarName] <- data[,varName] - data[,mVarName]
    
    # Reformulate the I() colnames, otherwise you will yield an error later on with formula()
    idVarName <- dVarName[grepl("^d_I\\(",dVarName)]
    imVarName <- mVarName[grepl("^m_I\\(",mVarName)]
    idVarName_new <- gsub("\\(|\\)|\\-|/|\\*|\\+|\\^|,|=| ",".",idVarName)
    imVarName_new <- gsub("\\(|\\)|\\-|/|\\*|\\+|\\^|,|=| ",".",imVarName)
    niceNames <- list(original=c(idVarName,    imVarName),
                      adapted=c( idVarName_new,imVarName_new))
    dVarName <- replace.values(niceNames[["original"]], niceNames[["adapted"]], dVarName) #replace.values(idVarName, idVarName_new, dVarName)
    mVarName <- replace.values(niceNames[["original"]], niceNames[["adapted"]], mVarName) #replace.values(imVarName, imVarName_new, mVarName)
    colnames(data) <- replace.values(niceNames[["original"]], niceNames[["adapted"]], colnames(data)) #replace.values(c(idVarName,imVarName), c(idVarName_new,imVarName_new), colnames(data))
    
    # Pruefen, ob gewisse Variablen sehr wenig aendern. Falls ja, dann Warnung ausgeben.
    notVaryingVars <- sapply(data[,dVarName,drop=FALSE], function(x)length(which(x!=0)))
    notVaryingVars <- names(notVaryingVars)[ notVaryingVars/nrow(data) < 0.05 ]
    if(length(notVaryingVars)>0) {
      notVaryingVars <- replace.values(niceNames[["adapted"]], niceNames[["original"]], notVaryingVars)
      notVaryingVars <- sub("^d_","",notVaryingVars)
      warning(paste0("Some variables hardly differ within observations. You should reconsider defining them as timeVariantX variables. timeInvariantX would be more appropriate:\n",paste0(notVaryingVars,collapse=", ")))
    }
    # Return
    return(list(data=data, varName=varName, mVarName=mVarName, dVarName=dVarName, colnames=niceNames))
                #imVarName=imVarName, idVarName=idVarName, imVarName_new=imVarName_new, idVarName_new=idVarName_new))
  }
  
  d <- pre.plm.data.transformation(data=data, timeVariantX=timeVariantX)
  data <- d[["data"]]; d[["data"]] <- NULL; invisible(gc())
  
  
  # Prepare model inputs
  input <- list()
  # summaryModels will be put into the summary table ...[["summary"]] of the result. wibe* is especially for this porpuse.
  summaryModels <- c("fix","rand","mund","wibe*")
  input[["fix"]] <-   list(model="within", variables=c(timeVariantX,                                         timeDummies))
  input[["rand"]] <-  list(model="random", variables=c(timeVariantX,                         timeInvariantX, timeDummies))
  input[["mund"]] <-  list(model="random", variables=c(timeVariantX,             d$mVarName, timeInvariantX, timeDummies))
  input[["wibe"]] <-  list(model="random", variables=c(              d$dVarName, d$mVarName, timeInvariantX, timeDummies))
  input[["wibe*"]] <- list(model="random", variables=c(timeVariantX,             d$mVarName, timeInvariantX, timeDummies))
  # Info: Die Variablennamen im Datensatz werden fuer wibe* nicht mehr umbenannt, weil dies zu falschen Koeffizienten fuehrt!!!
  if(names(input)[length(input)]!="wibe*") stop("Last model must be within between. Otherwise the calculation won't be correct. Because before the last step all d$varName-columns are replaced with d$dVarName-columns!")
  wibeName <- names(input)[length(input)]
  #
  for(i in names(input)){
    input[[i]][["formula"]] <- formula(paste(Y,"~",paste0(input[[i]][["variables"]],collapse=" + ")))
  }
  # Keep only relevant columns to save RAM.
  allVars <- unique(unlist(lapply(input,function(x)x[["variables"]])))
  # d$dVarName auch dran heften. Wegen Ersetzung timeVariantX mit d$dVarName unten im loop der Models.
  allVars <- unique(c(allVars, if(FALSE) d$dVarName))
  # I() Variablen extrahieren.
  allVars <- unique(c(index, extract.I.vars(Y, keep.original=TRUE), extract.I.vars(allVars, keep.original=TRUE), timeDummies))
  # Kontrolle, ob es NA-Werte gibt.
  isna <- sapply(data[,allVars], function(x)any(!is.finite(x)))
  isna <- names(isna[isna])
  if(length(isna)>0)
    stop(paste0("Some variables contain NA/NaN/Inf values. This is not allowed. See list below.\n",paste0(isna,collapse=", ")))
  # Make pdata.frame
  data <- pdata.frame(data[,allVars], index=index)
  invisible(gc())
  if(ncol(data)!=length(allVars) || colnames(data)!=c(allVars)){
    stop(paste0("Some variables where automatically dropped from pdata.frame because they don't show any variance. Something seems wrong with your data!\n",
                "Kicked variables are:\n", paste0(allVars[!allVars%in%colnames(data)],collapse=", ")))
  }
  
  # Calculate the models
  mod1 <- vector("list",length(input)); names(mod1) <- names(input)
  for(i in names(input)){
    # In case of wibeName-model, replace the real data with the "deMean" (difference from mean) data.
    if(FALSE && i==wibeName) {
      data[,paste0(timeVariantX,"_0r19iN4L")] <- data[,timeVariantX]
      data[,timeVariantX] <- data[,d$dVarName]
    }
    # Calc model
    tryCatch({
      mod1[[i]] <- plm(formula=input[[i]][["formula"]], data=data, index=index, effect="individual", model=input[[i]][["model"]], ...)
    },error=function(e){
      print(cor.table(data[,input[[i]][["variables"]]]))
      browser()
      stop(e$message)
    })
    
    # In case of wibeName-model, reverse the data replacement.
    if(FALSE && i==wibeName){
      data[,timeVariantX] <- data[,paste0(timeVariantX,"_0r19iN4L")]
      data <- data[,!colnames(data)%in%paste0(timeVariantX,"_0r19iN4L")]
    }
    # Garbage collect
    invisible(gc())
  }
  su1 <- lapply(mod1, function(x)summary(x))
  
  # Calculate summaries and put together the results
  # ... Prepare
  allVarsFin <- c("(Intercept)", timeVariantX, d$mVarName, timeInvariantX, timeDummies)
  suRes <- as.data.frame( matrix(NA_integer_, nrow=length(allVarsFin)+3, ncol=length(summaryModels)*2) )
  rRows <- c("R-Squared","Adj. R-Squared","P-Value")
  rownames(suRes) <- c(allVarsFin,rRows)
  pCols <- (1:ncol(suRes))%%2==0
  colnames(suRes)[!pCols] <- summaryModels
  colnames(suRes)[ pCols] <- paste0(substring(summaryModels,1,1),"_P")
  
  # ... Create the summary table
  for(i in summaryModels){ # i <- names(su1)[1]
    toRows <- match(
      # replace.values() to put all _d rows into the normal rows for simpler overview.
      replace.values(d$dVarName, d$varName, rownames(su1[[i]][["coefficients"]])),
      rownames(suRes) )
    if(any(is.na(toRows))){
      stop(paste0("After formatting the I() colnames, they do not fit to the original ones. Please adapt the original ones like this:", paste0(rownames(su1[[i]][["coefficients"]])[is.na(toRows)],collapse=", ")))
    }
    # Add Rsquared an P values into the coefficients table
    toCols <- which(colnames(suRes)==i)
    suRes[rRows,toCols+1] <- unname(round(c(su1[[i]]$r.squared, su1[[i]]$fstatistic$p.value),3))
    # Add all coefficients.
    toCols <- toCols:(toCols+1)
    suRes[toRows,toCols] <- su1[[i]][["coefficients"]][,c("Estimate","Pr(>|t|)")]
  }
  okRows <- !rownames(suRes)%in%rRows
  suRes[okRows,pCols] <- lapply(suRes[okRows,pCols], function(x){
    p <- character(length(x))
    p[x<=0.1 ] <- ".";    p[x<=0.05] <- "*";    p[x<=0.01] <- "**";    p[x<=0.001] <- "***"
    return(p)
  })
  # Re-establish the original I() colnames in the summary tables.
  rownames(suRes) <- replace.values(d$colnames[["adapted"]], d$colnames[["original"]], rownames(suRes))
  su1 <- lapply(su1, function(x){
    rownames(x[["coefficients"]]) <- replace.values(d$colnames[["adapted"]], d$colnames[["original"]], rownames(x[["coefficients"]]))
    return(x)
  })
  
  # Create final result
  res <- list(overview=suRes, summary=su1, model=mod1, colnames=d$colnames, pre.plm.data.transformation.function=pre.plm.data.transformation)
  class(res) <- "plm.within.between"
  # Clean workspace
  rm(data,suRes,su1,mod1);
  invisible(gc())
  # return
  return(res)
}
print.plm.within.between <- function(x, quote=FALSE, na.print="", digits=2, signif=FALSE, ...){
  ov <- x[["overview"]]
  if(!is.null(digits)){
    roundFunc <- if(signif) match.fun("signif") else match.fun("round")
    ov[,(1:ncol(ov))%%2==1] <- apply(ov[,(1:ncol(ov))%%2==1], 2, function(x)roundFunc(x,2))
  }
  print(as.matrix(ov), quote=quote, na.print=na.print, ...)
  cat("\n* The timeVariantX_d coefficients of the 'wibe*' model specification are not shown in separate rows. However, they have been calculated differently!")
}
####
predict.plm <- function(model, newdata=NULL){
  
  if(is.matrix(newdata)) newdata <- as.data.frame(newdata) else if(!is.null(newdata) & !is.data.frame(newdata)) stop("newdata must be NULL, matrix or data.frame!")
  if(is.null(model$fitted.values)) model$fitted.values <- model$model[,1]-model$residuals
  
  if(is.null(newdata)) {
    return(model$fitted.values)
  } else {
    # Not elegant: if(is.matrix(newdata)) newdata <- as.data.frame(new.data)
    coef0 <- mean(fixef(model))
    coefs <- model$coefficients
    # Neuen Datensatz erstellen:
    if( length(coefs)!=ncol(newdata) || any(names(coefs)!=colnames(newdata)) ){
      form <- model$formula
      class(form) <- "formula"
      newdata <- model.matrix(form, newdata)[,-1,drop=FALSE] # ohne Intercept
    }
    # Calculating fitted values:
    y <- as.vector(coefs %*% t(newdata)) + coef0
    # Not elegant but does the same (much slower!)
    # y <- as.vector( apply(coefs*newdata,1,function(x)sum(x)) )
    # y <- y+coef0
    return(y)
  }
}
# Predict rlm wie plm aber mit Intercept, falls vorhanden
#predict.rlm <- predict.plm

####
# x <- data[,"Region"]; index <- data[,c("ID","Region")]
# index <- c(3,3,5,1,2); x <- c(1,1,2,3,4)
make.variable.time.invariant <- function(x, index, method=c("mostFrequent","mean","median")){
  # This function makes a variable time invariant. E.g. if an observation has mostly value 1, but sometimes 2. The value 2 is rather a measurement error than the true value.
  # In this case, the value 2 will be replaced by either 1 (method="mostFrequent"), 1.xxx (method="mean") or the median value (method="median").
  #
  # Arguments
  # x =      The vector of values that shall be checked. If a matrix/data.frame, then the function will be applied to all columns.
  # index =  The index, e.g. the id of each observation.
  # metdho = The method to be applied.
  #
  # Value
  # The corrected vector x.
  
  if(is.matrix(x)) return(apply(x,2,function(x)make.variable.time.invariant(x=x,index=index,method=method))) else
    if(is.data.frame(x)) return(as.data.frame(lapply(x,function(x)make.variable.time.invariant(x=x,index=index,method=method)),stringsAsFactors=FALSE))
  
  method <- match.arg(method)
  if(is.factor(x)) stop("The procedure does not work for factors. You have to convert into numeric/integer/character first.")
  
  index <- .paste.elements(index, sep="_a@:!;q_", errorMsg="All indices must have same length!")
  fun <- if(method=="mostFrequent") function(x) names(table(x)[1]) else if(method=="mean") mean(x) else if(method=="median") median(x)
  
  res <- tapply(x, index, fun)
  res <- res[ match(as.character(index), names(res)) ]
  
  if(is.integer(x)) return(as.integer(res)) else
    if(is.numeric(x)) return(as.numeric(res)) else
      return(res)
}

####
extract.regression.p <- function (modelobject) {
  if (class(modelobject) != "lm") stop("Not an object of class 'lm' ")
  f <- summary(modelobject)$fstatistic
  p <- pf(f[1],f[2],f[3],lower.tail=F)
  attributes(p) <- NULL
  return(p)}
####
polyregressionplot <- function(data,dependent,independent,degree=NULL,degrees=NULL,root=FALSE,roots=NULL,intercept=FALSE,sig.level=0.05,model=c("ols","robust"),datapoints=1000,na.replace=NULL,invert=FALSE,grid=FALSE,plotrows=1,maxit=1000,psi=c(psi.huber, psi.hampel, psi.bisquare)[1],indeplimit=NULL,deplimit=NULL,main=NULL,sub=NULL,indeplab=NULL,deplab=NULL,cex.main=NULL,cex.sub=NULL,cex.lab=NULL,cex.axis=NULL,linecol="black",linewidth=1.5,scatter=FALSE,dotcol="black",dotsize=0.5, ...){
  # Explanation: This function makes a regression with the dependent and independent variables. If you use degree>1, then a non-linear regression is fitted.
  # witt degrees you can specify if some variables sould have a degree of 1-max(degree). Then a polynom is made from 1-degrees.
  # Use root=TRUE if some variables should be used not with ^degree but with ^(1/degree). With roots you can specify which variables should be calculated as roos (1) and which not (0).
  # Example for 3 variables: degree=2, degrees=c(1,2,2), root=TRUE, roots=c(1,0,0). Use the minimum significance level to kick out the calculated coefficients which are below this value.
  
  #data=datas;dependent=c("DABS_fk_ArbeitDritte_index");independent=c("DABS_SAK_neu");degree=3;degrees=NULL;root=FALSE;roots=NULL;intercept=TRUE;model=c("ols","robust")[1];sig.level=1;na.replace=NULL;invert=FALSE;grid=FALSE;plotrows=2;maxit=1000;psi=c(psi.huber, psi.hampel, psi.bisquare)[1];indeplimit=NULL;deplimit=NULL;indeplab=NULL;deplab=NULL;linecol="black";linewidth=1;dotcol="black";dotsize=0.5
  #degree=3;degrees=c(0,0,1)
  model <- match.arg(model)
  if(length(dependent)>1) stop("give only 1 dependent variable")
  if(is.null(degree)) degree <- 1
  if(!is.null(sig.level)&model=="robust") warning("there are no significance levels for coefficients calculated with the robust model rlm{MASS}")
  if(class(dependent)!="character"|class(independent)!="character") stop("Dependent and independent must be characters naming the colnames of data to work with")
  if(!all(c( is.null(indeplimit)|is.list(indeplimit)), is.null(deplimit)|is.list(deplimit) )) stop("indeplimit and deplimit must be NULL or a list of length(independent) each containing 2 elements")
  if(!is.null(degrees)&length(degrees)!=length(independent)&length(degrees)!=length(independent)*degree) stop("length(degrees) must be equal length(independent) -> short form. or length(independent)*degree -> long form")
  if(!is.null(degrees)) {
    if(length(degrees)==length(independent)*degree&max(degrees)>1) stop("written in this long form degrees must only contain 0 or 1")
    if(max(degrees)>degree) stop("max(degrees) must be smaller than degree") }
  if(!is.null(roots)&length(roots)!=length(independent)) stop("length(roots) must be equal length(independent)")
  if(!is.null(roots)) if(max(roots)>1) stop("roots must only contain 0 or 1")
  
  if(is.null(degrees)) degrees <- rep(degree,length(independent))
  if(length(degrees)!=length(independent)*degree) {
    degreesv <- vector("list",length(degrees))
    for (i in 1:length(degrees)) degreesv[[i]] <- c(rep(1,degrees[i]),rep(0,degree-degrees[i]))
    degrees <- do.call("c",degreesv)
  }
  
  if(!root) {roots <- rep(0,length(independent)*degree)} # ; roots.short <- rep(0,length(independent))    ist unnoetig! es gibt nur das roots, ohne lange version.
  if(is.null(roots)&root) {roots <- rep(1,length(independent)*degree)} # ; roots.short <- rep(1,length(independent))
  if(!is.null(roots)&root){
    rootsv <- vector("list",length(degrees))
    for (i in 1:length(roots)) {
      if (roots[i]==1) rootsv[[i]] <- rep(1,degree)
      if (roots[i]==0) rootsv[[i]] <- rep(0,degree)
    }
    roots <- do.call("c",rootsv)
  }
  
  independent.multi <- vector("list", length(independent))
  for(i in 1:length(independent)) independent.multi[[i]] <- rep(independent[i],degree)
  independent.multi <- do.call("c",independent.multi)
  degreesform <- rep(1:degree,length(independent))
  
  counter <- integer(0)
  for (i in length(degrees):1) if(degrees[i]!=0) counter[i] <- i
  counter <- max(counter[!is.na(counter)])
  form <- paste(dependent, "~")
  for (i in 1:length(degrees)) {
    if (degrees[i]==1) {
      if(i!=counter) {
        if(roots[i]==0) form <- paste(form, " I( ",independent.multi[i],"^",degreesform[i]," ) +" ,sep="")
        if(roots[i]==1) form <- paste(form, " I( ",independent.multi[i],"^(1/",degreesform[i],") ) +" ,sep="")
      } else {
        if(roots[i]==0) form <- paste(form, " I( ",independent.multi[i],"^",degreesform[i]," )" , sep="", collapse="")
        if(roots[i]==1) form <- paste(form, " I( ",independent.multi[i],"^(1/",degreesform[i],") )", sep="", collapse="")
      }
    }
  }
  
  if (model=="robust") {
    require.package(MASS)
    fit <- rlm(form,data=data,maxit=maxit,psi=psi)}
  if (model=="ols")
    fit <- lm(form,data=data)
  co <- fit$coefficients
  if(!is.null(sig.level)&model=="ols")   co[summary(fit)$coefficients[,4]>sig.level] <- 0
  if(intercept) interc <- co[1] else interc <- 0
  co <- co[2:length(co)]
  co[is.na(co)] <- 0
  
  degrees2 <- degrees; co2 <- co; coefs1 <- integer(0)
  for(i in 1:length(degrees)) {
    if(degrees2[1]==0) coefs1 <- c(coefs1,0)
    if(degrees2[1]==1) {
      coefs1 <- c(coefs1,co2[1])
      if(length(co2)>1) co2 <- co2[2:length(co2)] }
    if(length(degrees2)>1) degrees2 <- degrees2[2:length(degrees2)]
  }
  co.list <- vector("list",length(independent))
  for(i in 1:length(independent)) co.list[[i]] <- coefs1[((i-1)*degree+1):(i*degree)]
  
  polys <- function(x,degree,root) {
    if(root==0) y <- x^(1:degree)
    if(root==1) y <- x^(1/(1:degree))
    return(y)
  }
  #datapoints <- 1000
  #xval <- matrix(rep(seq(mindata<-min(variable),maxdata<-max(variable),(maxdata-mindata)/datapoints),length(independent)),nrow=datapoints)
  xval <- matrix(NA,nrow=datapoints, ncol=length(independent)); colnames(xval) <- independent
  for(j in 1:length(independent)) {variable <- data[,independent[j],drop=FALSE]; maxdata <- max(variable); mindata <- min(variable); intervall <- (maxdata-mindata)/datapoints; i <- 0; while (i*intervall <= (maxdata-mindata)) {xval[i,j] <- mindata + intervall*i; i <- i + 1}}
  
  yval <- matrix(NA,nrow=nrow(xval), ncol=ncol(xval)); colnames(yval) <- colnames(xval)
  for(i in 1:ncol(xval)){
    coefs <- unname(co.list[[i]])
    yval[,i] <-
      mapply(function(x,degree,coefs,root)
        interc + sum( mapply(prod,  polys(x,degree=degree,root=roots[i]), coefs) ), # root=roots.short[i] geloescht
        xval[,i],
        MoreArgs=list(degree,coefs,root))
  }
  if(!is.null(na.replace)) yval[is.na(yval)] <- na.replace
  
  if(!is.list(indeplimit))     indeplimit <- list(indeplimit)
  if(!is.list(indeplimit))     deplimit <- list(deplimit)
  if(is.null(deplab))  name.dependent <- dependent else name.dependent <- deplab
  if(is.null(indeplab))  name.independent <- independent else name.independent <- indeplab
  if(!is.null(main)) if(ncol(xval)!=length(main)) main <- rep(main[1],ncol(xval))
  if(!is.null(sub))  if(ncol(xval)!=length(sub))  sub <- rep(sub[1],ncol(xval))
  
  if(length(independent)>1) par(mfrow=c(plotrows,if(length(independent)%%plotrows==0) length(independent)/plotrows else floor(length(independent)/plotrows)+1 ))
  
  if(!invert) for(i in 1:ncol(xval))
  {
    plot(xval[,i],yval[,i], type="n",ylab=name.dependent, xlab=name.independent[i],cex.lab=cex.lab,cex.axis=cex.axis,xlim=indeplimit[[i]],ylim=deplimit[[i]],col=linecol, ...)
    title(main=main[i],sub=sub[i],cex.main=cex.main,cex.sub=cex.sub)
    if(scatter) lines(data[,independent[i]],data[,dependent], type="p", pch=20,col=dotcol,cex=dotsize)
    lines( xval[,i], yval[,i], type="l",col=linecol,lwd=linewidth)
    if(grid) grid()
  }
  if(invert)  for(i in 1:ncol(xval))
  {
    plot(yval[,i],xval[,i],  type="n",xlab=name.dependent, ylab=name.independent[i],cex.lab=cex.lab,cex.axis=cex.axis,xlim=deplimit[[i]],ylim=indeplimit[[i]],col=linecol, ...)
    title(main=main[i],sub=sub[i],cex.main=cex.main,cex.sub=cex.sub)
    if(scatter) lines(data[,dependent],data[,independent[i]], type="p", pch=20,col=dotcol,cex=dotsize)
    lines( yval[,i],xval[,i], type="l",col=linecol,lwd=linewidth)
    if(grid) grid()
  }
  
  co[co[]==0] <- NA
  if(intercept)  co <- c(interc,co) else co <- c(NA,co)
  
  p.total <- extract.regression.p(fit)
  r.squared <- summary(fit)$r.squared
  r.adj <- summary(fit)$adj.r.squared
  significance <- c(p.overall=p.total, r.squared=r.squared, r.adjusted=r.adj)
  #if(intercept) select.pvalue <- c(TRUE,!is.na(co)) else select.pvalue <- c(FALSE,!is.na(co))
  p.values <- summary(fit)$coefficients[!is.na(co),4]
  if(length(co)>0) usedcoef <- rbind(coef=co[!is.na(co)],p.values=p.values) else usedcoef <- NA
  
  if(invert) {
    invertcoef <- usedcoef; invertcoef[1,] <-  1/usedcoef[1,]
    result <- list(xval=xval, yval=yval, lm=fit, significance=significance, usedcoef=usedcoef, invertcoef=invertcoef)
  } else  result <- list(xval=xval, yval=yval, lm=fit, significance=significance, usedcoef=usedcoef)
  class(result) <- "regplot"
  return(result)
  #f.robftest {sfsmisc}
}
print.regplot <- function(x,digits=2, ...) {
  x$xval <- NULL
  x$yval <- NULL
  class(x) <- "list"
  print(x,digits=digits, ...)
  invisible(x)
}
rlm.mod <- function(form, data, ...){
  # Calculate weights with rlm and return all things like R-squared, Sinificance etc. from lm()
  #library(MASS)
  #formula <- string.to.formula(colnames(y),colnames(x))
  #data <- xy
  #weights1 <- rlm(formula=formula,data=data, maxit=1000)$w
  #lm(formula=formula, data=data, weights=weights1)
  weights1 <- MASS::rlm(formula=form,data=data)$w
  return(lm(formula=form, data=data, weights=weights1, ...))
}


#polyregressionplot(data=aaa.all8.alle.zugeteilt,dependent="DABS_JAE_FamAK",independent=c("DABS_SAK_neu","DABS_JAE_FamAK"),degree=2, degrees=c(1,2), root=TRUE, roots=c(0,1) ,intercept=TRUE,model=c("ols","robust")[1],sig.level=0.025,na.replace=NULL,invert=FALSE,grid=FALSE,plotrows=2,maxit=1000,psi=c(psi.huber, psi.hampel, psi.bisquare)[1],xlimit=NULL,ylimit=NULL)
####
nnls.mod <- function(y,x,...){
  # This function calculates regression coefficients restricted to values greater than zero without an intercept!
  # This function was created in order to calculate the causality between costs an revenues of the SWISS FADN data.
  A <- as.matrix(x); rm(x)
  b <- as.matrix(y); rm(y)
  #if(!is.null(weights)){
  #  A <- weights * A
  #  b <- weights * b
  #}
  library(nnls)
  result <- nnls(A,b)#,...)
  class(result) <- "nnls.mod"
  names(result)[names(result)=="x"] <- "coefficients"
  names(result$coefficients) <- colnames(A)
  names(result)[names(result)=="fitted"] <- "fitted.values"
  ## Calculate Variance-Covariance Matrix
  n <- nrow(A)
  k <- ncol(A)
  VCV = 1/(n-k) * as.numeric( t(result$residuals) %*% result$residuals) * solve(t(A)%*%A)
  ## Standard errors of the estimated coefficients
  StdErr = sqrt(diag(VCV))
  ## Calculate p-value for a t-test of coefficient significance
  P.Value = 2*pt(abs( result$coefficients / StdErr ), df=n-k,lower.tail= FALSE)
  result$StdErr <- StdErr
  result$p.value <- P.Value
  result$x <- A
  result$y <- b
  #if(!is.null(weights)){
  #  result$A.weighted <- A
  #  result$b.weighted <- b
  #  result$weights <- weights
  #} else {
  #  result$A.weighted <- result$b.weighted <- result$weights <- NULL
  #}
  return(result)
}
print.nnls.mod <- function(object, digits=2, ...){
  class(object) <- "list"
  print.1 <- data.frame("Est"=object$coefficients, StdErr=object$StdErr, P.Value=object$p.value) # "Coeff"=as.factor(colnames(object$A)),
  rownames(print.1) <- colnames(object$x)
  print.1 <- round(print.1,digits)
  print(print.1)
  invisible(print.1)
}
predict.nnls.mod <- function(model, newdata=NULL){
  if(is.null(newdata)) {
    return(model$fitted.values)
  } else {
    coefs <- model$coefficients
    if(is.null(dim(newdata))) newdata <- as.data.frame(matrix(newdata,nrow=1))
    if(length(coefs)!=ncol(newdata)) stop("length(coefs) != ncol(newdata)")
    y <- as.vector(crossprod(coefs,new.data))
    return(y)
  }
}
#nnls.autoweight <- function(A,b) {
#  library(MASS)
#  weights <- rlm(A,b)$w
#  return( nnls.mod(A, b, weights=weights) )
#}
####


find.collinear.variables.det.cov <- function(indep, data, catinfo=TRUE){
  # http://stackoverflow.com/questions/3042117/screening-multicollinearity-in-a-regression-model
  # The determinant of the covariance matrix which ranges from 0 (Perfect Collinearity) to 1 (No Collinearity)
  mm <- model.matrix(formula(paste0("~ ",string.to.formula(y=NULL, x=indep))), data=data)
  mm <- mm[,-1]
  txt <- "0 = Perfect Collinearity, 1 = No Collinearity"
  if(catinfo) { cat(txt, "\n", sep=""); cat(paste0(rep("-",nchar(txt)), collapse=""), "\n", sep="") }
  return(round(det(cov(mm)), 3)) # -1 da ohne Intercept
}

find.collinear.variables.eigenv.cov <- function(indep, data, catinfo=TRUE){
  # http://stackoverflow.com/questions/3042117/screening-multicollinearity-in-a-regression-model
  # Using the fact that the determinant of a diagonal matrix is the product of the eigenvalues => The presence of one or more small eigenvalues indicates collinearity
  mm <- model.matrix(formula(paste0("~ ",string.to.formula(y=NULL, x=indep))), data=data)
  mm <- mm[,-1]
  
  txt <- "The presence of one or more small eigenvalues indicates collinearity"
  if(catinfo) { cat(txt, "\n", sep=""); cat(paste0(rep("-",nchar(txt)), collapse=""), "\n", sep="") }
  
  #eigenvalues <- eigen( t(mm) %*% mm )$values; names(eigenvalues)  <- indep;  eigenvalues <- cbind(round( eigenvalues, digits=3)) # -1 da ohne Intercept
  #txt <- "Variant 1: eigen( t(mm) %*% mm )"
  #if(catinfo) { cat(txt, "\n", sep=""); cat(paste0(rep("-",nchar(txt)), collapse=""), "\n", sep="") }
  #print(eigenvalues)
  
  eigenvalues <- eigen(cov(mm))$values; names(eigenvalues)  <- indep;
  eigenvalues <- as.matrix(round(eigenvalues, 4))
  #txt <- "\nVariant 2: eigen(cov(mm))"
  #if(catinfo) { cat(txt, "\n", sep=""); cat(paste0(rep("-",nchar(txt)), collapse=""), "\n", sep="") }
  return(eigenvalues)
}

find.collinear.variables.kappa <- function(indep, data, catinfo=TRUE){
  # http://stackoverflow.com/questions/3042117/screening-multicollinearity-in-a-regression-model
  # CN > 30 indicate severe collinearity
  mm <- model.matrix(formula(paste0("~ ",string.to.formula(y=NULL, x=indep))), data=data)
  txt <- "A Condition Number (CN) > 30 indicates severe collinearity"
  if(catinfo) { cat(txt, "\n", sep=""); cat(paste0(rep("-",nchar(txt)), collapse=""), "\n", sep="") }
  return(kappa(mm))
}

find.collinear.variables.rsq <- function(indep, data, catinfo=TRUE){
  # indep = names of independent variables to be checked
  # data = data.frame that contains the variables (i.e. environment in which the created formula should be evaluated)
  # http://stackoverflow.com/questions/3042117/screening-multicollinearity-in-a-regression-model
  # A regression of x_i on all other predictors gives R^2_i. Repeat for all predictors.
  # R^2_i close to one indicates a problem - the offending linear combination may be found.
  mm <- model.matrix(formula(paste0("~ ",string.to.formula(y=NULL, x=indep))), data=data)
  mm <- mm[,-1]
  rsq <- numeric(length(indep)); names(rsq) <- indep
  for(i in 1:ncol(mm)){
    rsq[i] <- summary( lm( string.to.formula(y=indep[i], x=indep[-i]) , data ) )$r.squared
  }
  rsq <- sort(rsq, decreasing=TRUE)
  rsq <- as.matrix(round(rsq, 4))
  txt <- "R Squared close to 1 indigates problems - the offending linear combination may be found."
  if(catinfo) { cat(txt, "\n", sep=""); cat(paste0(rep("-",nchar(txt)), collapse=""), "\n", sep="") }
  return(rsq)
}
find.collinear.variables.vif <- function(form, data, catinfo=TRUE){
  # Variance Inflation Factor (VIF)
  # http://stackoverflow.com/questions/3042117/screening-multicollinearity-in-a-regression-model
  txt <- "VIF > 10 is of concern"
  if(catinfo) { cat(txt, "\n", sep=""); cat(paste0(rep("-",nchar(txt)), collapse=""), "\n", sep="") }
  vif.val <- sort( car::vif(lm(form, data)) ,decreasing=TRUE)
  vif.val <- as.matrix(round(vif.val, 4))
  return( vif.val )
}


#### FORMULA & STRING HANDLING ####
string.to.formula <- function(y=NULL,x,intercept=TRUE,func=c("x","x^2","log(2)")[1],help.func=c("no","I","s"),return.formula=TRUE){
  # The string can be fed into a lm(formula=) or glm(formula=) function without being converted to a formula
  help.func <- match.arg(help.func)
  if(is.null(y)) return.formula <- FALSE
  if(substr(func,1,1)=="x"){
    if(help.func=="no") { form <- paste(paste(x,substr(func,2,nchar(func)),sep=""),collapse=" + ")
    } else { form <- paste(paste(help.func,"(",x,substr(func,2,nchar(func)),")",sep=""),collapse=" + ")
    }
  } else {
    func.long <- character(0)
    for(i in 1:nchar(func)) func.long[i] <- substr(func,i,i)
    func.1 <- substr(func,1,which(func.long=="x")-1)
    func.2 <- substr(func,which(func.long=="x")+1,length(func.long))
    form <- paste(paste(help.func,"(",func.1,x,func.2,")",sep=""),collapse=" + ")
  }
  if(!intercept) form <- paste(form,"-1")
  if(!is.null(y)) form <- paste(y,"~",form)
  if(return.formula) form <- formula(form, env=globalenv())
  return(form)
}


#char <- c("0123456789", "9876543210")
substr.rev <- function(char, start, end, reverse=FALSE) {
  # Reverse substring
  
  if(any(start>end)) stop("All start must be smaller equal end.")
  
  nchar_char <- nchar(char)
  res <- substr(char, nchar_char-end+1, nchar_char-start+1)
  if(reverse) { # Hinweis: Diese Version mit sapply(lapply()) ist rund 25% schneller als wenn man es mit apply() loest.
    res <- sapply(lapply(strsplit(res, NULL), function(x)rev(x)), function(x)paste(x, collapse=""))
  }
  return(res)
}
####

if(FALSE){
  find.string("a", c("A", "B", "C", "a"), TRUE)
  pattern <- "a"; x <- c("A", "B", "C", "a"); ignore.case=TRUE
  pattern <- c("5%","25%","50%","75%","95%"); x <- c("100%", "95%", "90%", "85%", "80%", "75%", "70%", "65%", "60%", "55%", "50%", "45%", "40%", "35%", "30%", "25%", "20%", "15%", "10%", "5%", "0%")
  cn(gb)[ find.string("OAF",cn(gb)) ]
  grepl("OAF",cn(gb))
}
find.string <- function(pattern, x, ignore.case=FALSE, ...){
  # This function indicates all places of a character vector that contain a certain string.
  # Note that not the whole vector place must match but only a part!
  if(length(pattern)>0) pattern <- paste0(pattern, collapse="|")
  return( grepl(pattern=pattern, x=x, ignore.case=ignore.case, ...) )
}
####

find.col <- function(pattern, dat, ignore.case=TRUE, ...){
  # This function is a convenience function to find columns in a data.frame or matrix.
  # Use it like this: find.col("jae", dat1)
  if(!is.null(dim(pattern))) {
    if(!is.null(dim(dat))) stop("pattern (first argument) must be a value or vector, but not a matrix/data.frame. dat (second) argument must be a matrix/data.frame.")
    return( colnames(pattern)[ find.string(pattern=dat, x=colnames(pattern), ignore.case=ignore.case, ...) ] )
  }
  return( colnames(dat)[ find.string(pattern=pattern, x=colnames(dat), ignore.case=ignore.case, ...) ] )
}
find.gb.col <- function(...) find.col(..., dat=gb)
find.spa.col <- function(...) find.col(..., dat=spa)
find.spe.col <- function(...) find.col(..., dat=spe)
find.cost.col <- function(...) find.col(..., dat=cost)

harmonize.agis.colnames <- function(col.names){
  # This function harmonizes the colnames of AGIS data from different years.
  # At the end of the function all colnames are shifted to upper case letters
  # dat = the name of the agis data.frame
  
  varmat <- matrix(c(
    "ART_ID","art_id"
    ,"METER_X","BUR_EXT_GKODX"
    ,"METER_Y","BUR_EXT_GKODY"
    ,"METER_X","BUR_COORD_X"
    ,"METER_Y","BUR_COORD_Y"
    ,"SAK_TOT","SAKBLW"
    ,"SAK_TOT","sakblw"
    ,"GDENR","GMDEAKT"
    ,"GDENR", "gmde"
    ,"GVE_SCHWE","GVE_SCHE"
    ,"GVE_SCHWE","GVE_SCHW"
    ,"GVE_SCHWE","gveschw"
    ,"GVE_SCHWE","GVE_SCHWE_09"
    ,"GVE_RINDE","GVE_RIND"
    ,"GVE_RINDE","GVE_RINDE_09"
    ,"GVE_RINDE","gverin"
    ,"GVE_TOT","GVE_TOT_09"
    ,"GVE_TOT","gvetot"
    ,"GVE_GEFLU","GVE_GEFLU_09"
    ,"GVE_GEFLU","gvegef"
    ,"GVE_GEFLU","GVE_GELU"
    ,"HANG18_35","hangu35"
    ,"HANGGT35","hangg35"
    ,"HANGREB30_50","hangru50"
    ,"HANGREBGT50","hangrg50"
    ,"BIO","bio"
    ,"LN","ln"
  ),byrow=TRUE,ncol=2)
  new_vars <- varmat[,1];   old_vars <- varmat[,2];
  
  for(i in 1:length(new_vars)){
    col.names[col.names%in%old_vars[i]] <- new_vars[i]
  }
  
  col.names <- toupper( col.names )
  return(col.names)
}



#### TESTS OF SIGNIFICANT DIFFERENCES ####

kruskal.multiple <- function(data, grouping, sig.level=0.05, p.adj="holm", group=TRUE,...){   #, filtered=FALSE
  require.package(agricolae)
  result <- vector("list",ncol(data))
  if(!group){
    for (i in 1:ncol(data)) {
      cat(paste("\n\n################"))
      #result[[i]] <-
      kruskal(data[,i], grouping, main=colnames(data)[i], p.adj=p.adj, group=group)
    }
  } else {
    result <- list()
    for(i in 1:ncol(data)) { result[[i]] <- kruskal.groups(data[,i],grouping,print=FALSE,...)$summary; err.count <- a}
    names(result) <- colnames(data)
  }
  return(result)
}
# kruskal.multiple(results,clustering.rep[1:2000])
####
kruskalmc.multiple <- function(data, grouping, sig.level=0.05){
  require.package(pgirmess)
  result <- vector("list",ncol(data))
  for (i in 1:ncol(data))
    result[[i]] <- kruskalmc(data[,i], grouping)
  if(!is.null(colnames(data)))
    names(result) <- colnames(data)
  return(result)
}
#test1 <- kruskalmc.all(gruppiert[,1:2], gruppiert[,"grouping"])
####
kruskal.test.multiple <- function(data,grouping,extract.p=TRUE,round.p=3){
  result <- vector("list",ncol(data))
  for(i in 1:ncol(data))
    result[[i]] <- kruskal.test(data[,i], grouping)
  if(!is.null(colnames(data))) names(result) <- colnames(data)
  if(extract.p) {
    p.value <- numeric()
    for(i in 1:length(result)){
      p.value[i] <- result[[i]]$p.value
    }
    p.value <- round(p.value,round.p)
    names(p.value) <- names(result)
    result <- p.value
  }
  return(result)
}
#k <- kruskal.test.multiple(data[,1:10],clustering.rep[1:2000])
#k1 <- kruskal.multiple(data[,1:10],clustering.rep[1:2000],print=FALSE)

####
#sig.level=0.05;p.adj="holm";digits=2;print.result=FALSE
#kruskal.groups.multiple <- function(data,grouping,sig.level=0.05,p.adj="holm",median.mean="mean",digits=2,ranked="no",print.result=FALSE, ...){
#  result <- apply(data,2,function(x)kruskal.groups(y=x, trt=grouping, sig.level=sig.level, p.adj=p.adj, median.mean=median.mean, digits=digits, ranked=ranked, print.result=print.result, ...))
#  return(result)
#}
####
#y <- krusk.in[,2]; trt<-clustering; sig.level=0.05; p.adj="holm"; digits=2; ranked="character.left"; print.result=FALSE; median.mean="mean"
kruskal.groups <- function(y,trt,sig.level=0.05,p.adj="holm",median.mean=c("mean","median","both"),digits=2,ranked=c("no","character.left","character.right","rankmean","intern"),print.result=FALSE) {
  if(length(unique(trt))>9) stop("Due to the output structure of the underlying function kruskal{agricolae} this function only works up to 9 groups!")
  if(any(is.na(trt))) stop("NA values in trt are now allowed")
  
  if(is.matrix(y)) { y <- as.data.frame(y) #return( apply(y,2,function(x)kruskal.groups(y=x, trt=trt, sig.level=sig.level, p.adj=p.adj, median.mean=median.mean, digits=digits, ranked=ranked, print.result=print.result)) )
  } else if (is.data.frame(y)) return (return( lapply(y,function(x)kruskal.groups(y=x, trt=trt, sig.level=sig.level, p.adj=p.adj, median.mean=median.mean, digits=digits, ranked=ranked, print.result=print.result)) ))
  
  char <- c("a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p")
  ranked <- match.arg(ranked)
  median.mean <- match.arg(median.mean)
  grouping <- trt
  sizes <- table(grouping); sizename <- names(sizes); sizes <- unname(sizes); names(sizes) <- sizename # for removing "grouping" from the name
  grp <- sort(unique(grouping))
  comb <- combn(grp,2);        comb.names <- apply(comb,2,function(x)paste(x,collapse="-"))
  
  if(median.mean%in%c("mean","both")) {
    mean <- round(tapply(y,grouping,mean,na.rm=TRUE),digits=digits)
    sd <-   round(tapply(y,grouping,sd,na.rm=TRUE),digits=digits)
  }
  if(median.mean%in%c("median","both")) {
    median <- round(tapply(y,grouping,median,na.rm=TRUE),digits=digits)
    quantiles <- round(do.call("rbind",tapply(y,grouping,function(x)quantile(x,c(0.25,0.5,0.75),na.rm=TRUE))),digits=digits); colnames(quantiles) <- c("25%","50%","75%")
  }
  
  krsk <- .kruskal2.for.kruskal.groups(y,grouping,group=FALSE,p.adj=p.adj)
  diffs <- krsk[[1]][,1] # Extracting the needed information out of the kruskal()-Output
  sig <- krsk[[1]][,2]   # Extracting the needed information out of the kruskal()-Output
  sig.true <- sig < sig.level
  
  if( kruskal.test(y,trt)$p.value>=sig.level   |   all(!sig.true) ){
    # Wenn keine signifikanten Unterschiede bestehen, nur "a" ausgeben.
    # Conover 1999: If, and only if, the null hypothesis [of the normal Kruskal-Wallis-Test] is rejected, we may use the following procedure to determine which pairs of populations tend to differ. (Null hypothesis of the normal Kruskal-Wallis Test: All of the length(grp) population distribution functions are identical)
    mat.short <- mat <- matrix("a",nrow=length(grp),ncol=1); rownames(mat.short) <- rownames(mat) <- names(mean)
    if(median.mean=="both") out <- data.frame(median=median,mean=mean,sd=sd,group=mat.short) else if (median.mean=="mean") out <- data.frame(mean=mean,sd=sd,group=mat.short) else if (median.mean=="median") { out <- data.frame(quantiles,group=mat.short); colnames(out) <- c(colnames(quantiles),"group") }
    ranking.rankmean <- as.numeric(as.character(krsk[[2]][order(-krsk[[2]][,2]),1]))
    ranking.intern <- ranking.character.left <- ranking.character.right <- ranking.rankmean
    rank.mat <- cbind(ranking.character.left,ranking.character.right,ranking.rankmean,ranking.intern); colnames(rank.mat) <- c("character.left","character.right","rankmean","intern")
    if(ranked!="no") {
      out <- out[rank.mat[,ranked],]
      mat <- mat[rank.mat[,ranked],,drop=FALSE]
      mat.short <- mat.short[rank.mat[,ranked],,drop=FALSE]
      krsk[[2]] <- krsk[[2]][rank.mat[,ranked],]
    }
    result <- list(summary=out, groups=mat.short, groups.long=mat, kruskal=krsk, sizes=sizes, ranking.rankmean=ranking.rankmean, ranking.character.left=ranking.character.left, ranking.character.right=ranking.character.right, ranking.intern=ranking.intern)
    class(result) <- "kruskal.groups"
    if(print.result) print(result)
    return(result)
  }
  
  sig.value <- numeric()  # Converting the significance information (TRUE / FALSE) into (-1 & 1 / 0) for (TRUE greater & TRUE saller / FALSE)
  for(i in 1:length(sig)){
    if(!sig.true[i]) sig.value[i] <- 0 else if(sig.true[i] & diffs[i]<0) sig.value[i] <- -1 else if(sig.true[i] & diffs[i]>0) sig.value[i] <- 1
  }
  # Signifikanzmatrizen erstellen (1/0/-1)
  sig.value.mat <- matrix(NA,nrow=length(grp),ncol=length(grp))
  for(i in 1:length(sig.true))    sig.value.mat[comb[1,i],comb[2,i]] <- sig.value[i]
  for(i in 1:ncol(sig.value.mat)) sig.value.mat[,i] <- -sig.value.mat[i,]
  diag(sig.value.mat) <- 0
  
  rank.numbers.pos <- apply(sig.value.mat,1,function(x)sum(x[x>0]))
  rank.numbers.neg <- apply(sig.value.mat,1,function(x)sum(x[x<0]))
  ranking <- order(-rank.numbers.pos, rank.numbers.neg, -krsk[[2]][,2])
  
  # Schauen, wie viel mal man die n?chste Schleife (unter mat <-...) laufen lassen muss
  comb2 <- comb[,sig.true,drop=FALSE]
  s <- 1
  comb2.new <- comb2
  while(dim(comb2.new)[2]>0) {
    i.is.in <- apply(comb2.new==ranking[s],2,function(x)any(x))
    comb2.new <- comb2.new[,!i.is.in,drop=FALSE]
    s <- s+1
  }
  
  # THIS IS THE HEART OF THE FUNCTION (makes the grouping a, ab, a, ...)
  # *********************************
  mat <- matrix(NA,nrow=length(grp),ncol=s)
  count.auslassen <- 0
  for (i in 1:s){
    # Falls die positiven signifikanten Unterschiede f?r eine Gruppe genau dieselbe sind, wie f?r die letzte, dann wird der j-Loop ?bersprungen
    # Falls in der folge nur noch negative signifikante Unterschiede kommen, werden diese ?berspr?ngen, da diese ja schon bei den positiven Signifikanzen ber?cksichtigt wurden. Dann l?uft der Loop leer durch, bis er fertig ist.
    # Zur Kontrolle:
    # test <- sig.value.mat[ranking,]
    # rownames(test) <- ranking;test
    if
    ( i==1 |
      ( !all( (sig.value.mat[ranking[i],] == sig.value.mat[ranking[i-1],])[ sig.value.mat[ranking[i],]>=0 & sig.value.mat[ranking[i-1],]>=0 ] )
        &  !all(  sig.value.mat[ranking[(i-1):nrow(sig.value.mat)],] <= 0 )
      )
    )
    {
      looki <- apply(comb==ranking[i],2,function(x)any(x))
      for(j in 1:length(grp)){
        lookj <- apply(comb==grp[j],2,function(x)any(x))
        # Es wird dasjenige Element ausgew?hlt, welches der Kombination von i mit j entspricht.
        look <- looki & lookj
        ok <- look & sig.true
        # Wenn die gew?hlte Kombination signifikant ist, wird ein " " gesetzt, sonst der Buchstabe
        if(any(ok)) mat[j,i-count.auslassen] <- " " else mat[j,i-count.auslassen] <- char[i-count.auslassen]
        # Zur Kontrolle:
        # cat("###\ni= ",i,"\ngrouping= ",ranking[i],"\n")
        # cat("j= ",j,"\n")
        # print(mat)
      }
      # In der Spalte, in der man gerade ist, muss die Reihe der Gepr?ften Gruppe immer ein Buchstabe enthalten!
      mat[ranking[i],i-count.auslassen] <- char[i-count.auslassen]
      # Die Buchstaben werden nur nach rechts gesetzt, nicht aber nach links zu den Gruppen, die schon kontrolliert wurden. Die kontrollierten Gruppen werden mit einem Leerfeld belegt.
      if(i>1) mat[ranking[1:(i-1)],i-count.auslassen] <- " "
    } else { # Falls die Signifikanten unterschiede f?r eine Gruppe genau dieselben sind, wie f?r die letzte, dann wird der j-Loop ?bersprungen. Siehe if() oben.
      count.auslassen <- count.auslassen + 1
    }
    # Zur Kontrolle
    # print(mat)
    # names(sig.value) <- comb.names; print(sig.value)
  }
  
  mat <- mat[,apply(mat,2,function(x)!all(is.na(x)))] # Remove empty columns from table
  mat.short <- matrix(NA,ncol=1,nrow=nrow(mat))
  rownames(mat.short) <- rownames(mat) <- names(mean)
  for(i in 1:nrow(mat)){
    mat.short[i,] <- paste(mat[i,],sep="",collapse="")
  }
  if(median.mean=="both") out <- data.frame(median=median,mean=mean,sd=sd,group=mat.short) else if (median.mean=="mean") out <- data.frame(mean=mean,sd=sd,group=mat.short) else if (median.mean=="median") { out <- data.frame(quantiles,group=mat.short); colnames(out) <- c(colnames(quantiles),"group") }
  ranking.rankmean <- as.numeric(as.character(krsk[[2]][order(-krsk[[2]][,2]),1]))
  ranking.character.left  <- order(apply(mat,1,function(x)min(which(x!=" "))) , apply(mat,1,function(x)max(which(x!=" "))) , -rank.numbers.pos, rank.numbers.neg, -krsk[[2]][,2])
  ranking.character.right <- order(apply(mat,1,function(x)max(which(x!=" "))) , apply(mat,1,function(x)min(which(x!=" "))) , -rank.numbers.pos, rank.numbers.neg, -krsk[[2]][,2])
  ranking.intern <- ranking
  rank.mat <- cbind(ranking.character.left,ranking.character.right,ranking.rankmean,ranking.intern); colnames(rank.mat) <- c("character.left","character.right","rankmean","intern")
  if(ranked!="no") {
    out <- out[rank.mat[,ranked],]
    mat <- mat[rank.mat[,ranked],,drop=FALSE]
    mat.short <- mat.short[rank.mat[,ranked],,drop=FALSE]
    krsk[[2]] <- krsk[[2]][rank.mat[,ranked],]
  }
  result <- list(summary=out,groups=mat.short,groups.long=mat,kruskal=krsk, sizes=sizes, ranking.rankmean=ranking.rankmean, ranking.character.left=ranking.character.left, ranking.character.right=ranking.character.right, ranking.intern=ranking.intern)
  class(result) <- "kruskal.groups"
  if(print.result) print(result)
  return(result)
}
print.kruskal.groups <- function(x,...){
  a <- vector("list")
  a$summary <- x$summary
  print(a,...)
}
####

#y <- rnorm(100,10,5); trt <- round(runif(100,1,10))
#x <- .kruskal2.for.kruskal.groups(y,trt); x
####
.kruskal2.for.kruskal.groups <- function(y, trt,alpha=0.05,p.adj = c("none","holm", "hochberg", "bonferroni", "BH", "BY", "fdr"),group=FALSE,main=NULL) {
  require.package(agricolae)
  name.y <- paste(deparse(substitute(y)))
  name.t <- paste(deparse(substitute(trt)))
  p.adj <- match.arg(p.adj)
  junto <- subset(data.frame(y, trt), is.na(y) == FALSE)
  N<- nrow(junto)
  junto[, 1] <- rank(junto[, 1])
  means <- tapply.stat(junto[,1],junto[,2],stat="sum")  #change
  sds <-   tapply.stat(junto[,1],junto[,2], stat="sd")  #change
  nn <-   tapply.stat(junto[,1],junto[,2],stat="length") #change
  means<-data.frame(means,replication=nn[,2])
  names(means)[1:2]<-c(name.t,name.y)
  # row.names(means)<-means[,1]
  ntr<-nrow(means)
  nk <- choose(ntr, 2)
  DFerror<-N - ntr
  rs<- 0
  U <- 0
  for (i in 1:ntr) {
    rs <- rs + means[i, 2]^2/means[i, 3]
    U <- U + 1/means[i, 3]
  }
  S <- (sum(junto[, 1]^2) - (N * (N + 1)^2)/4)/(N - 1)
  H <- (rs - (N * (N + 1)^2)/4)/S
  #cat("\nStudy:",main)
  #cat("\nKruskal-Wallis test's\nTies or no Ties\n")
  #cat("\nValue:", H)
  #cat("\ndegrees of freedom:", ntr - 1)
  p.chisq <- 1 - pchisq(H, ntr - 1)
  #cat("\nPvalue chisq  :", p.chisq,"\n\n")
  DFerror <- N - ntr
  Tprob <- qt(1 - alpha/2, DFerror)
  MSerror <- S * ((N - 1 - H)/(N - ntr))
  #cat("\nComparison of treatments")
  #...............
  means[,2]<- means[, 2]/means[, 3]
  #cat(paste(name.t,",",sep="")," means of the ranks\n\n")
  #print(data.frame(row.names = means[,1], means[,-1]))
  if (p.adj != "none")
  {
    #cat("\nP value adjustment method:", p.adj)
    a <- 1e-06
    b <- 1
    for (i in 1:100) {
      x <- (b + a)/2
      xr <- rep(x,nk)
      d <- p.adjust(xr, p.adj)[1] - alpha
      ar <- rep(a,nk)
      fa <- p.adjust(ar, p.adj)[1] - alpha
      if (d * fa < 0)
        b <- x
      if (d * fa > 0)
        a <- x
    }
    Tprob <- qt(1 - x/2, DFerror)
  }
  nr <- unique(means[,3])
  if (group) {
    Tprob<-qt(1-alpha/2,DFerror)
    #cat("\nt-Student:", Tprob)
    #cat("\nAlpha    :",alpha)
    if (length(nr) == 1) {
      LSD <- Tprob * sqrt(2 * MSerror/nr)
      #cat("\nLSD      :", LSD,"\n")
    }
    else {
      nr1 <- 1/mean(1/nn[, 2])
      LSD1 <- Tprob * sqrt(2 * MSerror/nr1)
      #cat("\nLSD      :", LSD1,"\n")
      #cat("\nHarmonic Mean of Cell Sizes ", nr1)
    }
    #cat("\nMeans with the same letter are not significantly different\n")
    #cat("\nGroups, Treatments and mean of the ranks\n")
    output <- order.group(means[,1], means[,2], means[,3], MSerror, Tprob,std.err=sqrt(MSerror/ means[,3]))
  }
  if (!group) {
    comb <-combn(ntr,2)
    nn<-ncol(comb)
    dif<-rep(0,nn)
    LCL<-dif
    UCL<-dif
    pvalue<-dif
    sdtdif <- dif
    for (k in 1:nn) {
      i<-comb[1,k]
      j<-comb[2,k]
      #if (means[i, 2] < means[j, 2]){
      #comb[1, k]<-j
      #comb[2, k]<-i
      #}
      dif[k]<-means[i,2]-means[j,2]
      sdtdif[k]<- sqrt(S*((N-1-H)/(N-ntr))*(1/means[i,3]+1/means[j,3]))
      pvalue[k]<- 2*round(1-pt(abs(dif[k])/sdtdif[k],DFerror),6)
    }
    if (p.adj != "none")pvalue <- round(p.adjust(pvalue, p.adj),6)
    LCL <- dif - Tprob*sdtdif
    UCL <- dif + Tprob*sdtdif
    sig<-rep(" ",nn)
    for (k in 1:nn) {
      if (pvalue[k] <= 0.001) sig[k]<-"***"
      else  if (pvalue[k] <= 0.01) sig[k]<-"**"
      else  if (pvalue[k] <= 0.05) sig[k]<-"*"
      else  if (pvalue[k] <= 0.1) sig[k]<-"."
    }
    tr.i <- means[comb[1, ],1]
    tr.j <- means[comb[2, ],1]
    output<-data.frame("Difference" = dif, pvalue=pvalue,sig,LCL,UCL)
    rownames(output)<-paste(tr.i,tr.j,sep=" - ")
    #cat("\nComparison between treatments mean of the ranks\n\n")
    #print(output)
    
    output2<-data.frame(trt= means[,1],means= means[,2],M="",N=means[,3])
    #output  <-data.frame(trt= means[,1],means= means[,2],M="",N=means[,3])
  }
  invisible(list(output,output2))
  #     invisible(output)
}

###

#sig.groups( y=c(0,1,0,0,0,1,0,1,1,0,rep(3,10),rep(4,10)), trt=sample(c(0,1,2),30, replace=TRUE), sig=c(0.02,0.04,0.05), sig.level=0.05, digits=2, median.mean=c("mean","median","both")[2], ranked=c("no","character.left","character.right","mean","intern"), print.result=TRUE )

sig.groups <- function(y, trt, sig, sig.level=0.05, digits=2, median.mean=c("mean","median","both"), ranked=c("no","character.left","character.right","mean","intern"), print.result=TRUE) {
  if(length(y)!=length(trt)) stop("length(y)!=length(trt)")
  ranked <- match.arg(ranked)
  median.mean <- match.arg(median.mean)
  
  char <- c("a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p")
  grouping <- trt
  sizes <- table(grouping); sizename <- names(sizes); sizes <- unname(sizes); names(sizes) <- sizename # for removing "grouping" from the name
  grp <- sort(unique(grouping))
  comb <- combn(grp,2);        comb.names <- apply(comb,2,function(x)paste(x,collapse="-"))
  #mean <- round(tapply(y,grouping,mean,na.rm=TRUE),digits=digits)
  
  if(median.mean%in%c("mean","both")) {
    mean <- round(tapply(y,grouping,mean,na.rm=TRUE),digits=digits)
    sd <-   round(tapply(y,grouping,sd,na.rm=TRUE),digits=digits)
  }
  if(median.mean%in%c("median","both")) {
    median <- round(tapply(y,grouping,median,na.rm=TRUE),digits=digits)
    quantiles <- round(do.call("rbind",tapply(y,grouping,function(x)quantile(x,c(0.25,0.5,0.75),na.rm=TRUE))),digits=digits); colnames(quantiles) <- c("25%","50%","75%")
  }
  
  diffs <- rep(NA,ncol(comb)); names(diffs) <- comb.names
  for(i in 1:ncol(comb)) diffs[i] <- mean[comb[1,i]] - mean[comb[2,i]]
  sig.true <- rep(FALSE,length(sig));
  sig.true <- sig < sig.level; names(sig.true) <- comb.names
  
  
  if(all(!sig.true)){
    # Wenn keine signifikanten Unterschiede bestehen, nur "a" ausgeben.
    mat.short <- mat <- matrix("a",nrow=length(grp),ncol=1); rownames(mat.short) <- rownames(mat) <- names(mean)
    if(median.mean=="both") out <- data.frame(median=median,mean=mean,sd=sd,group=mat.short) else if (median.mean=="mean") out <- data.frame(mean=mean,sd=sd,group=mat.short) else if (median.mean=="median") { out <- data.frame(quantiles,group=mat.short); colnames(out) <- c(colnames(quantiles),"group") }
    ranking.rankmean <- as.numeric(as.character(names(mean)[order(-mean)]))
    ranking.intern <- ranking.character.left <- ranking.character.right <- ranking.rankmean
    rank.mat <- cbind(ranking.character.left,ranking.character.right,ranking.rankmean,ranking.intern); colnames(rank.mat) <- c("character.left","character.right","mean","intern")
    if(ranked!="no") {
      out <- out[rank.mat[,ranked],]
      mat <- mat[rank.mat[,ranked],,drop=FALSE]
      mat.short <- mat.short[rank.mat[,ranked],,drop=FALSE]
    }
    result <- list(summary=out, groups=mat.short, groups.long=mat, p.value=sig, sizes=sizes, ranking.rankmean=ranking.rankmean, ranking.character.left=ranking.character.left, ranking.character.right=ranking.character.right, ranking.intern=ranking.intern)
    class(result) <- "sig.groups"
    if(print.result) print(result)
    return(invisible(result))
  }
  
  sig.value <- numeric()  # Converting the significance information (TRUE / FALSE) into (-1 & 1 / 0) for (TRUE greater & TRUE saller / FALSE)
  for(i in 1:length(sig)){
    if(!sig.true[i]) sig.value[i] <- 0 else if(sig.true[i] & diffs[i]<0) sig.value[i] <- -1 else if(sig.true[i] & diffs[i]>0) sig.value[i] <- 1
  }
  # Signifikanzmatrizen erstellen (1/0/-1)
  sig.value.mat <- matrix(NA,nrow=length(grp),ncol=length(grp))
  for(i in 1:length(sig.true))    sig.value.mat[comb[1,i],comb[2,i]] <- sig.value[i]
  for(i in 1:ncol(sig.value.mat)) sig.value.mat[,i] <- -sig.value.mat[i,]
  diag(sig.value.mat) <- 0
  
  rank.numbers.pos <- apply(sig.value.mat,1,function(x)sum(x[x>0]))
  rank.numbers.neg <- apply(sig.value.mat,1,function(x)sum(x[x<0]))
  ranking <- order(-rank.numbers.pos, rank.numbers.neg, -mean)
  
  # Schauen, wie viel mal man die n?chste Schleife (unter mat <-...) laufen lassen muss
  comb2 <- comb[,sig.true,drop=FALSE]
  s <- 1
  comb2.new <- comb2
  while(dim(comb2.new)[2]>0) {
    i.is.in <- apply(comb2.new==ranking[s],2,function(x)any(x))
    comb2.new <- comb2.new[,!i.is.in,drop=FALSE]
    s <- s+1
  }
  
  # THIS IS THE HEART OF THE FUNCTION (makes the grouping a, ab, a, ...)
  # *********************************
  mat <- matrix(NA,nrow=length(grp),ncol=s)
  count.auslassen <- 0
  for (i in 1:s){
    # Falls die positiven signifikanten Unterschiede f?r eine Gruppe genau dieselbe sind, wie f?r die letzte, dann wird der j-Loop ?bersprungen
    # Falls in der folge nur noch negative signifikante Unterschiede kommen, werden diese ?berspr?ngen, da diese ja schon bei den positiven Signifikanzen ber?cksichtigt wurden. Dann l?uft der Loop leer durch, bis er fertig ist.
    # Zur Kontrolle:
    # test <- sig.value.mat[ranking,]
    # rownames(test) <- ranking;test
    if
    ( i==1 |
      ( !all( (sig.value.mat[ranking[i],] == sig.value.mat[ranking[i-1],])[ sig.value.mat[ranking[i],]>=0 & sig.value.mat[ranking[i-1],]>=0 ] )
        &  !all(  sig.value.mat[ranking[(i-1):nrow(sig.value.mat)],] <= 0 )
      )
    )
    {
      looki <- apply(comb==ranking[i],2,function(x)any(x))
      for(j in 1:length(grp)){
        lookj <- apply(comb==grp[j],2,function(x)any(x))
        # Es wird dasjenige Element ausgew?hlt, welches der Kombination von i mit j entspricht.
        look <- looki & lookj
        ok <- look & sig.true
        # Wenn die gew?hlte Kombination signifikant ist, wird ein " " gesetzt, sonst der Buchstabe
        if(any(ok)) mat[j,i-count.auslassen] <- " " else mat[j,i-count.auslassen] <- char[i-count.auslassen]
        # Zur Kontrolle:
        # cat("###\ni= ",i,"\ngrouping= ",ranking[i],"\n")
        # cat("j= ",j,"\n")
        # print(mat)
      }
      # In der Spalte, in der man gerade ist, muss die Reihe der Gepr?ften Gruppe immer ein Buchstabe enthalten!
      mat[ranking[i],i-count.auslassen] <- char[i-count.auslassen]
      # Die Buchstaben werden nur nach rechts gesetzt, nicht aber nach links zu den Gruppen, die schon kontrolliert wurden. Die kontrollierten Gruppen werden mit einem Leerfeld belegt.
      if(i>1) mat[ranking[1:(i-1)],i-count.auslassen] <- " "
    } else { # Falls die Signifikanten unterschiede f?r eine Gruppe genau dieselben sind, wie f?r die letzte, dann wird der j-Loop ?bersprungen. Siehe if() oben.
      count.auslassen <- count.auslassen + 1
    }
    # Zur Kontrolle
    # print(mat)
    # names(sig.value) <- comb.names; print(sig.value)
  }
  
  mat <- mat[,apply(mat,2,function(x)!all(is.na(x)))] # Remove empty columns from table
  mat.short <- matrix(NA,ncol=1,nrow=nrow(mat))
  rownames(mat.short) <- rownames(mat) <- names(mean)
  for(i in 1:nrow(mat)){
    mat.short[i,] <- paste(mat[i,],sep="",collapse="")
  }
  if(median.mean=="both") out <- data.frame(median=median,mean=mean,sd=sd,group=mat.short) else if (median.mean=="mean") out <- data.frame(mean=mean,sd=sd,group=mat.short) else if (median.mean=="median") { out <- data.frame(quantiles,group=mat.short); colnames(out) <- c(colnames(quantiles),"group") }
  ranking.rankmean <- as.numeric(as.character(names(mean)[order(-mean)]))
  ranking.character.left  <- order(apply(mat,1,function(x)min(which(x!=" "))) , apply(mat,1,function(x)max(which(x!=" "))) , -rank.numbers.pos, rank.numbers.neg, -mean)
  ranking.character.right <- order(apply(mat,1,function(x)max(which(x!=" "))) , apply(mat,1,function(x)min(which(x!=" "))) , -rank.numbers.pos, rank.numbers.neg, -mean)
  ranking.intern <- ranking
  rank.mat <- cbind(ranking.character.left,ranking.character.right,ranking.rankmean,ranking.intern); colnames(rank.mat) <- c("character.left","character.right","mean","intern")
  if(ranked!="no") {
    out <- out[rank.mat[,ranked],]
    mat <- mat[rank.mat[,ranked],,drop=FALSE]
    mat.short <- mat.short[rank.mat[,ranked],,drop=FALSE]
  }
  result <- list(summary=out, groups=mat.short, groups.long=mat, p.value=sig, sizes=sizes, ranking.rankmean=ranking.rankmean, ranking.character.left=ranking.character.left, ranking.character.right=ranking.character.right, ranking.intern=ranking.intern)
  class(result) <- "sig.groups"
  if(print.result) print(result)
  invisible(result)
}
print.sig.groups <- function(x,...){
  a <- vector("list")
  a$summary <- x$summary
  print(a,...)
}


####
sig.groups.by.interval <- function(diffs=NULL,sig.true=NULL,comb=NULL,special.input=NULL,input.type=c("normal","malm.ci"),digits=2,ranked=c("no","character.left","character.right","intern"),print.result=TRUE) {
  # For intervals like the malmquist output give the following arguments:
  #malmi <- matrix(c(1,1,1.5,3,2,2,1,4),nrow=2,byrow=TRUE)
  #comb <- combn(1:ncol(malmi),2);  comb.names <- apply(comb,2,function(x)paste(x,collapse="-"))
  #diffs <- numeric();
  #for(i in 1:ncol(comb)) diffs[i] <- max(malmi[,comb[1,i]]) - min(malmi[,comb[2,i]])
  #names(diffs) <- comb.names
  #sig.true <- diffs<0
  ranked <- match.arg(ranked)
  input.type <- match.arg(input.type)
  if(input.type=="malm.ci"){
    if(is.null(special.input)) stop("Please enter groupwise mean values of confidence intervals (value of dea.malm(...,nrep>1))")
    comb <- combn(1:ncol(special.input),2)
    diffs <- numeric()
    for(i in 1:ncol(comb)) diffs[i] <- max(special.input[,comb[1,i]]) - min(special.input[,comb[2,i]])
    sig.true <- diffs<0
  } else if (any(c( is.null(diffs),is.null(sig.true),is.null(comb) ))) stop("specify arguments diffs, sig.true and comb")
  
  char <- c("a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p")
  grp <- unique(sort(comb))
  if( !all(combn(grp,2)==comb) ) stop("Combine the differences with combn(  unique(grouplabels)  ,2)")
  
  if(all(!sig.true)){
    # Wenn keine signifikanten Unterschiede bestehen, nur "a" ausgeben.
    mat.short <- mat <- matrix("a",nrow=length(grp),ncol=1); rownames(mat.short) <- rownames(mat) <- names(mean)
    rownames(mat) <- rownames(mat.short) <- grp
    ranking.intern <- ranking.character.left <- ranking.character.right <- grp
    rank.mat <- cbind(ranking.character.left,ranking.character.right,ranking.intern); colnames(rank.mat) <- c("character.left","character.right","intern")
    if(ranked!="no") {
      mat <- mat[rank.mat[,ranked],,drop=FALSE]
      mat.short <- mat.short[rank.mat[,ranked],,drop=FALSE]
    }
    result <- list(groups=mat.short, groups.long=mat)
    class(result) <- "sig.groups.by.interval"
    if(print.result) print(result)
    return(invisible(result))
  }
  
  sig.value <- numeric()
  for(i in 1:length(sig.true)){
    if(!sig.true[i]) sig.value[i] <- 0 else if(sig.true[i] & diffs[i]<0) sig.value[i] <- -1 else if(sig.true[i] & diffs[i]>0) sig.value[i] <- 1
  }
  # Signifikanzmatrizen erstellen (1/0/-1)
  sig.value.mat <- matrix(NA,nrow=length(grp),ncol=length(grp))
  for(i in 1:length(sig.true))    sig.value.mat[comb[1,i],comb[2,i]] <- sig.value[i]
  for(i in 1:ncol(sig.value.mat)) sig.value.mat[,i] <- -sig.value.mat[i,]
  diag(sig.value.mat) <- 0
  
  rank.numbers.pos <- apply(sig.value.mat,1,function(x)sum(x[x>0]))
  rank.numbers.neg <- apply(sig.value.mat,1,function(x)sum(x[x<0]))
  ranking <- order(-rank.numbers.pos, rank.numbers.neg)
  
  # Schauen, wie viel mal man die n?chste Schleife (unter mat <-...) laufen lassen muss
  comb2 <- comb[,sig.true,drop=FALSE]
  s <- 1
  comb2.new <- comb2
  while(dim(comb2.new)[2]>0) {
    i.is.in <- apply(comb2.new==ranking[s],2,function(x)any(x))
    comb2.new <- comb2.new[,!i.is.in,drop=FALSE]
    s <- s+1
  }
  
  mat <- matrix(NA,nrow=length(grp),ncol=s)
  count.auslassen <- 0
  for (i in 1:s){
    # Falls die positiven signifikanten Unterschiede f?r eine Gruppe genau dieselbe sind, wie f?r die letzte, dann wird der j-Loop ?bersprungen
    # Falls in der folge nur noch negative signifikante Unterschiede kommen, werden diese ?berspr?ngen, da diese ja schon bei den positiven Signifikanzen ber?cksichtigt wurden. Dann l?uft der Loop leer durch, bis er fertig ist.
    # Zur Kontrolle:
    # test <- sig.value.mat[ranking,]
    # rownames(test) <- ranking;test
    if
    ( i==1 |
      ( !all( (sig.value.mat[ranking[i],] == sig.value.mat[ranking[i-1],])[ sig.value.mat[ranking[i],]>=0 & sig.value.mat[ranking[i-1],]>=0 ] )
        &  !all(  sig.value.mat[ranking[(i-1):nrow(sig.value.mat)],] <= 0 )
      )
    )
    {
      looki <- apply(comb==ranking[i],2,function(x)any(x))
      for(j in 1:length(grp)){
        lookj <- apply(comb==grp[j],2,function(x)any(x))
        # Es wird dasjenige Element ausgew?hlt, welches der Kombination von i mit j entspricht.
        look <- looki & lookj
        ok <- look & sig.true
        # Wenn die gew?hlte Kombination signifikant ist, wird ein " " gesetzt, sonst der Buchstabe
        if(any(ok)) mat[j,i-count.auslassen] <- " " else mat[j,i-count.auslassen] <- char[i-count.auslassen]
        # Zur Kontrolle:
        # cat("###\ni= ",i,"\ngrouping= ",ranking[i],"\n")
        # cat("j= ",j,"\n")
        # print(mat)
      }
      # In der Spalte, in der man gerade ist, muss die Reihe der Gepr?ften Gruppe immer ein Buchstabe enthalten!
      mat[ranking[i],i-count.auslassen] <- char[i-count.auslassen]
      # Die Buchstaben werden nur nach rechts gesetzt, nicht aber nach links zu den Gruppen, die schon kontrolliert wurden. Die kontrollierten Gruppen werden mit einem Leerfeld belegt.
      if(i>1) mat[ranking[1:(i-1)],i-count.auslassen] <- " "
    } else { # Falls die Signifikanten unterschiede f?r eine Gruppe genau dieselben sind, wie f?r die letzte, dann wird der j-Loop ?bersprungen. Siehe if() oben.
      count.auslassen <- count.auslassen + 1
    }
    # Zur Kontrolle
    # print(mat)
  }
  
  mat <- mat[,apply(mat,2,function(x)!all(is.na(x)))]
  mat.short <- matrix(NA,ncol=1,nrow=nrow(mat))
  rownames(mat.short) <- rownames(mat) <- names(mean)
  for(i in 1:nrow(mat)){
    mat.short[i,] <- paste(mat[i,],sep="",collapse="")
  }
  rownames(mat) <- rownames(mat.short) <- grp
  ranking.character.left  <- order(apply(mat,1,function(x)min(which(x!=" "))) , apply(mat,1,function(x)max(which(x!=" "))) , -rank.numbers.pos, rank.numbers.neg)
  ranking.character.right <- order(apply(mat,1,function(x)max(which(x!=" "))) , apply(mat,1,function(x)min(which(x!=" "))) , -rank.numbers.pos, rank.numbers.neg)
  ranking.intern <- ranking
  rank.mat <- cbind(ranking.character.left,ranking.character.right,ranking.intern); colnames(rank.mat) <- c("character.left","character.right","intern")
  if(ranked!="no") {
    mat <- mat[rank.mat[,ranked],,drop=FALSE]
    mat.short <- mat.short[rank.mat[,ranked],,drop=FALSE]
  }
  result <- list(groups=mat.short, groups.long=mat)
  class(result) <- "sig.groups.by.interval"
  if(print.result) print(result)
  invisible(result)
}
print.sig.groups.by.interval <- function(x, ...){
  class(x) <- "list"
  x$groups.long <- NULL
  print(x,quote=FALSE)
}

####
if(FALSE) {
  y <- sample(c(TRUE, FALSE), 100, TRUE)
  y <- sample(c(1, 2), 100, TRUE)
  trt <- c(rep(TRUE, 50), rep(FALSE, 50))
  chisq.test.2groups(y, trt)
}
chisq.test.2groups <- function(y, trt) {
  # This function checks if a categorial varialbes y (0,1 odr TRUE,FALSE) is significantly different for
  # two different groups (treatments) trt
  # The p-value is returned
  
  if(length(unique(trt))>2) stop("For more than 2 groups choose the function chisq.groups()")
  if(length(unique(y))>2) stop("There are more than 2 values in y (the variable is not binary!). -> length(unique(y)) must be 2!")
  
  tables <- table(list(trt=trt, y=y))
  
  result <- list()
  result[["table"]]   <- tables
  result[["proportions"]] <- tables/apply(tables,1,function(x)sum(x))
  result[["p.value"]] <- chisq.test(tables)$p.value
  
  return(result)
}
#y <- sample(c(0,1),30, replace=TRUE); trt <- sample(c(1,2,3),30, replace=TRUE); p.adj="holm"; perc=TRUE; digits=1; sig.level=0.05; median.mean=c("mean"); ranked=c("no"); print.result=TRUE# k1 <- kruskal.groups(y,trt); print.default(k1); k1
# y=dat[,"Out_soueringofmilk"]; trt=dat[,"reg"]
#a <- chisq.groups(y,trt,p.adj="holm"); a
chisq.groups <- function(y, trt, sig.level=0.05, p.adj="holm", perc=TRUE, digits=1, ranked=c("no","character.left","character.right","mean","intern"), print.result=TRUE ) {
  # Do the grouping (a, ab, b, bc,...) for grouped data (1,2,3,4,5,...) with binary categorial variables (0,1). For example a cluster analysis was conducted and now in every cluster there is a certain proportion of observations in the one category and in the other category.
  # i.e. a 2x2 contingency table is checked for significant differences with the chisq.test() function. The data is grouped by the sig.groups() function.
  # y: the data vector, trt (treatment): the grouping vecotr, perc=TRUE: The proportions are given as percentages, digits: digits, ranked: should the output table be sorted according to the proportions..?, print.result: should the result be printed?
  grouping <- trt
  ranked <- match.arg(ranked)
  
  grp <- sort(unique(grouping))
  comb <- combn(grp,2); comb.names <- apply(comb,2,function(x)paste(x,collapse="-"))
  comb <- apply(comb,2,function(x)as.character(x))
  mean <- tapply(y,grouping,function(x)mean(x,na.rm=TRUE))
  tables <- table(list(grouping=grouping, proportion=y))
  
  diffs <- sig <- rep(NA,ncol(comb)); names(diffs) <- names(sig) <- comb.names
  for(i in 1:ncol(comb)) {
    diffs[i] <- mean[comb[1,i]] - mean[comb[2,i]]
    sig[i] <- chisq.test( tables[comb[,i],] )$p.value
  }
  sig <- p.adjust(sig, method=p.adj)
  # Same procedure as in the Kruskal-Wallis-Test: Multiple comparisons between pairs of groups are only done if there is any difference between all of the groups. IF all sig[] are set to 1, then the grouping c(a,a,a,a,...) is produced in the sig.group() function.
  all.nonsig <- chisq.test(tables)$p.value>=sig.level
  # Wenn nur 1 y-Wert gegeben wurde, sind die Werte zwangsl?ufig auch nicht unterschiedlich.
  all.nonsig <- all.nonsig | length(sort(unique(y)))==1
  
  if(all.nonsig) {
    sig.orig <- sig
    sig[] <- 1
  }
  
  if(perc==TRUE) digits <- digits+2
  # set.args("y=y, trt=grouping, sig=sig, sig.level=sig.level ,digits=digits, median.mean='mean', ranked=ranked, print.result=FALSE")
  char.grouping <- sig.groups(y=y, trt=grouping, sig=sig, sig.level=sig.level ,digits=digits, median.mean="mean", ranked=ranked, print.result=FALSE)
  
  if(perc) char.grouping$summary[,c(1,2)] <- char.grouping$summary[,c(1,2)]*100
  char.grouping$summary <- char.grouping$summary[,c(1,3)]
  colnames(char.grouping$summary)[1] <- "prop."
  if(all.nonsig) {
    char.grouping$p.value <- sig.orig
  }
  if(print.result) print(char.grouping)
  invisible(char.grouping)
}

####


#sigdivar(aov.data, aov.factor, minsize=10)
#data(mtcars); data <- mtcars; attach(data); cyl=factor(cyl); aov.factor <- cyl; aov.data <- as.matrix(data[,c(3:11)]); conf.level <- 0.01; a <- 1
sigdivar <- function (aov.data, aov.factor, conf.level=0.05, round=TRUE, digits=3, minsize=NULL) {
  # Explanation: Extract all variables which which reveal significant differences at once out of your (huge) data.frame.
  # The significance is calculated with an ANOVA. Don't forget that you must fullfil the assumptions of normal distribution and homogeneity of variance.
  aov.data <- as.matrix( aov.data )
  nclust <- max(as.numeric(aov.factor))
  aov.factor <- as.factor(as.numeric(as.factor(aov.factor)))
  
  if(!is.null(minsize)) {         # Cluster Minsizes
    aov.factor <- as.numeric(aov.factor)
    aov.data <- as.matrix(cbind(aov.data, aov.factor))
    colnames(aov.data)[ncol(aov.data)] <- "aov.factor"
    
    clustersizes <- matrix(nrow=1, ncol=nclust)
    for(i in 1:nclust)
      clustersizes[,i] <- length(which(aov.data[,"aov.factor"]==i))
    if(max(clustersizes)<minsize) stop("No group has enough members to fullfil the minsize restriction")
    clustersizes.column <- matrix(nrow=nrow(aov.data), ncol=1)
    for (i in 1:nrow(clustersizes.column))
      clustersizes.column[i,] <- clustersizes[,aov.data[i,"aov.factor"]]
    aov.data <- as.matrix(cbind(aov.data, clustersizes.column))
    colnames(aov.data)[ncol(aov.data)] <- "clustersizes.column"
    aov.data <- aov.data[which(aov.data[,"clustersizes.column"]>=minsize),]
    aov.factor <- as.factor(as.numeric(as.factor(aov.data[,"aov.factor"])))
    
    nclust.diff <- nclust - max(as.numeric(aov.factor))
    nclust <- max(as.numeric(aov.factor))
    aov.data <- as.matrix(aov.data[,1:(ncol(aov.data)-2)])
  }
  
  clustersizes <- matrix(nrow=1, ncol=nclust)
  colnames(clustersizes) <- paste("Cluster", 1:nclust, sep=" "); rownames(clustersizes) <- "Clustersizes"
  for(i in 1:nclust)
    clustersizes[,i] <- length(which(aov.factor[]==i))
  
  aov.result <- aov(aov.data ~ aov.factor)
  sigdivar <- matrix(nrow=length(summary(aov.result)), ncol=(2+nclust)); rownames(sigdivar) <- colnames(aov.data); colnames(sigdivar) <- c("F value","Pr(>F)", paste("Cluster", 1:nclust, sep=" "))
  sigmat <- do.call("rbind",summary(aov.result))
  sigdivar[,1] <- sigmat[!is.na(sigmat[,"F value"]),"F value"]
  sigdivar[,2] <- sigmat[!is.na(sigmat[,"Pr(>F)"]),"Pr(>F)"]
  for(i in 1:nclust)
    sigdivar[,i+2] <- colMeans(aov.data[aov.factor==i, , drop=FALSE], na.rm=TRUE)
  if (round) sigdivar <- round(sigdivar, digits=3)
  sigdivar.true <- sigdivar[which(sigdivar[,2]<=conf.level), ,drop=FALSE]
  message <- if(min(clustersizes)<10) {"Warning: there are small groups constisting of less than 10 members. This could distort the analysis. Use the argument minsize=... to kick groups smaller than minsize before doing the ANOVA."
  } else if (!is.null(minsize)) {paste(nclust.diff,"groups were excluded from ANOVA due to small sizes.")
  } else if (is.null(minsize)) {"All groups have more than 10 members. Don't forget to fullfil the assumptions for an ANOVA (normality and equal variances)."}
  
  sigdivar.output <- list(full=sigdivar, sign=sigdivar.true, sizes=clustersizes, message=message)
  return(sigdivar.output)
}

####
tukeyhsd.multiple <- function(aov.data, aov.factor, conf.level=0.05, round=TRUE, digits=3, minsize=NULL){
  # Explanation: Extract all significant differences in variables, like with sigdivar()-function but in this case
  # post-hoc like which means: differences between the individual treatments are also given.
  # Don't forget to fullfill the assumptions of normal distribution and homogeneity of variances for the ANOVA.
  
  aov.data <- aov.data; aov.factor <- aov.factor
  aov.data <- as.matrix(aov.data)
  nclust <- max(as.numeric(aov.factor))
  if(!is.factor(aov.factor)) aov.factor <- factor(aov.factor)
  result <- vector("list", ncol(aov.data))
  
  if(!is.null(minsize)) {         # Cluster Minsizes
    aov.factor <- as.numeric(aov.factor)
    aov.data <- as.matrix(cbind(aov.data, aov.factor))
    colnames(aov.data)[ncol(aov.data)] <- "aov.factor"
    
    clustersizes <- matrix(nrow=1, ncol=nclust)
    for(i in 1:nclust)
      clustersizes[,i] <- length(which(aov.data[,"aov.factor"]==i))
    if(max(clustersizes)<minsize) stop("No group has enough members to fullfil the minsize restriction")
    clustersizes.column <- matrix(nrow=nrow(aov.data), ncol=1)
    for (i in 1:nrow(clustersizes.column))
      clustersizes.column[i,] <- clustersizes[,aov.data[i,"aov.factor"]]
    aov.data <- as.matrix(cbind(aov.data, clustersizes.column))
    colnames(aov.data)[ncol(aov.data)] <- "clustersizes.column"
    aov.data <- aov.data[which(aov.data[,"clustersizes.column"]>=minsize),]
    aov.factor <- as.factor(as.numeric(as.factor(aov.data[,"aov.factor"])))
    
    nclust.diff <- nclust - max(as.numeric(aov.factor))
    nclust <- max(as.numeric(aov.factor))
    aov.data <- as.matrix(aov.data[,1:(ncol(aov.data)-2)])
  }
  
  clustersizes <- matrix(nrow=1, ncol=nclust)
  colnames(clustersizes) <- paste("Cluster", 1:nclust, sep=" "); rownames(clustersizes) <- "Clustersizes"
  for(i in 1:nclust)
    clustersizes[,i] <- length(which(aov.factor[]==i))
  message <- if(min(clustersizes)<10) {"Warning: there are small groups constisting of less than 10 members. This could distort the analysis. Use the argument minsize=... to kick groups smaller than minsize before doing the ANOVA."
  } else if (!is.null(minsize)) {paste(nclust.diff,"groups were excluded from ANOVA due to small sizes.")
  } else if (is.null(minsize)) {"All groups have more than 10 members. Don't forget to fullfil all assumptions for the ANOVA (e.g.) equal variances."}
  
  
  for (a in 1:ncol(aov.data)) {
    aov.r <- aov(aov.data[,a] ~ aov.factor)
    tuk <- TukeyHSD(aov.r)$aov.factor  # , conf.level=conf.level
    result[[a]] <- tuk[which(tuk[,"p adj"]<=conf.level), ,drop=FALSE]
    
    if(nrow(result[[a]])==0) result[[a]] <- NA
    names(result)[[a]] <- paste(colnames(aov.data)[a],sep="")
  }
  result <- result[which(!is.na(result))]
  result <- list(result=result, sizes=clustersizes, message=message)
  return(result)
}
#data(mtcars); data <- mtcars; attach(data); cyl=factor(cyl); aov.factor <- cyl; aov.data <- as.matrix(data[,c(3:11)]); conf.level <- 0.01; a <- 1
#tukeyhsd.multiple(aov.data, aov.factor, conf.level=1, minsize=10)

# Sortieren der Ergebnisse nach Gruppennummer
#if (all(nchar(rownames(result.a))==3)) {result.a <- as.matrix(cbind(result.a, as.numeric(substr(rownames(result.a),1,1)), as.numeric(substr(rownames(result.a),3,3)))); doublesort <- TRUE
#} else {result.a <- as.matrix(cbind(result.a, as.numeric(substr(rownames(result.a),1,1)))); doublesort<-FALSE}
#if(doublesort) {colnames(result.a)[ncol(result.a)-1] <- "colname1"; colnames(result.a)[ncol(result.a)] <- "colname2"
#} else {colnames(result.a)[ncol(result.a)] <- "colname1"}
#if(doublesort) { if(nrow(result.a)>1) result.a <- result.a[order(result.a[,"colname1"],result.a[,"colname2"]),]
#                 result.a <- result.a[,1:(ncol(result.a)-2),drop=FALSE]
#} else { if(nrow(result.a)>1) result.a <- result.a[order(result.a[,"colname1"]),]
#         result.a <- result.a[,1:(ncol(result.a)-1), drop=FALSE] }
#result[[a]] <- result.a

# Altvernative without adding columns to the dataframe. But doesn't work!
#if(!is.null(minsize)) {         # Cluster Minsizes
#  aov.factor.m <- as.numeric(aov.factor)
#  clustersizes <- rep(0,nclust)
#  for(i in 1:nclust)
#    clustersizes[i] <- length(which(aov.factor.m[]==i))
#  clustersizes.column <- rep(0,nrow(aov.data))
#  for (i in 1:length(clustersizes.column))
#    clustersizes.column[i] <- clustersizes[aov.factor.m[i]]
#  aov.data <- aov.data[which(clustersizes.column[]>=minsize),]
#  aov.factor.m <- as.factor(as.numeric(aov.factor.m))
#
#  nclust.diff <- nclust - max(as.numeric(aov.factor.m))
#  nclust <- max(as.numeric(aov.factor.m))
#  aov.data <- as.matrix(aov.data)
#}

####
sigtest.1vsall <- function(x,trt,method=c("kruskal","wilcoxon","t.test")) {
  if(!is.vector(x)) stop(paste("x must be a vector. Your argument is of class",class(x),sep=""))
  method <- match.arg(method)
  uniquetrt <- sort(unique(trt))
  trt.new <- list()
  for(i in 1:length(uniquetrt)) trt.new[[i]] <- as.numeric( trt==uniquetrt[i] )
  
  sig <- rep(0,length(uniquetrt))
  sig.sign <- rep("",length(uniquetrt))
  
  if(method=="kruskal") {
    for(i in 1:length(uniquetrt)) sig[i] <- kruskal.test(x,trt.new[[i]])$p.value
  } else if (method=="wilcoxon") {
    for(i in 1:length(uniquetrt)) sig[i] <- wilcox.test(x,trt.new[[i]])$p.value
  } else if (method=="t.test") {
    for(i in 1:length(uniquetrt)) sig[i] <- t.test(x,trt.new[[i]])$p.value
  }
}
####

## insert ad.test() to test for normality (and rank() as a additional kind of transformation?)
## insert also: Erst positiv machen, dann transformationen durchfuehren.GEMACHT!
if(FALSE){
  m <- 10; sd = 2
  dat <- matrix(NA,nrow=1000, ncol=9)
  dat[,1] <- rnorm(1000,m,sd)
  dat[,2] <- rnorm(1000,m,sd)^(-0.5);     (m^(-0.5))^-2
  dat[,3] <- rnorm(1000,m,sd)^(-1);       (m^(-1))^-1
  dat[,4] <- rnorm(1000,m,sd)^(-2);       (m^(-2))^-(0.5)
  dat[,5] <- rnorm(1000,m,sd)^2;          (m^(2))^(0.5)
  dat[,6] <- rnorm(1000,m,sd)^(0.5);      (m^(0.5))^(2)
  dat[,7] <- exp(rnorm(1000,m,sd));       exp(log(m))
  dat[,8] <- log(rnorm(1000,m,sd));       log(exp(m))
  dat[,9] <- 1:1000;
  colnames(dat) <- c("x",
                     "x^(-0.5)",
                     "x^(-1)",
                     "x^(-2)",
                     "x^2",
                     "x^(0.5)",
                     "exp(x)",
                     "log(x)",
                     "never")
  normalize(dat)
  normalize.qqplot(dat,window=FALSE)
}

normalize.qqplot <- function(data,replace.Inf=TRUE,window=FALSE,mar=c(2.1,2.1,2.1,2.1),...){
  data <- as.data.frame(data)
  
  data.new <- data
  colnames(data.new) <- paste0(colnames(data.new)," original")
  qqplot.multiple(data.new,colnames(data.new),window=window,mt="orig",...)
  
  data.new <- data^(-2)
  if(replace.Inf) data.new[data.new==Inf] <- NA; data.new[data.new==-Inf] <- -NA
  colnames(data.new) <- paste0(colnames(data.new),"^(-2)")
  qqplot.multiple(data.new,colnames(data.new),window=window,mt="^(-2)",mar=mar,...)
  
  data.new <- data^(-1)
  if(replace.Inf) data.new[data.new==Inf] <- NA; data.new[data.new==-Inf] <- -NA
  colnames(data.new) <- paste0(colnames(data.new),"^(-1)")
  qqplot.multiple(data.new,colnames(data.new),window=window,mt="^(-1)",mar=mar,...)
  
  data.new <- data^(-0.5)
  if(replace.Inf) data.new[data.new==Inf] <- NA; data.new[data.new==-Inf] <- -NA
  colnames(data.new) <- paste0(colnames(data.new),"^(-0.5)")
  qqplot.multiple(data.new,colnames(data.new),window=window,mt="^(-0.5)",mar=mar,...)
  
  data.new <- data^(0.5)
  if(replace.Inf) data.new[data.new==Inf] <- NA; data.new[data.new==-Inf] <- -NA
  colnames(data.new) <- paste0(colnames(data.new),"^0.5")
  qqplot.multiple(data.new,colnames(data.new),window=window,mt="^(0.5)",mar=mar,...)
  
  data.new <- data^(2)
  if(replace.Inf) data.new[data.new==Inf] <- NA; data.new[data.new==-Inf] <- -NA
  colnames(data.new) <- paste0(colnames(data.new),"^2")
  qqplot.multiple(data.new,colnames(data.new),window=window,mt="^(2)",mar=mar,...)
  
  data.new <- log(data)
  if(replace.Inf) data.new[data.new==Inf] <- NA; data.new[data.new==-Inf] <- -NA
  colnames(data.new) <- paste0("log(",colnames(data.new),")")
  qqplot.multiple(data.new,colnames(data.new),window=window,mt="log()",mar=mar,...)
  
  data.new <- exp(data)
  if(replace.Inf) data.new[data.new==Inf] <- NA; data.new[data.new==-Inf] <- -NA
  colnames(data.new) <- paste0("exp(",colnames(data.new),")")
  qqplot.multiple(data.new,colnames(data.new),window=window,mt="exp()",mar=mar,...)
}

# data=dat; sig.level=0.05; qq=FALSE; window=FALSE
normalize <- function(data, qq=TRUE, window=FALSE, ...) {
  # This function tries to normalize each columng of a given data.frame by applying different functions.
  #
  # Arguments:
  # data =   data.frame or matrix with data to be normalized.
  # qq =     logical indicating if qq plot should be plotted.
  # window = logical indicating if a new window should be opened for the qq plot.
  #
  # Value:
  # A list containing
  # $data: the new data that has potentially been normalized
  # $pValsPerCol: the p values that were calculated by the lillifors test -> to check normal distribution.
  #               values *above* 0.05 indicate normal distribution.
  # $useFuncsPerCol: the functions that were applied on all columns of data to come closer to normal distribution.
  #
  # Detail:
  # Functions might be applied to different columns even though the lilliefors test did not yield a p.value > 0.05.
  # This will be the case if the p.value gets larger compared to the original distribution.
  
  funcs <- list("x^(-2)"=function(x)x^(-2),
                "x^(-1)"=function(x)x^(-1),
                "x^(-0.5)"=function(x)x^(-0.5),
                "log(x)"=function(x)log(x),
                "x^(0.5)"=function(x)x^(0.5),
                "x"=function(x)x,
                "x^(2)"=function(x)x^(2),
                "exp(x)"=function(x)exp(x)
  )
  
  lillie.test <- function (x)         # lillie.test{nortest} modified. Now it also works with data containing always the same number.
  {
    DNAME <- deparse(substitute(x))
    x <- sort(x[complete.cases(x)])
    n <- length(x)
    if (n > 4) {                                                   # change from original here
      
      p <- pnorm((x - mean(x))/sd(x))
      Dplus <- max(seq(1:n)/n - p)
      Dminus <- max(p - (seq(1:n) - 1)/n)
      K <- max(Dplus, Dminus)
      if (n <= 100) {
        Kd <- K
        nd <- n
      }  else {
        Kd <- K * ((n/100)^0.49)
        nd <- 100
      }
      pvalue <- exp(-7.01256 * Kd^2 * (nd + 2.78019) + 2.99587 *
                      Kd * sqrt(nd + 2.78019) - 0.122119 + 0.974598/sqrt(nd) +
                      1.67997/nd)
      if (!is.na(pvalue)) {                                        # change from original here
        if (pvalue > 0.1) {
          KK <- (sqrt(n) - 0.01 + 0.85/sqrt(n)) * K
          if (KK <= 0.302) {
            pvalue <- 1
          }    else if (KK <= 0.5) {
            pvalue <- 2.76773 - 19.828315 * KK + 80.709644 *
              KK^2 - 138.55152 * KK^3 + 81.218052 * KK^4
          }    else if (KK <= 0.9) {
            pvalue <- -4.901232 + 40.662806 * KK - 97.490286 *
              KK^2 + 94.029866 * KK^3 - 32.355711 * KK^4
          }    else if (KK <= 1.31) {
            pvalue <- 6.198765 - 19.558097 * KK + 23.186922 *
              KK^2 - 12.234627 * KK^3 + 2.423045 * KK^4
          }    else {
            pvalue <- 0
          }
        }
      } else { pvalue <- 0 }                                  # change from original here
    } else {
      pvalue <- 0                                             # change from original here
      K <- 0                                                  # change from original here
      D <- 0                                                  # change from original here
    }                                                 
    RVAL <- list(statistic = c(D = K), p.value = pvalue, method = "Lilliefors (Kolmogorov-Smirnov) normality test",
                 data.name = DNAME)
    class(RVAL) <- "htest"
    return(RVAL)
  }
  
  pValsPerCol <- sapply(funcs, function(func){
    apply( apply(data,2,function(x)func(x)) ,2,function(x)lillie.test(x)$p.value)
  })
  
  useFuncsPerCol <- apply(pValsPerCol,1,function(x)names(funcs)[which.max(x)] )
  data.new <- data
  cn1 <- colnames(data)
  for(i in sort(unique(useFuncsPerCol))){ # i <- sort(unique(useFuncsPerCol))[1]
    transformCol <- names(useFuncsPerCol)[useFuncsPerCol==i]
    data.new[,transformCol] <- funcs[[i]]( data.new[,transformCol] )
  }
  
  if(qq) qqplot.multiple(data.new, colnames(data.new), window=window, mt=useFuncsPerCol)
  return(list(data=data.new,
              pValsPerCol=pValsPerCol,
              useFuncsPerCol=useFuncsPerCol
  ) )
}

####
qqplot.multiple <- function(data,variables,plotrows=5,mar=c(2.1,2.1,2.1,2.1),window=FALSE,mt=NULL,...){
  par.orig <- par()$mar; mfrow.orig <- par()$mfrow; on.exit(par(mar=par.orig, mfrow=mfrow.orig))
  
  if(length(mt)==1) mt <- rep(mt,ncol(data))
  if(window) windows()
  nvariables <- length(variables)
  par(mar=mar, mfrow=c(plotrows,if(nvariables%%plotrows==0) nvariables/plotrows else floor(nvariables/plotrows)+1 ))
  for(i in 1:nvariables) {
    dat <- as.numeric( data[,variables[i]] )
    qqnorm(dat,pch=20,main=paste0(variables[i], "  ->  use:", mt[i]))
    qqline(dat)
  }
}



#### DELETE FUNCTIONS ####

if(FALSE) mean.weight_DELETE <- function(data, weights=NULL, index=NULL, calc.sum=FALSE, digits=NULL, na.rm=TRUE, edit.I.colnames=TRUE, del.I.help.columns=FALSE, I.help.columns=NULL){
  # This function calculates the weighted mean of all variables in a possibly indexed data.frame or matrix.
  
  # Arguments
  # data = data of which the weighted means should be calculated. Can be data.frame, matrix or vector
  #        If any colname of data contains an expression like I(Var_A/Var_B), then the the "weighted mean of the ratio" is calculated.
  #        This is done by building a model.matrix() of the result matrix.
  #        Use function extract.I.vars() to add all variables to your data frame that are used in the formula
  # weights = weights for the weighted mean calculation
  # index = index in the same structure as used in tapply(). Can be a vector or list of vectors.
  # calc.sum = Should sum(data*weights) should be calculated, rather than weighted means?
  # digits = digits for rounding the results
  # na.rm = na action
  # edit.I.colnames = Should the colnames containing expressions with I() be edited, such that I() won't be there anymore? TRUE/FALSE
  
  # Wenn innerhalb eines Indexes mehrere Indexe als Listen abgelegt sind, wird die Berechnung fuer alle Indexe gemacht.
  #if(is.list(index)){
  #  if(any(sapply(index,function(x)is.list(x)))){
  #    return(do.call("rbind", lapply(index, function(x)mean.weight(data=data, weights=weights, index=x, digits=digits, na.rm=na.rm, edit.I.colnames=edit.I.colnames, del.I.help.columns=del.I.help.columns, I.help.columns=I.help.columns))))
  #  }
  #}
  if(!is.list(index)) index <- list(index)
  
  # Im Falle, dass !is.null(dim(data)) folgt eine rekursive Funktionsdefinition!
  if(!is.null(dim(data))) {
    # Wenn !is.null(dim(data))
    # & es keinen oder nur einen Index gibt:
    if(is.null(index) || length(index)==1) {
      if(is.matrix(data)) {
        if(nrow(data)==0) stop("nrow of data is 0.")
        result <- apply(data, 2, function(x)mean.weight(data=x, weights=weights, index=index, calc.sum=calc.sum, digits=digits, na.rm=na.rm, edit.I.colnames=edit.I.colnames, del.I.help.columns=del.I.help.columns, I.help.columns=I.help.columns))
      } else if(is.data.frame(data)) {
        if(nrow(data)==0) stop("nrow of data is 0.")
        result <- sapply(data, function(x)mean.weight(data=x, weights=weights, index=index, calc.sum=calc.sum, digits=digits, na.rm=na.rm, edit.I.colnames=edit.I.colnames, del.I.help.columns=del.I.help.columns, I.help.columns=I.help.columns))
      }
      # Wieder zu Marix machen, falls es ein Vektor ist
      if(is.null(dim(result))) result <- t(as.matrix(result))
      #if(nrow(result)==1) rownames(result) <- NULL
      # Wieder die alten Colnames vergeben
      colnames(result) <- colnames(data)
      
      # Falls eine Expression mit I() in einem der colnames ist, werden diese Kennzahlen neu berechnet.
      # Konkret wird statt "weighted mean of ratio" das "ratio of weighted means" berechnet.
      cn.res <- colnames(result) # cn.res.orig
      icols <- substr(cn.res,1,2)=="I("
      if(any(icols)){
        if(!is.null(digits)) stop("When rounding (digts!=NULL) and using I() columns, the results might not be accurate")
        # Wert der I() columns berechnen
        result <- calc.I.cols(result, edit.I.colnames=edit.I.colnames, del.I.help.columns=del.I.help.columns, I.help.columns=I.help.columns)
      }
      
      # Resultat ausgeben.
      if(nrow(result)==1) rownames(result) <- NULL
      return(result)
      
      
      # Wenn !is.null(dim(data))
      # & 2 Indexe eingegeben wurden:
    } else if(length(index)==2) {
      # res1 <- mean.weight(data=data[,1], weights=weights, index=index, calc.sum=calc.sum, digits=digits, na.rm=na.rm)
      
      # Hier keine Fallunterscheidung zwischen matrix und data.frame einfuegen, sonst funktioniert es nicht!!
      res.prov <- apply(data, 2, function(x) mean.weight(data=x, weights=weights, index=index, calc.sum=calc.sum, digits=digits, na.rm=na.rm) )
      if(class(res.prov)!="matrix") res.prov <- t(as.matrix(res.prov))
      
      res.list <- list()
      su.index1 <- sort(unique(index[[1]]))
      su.index2 <- sort(unique(index[[2]]))
      for(i in 1:ncol(res.prov)){
        res.list[[i]] <- matrix(res.prov[,i],nrow=length(su.index1), ncol=length(su.index2))
        dimnames(res.list[[i]]) <- list(su.index1, su.index2)
      }
      names(res.list) <- colnames(data)
      
      # Falls eine Expression mit I() in einem der colnames ist, werden diese Kennzahlen neu berechnet.
      # Konkret wird statt "weighted mean of ratio" das "ratio of weighted means" berechnet.
      cn.res <- names(res.list)
      icols <- grepl("I\\(", cn.res)
      if(any(icols)){
        if(!is.null(digits)) stop("When rounding (digts!=NULL) and using I() columns, the results might not be accurate")
        #if(any(cn.res%in%c("_","."))) stop("When using I() colnames _ and . are not allowed.")
        res.list <- calc.I.cols(res.list, edit.I.colnames=edit.I.colnames, del.I.help.columns=del.I.help.columns, I.help.columns=I.help.columns)
      }
      return(res.list)
      
    } else if(length(index)>2) {
      stop("more than 2 indexes not possible if data is a matrix/data.frame. Please enter data as vector.")
    }
  }
  
  
  # Tatsaechliche mean.weight() Funktion.
  # Falls es keine numerische Variable ist (weil z.B. ein durchmischter data.frame eingegeben wird),
  # wird daraus eine 0 gemacht, damit die Funktion trotzdem funktioniert.
  if(! (is.numeric(data)||is.logical(data)) ) data <- rep(0, length(data))
  
  if(is.null(weights)) weights <- rep(1,length(data))
  
  # Falls kein index gegeben wurde, einfache Berechnung (mit weighted.mean)
  if( is.null(index) | is.null(index[[1]]) ){
    if(calc.sum){
      result <- sum( data * weights ,na.rm=na.rm )
    } else {
      result <- weighted.mean(data,weights, na.rm=na.rm)
    }
    
    # Sonst muss mit index und tapply() gerechnet werden.
  } else {
    index <- lapply(index, function(x)if(length(x)==1) return(rep(x,length(weights))) else return(x))
    length.index <- sapply(index,function(x)length(x))
    if(any(length.index!=length.index[1])) stop("All vectors in the index have to have the same length!")
    #print(length(weights)); print(length.index)
    if(!all(length(weights)==length.index)) stop("length(weights)!=length(index)")
    
    # NA Werte in weights uebertragen. Muss so sein, nicht mit na.rm innerhalb der Funktionen, da sonst data und weights evtl. nicht korrespondieren!!
    dataweights <- data*weights
    weights[is.na(dataweights)] <- NA
    
    if(calc.sum){
      # Resultat = Summe ( Werte * Gewichte )
      result <-  tapply(dataweights,index,  sum,na.rm=na.rm)
    } else {
      # Resultat = Summe ( Werte * Gewichte )                             / Summe( Gewichte )
      result <-  tapply(dataweights,index,  sum,na.rm=na.rm) / tapply(weights,index,  sum,na.rm=na.rm)
    }
    
  }
  
  # Falls gewuenscht, runden, dann Ergebnis ausgeben.
  if(!is.null(digits)) result <- round(result, digits)
  return(result)
}

# data=dat; sig.level=0.05; qq=FALSE; window=FALSE
if(FALSE) normalize_OLD_DELETE <- function(data, sig.level=0.05, qq=FALSE, window=FALSE, ...) {
  if(!any(c(is.matrix(data), is.data.frame(data)))) stop("Data must be a matrix or data.frame.")
  if(any(  c(data[!is.na(data[]==Inf)]==Inf, data[!is.na(data[]==-Inf)]==-Inf)  ))  stop("Infinite numbers are not allowed")
  
  data <- as.data.frame(data)
  lillie.test <- function (x)         # lillie.test{nortest} modified. Now it also works with data containing always the same number.
  {
    DNAME <- deparse(substitute(x))
    x <- sort(x[complete.cases(x)])
    n <- length(x)
    if (n > 4) {                                                   # change from original here
      
      p <- pnorm((x - mean(x))/sd(x))
      Dplus <- max(seq(1:n)/n - p)
      Dminus <- max(p - (seq(1:n) - 1)/n)
      K <- max(Dplus, Dminus)
      if (n <= 100) {
        Kd <- K
        nd <- n
      }  else {
        Kd <- K * ((n/100)^0.49)
        nd <- 100
      }
      pvalue <- exp(-7.01256 * Kd^2 * (nd + 2.78019) + 2.99587 *
                      Kd * sqrt(nd + 2.78019) - 0.122119 + 0.974598/sqrt(nd) +
                      1.67997/nd)
      if (!is.na(pvalue)) {                                        # change from original here
        if (pvalue > 0.1) {
          KK <- (sqrt(n) - 0.01 + 0.85/sqrt(n)) * K
          if (KK <= 0.302) {
            pvalue <- 1
          }    else if (KK <= 0.5) {
            pvalue <- 2.76773 - 19.828315 * KK + 80.709644 *
              KK^2 - 138.55152 * KK^3 + 81.218052 * KK^4
          }    else if (KK <= 0.9) {
            pvalue <- -4.901232 + 40.662806 * KK - 97.490286 *
              KK^2 + 94.029866 * KK^3 - 32.355711 * KK^4
          }    else if (KK <= 1.31) {
            pvalue <- 6.198765 - 19.558097 * KK + 23.186922 *
              KK^2 - 12.234627 * KK^3 + 2.423045 * KK^4
          }    else {
            pvalue <- 0
          }
        }
      } else {pvalue <- 0}                                          # change from original here
    } else {pvalue <- 0                                             # change from original here
    K <- 0                                                  # change from original here
    D <- 0}                                                 # change from original here
    RVAL <- list(statistic = c(D = K), p.value = pvalue, method = "Lilliefors (Kolmogorov-Smirnov) normality test",
                 data.name = DNAME)
    class(RVAL) <- "htest"
    return(RVAL)
  }
  
  minsize <- apply(data,2,function(x)length(x[!is.na(x)])>4) # lillie.test needs sample of size 4
  minsize <- names(minsize[minsize[]==TRUE])
  too.small <- apply(data,2,function(x)length(x[!is.na(x)])<=4)
  too.small <- names(too.small[too.small[]==TRUE])
  if (length(too.small)==0) too.small <- NULL
  data.orig <- data
  data <- data[,minsize,drop=FALSE]
  
  # 0 ok without operation
  # 1 ok with ^(-2)
  # 2 ok with ^(-1)
  # 3 ok with ^(-0.5)
  # 4 ok with log()
  # 5 ok with ^(0.5)
  # 6 ok with ^(2)
  # 7 ok with exp()
  
  if(qq) qqplot.multiple(data,colnames(data),window=window,mt="orig")#,...)
  a0.p <- apply(data,2,function(x)lillie.test(x)$p.value)
  a0 <- a0.p >= sig.level
  a0.ok <- names(a0)[a0]; if(length(a0.ok)==0) a0.ok <- NULL
  a0.next <- names(a0)[!a0]
  # Einschub: alle Werte positiv machen, zusaetzlich wurde ganz unten fuer data[,a0.ok] data.orig[,a0.ok] eingefuegt
  mins <- apply(data,2,function(x) if (min(x,na.rm=TRUE)<0) -min(x,na.rm=TRUE) else 0)
  min.matrix <- t(matrix( rep(mins,nrow(data)), ncol=nrow(data) ))
  data <- data + min.matrix
  # Ende Einschub
  data.new <- data[,a0.next,drop=FALSE]^(-2)
  data.new[data.new==Inf] <- NA; data.new[data.new==-Inf] <- NA
  if(qq) qqplot.multiple(data.new,colnames(data.new),window=window,mt="^(-2)")#,...)
  
  
  if(ncol(data.new)>0){
    a1.p <- apply(data.new,2,function(x)lillie.test(x)$p.value)
    a1 <- a1.p >= sig.level
    a1.ok <- names(a1)[a1]; if(length(a1.ok)==0) a1.ok <- NULL
    a1.next <- names(a1)[!a1]
    data.new <- data[,a1.next,drop=FALSE]^(-1)
    data.new[data.new==Inf] <- NA; data.new[data.new==-Inf] <- NA
    if(qq) qqplot.multiple(data.new,colnames(data.new),window=window,mt="^(-1)")#,...)
  } else {a1.ok <- NULL}
  
  if(ncol(data.new)>0){
    a2.p <- apply(data.new,2,function(x)lillie.test(x)$p.value)
    a2 <- a2.p >= sig.level
    a2.ok <- names(a2)[a2]; if(length(a2.ok)==0) a2.ok <- NULL
    a2.next <- names(a2)[!a2]
    data.new <- data[,a2.next,drop=FALSE]^(-0.5)
    data.new[data.new==Inf] <- NA; data.new[data.new==-Inf] <- NA
    if(qq) qqplot.multiple(data.new,colnames(data.new),window=window,mt="^(-0.5)")#,...)
  } else {a2.ok <- NULL}
  
  if(ncol(data.new)>0){
    a3.p <- apply(data.new,2,function(x)lillie.test(x)$p.value)
    a3 <- a3.p >= sig.level
    a3.ok <- names(a3)[a3]; if(length(a3.ok)==0) a3.ok <- NULL
    a3.next <- names(a3)[!a3]
    data.new <- log(data[,a3.next,drop=FALSE])
    data.new[data.new==Inf] <- NA; data.new[data.new==-Inf] <- NA
    if(qq) qqplot.multiple(data.new,colnames(data.new),window=window,mt="log()")#,...)
  } else {a3.ok <- NULL}
  
  if(ncol(data.new)>0){
    a4.p <- apply(data.new,2,function(x)lillie.test(x)$p.value)
    a4 <- a4.p >= sig.level
    a4.ok <- names(a4)[a4]; if(length(a4.ok)==0) a4.ok <- NULL
    a4.next <- names(a4)[!a4]
    data.new <- data[,a4.next,drop=FALSE]^(0.5)
    data.new[data.new==Inf] <- NA; data.new[data.new==-Inf] <- NA
    if(qq) qqplot.multiple(data.new,colnames(data.new),window=window,mt="^(0.5)")#,...)
  } else {a4.ok <- NULL}
  
  if(ncol(data.new)>0){
    a5.p <- apply(data.new,2,function(x)lillie.test(x)$p.value)
    a5 <- a5.p >= sig.level
    a5.ok <- names(a5)[a5]; if(length(a5.ok)==0) a5.ok <- NULL
    a5.next <- names(a5)[!a5]
    data.new <- data[,a5.next,drop=F]^(2)
    data.new[data.new==Inf] <- NA; data.new[data.new==-Inf] <- NA
    if(qq) qqplot.multiple(data.new,colnames(data.new),window=window,mt="^(2)")#,...)
  } else {a5.ok <- NULL}
  
  if(ncol(data.new)>0){
    a6.p <- apply(data.new,2,function(x)lillie.test(x)$p.value)
    a6 <- a6.p >= sig.level
    a6.ok <- names(a6)[a6]; if(length(a6.ok)==0) a6.ok <- NULL
    a6.next <- names(a6)[!a6]
    data.new <- exp(data[,a6.next,drop=FALSE])
    data.new[data.new==Inf] <- NA; data.new[data.new==-Inf] <- NA
    if(qq) qqplot.multiple(data.new,colnames(data.new),window=window,mt="exp()")#,...)
  } else {a6.ok <- NULL}
  
  if(ncol(data.new)>0){
    a7.p <- apply(data.new,2,function(x)lillie.test(x)$p.value)
    a7 <- a7.p >= sig.level
    a7.ok <- names(a7)[a7]; if(length(a7.ok)==0) a7.ok <- NULL
    a7.next <- names(a7)[!a7]
  } else {a7.ok <- NULL; a7.next <- NULL}
  
  ok.data <- c(a0.ok, a1.ok, a2.ok, a3.ok, a4.ok, a5.ok, a6.ok, a7.ok)
  data.new <- matrix(NA, ncol=length(ok.data), nrow=nrow(data))
  data.orig <- as.matrix(data.orig)
  data.new <- as.matrix(data.new)
  colnames(data.new) <- ok.data
  if (length(a0.ok)>0) data.new[,a0.ok] <- data.orig[,a0.ok] # if (length(a0.ok)>0) data.new[,a0.ok] <- data[,a0.ok]  # so war's vorher.
  if (length(a1.ok)>0) data.new[,a1.ok] <- data[,a1.ok]^(-2)
  if (length(a2.ok)>0) data.new[,a2.ok] <- data[,a2.ok]^(-1)
  if (length(a3.ok)>0) data.new[,a3.ok] <- data[,a3.ok]^(-0.5)
  if (length(a4.ok)>0) data.new[,a4.ok] <- log(data[,a4.ok])
  if (length(a5.ok)>0) data.new[,a5.ok] <- data[,a5.ok]^(0.5)
  if (length(a6.ok)>0) data.new[,a6.ok] <- data[,a6.ok]^(2)
  if (length(a7.ok)>0) data.new[,a7.ok] <- exp(data[,a7.ok])
  
  toImproveMessage <- ""
  toImproveP <- toImprove <- NULL
  if (length(a7.next)>0 | length(too.small)>0) {
    # Check if a transformation is at least better than the original. Although the significance value was not achieved.
    toImproveP <- cbind(a0.p[a7.next], a1.p[a7.next], a2.p[a7.next], a3.p[a7.next], a4.p[a7.next], a5.p[a7.next], a6.p[a7.next], a7.p[a7.next])
    colnames(toImproveP) <- c("original","x^(-2)","x^(-1)","x^(-0.5)","log(x)","x^(0.5)","x^(2)","exp(x)")
    if(any( apply(toImproveP,1,function(x)any(x>x[1]))  )) {
      toImprove <- apply(toImproveP,1,function(x) x > x[1] )
      if(ncol(toImprove)==1) toImprove <- t(toImprove)
      colnames(toImprove) <- colnames(toImproveP)
      toImproveMessage <- "Some variables can be improved by using other transformations. See $to.improve in result."
    }
    # Make the new data set.
    data.mixed <- data.new
    if (length(a7.next)>0)   data.mixed <- cbind(data.mixed, data.orig[,a7.next,drop=FALSE])
    if (length(too.small)>0) data.mixed <- cbind(data.mixed, data.orig[,too.small,drop=FALSE])
    data.mixed <- data.mixed[,colnames(data.orig),drop=FALSE]
  }
  if(qq) qqplot.multiple(data.mixed,colnames(data.mixed),window=window,mt="mixed")
  explanation <- "normal: data was already normally distributed, reciprocal.square: ^(-2) was needed to normalize, reciprocal: ^(-1) ..., reciprocal.square.root: ^(-0.5) ..., log: log() ..., square.root: ^0.5 ..., square: ^2 ..., exp: exp() ..."
  result <- list(normalized.data=data.new, mixed.normalized.data=data.mixed, normal=a0.ok, reciprocal.square=a1.ok, reciprocal=a2.ok, reciprocal.square.root=a3.ok, log=a4.ok, square.root=a5.ok, square=a6.ok, exp=a7.ok, never.normal=a7.next, not.enough.observations.for.analysis=too.small, to.improve.p.val=toImproveP, to.improve=toImprove, explanation=explanation, to.improve.message=toImproveMessage)
  return(result)
}


#### Anzeige, dass Funktionen geladen wurden ####
cat("**********************************************************************\nFunctions loaded\n**********************************************************************\n")
