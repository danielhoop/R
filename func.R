### Author: Daniel Hoop
###
### Insert the command: source("filedirectory of this file") into your Rprofile.SITE. You find the Rprofile.SITE in some location like:  C:\Programme\R\R-2.15.0\etc\Rprofile.SITE
### Or read it in via R console > source("filedirectory of this file")
### Or simply copy it into your R console.

# Functions for the ZA data
# -------------------------
# mean.weight / mean.gb
# categ.to.bin
# group.by.quantiles
# merge.gb
# signif.equally
# equal.n.decimals
#
# source("O:/Sites/TA/Transfer/hpda/R/func.R")
# source("//evdad.admin.ch/AGROSCOPE_OS/2/5/2/1/2/2841/hpda/R/func.R")
# source("https://raw.githubusercontent.com/danielhoop/R/master/func.R")
# browseURL("https://github.com/danielhoop/R/blob/master/func.R")


#### Options ####
# Optionen nur bei mir selbst einlesen. Nicht auf anderen Computern (falls Script-Ausfuehrung ueber Laufwerk W:)
if(FALSE) if(length(list.files("C:/Users/U80823148/"))>0) {
  options(scipen = 3) # mit scipen = 3 geht die Digits-Anzeige bis 0.000001 (also 1e-06). Ab 1e-07 in scientific notation.
  options(help.try.all.packages=TRUE)
  #options(prompt="    ")
  options(stringsAsFactors=FALSE)
  #options(max.print=1000)
  #options(na="")
  cat("**********************************************************************\n")
  cat("Options set.\n")
}

#### Automatisches Kopieren auf W ####
# Nicht auf anderen Computern (falls Script-Ausfuehrung ueber Laufwerk W:)
if(length(list.files("C:/Users/U80823148/"))>0) {
  pathP <- "//evdad.admin.ch/AGROSCOPE_OS/2/5/2/1/2/2841/hpda/R/"
  pathW <- "O:/Sites/TA/Transfer/hpda/R/"
  file.remove(paste0(pathW,"func.R"))
  file.copy(paste0(pathP,"func.R"), paste0(pathW,"func.R"))
  file.remove(paste0(pathW,"Rhelp.R"))
  file.copy(paste0(pathP,"Rhelp.R"), paste0(pathW,"Rhelp.R"))
  rm(pathP, pathW)
  cat("func.R and Rhelp.R copied from P to W\n")
}

#### GRAPHICS ####

show.pch <- function(show=1:255,mfrow=c(5,5),mar=c(4,1,1,3)){
  # Show what the pch numbers mean (in a graph).
  mar.orig <- par()$mar; mfrow.orig <- par()$mfrow;  on.exit(par(mar=mar.orig, mfrow=mfrow.orig))
  par(mar=mar, mfrow=mfrow)
  for(i in show) suppressWarnings( plot(1,pch=i,xlab=i) )
  par(mar=mar.orig, mfrow=mfrow.orig)
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
  
  yxlim <- par("usr")
  if(v=="o") {yjust <- 1; y <- yxlim[4];} else if(v=="m") {yjust <- 0.5; y <- mean(c(yxlim[3],yxlim[4]));} else if(v=="u") {yjust <- 0; y <- yxlim[3]+ply;}
  if(h=="l") {xjust <- 0; x <- yxlim[1];} else if(h=="m") {xjust <- 0.5; x <- mean(c(yxlim[1],yxlim[2]));} else if(h=="r") {xjust <- 1; x <- yxlim[2]+plx;}
  legend(x=x,y=y,xjust=xjust,yjust=yjust, ...)
}
####

linreg.plot <- function(form,data,method=c("lm","rlm"),...){
  # see also curve()
  
  method <- match.arg(method)
  if(method=="lm") {
    reg <- lm(form, data=data)
    coefs <- reg$coefficient
  } else if(method=="rlm") {
    require(MASS)
    reg <- rlm(form, data=data)
    coefs <- reg$coefficients
  }
  abline(coefs[1],coefs[2],...)
  invisible(summary(reg)$coefficient)
}
####
if(FALSE){
  pairs.smooth(cor.data, main="Zusammenhang zw. SDB u. Sampling Rate AG(SO)")
  pch=20; cors=c("no","upper","lower")[2]; abline01=TRUE; pointscol="black"; smoothcol="red"; ablinecol="chartreuse4"; digits=2
  x <- cor.data
}
pairs.smooth <- function(x, pch=20, cors=c("no","upper","lower"), abline01=TRUE, pointscol="black", smoothcol="red", ablinecol="chartreuse4", digits=2, ...){
  cors <- match.arg(cors)
  panel.cor <- function(x, y, digits=2, prefix="", cex.cor, ...)  {
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(0, 1, 0, 1))
    r <- round( cor(x, y), digits=digits) # abs(cor(x, y))
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
  
  binwidth <- h[[grp[1]]]$mids[2] - h[[grp[1]]]$mids[1] # Die bins sind für alle Gruppen gleich gross.
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
      
      binwidth <- h[[grp[1]]]$mids[2] - h[[grp[1]]]$mids[1] # Die bins sind für alle Gruppen gleich gross.
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
  
  #lux <- length(unique(x))
  #if(lux<15){
  #  lx <- length(x)
  #  choose <- apply(  
  #    abs( matrix( rep(x,colsteps),ncol=lx,byrow=TRUE) -
  #           matrix( rep(seq(min(x),max(x), length.out=colsteps),lx), ncol=lx ) ) ,
  #    2, function(x)which.min(x)[1] )
  #  return( colorRampPalette(colors) (colsteps) [ choose ] )
  #} else {
    return( colorRampPalette(colors) (colsteps) [ findInterval(x, seq(min(x),max(x), length.out=colsteps)) ] )  
  #}
}


if(FALSE) color.gradient.ALTERNATIVE <- function(x, colors=c("red","yellow","green")) {
  # Create Color Gradient for a given vector x with given colors.
  #
  # The function creates a color function with colorRampPalette().
  # Then it hands over the number of unique elements of x into this function()().
  # From the result of the function()()[ ] only these elements are picked which are most similar to the values in the sequence min(x) to max(x)
  # If length(unique(x)) is relatively small (<15) it is done in a computation intensive matter in order to to achieve better results.
  # Else it is done with findInterval() which is much faster.
  # Example found in the internet: browseURL("http://stackoverflow.com/questions/18827214/one-colour-gradient-according-to-value-in-column-scatterplot-in-r")
  
  lux <- length(unique(x))
  if(lux<15){
    lx <- length(x)
    choose <- apply(  
      abs( matrix( rep(x,lux),ncol=lx,byrow=TRUE) -
             matrix( rep(seq(min(x),max(x), length.out=lux),lx), ncol=lx ) ) ,
      2, function(x)which(x==min(x))[1] )
    return( colorRampPalette(colors) (lux) [ choose ] )
  } else {
    return( colorRampPalette(colors) (lux) [ findInterval(x, seq(min(x),max(x), length.out=lux)) ] )  
  }
}

#### CONVENIENCE FUNCTIONS ####

find.fun <- function(pattern){
  # This function finds all functions in the workspace that contain a certain pattern.
  allfun <- as.character( lsf.str( envir=environment(find.fun) ) )
  choose <- which(grepl(paste0(pattern,collapse="|"), allfun ))
  return(allfun[choose])
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

read.cb <- function(names=c("col","rowcol","row","no"), ...) {
  # Read tables that are stored in the clipboard (e.g. copied in excel)
  names <- match.arg(names)
  if(names=="row") {
    dat <- read.table("clipboard", sep="\t", header=FALSE, stringsAsFactors=FALSE, ...)
    rownames(dat) <- dat[,1]; dat <- dat[,-1]
    return(dat)
  } else if (names=="col") {
    return( read.table("clipboard", sep="\t", header=TRUE, stringsAsFactors=FALSE, ...) )
  } else if (names=="rowcol") {
    dat <- read.table("clipboard", sep="\t", header=TRUE, stringsAsFactors=FALSE, ...)
    rownames(dat) <- dat[,1]; dat <- dat[,-1]
    return(dat)
  } else {
    return( read.table("clipboard", sep="\t", header=FALSE, stringsAsFactors=FALSE, ...) )
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

#x <- as.data.frame(matrix(1:100, ncol=10))
printzeros <- function(x, zero.sign=".") {
  x[x==0] <- NA
  print(x, na=zero.sign)
}

l <- match.fun(length)
cn <- match.fun(colnames)
rn <- match.fun(rownames)
nc <- match.fun(ncol)
nr <- match.fun(nrow)
su <- function(x) sort(unique(x))
h <- function(x, n=6)  if(length(dim(x))==3) return(x[1:n,,]) else return(head(x,n))

ch <- function(x){
  # Look shortly at the most important properties of a matrix / data.frame
  print(head(x))
  cat("\ncolnames\n")
  print(colnames(x)); cat("\n")
  cat(paste("nrow:\t",nrow(x),"\n"))
  cat(paste("ncol:\t",ncol(x)))
}
####

####
narows <- function(x){
  apply(x,1,function(x)all(is.na(x)))
}
####

nacols <- function(x){
  apply(x,2,function(x)all(is.na(x)))
}
####

minmax <- function(x,na.rm=TRUE){
  c(min(x,na.rm=na.rm),max(x,na.rm=na.rm))
}
###





#### Funktionen, mit denen einfach die Tabellennr. Spaltennr. Zeilennr. etc. aus den Spaltenüberschriften des Merkmals
#### Katalogs rausgelesen werden können.
MKtab <- function(string) {if(is.null(dim(string))) substr(string,1,4) else substr(colnames(string),1,4)}
MKspalte <- function(string) {if(is.null(dim(string))) substr(string,6,9) else substr(colnames(string),6,9)}
MKzeile <- function(string) {if(is.null(dim(string))) substr(string,11,15) else substr(colnames(string),11,15)}

sort.MK <- function(data, order=c("zeile","spalte")){
  order <- match.arg(order)
  cn.data <- colnames(data)
  change_vec <- substr(cn.data,1,1)=="M" & nchar(cn.data)>=4 & !is.na(is.numeric(substr(cn.data,2,4)))
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
  change_vec <- substr(cn.data,1,1)=="M" & nchar(cn.data)>=4 & !is.na(is.numeric(substr(cn.data,2,4)))
  cn.keep <- cn.data[!change_vec]
  cn.change <- cn.data[change_vec]
  if(order=="zeile") {
    return( c(cn.keep, cn.change[ order(MKtab(cn.change), MKzeile(cn.change), MKspalte(cn.change)) ] ) )
  } else {
    return( c(cn.keep, cn.change[ order(MKtab(cn.change), MKspalte(cn.change),MKzeile(cn.change)) ] ) )
  }
}

wait <- function(secs) {
  Sys.sleep(secs)
}

#### CHANGE OBJECT STRUCUTRE ####

if(FALSE){
  data <- as.data.frame(t(array(1:100,c(10,10))))
  data[c(2,3),] <- NA
  data[,c(5,7)] <- NA
  remove.na.rowcols(data)
}
remove.na.rowcols <- function(data){
  return( data[ !apply(data,1,function(x)(all(is.na(x)))) , !apply(data,2,function(x)(all(is.na(x)))) ] )
}
####

if(FALSE){
  data <- as.data.frame(t(array(1:100,c(10,10))))
  change.row.order(data, old=c(2,3), new=c(8,9))
  
  data <- as.data.frame(array(1:100,c(10,10)))
  change.col.order(data, old=c(2,3), new=c(8,9))
}
change.row.order <- function(data,old,new){
  data.new <- data
  data.new[old,] <- data[new,]
  data.new[new,] <- data[old,]
  
  if(!is.null(rownames(data))){
    new.rownames <- rownames(data)
    new.rownames[old] <- rownames(data)[new]
    new.rownames[new] <- rownames(data)[old]
    rownames(data.new) <- new.rownames
  }
  return(data.new)
}
####

change.col.order <- function(data,old,new){
  data.new <- data
  data.new[,old] <- data[,new]
  data.new[,new] <- data[,old]
  
  if(!is.null(colnames(data))){
    new.colnames <- colnames(data)
    new.colnames[old] <- colnames(data)[new]
    new.colnames[new] <- colnames(data)[old]
    colnames(data.new) <- new.colnames
  }
  return(data.new)
}
####


rep.1b1 <- function(vector,times){
  if(length(times)>1) {
    if(length(vector)!=length(times)) stop("If length(times)>1 then condition length(vector)==length(times) must hold.")
    return(unlist(   apply(matrix(c(vector,times),ncol=2),1,function(x)rep(x[1],x[2])) ))
  }
  return(as.vector(   apply(matrix(vector,ncol=1),1,function(x)rep(x,times))   )) 
}
# Performance-Vergleich zwischen mapply und apply. Apply ist deutlich schneller.
# vector <- 1:10e3
# times <- 1:10e3
# system.time(t1 <- mapply(function(vector,times)rep(vector,times), vector, times) )
# system.time(t1 <- apply(matrix(c(vector,times),ncol=2),1,function(x)rep(x[1],x[2])) )


####
rep.rows.1b1 <- function(x, times){
  ncol.x <- ncol(x)
  res <- do.call("rbind", lapply(as.data.frame(t(x)),function(x) matrix(rep(x,times),ncol=ncol.x,byrow=TRUE) ))
  rownames(res) <- rep(rownames(x), times); colnames(res) <- colnames(x)
  return(res)
}
####
rep.rows <- function(x, times){
  ncol.x <- ncol(x)
  res <- do.call("rbind", lapply(as.data.frame(t(x)),function(x) matrix(rep(x,times),ncol=ncol.x,byrow=TRUE) ))
  rownames(res) <- rep(rownames(x), times); colnames(res) <- colnames(x)
  if(nrow(x)>1) newo <- order(rep(1:times, nrow(x))) else newo <- 1:times
  return( res[newo,] )
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
  dat <- list(...)
  
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
    if(length(own.names)!=n.arg) stop("length(own.names) must be equal the number uf rbind arguments")
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
  dat <- list(...)
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

####

switch.list.element <- function(LIST,from,to){
  if(length(from)!=length(to)) stop("length(from) must be equal length(to)")
  if(is.character(from)) {
    from.num <- to.num <- numeric()
    for(i in 1:length(from)) {
      from.num[i] <- which(names(LIST)==from[[i]])
      to.num[i] <- which(names(LIST)==to[[i]])
    }
    from <- from.num; to <- to.num; rm(from.num); rm(to.num)
  }
  list2 <- LIST
  for(i in 1:length(from)){
    list2[[ to[i] ]] <- LIST[[ from[i] ]]
    names(list2)[ to[i] ] <- names(LIST)[ from[i] ]
  }
  return(list2)
} 
#LIST <- list(a=c(12,3),b=c(4,5,6),c=c(9,5,3))
#switch.list.element(LIST,"a","b")
#switch.list.element(LIST,1,3)
####

movecols <- function(x,ncols=+1,fill=NA){
  if(ncols>0){            res <- cbind(rep(fill,nrow(x)),x[,1:(ncol(x)-1),drop=FALSE]);# colnames(res) <- colnames(x)
  } else if(ncols<0) {    res <- cbind(x[,2:ncol(x),drop=FALSE],rep(fill,nrow(x)));# colnames(res) <- colnames(x)
  } else {                res <- x  }
  return(res)
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

if(FALSE){
  x <- array(0, dim=c(5,5,2), dimnames=list(c("asdf1","asdf2","asdf3","asdf4","asdf5"),c("asdf1","asdf2","asdf3","asdf4","asdf5"),c("dim3.1", "dim3.2")))
  #dimnames(x)[[3]] <- NULL
  #dimnames(x) <- NULL
  sep.sign=NA; sep.line=FALSE; keep.colnames=FALSE; keep.dim3names=FALSE
  dim3.to.mat(x, sep.line=TRUE, sep.sign=NA, keep.colnames=TRUE, keep.dim3names=TRUE)
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

if(FALSE) dim3.to.mat_DELETE <- function(x, sep.sign=NA, sep.line=TRUE){
  di <- dim(x)
  res <- apply(x, 2, function(x)rbind(rep( if(sep.line)sep.sign ,ncol(x)), x) )
  rownames(res) <- rep(c(if(sep.line)"" ,rownames(x)), di[3])
  if(sep.line) res <- res[-1,]
  return(res)
}


#### SUMMARIES & MEANS ####

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


summaryna <- function(x,na.rm=TRUE,digits=NULL) {
  if(is.matrix(x)|is.data.frame(x)) { return(apply(x,2,function(x)summaryna(x,na.rm=na.rm)))
  } else if (is.list(x)) { return(sapply(x,function(x)summaryna(x,na.rm=na.rm)))}
  sum <- summary(x,na.rm=na.rm)
  if(length(sum)<7)    sum <- c(sum,"NA's"=0)
  if(!is.null(digits)) sum <- round(sum,digits)
  return(sum)
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

  x <- data; quant=10;digits=2;na.rm=TRUE;margin=2;reverse.quantiles=FALSE
}
summary.long <- function(x,quant=10,digits=2,na.rm=TRUE,margin=2,reverse.quantiles=FALSE,...) {
  # Gives a "long" summary with 11 quantiles from 0 to 1 (default).
  # margin is only used when a dataframe/matrix is given. Then apply() is used.
  # The argument matrixinput is only used inside the function and should not be used
  if(!is.null(dim(x))) {
    if(is.matrix(x))     return( apply(x,2,function(x)summary.long(x,quant=quant,digits=digits,na.rm=TRUE,margin=2,reverse.quantiles=TRUE, ...))  )
    if(is.data.frame(x)) return( as.data.frame( lapply(x,function(x)summary.long(x,quant=quant,digits=digits,na.rm=TRUE,margin=2,reverse.quantiles=TRUE, ...)) ,stringsAsFactors=FALSE) )
  }
  if(is.list(x)) return(lapply(x,function(x)summary.long(x,quant=quant,digits=digits,na.rm=TRUE,margin=2,reverse.quantiles=TRUE, ...)))
  
  if(!is.numeric(x)) x <- rep(0, length(quant+1))
  
  result <- numeric()
  quants <- seq(0,1,length.out=quant+1)
  if(reverse.quantiles) quants <- rev(quants)
  result <- round( quantile(x=x, probs=quants, na.rm=na.rm, ...), digits=digits)
  names(result) <- paste0(round(100*quants,digits=2), "%")
  return(result)
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
  mean.weight(data,weights,index)
  mean.weight(data[,1,drop=FALSE],weights,index)
  mean.weight(data[,2,drop=FALSE],weights,index)
}

mean.weight <- function(data, weights=NULL, index=NULL, digits=NULL, na.rm=TRUE){
  if(!is.list(index)) index <- list(index)
  
  # Im Falle, dass !is.null(dim(data)) folgt eine rekursive Funktionsdefinition!
  if(!is.null(dim(data))) {
    # Interne Funktion um ".1" am Schluss der Namen zu löschen (unschön, da das wegen tapply(...,rep(1,..)) passiert)
    substr.rev <- function(char, start, end, reverse=FALSE) {
      # Reverse substring
      
      if(any(start>end)) stop("All start must be smaller equal end.")
      
      nchar_char <- nchar(char)
      res <- substr(char, nchar_char-end+1, nchar_char-start+1)
      if(reverse) {
        res <- sapply(lapply(strsplit(res, NULL), function(x)rev(x)), function(x)paste(x, collapse=""))
      }
      return(res)
    }
    
    # Wenn !is.null(dim(data))
    # & es keinen oder nur einen Index gibt:
    if(is.null(index) || length(index)==1) {
      if(is.matrix(data)) {
        result <- apply(data, 2, function(x)mean.weight(x, weights, index, digits, na.rm) )
      } else if(is.data.frame(data)) {
        result <- as.data.frame( lapply(data, function(x)mean.weight(x, weights, index, digits, na.rm)) ,stringsAsFactors=FALSE )
      }
      
      # Wenn !is.null(dim(data))
      # & 2 Indexe eingegeben wurden:
    } else if(length(index)==2) {
      res1 <- mean.weight(data[,1], weights, index, na.rm)
      
      # Hier keine Fallunterscheidung zwischen matrix und data.frame einfuegen, sonst funktioniert es nicht!!
      res.prov <- apply(data, 2, function(x)mean.weight(x, weights, index, digits, na.rm) )
      
      res.list <- list()
      su.index1 <- sort(unique(index[[1]]))
      su.index2 <- sort(unique(index[[2]]))                  
      for(i in 1:ncol(res.prov)){
        res.list[[i]] <- matrix(res.prov[,i],nrow=length(su.index1), ncol=length(su.index2))
        dimnames(res.list[[i]]) <- list(su.index1, su.index2)
      }
      names(res.list) <- colnames(data)
      result <- res.list
    } else if(length(index)>2) {
      stop("more than 3 indexes not possible if data is a matrix/data.frame. Please enter data as vector.")
    }
  
    # Falls !is.null(dim(data)) werden am Ende für das Ergebnis "result" noch die Namen verschönert. Das .1 wird gelöscht, falls es wegen tapply() gemacht wurde.
    colnames.result <- colnames(result)
    if(!is.null(colnames.result)) if( all(substr.rev(colnames.result,1,2)==".1") ) colnames(result) <- substr(colnames.result,1,nchar(colnames.result)-2)
    return(result)
  }
  
  # Tatsächliche mean.weight() Funktion.
  # Falls es keine numerische Variable ist (weil z.B. ein durchmischter data.frame eingegeben wird),
  # wird daraus eine 0 gemacht, damit die Funktion trotzdem funktioniert.
  if(! (is.numeric(data)||is.logical(data)) ) data <- rep(NA, length(data))
    
  if(is.null(weights)) weights <- rep(1,length(data))
  if(is.null(index) | is.null(index[[1]])) index <- rep(1,length(data))
  
  
  if(!is.list(index)) {
    length.index <- length(index)
  } else {
    length.index <- sapply(index,function(x)length(x))
    if(any(length.index!=length.index[1])) stop("All vectors in the index have to have the same length!")
  }
  if(!all(length(weights)==length.index)) stop("length(weights)!=length(index)")
  
  sum.variable.weights <- tapply(data*weights,index,function(x)sum(x,na.rm=na.rm))
  sum.weights <- tapply(weights,index,function(x)sum(x,na.rm=na.rm))
  result <- sum.variable.weights/sum.weights
  if(!is.null(digits)) result <- round(result, digits)
  return(result)
}

mean.gb <- mean.GB <- function(...) { # wrapper function
  mean.weight(...)
}
####

meansd <- function(x,na.rm=TRUE) {
  c(Mean=mean(x,na.rm=na.rm),SD=sd(x,na.rm=na.rm))
}
####

mean.geom <- function(x,na.rm=TRUE) {
  if(na.rm==TRUE) x <- x[!is.na(x)]
  result <- (prod(x))^(1/length(x))
  # Wenn das Ergebnis zu gross wird (Inf) müssen die Anfangswerte erst verkleinert werten.
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

gb.diffs <- function(x,cols=list(c("2012","2013"),c("2004","2013")),
                     short.names=list(c("12","13"),c("04","13")), digits=2) {
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


tapply.fixed <- function(X, INDEX, FUN, names.result=NULL, missing.value=NA, sep.sign="_", vector.result=TRUE){
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
    l.INDEX <- sapply(INDEX, function(x)length(x))
    if(any(l.INDEX!=l.INDEX[1])) stop(paste0("All indexes must have the same length! ", paste0(l.INDEX,collapse=" ") ))
    
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
    nres0 <- unlist(dimnames(res0))
    nres1 <- unlist(names.result)
    # Warnung ausgeben, falls nicht alle Ergebnisse ausgegeben werden wegen zu kurzem names.result
    if(any(!nres0%in%nres1))
      warning(paste0("For some entries in X no corresponding entries in names.result were given. The resulting array is incomplete!\n", paste(nres0[!nres0%in%nres1], collapse=" ") ))
    # Aufwaendige Uebertragung nur machen, wenn es wirklich fehlende Eintraege in res0 gibt
    if(length(res0)!=length(nres1) || names(res0)!=nres1){
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
      return(res1)
      # Sonst Original-Ergebnis ausgeben
    } else {
      return(res0)
    }
    
    # Resultate-Berechnung im Falle von Vektor-Ergebnisstruktur
  } else{
    res0 <- tapply(X,INDEX,FUN)
    # Warnung ausgeben, falls nicht alle Ergebnisse ausgegeben werden wegen zu kurzem names.result
    if(any(!names(res0)%in%names.result))
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
      return(res1)
      # Sonst Original-Ergebnis ausgeben
    } else {
      return(res0)
    }
  }
}

#X=round(runif(100,1,15)); names.result=1:5;
#table.fixed(X, names.result=1:5)
#table.fixed(round(runif(100,1,15)), round(runif(100,1,15)), names.result=list(1:20, 5:15), vector.result=FALSE)
table.fixed <- function(..., names.result=NULL, vector.result=TRUE) {
  # This function puts the result of table(X) into a fixed given vector with names=names.result.
  # It is a wrapper function for tapply.fixed().
  # For information on the arguments see tapply.fixed()
  INDEX <- list(...)
  tapply.fixed(X=rep(1,length(INDEX[[1]])), INDEX=INDEX, FUN=function(x)length(x), names.result=names.result, missing.value=0, vector.result=vector.result)  
}


####
tapply.all <- function(X,INDEX,FUN, ..., simplify=TRUE, simplify2array=TRUE){
  # This function calculates tapply, but adds one additional calculation for all observations
  # irrespective of their group.
  
  FUN <- match.fun(FUN)
  X <- c(X,X)
  if(!is.list(INDEX)) INDEX <- list(INDEX)
  if(is.logical(INDEX[[1]])) {
    INDEX[[1]] <- as.numeric(INDEX[[1]])
  } else if(is.character(INDEX[[1]])) {
    INDEX[[1]] <- c(rep("all",length(INDEX[[1]])),INDEX[[1]])
  } else if(is.numeric(INDEX[[1]])) {
    #INDEX[[1]] <- c(rep("all",length(INDEX[[1]])),INDEX[[1]])
    if(min(INDEX[[1]])<=0) group_it <- min(INDEX[[1]])-1 else group_it <- 0
    INDEX[[1]] <- c(rep(group_it,length(INDEX[[1]])),INDEX[[1]])
  } else {
    stop("INDEX[[1]] must be either numeric, character or logical")
  }
  if(length(INDEX)>1) for(i in 2:length(INDEX)) INDEX[[i]] <- rep(INDEX[[i]],2)
  res <- tapply(X,INDEX,FUN, ...,simplify=simplify) # , ...
  if(simplify2array & length(INDEX)==1 & mode(res)=="list") res <- do.call("rbind",res) # für Funktionen, die mehrere Werte ausgeben, wird das Ergebnis statt als Liste als Matrix zurückgegeben
  return( res )
}
####

#X=round(runif(100,1,15)); INDEX=c(rep(1,50), rep(2,20), rep(3,30)); FUN=function(x)mean(x); names.result=1:5; missing.value=NA
#tapply.fixed(X, INDEX, FUN=function(x)mean(x), names.result, missing.value)
if(FALSE) tapply.fixed_OLD.DELETE <- function(X, INDEX, FUN, names.result=sort(unique(INDEX)), missing.value=NA){
  # This function puts the result of tapply(X,INDEX,FUN) into a fixed given vector with names=names.result.
  # This is especially useful if some entries are missing in INDEX but you want them to be displayed as well!
  # Otherwise they would be missing in the result of the tapply() function.
  #
  # Arguments:
  # X, INDEX, FUN: See help page for tapply()
  # names.result = The names of the resulting vector (including all possible 0/NA entries).
  # missing.value = Which value should be put into the resulting vector if there were no entries in INDEX?
  
  if(is.list(INDEX)) stop("This function only works for one INDEX. INDEX must be a vector, no list.")
  if(!is.null(dim(X))) stop("This function only works for vectors! dim(X) must be equal 0!")
  
  res0 <- tapply(X,INDEX,FUN)
  if(any(!names(res0)%in%names.result)) warning("For some entries in X no corresponding entries in names.result were given. The resulting vector is incomplete!")
  if(length(res0)!=length(names.result) || names(res0)!=names.result){
    res1 <- rep(missing.value, length(names.result))
    names(res1) <- names.result
    ind <- match(names(res0),names(res1))
    if(any(is.na(ind))){
      res0 <- res0[!is.na(ind)]
      ind <- ind[!is.na(ind)]
    }
    res1[ ind ] <- res0
    return(res1)
  } else {
    return(res0)
  }
}




#### OTHER ####

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
  result$info <-  "getSolStatusCLP(lp)   # Retrieve solve status of LP
  getObjValCLP(lp)      # Retrieve optimal (minimal/maximal) value of objective function.
  getColPrimCLP(lp)     # Retrieve the primal values of the structural variables (columns) after optimization.
  getColDualCLP(lp)     # Retrieve the dual values of the structural variables (columns) after optimization (reduced costs).
  delProbCLP(lp)        # remove problem object"
  
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
  # N = 46° 02' 38.86"
  # E =  8° 43' 49.80"
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
    Ng, "°", Nm, "'", Ns, "'' N,   ",
    Eg, "°", Em, "'", Es, "'' E"
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
                            point.size= 2+2*scale.extreme(1:n), point.col.in=color.gradient(1:n), cat.html=FALSE )
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
    zu tun hat, einfach aus dem Code löschen.
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
  # Matrix erzeugen, die alle Kennzahlen enthält
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
  
  # Temporäre Datei erstellen
  write.table(text4, "googlemaps_tmp.html", col.names=FALSE, row.names=FALSE, quote=FALSE)
  # Öffnen
  browseURL("googlemaps_tmp.html")
  
  if(!del.tmp.file){ # length(unique(labels))>1 & labels[1]!="" | 
    cat("File is stored in the folder:   ", getwd(),"/\n", sep="")
    cat("                    filename:   googlemaps_tmp.html\n" )
    cat("Use\nfile.remove('googlemaps_tmp.html')\nto delete the file when you don't need it anymore\n")
    
    # Wenn keine Labels gesetzt wurden, macht es keinen Sinn, die Datei länger zu behalten.
    # Es wird 8 Sekunden gewartet, bis der Browser die Datei sicher geöffnet hat. Dann wird die temporäre Datei wieder gelöscht.
  } else {
    t0 <- as.numeric(substr(Sys.time(), 15,16))*60 + as.numeric(substr(Sys.time(), 18,19))
    while(as.numeric(substr(Sys.time(), 15,16))*60 + as.numeric(substr(Sys.time(), 18,19)) - t0 <= 5){
      # Zeit schinden
      a <- 1+1
    }
    file.remove('googlemaps_tmp.html')
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

if(FALSE) list.dirs2 <- function(path=".", pattern=NULL, all.dirs=FALSE,  full.names=FALSE, ignore.case=FALSE) {
  # This function lists all Folders in a directory.
  
  all <- list.files(path, pattern, all.dirs,
                    full.names, recursive=FALSE, ignore.case)
  all[file.info(all)$isdir]
}

####
# pattern="B20"; replace="BH20"; recursive=TRUE;
# gsub.only.dirs("//evdad.admin.ch/AGROSCOPE_OS/2/5/2/1/2/2841/StatSkripte/", pattern="BH20", replace="B20", recursive=TRUE)
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

# gsub.only.files("//evdad.admin.ch/AGROSCOPE_OS/2/5/2/1/2/2841/StatSkripte/", pattern="B20", replace="BH20", recursive=TRUE)
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

# gsub.dirs.and.files("//evdad.admin.ch/AGROSCOPE_OS/2/5/2/1/2/2841/", pattern="B20", replace="BH20", recursive=TRUE)
# gsub.dirs.and.files("//evdad.admin.ch/AGROSCOPE_OS/2/5/2/1/2/2841/", pattern="BH20", replace="B20", recursive=TRUE)
# gsub.dirs.and.files("//evdad.admin.ch/AGROSCOPE_OS/2/5/2/1/2/2841/StatSkripte", pattern="BH20", replace="B20", recursive=TRUE)
# gsub.dirs.and.files("//evdad.admin.ch/AGROSCOPE_OS/2/5/2/1/2/2841/StatSkripte", pattern="B20", replace="BH20", recursive=TRUE)

gsub.dirs.and.files <- function(path=".", pattern, replace, recursive=FALSE) {
  # This function renames all directories and all files within a directory (and all files in subdirectories if recursive=TRUE).
  # pattern & replace shall be used as is used in the gsub() function.
  gsub.only.dirs(path, pattern, replace, recursive)
  gsub.only.files(path, pattern, replace, recursive)
}

#
rename.files <- function(path="."){
  name1 <- list.files("//evdad.admin.ch/AGROSCOPE_OS/2/5/2/1/2/2841/", pattern="BH20", full.names=TRUE, recursive=TRUE)
  name2 <- gsub("BH20", "B20", name1)
  file.rename(from=name1, to=name2)
  
  file.rename(from="//evdad.admin.ch/AGROSCOPE_OS/2/5/2/1/2/2841//SekDaten/Betr_B/AWP/BH2014/", to="//evdad.admin.ch/AGROSCOPE_OS/2/5/2/1/2/2841//SekDaten/Betr_B/AWP/B2014/")
  
  file.rename(from="./Betr_B/AWP/BH2014/001_AuswahlplanRef_Formel_Seite2.pdf", to="./Betr_B/AWP/B2014/001_AuswahlplanRef_Formel_Seite2.pdf")
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
  # Replace first/last value of vecor with -Inf/Inf and stratify by using the function group.by.fix.scale()
  
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
  grouping <- group.by.fix.scale(x=x, selection.levels=borders)
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
mclapply.own <- function(X, FUN, mc.cores=getOption("mc.cores", 8), type=c("PSOCK", "FORK", "MPI")){
  # The function depends on following packages
  # parallel, snow, Rmpi
  if(!grepl("return",paste0(deparse(FUN),collapse="")))
    stop(paste0("FUN must explicitly return the result by using return(). The entered function looks like this\n",
                "    ",paste0(deparse(FUN),collapse=""), "\n",
                "  But it should look like this:\n",
                "    function(x)return(x*2)"))
  
  type <- match.arg(type)
  
  library(parallel)
  cl <- makeCluster(mc.cores, type=type)
  
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
  require(parallel)
  
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
        require(yy , character.only=TRUE)})
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

mahalanobis.outliers <- function(data,p.val=0.025,method=c("quantile","chisq"),na.action=c("median","mean","remove")){
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
  maha <- mahalanobis(data,colMeans(data),cov(data))
  if(method=="quantile") {
    outliers <- maha > quantile(maha,1-p.val)
  } else if(method=="chisq") {
    p.chisq <- pchisq(q=maha,df=ncol(data)) # df ist richtig. Laut ETH Folien zu "mutivariate outliers"
    outliers <- (1-p.chisq) < p.val
    warning("Attention! By choosing method='chisq' you assume that all variables follow a normal distribution.")
  }
  return(outliers)
}

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
    semi.result <- sds==0[]
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
if(FALSE){
  x <- cbind(ID=as.character(1:25), BZ=LETTERS[1:25], 1:25)
  char.cols.to.num(x)
  summary(char.cols.to.num(x))
}
char.cols.to.num <- function(x, checkrows=NULL, stringsAsFactors=FALSE){
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
  
  if(is.null(checkrows) || checkrows>nrow(x)) checkrows <- nrow(x)
  
  rn <- rownames(x)
  cn <- colnames(x)
  
  res <- as.data.frame(
    lapply(
      as.data.frame(x, stringsAsFactors=stringsAsFactors),
      function(x) if( is( tryCatch(as.numeric(x[1:checkrows]),error=function(e)e,warning=function(w)w), "warning") ) return(x) else return(as.numeric(x))
    ),
    stringsAsFactors=stringsAsFactors)
  
  rownames(res) <- rn
  colnames(res) <- cn
  return(res)
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
  if(length(x)==0) return(x.orig) # Wenn alles NA Werte waren, sollen NA Werte zurückgegeben werden.
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
  stopifnot(class(x)=="numeric")
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
equal.n.decimals <- function(x, add=0, add.dot=TRUE, margin=2) {
  if(!is.null(dim(x))) {
    if(is.matrix(x)) return(apply(x,margin,function(x)equal.n.decimals(x=x, add=add, add.dot=add.dot, margin=margin)))
    if(is.data.frame(x)) return( as.data.frame( lapply(x,function(x)equal.n.decimals(x=x, add=add, add.dot=add.dot, margin=margin)) ,stringsAsFactors=FALSE) )
  }
  if(is.list(x)) return(lapply(x,function(x)equal.n.decimals(x=x, add=add, add.dot=add.dot, margin=margin)))
  
  # Funktioniert auch fuer character Argumente!
  #if(!is.numeric(x)) return(x)
  
  n.char.x <- nchar(x)
  x2 <- strsplit(as.character(x), "")
  dotplace <- lapply(x2, function(x) which(x == "."))
  dotplace <- unlist(lapply(dotplace, function(x) if(length(x)==0) 0 else x))
  n.decimals.x <- n.char.x-dotplace
  n.decimals.x[n.decimals.x==n.char.x] <- 0
  n.add <- max(n.decimals.x) - n.decimals.x
  
  x.new <- character()
  sort.unique.n.add <- sort(unique(n.add))
  sort.unique.n.add <- sort.unique.n.add[sort.unique.n.add!=0]
  if(add.dot) {
    for(i in sort.unique.n.add) x.new[n.add==i] <- paste0(x[n.add==i], "." ,paste0(rep(add,i),collapse="") )
  } else {
    for(i in sort.unique.n.add) x.new[n.add==i] <- paste0(x[n.add==i], " " ,paste0(rep(add,i),collapse="") )
  }
  x.new[n.add==0] <- x[n.add==0]
  names(x.new) <- names(x)
  return(x.new)  
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
    if(is.matrix(x)) return(apply(x,margin,function(x)equal.length(x=x, add=add, where=where, margin=margin)))
    if(is.data.frame(x)) return( as.data.frame( lapply(x,function(x)equal.length(x=x, add=add, where=where, margin=margin)) ,stringsAsFactors=FALSE) )
  }
  if(is.list(x)) return(lapply(x,function(x)equal.length(x=x, add=add, where=where, margin=margin)))
  
  where <- match.arg(where)
  nchar.x <- nchar(x)
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
  df1=as.data.frame(matrix(cn_cost)); df2=as.data.frame(matrix(colinfo[,2])); id1=cn_cost; id2=colinfo[,1]    
}


####
match.df.by.id <- function(df1,df2,id1,id2,keep.no.matches=TRUE){
  # This function matches two data frames by id.
  # If wished (by default) also no matches are kept.
  
  if(any( colnames(df1)%in%c("id1","id2") )) stop("There must be no colnames(df1) equal 'id1' or 'id2'")
  if(any( colnames(df2)%in%c("id1","id2") )) stop("There must be no colnames(df2) equal 'id1' or 'id2'")
  
  if(is.null(dim(df1))|is.null(dim(df2))) stop("df1 and df2 must be data.frame or matrix")
  if(nrow(df1)!=length(id1)) stop("length(id1) must be equal nrow(df1)")
  if(nrow(df2)!=length(id2)) stop("length(id2) must be equal nrow(df2)")
  
  # Check for NA values in the IDs
  is.na.id1 <- is.na(id1)
  is.na.id2 <- is.na(id2)
  
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
    if(any(id1.double)){
      stop("There are duplicated IDs in id1")
      # Veraltet. Wird nicht mehr zurueckgegeben.
      cat("There are duplicated IDs in id1. See function output and return one of each pair.\n")
      prov.return$which.id1.duplicated <- which(id1%in%id1[id1.double])
      prov.return$id1 <- id1[ prov.return$which.id1.duplicated ]
      prov.return$df1 <- data.frame(id1=id1[prov.return$which.id1.duplicated],df1[prov.return$which.id1.duplicated,,drop=FALSE])
    }
    if(any(id2.double)){
      stop("There are duplicated IDs in id2")
      # Veraltet. Wird nicht mehr zurueckgegeben.
      cat("There are duplicated IDs in id2. See function output and return one of each pair.\n")
      prov.return$which.id2.duplicated <- which(id2%in%id2[id2.double])
      prov.return$id2 <- id2[ prov.return$which.id2.duplicated ]
      prov.return$df2 <- data.frame(id2=id2[prov.return$which.id2.duplicated],df2[prov.return$which.id2.duplicated,,drop=FALSE])
    }
    class(prov.return) <- "match.df.by.id.prov"
    return(prov.return)
  }

  
  df1.gt.df2 <- nrow(df1)>nrow(df2)
  if(!df1.gt.df2){
    
    newo <- match(id1,id2)
    is.na.newo <- is.na(newo)
    id2.newo <- id2[newo]
    df2.newo <- df2[newo,,drop=FALSE]
    result <- data.frame(id1, df1, id2=id2.newo, df2.newo)
    
    if(keep.no.matches){
      id2.in.id2.new <- id2%in%result[,"id2"]
      if(any(!id2.in.id2.new)){
        add.df2 <- data.frame(id2=id2[!id2.in.id2.new],df2[!id2.in.id2.new,,drop=FALSE])
        add.df1 <- matrix(NA,nrow=nrow(add.df2),ncol=ncol(df1)+1)
        add.df <- data.frame(add.df1,add.df2)
        colnames(add.df) <- colnames(result)
        result <- rbind(result,add.df)
      }
    } else {
      result <- result[!is.na(result[,"id2"]),,drop=FALSE]
    }
    
    
  } else { #if(df1.gt.df2)
    if(FALSE){
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
    }
    ### Block reinkopieren - Anfang ###
    newo <- match(id2,id1)
    is.na.newo <- is.na(newo)
    id1.newo <- id1[newo]
    df1.newo <- df1[newo,,drop=FALSE]
    result <- data.frame(id2, df2, id1=id1.newo, df1.newo)
    
    if(keep.no.matches){
      id1.in.id1.new <- id1%in%result[,"id1"]
      if(any(!id1.in.id1.new)){
        add.df1 <- data.frame(id1=id1[!id1.in.id1.new],df1[!id1.in.id1.new,,drop=FALSE])
        add.df2 <- matrix(NA,nrow=nrow(add.df1),ncol=ncol(df2)+1)
        add.df <- data.frame(add.df2,add.df1)
        colnames(add.df) <- colnames(result)
        result <- rbind(result,add.df)
      }
    } else {
      result <- result[!is.na(result[,"id1"]),,drop=FALSE]
    }
    ### Block reinkopieren - Ende ###
    
    # Schliesslich Reihenfolge zuruecktauschen:
    result <- result[,c( (1+ncol(df2)+1)  #id1
                         ,(1+ncol(df2)+1+1):ncol(result) #df1
                         ,1 #id2                  
                         ,2:(1+ncol(df2))) #df2
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
    colnames(pseudo.df2) <- colnames(df2)
    result <- rbind(result,  data.frame(id1=NA, id2=NA, df1.na, pseudo.df2) )
  }
  if(any(is.na.id2) & keep.no.matches){
    pseudo.df1 <- as.data.frame(matrix(NA, nrow=nrow(df2.na), ncol=ncol(result)-2-ncol(df2.na)))
    colnames(pseudo.df1) <- colnames(df1)
    result <- rbind(result,  data.frame(id1=NA, id2=NA, pseudo.df1, df2.na) )
  }
                       
  return(result)
}    
####
print.match.df.by.id.prov <- function(object){
  object$df1 <- NULL
  object$df2 <- NULL
  class(object) <- "list"
  print(object)
  invisible(object)
}

if(FALSE) match.df.by.id.DELETE <- function(df1,df2,id1,id2,keep.no.matches=TRUE){
  # This function matches two data frames by id.
  # If wished (by default) also no matches are kept.
  
  if(any( colnames(df1)%in%c("id1","id2") )) stop("There must be no colnames(df1) equal 'id1' or 'id2'")
  if(any( colnames(df2)%in%c("id1","id2") )) stop("There must be no colnames(df2) equal 'id1' or 'id2'")
  
  if(is.null(dim(df1))|is.null(dim(df2))) stop("df1 and df2 must be data.frame or matrix")
  if(nrow(df1)!=length(id1)) stop("length(id1) must be equal nrow(df1)")
  if(nrow(df2)!=length(id2)) stop("length(id2) must be equal nrow(df2)")
  
  is.na.id1 <- is.na(id1)
  is.na.id2 <- is.na(id2)
  id1 <- id1[!is.na.id1]
  df1 <- df1[!is.na.id1,,drop=FALSE]
  id2 <- id2[!is.na.id2]
  df2 <- df2[!is.na.id2,,drop=FALSE]
  
  id1.double <- duplicated(id1)
  id2.double <- duplicated(id2)
  if(any(id1.double)|any(id2.double)){
    prov.return <- list()
    if(any(id1.double)){
      cat("There are duplicated IDs in id1. See function output and return one of each pair.\n")
      
      prov.return$which.id1.duplicated <- which(id1%in%id1[id1.double])
      prov.return$id1 <- id1[ prov.return$which.id1.duplicated ]
      prov.return$df1 <- cbind(id1=id1[prov.return$which.id1.duplicated],df1[prov.return$which.id1.duplicated,,drop=FALSE])
    }
    if(any(id2.double)){
      cat("There are duplicated IDs in id2. See function output and return one of each pair.\n")
      prov.return$which.id2.duplicated <- which(id2%in%id2[id2.double])
      prov.return$id2 <- id2[ prov.return$which.id2.duplicated ]
      prov.return$df2 <- cbind(id2=id2[prov.return$which.id2.duplicated],df2[prov.return$which.id2.duplicated,,drop=FALSE])
    }
    return(prov.return)
  }
  cn.df1.orig <- colnames(df1)
  cn.df2.orig <- colnames(df2)
  
  
  df1.gt.df2 <- nrow(df1)>nrow(df2)
  df2.gt.df1 <- nrow(df2)>nrow(df1)
  if(df1.gt.df2) {
    id1.orig <- id1
    id2.orig <- id2
    df1.orig <- df1
    df2.orig <- df2
    
    id1 <- id2.orig ;  rm(id2.orig)
    id2 <- id1.orig ;  rm(id1.orig)
    df1 <- df2.orig ;  rm(df2.orig)
    df2 <- df1.orig ;  rm(df1.orig)    
  }
  
  newo <- match(id1,id2)
  is.na.newo <- is.na(newo)
  id2.newo <- id2[newo]
  df2.newo <- df2[newo,,drop=FALSE]
  result <- cbind(id1, df1, id2=id2.newo, df2.newo)
  
  if(keep.no.matches){
    id2.in.id2.new <- id2%in%result[,"id2"]
    if(any(!id2.in.id2.new)){
      add.df2 <- cbind(id2=id2[!id2.in.id2.new],df2[!id2.in.id2.new,,drop=FALSE])
      add.df1 <- matrix(NA,nrow=nrow(add.df2),ncol=ncol(df1)+1)
      add.df <- cbind(add.df1,add.df2)
      colnames(add.df) <- colnames(result)
      result <- rbind(result,add.df)
    }
  } else {
    result <- result[!is.na(result[,"id2"]),,drop=FALSE]
  }
  
  result <- result[,c( which(colnames(result)=="id1"),
                       which(colnames(result)=="id2"),
                       which(!colnames(result)%in%c("id1","id2"))
  ) ]
  #result <- result[,c("id1", "id2", cn.df1.orig, cn.df2.orig)]
  
  # Kosmetik:
  # Wenn df1 grösser war als df2 ist die Reihenfolge der Spalten vertauscht. Dies wird wieder rückgängig gemacht.
  if(df1.gt.df2) {
    # IDs wieder umbenennen.
    which1 <- colnames(result)=="id1"
    which2 <- colnames(result)=="id2"
    colnames(result)[which1|which2] <- c("ayxcvbnoiwerfnasdferxcvweyxcver1", "ayxcvbnoiwerfnasdferxcvweyxcver2")
    colnames(result)[which1] <- "id2"
    colnames(result)[which2] <- "id1"
    
    # Angefuegte Spalten des 1. und 2. data.frame identifizieren & wieder rück-tauschen.
    which1 <- (2+2):(2+ncol(df2))
    which2 <- (2+ncol(df2)+1):(2+ncol(df2)+ncol(df1))
    
    res2 <- data.frame(id1=result$id1, id2result$id2, result[,which1], result[,which2])
    result <- cbind(result[,"id1"], result[,which1], result[,"id2"], result[,which2])
    #result2 <- result
    #result2[,which1] <- result[,which2]
    #result2[,which2] <- result[,which1]
    #colnames(result2)[which1] <- colnames(result2)[which2]
    #colnames(result2)[which2] <- colnames(result2)[which1]
    #
    #result <- result2; rm(result2)
    
    # Erneut id1 an den Anfang stellen.
    result <- result[,c( which(colnames(result)=="id1"),
                         which(colnames(result)=="id2"),
                         which(!colnames(result)%in%c("id1","id2"))
    ) ]
  }
  
  return(result)
}    

balanced.panel <- function(id, year, YEAR, output=c("logical","ID")){
  # id: Vector of IDs
  # year: Vector of year (same length as ID)
  # YEAR: Years that should be selected
  # output: Logical vector or IDs?
  output <- match.arg(output)
  mode.id <- mode(id) # Save mode of id for later output
  IDs <- list()
  for(i in 1:length(YEAR)){
    IDs[[i]] <- id[ year%in%YEAR[i] ]
  }
  IDs <- do.call("c",IDs)
  table.IDs <- table(IDs)
  if(any(table.IDs>length(YEAR))) {
    warning("The following observations occur several times in several years! Not able to create balanced panel.\n", call. = FALSE, immediate.=TRUE)
    return.error <- names(table.IDs)[table.IDs>length(YEAR)]
    mode(return.error) <- mode.id
    return(return.error)
  }
  IDs.final <- names(table.IDs)[table.IDs==length(YEAR)]
  mode(IDs.final) <- mode.id  # Set back the mode to original value (instead of character from names(table())... )
  if(output=="logical") {
    return( id%in%IDs.final & year%in%YEAR ) 
  } else {
    warning("This output only serves to show which IDs are in all years. It could however be, that they are in other years too. E.g. you choose YEAR=c(0,1,2), some IDs could be in all years c(0,1,2) but also in year 3. If you want to filter only the relevant IDs AND years choose output='logical'")
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
  x <- round( runif(100)*100 )
  id <- rep(1:50, 2)
  year <- rep.1b1(c(0,1),50)
  YEAR=sort(unique(year)); baseyear=min(YEAR); geometric=FALSE; absolute.diff=TRUE; filter=FALSE; filter.level=c(1/3,3); return.N=FALSE; 
  # Debugging
  mean(x[year==1]) - mean(x[year==0])
  mean(x[year==1]) / mean(x[year==0])
  balanced.panel.development(x,id,year,YEAR=sort(unique(year)),baseyear=min(YEAR),geometric=FALSE,absolute.diff=FALSE,filter=FALSE,filter.level=c(1/3,3),return.N=FALSE)
  mean.geom(x[year==1]) - mean.geom(x[year==0])
  mean.geom(x[year==1]) / mean.geom(x[year==0])
  balanced.panel.development(x,id,year,YEAR=sort(unique(year)),baseyear=min(YEAR),geometric=TRUE,absolute.diff=TRUE,filter=FALSE,filter.level=c(1/3,3),return.N=FALSE)
}
####
balanced.panel.development <- function(x,id,year,YEAR=sort(unique(year)),baseyear=min(YEAR),geometric=TRUE,absolute.diff=FALSE,filter=TRUE,filter.level=c(1/3,3),return.N=FALSE){
  # This function calculates an index of an unbalanced time series.
  # This can be useful if you want to follow the delevoptment of yields but the time horizon
  # is so long that a balanced panel over the whole period has 0 observations.
  # The function calculates relative changes of every pair of year (which builds some kind of 2-year balanced panel).
  # The changes from year to year are then multiplied over the hole period ( geometric=TRUE ) or summed up ( geometric=FALSE ).
  # x:            data like e.g. yield
  # id:           Vector of IDs
  # year:         Vector of year (same length as ID)
  # YEAR:         Years that should be selected
  # baseyear:     The baseyear in which the index has value 1
  # geometric:    See description above
  # filter:       Should extreme changes (that are probalby not realistic) be filtered out?
  # filter.level: If yes, which change-factor is the threshold to filter out c(upper,lower)
  # return.N:     Should the number of observations in each year be calculated instead of the index?
  
  # old function name: unbalanced.index <- function
  if(filter) warning("You have chosen filter=TRUE which excludes extreme values from analysis.")
  if(geometric & absolute.diff) warning("The combination of mean.geom and absoulte difference is rather unusual!")
  
  if(is.data.frame(x) | is.list(x)) return( sapply(x,function(x)balanced.panel.development(x=x,id=id,year=year,YEAR=YEAR,baseyear=baseyear, geometric=geometric, absolute.diff=absolute.diff, filter=filter,filter.level=c(1/3,3),return.N=return.N)) )
  if(is.matrix(x)) return( apply(x,2,function(x)balanced.panel.development(x=x,id=id,year=year,YEAR=YEAR,baseyear=baseyear, geometric=geometric, absolute.diff=absolute.diff, filter=filter,filter.level=c(1/3,3),return.N=return.N)) )
  if(length(x)!=length(id))   stop("length(x)!=length(id)")
  if(length(x)!=length(year)) stop("length(x)!=length(year)")
  if(!baseyear%in%YEAR) stop("baseyear must be in the range of YEAR")
  if(any(!YEAR%in%year)) warning("the years ", YEAR[!YEAR%in%year]," are used in YEAR but do not exist in year")
  
  YEAR <- sort(YEAR)
  d <- list()
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
    if(geometric){
      # Calculate the relative differences between the years.
      # Important: The values have to be ordered such that always the same observations are compared
      d[[i]] <- x[year2][order(id[year2])]   /   x[year1][order(id[year1])]
      if(filter) d[[i]][ d[[i]]<filter.level[1] | d[[i]]>filter.level[2] ] <- NA
      # Calculate the number of observations that were used to build the difference
      if(return.N) N[i+1] <- sum(!is.na(d[[i]]))
    } else {
      # Calculate the absolute differences between the years.
      d[[i]] <- x[year2][order(id[year2])]   -   x[year1][order(id[year1])]
      if(filter) { # The filtering is always done with relative differences.
        drel <- x[year2]/x[year1]
        d[[i]][ drel<filter.level[1] | drel>filter.level[2] ] <- NA
      }
      # Calculate the number of observations that were used to build the difference
      if(return.N) N[i+1] <- sum(!is.na(d[[i]]))
    }
  }
  # END OF THE LOOP
  
  if(return.N) return(N)
  
  if(geometric){
    # Der Wert im 1. Jahr ist der Ausgangswert. Also 1.
    d <- c(list(1),d)
    d.means <- unlist( lapply(d,function(x)mean.geom(x,na.rm=TRUE)) )
    # Bisher wird jeweils die Veränderung zum Vorjahr wiedergegeben.
    # Nun müssen die Werte noch miteinander multipliziert werden, damit sich der Verlauf von Anfang an ergibt.
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
    d <- c(list(0),d)
    d.means <- unlist( lapply(d,function(x)mean(x,na.rm=TRUE)) )
    
    # Bisher wird jeweils die Veränderung zum Vorjahr wiedergegeben.
    # Nun müssen die Werte noch addiert werden, damit sich der Verlauf von Anfang an ergibt.
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
    mean.year1 <- mean(x[year1],na.rm=TRUE)
    # Der Index wird relativiert durch den Mean des ersten Jahres.
    index <- (index+mean.year1)
    
    # Debugging
    # mean(x[year==2012])
    # mean(x[year==2013])
    
    # Der Inex wird im Jahr 1 = 0 gesetzt. Dann Der Mittelwert im Index-Jahr subtrahiert, sodass dort der 0-Punkt ensteht.
    if(absolute.diff){
      index <- index-mean.year1
      index <- index-index[names(index)==baseyear]
      # Der Inex wird im Jahr 1 = 1 gesetzt. Dann alle Zahlen durch den Index im Index-Jahr dividiert, sodass dort der 1-Punkt ensteht.
    } else {
      index <- index/mean.year1
      index <- index/index[names(index)==baseyear]
    }
  } # End if
  
  if(any(is.na(index)) & geometric) cat("Note that the geometric mean can only be calculated if all numbers are positive.\n")
  return(index)
}
# Überprüfung der Funktion #
if(FALSE){
  x <- dat[,"yi_Weizen"]
  id <- dat[,"_ID"]
  year <- dat[,"_Jahr"]
  YEAR <- 2003:2012
  baseyear <- 2003
  filter=FALSE
  filter.level=c(1/3,3)
  geometric=FALSE
  return.N <- TRUE
  
  x <- c(rnorm(10,100,10),rnorm(10,100,10),rnorm(10,100,10),rnorm(10,100,10))
  id <- rep(1:10,4)
  year <- rep.1b1(2001:2004,10)
  YEAR <- 2001:2004
  baseyear <- 2001
  x[c(1,5,14,24,29,36,37)] <- NA
  filter=FALSE
  filter.level=c(1/3,3)
  geometric=FALSE
  return.N <- TRUE
  
  mg <- tapply(x,year,function(x)mean.geom(x)); mg
  m <- tapply(x,year,function(x)mean(x)); m
  # Stimmen die nächsten Zahlen überein? Wenn ja, funktioniert die Funktion richtig.
  # --> Es gibt Abweichungen, sobald NAs in den Daten sind. Bei einem balancierten Panel sollte es aber keine Abweichungen geben!
  unbalanced.index(x,id,year,YEAR,baseyear,filter=FALSE)
  mg/mg[1]
}

####
#x <- rnorm(100); selection.levels <- 0.1; method=c("<= x <", "< x <=")[1]; include.min.max=TRUE; give.names=TRUE
#group.by.fix.scale(x,c(0),c("<= x <", "< x <="),FALSE)
group.by.fix.scale <- function(x, selection.levels, method=c("<= x <", "< x <="), include.min.max=FALSE, give.names=FALSE, names.sep="-"){
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
      if(give.names) names(grouping)[ x >= selection.levels[i] & x < selection.levels[i+1] ] <- paste(zapsmall(c(selection.levels[i],selection.levels[i+1])),collapse=names.sep)
    }
    if(include.min.max){
      grouping[ x >= selection.levels[length.selection.levels-1] & x <= selection.levels[length.selection.levels] ] <- length.selection.levels-1
      if(give.names) names(grouping)[ x >= selection.levels[length.selection.levels-1] & x <= selection.levels[length.selection.levels] ] <- paste(zapsmall(c(selection.levels[length.selection.levels-1],selection.levels[length.selection.levels])),collapse=names.sep)
    } else {
      grouping[ x >= selection.levels[length.selection.levels-1] & x < selection.levels[length.selection.levels] ] <- length.selection.levels-1
      if(give.names) names(grouping)[ x >= selection.levels[length.selection.levels-1] & x < selection.levels[length.selection.levels] ] <- paste(zapsmall(c(selection.levels[length.selection.levels-1],selection.levels[length.selection.levels])),collapse=names.sep)
    }
  } else  if(method=="< x <=")  {
    if(include.min.max){
      grouping[ x >= selection.levels[1] & x <= selection.levels[2] ] <- 1
      if(give.names) names(grouping)[ x >= selection.levels[1] & x <= selection.levels[2] ] <- paste(zapsmall(c(selection.levels[1],selection.levels[2])),collapse=names.sep)
    } else {
      grouping[ x > selection.levels[1] & x <= selection.levels[2] ] <- 1
      if(give.names) names(grouping)[ x > selection.levels[1] & x <= selection.levels[2] ] <- paste(zapsmall(c(selection.levels[1],selection.levels[2])),collapse=names.sep)
    }
    for(i in 2:(length(selection.levels-1)))  {
      grouping[ x > selection.levels[i] & x <= selection.levels[i+1] ] <- i
      if(give.names) names(grouping)[  x > selection.levels[i] & x <= selection.levels[i+1] ] <- paste(zapsmall(c(selection.levels[i],selection.levels[i+1])),collapse=names.sep)
    }
  }
  
  if(any(is.na(grouping))) warning("NAs produced")
  return(grouping)
}

####

group.by.quantiles <- function(x, selection.levels=seq(0,1,0.1), method=c("<= x <", "< x <="), include.min.max=TRUE, weights=NULL){
  # Groupy data by quantiles.
  # This is a wrapper for group.by.fixed scale with slightly altered interface.
  
  #x <- rnorm(1000);  selection.levels <- c(0, 0.25, 0.5, 0.75, 1); method=c("<= x <"); include.min.max=TRUE; weights=rnorm(1000)
  #group.by.quartiles(x=x, weights=weights);
  method <- match.arg(method)
  selection.levels <- sort(selection.levels)
  selection.levels.rel <- selection.levels; rm(selection.levels)
  if(any(selection.levels.rel<0) | any(selection.levels.rel>1)) stop("choose 0 >= selection.level >= 1")
  if(length(selection.levels.rel)==1) {
    include.min.max <- TRUE
    selection.levels.rel <- c(0, selection.levels.rel, 1)
  }
  
  if(is.null(weights))  {
    selection.levels.abs <- quantile(x,selection.levels.rel)
  } else {
    if(length(weights)!=length(x)) stop("length(x) must be equal length(weights)")
    require(Hmisc)
    selection.levels.abs <- wtd.quantile(x=x, weights=weights, probs=selection.levels.rel)
    # Vergleich: quantile(x=x, probs=selection.levels.rel)
    if(min(selection.levels.rel)==0) selection.levels.abs[which.min(selection.levels.rel)] <- min(x)-1
    if(max(selection.levels.rel)==1) selection.levels.abs[which.max(selection.levels.rel)] <- max(x)+1
  }
  
  grouping <- group.by.fix.scale(x=x, selection.levels=selection.levels.abs, method=method, include.min.max=include.min.max)
  return(grouping)
}
#group.by.quartiles(x)
group.by.quartiles <- function(x, ...){
  group.by.quantiles(x=x, selection.levels=c(0, 0.25, 0.5, 0.75, 1), ...)
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
#search <- c(1,2,3); replace <- c("a","b","c")
replace.values <- function(search, replace, x) {
  # This function replaces all elements in argument search with the corresponding elements in argument replace.
  # Replacement is done in x.
  if(length(search)!=length(replace)) stop("length(search) must be equal length(replace)")
  # Delete search & replace entries that are not in x
  keep <- search%in%x
  search <- search[keep]
  replace <- replace[keep]
  # Prepare result vector and replace values.
  y <- vector(mode(x))
  allind <- rep(FALSE, length(x))
  for(i in 1:length(search)) {
    ind1 <- x==search[i]
    y[ind1] <- replace[i]
    allind <- allind | ind1
  }
  y[!allind] <- x[!allind]
  return(y)
}

#### CSV MANIPULATION & READ IN####
# x <- matrix(1:10); nrowmax=10000; ncolmax=10000; folder=NULL; view(x)
view <- function(x, names=c("col","rowcol","row","no"), nrows=10000, ncols=1000, folder=NULL, quote=FALSE, na="NA", ...){
  # This function creates a CSV file from a data.frame/matrix and opens it with the default CSV-opening-program
  # of the computer.
  #
  # x = data.frame/matrix
  # nrows = maximum number of rows    to be saved (for higher speed with large datasets)
  #         if n=-1, all rows will be displayed.-> see also the help for read.table()
  # ncols = maximum number of columns to be saved (for higher speed with large datasets)
  # folder = directory, where the temporary file should be saved.
  #          If NULL an accessible folder in C:/Users/.../Documents will be created automatically.
  # quote = should quotes be written into the csv File? -> see also the help for write.table()
  # na = how should NA values be displayed in the csv File? -> see also the help for write.table()
  
  names <- match.arg(names)
  if(is.null(dim(x))) {
    x <- as.matrix(x)
  }
  if(is.null(colnames(x))) colnames(x) <- "x"
  
  if(nrows<0) nrows <- nrow(x)
  if(ncols<0) ncols <- ncol(x)
  # Shrink data.frame such that it can be saved & viewed faster.
  nrows <- min(nrow(x), nrows)
  if(nrows!=nrow(x)) x <- x[1:nrows,,drop=FALSE]
  ncols <- min(ncol(x), ncols)
  if(ncols!=ncol(x)) x <- x[,1:ncols,drop=FALSE]
  
  
  # Define paths
  # If is.null(folder), wird versucht, in den Unterordnern von C:/Users/.../Documents/ ein File zu erzeugen.
  # Wenn es gelingt, wird der erstmoegliche Folder ausgewaehlt, um nachher darin das temporaere CSV-File abzuspeichern.
  if(is.null(folder)) {
    dirs <- list.dirs(path="C:/Users/", recursive=FALSE)
    dirs <- paste0(dirs,"/Documents/")
    for(i in 1:length(dirs)){
      # Break loop if file was created successfully.
      if(! is( tryCatch( write.table( matrix(1), paste0(dirs[i],"wucklanpqal01jk.csvxyz")) ,
                         error=function(e)e,warning=function(w)w), "warning") ) break
    }
    # Remove check-file & set folder.
    file.remove( paste0(dirs[i],"wucklanpqal01jk.csvxyz") )
    folder <- paste0( dirs[i], "Rview_tmp" )
    suppressWarnings( dir.create(folder) )
  }  
  
  # Wenn am Schluss des Pfades kein "/" angefuegt wurde, wird dies gemacht:
  if( !substr(folder,nchar(folder),nchar(folder))%in%c("/","\\") ) folder <- paste0(folder, "/")
  pfad0 <- folder
  name <- "Rview_tmp"
  nr <- "01"
  csv <- ".csv"
  
  # Check if there are existing files in the folder
  fil <- list.files(pfad0)
  # If there are no files in the folder, use the default save path.
  if(length(fil)==0){
    pfad1 <- paste0(pfad0, name, nr, csv)
  } else {
    # Remove all files in the folder (if possible)
    fil <- paste0(pfad0, fil)
    suppressWarnings( try( file.remove( fil )  , silent=TRUE) )
    fil <- list.files(pfad0)
    # If there are no files anymore use the default save path.
    if( length(fil)==0 ) {
      pfad1 <- paste0(pfad0, name, nr, csv)
    } else {
      # If there are sill files, read out the number of the newest file (with the highest number)
      ncharfil <- nchar(fil)
      mx <- max( as.numeric( substr(fil,ncharfil-5,ncharfil-4) ) )
      # Add 1 to the number of the file
      mxpl1 <- as.character( mx+1 )
      if(nchar(mxpl1)==1) mxpl1 <- paste0("0",mxpl1)
      # Create a new path
      pfad1 <- paste0(pfad0, name, mxpl1, csv)
    }
  }
  
  # Write CSV file & open.
  if(names=="row") {
    # If the first cell of the file is named "ID" Microsoft Excel warns that a SYLK file is opened. Therefore it is renamed.
    if(rownames(x)[1]=="ID") rownames(x)[1] <- "lD"
    write.table(x, file=pfad1, sep = ";", col.names=FALSE, row.names=TRUE, quote=quote, na=na, ...)
  } else if (names=="col") {
    # If the first cell of the file is named "ID" Microsoft Excel warns that a SYLK file is opened. Therefore it is renamed.
    if(colnames(x)[1]=="ID") colnames(x)[1] <- "lD"
    write.table(x, file=pfad1, sep = ";", col.names=TRUE, row.names=FALSE, quote=quote, na=na, ...)
  } else if (names=="rowcol") {
    write.table(x, file=pfad1, sep = ";", col.names=NA)                    # Colnames & Rownames
  } else {
    write.table(x, file=pfad1, sep = ";", col.names=FALSE, row.names=FALSE, quote=quote, na=na, ...)
  }
  
  browseURL(pfad1)
}

load.gb <- function() {
  pfad1 <- "//evdad.admin.ch/AGROSCOPE_OS/2/5/2/1/2/2841/hpda/_ZA/Ref/Data/Grundlagenbericht/GB.RData"
  pfad2 <- "//evdad.admin.ch/AGROSCOPE_OS/2/5/2/1/2/2841/PrimDaten/GB/GB.RData"
  if(file.exists(pfad1)) pfad <- pfad1 else pfad <- pfad2
  cat("Tabellen werden aus folgendem Verzeichnis geladen:\n")
  cat(pfad, "\n", sep="")
  load(paste0(pfad))
  return(gb)
}

# id <- 72010409
id.entschluesseln <- function(...){
  pfad1 <- "//evdad.admin.ch/AGROSCOPE_OS/2/5/2/1/2/2841/hpda/_ZA/Ref/Data/Grundlagenbericht/"
  pfad2 <- "GB__allg_Einzel"
  if(!file.exists(paste0(pfad1,pfad2,".RData"))) {
    dat <- read.csv(paste0(pfad1,pfad2,".csv"), sep=";", header=TRUE, stringsAsFactors=FALSE, quote = "\"", na.strings=c("NA","","#DIV/0","#DIV/0!", "#WERT", "#WERT!"))
    save(dat, file=paste0(pfad1,pfad2,".RData") )
  }
  load( paste0(pfad1,pfad2,".RData") )
  
  id <- c(...)
  id <- id[!duplicated(id)]
  if(length(id)==1){
    res <- dat[dat[,"ID"]%in%id,"ID_unverschluesselt"]
    return(res[!duplicated(res)])
  } else {
    res <- dat[dat[,"ID"]%in%id,c("ID","ID_unverschluesselt")]
    return(res[!duplicated(res),])
  }
}

#folder <- "P:/_ZA/Ref/Data/Grundlagenbericht/"; filenames=NULL;extra_filename=NULL; update.files=FALSE; save.file=TRUE; save.name="GB"; filetype=c("csv"); NAto0=TRUE; header=TRUE; colnamesrow=1; skiprows=colnamesrow+1
merge.GB <- function(folder, filenames=NULL,extra_filename=NULL, update.files=FALSE, save.file=TRUE, save.name="GB", filetype=c("csv"), NAto0=TRUE, header=TRUE, colnamesrow=1, skiprows=colnamesrow+1, ...){
  # Grundlagenbericht importieren, indem die 6 csv Files nach der Vorlage der BO-Extraktionen "GB_A_Einzel, ..." eingelesen und aneinander gebunden werden.
  # Für aus BO exportierte csv die Einstellung colnamesrow=5 und skiprows=6 beibehalten.
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
      } else {
        headers <- read.csv(paste(fullnames[i],".csv",sep=""), sep=";", nrows=colnamesrow, stringsAsFactors=FALSE, header=FALSE, na.strings=c("NA","","#DIV/0","#DIV/0!", "#WERT", "#WERT!"))
        headers <- as.character(headers[colnamesrow,])
        na.cols <- headers=="NA"
        headers <- headers[!na.cols]
        gb <- read.csv(paste(fullnames[i],".csv",sep=""), sep=";", skip=skiprows, stringsAsFactors=FALSE, header=FALSE, na.strings=c("NA","","#DIV/0","#DIV/0!", "#WERT", "#WERT!"))
        gb <- gb[,!na.cols]
        colnames(gb) <- headers
      }
      gb <- gb[!is.na(gb[,1]),]
      # Falls manche Spalten Strings enthalten, werden diese in numeric umgewandelt.
      char.cols <- !sapply(gb,function(x)is.numeric(x))
      gb[,char.cols] <-   suppressWarnings( sapply(gb[,char.cols],function(x)as.numeric(x)) )
      # Falls gewünscht, werden die NA in 0 umgewandelt (nur bestimmte, bei denen 0 besser passt als NA)
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
          # Bei den letzten 4 ist es jedoch umgekehrt. Hier gehören NAs rein!
          # gb[, !convert.cols ] <- lapply(gb[, !convert.cols ],function(x)fillNA(x,invert=TRUE))
        }
      }
      
      gb.list[[i]] <- gb
      cat(filenames[i], "complete\n")
    }
  }
  # Prüfen, ob in allen Tabellen gleich viele Beobachtungen sind
  nrows <- do.call("c",lapply(gb.list,function(x)nrow(x)))
  if(any(nrows!=nrows[1])) stop(paste("Not the same filters were selected for the different years. You created an unbalanced panel.\nnrows= ",paste(nrows,collapse=", ")))
  # Prüfen, ob ID und Jahr immer in der ersten Spalte stehen
  gb.names <- do.call("rbind",lapply(gb.list,function(x)colnames(x)[1:2]))
  if(any(c(gb.names[,1]!="ID", gb.names[,2]!="Jahr"))) stop("The first two columns of each Excel file must contain the ID and the accounting year. Names must be 'ID' and 'Jahr'")
  
  # Prüfen, ob die Reihenfolge der Beobachtungen in allen Tabellen dieselbe ist
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
  
  # Nun noch fuer jeden Kanton das Kürzen anfuegen
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
merge.gb <- function(...)  merge.GB(...) # wrapper function
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

csv.to.rdata <- function(path, name="dat", ...){
  # This function converts csv data to RData (which can be read in much faster)
  
  # Remove .csv from string.
  if( substr.rev(path,1,4)%in%c( ".csv", ".CSV") ) path <- substr(path,1,nchar(path)-4)
  # Read in csv data
  assign(name, 
         read.csv(paste0(path, ".csv"), sep=";", header=TRUE, stringsAsFactors=FALSE, quote = "\"", na.strings=c("NA","","#DIV/0","#DIV/0!", "#WERT", "#WERT!"))
  )
  # Save RData
  eval(parse(text= paste0("save(",name,", file=",paste0("'", path, ".RData'") ,")")  ))
}

#### CLUSTER ANALYSIS ####

#x=matrix(1:100,ncol=4); method="kmeans"; nclust=2:10; measures=c(1,3,5,6,7); distance="euclidean"; modelName=NULL; nstart=100; outputfolder=NULL; filename=1:ncol(x); plot=TRUE;sign=FALSE; table.export=TRUE; rank.output=FALSE; count=FALSE; iter.max=1000; ablines=5; klassierung=FALSE# outputfolder=outputpfad
validation.stats.nice <- function(x, method, nclust=2:30, measures=NULL, distance="euclidean", modelName=NULL, nstart=10000, iter.max=1000, outputfolder=NULL, filename=1:ncol(x), plot=TRUE, ablines=5, sign=TRUE, table.export=TRUE, klassierung=FALSE, rank.output=FALSE, count=FALSE) { 
  # x: normal Matirx (not distance matrix!)
  performancemeasures <- c("Fehlerquadratsumme innerhalb der Cluster", "Erklaerte Streuung", "% Verbesserung ggnueber vorherigem Clustering", "Beal'sches F", "Calinski Harabasz Index", "Average Silhouette Width", "Beibehaltung der natuerlichen Datenstruktur","% Fehlklassifizierungen bei linearer Diskriminanzanalyse")
  perf.short <- c("within.ss","erkl.streuung", "PRE", "F Beale", "ch index","avg.sil","pearson gamma","% false lindisc")
  
  require(fpc)
  require(MASS)
  lower <- min(nclust); upper <- max(nclust)
  if(lower-upper==0) stop("You have to compare at least 2 different number of clusters (e.g. nclust=3:4)")
  if (method=="mclust") require(mclust)
  if (method=="mclust" & is.null(modelName)) cat("Unless you specify the exact model for mclust, the Mclust function will determine the best model for each number of clusters seperately (by using the BIC measure). If you decide to work with a certain number of clusters (after the validation.stats), check which model was used in validation.stats for the specific number of clusters by using the function Mclust(data, G=nclust). It will return the respective model. \nCalculating...\n")
  if (any(c(!is.logical(plot) , !is.logical(table.export) , !is.logical(rank.output) , !is.logical(count)))) stop("plot, table.export, rank.output and count must be logical (TRUE/FALSE)")
  if (!is.null(outputfolder)) {pdf(paste(outputfolder,"test.",method,".",lower,"-",upper,".",paste(colnames(x)[filename],collapse = "."),".pdf", sep=""), width=30, height=5); dev.off()
                               unlink(paste(outputfolder,"test.",method,".",lower,"-",upper,".",paste(colnames(x)[filename],collapse = "."),".pdf", sep=""), recursive = FALSE, force = TRUE)}
  ablinestest <- seq((lower+ablines-(lower%%ablines)),(floor(upper/ablines)*ablines),ablines)
  
  original.matrix <- x
  distance.matrix <- dist(x, method=distance)
  if(sign) {
    performancemeasures <- paste(performancemeasures,c("<",">",">",">",">",">",">","<"))
    perf.short <- paste(perf.short,c("<",">",">",">",">",">",">","<")) }
  perf.short.orig <- perf.short
  
  number <- wss <- avgsil <- ch <- pearson <- erkl.streuung <- proz.fehlklass <- Fbeale <- PRE <- numeric(0)
  clusterings <- clustermeans <- list()
  
  # Overall Sum of Squares - Fuer Fmax-Berechnung (alternative Methode zu der in cluster.stats, geht auch fuer within sum of squares)
  overall.cluster.mean <- apply(x,2,mean)
  d <- as.matrix(dist(rbind(overall.cluster.mean,x)))
  sqges <- sum(d[,1]^2)
  
  for(i in max(lower,2):upper)
  {
    clustering <- if (method=="kmeans") kmeans(original.matrix, centers=i, nstart=nstart, iter.max=iter.max)$cluster else if (method=="mclust") Mclust(original.matrix, G=i, modelNames= if (is.null(modelName)) Mclust(original.matrix)$modelName else modelName)$classification else cutree(hclust(distance.matrix, method=method),k=i)
    clstats <- cluster.stats( distance.matrix, clustering)
    clusterings[[i]] <- clustering
    clustermeans[[i]] <- apply(original.matrix,2,function(y)tapply(y,clustering,mean,na.rm=TRUE))
    colnames(clustermeans[[i]]) <- colnames(original.matrix); rownames(clustermeans[[i]]) <- 1:i
    
    number[i] <- i
    wss[i] <- clstats$within.cluster.ss
    avgsil[i] <- clstats$avg.silwidth
    ch[i] <- clstats$ch                       #Pseudo F Statistik nach Calinski, T. and Harabasz, J. (1974): A drendrite method for cluster analysis. In: Communications in Statistics - Theory and Methods 3(1):283-319
    pearson[i] <- clstats$pearsongamma
    erkl.streuung[i] <- 1-wss[i]/sqges                    # ist praktisch analog der WSS, nur umgekehrt
    #Fmax[i] <- (sqges-sqin)/(i-1) / ( sqin/(nrow(x)-i) ) # entspricht dem ch-Index
    
    lindisc <- lda(x,clustering)
    prediction <- predict(lindisc, x)$class
    crosstable <- table(prediction, clustering)
    n.false <- sum(crosstable)-sum(diag(crosstable))
    proz.fehlklass[i] <- n.false / nrow(x)
    
    
    if (count) cat(i,"/",upper,"\n")
  }
  
  # Fbeale & PRE
  if(lower==2) { wss2 <- c(sqges,wss[!is.na(wss)])
  } else {
    clustering <- if (method=="kmeans") kmeans(original.matrix, centers=(lower-1), nstart=nstart, iter.max=iter.max)$cluster else if (method=="mclust") Mclust(original.matrix, G=(lower-1), modelNames= if (is.null(modelName)) Mclust(original.matrix)$modelName else modelName)$classification else cutree(hclust(distance.matrix, method=method),k=(lower-1))
    sqin <- 0
    for(j in 1:i){
      cluster.mean <- apply(x[clustering==j,,drop=FALSE],2,mean)
      d <- as.matrix(dist( rbind( cluster.mean, x[clustering==j,,drop=FALSE] ) ))
      sqin <- sqin + sum(d[,1]^2)
    }
    wss2 <- c(sqin,wss[!is.na(wss)])
  }
  n <- nrow(x);m <- ncol(x) # m ist die Anzahl Clustervariablen
  for(i in 1:(length(wss2)-1))
    Fbeale[i] <- ((wss2[i]-wss2[i+1]) /wss2[i+1])  /   ( (n-(lower+i-2)) /(n-(lower+i-1)) * ((lower+i-1) /(lower+i-2))^(2/m) -1)
  Fbeale <- c(NA,Fbeale)
  #alternative Fbeale
  #for(j in 0:(length(wss2)-1))
  #  for(i in 0:(length(wss2)-1)) {
  #    Fbeale[j+lower,i+lower] <- ((wss2[i+1]-wss2[j+1]) /wss2[j+1])  /   ( (n-(lower+i)) /(n-(lower+j)) * ((lower+j) /(lower+i))^(2/m) -1)
  
  for(i in 1:(length(wss2)-1))
    PRE[i] <- 1-wss2[i+1]/wss2[i]
  PRE <- c(NA,PRE)
  
  outputmatrix <- cbind(number,wss,erkl.streuung,PRE,Fbeale,ch,avgsil,pearson,proz.fehlklass)
  outputmatrix <- outputmatrix[!is.na(outputmatrix[,1]),]
  colnames(outputmatrix) <- c("nclust",perf.short)
  rownames(outputmatrix) <- paste("k=",nclust,sep="")
  if(!is.null(measures)) {
    outputmatrix <- outputmatrix[,c(1,c(measures)+1)]
    performancemeasures <- performancemeasures[measures]
    perf.short <- perf.short[measures]
  }
  
  ### Tabellenausgabe als Tabelle
  if (!is.null(outputfolder)) { if(table.export==TRUE){write.table(outputmatrix, file=paste(outputfolder,"table.",method,".",lower,"-",upper,".",paste(colnames(original.matrix)[filename],collapse = "."),".csv", sep=""), sep=";", dec=".", col.names=NA)} }
  ### Output als PDF
  if(method=="kmeans") titl <- "k-Means Verfahren" else if (method=="complete") titl <- "Complete Linkage Verfahren" else if (method=="single") titl <- "Single Linkage Verfahren" else if (method=="average") titl <- "Average Linkage Verfahren" else if (method=="ward") titl <- "Ward's Verfahren" else if (method=="mclust") titl <- "Modellbasiertes Verfahren mit Mclust" else titl <- "Clustervalidierung"
  if (!is.null(outputfolder)) {
    pdf(paste(outputfolder,"graph.",method,".",lower,"-",upper,".",paste(colnames(original.matrix)[filename],collapse = "."),".pdf", sep=""), width=30, height=5)
    par(mfrow=c(1,length(performancemeasures)), mar=c(4,4,2,0.5))
    for (a in 1:length(performancemeasures))
    {
      plot( outputmatrix[,1], outputmatrix[,perf.short[a]], type="n", xlab=colnames(outputmatrix)[1], ylab=performancemeasures[a])
      lines( outputmatrix[,1], outputmatrix[,perf.short[a]], type="p", pch=20)
      lines( outputmatrix[,1], outputmatrix[,perf.short[a]], type="l")
      if(!is.null(ablines)) {
        abliner <- seq((lower+ablines-(lower%%ablines)),(floor(upper/ablines)*ablines),ablines)
        abline(v=abliner, col="gray60", lty=3)}
      if(a==1) title(main=titl)
    }
    dev.off()
  }  
  ### Plots
  if(plot==TRUE){
    dev.new(width=30, height=5)
    par(mfrow=c(1,length(performancemeasures)), mar=c(4,4,2,0.5))
    for (a in 1:length(performancemeasures))
    {
      plot( outputmatrix[,1], outputmatrix[,perf.short[a]], type="n", xlab="Anzahl Cluster", ylab=performancemeasures[a])
      lines( outputmatrix[,1], outputmatrix[,perf.short[a]], type="p", pch=20)
      lines( outputmatrix[,1], outputmatrix[,perf.short[a]], type="l")
      if(!is.null(ablines)) {
        abliner <- seq((lower+ablines-(lower%%ablines)),(floor(upper/ablines)*ablines),ablines)
        abline(v=abliner, col="gray60",lty=3)}
      if(a==1) title(main=titl)
    }
  }
  
  if(klassierung) {
    ### Rangklassifizierungen
    klass.vektor <- colnames(outputmatrix)
    rang.matrix <- outputmatrix
    if(any(colnames(rang.matrix)==perf.short.orig[1])) 
      rang.matrix[,which(colnames(rang.matrix)==perf.short.orig[1])] <- (-1) * rang.matrix[,which(colnames(rang.matrix)==perf.short.orig[1])]
    if(any(colnames(rang.matrix)==perf.short.orig[8])) 
      rang.matrix[,which(colnames(rang.matrix)==perf.short.orig[8])] <- (-1) * rang.matrix[,which(colnames(rang.matrix)==perf.short.orig[8])]
    
    for (i in 1:length(klass.vektor))
    { rang.matrix <- rang.matrix[order (-rang.matrix[,klass.vektor[i]]) , ]
      assign( paste("rang.vektor", i,sep=""), 1:nrow(outputmatrix) )
      rang.matrix <- cbind(rang.matrix, get(paste("rang.vektor", i,sep="")))
      #colnames(rang.matrix)[ncol(rang.matrix)] <- paste("k",i,sep="")
      colnames(rang.matrix)[ncol(rang.matrix)] <- paste("Rang",klass.vektor[i],sep=".")
    }
    rang.mittel <- rep(0, nrow(rang.matrix))
    for (i in 1:nrow(rang.matrix))
    { rang.mittel[i] <- sum( rang.matrix[ i , (ncol(rang.matrix)-length(klass.vektor)+1):ncol(rang.matrix)] )   /   length(klass.vektor) }
    rang.matrix <- cbind(rang.matrix, rang.mittel)
    colnames(rang.matrix)[ncol(rang.matrix)] <- "rang.mittel"
    rang.matrix <- rang.matrix[order (rang.matrix[,"rang.mittel"]) , ]
    rang.matrix <- rang.matrix[ , c(1,(ncol(rang.matrix)-length(klass.vektor)):ncol(rang.matrix))]
    ### Tabellenausgabe als File
    if (!is.null(outputfolder)) { if(rank.output==TRUE){write.table(rang.matrix, file=paste(outputfolder,"rank.",method,".",lower,"-",upper,".",paste(colnames(original.matrix)[filename],collapse = "."),".csv", sep=""), sep=";", dec=".", col.names=NA)} }
  }
  
  if(!klassierung) { result <- list(table = outputmatrix, clusterings=clusterings, clustermeans=clustermeans)
  } else { result <- list(table = outputmatrix, ranks=rang.matrix, clusterings=clusterings, clustermeans=clustermeans) }
  result$method <- method
  result$modelName <- modelName
  result$nstart <- nstart
  result$iter.max <- iter.max
  class(result) <- "cluster.validation"
  invisible(result)
}
print.cluster.validation <- function(x, ...){
  class(x) <- "list"
  x$clusterings <- NULL
  x$clustermeans <- NULL
  x$method <- NULL
  x$nstart <- NULL
  x$iter.max <- NULL
  x$modelName <- NULL
  
  print(x,...)
}

####
clusterboot.complete <- function(x, x.unscaled=NULL,  cluster.method=c("kmeans", "mclust","single","complete","average","mcquitty","ward","centroid","median"), nclust=2:10, boot.method=c("boot","subset"), dissolution=0.5, recover=0.75, iterations=100, seed=NULL, subtuning=floor(nrow(x)/2), km.runs=NULL, km.iter.max=100, count=FALSE)
{   #if(all(c(!is.null(x.unscaled), dim(x)!=dim(x.unscaled)))) stop("x and x.unscaled must have the same dimensions")
  cat("********************************************************************************************* \nBootstrapping... \nDepending on the number of iterations this may take long computation time. Please be patient. \n*********************************************************************************************\n")
  require(fpc)
  for (i in 1:length(cluster.method))
  { 
    if (cluster.method[i]=="kmeans")       { 
      
      for(j in min(nclust):max(nclust))     
      { if(km.runs==NULL) km.runs <- 1000*j
        cl.result <-  clusterboot(x, B=iterations, bootmethod=boot.method, clustermethod=kmeansCBI, krange=j, runs=km.runs, iter.max=km.iter.max, dissolution=dissolution, recover=recover, count=FALSE, seed=seed, subtuning=subtuning)
        x.temp <- if(is.null(x.unscaled)) x else x.unscaled
        x.zugeteilt <- as.data.frame(cbind( x.temp, cl.result$result$partition))
        colnames(x.zugeteilt)[ncol(x.zugeteilt)] <- "Clusternummer"
        centers <- matrix(nrow=j, ncol=ncol(x.temp) )
        colnames(centers) <- colnames(x.temp)
        for(s in 1:j)
          centers[s,] <- colMeans(x.temp[x.zugeteilt$Clusternummer==s, , drop=FALSE], na.rm=TRUE)
        round(centers, digits=3)
        sizes <- matrix(nrow=1, ncol=j); rownames(sizes) <- "cluster.sizes "   
        for (s in 1:j)
          sizes[s] <- apply(x.temp[x.zugeteilt$Clusternummer==s,1, drop=FALSE],2, length)
        
        if (length(boot.method)==1 & boot.method[1]=="boot") {
          boot.result <- rbind( round(cl.result$bootmean, digits=2), cl.result$bootbrd, cl.result$bootrecover)
          rownames(boot.result) <- c("boot.Jacc.mean", "boot.dissolved", "boot.recovered")
          result.final <- list(boot.result=boot.result, sizes=sizes, centers=centers)
          print(paste(cluster.method[i],".",j,sep="")); print(result.final)
          
        } else if (length(boot.method)==1 & boot.method[1]=="subset") {
          subset.result <- rbind( round(cl.result$subsetmean, digits=2), cl.result$subsetbrd, cl.result$subsetrecover)
          rownames(subset.result) <- c("subs.Jacc.mean", "subs.dissolved", "subs.recovered")
          result.final <- list(subset.result=subset.result, sizes=sizes, centers=centers)
          print(paste(cluster.method[i],".",j,sep="")); print(result.final)
          
        } else {
          boot.result <- rbind( round(cl.result$bootmean, digits=2), cl.result$bootbrd, cl.result$bootrecover)
          rownames(boot.result) <- c("boot.Jacc.mean", "boot.dissolved", "boot.recovered")
          subset.result <- rbind( round(cl.result$subsetmean, digits=2), cl.result$subsetbrd, cl.result$subsetrecover)
          rownames(subset.result) <- c("subs.Jacc.mean", "subs.dissolved", "subs.recovered")
          result.final <- list(boot.result=boot.result, subset.result=subset.result, sizes=sizes, centers=centers)
          print(paste(cluster.method[i],".",j,sep="")); print(result.final)
        }
        #assign(paste(cluster.method[i],".",j,sep=""),  list(bootmean=round(cl.result$bootmean, digits=3), dissolved=cl.result$bootbrd, recovered=cl.result$bootrecover, centers=cl.result$result$result$center, sizes=cl.result$result$result$size))      
        #print(paste(cluster.method[i],".",j,sep="")); print(get(paste(cluster.method[i],".",j,sep="")))
      }                                                     
    } else if (cluster.method[i]=="mclust")  {
      for(j in min(nclust):max(nclust))     
      { cl.result <- clusterboot(x, B=iterations, bootmethod=boot.method, clustermethod=noisemclustCBI, G=j, multipleboot=FALSE, dissolution=dissolution, recover=recover, count=count, seed=seed, subtuning=subtuning)
        cl.result$result$result <- cl.result$result$result[1,complete.cases(cl.result$result$result[1,1:10])] # NA Methoden entfernen                                                            
        inds <- which(cl.result$result$result == max(cl.result$result$result), arr.ind=TRUE)
        bestmodel <- names(cl.result$result$result)[inds]
        mcl.result <- Mclust(x, G=j, modelName=bestmodel)
        #centers <- t(mcl.result$parameters$mean)
        #sizes <- round( nrow(x)*mcl.result$parameters$pro, digits=0)
        
        x.temp <- if(is.null(x.unscaled)) x else x.unscaled
        x.zugeteilt <- as.data.frame(cbind( x.temp, mcl.result$classification))
        colnames(x.zugeteilt)[ncol(x.zugeteilt)] <- "Clusternummer"
        centers <- matrix(nrow=j, ncol=ncol(x.temp) )
        colnames(centers) <- colnames(x.temp)
        for(s in 1:j)
          centers[s,] <- colMeans(x.temp[x.zugeteilt$Clusternummer==s, , drop=FALSE], na.rm=TRUE)
        round(centers, digits=3)
        sizes <- matrix(nrow=1, ncol=j); rownames(sizes) <- "cluster.sizes "    
        for (s in 1:j)
          sizes[s] <- apply(x.temp[x.zugeteilt$Clusternummer==s,1, drop=FALSE],2, length)                                                                                                                    
        
        if (length(boot.method)==1 & boot.method[1]=="boot") {
          boot.result <- rbind( round(cl.result$bootmean, digits=2), cl.result$bootbrd, cl.result$bootrecover)
          rownames(boot.result) <- c("boot.Jacc.mean", "boot.dissolved", "boot.recovered")
          result.final <- list(boot.result=boot.result, sizes=sizes, centers=centers)
          print(paste(cluster.method[i],".",bestmodel,".",j,sep="")); print(result.final)
          
        } else if (length(boot.method)==1 & boot.method[1]=="subset") {
          subset.result <- rbind( round(cl.result$subsetmean, digits=2), cl.result$subsetbrd, cl.result$subsetrecover)
          rownames(subset.result) <- c("subs.Jacc.mean", "subs.dissolved", "subs.recovered")
          result.final <- list(subset.result=subset.result, sizes=sizes, centers=centers)
          print(paste(cluster.method[i],".",bestmodel,".",j,sep="")); print(result.final)
          
        } else {
          boot.result <- rbind( round(cl.result$bootmean, digits=2), cl.result$bootbrd, cl.result$bootrecover)
          rownames(boot.result) <- c("boot.Jacc.mean", "boot.dissolved", "boot.recovered")
          subset.result <- rbind( round(cl.result$subsetmean, digits=2), cl.result$subsetbrd, cl.result$subsetrecover)
          rownames(subset.result) <- c("subs.Jacc.mean", "subs.dissolved", "subs.recovered")
          result.final <- list(boot.result=boot.result, subset.result=subset.result, sizes=sizes, centers=centers)
          print(paste(cluster.method[i],".",bestmodel,".",j,sep="")); print(result.final)
        }
        
      }
    } else                               {
      for(j in min(nclust):max(nclust))     
      { cl.result <- clusterboot(x, B=iterations, bootmethod=boot.method, clustermethod=hclustCBI, method=cluster.method[i], k=j, cut="number", dissolution=dissolution, recover=recover, count=count, seed=seed, subtuning=subtuning)
        
        x.temp <- if(is.null(x.unscaled)) x else x.unscaled
        x.zugeteilt <- as.data.frame(cbind( x.temp, cl.result$result$partition))
        colnames(x.zugeteilt)[ncol(x.zugeteilt)] <- "Clusternummer"
        centers <- matrix(nrow=j, ncol=ncol(x.temp) )
        colnames(centers) <- colnames(x.temp)
        for(s in 1:j)
          centers[s,] <- colMeans(x.temp[x.zugeteilt$Clusternummer==s, , drop=FALSE], na.rm=TRUE)
        round(centers, digits=3)
        sizes <- matrix(nrow=1, ncol=j); rownames(sizes) <- "cluster.sizes "    
        for (s in 1:j)
          sizes[s] <- apply(x.temp[x.zugeteilt$Clusternummer==s,1, drop=FALSE],2, length)
        
        if (length(boot.method)==1 & boot.method[1]=="boot") {
          boot.result <- rbind( round(cl.result$bootmean, digits=2), cl.result$bootbrd, cl.result$bootrecover)
          rownames(boot.result) <- c("boot.Jacc.mean", "boot.dissolved", "boot.recovered")
          result.final <- list(boot.result=boot.result, sizes=sizes, centers=centers)
          print(paste(cluster.method[i],j,sep="")); print(result.final)
          
        } else if (length(boot.method)==1 & boot.method[1]=="subset") {
          subset.result <- rbind( round(cl.result$subsetmean, digits=2), cl.result$subsetbrd, cl.result$subsetrecover)
          rownames(subset.result) <- c("subs.Jacc.mean", "subs.dissolved", "subs.recovered")
          result.final <- list(subset.result=subset.result, sizes=sizes, centers=centers)
          print(paste(cluster.method[i],j,sep="")); print(result.final)
          
        } else {
          boot.result <- rbind( round(cl.result$bootmean, digits=2), cl.result$bootbrd, cl.result$bootrecover)
          rownames(boot.result) <- c("boot.Jacc.mean", "boot.dissolved", "boot.recovered")
          subset.result <- rbind( round(cl.result$subsetmean, digits=2), cl.result$subsetbrd, cl.result$subsetrecover)
          rownames(subset.result) <- c("subs.Jacc.mean", "subs.dissolved", "subs.recovered")
          result.final <- list(boot.result=boot.result, subset.result=subset.result, sizes=sizes, centers=centers)
          print(paste(cluster.method[i],".",j,sep="")); print(result.final)
        }
      } 
    }
  }
  cat("********************************************************************************************* \nBootstrapping done. There is no function value. Copy paste the results in order to keep them.\n")
}
#clusterboot.complete (x=aaa.mat,  x.unscaled=aaa.auswahl,cluster.method=c( "mclust","single","complete","average","mcquitty","ward","centroid","median", "kmeans"), nclust=2:3, boot.method=c("subset","boot"), iterations=3, subtuning=floor(nrow(x)/5))
#i<- 9;j <- 2; x <- aaa.mat; x.unscaled <- aaa.auswahl; nclust <- 2:3; iterations<-10; cluster.method<-c("mclust","single","complete","average","mcquitty","ward","centroid","median", "kmeans"); boot.method<-c("boot","subset");clustmodel="VEV"; dissolution <- 0.5; recover <- 0.75; seed=NULL


####
cluster.pairs.plot <- function(data, clustering, points="numbers", colors=NULL, main="clustering") {
  data.clustered <-  as.data.frame( cbind(data, clustering) )
  colnames(data.clustered)[ncol(data.clustered)] <- "cluster"
  
  if(!is.null(colors)) {colors <- colors} else {colors <- c("red", "blue", "green", "yellow", "purple", "gray87", "cyan", "yellowgreen", "steelblue1", "orchid", "purple", "orange", "black", "black", "black", "black", "black", "black", "black", "black", "black", "black", "black", "black", "black", "black")}
  if(points=="numbers") {pch <- c(49:57)} else if(points=="characters") {pch <- c(65:90)} else if(points=="points") {pch <- 21} else if (points=="empty.points") {pch <- 21}
  if(points=="numbers"& max(clustering)>9 ) {stop("The number of clusters is higher than 9. You have to choose characters or points for plotting.\n")}
  if(max(clustering)>length(colors)) {cat("Not enough colors for all clusters.")}
  
  if(points=="points") {
    pairs(data, main="Clustering", pch=pch[data.clustered$cluster], bg=colors[data.clustered$cluster]) 
  } else if(max(clustering)>length(colors)) {
    pairs(data, main="Clustering", pch=c(65:90)[data.clustered$cluster]) 
  } else {
    pairs(data, main="Clustering", pch=pch[data.clustered$cluster], col=colors[data.clustered$cluster])}
}
#windows();cluster.pairs(aaa.mat, aaa.cut, points="numbers")

####
clusterassignment.random <- function(data, data.new, clustering) {
  data <- as.matrix(data)
  data.new <-  as.matrix(data.new)
  rownames(data.new) <- 1:nrow(data.new)
  nclust <- max(clustering)
  
  clustersizes <- numeric()
  for(i in 1:nclust)
    clustersizes[i] <- apply(data[clustering==i,1, drop=F],2,length)
  clustersizes.new <- round(clustersizes*nrow(data.new)/nrow(data))
  
  if(sum(clustersizes.new)!=sum(clustersizes)) {
    diff <- sum(clustersizes) - sum(clustersizes.new)
    maxi <- max(clustersizes.new)
    clustersizes[which(clustersizes[]==maxi),] <- clustersizes[which(clustersizes[]==maxi),] + diff
    cat(paste("The greatest cluster has",diff,"members more than in the orignial dataset due to rounding errors\n"))
  }
  
  for (i in 1:nclust){
    if (!is.null(nrow(data.new))) {
      smpl <- sample(1:nrow(data.new), clustersizes.new[i])
      data.new.cl.smpl <- cbind(data.new[smpl,,drop=FALSE],matrix(i,nrow=nrow(data.new[smpl,,drop=FALSE]), ncol=1))
      colnames(data.new.cl.smpl)[ncol((data.new.cl.smpl))] <- "clustering"
      
      if (i==1) {data.new.cl <- as.matrix(data.new.cl.smpl)} else {data.new.cl <- as.matrix(rbind(data.new.cl, data.new.cl.smpl))} 
      data.new <- data.new[-smpl,,drop=F]
    }
  }
  return(data.new.cl)  
}
#data=aaa.alle; clustering=aaa.cut
#data <- matrix(rep(1,30), ncol=2); data.new <- data[1:10,1,drop=F];clustering=c(1,1,2,3,3,2,5,5,4,4,1,2,3,4,5)
#zufaellig <- clusterassignment.random(data=data, data.new=data.new, clustering=clustering)

####
#i <- 2; a <- 2; lower=2; upper=3; x <- aaa.mat; methods=c("kmeans", "ward", "single", "average", "complete", "mcquitty", "median", "centroid"); performancemeasures <- c("average.within <", "average.between >", "wb.ratio <", "within.cluster.ss <", "avg.silwidth [-1,1] >", "pearsongamma [0,1] >", "ch dissimilarities >")
#validation.stats.complete(aaa.mat, lower=2, upper=3, outputfolder="C:/Users/art-hoe/_Wachstumsstrategie/Statistik/Clusteranalyse/Ergebnistabellen/test/")
validation.stats.complete <- function(x=NULL, methods=c("kmeans", "ward", "single", "average", "complete", "mcquitty", "median", "centroid"), lower=2, upper=15, outputfolder=NULL, nstart=100, dist.method = "euclidean")
{ require(fpc)
  require(MASS)
  if (is.null(x)) stop("please give the matrix to be used for clustering")
  aaa.alle.df <- as.data.frame(x)
  original.matrix <- x
  distance.matrix <- dist(x, method=dist.method)
  performancemeasures <- c("average.within <", "average.between >", "wb.ratio <", "within.cluster.ss <", "avg.silwidth [-1,1] >", "pearsongamma [0,1] >", "ch dissimilarities >")
  for (a in 1:length(methods))
  {
    i <- max(lower,2)             # i entspricht nclust in cutree!
    while (i <= upper)
    {
      aaa.cut <- if (methods[a]=="kmeans") kmeans(original.matrix, centers=i, nstart=nstart, iter.max=100)$cluster else cutree(hclust(distance.matrix, method=methods[a]),k=i)
      clstats <- cluster.stats( distance.matrix, aaa.cut)
      outputline <- c(i, clstats$average.within, clstats$average.between, clstats$wb.ratio, clstats$within.cluster.ss, clstats$avg.silwidth, clstats$pearsongamma, clstats$ch)
      
      ### Dem Dataframe die Spalte hinzufuegen, die fuer jeden Betrieb die Clusternummer angibt.
      aaa.alle.zugeteilt <- cbind(aaa.alle.df, aaa.cut)
      colnames(aaa.alle.zugeteilt)[ncol(aaa.alle.zugeteilt)] <- "Clusternummer"
      
      ### Diskriminanzanalyse
      aaa.spaltenauswahl.lda <- lda(aaa.alle.zugeteilt[,1:ncol(aaa.alle.zugeteilt)-1], aaa.alle.zugeteilt[,"Clusternummer"])
      aaa.spaltenauswahl.lda.predict <- predict(aaa.spaltenauswahl.lda, aaa.alle.zugeteilt[,1:ncol(aaa.alle.zugeteilt)-1])$class
      crosstable <- table(aaa.spaltenauswahl.lda.predict, aaa.alle.zugeteilt[,"Clusternummer"])
      summe <- 0
      crosstable.overall <- crosstable
      for (ab in 1:ncol(crosstable.overall))
      {crosstable.overall[ab,ab] <- 0
       summe <- summe + sum(crosstable.overall[ab,])}
      n.false <- summe
      prop.false <- n.false / nrow(aaa.alle.zugeteilt)
      outputmatrix <- round( matrix( c(outputline, prop.false), nrow=1), digits=3)
      
      rownames(outputmatrix) <- paste(methods[a],".k=",i,sep="")
      assign(paste("outputmatrix.n.",i,sep=""), outputmatrix)
      
      
      if (i!=2) { assign(  paste("outputmatrix.n.",i,sep="")   ,rbind( get(paste("outputmatrix.n.",(i-1),sep="")), get(paste("outputmatrix.n.",i,sep="")) )) }
      
      i <- i+1
    }
    assign( paste("outputmatrix.", methods[a], sep=""),  get(paste("outputmatrix.n.", upper, sep="")) )
    if (a!=1) {  assign( paste("outputmatrix.", methods[a], sep=""),      rbind(get(paste("outputmatrix.", methods[a-1], sep="")), get(paste("outputmatrix.", methods[a], sep=""))) ) }
  }
  outputmatrix.final <- get(paste("outputmatrix.", methods[a], sep=""))
  colnames(outputmatrix.final) <- c( "nclust", performancemeasures, "Fehlklassifizierungen <")
  
  
  ### Rangklassierungen
  #klass.vektor <-  c("wb.ratio <", "within.cluster.ss <", "avg.silwidth [-1,1] >", "pearsongamma [0,1] >", "ch dissimilarities >", "Fehlklassifizierungen <")
  klass.vektor <-  c("avg.silwidth [-1,1] >", "pearsongamma [0,1] >", "ch dissimilarities >", "Fehlklassifizierungen <")
  rang.matrix <- outputmatrix.final[ , c("nclust",klass.vektor)]
  
  # Umkehrung fuer richtige Rangierung
  rang.matrix[,"avg.silwidth [-1,1] >"]  <- rang.matrix[,"avg.silwidth [-1,1] >"] * (-1)
  rang.matrix[,"ch dissimilarities >"]  <- rang.matrix[,"ch dissimilarities >"] * (-1)
  rang.matrix[,"pearsongamma [0,1] >"]  <- rang.matrix[,"pearsongamma [0,1] >"] * (-1)
  
  for (i in 1:length(klass.vektor))
  { rang.matrix <- rang.matrix[order (rang.matrix[,klass.vektor[i]]) , ]
    assign( paste("rang.vektor", i,sep=""), 1:nrow(outputmatrix.final) )
    rang.matrix <- cbind(rang.matrix, get(paste("rang.vektor", i,sep="")))
    colnames(rang.matrix)[ncol(rang.matrix)] <- paste("Rang",klass.vektor[i],sep=".")
  }
  
  # Zurueck-Umkehrung fuer spaeteren Output
  rang.matrix[,"avg.silwidth [-1,1] >"]  <- rang.matrix[,"avg.silwidth [-1,1] >"] * (-1)
  rang.matrix[,"ch dissimilarities >"]  <- rang.matrix[,"ch dissimilarities >"] * (-1)
  rang.matrix[,"pearsongamma [0,1] >"]  <- rang.matrix[,"pearsongamma [0,1] >"] * (-1)
  
  rang.mittel <- rep(0, nrow(rang.matrix))
  for (i in 1:nrow(rang.matrix))
  { rang.mittel[i] <- sum( rang.matrix[ i , (ncol(rang.matrix)-length(klass.vektor)+1):ncol(rang.matrix)] )   /   length(klass.vektor) }
  rang.matrix <- cbind(rang.matrix, rang.mittel)
  colnames(rang.matrix)[ncol(rang.matrix)] <- "rang.mittel"
  rang.matrix <- rang.matrix[order (rang.matrix[,"rang.mittel"]) , ]
  #rang.matrix <- rang.matrix[ , c(1,(ncol(rang.matrix)-length(klass.vektor)):ncol(rang.matrix))]
  
  ### Tabellenausgabe als File
  if (!is.null(outputfolder))
  {
    info.zeile <- matrix(nrow=1, ncol=ncol(rang.matrix))
    info.zeile [1,1] <- paste(colnames(x), collapse=", ")
    rang.matrix.output <- rbind(rang.matrix, info.zeile)
    write.table(rang.matrix.output, file=paste(outputfolder,"validation.stats.complete.output",lower,"-",upper," (automatic).csv", sep=""), sep = ";", dec=".", col.names=NA)
  }
  table <- outputmatrix.final
  message <- c("Ergebnisse unter validation.stats.output$ abrufbar.")
  result<- list(table = outputmatrix.final, ranks=rang.matrix)
  return(result)
}


#### DISCRIMINANT ANALYSIS, SIMILARITY MATCHING ####

#data(iris); similarity.matching.validation(iris[,1:4],iris[,5],divide=1,runs=100,scaling=TRUE); 
#similarity.matching.validation(divide=0,new=iris[,1:4],old=iris[,1:4],transfer.grouping=iris[,5],compare.grouping=iris[,5]);similarity.matching.validation.arg=list(data=iris[,1:4], grouping=iris[,5], distance.method="euclidean", scaling=FALSE, divide=1/2, runs=10, new=NULL,old=NULL,transfer.grouping=NULL,compare.grouping=NULL);  similarity.matching.validation.arg=list(data=NULL, grouping=NULL, distance.method="euclidean", scaling=FALSE, divide=FALSE, runs=10, new=iris[,1:4],old=iris[,1:4],transfer.grouping=iris[,5],compare.grouping=iris[,5])
#similarity.matching.validation.arg=list(data=iris[,1:4], grouping=as.numeric(as.factor(iris[,5])), distance.method="euclidean", scaling=FALSE, divide=1, runs=10, new=NULL,old=NULL,transfer.grouping=NULL,compare.grouping=NULL)
#similarity.matching.validation.arg=list(data=results.lda.orig[,1:400],grouping=grouping, data=NULL, grouping=NULL, distance.method="euclidean", scaling=FALSE, divide=1/2, runs=5, new=NULL, old=NULL, transfer.grouping=NULL, compare.grouping=NULL)
similarity.matching.validation <- function(data=NULL, grouping=NULL, divide=1/2, runs=100, new=NULL, old=NULL, transfer.grouping=NULL,compare.grouping=NULL, distance.method="euclidean", scaling=FALSE, na.rm=c("cols","rows","no")[1]) {
  # Explanation: The default version (divide=TRUE) only needs the data and grouping. Data and grouping are split into train and predict set of equal size. The predicted grouping (transferred rows via similarity.matching()) is then compared to the real grouping (originally given).
  # This is done in order not to compare same rows in one and the same data frame because obviously this would always result in a 100% correct prediction. The procedure is repeated runs=... times.
  # If you want to compare equal observations (rows) but the measurements (columns) differ because they were e.g. taken in a different year/experiment, then you can use new,old and transfer.grouping (as it is used in simliarity.matchin() function)
  # where the transfer.grouping is the grouping transferred from the old data of course. The resulting prediction will then be compared to the originially given compare.grouping which is the grouping of the new data (which is of course known, as you did the experiment yourself).
  if(!is.null(data)) if(nrow(data)!=length(grouping))          stop("nrow(data) and length(grouping) must be equal")
  if(!is.null(new))  if(nrow(new)!=length(compare.grouping))   stop("nrow(new) and length(compare.grouping) must be equal")
  if(!is.null(old))  if(nrow(old)!=length(transfer.grouping))  stop("nrow(old) and length(transfer.grouping) must be equal")
  if(na.rm=="no") {
    data <- data
  } else if(na.rm=="cols") {
    na.values <- is.na(apply(data,2,sum))
    if(any(na.values)) { data <- data[,!na.values]; message("columns containing NA values were removed by default") } 
  } else if (na.rm=="rows") {
    na.values <- is.na(apply(data,1,sum))
    if(any(na.values)) { data <- data[!na.values,]; message("rows containing NA values were removed") }
  } else stop("invalid na.rm agrument. choose 'cols','rows' or 'no'")
  lda.validation(similarity.matching.validation.arg=list(data=data, grouping=grouping, divide=divide, runs=runs, new=new, old=old, transfer.grouping=transfer.grouping, compare.grouping=compare.grouping, distance.method=distance.method, scaling=scaling))
}

####
#lda.validation(data,grouping,divide=9/10,runs=100); #b <-lda.validation(data,grouping,TRUE,5)
#data <- iris[,1:4]; grouping<-as.numeric(as.factor(iris[,5])); similarity.matching.validation.arg<-NULL; qda=FALSE;runs=10; divide=1/2; similarity.matching.validation.arg=NULL
#lda.validation(results[1:500,50:97],results[1:500,"Clusternummer"],divide=1,runs=10)
lda.validation <- function(data, grouping,divide=0,runs=10,qda=FALSE,similarity.matching.validation.arg=NULL) {
  # Explanation: This function tests the goodness of a linear discriminant prediction by comparing it with the given grouping.
  # Give the data and the grouping that should be used for the discriminant analysis. For the default divide=FALSE the model prediction is compared with the actual grouping.
  # Because the lda calculates the model such that the number of false predictions is a minimum this method is not completely free of critism. To avoid this drawback you can choose to
  # divide the data & grouping into a train and a predict set (by setting divide>0). The size of the train set is divide*nrow(data).
  # Then the observations for model calculation and prediction are not equal. runs=... can be specified to repeat the process several times in order to get higher accuracy (mean of all runs is taken).
  require(MASS)
  if(!is.null(similarity.matching.validation.arg)) sim.match <- TRUE else sim.match <- FALSE
  if(sim.match)  {
    data  <- similarity.matching.validation.arg$data
    grouping <- similarity.matching.validation.arg$grouping
    distance.method <- similarity.matching.validation.arg$distance.method
    scaling <- similarity.matching.validation.arg$scaling
    divide <- similarity.matching.validation.arg$divide
    runs <- similarity.matching.validation.arg$runs
    new <- similarity.matching.validation.arg$new
    old <- similarity.matching.validation.arg$old
    transfer.grouping <- similarity.matching.validation.arg$transfer.grouping
    compare.grouping <- similarity.matching.validation.arg$compare.grouping
  }
  if(ncol(data)==length(grouping)) stop("ncol(data) and length(grouping) must be equal")
  if(divide<0|divide>1) stop("choose 0 <= divide < 1")
  grouping <- as.factor(grouping)
  grouping.orig <- grouping
  sizes.orig <- tapply(grouping.orig,grouping.orig,length)
  
  if(!sim.match) data <- data[,!is.na(apply(data,2,sum))]
  if(!sim.match&divide==1) { # Removal of constant Variables for divide==1
    var.div1.out <- apply(data,2,function(x)any(tapply(x,grouping.orig,function(y) length(unique(y))<3 )))
    if(any(var.div1.out)) {
      data <- data[,!var.div1.out]
      warning(paste("following varaibles were excluded from lda because they are/became constant within groups:\n", paste(names(var.div1.out)[var.div1.out],collapse=" ")))
    }
  }
  
  # Procedure without division of data
  if(divide==0) {
    if(!sim.match) {
      # Procedure for lda validation
      if(!qda) lda.model <- lda(data, grouping) else if(qda) lda.model <- qda(data, grouping)
      prediction <- predict(lda.model, data)$class
    } else {
      # Procedure for similarity matching
      matching <- similarity.matching(new=new, old=old, transfer=transfer.grouping, distance.method="euclidean", scaling=scaling)
      prediction <- matching[,ncol(matching)]
      grouping <- compare.grouping
    }
  }
  
  if(divide==0) { runs <- 1
  } else if(divide==1) runs <- length(grouping.orig)
  crosstable.ls <- list()
  
  # Begin of the loop for Division of Data & Crosstable saving
  for(j in 1:runs) {
    
    if(divide==1) {
      grouping <- grouping.orig
      predict.samples <- which(1:runs %in% j)
      train.samples <- which(!1:runs %in% j)
      
      predict.set <- data[predict.samples,,drop=FALSE]
      predict.grouping <- grouping[ predict.samples ]
      train.set <- data[train.samples,]
      train.grouping <- grouping[ train.samples ]
      
    } else if(divide>0&divide<1) {
      grouping <- grouping.orig
      divided.groups <- train.samples <- list()
      
      for(i in unique(grouping)) {
        divided.groups[[i]] <- which(grouping==i)
        train.samples[[i]] <- sample(divided.groups[[i]], round(length(divided.groups[[i]])*divide))
      }
      train.samples <- do.call("c",train.samples)
      predict.samples <- which(!1:nrow(data) %in% train.samples)
      
      predict.set <- data[predict.samples,,drop=FALSE]
      predict.grouping <- grouping[predict.samples]
      train.set <- data[train.samples,,drop=FALSE]
      train.grouping <- grouping[train.samples]
    }
    
    if(!sim.match&divide!=0) {
      # Procedure for lda validation
      if(divide!=1) {
        no.variation <- apply(train.set,2,function(x)all(tapply(x,train.grouping,function(y) if(var(y)==0) TRUE else FALSE )))
        if(any(no.variation)) warning(paste("following varaibles were excluded from lda because they are/became constant within groups:\n", paste(names(no.variation)[no.variation],collapse=" ")))
      } else if (divide==1) no.variation <- rep(FALSE,length(ncol(data)))
      if(!qda) lda.model <- lda(train.set[,!no.variation], train.grouping) else if(qda) lda.model <- qda(train.set[,!no.variation], train.grouping)
      prediction <- predict(lda.model, predict.set[,!no.variation])$class
      grouping <- predict.grouping
      
    } else if(sim.match) {
      if(divide==1) {
        # Procedure for similarity matching
        memory.limit(4095)
        if(j==1) {
          if (scaling==TRUE) dist <- scale(data) else dist <- data
          dist <- as.matrix(dist(dist, method=distance.method))
        }
        d <- dist[sort(train.samples),sort(predict.samples),drop=FALSE]
        #transfer <- grouping[train.samples]
        rownames <- sort(train.samples)
        d <- cbind(rownames,d)
        z <- unname(apply(d[,2:ncol(d),drop=FALSE],2,function(x) (which(x==min(x))[1])))
        prediction <- grouping[z]
        grouping <- predict.grouping
      }
      if(divide!=1) {
        matching <- similarity.matching(new=predict.set, old=train.set, transfer=train.grouping, distance.method="euclidean", scaling=TRUE, new.keep=NULL)
        prediction <- matching[,ncol(matching)]
        grouping <- predict.grouping
      }
    }
    
    # From now on the procedure is equal for lda validation and similarity validation
    crosstable.ls[[j]] <- table(prediction, grouping)
  }
  # End of the Loop
  
  
  if(divide!=1) {
    crosstable <- Reduce("+",crosstable.ls)/runs
  } else if (divide==1) {
    crosstable <- Reduce("+",crosstable.ls)
    grouping <- grouping.orig
  }
  
  obs.total <- length(grouping)
  sizes <- tapply(grouping,grouping,length)
  
  crosstable.false <- crosstable
  diag(crosstable.false) <- 0
  n.false <- sum(crosstable.false)
  n.right <- obs.total-n.false
  prop.false <-  n.false / length(grouping)
  prop.right <- 1-prop.false
  
  n.right.detailed <- matrix(nrow=3, ncol=length(sizes))
  n.right.detailed[1,] <- sizes
  n.right.detailed[2,] <- apply(crosstable,1,sum)
  n.right.detailed[3,] <- diag(crosstable)
  prop.right.detailed <-  diag(crosstable) / sizes
  dimnames(n.right.detailed) <- list(c("origin. size","predict size","overlap size"),c(sort(unique(grouping.orig))))
  n.right.detailed <- cbind(n.right.detailed,obs.total=c(0,0,obs.total),right=c(0,0,n.right),false=c(0,0,n.false))
  sizedistortion <- round( apply(crosstable,1,sum) / sizes,2)
  
  probs <- sizes/obs.total
  n.hypo <- probs*sizes
  n.right.hypo <- sum(n.hypo)
  prop.right.hypo.detailed <- n.hypo/sizes
  prop.right.hypo <- n.right.hypo/obs.total
  
  proportion.table.detailed <- rbind(prop.right.detailed,prop.right.hypo.detailed); dimnames(proportion.table.detailed) <- list(c("%actually right","%right by chance"),c(sort(unique(grouping.orig))))
  proportion.table <- rbind(prop.right,prop.right.hypo); dimnames(proportion.table) <- list(c("%actually right","%right by chance"),c("overall"))
  proportion.table.comb <- cbind(proportion.table.detailed,  proportion.table)
  if(prop.right.hypo < max(sizes/sum(sizes)) ) {
    greatest.hypo <- max(sizes/sum(sizes))
    proportion.table.comb <- cbind( proportion.table.comb, c(prop.right, greatest.hypo))
    colnames(proportion.table.comb)[ncol(proportion.table.comb)] <- paste("all.in", names(sizes)[sizes==max(sizes)], sep="." )
  }
  better.than.chance <- proportion.table.comb[1,]/proportion.table.comb[2,]/100;
  proportion.result <- rbind(proportion.table.comb,"x.better"=better.than.chance); 
  proportion.result <- round(100*proportion.result,2)
  
  if(divide==0|divide==1) { result <- list(crosstable=crosstable,sizes=n.right.detailed, size.distortion=sizedistortion, proportions=proportion.result)
  } else {
    result <-  list(crosstable=round(crosstable),sizes=round(n.right.detailed))
    result2 <- list(round(crosstable/(1-divide)), round(n.right.detailed/(1-divide)), distortion=sizedistortion, proportion.result)
    if((1/(1-divide))%%1>0) { names(result2) <- c(paste("crosstable.x",round(1/(1-divide),2),sep=""), paste("sizes.x",round(1/(1-divide),2),sep=""),"size.distortion", "proportions") 
    } else                names(result2) <- c(paste("crosstable.x",1/(1-divide),sep=""), paste("sizes.x",1/(1-divide),sep=""),"size.distortion", "proportions")
    result <- c(result,result2) }
  
  return(result)
}
#lda.validation(data,grouping,divide=1); #b <-lda.validation(data,grouping,TRUE,5)
#data <- iris[,1:4]; grouping<-as.numeric(as.factor(iris[,5])); similarity.matching.validation.arg<-NULL; qda=FALSE;runs=10; divide=1; similarity.matching.validation.arg=NULL
#lda.validation(results[1:500,50:97],results[1:500,"Clusternummer"],divide=1,runs=10)

####
#new=predict.set; old=train.set; transfer=train.grouping; distance.method="euclidean"; scaling=TRUE;new.keep=NULL
similarity.matching <- function(new, old, transfer, new.keep=NULL, distance.method="euclidean", scaling=FALSE) {
  # Explanation: new = data to which "transfer" should be transferred from old by looking at similarity between rows of new and old.
  #              old = data from which transfer is taken and to which new is compared
  #              transfer = part to transfer from old to new
  #              new.keep = data to keep in new (after matching) without using it for the matching process
  memory.limit(4095)
  old <- as.data.frame(old)
  new <- as.data.frame(new)
  transfer <- as.data.frame(transfer)
  if(!is.null(new.keep)) { 
    new.keep <- as.data.frame(new.keep)
    if(is.null(colnames(new.keep))) colnames(new.keep) <- paste("new.keep",1:ncol(new.keep),sep=".")
  }
  if (ncol(old)!=ncol(new)) stop("The matrices to match need to have the same number of columns (as the column variables are used for matching).")
  if (nrow(transfer)!=nrow(old)) stop("The matrix to be transferred needs to have the same number of rows as has the old matrix.")
  if (any(colnames(old)!=colnames(new))) warning("The matrices to match do not have the same column names. The matrices will be matched anyway.", call.=T)
  
  d <- rbind(old,new)
  if (scaling==TRUE) d <- scale(d)
  d <- as.matrix(dist(d, method=distance.method)) 
  d <- d[1:nrow(old),(nrow(old)+1):ncol(d),drop=FALSE]
  rownames <- c(1:nrow(d))
  d <- cbind(rownames,d)
  z <- unname(apply(d[,2:ncol(d),drop=FALSE],2,function(x) (which(x==min(x))[1])))
  transferred <- data.frame(matrix(nrow=nrow(new), ncol=ncol(transfer))); colnames(transferred) <- NA
  transferred[1:nrow(transferred),] <- transfer[z,]; colnames(transferred) <- colnames(transfer)
  if(!is.null(new.keep)) result <- cbind(new,new.keep,transferred) else result <- cbind(new,transferred)
  #if(!is.null(new.keep)) colnames(result)[(ncol(new)+1):(ncol(new)+1+)] 
  return(result)
}
####

if(FALSE) similarity.matching.DELETE <- function(old, new, transfer, new.keep=NULL, distance.method="euclidean", scaling=FALSE) {
  # This is the old version with matrices instead of data.frames which causes mess, when there are characters in the orignial data.frame
  # Explanation: new = data to which "transfer" should be transferred from old by looking at similarity between rows of new and old.
  #              old = data from which transfer is taken and to which new is compared
  #              transfer = part to transfer from old to new
  #              new.keep = data to keep in new (after matching) without using it for the matching process
  memory.limit(4095)
  old <- as.matrix(old)
  new <- as.matrix(new)
  transfer <- as.matrix(transfer)
  if(!is.null(new.keep)) { 
    new.keep <- as.matrix(new.keep)
    if(is.null(colnames(new.keep))) colnames(new.keep) <- paste("new.keep",1:ncol(new.keep),sep=".")
  }
  if (ncol(old)!=ncol(new)) stop("The matrices to match need to have the same number of columns (as the column variables are used for matching).")
  if (nrow(transfer)!=nrow(old)) stop("The matrix to be transferred needs to have the same number of rows as has the old matrix.")
  if (any(colnames(old)!=colnames(new))) warning("The matrices to match do not have the same column names. The matrices will be matched anyway.", call.=T)
  
  d <- rbind(old,new)
  if (scaling==TRUE) d <- scale(d)
  d <- as.matrix(dist(d, method=distance.method)) 
  d <- d[1:nrow(old),(nrow(old)+1):ncol(d)]
  rownames <- c(1:nrow(d))
  d <- cbind(rownames,d)
  z <- unname(apply(d[,2:ncol(d)],2,function(x) (which(x==min(x))[1])))
  transferred <- matrix(nrow=nrow(new), ncol=ncol(transfer))
  transferred[1:nrow(transferred),] <- transfer[z,]
  ifelse(!is.null(new.keep), result <- cbind(new,new.keep,transferred), result <- cbind(new,transferred))
  #if(!is.null(new.keep)) colnames(result)[(ncol(new)+1):(ncol(new)+1+)] 
  
  return(result)
}
#old <- matrix(c(1,3,4,7,1,3,9,5,1,3,5,9,3,5,6,2,3,4,5,8,9), ncol=3);new <- matrix(c(2,5,4,6,8,6,4,6,8,0,8,4,3,5,3), ncol=3);transfer <- c(100,200,300,400,500,600,700)
#similarity.matching(old,new,transfer)

####
if(FALSE) similarity.matching.validation.orig.DELETE <- function(data=NULL, grouping=NULL, distance.method="euclidean", scaling=FALSE, divide=TRUE, runs=10, new=NULL,old=NULL,transfer.grouping=NULL,compare.grouping=NULL) {
  # In the new version (above) lda.validation is used instead.
  data.orig <- data
  grouping.orig <- grouping
  if(!is.null(data)) sizes.orig <- tapply(grouping,grouping,length)
  obs.total.orig <- nrow(data.orig)
  
  crosstable.ls <- n.right.detailed.ls <- proportion.result.ls <- list()
  if(!divide) runs <- 1
  for(j in 1:runs) {
    
    data <- data.orig
    grouping <- grouping.orig
    
    if(divide) {
      divided.groups <- predict.samples <- list()
      for(i in unique(grouping)) {
        divided.groups[[i]] <- which(grouping==i)
        predict.samples[[i]] <- sample(divided.groups[[i]], round(length(divided.groups[[i]])/2))
      }
      predict.samples <- do.call("c",predict.samples)
      
      predict.set <- data[predict.samples,]
      predict.grouping <- grouping[predict.samples]
      train.set <- data[-predict.samples,]
      train.grouping <- grouping[-predict.samples]
      
      matching <- similarity.matching(predict.set, train.set, train.grouping, distance.method="euclidean", scaling)
      prediction <- matching[,ncol(matching)]
      grouping <- predict.grouping
    }
    
    if(!divide) {
      matching <- similarity.matching(new, old, transfer.grouping, distance.method="euclidean", scaling)
      prediction <- matching[,ncol(matching)]
      grouping <- compare.grouping
      data <- new
    }
    
    crosstable <- table(prediction, grouping)
    obs.total <- length(grouping)
    sizes <- tapply(grouping,grouping,length)
    
    crosstable.false <- crosstable
    diag(crosstable.false) <- 0
    n.false <- sum(crosstable.false)
    n.right <- obs.total-n.false
    prop.false <-  n.false / nrow(data)
    prop.right <- 1-prop.false
    
    prop.right.detailed <- numeric(0)
    n.right.detailed <- matrix(nrow=2, ncol=length(sizes))
    n.right.detailed[1,] <- diag(crosstable)
    n.right.detailed[2,] <- sizes
    prop.right.detailed <- n.right.detailed[1,]/sizes
    dimnames(n.right.detailed) <- list(c("model size","real size"),c(sort(unique(grouping))))
    n.right.detailed <- cbind(n.right.detailed,obs.total=rep(obs.total,2),right=c(n.right,0),false=c(n.false,0))
    
    probs <- sizes/obs.total
    n.hypo <- probs*sizes
    n.right.hypo <- sum(n.hypo)
    prop.right.hypo.detailed <- n.hypo/sizes
    prop.right.hypo <- n.right.hypo/obs.total
    
    proportion.table.detailed <- rbind(prop.right.detailed,prop.right.hypo.detailed); dimnames(proportion.table.detailed) <- list(c("actually right","right by chance"),c(sort(unique(grouping))))
    proportion.table <- rbind(prop.right,prop.right.hypo); dimnames(proportion.table) <- list(c("actually right","right by chance"),c("overall"))
    proportion.result <- cbind(proportion.table.detailed,  proportion.table)
    
    crosstable.ls[[j]] <- crosstable
    n.right.detailed.ls[[j]] <- n.right.detailed
    proportion.result.ls[[j]] <- proportion.result    
  }
  crosstable <- round(  Reduce("+",crosstable.ls)/runs  )
  n.right.detailed <- round(  Reduce("+",n.right.detailed.ls)/runs  )
  proportion.result <- round(  100* Reduce("+",proportion.result.ls)/runs , 1)
  
  if(!divide) result <- list(crosstable=crosstable,sizes=n.right.detailed, proportions=proportion.result)
  if(divide) {
    result <- list(crosstable=crosstable,sizes=n.right.detailed)
    #n.right.detailed.x2 <- rbind(n.right.detailed[1,]*2, c(sizes.orig,obs.total.orig,0,0)); rownames(n.right.detailed.x2) <- c("model size","real size")
    result2 <- list(crosstable.x2=crosstable*2,sizes.x2=n.right.detailed*2, proportions=proportion.result)
    result <- c(result,result2) }
  
  return(result)  
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
  # Der Bereich über der Matrix-Diagonale wird mit dem selben unterhalb der Diagonale gefüllt, weil diese Teile der Matrix gleich sind
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
    sign[is.na(sign) & cors>=0] <- "§§§§"
    sign[is.na(sign)& cors<0]   <-  "§§§"
    cat("Export the table as .csv to Excel, then replace § by Alt+255\n")
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
    # Der Bereich über der Matrix-Diagonale wird mit dem selben unterhalb der Diagonale gefüllt, weil diese Teile der Matrix gleich sind
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
  
  # Im Fall, dass keine P-Werte berechnet werden konnten, werden diese Plätze in der Matrix als NA ausgegeben
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
predict.plm <- function(model, newdata=NULL){
  
  if(is.matrix(newdata)) newdata <- as.data.frame(newdata) else if(!is.null(newdata) & !is.data.frame(newdata)) stop("newdata must be NULL, matrix or data.frame!")
  if(is.null(mod$fitted.values)) mod$fitted.values <- mod$model[,1]-mod$residuals
  
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
    require(MASS)
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
  if(return.formula) form <- formula(form)
  return(form)
}


production.formula <- function(xnames,ynames,funcform=c("log","translog","log.rts.t_test","log.rts.loglik_test"),dist=FALSE,or=c("in","out"),output=c("formula","string")){
  # This function creates a formula that can be fed into frontier::sfa()
  # The function form can be log (Cobb-Douglas) or translog (Transcendental Logarithmic).
  # If dist=TRUE the distance based formula is formulated. This is needed if more than 2 inputs are used.
  # or=orientation (only used if dist=TRUE)
  
  if(FALSE){
    ynames <- paste("y",1:1,sep="")
    xnames <- paste("x",1:4,sep="")
    funcform="translog"
    dist <- FALSE
    or <- "in"
    output <- "formula"
  }
  
  output <- match.arg(output)
  funcform <- match.arg(funcform)
  or <- match.arg(or)
  nx <- length(xnames)
  ny <- length(ynames)
  
  if(funcform=="translog") {
    if(!dist){
      # See Bogetoft & Otto (2011) ch8 p241
      if(ny>1) stop("Without distance function only 1 output is allowed!")
      formy <- paste0("I(log(",ynames,"))")
      formx1 <- paste0("I(log(",xnames,"))",collapse=" + ")
      comb1 <- combn(xnames,2) # Cross-Interaction
      comb2 <- matrix(rep(xnames,2),nrow=2,byrow=TRUE) # Self-Interaction
      comb <- cbind(comb2,comb1)
      formx2 <- character()
      for(i in 1:ncol(comb)){
        formx2[i] <- paste0("I(1/2*", paste0("log(",comb[,i],")",collapse="*"),")" )
        # Consider this:
        #x1 <- 5; x2 <- 2
        #log(x1) * log(x2^(1/2))
        #log(x1^(1/2)) * log(x2)
        #1/2 * log(x1) * log(x2)
      }
      formx2 <- paste(formx2,collapse=" + ")
      formx <- paste( paste0(formx1," +") , formx2, collapse="")
      form <- paste0(formy,"  ~  ", formx, collapse= "")
    } else { # if(dist)
      if(or=="in") {
        # See Bogetoft & Otto (2011) ch8 p243
        formy <- paste0("I(log(1/",xnames[1],"))")
        formx1 <- paste0("I(log(",xnames[2:nx],"/",xnames[1],"))",collapse=" + ")
        formx2 <- paste0("I(log(",ynames,"))", collapse=" + ")
        
        comb1 <- combn(c(paste0(xnames[2:nx],"/",xnames[1]),ynames),2) # Cross-Interaction
        comb2 <- matrix(rep(paste0(xnames[2:nx],"/",xnames[1]),2),nrow=2,byrow=TRUE) # Self-Interaction x
        comb3 <- matrix(rep(ynames,2),nrow=2,byrow=TRUE) # Self-Interaction y
        comb <- cbind(comb2,comb1,comb3)
        # The order of first, second and third line is done like in Bogetoft & Otto (2011) ch8 p243
        first <-  which( apply(comb,2,function(x)length(grep("/",x))==2) )
        second <- which( apply(comb,2,function(x)length(grep("/",x))==0) )
        third <-  which( apply(comb,2,function(x)length(grep("/",x))==1) )
        formx3 <- character()
        for(i in 1:ncol(comb)){
          formx3[i] <- paste0("I(1/2*", paste0("log(",comb[,i],")",collapse="*"),")" )
        }
        formx3 <- paste( formx3[c(first,second,third)], collapse=" + ")
        
        formx1 <- paste0(formx1," + ")
        formx2 <- paste0(formx2," + ")
        formx <- paste0(formx1,formx2,formx3,collapse="")
        form <- paste0(formy,"  ~  ",formx,collapse="")
        
      } else { # if(or=="out")
        # See Bogetoft & Otto (2011) ch8 p244
        formy <- paste0("I(log(1/",ynames[1],"))")
        formx1 <- paste0("I(log(",xnames,"))",collapse=" + ")
        if(ny>1) {
          formx2 <- paste0("I(log(",ynames[2:ny],"/",ynames[1],"))",collapse=" + ")
        } else {
          formx2 <- ""
        }
        if(ny>1) {
          comb1 <- combn(c(paste0(ynames[2:ny],"/",ynames[1]),xnames),2) # Cross-Interaction
          comb2 <- matrix(rep(paste0(ynames[2:ny],"/",ynames[1]),2),nrow=2,byrow=TRUE) # Self-Interaction y
          comb3 <- matrix(rep(xnames,2),nrow=2,byrow=TRUE) # Self-Interaction x
          comb <- cbind(comb3,comb2,comb1)
        } else {
          comb1 <- combn(xnames,2) # Cross-Interaction
          comb2 <- matrix(rep(xnames,2),nrow=2,byrow=TRUE) # Self-Interaction x
          comb <- cbind(comb2,comb1)
        }
        # The order of first, second and third line is done like in Bogetoft & Otto (2011) ch8 p244
        first <-  which( apply(comb,2,function(x)length(grep("/",x))==0) )
        second <- which( apply(comb,2,function(x)length(grep("/",x))==2) )
        third <-  which( apply(comb,2,function(x)length(grep("/",x))==1) )
        formx3 <- character()
        for(i in 1:ncol(comb)){
          formx3[i] <- paste0("I(1/2*", paste0("log(",comb[,i],")",collapse="*"),")" )
        }
        formx3 <- paste( formx3[c(first,second,third)], collapse=" + ")
        
        formx1 <- paste0(formx1," + ")
        if(formx2!="") formx2 <- paste0(formx2," + ")
        formx <- paste0(formx1,formx2,formx3,collapse="")
        form <- paste0(formy,"  ~  ",formx,collapse="")
      }
    } 
  } else if(funcform=="log")  {
    if(!dist) {
      if(ny>1) stop("Without distance function only 1 output is allowed!")
      formy <- paste0("I(log(",ynames,"))")
      formx <- paste0("I(log(",xnames,"))",collapse=" + ")
      form <- paste0(formy, "  ~  ", formx)
      
    } else { # if(dist)
      # see Bogetoft & Otto (2011) ch 8 p 237
      if(or=="in"){
        if(nx>1) {
          formx1 <- c(paste(xnames[2:nx] ,"/",xnames[1],sep=""), ynames)
        } else {
          formx1 <- ynames
        }
        formx <- paste( paste("I(log(",formx1,"))",sep=""), collapse=" + ")
        formy1 <- paste("1/",xnames[1],sep="")
        formy <- paste("I(log(",formy1,"))",sep="")
      } else { # if(or="out")
        # see Bogetoft & Otto (2011) ch 8 p 239. Attention! Specification on p 239 is wrong!!!
        if(ny>1){
          formx1 <- c( paste(ynames[2:ny],"/",ynames[1],sep=""), xnames)
        } else {
          formx1 <- xnames
        }
        formx <- paste( paste("I(log(",formx1,"))",sep=""), collapse=" + ")
        formy1 <- paste("1/",ynames[1],sep="")
        formy <- paste("I(log(",formy1,"))",sep="")
      }
      form <- paste0(formy,"  ~  ",formx,collapse="")
    }
  } else if(funcform=="log.rts.t_test") {
    # See Bogetoft & Otto (2011) ch8 p255
    # Check if the Parameter of the last input variable (no quotient) is significantly different from 0 (Pr|>t|).
    # coef > (<) 0  means positive (negative) scale effects.
    if(ny>1) stop("For log_rts_check only 1 output is allowed!")
    formy <- paste0("I(log(",ynames,"/",xnames[1],"))")
    formx <- paste0("I(log(",xnames[2:length(xnames)],"/",xnames[1],"))",collapse=" + ")
    formx <- paste0(formx," + ", paste0("I(log(",xnames[1],"))") ,collapse="")
    form <- paste0(formy, "  ~  ", formx)
  } else if(funcform=="log.rts.loglik_test"){
    # Used in function sfa.rts.test()
    # The formula is used for the null hypothesis of constant returns to scale! We divide all parameters by one input. Then we leave this input completely away instead of summing all coefficients of the other inputs up (see Bogetoft & Otto ch8 p255 and p256 ).
    if(ny>1) stop("For log_rts_check only 1 output is allowed!")
    formy <- paste0("I(log(",ynames,"/",xnames[1],"))")
    formx <- paste0("I(log(",xnames[2:length(xnames)],"/",xnames[1],"))",collapse=" + ")
    form <- paste0(formy, "  ~  ", formx)
  }
  if(output=="formula") form <- formula(form)
  return(form)
}


long.string <- function(x){
  if( mode(x)=="call" ) {    # If x is a formula!
    x <- paste(x[2],x[1],x[3], collapse="")
    slashpos <- gregexpr(pattern ='\n',x)[[1]][[1]]
    x <- paste0( substr(x,1,(slashpos-1)), substr(x,(slashpos+5),nchar(x)) )
  }
  if(length(x)>1 & any(nchar(x)>1)) { x <- paste0(x,collapse="")  }
  nchar.x <- nchar(x)
  xnew <- character()
  for(i in 1:nchar.x) xnew[i] <- substr(x,i,i)
  return(xnew)
}

sep.string <- function(x,sign,keep.sign=TRUE){
  if( mode(x)=="call" ) {    # If x is a formula!
    x <- paste(x[2],x[1],x[3], collapse="")
    slashpos <- gregexpr(pattern ='\n',x)[[1]][[1]]
    x <- paste0( substr(x,1,(slashpos-1)), substr(x,(slashpos+5),nchar(x)) )
  }
  if(length(x)==1) x <- long.string(x)
  sign.pos <- which(x%in%sign)
  sign.pos <- c(0,sign.pos,length(x)+1)
  nblocks <- length(sign.pos)-1
  blocks <- character(nblocks)
  if(!keep.sign)  {
    for(i in 1:nblocks)   blocks[i] <- paste0(x[(sign.pos[i]+1):(sign.pos[i+1]-1) ], collapse="")
  } else if(keep.sign) {
    for(i in 1:(nblocks-1))   blocks[i] <- paste0(x[(sign.pos[i]+1):(sign.pos[i+1]  ) ], collapse="")
    for(i in       nblocks)   blocks[i] <- paste0(x[(sign.pos[i]+1):(sign.pos[i+1]-1  ) ], collapse="")
  }
  return(blocks)
}

#char <- c("0123456789", "9876543210")
substr.rev <- function(char, start, end, reverse=FALSE) {
  # Reverse substring
  
  if(any(start>end)) stop("All start must be smaller equal end.")
  
  nchar_char <- nchar(char)
  res <- substr(char, nchar_char-end+1, nchar_char-start+1)
  if(reverse) {
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
  if(!is.null(dim(pattern))) stop("pattern (first argument) must be a value or vector, not data.frame.")
  # This function is a convenience function to find columns in the Grundlagenbericht.
  # Use it like this: find.col("jae", dat1)
  return( colnames(dat)[ find.string(pattern=pattern, x=colnames(dat), ignore.case=ignore.case) ] )
}

find.gb.col <- function(pattern, dat=gb, ignore.case=TRUE, ...){
  # This function is a convenience function to find columns in the Grundlagenbericht.
  # Use it like this: find.gb.col("jae")
  return( colnames(dat)[ find.string(pattern=pattern, x=colnames(dat), ignore.case=ignore.case) ] )
}

gbc <- function(...){
  # Wrapper function (short)
  find.gb.col(...)
}

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
  require(agricolae)
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
  require(pgirmess)
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
  
  krsk <- kruskal2.for.kruskal.groups(y,grouping,group=FALSE,p.adj=p.adj)
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
  
  # Schauen, wie viel mal man die nächste Schleife (unter mat <-...) laufen lassen muss
  comb2 <- comb[,sig.true,drop=FALSE]
  s <- 1
  comb2.new <- comb2
  while(dim(comb2.new)[2]>0) {
    i.is.in <- apply(comb2.new==ranking[s],2,function(x)any(x))
    comb2.new <- comb2.new[,!i.is.in,drop=FALSE]
    s <- s+1
  }
  
  # THIS IS THE HEART OF THE FUNCTION (makes the grouping a, ab, a, ...)
  # ---------------------------------
  mat <- matrix(NA,nrow=length(grp),ncol=s)
  count.auslassen <- 0
  for (i in 1:s){
    # Falls die positiven signifikanten Unterschiede für eine Gruppe genau dieselbe sind, wie für die letzte, dann wird der j-Loop übersprungen
    # Falls in der folge nur noch negative signifikante Unterschiede kommen, werden diese übersprüngen, da diese ja schon bei den positiven Signifikanzen berücksichtigt wurden. Dann läuft der Loop leer durch, bis er fertig ist.
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
        # Es wird dasjenige Element ausgewählt, welches der Kombination von i mit j entspricht.
        look <- looki & lookj
        ok <- look & sig.true
        # Wenn die gewählte Kombination signifikant ist, wird ein " " gesetzt, sonst der Buchstabe
        if(any(ok)) mat[j,i-count.auslassen] <- " " else mat[j,i-count.auslassen] <- char[i-count.auslassen] 
        # Zur Kontrolle:
        # cat("###\ni= ",i,"\ngrouping= ",ranking[i],"\n")
        # cat("j= ",j,"\n")
        # print(mat)
      }
      # In der Spalte, in der man gerade ist, muss die Reihe der Geprüften Gruppe immer ein Buchstabe enthalten!
      mat[ranking[i],i-count.auslassen] <- char[i-count.auslassen]
      # Die Buchstaben werden nur nach rechts gesetzt, nicht aber nach links zu den Gruppen, die schon kontrolliert wurden. Die kontrollierten Gruppen werden mit einem Leerfeld belegt.
      if(i>1) mat[ranking[1:(i-1)],i-count.auslassen] <- " "
    } else { # Falls die Signifikanten unterschiede für eine Gruppe genau dieselben sind, wie für die letzte, dann wird der j-Loop übersprungen. Siehe if() oben.
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
#x <- kruskal2.for.kruskal.groups(y,trt); x
####
kruskal2.for.kruskal.groups <- function(y, trt,alpha=0.05,p.adj = c("none","holm", "hochberg", "bonferroni", "BH", "BY", "fdr"),group=FALSE,main=NULL) {
  require(agricolae)
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
  
  # Schauen, wie viel mal man die nächste Schleife (unter mat <-...) laufen lassen muss
  comb2 <- comb[,sig.true,drop=FALSE]
  s <- 1
  comb2.new <- comb2
  while(dim(comb2.new)[2]>0) {
    i.is.in <- apply(comb2.new==ranking[s],2,function(x)any(x))
    comb2.new <- comb2.new[,!i.is.in,drop=FALSE]
    s <- s+1
  }
  
  # THIS IS THE HEART OF THE FUNCTION (makes the grouping a, ab, a, ...)
  # ---------------------------------
  mat <- matrix(NA,nrow=length(grp),ncol=s)
  count.auslassen <- 0
  for (i in 1:s){
    # Falls die positiven signifikanten Unterschiede für eine Gruppe genau dieselbe sind, wie für die letzte, dann wird der j-Loop übersprungen
    # Falls in der folge nur noch negative signifikante Unterschiede kommen, werden diese übersprüngen, da diese ja schon bei den positiven Signifikanzen berücksichtigt wurden. Dann läuft der Loop leer durch, bis er fertig ist.
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
        # Es wird dasjenige Element ausgewählt, welches der Kombination von i mit j entspricht.
        look <- looki & lookj
        ok <- look & sig.true
        # Wenn die gewählte Kombination signifikant ist, wird ein " " gesetzt, sonst der Buchstabe
        if(any(ok)) mat[j,i-count.auslassen] <- " " else mat[j,i-count.auslassen] <- char[i-count.auslassen] 
        # Zur Kontrolle:
        # cat("###\ni= ",i,"\ngrouping= ",ranking[i],"\n")
        # cat("j= ",j,"\n")
        # print(mat)
      }
      # In der Spalte, in der man gerade ist, muss die Reihe der Geprüften Gruppe immer ein Buchstabe enthalten!
      mat[ranking[i],i-count.auslassen] <- char[i-count.auslassen]
      # Die Buchstaben werden nur nach rechts gesetzt, nicht aber nach links zu den Gruppen, die schon kontrolliert wurden. Die kontrollierten Gruppen werden mit einem Leerfeld belegt.
      if(i>1) mat[ranking[1:(i-1)],i-count.auslassen] <- " "
    } else { # Falls die Signifikanten unterschiede für eine Gruppe genau dieselben sind, wie für die letzte, dann wird der j-Loop übersprungen. Siehe if() oben.
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
  
  # Schauen, wie viel mal man die nächste Schleife (unter mat <-...) laufen lassen muss
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
    # Falls die positiven signifikanten Unterschiede für eine Gruppe genau dieselbe sind, wie für die letzte, dann wird der j-Loop übersprungen
    # Falls in der folge nur noch negative signifikante Unterschiede kommen, werden diese übersprüngen, da diese ja schon bei den positiven Signifikanzen berücksichtigt wurden. Dann läuft der Loop leer durch, bis er fertig ist.
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
        # Es wird dasjenige Element ausgewählt, welches der Kombination von i mit j entspricht.
        look <- looki & lookj
        ok <- look & sig.true
        # Wenn die gewählte Kombination signifikant ist, wird ein " " gesetzt, sonst der Buchstabe
        if(any(ok)) mat[j,i-count.auslassen] <- " " else mat[j,i-count.auslassen] <- char[i-count.auslassen] 
        # Zur Kontrolle:
        # cat("###\ni= ",i,"\ngrouping= ",ranking[i],"\n")
        # cat("j= ",j,"\n")
        # print(mat)
      }
      # In der Spalte, in der man gerade ist, muss die Reihe der Geprüften Gruppe immer ein Buchstabe enthalten!
      mat[ranking[i],i-count.auslassen] <- char[i-count.auslassen]
      # Die Buchstaben werden nur nach rechts gesetzt, nicht aber nach links zu den Gruppen, die schon kontrolliert wurden. Die kontrollierten Gruppen werden mit einem Leerfeld belegt.
      if(i>1) mat[ranking[1:(i-1)],i-count.auslassen] <- " "
    } else { # Falls die Signifikanten unterschiede für eine Gruppe genau dieselben sind, wie für die letzte, dann wird der j-Loop übersprungen. Siehe if() oben.
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
#y <- results[,"Lehre.minus.Ant04"]; trt <- results[,"Clusternummer"]; p.adj="holm"; perc=TRUE; digits=1; sig.level=0.05; median.mean=c("mean"); ranked=c("no"); print.result=TRUE# k1 <- kruskal.groups(y,trt); print.default(k1); k1
#a <- chisq.groups(y,trt,p.adj="holm"); a
chisq.groups <- function(y, trt, sig.level=0.05, p.adj="holm", perc=TRUE, digits=1, ranked=c("no","character.left","character.right","mean","intern"), print.result=TRUE ) {
  # Do the grouping (a, ab, b, bc,...) for grouped data (1,2,3,4,5,...) with binary categorial variables (0,1). For example a cluster analysis was conducted and now in every cluster there is a certain proportion of observations in the one category and in the other category.
  # i.e. a 2x2 contingency table is checked for significant differences with the chisq.test() function. The data is grouped by the sig.groups() function.
  # y: the data vector, trt (treatment): the grouping vecotr, perc=TRUE: The proportions are given as percentages, digits: digits, ranked: should the output table be sorted according to the proportions..?, print.result: should the result be printed?
  grouping <- trt
  ranked <- match.arg(ranked)
  
  grp <- sort(unique(grouping))
  comb <- combn(grp,2); comb.names <- apply(comb,2,function(x)paste(x,collapse="-"))
  mean <- tapply(y,grouping,function(x)mean(x,na.rm=TRUE))
  tables <- table(list(grouping=grouping, proportion=y))
  
  diffs <- sig <- rep(NA,ncol(comb)); names(diffs) <- names(sig) <- comb.names
  for(i in 1:ncol(comb)) {
    diffs[i] <- mean[comb[1,i]] - mean[comb[2,i]]
    sig[i] <- chisq.test( tables[comb[,i],] )$p.value
  }
  sig <- p.adjust(sig, method=p.adj)
  # Same procedure as in the Kruskal-Wallis-Test: Multiple comparisons between pairs of groups are only done if there is any difference between all of the groups. IF all sig[] are set to 1, then the grouping c(a,a,a,a,...) is produced in the sig.group() function.
  if(all.nonsig <- chisq.test(tables)$p.value>=sig.level) {
    sig.orig <- sig
    sig[] <- 1
  }
  
  if(perc==TRUE) digits <- digits+2
  char.grouping <- sig.groups(y, grouping, sig, sig.level=sig.level ,digits=digits, median.mean="mean", ranked=ranked, print.result=FALSE)
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
  dat <- matrix(NA,nrow=1000, ncol=8)
  dat[,1] <- rnorm(1000,m,sd)
  dat[,2] <- rnorm(1000,m,sd)^(-0.5);     (m^(-0.5))^-2
  dat[,3] <- rnorm(1000,m,sd)^(-1);       (m^(-1))^-1
  dat[,4] <- rnorm(1000,m,sd)^(-2);       (m^(-2))^-(0.5)
  dat[,5] <- rnorm(1000,m,sd)^2;          (m^(2))^(0.5)
  dat[,6] <- rnorm(1000,m,sd)^(0.5);      (m^(0.5))^(2)
  dat[,7] <- exp(rnorm(1000,m,sd));       exp(log(m))
  dat[,8] <- log(rnorm(1000,m,sd));       log(exp(m))
  
  colnames(dat) <- c("x",
                     "x^(-0.5)",
                     "x^(-1)",
                     "x^(-2)",
                     "x^2",
                     "x^(0.5)",
                     "exp(x)",
                     "log(x)")
  normalize.qqplot(dat,window=TRUE)
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

normalize <- function(data, sig.level=0.05,qq=FALSE,window=FALSE,...) {
  if(!any(c(is.matrix(data), is.data.frame(data)))) stop("Data must be a matrix or data.frame.")
  if(any(  c(data[!is.na(data[]==Inf)]==Inf, data[!is.na(data[]==-Inf)]==-Inf)  ))  stop("Infinite numbers are not allowed")
  
  data <- as.data.frame(data)
  lillie.test <- function (x)         # lillie.test{nortest} modified. Now it also works with data containing always the same number.
  {
    DNAME <- deparse(substitute(x))
    x <- sort(x[complete.cases(x)])
    n <- length(x)
    if (n > 4) {                                                  # change from original here
      
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
      if (!is.na(pvalue)) {                                           # change from original here
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
      } else {pvalue <- 0}                                            # change from original here
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
  data <- data[,minsize,drop=F]
  
  # 0 ok without operation
  # 1 ok with ^(-2)
  # 2 ok with ^(-1)
  # 3 ok with ^(-0.5)
  # 4 ok with log()
  # 5 ok with ^(0.5)
  # 6 ok with ^(2)
  
  if(qq) qqplot.multiple(data,colnames(data),window=window,mt="orig")#,...)
  a0 <- apply(data,2,function(x)lillie.test(x)$p.value >=sig.level)
  a0.ok <- names(a0[a0[]==TRUE]); if(length(a0.ok)==0) a0.ok <- NULL
  a0.next <- names(a0[a0[]==FALSE])
  # Einschub: alle Werte positiv machen, zusaetzlich wurde ganz unten fuer data[,a0.ok] data.orig[,a0.ok] eingefuegt
  mins <- apply(data,2,function(x) if (min(x,na.rm=TRUE)<0) -min(x,na.rm=TRUE) else 0)
  min.matrix <- t(matrix( rep(mins,nrow(data)), ncol=nrow(data) ))
  data <- data + min.matrix
  # Ende Einschub
  data.new <- data[,a0.next,drop=F]^(-2)
  data.new[data.new==Inf] <- 10^12; data.new[data.new==-Inf] <- -10^12
  if(qq) qqplot.multiple(data.new,colnames(data.new),window=window,mt="^(-2)")#,...)
  
  
  if(ncol(data.new)>0){
    a1 <- apply(data.new,2,function(x)lillie.test(x)$p.value >=sig.level)
    a1.ok <- names(a1[a1[]==TRUE]); if(length(a1.ok)==0) a1.ok <- NULL
    a1.next <- names(a1[a1[]==FALSE])
    data.new <- data[,a1.next,drop=F]^(-1)
    data.new[data.new==Inf] <- 10^12; data.new[data.new==-Inf] <- -10^12
    if(qq) qqplot.multiple(data.new,colnames(data.new),window=window,mt="^(-1)")#,...)
  } else {a1.ok <- NULL}
  
  if(ncol(data.new)>0){
    a2 <- apply(data.new,2,function(x)lillie.test(x)$p.value >=sig.level)
    a2.ok <- names(a2[a2[]==TRUE]); if(length(a2.ok)==0) a2.ok <- NULL
    a2.next <- names(a2[a2[]==FALSE])
    data.new <- data[,a2.next,drop=F]^(-0.5)
    data.new[data.new==Inf] <- 10^12; data.new[data.new==-Inf] <- -10^12
    if(qq) qqplot.multiple(data.new,colnames(data.new),window=window,mt="^(-0.5)")#,...)
  } else {a2.ok <- NULL}
  
  if(ncol(data.new)>0){
    a3 <- apply(data.new,2,function(x)lillie.test(x)$p.value >=sig.level)
    a3.ok <- names(a3[a3[]==TRUE]); if(length(a3.ok)==0) a3.ok <- NULL
    a3.next <- names(a3[a3[]==FALSE])
    data.new <- log(data[,a3.next,drop=F])
    data.new[data.new==Inf] <- 10^12; data.new[data.new==-Inf] <- -10^12
    if(qq) qqplot.multiple(data.new,colnames(data.new),window=window,mt="log()")#,...)
  } else {a3.ok <- NULL}
  
  if(ncol(data.new)>0){
    a4 <- apply(data.new,2,function(x)lillie.test(x)$p.value >=sig.level)
    a4.ok <- names(a4[a4[]==TRUE]); if(length(a4.ok)==0) a4.ok <- NULL
    a4.next <- names(a4[a4[]==FALSE])
    data.new <- data[,a4.next,drop=F]^(0.5)
    data.new[data.new==Inf] <- 10^12; data.new[data.new==-Inf] <- -10^12
    if(qq) qqplot.multiple(data.new,colnames(data.new),window=window,mt="^(0.5)")#,...)
  } else {a4.ok <- NULL}
  
  if(ncol(data.new)>0){
    a5 <- apply(data.new,2,function(x)lillie.test(x)$p.value >=sig.level)
    a5.ok <- names(a5[a5[]==TRUE]); if(length(a5.ok)==0) a5.ok <- NULL
    a5.next <- names(a5[a5[]==FALSE])
    data.new <- data[,a5.next,drop=F]^(2)
    data.new[data.new==Inf] <- 10^12; data.new[data.new==-Inf] <- -10^12
    if(qq) qqplot.multiple(data.new,colnames(data.new),window=window,mt="^(2)")#,...)
  } else {a5.ok <- NULL}
  
  if(ncol(data.new)>0){
    a6 <- apply(data.new,2,function(x)lillie.test(x)$p.value >=sig.level)
    a6.ok <- names(a6[a6[]==TRUE]); if(length(a6.ok)==0) a6.ok <- NULL
    a6.next <- names(a6[a6[]==FALSE])
    data.new <- exp(data[,a6.next,drop=F])
    data.new[data.new==Inf] <- 10^12; data.new[data.new==-Inf] <- -10^12
    if(qq) qqplot.multiple(data.new,colnames(data.new),window=window,mt="exp()")#,...)
  } else {a6.ok <- NULL}
  
  if(ncol(data.new)>0){
    a7 <- apply(data.new,2,function(x)lillie.test(x)$p.value >=sig.level)
    a7.ok <- names(a7[a7[]==TRUE]); if(length(a7.ok)==0) a7.ok <- NULL
    a7.next <- names(a7[a7[]==FALSE])
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
  
  if (length(a7.next)>0 | length(too.small)>0) {
    data.mixed <- data.new
    if (length(a7.next)>0) {
      data.mixed <- cbind(data.mixed, data.orig[,a7.next])
      colnames(data.mixed)[(ncol(data.mixed)-length(a7.next)+1):ncol(data.mixed)] <- a7.next
    }
    if (length(too.small)>0) {
      data.mixed <- cbind(data.mixed, data.orig[,too.small])
      colnames(data.mixed)[(ncol(data.mixed)-length(too.small)+1):ncol(data.mixed)] <- too.small
    }
  }
  qqplot.multiple(data.mixed,colnames(data.mixed),window=window,mt="mixed")
  explanation <- "normal: data was already normally distributed, reciprocal.square: ^(-2) was needed to normalize, reciprocal: ^(-1) ..., reciprocal.square.root: ^(-0.5) ..., log: log() ..., square.root: ^0.5 ..., square: ^2 ..., exp: exp() ..."
  result <- list(normalized.data=data.new, mixed.normalized.data=data.mixed,normal=a0.ok, reciprocal.square=a1.ok, reciprocal=a2.ok, reciprocal.square.root=a3.ok, log=a4.ok, square.root=a5.ok, square=a6.ok, exp=a7.ok, never.normal=a7.next, not.enough.observations.for.analysis=too.small, explanation=explanation)
  return(result)
}
####
qqplot.multiple <- function(data,variables,plotrows=5,mar=c(2.1,2.1,2.1,2.1),window=FALSE,mt=NULL,...){
  par.orig <- par()$mar; mfrow.orig <- par()$mfrow; on.exit(par(mar=par.orig, mfrow=mfrow.orig))
  
  if(window) windows()
  nvariables <- length(variables)
  par(mar=mar, mfrow=c(plotrows,if(nvariables%%plotrows==0) nvariables/plotrows else floor(nvariables/plotrows)+1 ))
  for(i in 1:nvariables) {
    dat <- as.numeric( data[,variables[i]] )
    qqnorm(dat,pch=20,main=variables[i])
    qqline(dat)
  }
}
####

#### EFFICIENCY ANALYSIS ####
dea.info <- function() {
  cat("
      Wenn die technische Effizienz zweier Länder miteinander verglichen wird, müssen die Preise angepasst werden. Die Kosten-/Einnahmen-Positionen entsprechen dann physischen Einheiten, weil diese durch eine Division durch einheitliche Preise (=lineare Transformation) errechnet werden können.\n
      Soll die allokative Effizienz verschiedener Länder miteinander verglichen werden, muss mit den originalen Preisen gearbeitet werden. Eine Preisanpassung verfälscht das Ergebnis!\n
      Nach der Effizienzschätzung unbedingt einen pairs plot mit allen x und y sowie TE machen, um zu sehen, ob TE unabhängig von x und y ist. Dies ist vor allem wichtig für SFA. Siehe Bogetoft & Otto (2011), ch8 p261")
}
####
dea.plot.multi <- function(x,y,or="in",RTS="vrs",mar=c(2.8, 2.5, 1, 1)+0.1,line.lab=1.8,cex.lab=0.7,...) { # ,wp (würde gebraucht für Isokostenlinie. Funktioniert aber nicht.)
  library(Benchmarking)
  par.orig <- par()$mar; mfrow.orig <- par()$mfrow; on.exit(par(mar=par.orig, mfrow=mfrow.orig))
  
  par(mar=mar)
  if(or=="in") {
    par(mfrow=c(ncol(x),ncol(x)))
    for(i in 1:ncol(x)){
      for(j in 1:ncol(x)){
        if(j==i) { plot(1,axes=FALSE,xlab="",ylab="",col="white")
        } else {dea.plot(x[,i],x[,j], ORIENTATION=or, RTS=RTS, xlab="", ylab="", ...)
                mtext(colnames(x)[i],side=1,line=line.lab,cex=cex.lab)
                mtext(colnames(x)[j],side=2,line=line.lab,cex=cex.lab)
        }
      }
    }
  } else if(or=="out") {
    par(mfrow=(c(ncol(y),ncol(y))))
    for(i in 1:ncol(y)){
      for(j in 1:ncol(y)){
        if(j==i) { plot(1,axes=FALSE,xlab="",ylab="",col="white")
        } else {dea.plot(y[,i],y[,j], ORIENTATION=or, RTS=RTS, xlab="", ylab="", ...)
                mtext(colnames(y)[i],side=1,line=line.lab,cex=cex.lab)
                mtext(colnames(y)[j],side=2,line=line.lab,cex=cex.lab)
        }
      }
    }
  } else if(or=="in-out") {
    par(mfrow=(c(ncol(y),ncol(x))))
    for(i in 1:ncol(y)){
      for(j in 1:ncol(x)){
        dea.plot(x[,j],y[,i], ORIENTATION=or, RTS=RTS, xlab="", ylab="", ...)
        mtext(colnames(x)[j],side=1,line=line.lab,cex=cex.lab)
        mtext(colnames(y)[i],side=2,line=line.lab,cex=cex.lab)
      }
    }
  }
}
####
#w <- xw; p <- yw; grouping=c(rep(1,floor(nrow(x))),rep(2,ceiling(nrow(x)))); or="in"; profit=FALSE
dea.ce <- function(x,y,w=NULL,p=NULL,grouping=NULL,or=c("in","out","in-out"),RTS="vrs",direct=NULL,profit=FALSE,effmax1=FALSE,effmin0=FALSE,short.result=TRUE,short.print=TRUE,correct.unrealistic.ce=TRUE) {
  # Reference: Bogetoft & Otto (2011): Benchmarking with DEA, SFA and R. chapter 4.8 page 103
  # or="in"; RTS="vrs"
  or <- match.arg(or)
  require(Benchmarking)
  #require(FEAR)
  x <- as.matrix(x); y <- as.matrix(y);
  if(is.null(grouping)) {
    grouping <- rep(1,nrow(x))
    sizes <- NULL
  } else {sizes <- table(grouping); sizes <- c(all=sum(sizes),sizes)}
  grouped <- length(unique(grouping))>1
  
  if(or%in%c("in","out") & profit) warning("It is recommended to choose in-out orientation in combination with profit optimization.", call. = FALSE, immediate.=TRUE)
  if(or=="in-out" & is.null(direct)) stop("For in-out orientation you have to specify direct. Usually: direct=cbind(x,y)")
  if(or=="in-out") profit <- TRUE
  if(or=="in"  & is.null(w)) stop("w must be specified for cost optimization")
  if(or=="out" & is.null(p)) stop("p must be specified for revenue optimization")
  if(profit & (is.null(p)|is.null(w))) stop("w & p must be specified for profit optimization")
  if(!is.null(w) & is.null(dim(w))) {if(or=="in") w <- matrix(w,ncol=ncol(x)) else w <- matrix(w,ncol=ncol(y))}; if(is.data.frame(w)) {w <- as.matrix(w)}
  if(!is.null(p) & is.null(dim(p))) {if(or=="in") p <- matrix(p,ncol=ncol(x)) else p <- matrix(p,ncol=ncol(y))}; if(is.data.frame(p)) {p <- as.matrix(p)}
  if(or=="in" | profit){
    if(ncol(w)!=ncol(x)) stop("ncol(w) and ncol(x) must be equal")
    if(nrow(w)!=1 & nrow(w)!=nrow(x)) stop("nrow(w) must either be 1 or nrow(x)")
  }
  if(or=="out" | profit){
    if(ncol(p)!=ncol(y)) stop("ncol(p) and ncol(y) must be equal")
    if(nrow(p)!=1 & nrow(p)!=nrow(y)) stop("nrow(p) must either be 1 or nrow(y)")
  }
  summasd <- function(x,na.rm=TRUE)  c(summary(x,na.rm=na.rm)[1:6],"SD"=sd(x,na.rm=na.rm))
  
  if(or=="in") {
    x.obs <- x
    c.obs <- apply( x.obs*w ,1,sum)
    opt   <- cost.opt(XREF=x,YREF=y,W=w, RTS=RTS)
    x.opt <- opt$xopt
    c.opt <- apply( x.opt*w ,1,sum) # das geht auch: cost.opt(x,y,w, RTS=RTS)$cost
    ce    <- c.opt/c.obs; ce <- round(ce,5)
    if(any(ce.gt.1 <- ce>1)) {
      cat("Warning:\nThere is/are", sum(ce.gt.1) ,"observation(s) with CE > 1:\n",unname(which(ce>1)))
      if(correct.unrealistic.ce) {
        cat("Observation was 'corrected'\n")
        c.obs[ce.gt.1] <- c.opt[ce.gt.1]  #
        x.obs[ce.gt.1,] <- x.opt[ce.gt.1,]
        ce <- c.opt/c.obs; ce <- round(ce,5)
      }
    }
    y.obs <- y
    y.opt <- NULL
    r.opt <- NULL
    r.obs <- NULL
    p.opt <- NULL
    p.obs <- NULL
  } else if(or=="out") {
    y.obs <- y
    r.obs <- apply( y.obs*p ,1,sum)
    opt   <- revenue.opt(XREF=x,YREF=y,P=p, RTS=RTS)
    y.opt <- opt$yopt
    r.opt <- apply( y.opt*p ,1,sum)
    ce    <- r.opt/r.obs; ce <- round(ce,5)
    if(any(ce.gt.1 <- ce<1)) {
      cat("Warning:\nThere is/are", sum(ce.gt.1) ,"observation(s) with CE < 1:\n",unname(which(ce<1)))
      if(correct.unrealistic.ce) {
        cat("Observation was 'corrected'\n")
        r.obs[ce.gt.1] <- r.opt[ce.gt.1]
        y.obs[ce.gt.1,] <- y.opt[ce.gt.1,]
        ce <- r.opt/r.obs; ce <- round(ce,5)
      }
    }
    x.obs <- x
    x.opt <- NULL
    c.opt <- NULL
    c.obs <- NULL
    p.opt <- NULL
    p.obs <- NULL
  } else if(profit) {
    x.obs <- x
    y.obs <- y
    p.obs <- apply( y.obs*p ,1,sum) - apply( x.obs*w ,1,sum)
    opt   <- profit.opt(XREF=x.obs,YREF=y.obs,W=w,P=p, RTS=RTS)
    p.opt <- opt$profit
    x.opt <- opt$xopt
    y.opt <- opt$yopt
    ce    <- p.obs/p.opt; ce <- round(ce,5); # Does not need to be bounded to [0;1] because values below 0 are also possible if the profit is negative. Reference: Coelli et al. (2005) An Introduction into Efficiency ...
    if(any(ce.gt.1 <- ce>1)) {
      cat("Warning:\nThere is/are", sum(ce.gt.1) ,"observation(s) with CE > 1:\n",unname(which(ce>1)))
      if(correct.unrealistic.ce) {
        cat("\nObservation was 'corrected'\n")
        p.obs[ce.gt.1] <- p.opt[ce.gt.1]
        x.obs[ce.gt.1,] <- x.opt[ce.gt.1,]
        y.obs[ce.gt.1,] <- y.opt[ce.gt.1,]
        ce <- p.opt/p.obs; ce <- round(ce,5)
      }
    }
    c.opt <- NULL
    c.obs <- NULL
    r.opt <- NULL
    r.obs <- NULL
  }
  
  if(is.null(direct)) {
    te <- Benchmarking::dea(x,y,RTS=RTS,ORIENTATION=or, FAST=TRUE)
  } else if(!is.null(direct) & or=="in") { 
    te <- 1- Benchmarking::dea(x,y,RTS=RTS,ORIENTATION=or, DIRECT=direct, FAST=TRUE)
  } else if(!is.null(direct) & or=="out") {
    te <- 1+ Benchmarking::dea(x,y,RTS=RTS,ORIENTATION=or, DIRECT=direct, FAST=TRUE)
  } else if(!is.null(direct) & or=="in-out") {
    te <- 1- Benchmarking::dea(x,y,RTS=RTS,ORIENTATION=or, DIRECT=direct, FAST=TRUE)
    # Interpretation of the original distance function: By how much can the output vector can be expanded
    # and the input vector be contracted and still be feasible.
    # Umwandlung Könnte je nach Definitions-Belieben auch so gemacht werden: Dann ist es wie bei Output-Orientierung
    # te <- 1+Benchmarking::dea(x,y,RTS=RTS,ORIENTATION=or, DIRECT=direct, FAST=TRUE) 
  }
  
  if(or=="out" & effmax1) { te <- 1/te; ce <- 1/ce 
  } #else if (or=="in-out" & !effmax1) {te <- 1/te; ce <- 1/ce}
  if(effmin0) ce[ce<0] <- 0
  ae <- ce/te
  
  # Calculating summaries and function output
  info <- paste(RTS," and ",or,"put orientation used for the analysis  // ", "CE = TE * AE", sep="")
  if(grouped) {
    eff.table <- cbind(ce,te,ae,grouping); colnames(eff.table) <- c("CE","TE","AE","grouping")
  } else {
    eff.table <- cbind(ce,te,ae); colnames(eff.table) <- c("CE","TE","AE")
  }
  
  summarylist <- list()
  if(grouped){
    for(i in 1:3) {
      summarylist[[i]] <- cbind( summasd(eff.table[,i]) ,do.call("cbind", tapply(eff.table[,i],grouping,function(x)summasd(x))))
      colnames(summarylist[[i]]) <- c("all",sort(unique(grouping)))
    }
    names(summarylist) <- colnames(eff.table)[1:3]
  } else {
    for(i in 1:3) {
      summarylist[[i]] <- matrix( summasd(eff.table[,i]), ncol=1)
      colnames(summarylist[[i]]) <- "all"
    }
    names(summarylist) <- colnames(eff.table)[1:3]
  }
  
  # Combining into the result
  if(!grouped & short.result) {
    summary <- apply(eff.table,2,function(x)summasd(x))
    result <- list(eff.table=eff.table,summary=summary,CE=summarylist$CE,TE=summarylist$TE,AE=summarylist$AE,x.opt=x.opt,x.obs=x.obs,y.opt=y.opt,y.obs=y.obs,c.opt=c.opt,c.obs=c.obs,r.opt=r.opt,r.obs=r.obs,p.opt=p.opt,p.obs=p.obs,grouping=grouping,sizes=sizes,info=info, print.info=list(short.result=short.result,short.print=short.print))
  } else {
    short.result <- FALSE
    result <- list(eff.table=eff.table,summary=NULL,   CE=summarylist$CE,TE=summarylist$TE,AE=summarylist$AE,x.opt=x.opt,x.obs=x.obs,y.opt=y.opt,y.obs=y.obs,c.opt=c.opt,c.obs=c.obs,r.opt=r.opt,r.obs=r.obs,p.opt=p.opt,p.obs=p.obs,grouping=grouping,sizes=sizes,info=info, print.info=list(short.result=short.result,short.print=short.print))  
  }
  
  class(result) <- "dea.ce"
  return(result)
}
print.dea.ce  <- function(x, digits=2, short.print=NULL,...)  {
  if(is.null(short.print)) short.print <- x$print.info$short.print
  if(x$print.info$short.result){
    if(short.print){ print(list(summary=round(x$summary[c(4,7),,drop=FALSE],digits) ))#, info=x$info))
    } else print(list(summary=round(x$summary,digits) ))#, info=x$info))
  } else {
    x$CE <- round(x$CE,digits); x$TE <- round(x$TE,digits); x$AE <- round(x$AE,digits)
    if(short.print){ print(list(CE=x$CE[c(4,7),,drop=FALSE], TE=x$TE[c(4,7),,drop=FALSE], AE=x$AE[c(4,7),,drop=FALSE]))#, sizes=x$sizes, info=x$info))
    } else print(list(CE=x$CE, TE=x$TE, AE=x$AE))#, sizes=x$sizes, info=x$info))
  }
}
####
#x1=x[jahr==2004,][1:100,];y1=y[jahr==2004,][1:100,];id1=(1:sum(jahr==2004))[1:100]; gr1 <- grouping[1:100]
#x2=x[jahr==2009,][50:100,];y2=y[jahr==2009,][50:100,];id2=(1:sum(jahr==2009))[50:100]; gr2 <- grouping[50:100]
#or=1;nrep=100;errchk=TRUE; digits=3;ci.type=2;alpha=c(1,0.1,0.05,0.01);short.result <- TRUE
#inv.inp.eff <- TRUE #gr1 <- rep(2,100); gr2 <- rep(2,51);
#filters=NULL
#filters[[1]] <- c(rep(TRUE,50),rep(FALSE,50)); filters[[2]] <- c(rep(FALSE,50),rep(TRUE,50))
dea.malmquist <- function(x1,y1,id1,gr1=NULL,x2,y2,id2,gr2=NULL,filters=NULL,or=c("in","out"),nrep=0,errchk=TRUE,alpha=c(0.1,0.05,0.01),ci.type=2,inv.inp.eff=TRUE,short.result=TRUE, short.print=TRUE){
  # short.result=TRUE --> If there is only one group the result is wrapped up into a matrix instead of a list
  # short.print=TRUE --> when printing the result only the mean and the standard devaition is printed (no Min,Max, etc.)
  
  # Reference: E.g. Fried et al. (2008): The Measurement of Productive Efficiency and Productivity Growth: ch 5.3.3 p 574-578
  # The equations used in the malmquist function are...   Refer to ?malmquist
  # malm = sqrt(LIST$c21 * LIST$c22/(LIST$c11 * LIST$c12))
  # eff = LIST$c22/LIST$c11
  # tech = sqrt(LIST$c21 * LIST$c11/(LIST$c22 * LIST$c12))
  # pure.eff = LIST$v22/LIST$v11
  # scale = LIST$c22 * LIST$v11/(LIST$v22 * LIST$c11)
  # pure.tech = sqrt(LIST$v21 * LIST$v11/(LIST$v22 * LIST$v12))
  # scale.tech = malm/(pure.eff * scale * pure.tech)
  # sch = scale * scale.tech
  
  
  # The input oriented productivity gain is below 0 in the malmquist function!
  #id1 <- id2 <- 1:90
  #gr1 <- gr2 <- c(rep(1,45),rep(2,45))
  #x1 <- matrix(rep( c(1,1,1, 1,1,1, 1,1,1),30),nrow=90,byrow=TRUE)
  #x2 <- matrix(rep(c(0.5,1,1, 1,1,1, 1,1,1),30),nrow=90, byrow=TRUE)
  #y1 <- matrix(rep(c(1,1,1),30),nrow=90,byrow=TRUE)
  #y2 <- matrix(rep(c(1,1,1),30),nrow=90,byrow=TRUE)
  #filters <- list()
  #filters[[1]] <- c( rep(TRUE,45), rep(FALSE,45))
  #filters[[2]] <- c( rep(FALSE,44), rep(TRUE,46))
  #filters[[2]][90] <- FALSE
  #filters[[3]] <- c(rep(FALSE,85),rep(TRUE,5))
  #or="in"; nrep=0; errchk=TRUE; alpha=c(0.1,0.05,0.01); ci.type=2; inv.inp.eff=TRUE; short.result=TRUE; short.print=TRUE
  
  #malm <- dea.malmquist(x1=inp1,y1=out1,id1=id1,gr1=gr1, x2=inp2,y2=out2,id2=id2,gr2=gr2, or="in")
  #print.default(malm)
  
  #malm.boot.22 <- malmquist.components(X1=t(x[jahr==2004,]),Y1=t(y[jahr==2004,]),ID1=(1:sum(jahr==2004)),X2=t(x[jahr==2009,]),Y2=t(y[jahr==2009,]),ID2=(1:sum(jahr==2009)),ORIENTATION=1,NREP=nrep,errchk=TRUE)
  #malm.boot.22 <- malmquist(LIST=malm.boot5)
  #tapply(malm.boot.22$malm,gr1)
  
  require(Benchmarking)
  require(FEAR)
  if(is.null(dim(x1))) x1 <- matrix(x1,nrow=1)
  if(is.null(dim(x2))) x2 <- matrix(x2,nrow=1)
  if(is.null(dim(y1))) y1 <- matrix(y1,nrow=1)
  if(is.null(dim(y2))) y2 <- matrix(y2,nrow=1)
  if(nrep==1) stop("choose nrep=0 or at least nrep=2")
  if(!is.null(filters)) {
    if(length(id1)!=length(id2)) stop("when working with argument 'filters' all id1 must be equal id2!")
    if(!all(id1==id2)) stop("when working with argument 'filters' all id1 must be equal id2!")
    if(!is.list(filters)) stop("filters must be a list")
    falselength <- logical(); for(i in 1:length(filters)) falselength <- length(filters[[i]]) != nrow(x1)
    if(any(falselength)) stop("any length(filters[[i]]) must be equal nrow(x1)")
    filtertable <- do.call("cbind", filters);
    if(any(show <- rowSums(filtertable)>1)) { warning("Some objects will be evaluated in several DEA. These objects have the following ID:",call. = FALSE, immediate.=TRUE); print(id1[show]) }
    if(any(show <- apply(!filtertable,1,function(x)all(x)) )) { warning("Some objects will evaluated in no DEA. These objects have the following ID:",call. = FALSE, immediate.=TRUE); print(id1[show]) }
    notenough <- logical(); for(i in 1:length(filters)) notenough[i] <- sum(filters[[i]]) <2*ncol(x1)*ncol(y1)
    if(any(notenough)) { warning("some filters don't leave enough objects to fulfill the rule of thumb:\nn >= 2 * (number of inputs) * (number of outputs)           These are filters:",call. = FALSE, immediate.=TRUE); print(which(notenough)) }
  }
  
  filterlength <- length(filters)
  alpha <- alpha[order(-alpha)]
  or <- match.arg(or)
  or.orig <- or
  if(or=="in") or <- 1 else if (or=="out") or <- 2
  #ci <- NULL # UNNÜTZ!
  if(is.null(gr1)|is.null(gr2)) {
    gr1 <- rep(1,nrow(x1)); gr2 <- rep(1,nrow(x2))
    sizes <- list(sizes=NULL)
  }
  grouped <- length(unique(c(gr1,gr2)))>1
  
  # Definition of summasd function & calculation of malmquist indexes
  summasd <- function(x,na.rm=TRUE)  c(summary(x,na.rm=na.rm)[1:6],"SD"=sd(x,na.rm=na.rm))
  
  if(is.null(filters)){
    malm.comp <- malmquist.components(X1=t(x1),Y1=t(y1),ID1=id1,X2=t(x2),Y2=t(y2),ID2=id2,ORIENTATION=or,NREP=nrep,errchk=errchk)
    malm.res <- dea.malmquist(malm.comp, alpha=alpha, CI.TYPE=ci.type)
    gr <- gr1[id1%in%id2]
    sizes <- table(gr); sizes <- c(all=sum(sizes),sizes); sizes <- list(sizes=sizes)
  } else {
    malm.res.filters <- gr.filters <- id.filters <- list()
    for(i in 1:filterlength){
      malm.comp <- malmquist.components(X1=t(x1[filters[[i]],]),Y1=t(y1[filters[[i]],]),ID1=id1[filters[[i]]],X2=t(x2[filters[[i]],]),Y2=t(y2[filters[[i]],]),ID2=id2[filters[[i]]],ORIENTATION=or,NREP=nrep,errchk=errchk)
      malm.res.filters[[i]] <- malmquist(malm.comp, alpha=alpha, CI.TYPE=ci.type)
      gr.filters[[i]] <- gr1[filters[[i]]];  #gr1.filt <- gr1[filters[[i]]]; gr2.filt <- gr2[filters[[i]]]; id1.filt <- id1[filters[[i]]]; id2.filt <- id2[filters[[i]]]; gr.filters[[i]] <- gr1.filt[id1.filt%in%id2.filt]
      id.filters[[i]] <- id1[filters[[i]]];
    }
    gr <- do.call("c",gr.filters)
    sizes <- table(gr); sizes <- c(all=sum(sizes),sizes); sizes <- list(sizes=sizes)
    malm.res <- vector("list",length(malm.res.filters[[i]])); names(malm.res) <- names(malm.res.filters[[i]])
    for(i in 1:length(malm.res)){
      for(j in 1:length(malm.res.filters)){
        if(is.null(dim(malm.res.filters[[1]][[i]]))) {
          malm.res[[i]] <- c(malm.res[[i]],malm.res.filters[[j]][[i]])
        } else {
          malm.res[[i]] <- rbind(malm.res[[i]],malm.res.filters[[j]][[i]])
        }
      }
    }
    # Setting the Values back into the original order
    original.order <- match(id1,malm.res$id)
    for(i in 1:length(malm.res)) {
      if(is.null(dim(malm.res[[i]]))) { malm.res[[i]] <- malm.res[[i]][original.order]
      } else { malm.res[[i]] <- malm.res[[i]][original.order,] }
    }
  }
  
  # Inverting Input Efficiencies (if wished)
  if(or.orig=="in" & inv.inp.eff) {
    for(i in 1:length(malm.res)) malm.res[[i]] <- 1/malm.res[[i]]
    malm.res$id <- 1/malm.res$id
  }
  
  # Groupwise summaries of values
  values <- list()
  for(i in 1:8){
    values[[i]] <- do.call("cbind",tapply(malm.res[[i]],gr,summasd))
    # Add a column "all" which combines all groups together
    if(grouped) values[[i]] <- cbind(all=summasd(malm.res[[i]]),values[[i]])
  }
  names(values) <- paste( names(malm.res)[1:8] ,".sum",sep="")
  # Groupwise calculation of conficence interval summaries
  if(nrep>0){
    if(names(malm.res)[9]!="ci.malm") stop("The output structure of the malmquist() function has changed. You have to adapt the code!")
    # Giving rownames for summaries depending on the alpha chosen (The rounding should be as small as possible but it must be great enough in order not to have 100% and 0% at the start & the end of the rownames.)
    round.rowname.values <- 1
    while( any( round( c( rev(1-alpha/2) , rev(alpha[length(alpha):1]/2) )*100 ,round.rowname.values)[c(1,length(alpha)*2)] %% 1 == 0) ) round.rowname.values <- round.rowname.values+1
    rowname.values <- round( c( rev(1-alpha/2) , rev(alpha[length(alpha):1]/2) )*100 ,round.rowname.values)
    # Einschub: Giving colnames to $ci.malm until ci.sch (same as rownames)
    for(i in 9:16) colnames(malm.res[[i]]) <- paste(rowname.values,"%",sep="")
    # weiter: Konfidenzintervalle zusammenfassen.
    ci <- list()
    for(i in 9:(length(malm.res)-1)) {
      # Originale Reihenfolge ist so (Grösste Zahl = Grösster Wert): 3,2,1,4,5,6. Für absteigende Reihenfolge muss man machen: c(6:4,1:3)
      r.nr <- 1
      order.row <- malm.res[[i]][r.nr,]
      while(any(is.na(order.row))) order.row <- malm.res[[i]][r.nr+1,]
      malm.res[[i]] <- malm.res[[i]][,order(-order.row)] # Order has to be changed first!           # malm.res[[i]][,c( (2*length(alpha)):(length(alpha)+1) , 1:length(alpha) )]       # malm.res[[i]] <- malm.res[[i]][,c(3:1,4:6)]
      ci[[i-8]] <- t(apply(malm.res[[i]],2,function(x)tapply(x,gr,mean,na.rm=TRUE)))
      if(!grouped) { ci[[i-8]] <- t(ci[[i-8]]); colnames(ci[[i-8]]) <- unique(gr) }
      # Add a column "all" which combines all groups together
      if(grouped) ci[[i-8]] <- cbind(all=apply(malm.res[[i]],2,mean,na.rm=TRUE), ci[[i-8]])
      rownames(ci[[i-8]]) <- paste(rowname.values,"%",sep="")
    }
    names(ci) <- paste( names(malm.res)[9:(length(malm.res)-1)] ,".sum",sep="")
    # Result with confidence intervals
    if(!short.result | length(unique(gr))>1) {
      # As a list
      result <- c(malm.res,values,ci,sizes)
      short.result <- FALSE
    } else if (short.result & length(unique(gr))==1){
      # Shortened into a matrix (if short.result=TRUE)
      values.res.mat <- do.call("cbind",values); colnames(values.res.mat) <- names(malm.res)[1:8] # same as names(values) but without ".sum" appendix
      ci.res.mat <- do.call("cbind",ci); colnames(ci.res.mat) <- names(malm.res)[9:(length(malm.res)-1)] # same as names(ci) but without ".sum" appendix
      result.val.ci <- list(values=values.res.mat,ci=ci.res.mat)
      result <- c(malm.res, result.val.ci)
    }
    # Result without confidence intervals
  } else {
    if(!short.result | length(unique(gr))>1){
      # As a list
      result <- c(malm.res,values,sizes)
      short.result <- FALSE
    } else if (short.result & length(unique(gr))==1){
      # Shortened into a matrix (if short.result=TRUE)
      values.res.mat <- do.call("cbind",values); colnames(values.res.mat) <- names(malm.res)[1:8] # same as names(values) but without ".sum" appendix
      values.res.mat <- list(values=values.res.mat)
      result <- c(malm.res,values.res.mat)
    }
  }
  print.ci <- !is.null(ci)
  print.info <- list(print.ci=print.ci, short.result=short.result, short.print=short.print)
  info <- list(info=paste("orientation:",or.orig))
  result <- c(result, list(grouping=gr), info, list(print.info=print.info))
  class(result) <- "dea.malmquist"
  return(result)
}
print.dea.malmquist <- function(object,digits=2,short.print=NULL,transpose=FALSE){
  if(is.null(short.print)) short.print <- object$print.info$short.print
  if(object$print.info$print.ci & object$print.info$short.result){
    if(short.print){ result <- list(values=object$values[c(4,7),,drop=FALSE], ci=object$ci[c(4,7),,drop=FALSE], info=object$info)
    } else result <- list(values=object$values, ci=object$ci, info=object$info)
  } else if(!object$print.info$print.ci & object$print.info$short.result){
    if(short.print){ result <- list(values=object$values[c(4,7),,drop=FALSE], info=object$info)
    } else result <- list(values=object$values, info=object$info)
  } else if(object$print.info$print.ci & !object$print.info$short.result){
    for(i in 18:25) { if(short.print) object[[i]] <- object[[i]][c(4,7),,drop=FALSE] else object[[i]] <- object[[i]] }
    i<-1; while(i<=17) {
      object[[1]] <- NULL
      i <- i+1
    }
    result <- object
  } else {
    for(i in 10:17) { if(short.print) object[[i]] <- object[[i]][c(4,7),,drop=FALSE] else object[[i]] <- object[[i]] }
    i<-1; while(i<=9) {
      object[[1]] <- NULL
      i <- i+1
    }
    result <- object
  }
  result$print.info <- NULL
  result$grouping <- NULL
  class(result) <- "list"
  if(transpose) for(i in 1:length(result)) if(!is.null(dim(result[[i]]))) result[[i]] <- t(result[[i]])
  for(i in 1:length(result)) if(is.numeric(result[[i]])) result[[i]] <- round(result[[i]],digits=digits)
  print(result)
  invisible(result)
}
####
#x1=x[jahr==2004,][1:100,];y1=y[jahr==2004,][1:100,];id1=(1:sum(jahr==2004))[1:100]; gr1 <- grouping[1:100]
#x2=x[jahr==2009,][1:100,];y2=y[jahr==2009,][1:100,];id2=(1:sum(jahr==2009))[1:100]; gr2 <- grouping[1:100]
#or=1;nrep=100;errchk=TRUE; digits=3;ci.type=2;alpha=c(1,0.1,0.05,0.01); short.result <- TRUE; short.print <- TRUE; mean.type=c("geom","arith")[1]
#inv.inp.eff <- TRUE #gr1 <- rep(2,100); gr2 <- rep(2,51);
#filters <- NULL
#filters <- list(); filters[[2]] <- c(rep(TRUE,50),rep(FALSE,50)); filters[[1]] <- c(rep(FALSE,50),rep(TRUE,50))
#gr1 <- NULL; gr2 <- NULL
dea.malmquist2 <- function(x1,y1,id1,gr1=NULL,x2,y2,id2,gr2=NULL,filters=NULL,or=c("in","out"),nrep=0,errchk=TRUE,alpha=c(0.1,0.05,0.01),ci.type=2,inv.inp.eff=TRUE,mean.type=c("geom","arith"),short.result=TRUE, short.print=TRUE){
  # short.result=TRUE --> If there is only one group the result is wrapped up into a matrix instead of a list
  # short.print=TRUE --> when printing the result only the mean and the standard devaition is printed (no Min,Max, etc.)
  
  # Reference: E.g. Fried et al. (2008): The Measurement of Productive Efficiency and Productivity Growth: ch 5.3.3 p 574-578
  # The equations used in the malmquist function are...   Refer to ?malmquist
  # malm = sqrt(LIST$c21 * LIST$c22/(LIST$c11 * LIST$c12))
  # eff = LIST$c22/LIST$c11
  # tech = sqrt(LIST$c21 * LIST$c11/(LIST$c22 * LIST$c12))
  # pure.eff = LIST$v22/LIST$v11
  # scale = LIST$c22 * LIST$v11/(LIST$v22 * LIST$c11)
  # pure.tech = sqrt(LIST$v21 * LIST$v11/(LIST$v22 * LIST$v12))
  # scale.tech = malm/(pure.eff * scale * pure.tech)
  # sch = scale * scale.tech
  
  
  # The input oriented productivity gain is below 0 in the malmquist function!
  #id1 <- id2 <- 1:90
  #gr1 <- gr2 <- c(rep(1,45),rep(2,45))
  #x1 <- matrix(rep( c(1,1,1, 1,1,1, 1,1,1),30),nrow=90,byrow=TRUE)
  #x2 <- matrix(rep(c(0.5,1,1, 1,1,1, 1,1,1),30),nrow=90, byrow=TRUE)
  #y1 <- matrix(rep(c(1,1,1),30),nrow=90,byrow=TRUE)
  #y2 <- matrix(rep(c(1,1,1),30),nrow=90,byrow=TRUE)
  #filters <- list()
  #filters[[1]] <- c( rep(TRUE,45), rep(FALSE,45))
  #filters[[2]] <- c( rep(FALSE,44), rep(TRUE,46))
  #filters[[2]][90] <- FALSE
  #filters[[3]] <- c(rep(FALSE,85),rep(TRUE,5))
  #or="in"; nrep=100; errchk=TRUE; alpha=c(0.1,0.05,0.01); ci.type=2; inv.inp.eff=TRUE; short.result=TRUE; short.print=TRUE
  
  #malm <- dea.malmquist(x1=inp1,y1=out1,id1=id1,gr1=gr1, x2=inp2,y2=out2,id2=id2,gr2=gr2, or="in")
  #print.default(malm)
  
  mean.type <- match.arg(mean.type)
  # Definition of summasd function
  if(mean.type=="geom") {
    mean.func <- function(x,na.rm=TRUE) {
      if(na.rm==TRUE) x <- x[!is.na(x)]
      result <- (prod(x))^(1/length(x))
      names(result) <- "Mean"
      return(result)
    }
  } else { #if(!is.function(mean)) stop(paste("mean() has to be a function. you changed it to class:", class(mean)))
    mean.func <- match.fun(mean) }
  # The summas.geom function is built with geometric or with arithmetic means but is always called summasd.geom
  summasd.geom <- function(x,na.rm=TRUE)  { a <- c(summary(x,na.rm=na.rm)[1:6],"SD"=sd(x,na.rm=na.rm)); a[4] <- mean.func(x,na.rm=na.rm); return(a) }
  
  
  require(Benchmarking)
  require(FEAR)
  if(nrep>0&ci.type>1) warning("It's not sure if you can trust the boot results ($boot...) if ci.type>1. Because the group means of the DEA scores is taken instead of one score for each element. The CI-results ($ci...) are ok anyway.",call. = FALSE, immediate. = TRUE)
  if(is.null(dim(x1))) x1 <- matrix(x1,nrow=1)
  if(is.null(dim(x2))) x2 <- matrix(x2,nrow=1)
  if(is.null(dim(y1))) y1 <- matrix(y1,nrow=1)
  if(is.null(dim(y2))) y2 <- matrix(y2,nrow=1)
  if(nrep==1) stop("choose nrep=0 or at least nrep=2")
  if(!is.null(filters)) {
    if(length(id1)!=length(id2)) stop("when working with argument 'filters' all id1 must be equal id2!")
    if(!all(id1==id2)) stop("when working with argument 'filters' all id1 must be equal id2!")
    if(!is.list(filters)) stop("filters must be a list")
    falselength <- logical(); for(i in 1:length(filters)) falselength <- length(filters[[i]]) != nrow(x1)
    if(any(falselength)) stop("any length(filters[[i]]) must be equal nrow(x1)")
    filtertable <- do.call("cbind", filters);
    if(any(show <- rowSums(filtertable)>1)) { stop("Some objects will be evaluated in several DEA. These objects have the following ID:",call. = FALSE, immediate. = TRUE); print(id1[show]) }
    if(any(show <- apply(!filtertable,1,function(x)all(x)) )) { stop("Some objects will evaluated in no DEA. These objects have the following ID:",call. = FALSE, immediate. = TRUE); print(id1[show]) }
    notenough <- logical(); for(i in 1:length(filters)) notenough[i] <- sum(filters[[i]]) <2*ncol(x1)*ncol(y1)
    if(any(notenough)) { warning("some filters don't leave enough objects to fulfill the rule of thumb:\nn >= 2 * (number of inputs) * (number of outputs)           These are filters:",call. = FALSE, immediate. = TRUE); print(which(notenough)) }
  }
  
  # Giving rownames for CI-summaries depending on the alpha chosen (The rounding should be as small as possible but it must be great enough in order not to have 100% and 0% at the start & the end of the rownames.)
  round.rowname.values <- 1
  while( any( round( c( rev(1-alpha/2) , rev(alpha[length(alpha):1]/2) )*100 ,round.rowname.values)[c(1,length(alpha)*2)] %% 1 == 0) ) round.rowname.values <- round.rowname.values+1
  rowname.values <- round( c( rev(1-alpha/2) , rev(alpha[length(alpha):1]/2) )*100 ,round.rowname.values)
  
  # Preparing other stuff
  filterlength <- length(filters)
  alpha <- alpha[order(-alpha)]
  or <- match.arg(or)
  or.orig <- or
  if(or=="in") or <- 1 else if (or=="out") or <- 2
  #ci <- NULL # UNNÜTZ!
  if(is.null(gr1)|is.null(gr2)) {
    gr1 <- rep(1,nrow(x1)); gr2 <- rep(1,nrow(x2))
    sizes <- NULL
  }
  grouped <- length(unique(c(gr1,gr2)))>1
  
  #  Calculation of malmquist indexes
  if(is.null(filters)){
    malm.comp <- malmquist.components(X1=t(x1),Y1=t(y1),ID1=id1,X2=t(x2),Y2=t(y2),ID2=id2,ORIENTATION=or,NREP=nrep,errchk=errchk)
    malm.res <- dea.malmquist.boot(malm.comp, alpha=alpha, CI.TYPE=ci.type)
    gr <- gr1[id1%in%id2]
    sizes <- table(gr); sizes <- c(all=sum(sizes),sizes)
  } else {
    # Calculation of Malmquist index when filters are chosen
    malm.res.filters <- gr.filters <- list()
    for(i in 1:filterlength){
      malm.comp <- malmquist.components(X1=t(x1[filters[[i]],]),Y1=t(y1[filters[[i]],]),ID1=id1[filters[[i]]],X2=t(x2[filters[[i]],]),Y2=t(y2[filters[[i]],]),ID2=id2[filters[[i]]],ORIENTATION=or,NREP=nrep,errchk=errchk)
      malm.res.filters[[i]] <- dea.malmquist.boot(malm.comp, alpha=alpha, CI.TYPE=ci.type)
      gr.filters[[i]] <- gr1[filters[[i]]];
    }
    gr <- do.call("c",gr.filters)
    sizes <- table(gr); sizes <- c(all=sum(sizes),sizes)
    malm.res <- vector("list",length(malm.res.filters[[i]])); names(malm.res) <- names(malm.res.filters[[i]])
    # Combining the results of the filters to one output
    for(i in 1:length(malm.res)){
      for(j in 1:length(malm.res.filters)){
        if(is.null(dim(malm.res.filters[[1]][[i]]))) {
          malm.res[[i]] <- c(malm.res[[i]],malm.res.filters[[j]][[i]])
        } else {
          malm.res[[i]] <- rbind(malm.res[[i]],malm.res.filters[[j]][[i]])
        }
      }
    }
    # Setting the Values back into the original order
    original.order <- match(id1,malm.res$id)
    gr <- gr[original.order]
    for(i in 1:length(malm.res)) {
      if(is.null(dim(malm.res[[i]]))) { malm.res[[i]] <- malm.res[[i]][original.order]
      } else { malm.res[[i]] <- malm.res[[i]][original.order,] }
    }
  }
  
  uniquegr <- sort(unique(gr))
  # Inverting Input-Efficiencies (so that Malm.>1 means technical progress)
  if(or==1 & inv.inp.eff) {
    for(i in 1:length(malm.res)) malm.res[[i]] <- 1/malm.res[[i]]
    malm.res$id <- 1/malm.res$id # IDs are back converted
  }
  
  # Groupwise summaries of values (without bootstrapping)
  values <- list()
  for(i in 1:8){
    values[[i]] <- do.call("cbind",tapply(malm.res[[i]],gr,summasd.geom))
    # Add a column "all" which combines all groups together
    if(grouped) values[[i]] <- cbind(all=summasd.geom(malm.res[[i]]),values[[i]])
  }
  names(values) <- paste( names(malm.res)[1:8] ,".sum",sep="")
  
  # Groupwise calculation of bootstrapping summaries
  if(nrep>0){
    
    # Own CI-calculation by pooling the data of every group ### NOT USED ANYMORE! ###
    if(!TRUE){
      boot <- vector("list",length(malm.res)); names(boot) <- names(malm.res)
      boot.res <- boot
      for(i in 1:8){ #17:24
        boot[[i+16]] <- boot[[i]] <-  list()
        for(j in c( uniquegr ,max(uniquegr)+1) ) {
          if(j!=max(uniquegr)+1){
            boot[[i]][[j]]        <- malm.res[[i]][gr==j] # No bootstrap values for calculating the mean for agrument BHAT in bootstrap.ci()
            boot[[i+16]][[j]]     <- rbind(c( malm.res[[i+16]][gr==j,] )) # Bootstrap values
            boot.res[[i+16]][[j]] <- bootstrap.ci(BOOT = boot[[i+16]][[j]] [,!is.na(boot[[i+16]][[j]]),drop=FALSE], BHAT = mean.func(boot[[i]][[j]],na.rm=TRUE), 
                                                  alpha = alpha, DEA = FALSE, METHOD = ci.type)
          } else {
            boot[[i]][[j]]        <- malm.res[[i]]
            boot[[i+16]][[j]]     <- rbind(c( malm.res[[i+16]] ))
            boot.res[[i+16]][[j]] <- bootstrap.ci(BOOT = boot[[i+16]][[j]] [,!is.na(boot[[i+16]][[j]]),drop=FALSE], BHAT = mean.func(boot[[i]][[j]],na.rm=TRUE), 
                                                  alpha = alpha, DEA = FALSE, METHOD = ci.type)
          }
        }
        boot.res[[i+16]] <- lapply(boot.res[[i+16]],c)
        boot.res[[i+16]] <- do.call("cbind",boot.res[[i+16]])
        boot.res[[i+16]] <- cbind( boot.res[[i+16]][,max(uniquegr)+1], boot.res[[i+16]][,uniquegr] )
        boot.res[[i+16]] <- boot.res[[i+16]][order(-boot.res[[i+16]][,1]),]
        colnames(boot.res[[i+16]]) <- c("all",uniquegr)
        rownames(boot.res[[i+16]]) <- paste(rowname.values,"%",sep="")
        if(!grouped) boot.res[[i+16]] <- boot.res[[i+16]][,1,drop=FALSE]
        
        #boot.restructure <- list();
        #boot.restructure[[1]] <- boot[[i]][[length(boot[[i]])]]
        #for(k in 2:(length(boot[[i]]))) boot.restructure[[k]] <- boot[[i]][[k-1]]
        #boot[[i]] <- boot.restructure;
        boot.restructure <- list();
        boot.restructure[[1]] <- boot[[i+16]][[length(boot[[i+16]])]]
        for(k in 2:(length(boot[[i+16]]))) boot.restructure[[k]] <- boot[[i+16]][[k-1]]
        boot[[i+16]] <- boot.restructure; rm(boot.restructure)
      }
      for(i in 16:1) boot[[i]] <- NULL; boot$id <- NULL
      for(i in 1:length(boot)) boot[[i]] <- c(boot[[i]])
      
      boot.res2 <- list()
      for(i in 1:8) boot.res2[[i]] <- boot.res[[i+16]]
      names(boot.res2) <- paste(names(boot.res)[17:24],".sum",sep="")
      boot.res <- boot.res2
      rm(boot.res2)
    }
    # The boot result has to be created because there is no boot result anymore (deleted!)
    boot <- vector("list",length(malm.res)); names(boot) <- names(malm.res)
    boot.res <- vector("list",8);  names(boot.res) <- c("boot.malm.sum","boot.eff.sum","boot.tech.sum","boot.pure.eff.sum","boot.scale.sum","boot.pure.tech.sum","boot.scale.tech.sum","boot.sch.sum")
    
    
    # Calculating CI-summaries with means of the given CI-values (from malmquist() output)
    if(names(malm.res)[9]!="ci.malm") stop("The output structure of the malmquist() function has changed. You have to adapt the code!")
    ci <- list()
    for(i in 9:16) {
      # Einschub: Giving colnames to $ci.malm until ci.sch (same as rownames)
      colnames(malm.res[[i]]) <- paste(rowname.values,"%",sep="")
      # weiter: Konfidenzintervalle zusammenfassen.
      r.nr <- 1
      order.row <- malm.res[[i]][r.nr,]
      while(any(is.na(order.row))) order.row <- malm.res[[i]][r.nr+1,]
      malm.res[[i]] <- malm.res[[i]][,order(-order.row)] # Order has to be changed first!           # malm.res[[i]][,c( (2*length(alpha)):(length(alpha)+1) , 1:length(alpha) )]       # malm.res[[i]] <- malm.res[[i]][,c(3:1,4:6)]
      ci[[i-8]] <- t(apply(malm.res[[i]],2,function(x)tapply(x,gr,function(x)mean.func(x,na.rm=TRUE))))
      if(!grouped) { ci[[i-8]] <- t(ci[[i-8]]); colnames(ci[[i-8]]) <- unique(gr) }
      # Add a column "all" which combines all groups together
      if(grouped) ci[[i-8]] <- cbind(all=apply(malm.res[[i]],2,function(x)mean.func(x,na.rm=TRUE)), ci[[i-8]])
      rownames(ci[[i-8]]) <- paste(rowname.values,"%",sep="")
    }
    names(ci) <- paste( names(malm.res)[9:16] ,".sum",sep="")
    
    # Combining the result with bootstrapping results
    if(length(uniquegr)>1) {
      # As a list
      pre.result <- c(values,ci,boot.res,list(values.mat=NULL),list(ci.mat=NULL),list(boot.mat=NULL))
      short.result <- FALSE
    } else if (length(uniquegr)==1){
      # Shortened into a matrix (if short.result=TRUE)
      values.res.mat <- do.call("cbind",values); colnames(values.res.mat) <- names(malm.res)[1:8] # same as names(values) but without ".sum" appendix
      ci.res.mat <- do.call("cbind",ci); colnames(ci.res.mat) <- names(malm.res)[9:16] # same as names(ci) but without ".sum" appendix
      boot.res.mat <- do.call("cbind",boot.res); # old, because there is no alternative boot calculation anymore. colnames(boot.res.mat) <- names(malm.res)[17:24]
      result.val.ci <- list(values.mat=values.res.mat, ci.mat=ci.res.mat, boot.mat=boot.res.mat)
      pre.result <- c(values,ci,boot.res,result.val.ci)
    }
    
    # Result without bootstrapping results
  } else {
    # Preparing some empty lists for the output
    boot <- vector("list",length(malm.res)); names(boot) <- names(malm.res)
    boot.res <- vector("list",8);  names(boot.res) <- c("boot.malm.sum","boot.eff.sum","boot.tech.sum","boot.pure.eff.sum","boot.scale.sum","boot.pure.tech.sum","boot.scale.tech.sum","boot.sch.sum")
    ci <- vector("list",8);        names(ci)       <- c("ci.malm.sum","ci.eff.sum","ci.tech.sum","ci.pure.eff.sum","ci.scale.sum","ci.pure.tech.sum","ci.scale.tech.sum","ci.sch.sum")
    
    if(length(uniquegr)>1){
      # As a list
      pre.result <- c(values,ci,boot.res,list(values.mat=NULL),list(ci.mat=NULL),list(boot.mat=NULL))
      short.result <- FALSE
    } else if (length(uniquegr)==1){
      # Shortened into a matrix (if short.result=TRUE)
      values.res.mat <- do.call("cbind",values); colnames(values.res.mat) <- names(malm.res)[1:8] # same as names(values) but without ".sum" appendix
      values.mat <- list(values.mat=values.res.mat)
      pre.result <- c(values,ci,boot.res,values.mat,list(ci.mat=NULL),list(boot.mat=NULL)) 
    }
  }
  result <- c(malm.res,list(bootdata.grouped=boot),pre.result, list(grouping=gr), list(sizes=sizes), list(orientation=or.orig),list(nrep=nrep), list(mean.type=mean.type))
  class(result) <- "dea.malmquist2"
  return(result)
}
print.dea.malmquist2 <- function(ob,digits=2,short.print=TRUE,transpose=FALSE,...){
  names.ob <- names(ob)
  class(ob) <- "list"
  ci <- mat <- sum <- mat.not.null <- logical()
  # Checking which list-places are for the CI, summaries and matrices (=short result)
  for(i in 1:length(ob)){
    ci[i]  <- substr( names.ob[i],  1, 2) == "ci"
    sum[i] <- substr( names.ob[i],  max(1,nchar(names.ob[i])-2), nchar(names.ob[i])) == "sum"
    mat[i] <- substr( names.ob[i],  max(1,nchar(names.ob[i])-2), nchar(names.ob[i])) == "mat"
    mat.not.null[i] <- mat[i] & !is.null(ob[[i]])
  }
  # Deleting Min. Max. etc from the summaries (if short.print==TRUE)
  if(short.print) for(i in 1:length(ob)) if(sum[i]) if(!is.null(dim(ob[[i]]))) if(rownames(ob[[i]])[1]=="Min.") ob[[i]] <- ob[[i]][c(4,7),,drop=FALSE]
  ci.true <- !is.na(ci[ci][1])
  
  # If there are non-empty matrices, only print the matrices
  if(any(mat.not.null)){
    for(i in length(ob):1) if(!mat[i]) ob[[i]] <- NULL
  } else {
    # If the matrices are empty delete all list elements except for the summaries
    for(i in length(ob):1) if(!sum[i]) ob[[i]] <- NULL
  }
  # Transpose the result if wished
  for(i in 1:length(ob)){
    if(transpose) if(!is.null(dim(ob[[i]]))) ob[[i]] <- t(ob[[i]])
    if(is.numeric(ob[[i]])) ob[[i]] <- round(ob[[i]],digits)
  }
  # Make a new list=result and name it, such that the NULLs aren't printed afterwards
  result <- list()
  ob.not.null <- logical()
  for(i in 1:length(ob)) {
    if(!is.null(ob[[i]]))  result[[i]] <- ob[[i]]
    ob.not.null[i] <- !is.null(ob[[i]])
  }
  names(result) <- names(ob)[ob.not.null]
  # Print the result
  print(result)
  invisible(ob)
}
####
dea.malmquist.boot <- function (LIST, alpha = c(0.1, 0.05, 0.01), CI.TYPE = 2) {
  n = length(LIST$v11)
  no.bs = is.null(LIST$bv11)
  malm = sqrt(LIST$c21 * LIST$c22/(LIST$c11 * LIST$c12))
  eff = LIST$c22/LIST$c11
  tech = sqrt(LIST$c21 * LIST$c11/(LIST$c22 * LIST$c12))
  pure.eff = LIST$v22/LIST$v11
  scale = LIST$c22 * LIST$v11/(LIST$v22 * LIST$c11)
  pure.tech = sqrt(LIST$v21 * LIST$v11/(LIST$v22 * LIST$v12))
  scale.tech = malm/(pure.eff * scale * pure.tech)
  sch = scale * scale.tech
  if (!no.bs) {
    boot.malm = sqrt(LIST$bc21 * LIST$bc22/(LIST$bc11 * LIST$bc12))
    ci.malm = bootstrap.ci(BOOT = boot.malm, BHAT = malm, 
                           alpha = alpha, DEA = FALSE, METHOD = CI.TYPE)
    boot.eff = LIST$bc22/LIST$bc11
    ci.eff = bootstrap.ci(BOOT = boot.eff, BHAT = eff, alpha = alpha, 
                          DEA = FALSE, METHOD = CI.TYPE)
    boot.tech = sqrt(LIST$bc21 * LIST$bc11/(LIST$bc22 * LIST$bc12))
    ci.tech = bootstrap.ci(BOOT = boot.tech, BHAT = tech, 
                           alpha = alpha, DEA = FALSE, METHOD = CI.TYPE)
    boot.pure.eff = LIST$bv22/LIST$bv11
    ci.pure.eff = bootstrap.ci(BOOT = boot.pure.eff, BHAT = pure.eff, 
                               alpha = alpha, DEA = FALSE, METHOD = CI.TYPE)
    boot.scale = LIST$bc22 * LIST$bv11/(LIST$bv22 * LIST$bc11)
    ci.scale = bootstrap.ci(BOOT = boot.scale, BHAT = scale, 
                            alpha = alpha, DEA = FALSE, METHOD = CI.TYPE)
    boot.pure.tech = sqrt(LIST$bv21 * LIST$bv11/(LIST$bv22 * 
                                                   LIST$bv12))
    ci.pure.tech = bootstrap.ci(BOOT = boot.pure.tech, BHAT = pure.tech, 
                                alpha = alpha, DEA = FALSE, METHOD = CI.TYPE)
    boot.scale.tech = sqrt(LIST$bc21 * LIST$bv22 * LIST$bc11 * 
                             LIST$bv12/(LIST$bv21 * LIST$bc22 * LIST$bv11 * LIST$bc12))
    ci.scale.tech = bootstrap.ci(BOOT = boot.scale.tech, 
                                 BHAT = scale.tech, alpha = alpha, DEA = FALSE, METHOD = CI.TYPE)
    boot.sch = sqrt(LIST$bc21 * LIST$bv22 * LIST$bc12 * LIST$bv11/(LIST$bv21 * 
                                                                     LIST$bc22 * LIST$bv12 * LIST$bc11))
    ci.sch = bootstrap.ci(BOOT = boot.sch, BHAT = sch, alpha = alpha, 
                          DEA = FALSE, METHOD = CI.TYPE)
  }
  if (no.bs) {
    return(list(malm = malm, eff = eff, tech = tech, pure.eff = pure.eff, 
                scale = scale, pure.tech = pure.tech, scale.tech = scale.tech, 
                sch = sch, id = LIST$id))
  }
  else {
    return(list(malm = malm, eff = eff, tech = tech, pure.eff = pure.eff, 
                scale = scale, pure.tech = pure.tech, scale.tech = scale.tech, 
                sch = sch,
                ci.malm = ci.malm, ci.eff = ci.eff, ci.tech = ci.tech, 
                ci.pure.eff = ci.pure.eff, ci.scale = ci.scale, ci.pure.tech = ci.pure.tech, 
                ci.scale.tech = ci.scale.tech, ci.sch = ci.sch, 
                boot.malm = boot.malm, boot.eff = boot.eff, boot.tech = boot.tech, boot.pure.eff = boot.pure.eff, 
                boot.scale = boot.scale, boot.pure.tech = boot.pure.tech, boot.scale.tech = boot.scale.tech, 
                boot.sch = boot.sch,
                id = LIST$id))
  }
}
####
dea.meta <- function(x,y,grouping=NULL,add.to.table=NULL,or=c("in","out"),DUAL=NULL,p.digits=1,eff.digits=2,short.result=TRUE,short.print=TRUE) {
  # p.digits: digits for %, eff.digits: digits for efficiencies
  # DUAL --> give weights to x and y, such that some firms do not assign wheights of zero to some inputs or outputs. See Bogetoft & Otto (2011) ch 5 p 141
  # 
  # or="in"; RTS="vrs"; DUAL=NULL; p.digits=1; eff.digits=2;
  or <- match.arg(or)
  #require(FEAR)
  if(is.null(grouping)) grouping <- rep(1,nrow(x))
  x <- as.matrix(x); y <- as.matrix(y)
  if(!is.matrix(x)|!is.matrix(y)) stop("enter x and y as matrices with columns=variables & rows=observations")
  if(nrow(x)!=nrow(y)) stop("number of observations in x and y must be equal")
  if(length(grouping)!=nrow(x)) stop("length(grouping) must be equal nrow(x)=nrow(y)"); if(is.matrix(grouping)) if(nrow(grouping)!=nrow(x)) stop("length(grouping) must be equal nrow(x)=nrow(y)")
  if(any(is.na(grouping))) stop("Grouping cannot be NA!")
  if(any(as.numeric(grouping)!=grouping)) stop("Grouping must be numeric or convertable to numeric.")
  # Funktionsdefinition:
  summasd <- function(x,na.rm=TRUE)  c(summary(x,na.rm=na.rm),"SD"=sd(x,na.rm=na.rm))
  info <- "RTS coding: 0=constant, 1=increasing, 2=decreasing \n_gr colnames: within group values (TEcrs_gr, etc.) \n_m colnames: meta values (TEcrs_m, etc.) \nsummaries: if more than one country is used, summaries are only for meta values.\n           if only one country is used, summaries are for the country (within)."
  
  # DEA calculations
  if(is.null(DUAL)) {
    crs <- round( Benchmarking::dea(x,y, RTS="crs", ORIENTATION=or,FAST=TRUE) , 5)
    vrs <- round( Benchmarking::dea(x,y, RTS="vrs", ORIENTATION=or,FAST=TRUE) , 5)
    drs <- round( Benchmarking::dea(x,y, RTS="drs", ORIENTATION=or,FAST=TRUE) , 5)
  } else {
    crs <- round( Benchmarking::dea.dual(x,y, RTS="crs", ORIENTATION=or,DUAL=DUAL)$eff , 5)
    vrs <- round( Benchmarking::dea.dual(x,y, RTS="vrs", ORIENTATION=or,DUAL=DUAL)$eff , 5)
    drs <- round( Benchmarking::dea.dual(x,y, RTS="drs", ORIENTATION=or,DUAL=DUAL)$eff , 5)
  }
  #getdea <- cbind(crs=crs, vrs=vrs, drs=drs)
  
  # Calculating the returns to scale
  se <- round( crs/vrs, 5)
  rts <- numeric(0)
  rts[vrs >drs] <- 1; 
  rts[vrs==drs] <- 3;
  rts[vrs==crs] <- 2;
  #alldea <- cbind(TEcrs=crs, TEvrs=vrs, SE=se, RTS=rts, grouping=grouping)
  #if(!is.null(add.to.table)) alldea <- cbind(alldea,add.to.table)
  
  # ncrs: number of firms in constant returns to scale, peff: percent efficient firms...
  ncrs <- nirs <- ndrs <- n <- neff.crs <- neff.vrs <- peff.crs <- peff.crs <- peff.vrs <- numeric(0)
  nrs <- prs <- rs <- list()
  for (i in unique(grouping)) {
    ncrs[i] <- sum( rts==2&grouping==i ,na.rm=T)
    nirs[i] <- sum( rts==1&grouping==i ,na.rm=T)
    ndrs[i] <- sum( rts==3&grouping==i ,na.rm=T)
    n[i]    <- sum( grouping==i ,na.rm=T)
    nrs[[i]]  <- c( n[i], nirs[i], ncrs[i], ndrs[i] )
    prs[[i]]  <- nrs[[i]][2:4] /n[[i]]*100
    rs[[i]]   <- matrix(c(nrs[[i]],prs[[i]]),ncol=1);
    neff.crs[i] <- sum(crs[grouping==i]==1 ,na.rm=T)
    neff.vrs[i] <- sum(vrs[grouping==i]==1 ,na.rm=T)
    peff.crs[i] <- neff.crs[i]/n[[i]] * 100
    peff.vrs[i] <- neff.vrs[i]/n[[i]] * 100
  }
  # Returns to scale summary
  rs.sum <- do.call("cbind",rs)
  rs.sum <- round(rs.sum,p.digits)
  rownames(rs.sum) <- c("n.tot","n.irs","n.crs","n.drs","%.irs","%.crs","%.drs")
  # Number and percent of efficient firms summary
  npeff.sum <- rbind(neff.crs,neff.vrs,peff.crs,peff.vrs)
  npeff.sum <- npeff.sum[,!is.na(colSums(npeff.sum)),drop=FALSE]
  npeff.sum <- rbind(rs.sum[1,], npeff.sum) # Adding n.tot row (from rs.sum)
  colnames(npeff.sum) <- sort(unique(grouping)); rownames(npeff.sum) <- c("n.tot","n.eff.crs","n.eff.vrs","%.eff.crs","%.eff.vrs")
  npeff.sum <- round(npeff.sum,p.digits)
  # Groupwise TE under CRS & VRS (constant and varying returns to scale)
  crs.sum <- tapply(crs,grouping,summasd)
  crs.sum <- do.call("cbind", crs.sum)
  vrs.sum <- tapply(vrs,grouping,summasd)
  vrs.sum <- do.call("cbind", vrs.sum)
  # Scale efficiencies of all firms, of firms wirth IRS, and of firms with DRS (to see if the scale eff of firms in DRS is much better than of firms in IRS)
  se.sum <- tapply(se,grouping,summasd)
  se.sum <- do.call("cbind", se.sum)
  se.sum.irs <- list(); for(i in unique(grouping)) se.sum.irs[[i]] <- summasd(se[rts==1&grouping==i])
  se.sum.irs <- do.call("cbind", se.sum.irs); colnames(se.sum.irs) <- colnames(crs.sum)
  se.sum.drs <- list(); for(i in unique(grouping)) se.sum.drs[[i]] <- summasd(se[rts==3&grouping==i])
  se.sum.drs <- do.call("cbind", se.sum.drs); colnames(se.sum.drs) <- colnames(crs.sum)
  
  if(length(unique(grouping))>1) {
    # If there are more than 1 group, a column is added which describes the statistics of all observations together (here for RTS)
    nrs.tot <- prs.tot <- neffall <- numeric(0)
    for (i in 1:4) nrs.tot[i] <- sum(rs.sum[i,])
    prs.tot <- nrs.tot[2:4] / nrs.tot[1] * 100
    rs.sum <- cbind(c(nrs.tot,prs.tot),rs.sum)
    rs.sum <- round(rs.sum,p.digits)
    # Same for the number and percent of efficient firms
    for (i in 1:3) neffall[i] <- sum(npeff.sum[i,])
    peffall <- neffall[2:3]/length(grouping) * 100
    npeff.sum <- cbind(c(neffall,peffall),npeff.sum)
    npeff.sum <- round(npeff.sum,p.digits)
    # And for the TE with CRS and VRS assumption and the scale efficiencies
    crs.sum <- cbind(summasd(crs),crs.sum)
    vrs.sum <- cbind(summasd(vrs),vrs.sum)
    se.sum <- cbind(summasd(se),se.sum)
    se.sum.irs <- cbind(summasd(se[rts==1]),se.sum.irs)
    se.sum.drs <- cbind(summasd(se[rts==3]),se.sum.drs)
    # The Technology Gap Ratio is calculated (=effratio.vrs) and then summarized
    meta.sum.vrs <- meta.sum.crs <- list() # eff.vrs <- eff.crs <-
    effratio.vrs <- effratio.crs <- vrs.gr <- crs.gr <- rts.gr <- se.gr <- numeric()
    for (i in unique(grouping)) {
      if(is.null(DUAL)) {
        crs_within <- round( Benchmarking::dea(x[grouping==i,,drop=FALSE],y[grouping==i,,drop=FALSE], RTS="crs", ORIENTATION=or,FAST=TRUE) , 5)
        vrs_within <- round( Benchmarking::dea(x[grouping==i,,drop=FALSE],y[grouping==i,,drop=FALSE], RTS="vrs", ORIENTATION=or,FAST=TRUE) , 5)
        drs_within <- round( Benchmarking::dea(x[grouping==i,,drop=FALSE],y[grouping==i,,drop=FALSE], RTS="drs", ORIENTATION=or,FAST=TRUE) , 5)
      } else {
        crs_within <- round( Benchmarking::dea.dual(x[grouping==i,,drop=FALSE],y[grouping==i,,drop=FALSE], RTS="crs", ORIENTATION=or,DUAL=DUAL)$eff , 5)
        vrs_within <- round( Benchmarking::dea.dual(x[grouping==i,,drop=FALSE],y[grouping==i,,drop=FALSE], RTS="vrs", ORIENTATION=or,DUAL=DUAL)$eff , 5)
        drs_within <- round( Benchmarking::dea.dual(x[grouping==i,,drop=FALSE],y[grouping==i,,drop=FALSE], RTS="drs", ORIENTATION=or,DUAL=DUAL)$eff , 5)
      }
      
      effratio.crs[grouping==i] <- crs[grouping==i] / crs_within
      meta.sum.crs[[i]] <- summasd(effratio.crs[grouping==i])
      
      effratio.vrs[grouping==i] <- vrs[grouping==i] / vrs_within
      meta.sum.vrs[[i]] <- summasd(effratio.vrs[grouping==i])
      
      se_within <- round(crs_within/vrs_within, 5)
      rts_within <- numeric()
      rts_within[vrs_within >drs_within] <- 1; 
      rts_within[vrs_within==drs_within] <- 3;
      rts_within[vrs_within==crs_within] <- 2;
      
      crs.gr[grouping==i] <- crs_within
      vrs.gr[grouping==i] <- vrs_within
      se.gr[grouping==i]  <- se_within
      rts.gr[grouping==i] <- rts_within 
    }
    
    meta.sum.crs <- do.call("cbind",meta.sum.crs)
    meta.sum.crs <- cbind(summasd(effratio.crs),meta.sum.crs)
    meta.sum.vrs <- do.call("cbind",meta.sum.vrs)
    meta.sum.vrs <- cbind(summasd(effratio.vrs),meta.sum.vrs)
    
    # Rounding of all summaries & adding the colnames (WITH "all" for the first column)
    meta.sum.crs <- round(meta.sum.crs,eff.digits); meta.sum.vrs <- round(meta.sum.vrs,eff.digits); crs.sum <- round(crs.sum,eff.digits); vrs.sum <- round(vrs.sum,eff.digits); se.sum <- round(se.sum,eff.digits); se.sum.irs <- round(se.sum.irs,eff.digits); se.sum.drs <- round(se.sum.drs,eff.digits); rs.sum <- round(rs.sum,eff.digits)
    colnames(meta.sum.crs) <- colnames(meta.sum.vrs) <- colnames(crs.sum) <- colnames(vrs.sum) <- colnames(se.sum) <- colnames(se.sum.irs) <- colnames(se.sum.drs) <- colnames(rs.sum) <- colnames(npeff.sum) <- c("all", sort(unique(grouping)))
    # Adding Information about RTS used for meta analysis and putting all together in the function result
    output.table <- cbind(TEcrs_gr=crs.gr, TEvrs_gr=vrs.gr, SE_gr=se.gr, RTS_gr=rts.gr, TEcrs_m=crs, TEvrs_m=vrs, SE_m=se, RTS_m=rts, Meff.crs=effratio.crs, Meff.vrs=effratio.vrs, grouping=grouping)
    if(!is.null(add.to.table)) output.table <- cbind(output.table,add.to.table)
    short.result <- FALSE
    result <- list(eff.table=output.table, summary=NULL,         Meff.crs=meta.sum.crs, Meff.vrs=meta.sum.vrs, TEcrs=crs.sum, TEvrs=vrs.sum, SE=se.sum, SEirs=se.sum.irs, SEdrs=se.sum.drs, RTS=rs.sum, eff.firms=npeff.sum, info=info, print.info=list(short.result=short.result,short.print=short.print))
  } else {
    # Rounding of all summaries & adding the colnames (WITHOUT "all" for the first column)
    crs.sum <- round(crs.sum,eff.digits); vrs.sum <- round(vrs.sum,eff.digits); se.sum <- round(se.sum,eff.digits); se.sum.irs <- round(se.sum.irs,eff.digits); se.sum.drs <- round(se.sum.drs,eff.digits); rs.sum <- round(rs.sum,eff.digits)
    colnames(crs.sum) <- colnames(vrs.sum) <- colnames(se.sum) <- colnames(se.sum.irs) <- colnames(se.sum.drs) <- colnames(rs.sum) <- colnames(npeff.sum) <- sort(unique(grouping))
    # Adding Information
    result.sum <- cbind(crs.sum, vrs.sum, se.sum, se.sum.irs, se.sum.drs); colnames(result.sum) <- c("TEcrs","TEvrs","SE","SEirs","SEdrs")
    output.table <- cbind(TEcrs=crs, TEvrs=vrs, SE=se, RTS=rts, grouping=grouping)
    if(!is.null(add.to.table)) output.table <- cbind(output.table,add.to.table)
    
    result <- list(eff.table=output.table, summary=result.sum, Meff.crs=NULL, Meff.vrs=NULL,               TEcrs=crs.sum, TEvrs=vrs.sum, SE=se.sum, SEirs=se.sum.irs, SEdrs=se.sum.drs, RTS=rs.sum, eff.firms=npeff.sum, info=info, print.info=list(short.result=short.result,short.print=short.print))
  }
  names.result <- names(result);   for(i in 1:length(result)) if(!is.null(dim(result[[i]]))) if(ncol(result[[i]])==1) if(colnames(result[[i]])==1) colnames(result[[i]]) <- ""
  result$eff.table <- as.data.frame(result$eff.table)
  class(result) <- "dea.meta"
  return(result)
}
print.dea.meta  <- function(x, digits=2, short.print=NULL, ...)  {
  class(x) <- "list"
  if(is.null(short.print)) short.print <- x$print.info$short.print
  short.result <- x$print.info$short.result
  if(short.print) for(i in 1:length(x)) if(!is.null(dim(x[[i]]))) if(rownames(x[[i]])[1]=="Min.") x[[i]] <- x[[i]][c(4,7),,drop=FALSE]
  if(short.result) {
    for(i in c(9:3,1)) x[[i]] <- NULL
  } else {
    for(i in c(2,1)) x[[i]] <- NULL
  }
  x$print.info <- NULL
  x$eff.table <- NULL
  
  y <- list(); nullcount <- 0
  for(i in 1:length(x)) {
    if(is.null(x[[i]])) { nullcount <- nullcount + 1
    } else {
      y[[i-nullcount]] <- x[[i]]
      names(y)[i-nullcount] <- names(x)[i]
    }
  }
  print(y, digits=digits, ...)
  invisible(y)
}
summary.dea.meta <- function(x, digits=2, ...)  NULL
####
dea.weights <- function(x,y,grouping=NULL,or="in",RTS="vrs",DUAL=TRUE,invert=FALSE,digits=2,rm.inf=FALSE,rm.nainf.row=TRUE,short.print=TRUE) {
  # rm.nainf.row: should rows with only 0 in NA's and Inf's summary be removed from summary statistics?
  # rm.inf: should Inf values be removed for quantile calculation? If FALSE, the function works with a pseudo quantile. max and mean are sillt calculated without Infs as it would not make much sense.
  require(Benchmarking)
  #require(FEAR)
  x <- as.matrix(x); y <- as.matrix(y)
  if(is.logical(DUAL)) if(!DUAL) DUAL <- TRUE 
  if(is.null(grouping)) {
    grouping <- rep(1,nrow(x))
    sizes <- c(all=nrow(x))
  } else { sizes <- table(grouping); sizes <- c(all=sum(sizes),sizes) }
  
  if(nrow(x)!=nrow(y)) stop("number of observations in x and y must be equal")
  if(length(grouping)!=nrow(x)) stop("length(grouping) must be equal nrow(x)=nrow(y)"); if(is.matrix(grouping)) if(nrow(grouping)!=nrow(x)) stop("length(grouping) must be equal nrow(x)=nrow(y)")
  
  if(is.logical(DUAL)) { 
    e <- Benchmarking::dea(x,y, RTS="crs", ORIENTATION=or,DUAL=DUAL)
    outweights <- e$vy
    inpweights <- e$ux
  } else {
    e <- dea.dual(x,y, RTS="crs", ORIENTATION=or,DUAL=DUAL)
    outweights <- e$v
    inpweights <- e$u
  }
  if(!is.matrix(inpweights)) inpweights <- matrix(inpweights,ncol=1)
  if(!is.matrix(outweights)) outweights <- matrix(outweights,ncol=1)
  colnames(inpweights) <- colnames(x)  
  colnames(outweights) <- colnames(y)
  
  # Input Analysis
  inpw <- list()
  for(i in unique(grouping)){
    inpw[[i]] <- list()
    for(j in 1:ncol(inpweights)) {
      inpw[[i]][[j]] <- summary(inpweights[grouping==i,j])
      inpw[[i]][[j]][7] <- sd(inpweights[grouping==i,j])
    }
    inpw[[i]] <- do.call("cbind",inpw[[i]])
    inpw[[i]] <- round(inpw[[i]],digits)
    colnames(inpw[[i]]) <- colnames(inpweights)
    rownames(inpw[[i]]) <- c("Min.   :","1st Qu.:", "Median :","Mean   :","3rd Qu.:","Max.   :","SD     :")
  }
  
  if(ncol(inpweights)>1) {
    inpcomb <- combn(ncol(inpweights),2)
    if(invert) inpcomb <- inpcomb[c(2,1),]
    inpvar1 <- inpvar2 <- character()
    inpratio <- inpratioall <- list()
    for(i in unique(grouping)){
      inpratio[[i]] <- inpratioall[[i]] <- list()
      for(j in 1:ncol(inpcomb)) {
        inpvar1[j] <- colnames(inpweights)[inpcomb[1,j]]
        inpvar2[j] <- colnames(inpweights)[inpcomb[2,j]]
        
        rat <- inpweights[grouping==i,inpcomb[1,j]] / inpweights[grouping==i,inpcomb[2,j]]
        sumrat <- numeric(0)
        notinf <- rat[rat!=Inf&rat!=-Inf]
        notinf <- notinf[!is.na(notinf)]
        n.notinf  <- sum(rat!=Inf&rat!=-Inf)
        sumrat[1] <- min(notinf)
        if(!rm.inf) {
          sortrat <- sort(rat)
          sumrat[2] <- sortrat[round(0.25*length(rat))]
          sumrat[3] <- median(rat,na.rm=TRUE)
          sumrat[5] <- sortrat[round(0.75*length(rat))]
        } else {
          sumrat[2] <- quantile(notinf,0.25)
          sumrat[3] <- median(notinf)
          sumrat[5] <- quantile(notinf,0.75)
        }
        sumrat[4] <- mean(notinf)
        sumrat[6] <- max(notinf)
        sumrat[7] <- sd(notinf)
        sumrat[8] <- sum(is.na(rat))
        sumrat[9] <- sum(rat==Inf|rat==-Inf, na.rm=T)
        inpratio[[i]][[j]] <- sumrat
        
        inpratioall[[i]][[j]] <- mean(inpweights[grouping==i,inpcomb[1,j]],na.rm=T) / mean(inpweights[grouping==i,inpcomb[2,j]],na.rm=T)
      }
      inpratio[[i]] <- do.call("cbind",inpratio[[i]])
      rownames(inpratio[[i]]) <- c("Min.   :","1st Qu.:", "Median :","Mean   :","3rd Qu.:","Max.   :","SD     :", "NA's   :","Inf's  :")
      inpratio[[i]] <- round(inpratio[[i]],digits)
      inpratioall[[i]] <- do.call("cbind",inpratioall[[i]])
      rownames(inpratioall[[i]]) <- c("ovrlMean")
      inpratioall[[i]] <- round(inpratioall[[i]],digits)
      
      if(rm.nainf.row) {
        if        (sum(inpratio[[i]][8,])==0&sum(inpratio[[i]][9,])==0) { inpratio[[i]] <- inpratio[[i]][1:7,] 
        } else if (sum(inpratio[[i]][8,])!=0&sum(inpratio[[i]][9,])==0) { inpratio[[i]] <- inpratio[[i]][1:8,] 
        } else if (sum(inpratio[[i]][8,])==0&sum(inpratio[[i]][9,])!=0) { inpratio[[i]] <- inpratio[[i]][c(1:7,9),]
        }
      }
    }
    inpnames <- rbind(inpvar1,inpvar2)
    inpratioall <- do.call("rbind",inpratioall)
    if(length(unique(grouping))==1) rownames(inpratioall) <- "all" else rownames(inpratioall) <- sort(unique(grouping))
    #if(length(unique(grouping))==1) {
    #  inpratio <- inpratio[[!is.na(inpratio)]]
    #  inpratioall <- inpratioall[[!is.na(inpratioall)]]
    #  }
  } else {inpratio <- inpratioall <- inpnames <- NULL}
  
  # Output Analysis
  outw <- list()
  for(i in unique(grouping)){
    outw[[i]] <- list()
    for(j in 1:ncol(outweights)) {
      outw[[i]][[j]] <- summary(outweights[grouping==i,j])
      outw[[i]][[j]][7] <- sd(outweights[grouping==i,j])
    }
    outw[[i]] <- do.call("cbind",outw[[i]])
    outw[[i]] <- round(outw[[i]],digits)
    colnames(outw[[i]]) <- colnames(outweights)
    rownames(outw[[i]]) <- c("Min.   :","1st Qu.:", "Median :","Mean   :","3rd Qu.:","Max.   :","SD     :")
  }
  
  if(ncol(outweights)>1) {
    outcomb <- combn(ncol(outweights),2)
    if(invert) outcomb <- outcomb[c(2,1),]
    outvar1 <- outvar2 <- character()
    outratio <- outratioall <- list()
    for(i in unique(grouping)){
      outratio[[i]] <- outratioall[[i]] <- list()
      for(j in 1:ncol(outcomb)) {
        outvar1[j] <- colnames(outweights)[outcomb[1,j]]
        outvar2[j] <- colnames(outweights)[outcomb[2,j]]
        
        rat <- outweights[grouping==i,outcomb[1,j]] / outweights[grouping==i,outcomb[2,j]]
        sumrat <- numeric(0)
        notinf <- rat[rat!=Inf&rat!=-Inf]
        notinf <- notinf[!is.na(notinf)]
        n.notinf  <- sum(rat!=Inf&rat!=-Inf)
        sumrat[1] <- min(notinf)
        if(!rm.inf) {
          sortrat <- sort(rat)
          sumrat[2] <- sortrat[round(0.25*length(rat))]
          sumrat[3] <- median(rat,na.rm=TRUE)
          sumrat[5] <- sortrat[round(0.75*length(rat))]
        } else {
          sumrat[2] <- quantile(notinf,0.25)
          sumrat[3] <- median(notinf)
          sumrat[5] <- quantile(notinf,0.75)
        }
        sumrat[4] <- mean(notinf)
        sumrat[6] <- max(notinf)
        sumrat[7] <- sd(notinf)
        sumrat[8] <- sum(is.na(rat))
        sumrat[9] <- sum(rat==Inf|rat==-Inf, na.rm=T)
        outratio[[i]][[j]] <- sumrat
        
        outratioall[[i]][[j]] <- mean(outweights[grouping==i,outcomb[1,j]],na.rm=T) / mean(outweights[grouping==i,outcomb[2,j]],na.rm=T)
      }
      outratio[[i]] <- do.call("cbind",outratio[[i]])
      rownames(outratio[[i]]) <- c("Min.   :","1st Qu.:", "Median :","Mean   :","3rd Qu.:","Max.   :","SD     :", "NA's   :","Inf's  :")
      outratio[[i]] <- round(outratio[[i]],digits)
      outratioall[[i]] <- do.call("cbind",outratioall[[i]])
      rownames(outratioall[[i]]) <- c("ovrlMean")
      outratioall[[i]] <- round(outratioall[[i]],digits)
      
      if(rm.nainf.row) {
        if        (sum(outratio[[i]][8,])==0&sum(outratio[[i]][9,])==0) { outratio[[i]] <- outratio[[i]][1:7,] 
        } else if (sum(outratio[[i]][8,])!=0&sum(outratio[[i]][9,])==0) { outratio[[i]] <- outratio[[i]][1:8,] 
        } else if (sum(outratio[[i]][8,])==0&sum(outratio[[i]][9,])!=0) { outratio[[i]] <- outratio[[i]][c(1:7,9),]
        }
      }
    }
    outnames <- rbind(outvar1,outvar2)
    outratioall <- do.call("rbind",outratioall)
    if(length(unique(grouping))==1) rownames(outratioall) <- "all" else rownames(outratioall) <- sort(unique(grouping))
    #if(length(unique(grouping))==1) {
    #  outratio <- outratio[[!is.na(outratio)]]
    #  outratioall <- outratioall[[!is.na(outratioall)]]
    #}
  } else { outratio <- outratioall <- outnames <- NULL }  
  
  # Combine
  if(length(unique(grouping))==1) {
    outw <- outw[[!is.na(outw)]]
    inpw <- inpw[[!is.na(inpw)]]
  }
  
  inpweights <- round(inpweights,digits)
  outweights <- round(outweights,digits)
  if(length(unique(grouping))>1) {
    inpweights <- cbind(inpweights,grouping=grouping)
    outweights <- cbind(outweights,grouping=grouping)
  }
  info <- "Read the output like this:
  $inpweight.sum[[1]] gives a summary about the input weightings in group 1
  $inpratio.sum[[1]] gives a summary about the input ratios in group 1. The ratio in column (1): $inpratio.sum[[1]][,(1)] is the ratio $inpnames[1,(1)] / $inpnames[1,(2)]. Because some firms give weightings of 0 some values can be Inf or -Inf
  $inpratio.mean does not give summaries of the group ratios. It first calculates all group means of weights and with this group mean the ratio is caluclated. Unless all weights of input in a group are = 0 with that calculation method no Inf or -Inf values are obtained. Because the ratio is calculated with means there is no summary statistic (as for every group only 1 ratio value is calculated)."
  result <- list(inpweights=inpweights, inpweight.sum=inpw, inpratio.sum=inpratio, inpratio.mean=inpratioall, inpnames=inpnames,
                 outweights=outweights, outweight.sum=outw, outratio.sum=outratio, outratio.mean=outratioall, outnames=outnames, sizes=sizes, info=info, short.print=short.print )
  class(result) <- "dea.weights"
  return(result)
}
print.dea.weights  <- function(x, type=c("both","input","output"),digits=2, short.print=NULL, ...)  {
  type <- match.arg(or)
  if(is.null(short.print)) short.print <- x$short.print
  x$short.print <- NULL
  x$inpweights <- NULL
  x$outweights <- NULL
  x$info <- NULL
  if(short.print) {
    for(i in 1:length(x$inpweight.sum)) x$inpweight.sum[[i]] <- x$inpweight.sum[[i]][c(4,7),,drop=FALSE]
    for(i in 1:length(x$outweight.sum)) x$outweight.sum[[i]] <- x$outweight.sum[[i]][c(4,7),,drop=FALSE]
    for(i in 1:length(x$inpratio.sum)) x$inpratio.sum[[i]] <- x$inpratio.sum[[i]][c(4,7),,drop=FALSE]
    for(i in 1:length(x$outratio.sum)) x$outratio.sum[[i]] <- x$outratio.sum[[i]][c(4,7),,drop=FALSE]
  }
  if(type=="input") {
    x <- list(inpweight.sum=x$inpweight.sum, inpratio.sum=x$inpratio.sum, inpratio.mean=x$inpratio.mean, inpnames=x$inpnames, sizes=x$sizes)
  } else if(type=="output") {
    x <- list(outweight.sum=x$outweight.sum, outratio.sum=x$outratio.sum, outratio.mean=x$outratio.mean, outnames=x$outnames, sizes=x$sizes)
  }
  nulls <- logical()
  for(i in 1:length(x)) nulls[i] <- is.null(x[[i]])
  for(i in length(x):1) if(nulls[i]) x[[i]] <- NULL
  class(x) <- "list"
  print(x, digits=digits, ...)
  invisible(x)
}
####

sfa.rts.loglik_test <- function(xnames,ynames,data, ...){
  # See Bogetoft & Otto (2011) ch8 p251 (theory) and p256 (example).
  # I changed the notation of oA and o0.
  # You can use the lrtest() generally to check if one model is better than another.
  # The higher the loglik of the alternative hypothesis, the better!
  if(FALSE){
    data <- read.csv(paste("C:/Program Files/R/R-3.0.1/library/Benchmarking/data/","milkProd.csv",sep=""), sep=";", header=TRUE, na.strings=c("NA","","#DIV/0","#DIV/0!", "#WERT", "#WERT!"))
    xnames <- c("cows","vet", "energy") 
    ynames <- "milk"
    sfa.rts.loglik_test(xnames,ynames,data)
  }
  form1 <- production.formula(xnames,ynames,funcform="log",dist=FALSE)
  form2 <- production.formula(xnames,ynames,funcform="log.rts.loglik_test",dist=FALSE)
  HA_RTS <- frontier::sfa(form1,data=data, ...) # Alternative hypothesis of non constant returns to scale
  H0_const_RTS <- frontier::sfa(form2,data=data, ...) # Null hypothesis of constant returns to scale! We divide all parameters by one input. Then we leave this input completely away instead of summing all coefficients of the other inputs up (see Bogetoft & Otto ch8 p255 and p256 ).
  cat("Null hypothesis is constant returns to scale. If Pr(>Chisq) < 0.05 the null hypothesis is rejected and there are non constant returns to scale!\n")
  return(lmtest::lrtest(H0_const_RTS,HA_RTS))
}

sfa.rts.t_test <- function(xnames,ynames,data, ...){
  # See Bogetoft & Otto (2011) ch8 p255
  form1 <- production.formula(xnames,ynames,funcform="log",dist=FALSE)
  form2 <- production.formula(xnames,ynames,funcform="log.rts.t_test",dist=FALSE)
  su1 <- summary(frontier::sfa(form1,data=data, ...))  
  su2 <- summary(frontier::sfa(form2,data=data, ...))
  res1 <- su1$mleParam[2:(1+length(xnames)),]
  #res1 <- rbind(res1, sum=apply(res1,2,function(x)sum(x)))
  #res1[nrow(res1),2:ncol(res1)] <- NA
  res2 <- su2$mleParam[1+length(xnames),,drop=FALSE]; rownames(res2) <- "sum(coefs)-1"
  res <- rbind(res1,res2)
  return(res)
}

sfa.meta <- function(form, data, grouping, id, year=NULL, timeEffect=FALSE, eff.digits=3, short.print=TRUE, large.output=FALSE, count=TRUE, ...) {
  # This function performs a SFA and calculates the meta efficiency ratios. It follows O'Donnell et al. (2008): Metafrontier frameworks for the study of firm-level efficiencies and technology ratios ...
  
  if(any(c(length(grouping)!=length(id)))) stop("Please fulfill length(grouping) == length(id)")
  if(!is.null(year))  if(any(c(length(grouping)!=length(year)))) stop("Please fulfill length(grouping) == length(id) == length(year)")
  if(timeEffect & is.null(year)) stop("year not specified but needed when timeEffect==TRUE")
  if(is.character(form)) form <- formula(form)
  if(class(data)[1]=="plm.dim") stop("Enter data as normal data.frame or matrix but not as 'plm.dim'. The conversion of the data is done inside the function.")
  if(is.null(year)) {
    id.year <- id
    table.id <- table(id.year)
    if(any(table.id>1)) {
      warning("The following observations occur several times", call. = FALSE, immediate.=TRUE)
      return.error <- names(table.id)[table.id>1]
      mode(return.error) <- mode(id)
      return(return.error)
    }
  }
  if(!is.null(year)) {
    id.year <- paste0(id,year)
    table.id <- table(id)
    if(any(table.id>length(unique(year)))) {
      warning("The following observations occur several times in several years!", call. = FALSE, immediate.=TRUE)
      return.error <- names(table.id)[table.id>length(unique(year))]
      mode(return.error) <- mode(id)
      return(return.error)
    }
  }
  
  u.grouping <- sort(unique(grouping))
  lu.grouping <- length(u.grouping)
  no.coefs <- length(sep.string(form,sign=c("+","~")))
  
  sfa.gr <- su.sfa.gr <- coefs.gr <- coefs.meta <- A.gr <- b.gr <- id.year.gr <- eff.gr <- data.gr.l <- fitted.gr <- list()  
  for(i in u.grouping){
    if(count) cat("grouping =", i, "...\n")
    if(timeEffect) {
      # Step 0: Prepare the data
      data.gr <- cbind(data[grouping==i,], id=id[grouping==i], year=year[grouping==i])
      data.gr <- plm::plm.data( data.gr, c( "id", "year" ) )
      id.year.gr[[i]] <- id.year.data.gr <- paste0(data.gr[,"id"],data.gr[,"year"]) 
      # Zum späteren Test, ob das ID Year matching funktioniert.
      #     data.gr.l[[i]] <- data.gr
      # Step 1: The normal group frontier is estimated and the coefficents are extracted
      sfa.gr[[i]] <- sfa.own(formula=form, data=data.gr,timeEffect=TRUE) # , ...
      su.sfa.gr[[i]] <- summary(sfa.gr[[i]]);
      eff0 <- su.sfa.gr[[i]]$effic
      # zur Kontrolle
      #colnames.eff0 <- matrix(rep(colnames(eff0),nrow(eff0)),nrow=nrow(eff0),byrow=TRUE)
      #rownames.eff0 <- matrix(rep(rownames(eff0),ncol(eff0)),ncol=ncol(eff0))
      #id.year.eff.gr <- paste0(t(rownames.eff0),t(colnames.eff0))
      # id.year.data.gr == id.year.eff.gr
      eff.gr[[i]] <- c(t(eff0)); rm(eff0)
      coefs.gr[[i]] <- su.sfa.gr[[i]]$mleParam[1:no.coefs,1]
      # Step 2: A linear problem is solved. It is tried to minimize all coefficients of the meta frontier such that the output f(x) of each observation with the meta frontier
      # is as least as large as the output under the group specific frontier.
      A.gr[[i]] <- model.matrix(form,data=data.gr)
      b.gr[[i]] <- A.gr[[i]]%*%coefs.gr[[i]]
      # Kontrolle
      #     b.gr[[i]] == matrix(t( sfa.gr[[i]]$fitted ))
      if(any(is.na(A.gr[[i]]), A.gr[[i]]%in%c(-Inf,Inf))) stop("remove observations with NA values or that become NaN/Inf/-Inf by formula calculation ( e.g. log(-1) or log(0) ) before the analysis.")
    } else {
      # Step 0: Prepare the data
      data.gr <- data[grouping==i,]
      # Zum späteren Test, ob das ID Year matching funktioniert.
      #     data.gr.l[[i]] <- data.gr
      id.year.gr[[i]] <- id.year[grouping==i]
      # Step 1: The normal group frontier is estimated and the coefficents are extracted
      sfa.gr[[i]] <- sfa.own(formula=form, data=data.gr,timeEffect=FALSE) # , ...
      su.sfa.gr[[i]] <- summary(sfa.gr[[i]])
      eff.gr[[i]] <- c(su.sfa.gr[[i]]$effic)
      coefs.gr[[i]] <- su.sfa.gr[[i]]$mleParam[1:no.coefs,1]
      # Step 2: A linear problem is solved. It is tried to minimize all coefficients of the meta frontier such that the output f(x) of each observation with the meta frontier
      # is as least as large as the output under the group specific frontier.
      A.gr[[i]] <- model.matrix(form,data=data.gr)
      b.gr[[i]] <- A.gr[[i]]%*%coefs.gr[[i]]
      # Kontrolle
      #     b.gr[[i]] == sfa.gr[[i]]$fitted 
      if(any(is.na(A.gr[[i]]))) stop("remove observations with NA values or that get NaN values by formula calculation before the analysis.")
    }
  }
  # Combine all values of the lists
  eff <- do.call("c",eff.gr)
  coefs <- do.call("cbind",coefs.gr); colnames(coefs) <- u.grouping
  id.year.new <- do.call("c",id.year.gr)
  
  A <- do.call("rbind",A.gr)
  b <- do.call("c",b.gr)
  mean.A <- colMeans(A)
  # Zum späteren Test, ob das ID Year matching funktioniert.
  #    data.new <- do.call("rbind",data.gr.l)
  
  # Solve the linear programmimg problem and extract the smalles possible coefficients.
  coefs.meta <- lpsolve(obj_coef=mean.A, A=A, LHS_ge=b, opt_val_ge=rep(-Inf,ncol(A)), opt_val_le=rep(Inf,ncol(A)), maximize=FALSE)$opt_val
  names(coefs.meta) <- names(coefs.gr[[1]])
  coefs <- cbind(coefs,meta=coefs.meta)
  # Slow solver:    coefs.meta <- linprog::solveLP(cvec=mean.x, bvec=b, Amat=x, maximum=FALSE, const.dir = rep("<=",length(b)), lpSolve = FALSE)$solution
  
  if(FALSE){
    # Check results of optimization
    check <- round( cbind(A%*%coefs.meta, b), 5 )
    sum( check[,1]<check[,2] )
    check[check[,1]==check[,2],]
  }
  
  # Defining the meta technology ratio according do O'Donnell (2008) p. 11 (241)
  mtr.gr <- b.meta.gr <- exp.b.meta.gr <- exp.b.gr<- list()
  for(i in u.grouping){
    b.meta.gr[[i]] <- A.gr[[i]]%*%coefs.meta
    exp.b.meta.gr[[i]] <- exp(b.meta.gr[[i]])
    exp.b.gr[[i]] <-  exp(b.gr[[i]])
    #mtr.gr[[i]] <- b.gr[[i]] / b.meta.gr[[i]] 
    mtr.gr[[i]] <- exp.b.gr[[i]] / exp.b.meta.gr[[i]] 
  }
  mtr <- do.call("c",mtr.gr)
  eff.meta <- eff*mtr
  mtr <- round(mtr,5)
  if(any(mtr>1)) stop("An error in optimization occured!    any(mtr>1)==TRUE")
  if(any(mtr<0)) stop("An error in optimization occured!    any(mtr<0)==TRUE")
  
  # Bring the values back to the original order
  nwo <- match(id.year,id.year.new) #; all(id==do.call("c",id.test)[nwo])    # Check if it worked
  mtr <- mtr[nwo] # As data.greff and
  eff <- eff[nwo]
  eff.meta <- eff.meta[nwo]
  
  #if(FALSE){
  #  # Test, ob die Reihenfolge wieder stimmt:
  #  data.new[nwo,c("ID","Jahr")]==data[,c("ID","Jahr")]
  #  all(data.new[nwo,c("ID","Jahr")]==data[,c("ID","Jahr")])
  #}
  
  summasdna <- function(x,na.rm=TRUE)  {
    return(c(summary(c(x),na.rm=na.rm)[1:6],"SD"=sd(x,na.rm=na.rm),"NA's"=sum(is.na(x))))
  }
  
  mtr.sum <- do.call("cbind", lapply(mtr.gr,function(x)summasdna(x)))
  # Kontrolle, ob die Reihenfolge stimmt  do.call("cbind", tapply(mtr,grouping,function(x)summasdna(x))) == mtr.sum
  mtr.sum <- cbind(all=summasdna(mtr),mtr.sum)
  if(sum(mtr.sum[8,])==0) mtr.sum <- mtr.sum[1:7,]
  
  eff.sum <- do.call("cbind", lapply(eff.gr,function(x)summasdna(x)))
  # Kontrolle, ob die Reihenfolge stimmt   do.call("cbind", tapply(eff,grouping,function(x)summasdna(x)) )  == eff.sum
  eff.sum <- cbind(all=summasdna(eff),eff.sum)
  if(sum(eff.sum[8,])==0) eff.sum <- eff.sum[1:7,]
  
  eff.meta.sum <- do.call("cbind", tapply(eff.meta,grouping,function(x)summasdna(x)) )
  eff.meta.sum <- cbind(all=summasdna(eff),eff.meta.sum)
  if(sum(eff.meta.sum[8,])==0) eff.meta.sum <- eff.meta.sum[1:7,]
  
  colnames(mtr.sum)[2:ncol(mtr.sum)] <- colnames(eff.sum)[2:ncol(eff.sum)] <- colnames(eff.meta.sum)[2:ncol(eff.meta.sum)] <- u.grouping
  mtr.sum <- round(mtr.sum, eff.digits); eff.sum <- round(eff.sum, eff.digits); eff.meta.sum <- round(eff.meta.sum, eff.digits)
  
  output.table <- data.frame(eff=eff, eff.meta=eff.meta, mtr=mtr, id=id, year=year, grouping=grouping)
  
  result <- list()
  result$sfa.grouped <- sfa.gr
  result$sfa.meta <- coefs.meta
  result$coef <- coefs
  result$table <- output.table
  result$eff <- eff
  result$eff.sum <- eff.sum
  result$eff.meta <- eff.meta
  result$eff.meta.sum <- eff.meta.sum
  result$mtr <- mtr
  result$mtr.sum <- mtr.sum
  
  # If wished an extensive output is produced.
  if(large.output){
    b.meta <- do.call("c",b.meta.gr)
    exp.b <- do.call("c",exp.b.gr)
    exp.b.meta <- do.call("c",exp.b.meta.gr)
    
    b <- b[nwo]
    b.meta <- b.meta[nwo]
    exp.b <- exp.b[nwo]
    exp.b.meta <- exp.b.meta[nwo]
    A <- A[nwo,]
    
    b.sum <- do.call("cbind", lapply(b.gr,function(x)summasdna(x))); #  do.call("cbind", tapply(b,grouping,function(x)summasdna(x))) == b.sum
    b.sum <- cbind(all=summasdna(b.sum),b.sum)
    if(sum(b.sum[8,])==0) b.sum <- b.sum[1:7,]
    b.meta.sum <- do.call("cbind", lapply(b.meta.gr,function(x)summasdna(x)));
    b.meta.sum <- cbind(all=summasdna(b.meta.sum),b.meta.sum)
    if(sum(b.meta.sum[8,])==0) b.meta.sum <- b.meta.sum[1:7,]
    exp.b.sum <- do.call("cbind", lapply(exp.b.gr,function(x)summasdna(x))); #  do.call("cbind", tapply(b,grouping,function(x)summasdna(x))) == exp.b.sum
    exp.b.sum <- cbind(all=summasdna(exp.b.sum),exp.b.sum)
    if(sum(exp.b.sum[8,])==0) exp.b.sum <- exp.b.sum[1:7,]
    exp.b.meta.sum <- do.call("cbind", lapply(exp.b.meta.gr,function(x)summasdna(x)));
    exp.b.meta.sum <- cbind(all=summasdna(exp.b.meta.sum),exp.b.meta.sum)
    if(sum(exp.b.meta.sum[8,])==0) exp.b.meta.sum <- exp.b.meta.sum[1:7,]
    A.sum <- apply(A,2,function(x)summasdna(x))
    
    colnames(b.sum)[2:ncol(b.sum)]  <- colnames(b.meta.sum)[2:ncol(b.meta.sum)]  <- colnames(exp.b.sum)[2:ncol(exp.b.sum)] <- colnames(exp.b.meta.sum)[2:ncol(exp.b.meta.sum)] <- u.grouping
    
    result$y <- b
    result$y.sum <- b.sum
    result$y.meta <- b.meta
    result$y.meta.sum <- b.meta.sum
    result$exp.y <- exp.b
    result$exp.y.sum <- exp.b.sum
    result$exp.y.meta <- exp.b.meta
    result$exp.y.meta.sum <- exp.b.meta.sum
    result$model.matrix <- A 
  } else {
    result$y <- NULL
    result$y.sum  <- NULL
    result$y.meta  <- NULL
    result$y.meta.sum  <- NULL
    result$exp.y  <- NULL
    result$exp.y.sum  <- NULL
    result$exp.y.meta  <- NULL
    result$exp.y.meta.sum  <- NULL
    result$model.matrix  <- NULL
  }
  result$id <- id
  result$grouping <- grouping
  result$year <- year
  result$short.print <- short.print
  
  class(result) <- "sfa.meta"
  
  return(result)
}

print.sfa.meta <- function(object,short.print=NULL,digits=3,...){
  if(is.null(short.print)) short.print <- object$short.print
  sums <- lapply(as.list(names(object)),function(x)substr(x,nchar(x)-2,nchar(x)))=="sum"
  sums.not.null <- sums & unlist(lapply(object,function(x)!is.null(x)))
  
  print_it <- NULL
  for(i in which(sums.not.null)) print_it <- c(print_it, list(object[[i]]) )
  print_it <- lapply(print_it,function(x)round(x,digits))
  if(short.print) print_it <- lapply(print_it,function(x)x[c(4,7),])
  names(print_it) <- names(object)[sums.not.null]
  
  print(print_it)
  invisible(print_it)
}

sfa.own <- function (formula, data = sys.frame(sys.parent()), ineffDecrease = TRUE, 
                     truncNorm = FALSE, timeEffect = FALSE, startVal = NULL, tol = 1e-05, 
                     maxit = 1000, muBound = 2, bignum = 1e+16, searchStep = 1e-05, 
                     searchTol = 0.001, searchScale = NA, gridSize = 0.1, gridDouble = TRUE, 
                     restartMax = 10, restartFactor = 0.999, printIter = 0) 
{
  library(frontier)
  # This function is needed to have an immediate warning message inside the sfa.meta function
  # such that we can see in which country the model is not appropriate.
  formula <- Formula::as.Formula(formula)
  if (length(formula)[2] == 1) {
    modelType <- 1
    effFormula <- NULL
  }
  else if (length(formula)[2] == 2) {
    modelType <- 2
    effFormula <- formula(formula, lhs = 0, rhs = 2)
  }
  else {
    stop("argument 'formula' has an inappropriate number of RHS parts")
  }
  formula <- formula(formula, lhs = 1, rhs = 1)
  if (class(formula) != "formula") {
    stop("argument 'formula' must be a formula")
  }
  else if (length(formula) != 3) {
    stop("argument 'formula' must be a 2-sided formula")
  }
  if (!is.logical(ineffDecrease) || length(ineffDecrease) != 
        1) {
    stop("argument 'ineffDecrease' must be a single logical value")
  }
  if (!is.logical(truncNorm)) {
    stop("argument 'truncNorm' must be logical")
  }
  if (truncNorm && modelType == 2) {
    warning("argument 'truncNorm' is ignored in", " Efficiency Effects Frontiers (EEF)")
  }
  if (!is.logical(timeEffect)) {
    stop("argument 'timeEffect' must be logical")
  }
  if (timeEffect && !"plm.dim" %in% class(data)) {
    warning("argument 'timeEffect' is ignored in case of", 
            " cross-sectional data")
  }
  if (!is.numeric(printIter)) {
    stop("argument 'printIter' must be numeric")
  }
  else if (printIter != round(printIter)) {
    stop("argument 'printIter' must be an iteger")
  }
  else if (printIter < 0) {
    stop("argument 'printIter' must be non-negative")
  }
  printIter <- as.integer(printIter)
  if (length(searchScale) != 1) {
    stop("argument 'searchScale' must be a single logical value or NA")
  }
  else if (is.na(searchScale)) {
    indic <- as.integer(1)
  }
  else if (is.logical(searchScale)) {
    indic <- as.integer(2 - 2 * searchScale)
  }
  else {
    stop("argument 'searchScale' must be a logical value or NA")
  }
  if (!is.numeric(tol)) {
    stop("argument 'tol' must be numeric")
  }
  else if (tol < 0) {
    stop("argument 'tol' must be non-negative")
  }
  if (!is.numeric(searchTol)) {
    stop("argument 'searchTol' must be numeric")
  }
  else if (searchTol < 0) {
    stop("argument 'searchTol' must be non-negative")
  }
  if (!is.numeric(muBound) || length(muBound) != 1) {
    stop("argument 'muBound' must be a numeric scalar")
  }
  else if (is.infinite(muBound)) {
    muBound <- 0
  }
  if (!is.numeric(bignum)) {
    stop("argument 'bignum' must be numeric")
  }
  else if (bignum <= 0) {
    stop("argument 'bignum' must be positive")
  }
  if (!is.numeric(searchStep)) {
    stop("argument 'searchStep' must be numeric")
  }
  else if (searchStep <= 0) {
    stop("argument 'searchStep' must be positive")
  }
  if (!is.logical(gridDouble) || length(gridDouble) != 1) {
    stop("argument 'gridDouble' must be a single logical value")
  }
  if (!is.numeric(gridSize)) {
    stop("argument 'gridSize' must be numeric")
  }
  else if (gridSize <= 0) {
    stop("argument 'gridSize' must be positive")
  }
  if (!is.numeric(maxit) || length(maxit) != 1) {
    stop("argument 'maxit' must be a single numeric scalar")
  }
  else if (maxit != round(maxit)) {
    stop("argument 'maxit' must be an integer")
  }
  else if (maxit < 0) {
    stop("argument 'maxit' must not be negative")
  }
  maxit <- as.integer(maxit)
  if (!is.numeric(restartMax) || length(restartMax) != 1) {
    stop("argument 'restartMax' must be a single numeric scalar")
  }
  else if (restartMax != round(restartMax)) {
    stop("argument 'restartMax' must be an integer")
  }
  else if (restartMax < 0) {
    stop("argument 'restartMax' must not be negative")
  }
  restartMax <- as.integer(restartMax)
  if (!is.numeric(restartFactor) || length(restartFactor) != 
        1) {
    stop("argument 'restartFactor' must be a numeric scalar")
  }
  else if (is.infinite(restartFactor)) {
    stop("argument 'restartFactor' must be finite")
  }
  mc <- match.call(expand.dots = FALSE)
  m <- match("data", names(mc), 0)
  mf <- mc[c(1, m)]
  mf$formula <- formula
  attributes(mf$formula) <- NULL
  mf$na.action <- na.pass
  mf[[1]] <- as.name("model.frame")
  mf <- eval(mf, parent.frame())
  mt <- attr(mf, "terms")
  xMat <- model.matrix(mt, mf)
  xNames <- colnames(xMat)
  yVec <- model.response(mf)
  yName <- as.character(formula)[2]
  if (length(yVec) != nrow(xMat)) {
    stop("the number of observations of the endogenous variable (", 
         length(yVec), ") is not equal to the number of observations", 
         " of the exogenous variables (", nrow(xMat), ")")
  }
  if ("plm.dim" %in% class(data)) {
    dataTable <- matrix(as.integer(data[[1]]), ncol = 1)
    dataTable <- cbind(dataTable, as.integer(data[[2]]))
  }
  else {
    dataTable <- matrix(1:length(yVec), ncol = 1)
    dataTable <- cbind(dataTable, rep(1, nrow(dataTable)))
  }
  nb <- length(xNames)
  dataTable <- cbind(dataTable, yVec)
  if (sum(!is.na(yVec) & is.finite(yVec)) == 0) {
    stop("the dependent variable has no valid observations")
  }
  dataTable <- cbind(dataTable, xMat)
  paramNames <- NULL
  if (nb > 0) {
    for (i in 1:nb) {
      paramNames <- c(paramNames, xNames[i])
      if (sum(!is.na(xMat[, i]) & is.finite(xMat[, i])) == 
            0) {
        stop("regressor '", xNames[i], "' has no valid observations")
      }
    }
  }
  if (is.null(effFormula)) {
    zNames <- NULL
    zIntercept <- FALSE
  }
  else {
    if (class(effFormula) != "formula") {
      stop("argument 'effFormula' must be a formula")
    }
    else if (length(effFormula) != 2) {
      stop("argument 'formula' must be a 1-sided formula")
    }
    me <- match("data", names(mc), 0)
    mfe <- mc[c(1, me)]
    mfe$formula <- effFormula
    attributes(mfe$formula) <- NULL
    mfe$na.action <- na.pass
    mfe[[1]] <- as.name("model.frame")
    mfe <- eval(mfe, parent.frame())
    mte <- attr(mfe, "terms")
    zMat <- model.matrix(mte, mfe)
    if (ncol(zMat) > 0 && colnames(zMat)[1] == "(Intercept)") {
      zIntercept <- TRUE
      zMat <- zMat[, -1, drop = FALSE]
    }
    else {
      zIntercept <- FALSE
    }
    if (nrow(zMat) != nrow(xMat)) {
      stop("the number of observations of the variables explaining", 
           " efficiency (", nrow(zMat), ") is not equal to the number", 
           " of observations of the (regular) regressors (", 
           nrow(xMat), ")")
    }
    dataTable <- cbind(dataTable, zMat)
    zNames <- colnames(zMat)
    if (length(zNames) > 0) {
      for (i in 1:length(zNames)) {
        if (sum(!is.na(zMat[, i]) & is.finite(zMat[, 
                                                   i])) == 0) {
          stop("the regressor for the inefficiency term '", 
               zNames[i], "' has no valid observations")
        }
      }
    }
  }
  nZvars <- length(zNames)
  validObs <- rowSums(is.na(dataTable) | is.infinite(dataTable)) == 
    0
  dataTable <- dataTable[validObs, ]
  nob <- sum(validObs)
  firmId <- sort(unique(dataTable[, 1]))
  nn <- length(firmId)
  firmNo <- rep(NA, nrow(dataTable))
  for (i in 1:nn) {
    firmNo[dataTable[, 1] == firmId[i]] <- i
  }
  dataTable[, 1] <- firmNo
  if (any(is.na(dataTable[, 1]))) {
    stop("internal error: at least one firm number is NA")
  }
  if (min(dataTable[, 1]) != 1) {
    stop("internal error: the smallest firm number must be one")
  }
  if (max(dataTable[, 1]) > nn) {
    stop("internal error: a firm number is larger than the number of firms")
  }
  timeId <- sort(unique(dataTable[, 2]))
  nt <- length(unique(dataTable[, 2]))
  timeNo <- rep(NA, nrow(dataTable))
  for (i in 1:nt) {
    timeNo[dataTable[, 2] == timeId[i]] <- i
  }
  dataTable[, 2] <- timeNo
  if (any(is.na(dataTable[, 2]))) {
    stop("internal error: at least one time period number is NA")
  }
  if (min(dataTable[, 2]) != 1) {
    stop("internal error: the smallest time period number must be one")
  }
  if (max(dataTable[, 2]) > nt) {
    stop("internal error: a time period number is larger", 
         " than the number of time periods")
  }
  for (i in 1:nn) {
    for (j in 1:nt) {
      if (sum(dataTable[, 1] == i & dataTable[, 2] == j) > 
            1) {
        stop("more than one observation for firm '", 
             firmId[i], "' in period '", timeId[j], "'")
      }
    }
  }
  if (modelType == 1) {
    mu <- truncNorm
  }
  else {
    mu <- zIntercept
  }
  if (modelType == 1) {
    eta <- timeEffect
  }
  else {
    eta <- nZvars
  }
  colnames(dataTable) <- c("id", "t", yName, xNames, zNames)
  if (!is.null(rownames(data))) {
    obsNames <- rownames(data)
  }
  else if (!is.null(names(yVec))) {
    obsNames <- names(yVec)
  }
  else if (!is.null(rownames(xMat))) {
    obsNames <- rownames(xMat)
  }
  else if (!is.null(rownames(zMat))) {
    obsNames <- rownames(zMat)
  }
  else {
    obsNames <- NULL
  }
  rownames(dataTable) <- obsNames[validObs]
  names(validObs) <- obsNames
  nParamTotal <- nb + 2 + mu + eta
  if (nParamTotal > nob) {
    stop("the model cannot be estimated,", " because the number of parameters (", 
         nParamTotal, ") is larger than the number of", ifelse(sum(!validObs) > 
                                                                 0, " valid", ""), " observations (", nob, ")")
  }
  if (is.null(startVal)) {
    startVal <- 0
  }
  else {
    if (nParamTotal != length(startVal)) {
      stop("wrong number of starting values (you provided ", 
           length(startVal), " starting values but the model has ", 
           nParamTotal, " parameters)")
    }
  }
  if (nb > 0) {
    ols <- lm(dataTable[, 3] ~ dataTable[, 4:(3 + nb)] - 
                1)
  }
  else if (nb == 0) {
    ols <- lm(dataTable[, 3] ~ -1)
  }
  olsParam <- c(coef(ols), summary(ols)$sigma^2)
  olsStdEr <- sqrt(diag(vcov(ols)))
  olsLogl <- logLik(ols)[1]
  if (nb > 0) {
    gridAdj <- coef(lm(rep(1, nrow(dataTable)) ~ dataTable[, 
                                                           4:(3 + nb)] - 1))
  }
  else {
    gridAdj <- numeric(0)
  }
  if (length(gridAdj) != nb) {
    stop("internal error: the length of 'gridAdj' is not equal to 'nb'.", 
         " Please contact the maintainer of the frontier package")
  }
  returnObj <- .Fortran("front41", modelType = as.integer(modelType), 
                        ineffDecrease = as.integer((!ineffDecrease) + 1), icept = as.integer(0), 
                        nn = as.integer(nn), nt = as.integer(nt), nob = as.integer(nob), 
                        nb = as.integer(nb), mu = as.integer(mu), eta = as.integer(eta), 
                        printIter = as.integer(printIter), indic = as.integer(indic), 
                        tol = as.double(tol), searchTol = as.double(searchTol), 
                        bignum = as.double(bignum), searchStep = as.double(searchStep), 
                        gridDouble = as.integer(gridDouble), gridSize = as.double(gridSize), 
                        maxit = as.integer(maxit), muBound = as.double(muBound), 
                        restartMax = as.integer(restartMax), restartFactor = as.double(restartFactor), 
                        nRestart = as.integer(0), nStartVal = as.integer(length(startVal)), 
                        startVal = as.double(startVal), nRowData = as.integer(nrow(dataTable)), 
                        nColData = as.integer(ncol(dataTable)), dataTable = matrix(as.double(dataTable), 
                                                                                   nrow(dataTable), ncol(dataTable), dimnames = dimnames(dataTable)), 
                        nParamTotal = as.integer(nParamTotal), olsParam = as.double(c(olsParam, 
                                                                                      rep(0, 1 + mu + eta))), gridAdj = as.double(gridAdj), 
                        gridParam = as.double(rep(0, nParamTotal)), startLogl = as.double(0), 
                        mleParam = as.double(rep(0, nParamTotal)), mleCov = matrix(as.double(0), 
                                                                                   nParamTotal, nParamTotal), mleLogl = as.double(0), 
                        nIter = as.integer(0), code = as.integer(0), nFuncEval = as.integer(0))
  if (returnObj$code == 101) {
    stop("the total number of observations exceeds the product of", 
         " the number of firms by the number of years")
  }
  else if (returnObj$code == 102) {
    stop("internal error: calculated variable 'n'", " is not equal to argument 'nParamTotal'.", 
         " Please contact the maintainer of the 'frontier' package", 
         " (arne.henningsen@gmail.com)")
  }
  else if (returnObj$code == 103) {
    stop("wrong number of starting values")
  }
  else if (returnObj$code == 104) {
    stop("a firm number is < 1")
  }
  else if (returnObj$code == 105) {
    stop("a firm number is > number of firms")
  }
  else if (returnObj$code == 106) {
    stop("a period number is < 1")
  }
  else if (returnObj$code == 107) {
    stop("a period number is > number of periods")
  }
  else if (returnObj$code == 108) {
    stop("there are no observations on at least one firm")
  }
  else if (returnObj$code == 109) {
    stop("internal error: 2 + nr - nmu * (im-1)", " is not equal to argument 'nColData'.", 
         " Please contact the maintainer of the 'frontier' package", 
         " (arne.henningsen@gmail.com)")
  }
  else if (returnObj$code > 100) {
    stop("unknown error.", " Please contact the maintainer of the 'frontier' package", 
         " (arne.henningsen@gmail.com)")
  }
  returnObj$nStartVal <- NULL
  returnObj$nRowData <- NULL
  returnObj$nColData <- NULL
  returnObj$nParamTotal <- NULL
  returnObj$ineffDecrease <- as.logical(2 - returnObj$ineffDecrease)
  returnObj$gridDouble <- as.logical(returnObj$gridDouble)
  returnObj$olsParam <- olsParam
  returnObj$olsStdEr <- olsStdEr
  returnObj$olsLogl <- olsLogl
  if (ncol(xMat) == 0) {
    fitVal <- rep(0, sum(validObs))
  }
  else {
    fitVal <- drop(xMat[validObs, , drop = FALSE] %*% returnObj$mleParam[1:nb])
  }
  returnObj$fitted <- matrix(NA, nrow = nn, ncol = nt)
  if (length(fitVal) != nrow(dataTable)) {
    stop("internal error: length of the fitted values is not equal to", 
         " the number of rows of the data table (valid observations)")
  }
  for (i in 1:length(fitVal)) {
    returnObj$fitted[dataTable[i, 1], dataTable[i, 2]] <- fitVal[i]
  }
  resid <- drop(dataTable[, 3] - fitVal)
  returnObj$resid <- matrix(NA, nrow = nn, ncol = nt)
  if (length(resid) != nrow(dataTable)) {
    stop("internal error: length of residuals is not equal to", 
         " the number of rows of the data table (valid observations)")
  }
  for (i in 1:length(resid)) {
    returnObj$resid[dataTable[i, 1], dataTable[i, 2]] <- resid[i]
  }
  returnObj$olsResid <- residuals(ols)
  returnObj$olsSkewness <- moments::skewness(returnObj$olsResid)
  returnObj$olsSkewnessOkay <- returnObj$olsSkewness * (-1)^ineffDecrease >= 
    0
  warnMaxit <- maxit <= returnObj$nIter && maxit > 0
  if (!returnObj$olsSkewnessOkay && returnObj$mleLogl < returnObj$olsLogl) {
    warning("the residuals of the OLS estimates are ", ifelse(ineffDecrease, 
                                                              "right", "left"), "-skewed", " and the likelihood value of the ML estimation is less", 
            " than that obtained using OLS;", " this usually indicates that there is no inefficiency", 
            " or that the model is misspecified", call.=FALSE, immediate.=TRUE)
  }
  else if (!returnObj$olsSkewnessOkay) {
    warning("the residuals of the OLS estimates are ", ifelse(ineffDecrease, 
                                                              "right", "left"), "-skewed;", " this might indicate that there is no inefficiency", 
            " or that the model is misspecified", call.=FALSE, immediate.=TRUE)
  }
  else if (returnObj$mleLogl < returnObj$olsLogl && warnMaxit) {
    warning("the maximum number of iterations has been reached and", 
            " the likelihood value of the ML estimation is less", 
            " than that obtained using OLS;", " please try again using different starting values and/or", 
            " increase the maximum number of iterations")
    warnMaxit <- FALSE
  }
  else if (returnObj$mleLogl < returnObj$olsLogl && maxit > 
             0) {
    warning("the likelihood value of the ML estimation is less", 
            " than that obtained using OLS;", " this indicates that the likelihood maximization did not", 
            " converge to the global maximum or", " that there is no inefficiency", 
            " (you could try again using different starting values)")
  }
  if (warnMaxit) {
    warning("the maximum number of iterations has been reached;", 
            " please try again using different starting values and/or", 
            " increase the maximum number of iterations")
  }
  if (modelType == 1) {
    returnObj$truncNorm <- as.logical(returnObj$mu)
    returnObj$zIntercept <- zIntercept
    returnObj$mu <- NULL
  }
  else {
    returnObj$truncNorm <- truncNorm
    returnObj$zIntercept <- as.logical(returnObj$mu)
    returnObj$mu <- NULL
  }
  if (modelType == 1) {
    returnObj$timeEffect <- as.logical(returnObj$eta)
  }
  else {
    returnObj$timeEffect <- timeEffect
  }
  returnObj$eta <- NULL
  if (returnObj$indic == 2) {
    returnObj$searchScale <- FALSE
  }
  else if (returnObj$indic == 1) {
    returnObj$searchScale <- NA
  }
  else {
    returnObj$searchScale <- TRUE
  }
  returnObj$indic <- NULL
  if (length(startVal) == 1) {
    if (modelType == 1) {
      idx <- 1:(nb + 2)
    }
    else {
      if (nb == 0) {
        idx <- NULL
      }
      else {
        idx <- 1:nb
      }
      idx <- c(idx, (nParamTotal - 1):nParamTotal)
    }
    if (any(returnObj$gridParam[(1:nParamTotal)[-idx]] != 
              0)) {
      warning("internal error: some unused grid-search parameters are", 
              " non-zero: ", paste(returnObj$gridParam, collapse = " "), 
              " please contact the maintainer of the 'frontier' package")
    }
    returnObj$gridParam <- returnObj$gridParam[idx]
    names(returnObj)[names(returnObj) == "startLogl"] <- "gridLogl"
  }
  else {
    returnObj$gridParam <- NULL
  }
  if ("plm.dim" %in% class(data)) {
    rownames(returnObj$resid) <- levels(data[[1]])[firmId]
    colnames(returnObj$resid) <- levels(data[[2]])[timeId]
  }
  else {
    rownames(returnObj$resid) <- obsNames[validObs]
    colnames(returnObj$resid) <- "residuals"
  }
  if (modelType == 2) {
    if (zIntercept) {
      paramNames <- c(paramNames, "Z_(Intercept)")
    }
    if (nZvars > 0) {
      paramNames <- c(paramNames, paste("Z", zNames, sep = "_"))
    }
  }
  if (length(startVal) == 1) {
    returnObj$startVal <- NULL
  }
  paramNames <- c(paramNames, "sigmaSq", "gamma")
  if (modelType == 1) {
    if (truncNorm) {
      paramNames <- c(paramNames, "mu")
    }
    if (timeEffect) {
      paramNames <- c(paramNames, "time")
    }
  }
  if (nb >= 1) {
    betaNames <- paramNames[1:nb]
  }
  else {
    betaNames <- NULL
  }
  names(returnObj$olsParam) <- c(betaNames, "sigmaSq")
  names(returnObj$olsStdEr) <- betaNames
  if (!is.null(returnObj$gridParam)) {
    names(returnObj$gridParam) <- c(betaNames, "sigmaSq", 
                                    "gamma")
  }
  names(returnObj$mleParam) <- paramNames
  rownames(returnObj$mleCov) <- paramNames
  colnames(returnObj$mleCov) <- paramNames
  if (!is.null(returnObj$startVal)) {
    names(returnObj$startVal) <- paramNames
  }
  returnObj$call <- match.call()
  returnObj$validObs <- validObs
  if (maxit > 0) {
    if ((returnObj$mleParam["gamma"] < 0.01 || returnObj$mleParam["gamma"] > 
           0.99)) {
      warning("the parameter 'gamma' is close to the boundary", 
              " of the parameter space [0,1]:", " this can cause convergence problems and", 
              " can negatively affect the validity and reliability", 
              " of statistical tests", " and might be caused by model misspecification", call.=FALSE, immediate.=TRUE)
    }
    if (!semidefiniteness(returnObj$mleCov)) {
      warning("the covariance matrix of the maximum likelihood estimates", 
              " is not positive semidefinite")
    }
    else {
      testSingularCov <- try(solve(returnObj$mleCov), silent = TRUE)
      if (class(testSingularCov) == "try-error") {
        if (grepl("singular", testSingularCov[1])) {
          warning("the covariance matrix of the maximum likelihood estimates", 
                  " is singular")
        }
        else {
          warning("the covariance matrix of the maximum likelihood estimates", 
                  " is not invertible")
        }
      }
    }
  }
  class(returnObj) <- "frontier"
  return(returnObj)
}

dea.bias <- function(x,y,rts=c("vrs","crs","nirs","ndrs"),or=c("in","out","hyperbolic"),nrep=1000, h=0.014) {
  ## Erkärung
  # --> This function does not really work!!!
  # Use boot.sw98() instead.
  # Reference: Bogetoft & Otto (2011): Benchmarking with DEA, SFA and R. chapter 6.4 p. 173-175
  # x & y: input & output matrices. With N observations and nrow(x)==nrow(y)==N
  # rts: Returns to Scale assumption
  # or: orientation
  # nrep: number of repetition (bootstrapping)
  # h: bandwidth used for smooth bootstrapping (see p.172)
  
  ## Pre-calculations & Checks
  if(nrow(x)!=nrow(y)) stop("nrow(x) must be equal nrow(y)")
  rts <- match.arg(rts)
  if(rts=="vrs") rts<-1 else if(rts=="nirs") rts<-2 else if(rts=="crs") rts<-3 else if(rts=="ndrs") rts<-4
  or <- match.arg(or)
  if(or=="in") or<-1 else if(or=="out") or<-2 else if(or=="hyperbolic") or<-3
  x <- t(x)
  y <- t(y)
  N <- ncol(x)
  B <- nrep
  nx <- nrow(x)
  ny <- nrow(y)
  
  ## Bias-Correction algorithm (p. 175)
  # (1)
  theta <- 1/FEAR::dea(x,y,RTS=rts,ORIENTATION=or)
  # (2.1)
  beta <- eps <- matrix(nrow=B,ncol=1)
  beta <- t( apply(beta,1,function(x)sample(theta, N, replace=TRUE)) )
  # (2.2)
  eps <- t( apply(eps,1,function(x)rnorm(N)) )
  # (2.3)
  thetatilde <- matrix(nrow=B, ncol=N)
  condition <- beta + h*eps <= 1
  thetatilde[condition] <- beta[condition] + h*eps[condition]
  thetatilde[!condition] <- 2.0 -beta[!condition] - h*eps[!condition]
  # (2.4)
  v <- var(theta)
  sqrt.c <- sqrt(1.+h^2/v)
  mean.beta <- array( apply(beta,1,function(x)mean(x)) ,dim=dim(beta))
  thetastar <- mean.beta + (thetatilde-mean.beta ) / sqrt.c
  
  # Für multiple input/output (ohne t() in apply-Funktion --> effizient!)
  # (3)
  xstar <- array(NA,dim=c(B,nx,N))
  ystar <- array(NA,dim=c(B,ny,N))
  if(or==1){
    for(i in 1:nx)  xstar[,i,] <- matrix(theta,nrow=B,ncol=N,byrow=TRUE) /thetastar * matrix(x[i,],nrow=B,ncol=N,byrow=TRUE)
    for(i in 1:ny)  ystar[,i,] <- matrix(y[i,],nrow=B,ncol=N,byrow=TRUE)
  } else if(or==2) { # Ich weiss nicht, ob das richtig ist...?
    for(i in 1:nx)  xstar[,i,] <- matrix(x[i,],nrow=B,ncol=N,byrow=TRUE)
    for(i in 1:ny)  ystar[,i,] <- matrix(theta,nrow=B,ncol=N,byrow=TRUE) /thetastar * matrix(y[i,],nrow=B,ncol=N,byrow=TRUE)
  }
  xystar <- array(NA,dim=c(B,(nx+ny),N))
  xystar[,1:nx,] <- xstar
  xystar[,(nx+1):dim(xystar)[2],] <- ystar
  thetaboot <- t( apply(xystar,1,function(x)1/FEAR::dea(XOBS=x[1:nx,,drop=FALSE],YOBS=x[(nx+1):nrow(x),,drop=FALSE],RTS=rts,ORIENTATION=or)) )
  # Test
  #   x <- xystar[1,,]
  #   x;t(x)
  #   1/FEAR::dea(XOBS=x[1:nx,drop=FALSE,],YOBS=x[(nx+1):nrow(x),,drop=FALSE],RTS=rts,ORIENTATION=or)
  # (4)
  
  
  # Für multiple input/output (aber t() in der apply-Funktion nötig! Ineffizient!)
  ## (3)
  #xstar <- array(NA,dim=c(B,N,nx))
  #ystar <- array(NA,dim=c(B,N,ny))
  #if(or==1){
  #  for(i in 1:nx)  xstar[,,i] <- matrix(theta,nrow=B,ncol=N,byrow=TRUE) /thetastar * matrix(x[i,],nrow=B,ncol=N,byrow=TRUE)
  #  for(i in 1:ny)  ystar[,,i] <- matrix(y[i,],nrow=B,ncol=N,byrow=TRUE)
  #} else if(or==2) { # Ich weiss nicht, ob das richtig ist...?
  #  for(i in 1:nx)  xstar[,,i] <- matrix(x[i,],nrow=B,ncol=N,byrow=TRUE)
  #  for(i in 1:ny)  ystar[,,i] <- matrix(theta,nrow=B,ncol=N,byrow=TRUE) /thetastar * matrix(y[i,],nrow=B,ncol=N,byrow=TRUE)
  #}
  #xystar <- array(NA,dim=c(B,N,(nx+ny)))
  #xystar[,,1:nx] <- xstar
  #xystar[,,(nx+1):dim(xystar)[3]] <- ystar
  ## Test
  ##   x <- xystar[1,,]
  ##   x;t(x)
  ##   1/FEAR::dea(XOBS=t(x[,1:nx,drop=FALSE]),YOBS=t(x[,(nx+1):ncol(x),drop=FALSE]),RTS=rts,ORIENTATION=or)
  ## (4)
  #thetaboot <- t( apply(xystar,1,function(x)1/FEAR::dea(XOBS=t(x[,1:nx,drop=FALSE]),YOBS=t(x[,(nx+1):ncol(x),drop=FALSE]),RTS=rts,ORIENTATION=or)) )
  
  mean.theta <- apply(thetaboot,2,function(x)mean(x))
  mean.thetatilde <- apply(thetatilde,2,function(x)mean(x))
  bias <- thetaboot - thetatilde
  mean.bias <- mean.theta - mean.thetatilde
  sd.theta <- apply(thetaboot,2,function(x)sd(x))
  
  result <- list(TE.orig=mean.theta, TE.corr=mean.thetatilde, bias=mean.bias)
  return(result)
}
###

#### DELETE FUNCTIONS ####
if(FALSE){
  ####
  regressionen.ln.abschnitte.einzeln.DELETE <- function(data, columns=NULL, selection.variable, selection.levels, verhaeltnis=NULL) {
    # directly return P-values out of Regression
    
    if(is.null(columns)) columns <- colnames(data)
    if(!is.null(columns)) data <- data[,columns]
    c.table <- matrix(nrow=length(selection.levels), ncol=length(columns)+1); rownames(c.table) <- c(1:nrow(c.table))
    p.table <- matrix(nrow=length(selection.levels), ncol=length(columns)+1); rownames(p.table) <- c(1:nrow(p.table))
    r.table <- matrix(nrow=length(selection.levels), ncol=length(columns)+1); rownames(r.table) <- c(1:nrow(r.table))
    
    for (i in 1:length(selection.levels)) {
      if (i==length(selection.levels)) mean.data <- data[which(data[,selection.variable] > selection.levels[i]),] else  mean.data <- data[which(data[,selection.variable] > selection.levels[i] & data[,selection.variable] <= selection.levels[i+1]),]
      
      for (j in 2:length(columns))
      {model <- lm(mean.data[,columns[j]] ~ mean.data[,selection.variable])
       c.table[i,j] <- unname(model$coefficients[2])
       p.table[i,j] <- extract.regression.p(model)
       r.table[i,j] <- summary(model)$r.squared}
      n <- nrow(mean.data); c.table[i, ncol(c.table)] <- n; p.table[i, ncol(p.table)] <- n; r.table[i, ncol(r.table)] <- n
      rownames(c.table)[i] <- rownames(p.table)[i] <- rownames(r.table)[i] <- paste(round(selection.levels[i], digits=1), "<", selection.variable, "<=", round(selection.levels[i+1],digits=1))
      #if (!is.null(verhaeltnis)) {verh <- mean[1,verhaeltnis[1]]/mean[1,verhaeltnis[2]]; mean <- cbind(mean,verh); colnames(mean)[ncol(mean)] <- paste(verhaeltnis[1], verhaeltnis[2], sep="/")}
    }
    colnames(c.table) <- c(columns, "N")
    colnames(p.table) <- c(paste("P",columns, sep="_"), "N")
    colnames(r.table) <- c(paste("R^2", columns, sep="_"), "N")
    output <- list(c.table=c.table, p.table=p.table, r.table=r.table)
    return(output)
  }
  
  ####
  ### Regression zusammen ###
  regressionen.ln.abschnitte.DELETE <- function(form, data, selection.variable, selection.levels) { #dependent, explaining,
    if (class(form)!="formula") stop("Please enter the fromula as form= formula(a ~ b + c + d ...)") # MUSS EIGENTLICH NICHT SEIN! formula = x~y geht auch!
    # directly return P-values out of Regression
    
    s.list <- vector("list", (length(selection.levels)-1))    
    c.matrix <- matrix(ncol=length(unname(lm(formula=form, data=as.data.frame(data))$coefficients))+2, nrow=(length(selection.levels)-1)) # +2 for Intercept, P, R
    rownames(c.matrix) <- c(1:nrow(c.matrix))
    
    for (i in 1:(length(selection.levels)-1)) {
      # if (i==(length(selection.levels)-1)) mean.data <- data[which(data[,selection.variable] > selection.levels[i]),] else
      # aber in der ganzen Funktion (selection.levels-1) durch selection.levels ersetzen.
      mean.data <- data[which(data[,selection.variable] > selection.levels[i] & data[,selection.variable] <= selection.levels[i+1]),]
      model <- lm(formula=form, data=as.data.frame(mean.data))
      
      s.list[[i]] <- summary(model)
      c.matrix[i,1:length(unname(model$coefficients))] <- unname(model$coefficients)
      c.matrix[i,length(unname(model$coefficients))+1] <- extract.regression.p(model)
      c.matrix[i,length(unname(model$coefficients))+2] <- summary(model)$r.squared
      #c.matrix[i,length(unname(model$coefficients))+3] <- if (extract.regression.p(model)<=0.001) {"***"} else if (extract.regression.p(model)<=0.01) {"**"} else if (extract.regression.p(model)<=0.05) {"*"} else if (extract.regression.p(model)<=0.1) {"."} else if (extract.regression.p(model)<=1) {" "}
      rownames(c.matrix)[i] <- names(s.list)[[i]] <-  paste(round(selection.levels[i], digits=1), "<", selection.variable, "<=", round(selection.levels[i+1],digits=1))
    }
    colnames(c.matrix) <- c(names(model$coefficients), "P", "R^2")
    result <- list(model=s.list,coefficients=c.matrix)
    return(result)
  }
  #regressionen.ln.abschnitte(form=formula(DABS_LN ~ DABS_JAE_FamAK+DABS_JAE_FremdAK+DABS_Kosten_ArbDritte+DABS_JAE_NE), data=aaa.alle, selection.variable="DABS_LN", selection.levels=c(1,2,3,4))
  #data <- aaa.alle
  #form=formula(DABS_LN ~ DABS_JAE_FamAK+DABS_JAE_FremdAK+DABS_Kosten_ArbDritte+DABS_JAE_NE)
  #selection.variable="DABS_LN"
  #selection.levels=c(1,2,3,4)
  
  
  ####
  clusterassignment.random.old.DELETE <- function(data, clustering) {
    nclust <- max(clustering)
    clustersizes <- matrix(0, nrow=nclust)
    for(i in 1:nclust)
      clustersizes[i] <- apply(data[clustering==i,1, drop=FALSE],2,length)
    clustersizes.column <- matrix(nrow=nrow(data), ncol=1)
    for (i in 1:nrow(data))
      clustersizes.column[i,] <- clustersizes[clustering[i]]
    data.npc <- cbind(data, clustersizes.column); colnames(data.npc)[ncol(data.npc)] <- "clustersize"
    rownames(data.npc) <- c(1:nrow(data.npc))
    
    cluster <- vector("list", nclust)
    for (i in 1:nclust){
      smpl <- sample(nrow(data.npc), clustersizes[i])
      cluster[[i]] <- data.npc[smpl,1:(ncol(data.npc)-1)]
      names(cluster)[[i]] <- paste("Cluster", i)
      data.npc <- data.npc[-smpl,]
    }
    if(nrow(data.npc)!=0) cat("random assignment didn't work properly")
    return(cluster)  
  }
  #data=aaa.alle; clustering=aaa.cut
  #zufaellig <- clusterassignment.random(data=aaa.alle, clusterin=aaa.cut)
  
  #scale.own.new <- function(data,range=1,center=NULL,rowcol=row){
  #  if(dim(data)==NULL) {
  #    range.old <- max(data)-min(data)# -1
  #    verh <- range/range.old
  #    data.new <- data*verh
  #    if(!is.null(center)) data.new <- data.new - (max(data.new)-range/2) + center
  #  }
  #  else {
  #    if(rowcol=row) {
  #      range.orig <- range
  #      range <- rep(range,nrow(data))
  #      range.old <- apply(data,1,function(x) max(data)-min(data))
  #      verh <- range/range.old
  #      data.new <- data*verh
  #      if(!is.null(center)) {
  #        minus <- apply(data.new,1,function(x) max(x)-range.orig/2 )
  #        data.new <- data.new - minus
  #        data.new <- data.new + center
  #      }
  #    }
  #    }
  #  return(data.new)
  #}
  ####
  
  ####
  highly.correlated.old.DELETE <- function(data,level=0.8,digits) {
    # Explanation: This function returns a table of variables which correlate more than the specified level.
    # use result= to specify if colnames or colnumbers are returned.
    corrs <- cor(data)
    diag(corrs) <- 0
    select.corrs <- row(corrs)-col(corrs)
    highly.corr <- which(abs(corrs[])>level&select.corrs>0)
    Variable1 <- Variable2 <- character(0)
    Correlation <- numeric(0)
    for(i in 1:length(highly.corr)) {
      Variable1[i] <- rownames(corrs)[highly.corr[i]-floor(highly.corr[i]/ncol(data)-10^(-10))*ncol(data)]
      Variable2[i] <- colnames(corrs)[1+floor(highly.corr[i]/ncol(data)-10^(-10))]
      Correlation[i] <- round(corrs[highly.corr[i]],digits) }
    if(length(Variable1)<1) {result <- NA
    } else  result <- data.frame(list(Variable1,Variable2,Correlation));  colnames(result) <- c("Variable 1","Variable 2","Correlation")
    return(result)
  }
  #data <- datas[spaltenauswahl];level<-0.9;digits<-2
  
  
  ###
  #validation.stats(aaa.mat, method="mclust", lower=2, upper=3, modelName="VEV") 
  #lower <- 2; upper <- 3; x <- aaa.mat; method="mclust"
  validation.stats.old.DELETE <- function(x=NULL, method=NULL, nclust=2:30, distance="euclidean", modelName=NULL, nstart=1000, outputfolder=NULL, filename=1:ncol(x), plot=TRUE, table.export=TRUE, rank.output=FALSE, count=FALSE){
    # Einbauen: eine andere Darstellung der Verringerung der within-cluster SS (erklaert in Bacher et al. 2010 Clusteranalyse: Anwendungsordientierte Einfuehrung.)
    # Idee: Pre = 1 - WSS(k clusters) / WSS( k-1 clusters)
    # somit sieht man die prozentuale verringerung der within cluster SS bei erhoehung der Clusterzahl.
    require(fpc)
    require(MASS)
    lower <- min(nclust); upper <- max(nclust)
    if(lower-upper==0) stop("You have to compare at least 2 different number of clusters (e.g. nclust=3:4)")
    if (method=="mclust") require(mclust)
    if (method=="mclust" & is.null(modelName)) cat("Unless you specify the exact model for mclust, the Mclust function will determine the best model for each number of clusters seperately (by using the BIC measure). If you decide to work with a certain number of clusters (after the validation.stats), check which model was used in validation.stats for the specific number of clusters by using the function Mclust(data, G=nclust). It will return the respective model. \nCalculating...\n")
    if (any(c(!is.logical(plot) , !is.logical(table.export) , !is.logical(rank.output) , !is.logical(count)))) stop("plot, table.export, rank.output and count must be logical (TRUE/FALSE)")
    if (!is.null(outputfolder)) {pdf(paste(outputfolder,"test.",method,".",lower,"-",upper,".",paste(colnames(x)[filename],collapse = "."),".pdf", sep=""), width=30, height=5); dev.off()
                                 unlink(paste(outputfolder,"test.",method,".",lower,"-",upper,".",paste(colnames(x)[filename],collapse = "."),".pdf", sep=""), recursive = FALSE, force = TRUE)}
    
    aaa.alle.df <- as.data.frame(x)
    original.matrix <- x
    distance.matrix <- dist(x, method=distance)
    performancemeasures <- c("average.within <", "average.between >", "wb.ratio <", "within.cluster.ss <", "avg.silwidth [-1,1] >", "pearsongamma [0,1] >", "ch dissimilarities >")
    
    i <- max(lower,2)             # i entspricht nclust in cutree!
    while (i <= upper)
    {
      aaa.cut <- if (method=="kmeans") kmeans(original.matrix, centers=i, nstart=nstart, iter.max=100)$cluster else if (method=="mclust") Mclust(original.matrix, G=i, modelNames= if (is.null(modelName)) Mclust(original.matrix)$modelName else modelName)$classification else cutree(hclust(distance.matrix, method=method),k=i)
      clstats <- cluster.stats( distance.matrix, aaa.cut)
      outputline <- c(i, clstats$average.within, clstats$average.between, clstats$wb.ratio, clstats$within.cluster.ss, clstats$avg.silwidth, clstats$pearsongamma, clstats$ch)
      
      ### Dem Dataframe die Spalte hinzufuegen, die fuer jeden Betrieb die Clusternummer angibt.
      aaa.alle.zugeteilt <- cbind(aaa.alle.df, aaa.cut)
      colnames(aaa.alle.zugeteilt)[ncol(aaa.alle.zugeteilt)] <- "Clusternummer"
      
      ### Diskriminanzanalyse
      aaa.spaltenauswahl.lda <- lda(aaa.alle.zugeteilt[,1:ncol(aaa.alle.zugeteilt)-1], aaa.alle.zugeteilt[,"Clusternummer"])
      aaa.spaltenauswahl.lda.predict <- predict(aaa.spaltenauswahl.lda, aaa.alle.zugeteilt[,1:ncol(aaa.alle.zugeteilt)-1])$class
      crosstable <- table(aaa.spaltenauswahl.lda.predict, aaa.alle.zugeteilt[,"Clusternummer"])
      summe <- 0
      crosstable.overall <- crosstable
      for (ab in 1:ncol(crosstable.overall))
      {crosstable.overall[ab,ab] <- 0
       summe <- summe + sum(crosstable.overall[ab,])}
      n.false <- summe
      prop.false <- n.false / nrow(aaa.alle.zugeteilt)
      
      outputmatrix <- round( matrix( c(outputline, prop.false), nrow=1), digits=3)
      rownames(outputmatrix) <- paste("k=",i,sep="")
      assign(paste("outputmatrix.n.",i,sep=""), outputmatrix)
      
      if (i!=2) { assign(  paste("outputmatrix.n.",i,sep="")   ,rbind( get(paste("outputmatrix.n.",(i-1),sep="")), get(paste("outputmatrix.n.",i,sep="")) )) }
      if (count) cat(i,"/",upper,"\n")
      i <- i+1
    }
    outputmatrix <- get(paste("outputmatrix.n.",i-1,sep=""))
    colnames(outputmatrix) <- c( "nclust", performancemeasures, "Fehlklassifizierungen <")
    
    ### Tabellenausgabe als 
    if (!is.null(outputfolder)) { if(table.export==TRUE){write.table(outputmatrix, file=paste(outputfolder,"table.",method,".",lower,"-",upper,".",paste(colnames(original.matrix)[filename],collapse = "."),".csv", sep=""), sep=";", dec=".", col.names=NA)} }
    
    ### Output als PDF
    if (!is.null(outputfolder)) {
      pdf(paste(outputfolder,"graph.",method,".",lower,"-",upper,".",paste(colnames(original.matrix)[filename],collapse = "."),".pdf", sep=""), width=30, height=5)
      #pdf(paste(outputfolder,"validation.stats.output",colnames(original.matrix),method,lower,"-",upper," (automatic).pdf", sep="", collapse = ""), width=30, height=5)
      # aus cluster.stat ()
      par(mfrow=c(1,length(performancemeasures)+1), mar=c(4,4,2,0.5))
      for (a in 1:length(performancemeasures))
      {
        plot( outputmatrix[,1], outputmatrix[,performancemeasures[a]], type="n", xlab=colnames(outputmatrix)[1], ylab=performancemeasures[a])
        lines( outputmatrix[,1], outputmatrix[,performancemeasures[a]], type="p")
        lines( outputmatrix[,1], outputmatrix[,performancemeasures[a]], type="l")
      }
      # Plot fuer Fehlklassifizierungen
      plot( outputmatrix[,1], outputmatrix[,ncol(outputmatrix)], type="n", xlab=colnames(outputmatrix)[1], ylab="Fehlklassifizierungen <")
      lines( outputmatrix[,1], outputmatrix[,ncol(outputmatrix)], type="p")
      lines( outputmatrix[,1], outputmatrix[,ncol(outputmatrix)], type="l")
      title(main=method)
      dev.off()
    }
    
    ### Plots
    # aus cluster.stat ()
    if(plot==TRUE){
      dev.new(width=30, height=5)
      par(mfrow=c(1,length(performancemeasures)+1), mar=c(4,4,2,0.5))
      for (a in 1:length(performancemeasures))
      {
        plot( outputmatrix[,1], outputmatrix[,performancemeasures[a]], type="n", xlab=colnames(outputmatrix)[1], ylab=performancemeasures[a])
        lines( outputmatrix[,1], outputmatrix[,performancemeasures[a]], type="p")
        lines( outputmatrix[,1], outputmatrix[,performancemeasures[a]], type="l")
      }
      # Plot fuer Fehlklassifizierungen
      plot( outputmatrix[,1], outputmatrix[,ncol(outputmatrix)], type="n", xlab=colnames(outputmatrix)[1], ylab="Fehlklassifizierungen <")
      lines( outputmatrix[,1], outputmatrix[,ncol(outputmatrix)], type="p")
      lines( outputmatrix[,1], outputmatrix[,ncol(outputmatrix)], type="l")
      title(main=method)
    }
    
    ### Rangklassifizierungen
    #klass.vektor <-  c("wb.ratio <", "within.cluster.ss <", "avg.silwidth [-1,1] >", "pearsongamma [0,1] >", "ch dissimilarities >", "Fehlklassifizierungen <")
    klass.vektor <-  c(performancemeasures[5:7], "Fehlklassifizierungen <")
    rang.matrix <- outputmatrix[ , c("nclust",klass.vektor)]
    rang.matrix[,"avg.silwidth [-1,1] >"]  <- rang.matrix[,"avg.silwidth [-1,1] >"] * (-1)
    rang.matrix[,"ch dissimilarities >"]  <- rang.matrix[,"ch dissimilarities >"] * (-1)
    rang.matrix[,"pearsongamma [0,1] >"]  <- rang.matrix[,"pearsongamma [0,1] >"] * (-1)
    
    for (i in 1:length(klass.vektor))
    { rang.matrix <- rang.matrix[order (rang.matrix[,klass.vektor[i]]) , ]
      assign( paste("rang.vektor", i,sep=""), 1:nrow(outputmatrix) )
      rang.matrix <- cbind(rang.matrix, get(paste("rang.vektor", i,sep="")))
      #colnames(rang.matrix)[ncol(rang.matrix)] <- paste("k",i,sep="")
      colnames(rang.matrix)[ncol(rang.matrix)] <- paste("Rang",klass.vektor[i],sep=".")
    }
    
    rang.mittel <- rep(0, nrow(rang.matrix))
    for (i in 1:nrow(rang.matrix))
    { rang.mittel[i] <- sum( rang.matrix[ i , (ncol(rang.matrix)-length(klass.vektor)+1):ncol(rang.matrix)] )   /   length(klass.vektor) }
    rang.matrix <- cbind(rang.matrix, rang.mittel)
    colnames(rang.matrix)[ncol(rang.matrix)] <- "rang.mittel"
    rang.matrix <- rang.matrix[order (rang.matrix[,"rang.mittel"]) , ]
    rang.matrix <- rang.matrix[ , c(1,(ncol(rang.matrix)-length(klass.vektor)):ncol(rang.matrix))]
    #rang.matrix.info <- paste( c("k[1,2,3,4] means", klass.vektor), collapse = " , " )
    #message <- "Ergebnisse unter validation.stats.output$ abrufbar."
    
    ### Tabellenausgabe als 
    if (!is.null(outputfolder)) { if(rank.output==TRUE){write.table(rang.matrix, file=paste(outputfolder,"rank.",method,".",lower,"-",upper,".",paste(colnames(original.matrix)[filename],collapse = "."),".csv", sep=""), sep=";", dec=".", col.names=NA)} }
    
    result <- list(table = outputmatrix, ranks=rang.matrix)
    return(result)
  }
  
  #### "See also gruppieren nach merkmal"
  means.by.interval.DELETE <- function(variables,data,selection.variable,selection.levels){
    data <- data[,c(selection.variable,variables)]
    c.matrix <- matrix(ncol=length(variables)+1, nrow=(length(selection.levels)-1))
    group.sizes <- rep(NA, (length(selection.levels)-1))
    rownames.vector <- rep(NA,(length(selection.levels)-1))
    
    for (i in 1:(length(selection.levels)-1)) {
      # if (i==(length(selection.levels)-1)) mean.data <- data[which(data[,selection.variable] > selection.levels[i]),] else
      # aber in der ganzen Funktion (selection.levels-1) durch selection.levels ersetzen.
      mean.data <- data[which(data[,selection.variable] > selection.levels[i] & data[,selection.variable] <= selection.levels[i+1]),]
      c.matrix[i,] <- apply(mean.data,2,mean)
      group.sizes[i] <- nrow(mean.data)
      rownames.vector[i] <- paste(round(selection.levels[i], digits=1), "<", selection.variable, "<=", round(selection.levels[i+1],digits=1))
    }
    c.matrix <- cbind(group.sizes, c.matrix)
    rownames(c.matrix) <- rownames.vector
    colnames(c.matrix) <- c("N",selection.variable,variables)
    return(c.matrix)
  }
  #testdata <- cbind(c(1,1,1,1,2,2,2,2,3,3,3,3,4,4,4,4,5,5,5,5),c(1,1,1,1,2,2,2,2,3,3,3,3,4,4,4,4,5,5,5,5)); colnames(testdata) <- c("selection","values")
  #means.ln.abschnitte("values",testdata,"selection",c(0.5,1.5,2.5,3.5,4.5))
  
  #### Simple Version for 1 variable
  density.overlap.DELETE <- function(data,grouping,cols=NULL,bw="nrd0",xlab="",...) {
    # Plot several densities in one plot
    if(is.null(cols)) cols <- c("red","darkgreen","blue","cyan","green","gray87","yellowgreen","steelblue1","orchid1","purple","orange","yellow")
    datas <- dens <-densx <- densy <- list()
    grp <- sort(unique(grouping))
    
    for(i in grp) {
      datas[[i]] <- data[grouping==i]
      de <- density(datas[[i]],bw=bw)
      dens[[i]] <- de
      densx[[i]] <- de$x
      densy[[i]] <- de$y
    }
    
    xlim <- ylim <- numeric()
    xlim[1] <- min(calldensx <- do.call("c",densx))
    xlim[2] <- max(calldensx)
    ylim[1] <- min(calldensy <- do.call("c",densy))
    ylim[2] <- max(calldensy)
    
    plot(dens[[grp[1]]],xlim=xlim,ylim=ylim,col=cols[grp[1]],xlab=xlab,...)#,main="Dens. Plot",xlab="Efficiency");
    for(i in grp[2:length(grp)]) lines(dens[[i]],col=cols[i])
  }
  
  #### without splitting possibility
  histogram.multi.old.DELETE <- function(data,grouping,plotrows=3,mar=c(2,2.6,2,0.4),bins=10,cols=NULL,transparency=1/4,main=NULL,xlab="") {
    if(is.null(cols)) cols <- c("red","darkgreen","blue","cyan","green","gray87","yellowgreen","steelblue1","orchid1","purple","orange","yellow")
    colsrgb <- col2rgb(cols, alpha = 1)
    colsrgb <- colsrgb/255
    colsrgb[4,] <- transparency
    
    if(is.null(dim(data))) data <- matrix(data,ncol=1)
    if(ncol(data)==1) {
      plotrows <- 1
      if(all(mar%in%c(2,2.6,2,0.4))) mar <- par()$mar
    }
    
    if(is.null(main)) main <- colnames(data) else if(length(main)==1) main <- rep(main,ncol(data))
    if(length(xlab)==1) xlab <- rep(xlab,ncol(data))
    grp <- sort(unique(grouping))
    mar.orig <- par()$mar
    mfrow.orig <- par()$mfrow
    
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
      
      binwidth <- h[[1]]$mids[2] - h[[1]]$mids[1]
      xlim <- c(min(mids.call<-do.call("c",mids))-binwidth, max(mids.call)+binwidth)
      ylim <- c(min(counts.call<-do.call("c",counts)), max(counts.call))
      
      col <- rgb(colsrgb[1,grp[1]],colsrgb[2,grp[1]],colsrgb[3,grp[1]],alpha=colsrgb[4,grp[1]])
      plot(h[[grp[1]]], col=col, xlim=xlim, ylim=ylim, main=main[i],xlab=xlab[i])
      for(j in grp[2:length(grp)]){
        col <- rgb(colsrgb[1,j],colsrgb[2,j],colsrgb[3,j],alpha=colsrgb[4,j])
        plot(h[[j]], col=col, xlim=xlim, ylim=ylim, add=TRUE)
      }
    }
    par(mfrow=mfrow.orig,mar=mar.orig)
  }
  #### without splitting possibility
  density.multi.old.DELETE <- function(data,grouping,bw="nrd0",na.rm=TRUE,cols=NULL,xlab="",main=NULL,mar=c(2,2.6,2,0.4),plotrows=3,...) {
    # Plot several densities in one plot
    if(is.null(cols)) cols <- c("red","darkgreen","blue","cyan","green","gray87","yellowgreen","steelblue1","orchid1","purple","orange","yellow")
    grp <- sort(unique(grouping))
    if(is.null(dim(data))) data <- matrix(data,ncol=1)
    if(ncol(data)==1) {
      plotrows <- 1
      if(all(mar%in%c(2,2.6,2,0.4))) mar <- par()$mar
    }
    if(is.null(main)) main <- colnames(data) else if(length(main)==1) main <- rep(main,ncol(data))
    if(length(xlab)==1) xlab <- rep(xlab,ncol(data))
    nvariables <- ncol(data)
    
    mar.orig <- par()$mar
    mfrow.orig <- par()$mfrow
    par(mar=mar, mfrow=c(plotrows,if(nvariables%%plotrows==0) nvariables/plotrows else ceiling(nvariables/plotrows) ))
    
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
      
      plot(dens[[grp[1]]],xlim=xlim,ylim=ylim,col=cols[grp[1]],xlab=xlab[i],main=main[i],...)#,main="Dens. Plot",xlab="Efficiency");
      for(j in grp[2:length(grp)]) lines(dens[[j]],col=cols[j])
    }
    par(mar=mar.orig,mfrow=mfrow.orig)
  }
  
  ###
  mean.weight.DELETE <- function(data,weights=NULL,index=NULL,digits=3,round=c("signif.equally","round","signif"),na.rm=TRUE, ...){
    round <- match.arg(round)
    is.null.dim.data <- is.null(dim(data))
    is.null.index <- is.null(index)
    is.null.weights <- is.null(weights)
    
    if(is.null.dim.data){
      if(is.null.weights) weights <- rep(1,length(data))
      if(is.null.index) index <- rep(1,length(data))
      data.w <- data*weights
      data.sums <- tapply(data.w,index,function(x)sum(x,na.rm=TRUE))
      weight.sums <- tapply(weights,index,function(x)sum(x,na.rm=TRUE))
      means <- data.sums / weight.sums
      if(is.null.index) means <- unname(means)
    } else {
      if(is.null.weights) weights <- rep(1,nrow(data))
      if(is.null.index) index <- rep(1,nrow(data))
      data.w <- data*weights
      data.sums <- as.data.frame(apply(data.w,2,function(x)tapply(x,index,function(x)sum(x,na.rm=TRUE))))
      weight.sums <- tapply(weights,index,function(x)sum(x,na.rm=TRUE))
      means <- data.sums / weight.sums
      if(is.null.index) {
        means <- t(means)
        rownames(means) <- "mean"
      }
    }
    
    if(!is.null(digits)) if(round=="round") {
      means <- round(means,digits)
    } else if(round=="signif") {
      means <- signif(means,digits)
    } else {
      #if(is.null.dim.data){
      means <- signif.equally(means,digits, ...)
      #} else {
      #  means <- apply(means,2,function(x)signif.equally(x,digits, ...))
      #}
    }
    
    return(means)
  }
}

cat("**********************************************************************\nFunctions loaded\n**********************************************************************\n")
