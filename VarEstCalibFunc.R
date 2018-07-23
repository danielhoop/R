# ***************************************
# Title:   Functions to estimate the variance (imprecision) of estimators based on random samples
# Authors: Daniel Hoop, Swetlana Renner
# Version: 2018-07-21
# ***************************************

# Description of package
# **********************
# The main function to use is variance.estimate()
# It provides a high level interface to access all functionalities.
# If you want to operate on the level of single vectors, you can also use varEstZaBh(...) or varEstDelta(...). But It's not so convenient and not all possible errors are catched.
# As preparation for calibration (with the function sampling::calib(...)) and subsequent variance estimation you can use the function calcXsMultiLevel(...).

# Loading the package
# *******************
# The functions depend on other functions you will also need to source from a specific URL.
# In your scripts, do it like this:
# if(!exists("mean.weight")) source("https://raw.githubusercontent.com/danielhoop/R/master/func.R")
# if(!exists("variance.estimate")) loadVarEstCalibFunc()
# Then you're done. All functions loaded.


#### Public functions ####
variance.estimate <- function(data, cols=NULL, weights, inclusProbs, index=NULL, indexStrata=NULL,
                              fixedIndex=FALSE, indexOfFixedResult=NULL, indexSep="_", 
                              method=c("ht","calib"), figure=c("var","SE","halfLengthCI", "pValue","cor"), relativeToMean=FALSE, CIprob=NULL, CImultiplier=NULL, DFestimator=c("simple","satterthwaite"),
                              deltaBetweenYears=FALSE, id=NULL, year=NULL, year0=NULL, year1=NULL, weightsBalanced=NULL, inclusProbsBalanced=NULL,
                              Xs=NULL, Nmin=0, NminBalNonZero=0, calcDeltaForNonZeroOnly=FALSE,
                              na.rm=TRUE, edit.I.colnames=FALSE) {
  # This function calculates the variance, standard error or confidence invertals for estimated mean values.
  #
  # Arguments
  # data =            the data.frame/matrix that contains the variables of interest
  # weights =         the vector that contains the weights
  # inclusProbs =     the vector that contains the inclusion probabilities
  # index =           the vector that contains the index for the aggregation level to be estimated. E.g. something like "region". If NULL, the calculation is done for the whole data.frame.
  # indexStrata =     the vector that contains the statification according to the sample design. This can be different from the index.
  # fixedIndex, indexOfFixedResult, indexSep
  #                   special arguments if a fixed index should be contained in the result. E.g. if only region 1 and region 2 occur in the sample, but also region 3 should be displayed as NA.
  #                   in this case you could specify: index=region, fixedIndex=TRUE, indexOfFixedResult=c("1","2","3"). Or if you want to display all permutations of region and type, also if they don't occur in the sample.
  #                   then specify something like fixedIndex=TRUE, index=list(region, type)
  # method =          "ht" for Horvith-Thompson method. "calib" for the calibration method using varest{sampling}
  # figure =          the figure to be calculated. var=variance, stErr=standard error, CI=confidence interval
  # relativeToMean =  if TRUE, then the calculated variance/SE/CI will be divided by the weighted mean. Use colnames like "I(a/b)" for ratio of mean figures. See also function mean.weight().
  # CIprob =          the relative accuracy of the one-sided confidence interval (CI). The default value 0.975 will yield CImultiplier =~ 1.96 for a large sample (student distribution). 0.975 means 97.5% certainty for one side i.e. 95% certainty, for both sides.
  # CImultiplier =    the factor to calculate the confidence interval (CI). The default value 1.96 is equivalent to CIprob=0.975, i.e. 95% CI in a large sample (student distribution).
  # DFestimator =     the estimator to calculate the degrees of freedom if is.null(CImultiplier). CImultiplier is calculated from CIprob with assumed t-distribution. "simple" will use  df=n-(number of strata). "satterthwaite" will use the satterthwaite approximation
  # deltaBetweenYears if TRUE, then the variation/significance/... between two years given in year0 and year1 is calculated.
  # special arguments for the case deltaBetweenYears==TRUE (otherwise leave NULL).
  #   id =            the vector of unique identifiers for each observation in data.
  #   year =          the vector containing the year for each observation in data.
  #   year0, year1 =  the reference year (year0) and the subsequent year (year1) for with the calculations should be done.
  #   weightsBalanced = the vector that contains the balanced weights
  #   inclusProbsBalanced = the vector that contains the balanced inclusion probabilities
  # Xs =              the Xs matrix that was used to calibrate the weights with the function sampling::calib(). Use the function calcXsMultiLevel() to calculate Xs.
  # Nmin =            the minimum number of observation required for the variance estimation. If the number of observations is smaller than Nmin, then NA will be returned.
  # NminBalNonZero =     The minimum number of non zero observations required for the variance estimation of yearly changes. Consider sparce vector with few values other than 0.
  #                   It is kind of a mixtrue between normal and binomial distribution. In that case, the formulas for variacne estimation dont work.
  #                   NminBalNonZero=20 will only yield a non-NA value if there are at least 20 non-zero observations in each year.
  # calcDeltaForNonZeroOnly = Logical indicating if only these observations should be included into the calculation of the significant difference, which have a value other than 0
  #                   In one of year0 or year1. If the value is 0 in both years, then the observation will not be included for the calculations.
  # na.rm =           if TRUE, then for each column of data the NA values will be excluded from the calculation.
  # edit.I.colnames = if TRUE, then colnames like "I(a/b)" will be edited as "a/b" for the result. This is in accordance to the mean.weight() function.
  
  figure <- match.arg(figure)
  method <- match.arg(method)
  DFestimator <- match.arg(DFestimator)
  
  # Format data
  if (is.list(data) && !is.data.frame(data)) stop ("data must be matrix or data.frame but not a list.")
  isNullDimData <- is.null(dim(data))
  if (isNullDimData) {
    data <- as.data.frame(data, stringsAsFactors=FALSE)
  } else if (!is.data.frame(data)) {
    namesOrig <- colnames(data)
    data <- as.data.frame(data, stringsAsFactors=FALSE)
    colnames(data) <- namesOrig; rm(namesOrig)
  }
  
  # Check length of all vectors
  if (length(inclusProbs)!=nrow(data)) stop ("length(inclusProbs) must be equal nrow(data)")
  if (length(weights)!=nrow(data)) stop ("length(weights) must be equal nrow(data)")
  if (!is.null(index)) {
    if (!is.list(index)) {
      stopifnot(length(index) == nrow(data))
    } else {
      lapply(index, function(x)if (length(x)!=nrow(data)) stop ("If index is a list, for all list entries must hold: length(index[[?]])==nrow(data)"))
      index <- .paste.elements(index, errorMsg="All list places in index must have same length!")
    }
  }
  
  # Assure no NA weights and inclusProbs.
  if (any(is.na(weights))) stop ("There must be no NA values in weights.")
  if (any(is.na(inclusProbs))) stop ("There must be no NA values in inclusProbs")
  
  # CI preparations
  if (figure=="halfLengthCI") {
    if ( is.null(CImultiplier) &&  is.null(CIprob)) stop ("Either CImultiplier or CIprob must be specified. E.g. CImultiplier=1.96 or CIprob=0.975")
    if (!is.null(CImultiplier) && !is.null(CIprob)) stop ("Either specifiy CImultiplier or CIprob but not both.")
    #if (!is.null(CImultiplier) && !is.null(index)) warning("If index is given, and some strata are small, the CImultiplier should be calculated for each iteration separately based on CIprob. Thus, CIprob should be specified, not CImultiplier.")
    if (!is.null(CIprob) && is.null(indexStrata)) stop ("If CIprob is given, then indexStrata must be specified, such that the degrees of freedom can be calculated. If it isn't a stratified sample, then use indexStrata=rep(1,nrow(data)).")
    if (!is.null(CIprob))
      if (CIprob < 0 || CIprob > 1) stop ("CIprob must lie between 0 and 1. For both-sided 95% confidence, i.e. one-sided 97.5% confidence, choose 0.975.")
  }
  calcCImultiplierFlag <- figure=="halfLengthCI" && !is.null(CIprob)
  
  # yearDiff preparations
  if (deltaBetweenYears) {
    if (is.null(year0) || length(year0)!=1) stop ("If deltaBetweenYears==TRUE, then year0 must be a numeric vector of length 1.")
    if (is.null(year1) || length(year1)!=1) stop ("If deltaBetweenYears==TRUE, then year1 must be a numeric vector of length 1.")
    if (length(id)!=nrow(data)) stop ("If deltaBetweenYears==TRUE, then length(id) must be equal nrow(data).")
    if (length(year)!=nrow(data)) stop ("If deltaBetweenYears==TRUE, then length(year) must be equal nrow(data).")
    if (length(weightsBalanced)!=nrow(data)) stop ("If deltaBetweenYears==TRUE, then length(weightsBalanced) must be equal nrow(data).")
    if (length(inclusProbsBalanced)!=nrow(data)) stop ("If deltaBetweenYears==TRUE, then length(inclusProbsBalanced) must be equal nrow(data).")
    if (!figure%in%c("SE", "pValue", "cor")) stop ('If deltaBetweenYears==TRUE, then figure must be one of "SE", "pValue", "cor".')
    if (Nmin != 0 && NminBalNonZero != 0 && Nmin < NminBalNonZero) stop ("Setting Nmin < NminBalNonZero doest not make sense.")
  }
  
  # Other preparations
  if (figure%in%c("pValue","cor")) {
    if (relativeToMean) stop ("Invalid settings: figure=='pValue' and relativeToMean==TRUE. This does not make sense.")
    if (!deltaBetweenYears) stop ("Invalid settings: figure=='pValue' does only make sense for deltaBetweenYears==TRUE.")
  }
  
  # Wenn kein Aggregationslevel angegeben wurde, dann einen kuenstlichen erzeugen
  isNullIndex <- is.null(index)
  if (isNullIndex) {
    index <- rep("", nrow(data))
  } else {
    if (any(is.na(index))) stop ("There must be no NA values in index")
  }
  # Fixed index ausschalten, wenn Index ein Vektor ist. Dann bringt es nichts.
  if (fixedIndex && is.null(indexOfFixedResult) && !is.list(index)) stop ("fixedIndex & is.null(indexOfFixedResult) & !is.list(index)   -> fixedIndex doesn't have any effect this way. Give index as a list!")
  # Im Falle, dass der index fixiert sein soll, hier die rohe Ergebnisstruktur erstellen.
  if (fixedIndex) {
    rawResult <- .prepare.fixed.index.result(data=data, index=index, names.result=indexOfFixedResult, index.sep=indexSep)
    index <- .paste.elements(index, sep=indexSep, errorMsg="All indices must have same length!")
  }
  
  # Warnen, wenn kleine Stichprobe ausgewertet wird, aber mit fixem CImultiplier gearbeitet wird.
  if (!is.null(CImultiplier)) {
    tab <- table(index)
    if (any(tab<100)) warning(paste0("There are small strata with less than 500 observations. You specified a CImultiplier of ", CImultiplier,
                                    ". However, you should consider specifying the argument CIprob instdeat of CImultiplier, because it will yield more accurate results for small strata."))
  }
  
  if (method=="calib" && (is.null(dim(Xs)) || nrow(data)!=nrow(Xs))) stop ("nrow(data) must be equal nrow(Xs)")
  if (method=="ht" && !is.null(Xs)) warning("Specifying Xs with method='ht' does not have an effect. Consider specifying method='calib'.")
  
  # Indices (0,1) fuer die verschiedenen Aggregationslevel machen.
  levelBin <- apply(categ.to.bin(index, varname="var", sep="_"), 2, function(x)as.logical(x) )
  
  # Extract only the cols that were chosen
  if (!is.null(cols)) {
    cols_add <- extract.I.vars(cols, keep.only.necessary=TRUE)
    cols_mis <- cols_add[!cols_add%in%colnames(data)]
    if (length(cols_mis)>0)
      stop (paste0("Some columns used in formulas like 'I(a/b)' are missing in data: ", paste0(cols_mis,sep=", ")))
    data <- create.cols(data, cols)
    cols_all <- c(cols, cols_add)
    data <- data[,cols_all,drop=FALSE]
  } else {
    cols <- colnames(data)
    cols_all <- colnames(data)
  }

  # Indices der Aggregationslevel mit den zugehoerigen Variablen multiplizieren.
  # Davon die Varianz-Total-Population berechnen
  var0 <- NULL
  # This is necessary because new columns will be created.
  cnData <- colnames(data)
  # Loop over all binary levels (indices)
  for(i1 in 1:ncol(levelBin)) { # i1 <- 1
    # Wenn weniger als 2 Betriebe, dann NaN zurueckgeben
    if (sum(levelBin[,i1]) < 2) {
      var1 <- rep(NA_real_, length(cnData))
      names(var1) <- cnData
      # Berechnung im fall dass mind. 2 Betriebe
    } else {
      
      # Calculate variance...
      if (deltaBetweenYears) {
        filt <- levelBin[,i1]
        var1 <- varEstDelta(cbind(data[,cnData,drop=FALSE], "pikCol_xc87h23"=inclusProbs, "pikBalCol_xc87h23"=inclusProbsBalanced, "wCol_xc87h23"=weights, "wBalCol_xc87h23"=weightsBalanced, "idCol_xc87h23"=id, "yearCol_xc87h23"=year)[filt,,drop=FALSE],
                            pikCol="pikCol_xc87h23", pikBalCol="pikBalCol_xc87h23", wCol="wCol_xc87h23", wBalCol="wBalCol_xc87h23", idCol="idCol_xc87h23", yearCol="yearCol_xc87h23",
                            year0=year0, year1=year1, Ycols=cnData, Xs=if (method=="calib") Xs[filt,,drop=FALSE] else NULL, Nmin=Nmin, NminBalNonZero=NminBalNonZero, calcDeltaForNonZeroOnly=calcDeltaForNonZeroOnly, na.rm=na.rm)
        if (is.null(dim(var1)))
          var1 <- as.matrix(var1)
        var1 <- if (figure=="SE") var1["se",,drop=FALSE] else if (figure=="pValue") var1["p.value",,drop=FALSE] else if (figure=="cor") var1["cor",,drop=FALSE]
        
      } else {
        # Warn if there are non-infinite values in w or inclusion probabilities
        if (any(!is.finite(weights)) || any(!is.finite(inclusProbs)))
          warning("There are non-finite weights/inclusion-probabilities in the data which will result in NaN values for precision estimates.")
          
        # Calculate CI factor, simple case
        if (calcCImultiplierFlag && DFestimator=="simple")
          CImultiplier <- .estStudentTmultiplier(prob=CIprob, indStr=indexStrata[levelBin[,i1]], y=NULL, w=NULL, simple=TRUE, na.rm=na.rm)
        
        # Now loop over all columns
        var1 <- numeric(length(cnData))
        names(var1) <- cnData
        mean1 <- var1
        for(col in cnData) {
          # Prepare the result that the column has no outer operator like (a/b)*100, so that this information can be accessed, even if the colname was no formula.
          op <- list("hasOuter"=FALSE)
          # Case I() column
          if (startsWith(col,"I(") && endsWith(col,")") && grepl("/",col)) {
            col1 <- substr(col,3,nchar(col)-1) # Remove I()
            op <- .getOperands(col1, innerOperator="/", outerOperator="*")
            if (!op$hasInner)
              stop (paste0("A formula contains a quitient but parsing does not work. Check the syntax of your expression. Combined with division, only a multiplication by a fixed scalar is allowed.",
                           "OK: '(a/b)*100'. NOT OK: '(a/b)*c' or '(a/b)+100'.  Errorneous column: ", paste0(col1, collapse=", ")))
            divCols <- op$innerVal
            if (length(divCols) != 2)
              stop (paste0("In quotients like 'I(x/y)' only two columns are allowed. Errorneous expression: ", paste0(col1, collapse=", ")))
            divColsNotAvail <- divCols[!divCols%in%colnames(data)]
            if (length(divColsNotAvail)>0)
              tryCatch({
                for (calcDivCol in divColsNotAvail) {
                  data[,calcDivCol] <- with(data, eval(parse(text=calcDivCol)))
                }
              }, error=function(e){
                stop (paste0("Some columns specified in quotiens like 'I(x/y)' are not available in data or the calculation syntax is erroneous: ",paste0(calcDivCol, collapse=", "))) # divColsNotAvail
              })
            Yc <- data[,divCols[1]]
            Yd <- data[,divCols[2]]
            filt <- if (na.rm) which(levelBin[,i1] & !is.na(Yc) &!is.na(Yd)) else which(levelBin[,i1])
            # Case not I() column
          } else {
            Yc <- data[,col]
            Yd <- NULL
            filt <- if (na.rm) which(levelBin[,i1] & !is.na(Yc)) else which(levelBin[,i1])
          }
          if (length(filt) < 2) {
            var1[col] <- mean1[col] <- NA
          } else {
            # Calculate variance
            rawVar <- varEstZaBh(Yc=Yc[filt], Yd=Yd[filt], Xs=if (method=="calib") Xs[filt,,drop=FALSE] else NULL, pik=inclusProbs[filt], w=weights[filt], calcMean=relativeToMean, Nmin=Nmin)
            # If there was a multiplication by a fix factor in the formula, apply it now.
            # E.g. I(a/b*100)
            if (op$hasOuter) {
              #warning("resw, meinst du, man kann hier einfach A und e mit einem Faktor multiplizieren? Anhand der Berechnungsformeln denke ich, schon. 2018-07-23")
              if (op$outerAtStart) {
                rawVar$v <- eval(parse(text=paste0(op$outerVal, op$outerOp, "rawVar$v")))
                rawVar$A <- eval(parse(text=paste0(op$outerVal, op$outerOp, "rawVar$A")))
                rawVar$e <- eval(parse(text=paste0(op$outerVal, op$outerOp, "rawVar$e")))
              } else {
                rawVar$v <- eval(parse(text=paste0(                         "rawVar$v", op$outerOp, op$outerVal)))
                rawVar$A <- eval(parse(text=paste0(                         "rawVar$A", op$outerOp, op$outerVal)))
                rawVar$e <- eval(parse(text=paste0(                         "rawVar$e", op$outerOp, op$outerVal)))
              }
            }
            # Calculate CI factor, with satterthwaite
            if (calcCImultiplierFlag && DFestimator=="satterthwaite")
              CImultiplier <- .estStudentTmultiplier(prob=CIprob, indStr=indexStrata[filt], y=data[filt,col], w=weights[filt], simple=FALSE, na.rm=na.rm)
            # Further process variance and return
            var1[col] <- .calcVarStErrOrCI(rawVar=rawVar$var, w=weights[filt], figure=figure, CImultiplier=CImultiplier)
            mean1[col] <- rawVar$mean
          }
        }
        # Calculate the variance relative to mean. Absoulte value, in case the mean is negative.
        # Info: mean1 from varEstZaBh is ratio of means: mean(Yc)/mean(Yd)
        if (relativeToMean) var1 <- abs(var1/mean1)
      }
    }
    #
    var0 <- rbind(var0, var1)
  }

  # Schlussformatierung
  if (is.null(dim(var0))) {
    var0 <- t(as.matrix(var0))
  }
  rownames(var0) <- gsub("var_","",colnames(levelBin))
  
  # Ergebnis in fixierte Ergebnisstrutkur einfuegen.
  if (fixedIndex) {
    var0 <- var0[rownames(var0)%in%rownames(rawResult),,drop=FALSE]
    rawResult[match(rownames(var0),rownames(rawResult)),] <- var0
    var0 <- rawResult
  }

  # Urspruengliche Namen wiederherstellen. Diese wurden durch data.frame zerstoert.
  if (edit.I.colnames) {
    cols_all <- .rm.I.from.names(cols_all)
    cols <- .rm.I.from.names(cols)
  }
  if (isNullIndex && !fixedIndex)
    rownames(var0) <- NULL

  # Schlussformatierung und return
  if (isNullDimData) {
    rn1 <- rownames(var0)
    var0 <- var0[,1]
    names(var0) <- rn1
    return (var0)
  } else {
    colnames(var0) <- cols_all
    return (var0[,cols])
  }
}

# .v1arEstZaBh(Yc=spe[spe[,"JAHR"]==2016, "LE"], w=spe[spe[,"JAHR"]==2016,"GEWICHT_PS"], Xs=NULL)
# .v1arEstZaBh(Yc=spe[spe[,"JAHR"]==2016, "AV"],Yd=spe[spe[,"JAHR"]==2016, "FJAE"],w<-spe[spe[,"JAHR"]==2016,"GEWICHT_PS"], Xs=NULL)
# .v1arEstZaBh(Yc=spe[spe[,"JAHR"]==2016, "LE"],w=spe[spe[,"JAHR"]==2016,"GEWICHT"], Xs=Xs)
# .v1arEstZaBh(Yc=spe[spe[,"JAHR"]==2016, "AV"], Yd=spe[spe[,"JAHR"]==2016, "FJAE"],w=spe[spe[,"JAHR"]==2016,"GEWICHT"], Xs=Xs)

varEstZaBh <- function(Yc, Yd=NULL, Xs=NULL, pik, w=NULL, calcMean=FALSE, Nmin=0) {
  # This function is an extention of the function sampling::varest(). It depends on the function MASS::ginv().
  # It can be used to calculate mean using HT and Calib approach as well es the corresponding ration of means (quotient)
  # It is used inside the variance.estimate() and .varEstDelta function.
  #
  # Arguments
  # Yc:   Numeric vector giving the values of interest for which the variance should be caluclated. In case the variance of a quotent should be calculated: Yc (c for counter) is the counter.
  # Yd:   In case the variance of a quotient should be calculated: Yd (d for divisor) contains the values of the divisor.
  # Xs:   The calibration matrix (numeric).
  # pik:  The inclusion probabilities (numeric).
  # w:    The weights. For poststratification the condition pik=1/w usually holds. For calibration, however, this might not be the case.
  # Nmin: The minimum number of observation required for the variance estimation. If the number of observations is smaller than Nmin, then NA will be returned.
  # Value
  # A list containing the variance (var), the mean (mean), the residuals (e) and A.
  
  if (any(is.na(pik))) stop ("There must be no missing values in pik.")
  if (any(is.na(Yc))) stop ("There must be no missing values in Yc.")
  if (calcMean && length(Yc) != length(w)) stop ("length(Yc) must be euqla ot length(w).")
  if (!is.null(Xs)) {
    if (is.data.frame(Xs))
      Xs <- as.matrix(Xs)
    if (is.vector(Xs)){
      if (length(Yc) != length(Xs)) stop ("length(Yc) must be equal to length(Xs).")
      if (length(w)  != length(Xs)) stop ("length(w) must be equal to length(Xs).")
    }
    if (is.matrix(Xs)) {
      if (length(Yc) != nrow(Xs)) stop ("length(Yc) must be equal to nrow(Xs).")
      if (length(w)  != nrow(Xs)) stop ("length(w) must be equal to nrow(Xs).")
    }
  }
  # If the number of observations is too small, then return NA values.
  if (length(Yc)<Nmin)
    return (list(var=NA, A=NA, e=rep(NA_real_, length(Yc)), mean=NA))
  
  #pik=1/w # inclusion probabilities
  if (is.null(Yd)) { # Mean or ration of means
    m <- if (calcMean) weighted.mean(Yc,w) else NA # Mean can only be calculated if w is given.
    z <- Yc 
    N <- round(sum(1/pik)) # Population size (used in variance formula of mean)
  } else {
    m <- if (calcMean) weighted.mean(Yc,w)/weighted.mean(Yd,w) else NA # Mean can only be calculated if w is given.
    Ychat <- sum(Yc/pik)
    Ydhat <- sum(Yd/pik)
    r <- Ychat/Ydhat 
    z <- (Yc - r * Yd)/Ydhat 
    N <- 1
  }
  
  if (is.null (Xs)) {
    e <- z
  } else {
    B <- t(Xs*w)
    beta <- MASS::ginv(B%*%Xs)%*%B%*%z
    e <- unname((z-Xs%*%beta)[,1]) # Result is a matrix. Convert to vector.
  }
  
  a <-(1-pik)/sum(1-pik)
  A <- sum(a*e/pik)
  v <- sum((1-pik)*(e/pik-A)^2)/(1-sum(a^2)) / N^2
  
  return (list(var=v, A=A, e=e, mean=m))
}

# dat <- data.frame(a=1:10, b=10:1, "I(a/b)"=NA); colnames(dat)[3] <- col <- "I(a/b)"
varEstDelta <- function(dat, Ycols=NULL, YcCol=NULL, YdCol=NULL, Xs=NULL, pikCol, pikBalCol, wCol, wBalCol, idCol, yearCol, year0, year1,
                        Nmin=0, NminBalNonZero=0, calcDeltaForNonZeroOnly=FALSE,
                        na.rm=FALSE) {
  # This function calculates the variance for the difference between years.
  # Arguments
  # dat:     The data.frame containing all necessary columns to do the calculations: YcCol, YdCol (if not NULL), wCol, wBalCol, idCol, yearCol
  # Ycols:   The name of all columns in dat for which the variance should be calculated.
  #          If Ycols contains variables that look like I(a/b), then for these variables, the quotient calculation is applied (special methodology).
  # YcCol:   The name of the column in dat for which the variance should be calculated (character). See also argument Yc of function varEstZaBh.
  # YdCol:   The name of the column containing the divisor variable (character). See also argument Yc of function varEstZaBh.
  # Xs:      The calibration matrix (numeric).
  # pikCol:  The name of the column in dat containing the probabilities (character). See also argument pik of function varEstZaBh.
  # wCol:    The name of the column in dat containing the weights (character). See also argument w of function varEstZaBh.
  # wBalCol: The name of the column in dat containing the balanced weights (character). I.e. for the observations that are available in both years.
  # idCol:   The name of the column in dat containing the unique identifiers for the observations (character).
  # yearCol: The name of the column in dat containing the years (character).
  # year0:   The preceding year to compare the data in year1 with. Numeric of length 1.
  # year1:   The following year to compare the data in year0 with. Numeric of length 1.
  # Nmin:    The minimum number of observation required for the variance estimation. If the number of observations is smaller than Nmin, then NA will be returned.
  # NminBalNonZero: The minimum number of non zero observations required for the variance estimation of yearly changes. Consider sparce vector with few values other than 0.
  #          It is kind of a mixtrue between normal and binomial distribution. In that case, the formulas for variacne estimation dont work.
  #          NminBalNonZero=20 will only yield a non-NA value if there are at least 20 non-zero observations in each year.
  # na.rm:   Logical indicating if NA values should be removed.
  # Value
  # A list containing the following elements:
  # delta, deltaBal, Rdelta, RdeltaBal, se, p.value
  
  # Error checks.
  if (!is.null(YcCol) && length(YcCol)!=1) stop ("YcCol must be a numeric vector of length 1.")
  if (!is.null(YdCol) && length(YdCol)!=1) stop ("YdCol must be a numeric vector of length 1.")
  if (length(year0)!=1) stop ("year0 must be a numeric vector of length 1.")
  if (length(year1)!=1) stop ("year1 must be a numeric vector of length 1.")
  
  allCols <- c(YcCol, YdCol, pikCol, pikBalCol, wCol, wBalCol, idCol, yearCol)
  colsNotAvail <- allCols[!allCols%in%colnames(dat)]
  if (length(colsNotAvail)>0) stop (paste0("The following columns are not available in dat: ",paste0(colsNotAvail,collapse=", ")))
  
  if (any(is.na(dat[,yearCol]))) stop ("There must be no NA values in dat[,yearCol]")
  
  # As data.frame if given as matrix.
  if (!is.data.frame(dat)) {
    cn1 <- colnames(dat)
    dat <- as.data.frame(dat,stringsAsFactors=FALSE)
    colnames(dat) <- cn1
  }
  
  # Reformulate Ycols such that the function can be universally applied to many columns includint "I()"
  # Ycols <- c("a","b","I(a/b)", "I(c/d)")
  divCols <- NULL
  if (!is.null(Ycols)) {
    calcVersion <- "all_Ycols"
    if (!is.null(YcCol) || !is.null(YdCol)) stop ("Either specify Ycols or YcCol/YdCol. Not all together.")
    divCols <- extract.I.vars(Ycols, keep.original=FALSE, keep.only.necessary=TRUE)
    divColsNotAvail <- divCols[!divCols%in%colnames(dat)]
    if (length(divColsNotAvail)>0)
      stop (paste0("Some columns specified in quotiens of Ycols like 'I(x/y)' are not available in dat: ",paste0(divColsNotAvail, collapse=", ")))
  } else if (is.null(YdCol)) {
    calcVersion <- "only_YcCol"
    Ycols <- YcCol
  } else {
    calcVersion <- "division"
    divCols <- c(YcCol, YdCol)
    Ycols <- c(YcCol, YdCol, paste0("I(",YcCol,"/",YdCol,")"))
    dat <- create.cols(dat, Ycols)
  }
  allCols <- unique(c(allCols, Ycols, divCols))
  
  # Preparations in case a selection with NminBalNonZero should be done.
  if (NminBalNonZero > 0 || calcDeltaForNonZeroOnly) {
    dat <- dat[order(dat[,yearCol], dat[,idCol]),]
  }
  # Balancierten Datensatz bilden
  filtdatBal <- balanced.panel(id=dat[,idCol], year=dat[,yearCol], YEAR=c(year0, year1))
  if (!any(filtdatBal))
    stop ("There are no observations that occur in both years. The variance estimation is impossible.")
  datBal <- dat[filtdatBal,allCols]
  
  # Set values in datBal to NA if there are wo few values other than 0.
  if (NminBalNonZero > 0 || calcDeltaForNonZeroOnly) {
    # It is important, that dat has been sorted according yo yearCol and idCol (see above).
    if (any(order(datBal[,yearCol], datBal[,idCol]) != 1:nrow(datBal)))
      stop ("Internal error: datBal must be sorted according to yearCol and idCol.")
    # datBal <- data.frame(a=c(0,0,0,4:10), b=c(rep(0,8),(-9):(-10)), year=c(1,2,1,2,1,2,1,2,1,2)); yearCol="year"; year0=1; year1=2; Ycols <- c("a","b"); divCols = NULL; NminBalNonZero <- 2
    setNaCols <- c(Ycols,divCols)
    notOkDatBal <- datBal[datBal[,yearCol]==year0,setNaCols,drop=FALSE]==0 & datBal[datBal[,yearCol]==year1,setNaCols,drop=FALSE]==0
    notOkDatBal[is.na(notOkDatBal)] <- TRUE
    colSumsNotOkDatBal <- colSums(notOkDatBal)
    if (calcDeltaForNonZeroOnly) {
      if (!na.rm && any(colSumsNotOkDatBal > 0))
        stop ("If calcDeltaForNonZeroOnly==TRUE, then na.rm must be TRUE, otherwise the function does not work. Unfortunately, it cannot be programmed in another way.")
      datBal[datBal[,yearCol]==year0,setNaCols][ notOkDatBal ] <- NA
      datBal[datBal[,yearCol]==year1,setNaCols][ notOkDatBal ] <- NA
    }
    if (NminBalNonZero > 0) {
      setNaCols <- (nrow(notOkDatBal)-colSumsNotOkDatBal) < NminBalNonZero
      setNaCols <- names(setNaCols)[setNaCols]
      if (!na.rm && length(setNaCols) > 0)
        stop ("If NminBalNonZero > 0, then na.rm must be TRUE, otherwise the function does not work. Unfortunately, it cannot be programmed in another way.")
      datBal[,setNaCols] <- NA
    }
  }
  
  # Give warning if weights are strange.
  filtBal <- datBal[,yearCol] %in% c(year1, year0)
  if (any(!is.finite(datBal[filtBal,wBalCol])) || any(datBal[filtBal,wBalCol]==0) || any(!is.finite(datBal[filtBal,pikBalCol])) || any(datBal[filtBal,pikBalCol]==0))
    warning("There are either non-finite balanced weights/inclusion-probabilities or weights/inclusion-probabilities with value 0 in the data which will result in NaN values for precision estimates.")
  rm(filtBal)
  
  # Loop over years
  res <- list()
  yearsLoop <- c(year1, year0)
  for(i in 1:length(yearsLoop)) {
    filt    <- dat   [,yearCol]==yearsLoop[i]
    filtBal <- datBal[,yearCol]==yearsLoop[i]
    res[[i]] <- .varEstDeltaInnerPreparationForEachCol(dat=dat[filt,c(Ycols,divCols),drop=FALSE], datBal=datBal[filtBal,c(Ycols,divCols),drop=FALSE],
                                                       pik=dat[filt,pikCol], pikBal=datBal[filtBal,pikBalCol],
                                                       w=dat[filt,wCol], wBal=datBal[filtBal,wBalCol], Xs=Xs[filt,,drop=FALSE], Nmin=Nmin, na.rm=na.rm, I.help.columns=divCols)
  }
  res <- .varEstDeltaInnerFinalForEachCol(t0=res[[1]], t1=res[[2]])
  if (ncol(res)==1) {
    nam1 <- rownames(res)
    res <- res[,1]
    names(res) <- nam1
  }
  return (res)
}

calcXsMultiLevel <- function(dat, optVarListMultiLevel) {
  # This function calculates the Xs matrix that is needed for the calib{sampling} function if different variables should be calibrated for different aggregation levels.
  # E.g. you'd like to calibrate variable a on puplation level, but variable b only on regional level.
  # Arguments
  # dat =                  the data.frame/matrix that contains the variables that should be calibrated
  # optVarListMultiLevel = the variable list for different levels. The names of the list correspond to each level that should be calibrated.
  #                        inside each list place there must be a character vector that holds the variables to be calibrated. e.g. list(levelA=c("var1","var2"), levelB=c("var3"))
  
  if (!is.list(optVarListMultiLevel) || is.null(names(optVarListMultiLevel))) stop ("optVarListMultiLevel must be a named list.")
  namesNotInColnames <- names(optVarListMultiLevel)[ !names(optVarListMultiLevel)%in%colnames(dat) ]
  if (length(namesNotInColnames) > 0) stop (paste0("the names of optVarListMultiLevel must represent columns in dat. Some are not contained in colnames(dat):\n",paste0(namesNotInColnames,collapse=", ")))
  lapply(optVarListMultiLevel, function(x)if (!is.null(dim(x))) stop ("In each list place of optVarListMultiLevel there must be a character vector."))
  varsNotInColnames <- sort(unique( unlist(optVarListMultiLevel)[ !unlist(optVarListMultiLevel)%in%colnames(dat) ] ))
  if (length(varsNotInColnames) > 0) stop (paste0("the values in each list place of optVarListMultiLevel must represent columns in dat. Some are not contained in colnames(dat):\n",paste0(varsNotInColnames,collapse=", ")))
  
  # Optimierungs-Zielvariablen in Xs ablegen.
  Xs <- as.list(rep(0, length(optVarListMultiLevel)))
  names(Xs) <- names(optVarListMultiLevel)
  specialCase <- FALSE
  binVars <- NULL
  
  # Multiply the variables to be optimized with the binary index for the stratification on each level.
  for(i in names(optVarListMultiLevel)) {
    # Create binaries (0,1)
    if ( suppressWarnings(all(sort(unique(dat[,i]))==c(0,1))) )  {
      binInd <- dat[,i,drop=FALSE]
      specialCase <- TRUE # Prepare warning for special case of pure binary variable.
      binVars <- c(binVars, i)
    } else {
      binInd <- categ.to.bin(dat[,i])
    }
    # Multiply the binary indices with the corresponding variables that will be calibrated
    # First multiplication is done "manually"
    indTimesVar <- dat[,optVarListMultiLevel[[i]],drop=FALSE]*binInd[,1]
    # The latter will be added to the
    if (ncol(binInd)>1) for(i2 in 2:ncol(binInd)) indTimesVar <- cbind(indTimesVar, dat[,optVarListMultiLevel[[i]],drop=FALSE]*binInd[,i2])
    Xs[[i]] <- indTimesVar
  }
  # Show warning for special case that is not handled correctly in all cases.
  if (specialCase) warning(paste0("Binary indexes (0,1) are directly multiplied with the calibration variables to calculate Xs. If you want to avoid this, use a categorial variable that starts with 1, not 0. -> E.g. use values 1 and 2.\n",
                                  "The following variables are binary:\n", paste0(binVars, collapse=", ")))
  
  # Collapse Xs from list to a matrix/data.frame
  return ( as.matrix(do.call("cbind",Xs)) )
}

#### Private functions ####

#indStr=c(1,1,2,2,3,3,3,3); y <- c(1,2,3,4,5,6,7,8); w <- c(3,6,5,8,1,3,1,9)
.estStudentTmultiplier <- function(prob, indStr, y=NULL, w=NULL, simple=TRUE, na.rm=FALSE) {
  # This function calculates the student t multiplier for a given probability. The degrees of freedom are calculated by simple method or satterthwaite approximation.
  # Arguments
  # prob =   probability
  # indStr = an index defining the strata
  # y =      variable of interest. Needed if !simple.
  # w =      weights. Needed if !simple.
  # simple = simple calculation or more sophisticated calculation using the satterthwaite approximation?
  
  df <- if (simple) length(indStr) - length(unique(indStr)) else .estSatterDf(y=y, w=w, indStr=indStr, na.rm=na.rm)
  #message("df=",df, ", ", sep="")
  return (qt(prob, max(1,df)))  # suppress NaN warnings if df is 0 or -1
}

 #y <- 1:10; w <- rep(1,10); indStr <- c(NA,NA,1,2,2,2,3,3,3,3); .estSatterDf(y=y, w=w, indStr=indStr, na.rm=TRUE, excludeEmptyStrata=TRUE)
.estSatterDf <- function(y, w, indStr, na.rm=FALSE,excludeEmptyStrata=FALSE) {
  # This function calculates the satterthwaite approximation of degrees of freedom.
  # Arguments
  # y =      variable of interest
  # w =      weights
  # indStr = an index defining the strata
  
  s2 <- tapply(y,indStr,var, na.rm=na.rm) # s^2 = var
  Nh <- tapply(w,indStr,sum, na.rm=na.rm)
  nh <- tapply(w,indStr, function(x) if (na.rm) sum(!is.na(x)) else length(x) )
  a <- Nh*(Nh-nh)/nh
  
  if (length(Nh)==0)
    stop ("indStr seems to contain only NA values.")
  
  if (any(is.na(s2))) {
    if (!na.rm)
      return (NA)
    if (excludeEmptyStrata) {
      Nh <- Nh[!is.na(s2)]
      nh <- nh[!is.na(s2)]
      a  <- a [!is.na(s2)]
      s2 <- s2[!is.na(s2)]
      message("There are strata containing either 100% NA values or only 1 non-NA observation. In this case the satterthwaite approximation of degrees of freedom is impossible because var(Ys)==NA. Because excludeEmptyStrata==TRUE, these strata were dropped in order to estimate of the degrees of freedom anyway.")
    } else {
      stop ("There are strata containing either 100% NA values or only 1 non-NA observation. In this case the satterthwaite approximation of degrees of freedom is impossible. If you specify excludeEmptyStrata=TRUE, then these stata will be ignored and the approximation will be carried out anyway.")
    }
  }
  
  if (sum(a)==0) {
    warning("If all(Nh==nh), in other words, if all(weights==1), then the satterthwaite approximation does not work. Returning NA.")
    return (NA)
  }
  return (sum(a*s2)^2 / sum((a*s2)^2 / (nh-1)))
}

# dat <- data.frame(a=1:10, b=10:1, "I(a/b)"=NA); colnames(dat)[3] <- col <- "I(a/b)"
# .varEstDeltaInnerPreparationForEachCol(dat, dat, rep(1,nrow(dat)), rep(1,nrow(dat)), rep(1,nrow(dat)), rep(1,nrow(dat)), NULL)
.varEstDeltaInnerPreparationForEachCol <- function (dat, datBal, pik, pikBal, w, wBal, Xs, Nmin=0, na.rm=FALSE, I.help.columns=NULL) {
  
  divColInd <- startsWith(colnames(dat),"I(") & endsWith(colnames(dat),")") & grepl("/",colnames(dat),fixed=TRUE)
  names(divColInd) <- colnames(dat)
  full <- bal <- vBal <- notNaBalList <- list()
  for (col in colnames(dat)) {
    if (na.rm) {
      notNa <- !is.na(dat[,col])
      notNaBal <- !is.na(datBal[,col])
      # notNaBalList is necessary to pass on to the next function, otherwise wbal and vBal won't have the same length.
      if (!all(notNaBal)) {
        notNaBalList[[col]] <- notNaBal
      }
    } else {
      notNa <- rep(TRUE,nrow(dat))
      notNaBal <- rep(TRUE,nrow(datBal))
    }
    #notNa <- if (na.rm) !is.na(dat[,col]) else rep(TRUE,nrow(dat))
    #notNaBal <- if (na.rm) !is.na(datBal[,col]) else rep(TRUE,nrow(dat))
    if (!divColInd[col]) {
      full[[col]] <- varEstZaBh(Yc=dat[notNa,col], Yd=NULL, pik=pik[notNa], w=w[notNa], Xs=Xs[notNa,,drop=FALSE], calcMean=TRUE, Nmin=Nmin)
      bal[[col]] <- varEstZaBh(Yc=datBal[notNaBal,col], Yd=NULL, pik=pikBal[notNaBal], w=wBal[notNaBal], Xs=NULL, calcMean=TRUE, Nmin=Nmin)
    } else {
      colsDiv <- gsub(" ", "", strsplit(substr(col,3,nchar(col)-1),"/")[[1]])
      full[[col]] <- varEstZaBh(Yc=dat[notNa,colsDiv[1]], Yd=dat[notNa,colsDiv[2]], pik=pik[notNa], w=w[notNa], Xs=Xs[notNa,,drop=FALSE], calcMean=TRUE, Nmin=Nmin)
      bal[[col]] <- varEstZaBh(Yc=datBal[notNaBal,colsDiv[1]], Yd=datBal[notNaBal,colsDiv[2]], pik=pikBal[notNaBal], w=wBal[notNaBal], Xs=NULL, calcMean=TRUE, Nmin=Nmin)
    }
    vBal[[col]] <- bal[[col]]$e * wBal[notNaBal] - bal[[col]]$A
  }
  if (!is.null(I.help.columns)) {
    full <- full[!names(full)%in%I.help.columns]
    bal <- bal[!names(bal)%in%I.help.columns]
  }
  #warning("resw, muesste man hier nicht pik und pikBal mitgeben, damit es spaeter verwendet werden kann.")
  return (list(vary=full, varyBal=bal, vBal=vBal, wBal=wBal, notNaBal=notNaBalList, n=nrow(dat))) # w=w
}

.varEstDeltaInnerFinalForEachCol <- function (t0, t1) {
  res <- vector("list",length(t0$vary))
  names(res) <- names(t0$vary)
  for(i in names(t0$vary)) {
    if (is.null(t0$vary[[i]]$mean) || is.null(t1$vary[[i]]$mean))
      stop ("t0$mean and/or t1$mean must not be NULL. Be sure to set the argument 'calcMean' in the upstream functions to TRUE.")
    # Remove NA values from the weights.
    notNaBal <- rep(TRUE, length(t0$wBal))
    if (i %in% c(names(t0$notNaBal))) notNaBal <- notNaBal & t0$notNaBal[[i]]
    if (i %in% c(names(t1$notNaBal))) notNaBal <- notNaBal & t1$notNaBal[[i]]
    t0wBal <- t0$wBal[notNaBal]
    t1wBal <- t1$wBal[notNaBal]
    # Calculate the figures.
    delta <- t1$vary[[i]]$mean - t0$vary[[i]]$mean
    deltaBal <- t1$varyBal[[i]]$mean - t0$varyBal[[i]]$mean
    Rdelta <- 100*delta / t0$vary[[i]]$mean
    RdeltaBal <- 100*delta / t0$varyBal[[i]]$mean
    #if (i==names(t0$vary)[1])
    #  warning("resw, muesste man hier nicht   1-t1$pikBal  rechnen statt  1-1/t1wBal")
    corBal <- sum((1-1/t1wBal) * t1$vBal[[i]] * t0$vBal[[i]]) /  # Korrelationskoeffizent berechnen
      sqrt( sum((1-1/t0wBal) * t0$vBal[[i]]^2) * sum((1-1/t1wBal) * t1$vBal[[i]]^2) )
    cov. <- corBal * sqrt(t0$vary[[i]]$var * t1$vary[[i]]$var)    # Kovarianz Berechnen
    se <- sqrt(t0$vary[[i]]$var + t1$vary[[i]]$var - 2*cov.) # Standard-Fehler berechnen

    t.value <- delta / se # T-Test berechnen
    p.value <- pt(-abs(t.value),  sum(t0$n) + sum(t1$n)-2); # Einseitiger Test
    
    res[[i]] <- c("delta"=delta, "deltaBal"=deltaBal, "Rdelta"=Rdelta, "RdeltaBal"=RdeltaBal , "cor"=corBal, "se"=se, "p.value"=p.value)
  }
  res <- as.data.frame(do.call("cbind",res))
  return (res)
}

.calcVarStErrOrCI <- function(rawVar, w, figure=c("var","SE","halfLengthCI"), CImultiplier) {
  # Function to calculate variance, standard error or confidence interval on the level of the mean (not total population)
  # Arguments
  # rawVar =       the "raw" variance that should be further processed. rawVar can be calculated e.g. using sampling::varest().
  # w =            vector of weights
  # figure =       the figure to be calculated. Either "var"=variance, "SE"=standard error, "halfLengthCI"=half length of confidence interval.
  # CImultiplier = multiplier to calculate the confidence interval (CI)
  
  figure <- match.arg(figure)
  if (figure=="halfLengthCI") { sqrt(rawVar) * CImultiplier
  } else if (figure=="SE")    { sqrt(rawVar)
  } else if (figure=="var")   {      rawVar }
}

if (TRUE) {
  # These functions split a character into two parts, e.g. using "/" as delimiter. However, only if there are no open brackets.
  # 2018-07-21 -> Function sent to daniel.hoop@agroscope.admin.ch.
  # Can be deleted on hpda PC.
  #.getOperands("(a+b+c)", "+")
  #.getOperands("(a+b)*100", "+", "*")
  .getOperands <- function (txt, innerOperator, outerOperator=NULL) {
    if (missing(innerOperator))
      stop ('argument "innerOperator" is missing, with no default')
    outerVal <- NULL
    outerAtStart <- NULL
    if (!is.null(outerOperator)) {
      txt <- .splitByChar(.rmOuterBrackets(txt), outerOperator)
      if (length(txt) > 2)
        stop ("Only one outerOperator is allowed.")
      if (length(txt) == 2) {
        numbInd <- grepl("^[0-9]+$", txt)
        if (!any(numbInd))
          stop ("The outerOperator must refer to a number, not variable. OK: (a/b)*100,  NOT OK: (a/b)*c")
        outerAtStart <- numbInd[1]
        outerVal <- txt[numbInd]
        txt <- txt[!numbInd]
        if (outerOperator%in%c("-", "/") && !grepl("^\\(|\\)$", txt))
          stop ("If an outerOperator '-' or '/' is used, then the inner operation has to be surrounded by brackets. OK: (a+b)/c,  NOT OK: a+b/c")
      }
    }
    innerVal <- .splitByChar(.rmOuterBrackets(txt), innerOperator)
    return (list("hasInner"=length(innerVal)>1, "innerVal"=innerVal, "innerOp"=innerOperator,
                 "hasOuter"=!is.null(outerVal), "outerVal"=outerVal, "outerOp"=outerOperator, "outerAtStart"=outerAtStart))
  }
  .unRegex <- function (x) {
    if (x == "+")
      return ("\\+")
    if (x == "*")
      return ("\\*")
    return (x)
  }
  .splitByChar <- function (txt, char) {
    if (missing(char))
      stop ('argument "char" is missing, with no default')
    if (length(txt)>1)
      return (apply(matrix(txt), 1, .splitByChar))
    if (!grepl(.unRegex(char), txt))
      return (txt)
    
    res <- NULL
    if (!grepl("\\(|\\)", txt)) {
      res <- strsplit(txt, .unRegex(char))[[1]]
    } else {
      nc <- nchar(txt)
      n <- 0
      for (i in 1:nc) { # i <- 1
        n <- n + (substring(txt,i,i)=="(")
        n <- n - (substring(txt,i,i)==")")
        if (n == 0 && substring(txt, i, i) == char) {
          partTwo <- if (i+1 > nc) NULL else substring(txt, i+1, nc)
          res <- c(substring(txt, 1, i-1), partTwo)
        }
      }
    }
    if (!is.null(res)) {
      if (length(res) == 1)
        stop ("The operator was not between two operands. OK: 'a/b'.  NOT OK: 'ab/'")
      return (res)
    }
    return (txt)
  }
  .rmOuterBrackets <- function (txt) {
    if (length(txt)>1)
      return (apply(matrix(txt), 1, .rmOuterBrackets))
    txt <- trimws(txt)
    if (!grepl("^\\(|\\)$", txt))
      return (txt)
    if (grepl("^\\([^()]*\\)$", txt)) {
      txt <- substring(txt, 2, nchar(txt)-1)
      return (.rmOuterBrackets(txt))
    }
    nc <- nchar(txt)
    n <- 0
    takeOuter <- TRUE
    for (i in 1:nc) { # i <- 1
      n <- n + (substring(txt,i,i)=="(")
      n <- n - (substring(txt,i,i)==")")
      if (n == 0 && i != nc) {
        takeOuter <- FALSE
        break
      }
    }
    if (takeOuter) {
      txt <- substring(txt, 2, nchar(txt)-1)
      return (.rmOuterBrackets(txt))
    }
    return (txt)
  }
}

#### OLD functions to delete ####
if (FALSE) .varestSampling_DELETE <- function(Ys, Xs = NULL, pik, w = NULL) {
  # This function is a copy of the function sampling::varest(). It depends on the function MASS::ginv().
  # It is used inside the variance.estimate() function.
  
  if (any(is.na(pik)))  stop ("there are missing values in pik")
  if (any(is.na(Ys)))  stop ("there are missing values in Ys")
  if (length(Ys) != length(pik))  stop ("Ys and pik have different sizes")
  if (!is.null(Xs)) {
    if (is.data.frame(Xs))
      Xs = as.matrix(Xs)
    if (is.vector(Xs) & (length(Ys) != length(Xs)))  stop ("Ys and Xs have different sizes")
    if (is.matrix(Xs) & (length(Ys) != nrow(Xs))) stop ("Ys and Xs have different sizes")
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
  return (var)
}

#### Testing ####
#index="Reg"; inclusProbs="pik_w0"; weights="Gew_Calib"; dat=db[filtX(),]; Xs=Xs[filtX(),]
#data=spa[filt_all,unique_form]; weights=spa[filt_all,"Gewicht"]; inclusProbs=spa[filt_all,"pik_w0"]; index=x[["vector"]][filt_all]; fixedIndex=TRUE; indexOfFixedResult=indexOfFixedResult; indexStrata=rep(1,sum(filt_all)); method="ht"; figure="halfLengthCI"; CIprob=0.975; na.rm=TRUE; edit.I.colnames=TRUE; CImultiplier=NULL; relativeToMean=TRUE
#filt=spa[,"JAHR"]==2016; data=spa[filt,c("P430_0100_94000","P430_0100_94000")]; weights=spa[filt,"Gewicht"]; inclusProbs=spa[filt,"pik_w0"]; index=spa[filt,"ZATYP"]; fixedIndex=TRUE; indexOfFixedResult=c(11,12,21); indexStrata=rep(1,sum(filt)); method="ht"; figure="halfLengthCI"; CIprob=0.975; na.rm=TRUE; edit.I.colnames=TRUE; CImultiplier=NULL; relativeToMean=TRUE
#filt=spa[,"JAHR"]==2016; variance.estimate(data=spa[filt,c("P430_0100_94000")], weights=spa[filt,"Gewicht"], inclusProbs=spa[filt,"pik_w0"], index=NULL, method="ht", figure="halfLengthCI", CIprob=0.975, na.rm=TRUE, edit.I.colnames=TRUE, CImultiplier=NULL, relativeToMean=TRUE) #spa[filt,"ZATYP"], fixedIndex=TRUE, indexOfFixedResult=c(11,12,21), indexStrata=rep(1,sum(filt)),
if (FALSE) {
  spe <- load.spe.gb()
  
  filt <- spe[,"Jahr"]%in%c(2015,2016) & spe[,"Region"]==3 & spe[,"TypS3"]==1512 & spe[,"Gewicht"]>0 & spe[,"GEWICHT_VGLTYP1516"] > 0
  sort(spe[filt,"ha_LN"])
  sort(spe[filt,"Gewicht"])
  sort(spe[filt,"GEWICHT_VGLTYP1516"])
  
  variance.estimate(data=spe[filt,], cols=c("I(Arbeitsverdienst/JAE_FamAK)", "ha_LN", "ha_uebriges_Brotgetreide","hh_ErfolgLiegensEff_davon"), weights=spe[filt,"Gewicht"], inclusProbs=1/spe[filt,"Gewicht"], index=spe[filt,c("Region","TypS3")], deltaBetweenYears = TRUE, Nmin=0, NminBalNonZero=3,
                    calcDeltaForNonZeroOnly=TRUE, year0=2015, year1=2016, id=spe[filt,"ID"], year=spe[filt,"Jahr"], weightsBalanced=spe[filt,"GEWICHT_VGLTYP1516"], inclusProbsBalanced = 1/spe[filt,"GEWICHT_VGLTYP1516"], figure="pValue")
}
