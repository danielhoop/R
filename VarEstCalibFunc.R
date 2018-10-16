# ***************************************
# Title:   Functions to estimate the variance (imprecision) of estimators based on random samples
# Authors: Daniel Hoop, Swetlana Renner
# Version: 2018-07-26
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
  # figure =          the figure to be calculated. var=variance, SE=standard error, halfLengthCI=half length of confidence interval, pValue=probability value, cor=correlation
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
  
  # Create all required columns in the data frame.
  if (is.null(cols)) {
    cols <- colnames(data)
  }
  # Check if some cols (possibly in I(a+b) columns) are missing. If yes, throw an error.
  .checkMissingICols(cols, colnames(data))
  
  # Prepare the data - create new cols if necessary.
  newDataAndCols <- .getYnAndYd(data=data, col=cols, rowFilt=rep(TRUE, nrow(data)), na.rm=FALSE, prepareDataOnly=TRUE)
  cols_add <- c(extract.I.vars(cols, keep.original=FALSE, keep.only.necessary=TRUE), newDataAndCols$newCols) # can be NULL
  cols_all <- unique(c(cols, cols_add))
  data <- newDataAndCols$data[,cols_all]
  
  # Assure that all columns are numeric/complex
  notNumeric <- sapply(data[,cols,drop=FALSE], function(x)!mode(x)%in%c("numeric","complex"))
  if (any(notNumeric))
    stop (paste0("All columns need to be numeric. The following columns are not: ", paste0(names(notNumeric[notNumeric]), collapse=", ")))
  rm(notNumeric)
  
  # Indices der Aggregationslevel mit den zugehoerigen Variablen multiplizieren.
  # Davon die Varianz-Total-Population berechnen
  var0 <- NULL
  # Loop over all binary levels (indices)
  for(i1 in 1:ncol(levelBin)) { # i1 <- 1
    # Wenn weniger als 2 Betriebe, dann NaN zurueckgeben
    if (sum(levelBin[,i1]) < 2) {
      var1 <- rep(NA_real_, length(colnames(data)))
      names(var1) <- colnames(data)
      # Berechnung im fall dass mind. 2 Betriebe
    } else {
      
      # Calculate variance...
      if (deltaBetweenYears) {
        filt <- levelBin[,i1]
        var1 <- varEstDelta(cbind(data[,cols_all,drop=FALSE], "pikCol_xc87h23"=inclusProbs, "pikBalCol_xc87h23"=inclusProbsBalanced, "wCol_xc87h23"=weights, "wBalCol_xc87h23"=weightsBalanced, "idCol_xc87h23"=id, "yearCol_xc87h23"=year)[filt,,drop=FALSE],
                            pikCol="pikCol_xc87h23", pikBalCol="pikBalCol_xc87h23", wCol="wCol_xc87h23", wBalCol="wBalCol_xc87h23", idCol="idCol_xc87h23", yearCol="yearCol_xc87h23",
                            year0=year0, year1=year1, Ycols=cols_all, Xs=if (method=="calib") Xs[filt,,drop=FALSE] else NULL, Nmin=Nmin, NminBalNonZero=NminBalNonZero, calcDeltaForNonZeroOnly=calcDeltaForNonZeroOnly, na.rm=na.rm)
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
        var1 <- numeric(length(colnames(data)))
        names(var1) <- colnames(data)
        mean1 <- var1
        for(col in colnames(data)) {
          yObj <- .getYnAndYd(data=data, col=col, rowFilt=levelBin[,i1], na.rm=na.rm)
          Yn <- yObj$Yn
          Yd <- yObj$Yd
          filt <- yObj$notNa
          op <- yObj$op
          
          if (length(filt) < 2) {
            var1[col] <- mean1[col] <- NA_real_
          } else {
            # Calculate variance
            rawVar <- varEstZaBh(Yn=Yn[filt], Yd=Yd[filt], Xs=if (method=="calib") Xs[filt,,drop=FALSE] else NULL, pik=inclusProbs[filt], w=weights[filt], calcMean=relativeToMean, Nmin=Nmin)
            # If there was a multiplication by a fix factor in the formula, apply it now. E.g. I(a/b*100)
            rawVar <- .correctRawVarianceByOuterValue(rawVar, op)
            # Calculate CI factor, with satterthwaite
            if (calcCImultiplierFlag && DFestimator=="satterthwaite")
              CImultiplier <- .estStudentTmultiplier(prob=CIprob, indStr=indexStrata[filt], y=data[filt,col], w=weights[filt], simple=FALSE, na.rm=na.rm)
            # Further process variance and return
            var1[col] <- .calcVarStErrOrCI(rawVar=rawVar$var, w=weights[filt], figure=figure, CImultiplier=CImultiplier)
            mean1[col] <- rawVar$mean
          }
        }
        # Calculate the variance relative to mean. Absoulte value, in case the mean is negative.
        # Info: mean1 from varEstZaBh is ratio of means: mean(Yn)/mean(Yd)
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
    var0 <- var0[rownames(var0)%in%rownames(rawResult), colnames(var0)%in%colnames(rawResult),drop=FALSE]
    rawResult[match(rownames(var0),rownames(rawResult)), match(colnames(var0),colnames(rawResult))] <- var0
    var0 <- rawResult
    cols_all <- colnames(var0)
    cols <- colnames(var0)
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

# .v1arEstZaBh(Yn=spe[spe[,"JAHR"]==2016, "LE"], w=spe[spe[,"JAHR"]==2016,"GEWICHT_PS"], Xs=NULL)
# .v1arEstZaBh(Yn=spe[spe[,"JAHR"]==2016, "AV"],Yd=spe[spe[,"JAHR"]==2016, "FJAE"],w<-spe[spe[,"JAHR"]==2016,"GEWICHT_PS"], Xs=NULL)
# .v1arEstZaBh(Yn=spe[spe[,"JAHR"]==2016, "LE"],w=spe[spe[,"JAHR"]==2016,"GEWICHT"], Xs=Xs)
# .v1arEstZaBh(Yn=spe[spe[,"JAHR"]==2016, "AV"], Yd=spe[spe[,"JAHR"]==2016, "FJAE"],w=spe[spe[,"JAHR"]==2016,"GEWICHT"], Xs=Xs)

varEstZaBh <- function(Yn, Yd=NULL, Xs=NULL, pik, w=NULL, calcMean=FALSE, Nmin=0) {
  # This function is an extention of the function sampling::varest(). It depends on the function MASS::ginv().
  # It can be used to calculate mean using HT and Calib approach as well es the corresponding ration of means (quotient)
  # It is used inside the variance.estimate() and .varEstDelta function.
  #
  # Arguments
  # Yn:   Numeric vector giving the values of interest for which the variance should be caluclated. In case the variance of a quotent should be calculated: Yn (c for counter) is the counter.
  # Yd:   In case the variance of a quotient should be calculated: Yd (d for divisor) contains the values of the divisor.
  # Xs:   The calibration matrix (numeric).
  # pik:  The inclusion probabilities (numeric).
  # w:    The weights. For poststratification the condition pik=1/w usually holds. For calibration, however, this might not be the case.
  # Nmin: The minimum number of observation required for the variance estimation. If the number of observations is smaller than Nmin, then NA will be returned.
  # Value
  # A list containing the variance (var), the mean (mean), the residuals (e) and A.
  
  if (any(is.na(pik))) stop ("There must be no missing values in pik.")
  if (any(is.na(Yn))) stop ("There must be no missing values in Yn.")
  if (calcMean && length(Yn) != length(w)) stop ("length(Yn) must be euqla ot length(w).")
  if (!is.null(Xs)) {
    if (is.data.frame(Xs))
      Xs <- as.matrix(Xs)
    if (is.vector(Xs)){
      if (length(Yn) != length(Xs)) stop ("length(Yn) must be equal to length(Xs).")
      if (length(w)  != length(Xs)) stop ("length(w) must be equal to length(Xs).")
    }
    if (is.matrix(Xs)) {
      if (length(Yn) != nrow(Xs)) stop ("length(Yn) must be equal to nrow(Xs).")
      if (length(w)  != nrow(Xs)) stop ("length(w) must be equal to nrow(Xs).")
    }
  }
  # If the number of observations is too small, then return NA values.
  if (length(Yn)<Nmin)
    return (list(var=NA_real_, A=NA_real_, e=rep(NA_real_, length(Yn)), mean=NA_real_))
  
  # Chose the right denomitator, depending on w / pik
  if (is.null(w)) {
    div <- pik
  } else {
    div <- 1/w
  }
  
  
  # Denomitators are either pik or w (in the following lines)
  if (!mode(Yn) %in% c("numeric","complex"))
    stop ("Yn must be numeric")
  if (is.null(Yd)) { # Mean or ration of means
    m <- if (calcMean) weighted.mean(Yn,w) else NA_real_ # Mean can only be calculated if w is given.
    z <- Yn 
    N <- round(sum(1/div)) # Population size (used in variance formula of mean)
  } else {
    if (!mode(Yd) %in% c("numeric","complex"))
      stop ("Yd must be numeric")
    m <- if (calcMean) weighted.mean(Yn,w)/weighted.mean(Yd,w) else NA_real_ # Mean can only be calculated if w is given.
    Ynhat <- sum(Yn/div)
    Ydhat <- sum(Yd/div)
    r <- Ynhat/Ydhat 
    z <- (Yn - r * Yd)/Ydhat 
    N <- 1
  }
  
  if (is.null (Xs)) {
    e <- z
  } else {
    B <- t(Xs*w)
    tryCatch({
      beta <- MASS::ginv(B %*% Xs) %*% B %*% z
    }, error=function(e)browser())
    e <- unname((z-Xs%*%beta)[,1]) # Result is a matrix. Convert to vector.
  }
  
  # Denominator is pik! Not div!
  a <-(1-pik)/sum(1-pik)
  A <- sum(a*e/pik)
  v <- sum((1-pik)*(e/pik-A)^2)/(1-sum(a^2)) / N^2
  
  return (list(var=v, A=A, e=e, mean=m))
}

# dat <- data.frame(a=1:10, b=10:1, "I(a/b)"=NA); colnames(dat)[3] <- col <- "I(a/b)"
varEstDelta <- function(dat, Ycols=NULL, YnCol=NULL, YdCol=NULL, Xs=NULL, pikCol, pikBalCol, wCol, wBalCol, idCol, yearCol, year0, year1,
                        Nmin=0, NminBalNonZero=0, calcDeltaForNonZeroOnly=FALSE,
                        na.rm=FALSE) {
  # This function calculates the variance for the difference between years.
  # Arguments
  # dat:     The data.frame containing all necessary columns to do the calculations: YnCol, YdCol (if not NULL), wCol, wBalCol, idCol, yearCol
  # Ycols:   The name of all columns in dat for which the variance should be calculated.
  #          If Ycols contains variables that look like I(a/b), then for these variables, the quotient calculation is applied (special methodology).
  # YnCol:   The name of the column in dat for which the variance should be calculated (character). See also argument Yn of function varEstZaBh.
  # YdCol:   The name of the column containing the divisor variable (character). See also argument Yn of function varEstZaBh.
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
  if (is.null(Ycols) && is.null(YnCol) && is.null(YdCol))
    Ycols <- colnames(dat)
  if (!is.null(YnCol) && length(YnCol)!=1) stop ("YnCol must be a numeric vector of length 1.")
  if (!is.null(YdCol) && length(YdCol)!=1) stop ("YdCol must be a numeric vector of length 1.")
  if (length(year0)!=1) stop ("year0 must be a numeric vector of length 1.")
  if (length(year1)!=1) stop ("year1 must be a numeric vector of length 1.")
  
  allCols <- c(YnCol, YdCol, pikCol, pikBalCol, wCol, wBalCol, idCol, yearCol)
  colsNotAvail <- allCols[!allCols%in%colnames(dat)]
  if (length(colsNotAvail)>0) stop (paste0("The following columns are not available in dat: ",paste0(colsNotAvail,collapse=", ")))
  
  if (any(is.na(dat[,yearCol]))) stop ("There must be no NA values in dat[,yearCol]")
  
  # As data.frame if given as matrix.
  if (!is.data.frame(dat)) {
    cn1 <- colnames(dat)
    dat <- as.data.frame(dat, stringsAsFactors=FALSE)
    colnames(dat) <- cn1
  }
  
  # Reformulate Ycols such that the function can be universally applied to many columns includint "I()"
  # Ycols <- c("a","b","I(a/b)", "I(c/d)")
  divCols <- NULL
  if (!is.null(Ycols)) {
    calcVersion <- "all_Ycols"
    if (!is.null(YnCol) || !is.null(YdCol)) stop ("Either specify Ycols or YnCol/YdCol. Not all together.")
    newDataAndCols <- .getYnAndYd(data=dat, col=Ycols, rowFilt=rep(TRUE, nrow(dat)), na.rm=FALSE, prepareDataOnly=TRUE)
    divCols <- newDataAndCols$newCols
    dat <- newDataAndCols$data
  } else if (is.null(YdCol)) {
    calcVersion <- "only_YnCol"
    Ycols <- YnCol
  } else {
    calcVersion <- "division"
    divCols <- c(YnCol, YdCol)
    Ycols <- c(YnCol, YdCol, paste0("I(",YnCol,"/",YdCol,")"))
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
    stop (paste0("There are no observations that occur in both years. The variance estimation is impossible. Please be aware that you must not include the year into the index if you want to calculate differences between years. Available years are: ", sort(unique(dat[,yearCol]))))
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
      datBal[datBal[,yearCol]==year0,setNaCols][ notOkDatBal ] <- NA_real_
      datBal[datBal[,yearCol]==year1,setNaCols][ notOkDatBal ] <- NA_real_
    }
    if (NminBalNonZero > 0) {
      setNaCols <- (nrow(notOkDatBal)-colSumsNotOkDatBal) < NminBalNonZero
      setNaCols <- names(setNaCols)[setNaCols]
      if (!na.rm && length(setNaCols) > 0)
        stop ("If NminBalNonZero > 0, then na.rm must be TRUE, otherwise the function does not work. Unfortunately, it cannot be programmed in another way.")
      datBal[,setNaCols] <- NA_real_
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
  if (length(namesNotInColnames) > 0) stop (paste0("The names of optVarListMultiLevel must represent columns in dat. Some are not contained in colnames(dat):\n",paste0(namesNotInColnames,collapse=", ")))
  lapply(optVarListMultiLevel, function(x)if (!is.null(dim(x))) stop ("In each list place of optVarListMultiLevel there must be a character vector."))
  varsNotInColnames <- sort(unique( unlist(optVarListMultiLevel)[ !unlist(optVarListMultiLevel)%in%colnames(dat) ] ))
  if (length(varsNotInColnames) > 0) stop (paste0("The values in each list place of optVarListMultiLevel must represent columns in dat. Some are not contained in colnames(dat):\n",paste0(varsNotInColnames,collapse=", ")))
  
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
      return (NA_real_)
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
    return (NA_real_)
  }
  return (sum(a*s2)^2 / sum((a*s2)^2 / (nh-1)))
}

# dat <- data.frame(a=1:10, b=10:1, "I(a/b)"=NA); colnames(dat)[3] <- col <- "I(a/b)"
# .varEstDeltaInnerPreparationForEachCol(dat, dat, rep(1,nrow(dat)), rep(1,nrow(dat)), rep(1,nrow(dat)), rep(1,nrow(dat)), NULL)
.varEstDeltaInnerPreparationForEachCol <- function (dat, datBal, pik, pikBal, w, wBal, Xs, Nmin=0, na.rm=FALSE, I.help.columns=NULL) {
  
  full <- bal <- vBal <- notNaBalList <- list()
  for (col in colnames(dat)) {
    yObj <- .getYnAndYd(data=dat, col=col, rowFilt=rep(TRUE, nrow(dat)), na.rm=na.rm)
    yObjBal <- .getYnAndYd(data=datBal, col=col, rowFilt=rep(TRUE, nrow(datBal)), na.rm=na.rm)
    notNa <- yObj$notNa
    notNaBal <- yObjBal$notNa
    # if (col %in% c("I(ha_Weizen+ha_Dinkel)","ha_WeizenUndDinkel")) {
    #   print(col)
    #   s1 <- sum(yObj$Yn, na.rm=TRUE)
    #   s2 <- sum(yObjBal$Yn, na.rm=TRUE)
    #   print(s1)
    #   print(s2)
    #   if (s1 != s2)
    #     browser()
    # }
    # notNaBalList is necessary to pass on to the next function, otherwise wbal and vBal won't have the same length.
    if (na.rm && !all(notNaBal)) {
      notNaBalList[[col]] <- notNaBal
    }
    full[[col]] <- .correctRawVarianceByOuterValue(varEstZaBh(Yn=yObj$Yn[notNa], Yd=yObj$Yd[notNa], pik=pik[notNa], w=w[notNa], Xs=Xs[notNa,,drop=FALSE], calcMean=TRUE, Nmin=Nmin),
                                                   yObj$op)
    bal[[col]] <- .correctRawVarianceByOuterValue(varEstZaBh(Yn=yObjBal$Yn[notNaBal], Yd=yObjBal$Yd[notNaBal], pik=pikBal[notNaBal], w=wBal[notNaBal], Xs=NULL, calcMean=TRUE, Nmin=Nmin),
                                                  yObjBal$op)
    vBal[[col]] <- bal[[col]]$e * (1/pikBal[notNaBal]) - bal[[col]]$A
    #bal[[col]]$Y <- datBal[notNaBal,col] # Only needed if cor(y1, y2) is called downstream
  }
  if (!is.null(I.help.columns)) {
    full <- full[!names(full)%in%I.help.columns]
    bal <- bal[!names(bal)%in%I.help.columns]
  }
  #warning("resw, muesste man hier nicht pik und pikBal mitgeben, damit es spaeter verwendet werden kann.")
  return (list(vary=full, varyBal=bal, vBal=vBal, pikBal=pikBal, notNaBal=notNaBalList, n=sum(notNa), nBal=sum(notNaBal))) # w=w
}

.varEstDeltaInnerFinalForEachCol <- function (t0, t1) {
  res <- vector("list",length(t0$vary))
  names(res) <- names(t0$vary)
  for(i in names(t0$vary)) {
    if (is.null(t0$vary[[i]]$mean) || is.null(t1$vary[[i]]$mean))
      stop ("t0$mean and/or t1$mean must not be NULL. Be sure to set the argument 'calcMean' in the upstream functions to TRUE.")
    # Remove NA values from the weights.
    notNaBal <- rep(TRUE, length(t0$pikBal))
    if (i %in% c(names(t0$notNaBal))) notNaBal <- notNaBal & t0$notNaBal[[i]]
    if (i %in% c(names(t1$notNaBal))) notNaBal <- notNaBal & t1$notNaBal[[i]]
    t0pikBal <- t1$pikBal[notNaBal]
    t1pikBal <- t0$pikBal[notNaBal]
    if (length(t0$varyBal[[i]]) != length(t1$varyBal[[i]]))
      stop ("Some observations have NA values in one year but non-NA values in the other year. In that case, na.rm=TRUE does not work. Please remove all NA values first, then call the function with na.rm=FALSE.")
    # Calculate the figures.
    delta <- t1$vary[[i]]$mean - t0$vary[[i]]$mean
    deltaBal <- t1$varyBal[[i]]$mean - t0$varyBal[[i]]$mean
    Rdelta <- 100*delta / t0$vary[[i]]$mean
    RdeltaBal <- 100*delta / t0$varyBal[[i]]$mean
    corBal <- sum((1-t1pikBal) * t1$vBal[[i]] * t0$vBal[[i]]) /  # Korrelationskoeffizent berechnen
      sqrt( sum((1-t0pikBal) * t0$vBal[[i]]^2) * sum((1-t1pikBal) * t1$vBal[[i]]^2) )
    #corBal <- cor(t0$varyBal[[i]]$Y, t1$varyBal[[i]]$Y) # Alternative, proposed by Ben Derrick, 2017
    cov. <- corBal * sqrt(t0$vary[[i]]$var * t1$vary[[i]]$var)    # Kovarianz Berechnen
    tryCatch({
      se <- sqrt(t0$vary[[i]]$var + t1$vary[[i]]$var - 2*cov.) # Standard-Fehler berechnen  
    }, warning=function(w){
      warning("The sum of variances in the samples of t0 and t1 is smaller than 2*covariance. In this case the calculation of the standard error is impossible.")
    })
    
    # Version resw
    t.value <- delta / se # T-Test berechnen
    df <- .pValuePartiallyOverlappingNew2(delta=delta, s1=sqrt(t0$vary[[i]]$var), s2=sqrt(t1$vary[[i]]$var), n1=t0$n, n2=t1$n, nc=length(t0pikBal), r=corBal)$df
    # df <- sum(t0$n) + sum(t1$n)-2 # This is wrong. Old version of resw, before 2018-07-26.
    p.value <- pt(-abs(t.value), df); # Einseitiger Test
    # Calculate p value, something is wrong with this apporach. Checked on 2018-07-26.
    # p.value <- .pValuePartiallyOverlappingNew2(delta=delta, s1=sqrt(t0$vary[[i]]$var), s2=sqrt(t1$vary[[i]]$var), n1=t0$n, n2=t1$n, nc=length(t0pikBal), r=corBal)$p.value
    
    res[[i]] <- c("delta"=delta, "deltaBal"=deltaBal, "Rdelta"=Rdelta, "RdeltaBal"=RdeltaBal , "cor"=corBal, "se"=se, "p.value"=p.value)
  }
  res <- as.data.frame(do.call("cbind", res))
  return (res)
}

.pValuePartiallyOverlappingNew2 <- function (delta, s1, s2, n1, n2, nc, r) {
  # Ben Derrick, 2017, Test Statistics for the Comparison of Means for Two Samples That Include Both Paired and Independent Observations, Journal of Modern Applied Statistical Methods
  # p. 144, T_new2, v_new2, gamma
  # Calculating the t-value, degrees of freedom and eventually, p-value.
  # delta = mean1 - mean2
  # s1, s2 = standard deviations in samples t1 and t2
  # n1, n2 = number of observations in samples t1 and t2
  # nc = number of obs. in balanced / constant sample
  # r = correlation coefficient between values in t1 and t2 (balanced sample)
  
  # t.value
  t.value <- NULL
  #t.value <- (delta /
  #              (sqrt(s1^2/n1 + s2^2/n2 - 2*r*(s1*s2*nc/(n1*n2)))))
  # degrees of freedom
  na <- n1 - nc
  nb <- n2 - nc
  gamma <- ( (s1^2/n1 + s2^2/n2)^2 /
               ((s1^2/n1)^2 / (n1-1)   +   (s2^2/n2)^2 / (n2-1)) )
  df <- ((nc-1) + ((gamma-nc-1)/(na+nb+2*nc)) * (na+nb))
  #cat("df=", df, " gamma=", gamma, " n1=", n1, " n2=", n2, " nc=", nc, "\n", sep="")
  # p.value
  p.value <- NULL
  #p.value <- pt(-abs(t.value))
  return (list(p.value=p.value, t.value=t.value, df=df))
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
    txtOrig <- txt
    outerVal <- NULL
    outerAtStart <- NULL
    # If innerOperator==outerOperator and there is only one occurence of the innerOperator, then skip outerOperator.
    if (!is.null(outerOperator)) {
      if (innerOperator == outerOperator) {
        if (nchar(txt) - nchar(gsub(.unRegex(innerOperator), "", txt)) == 1) {
          outerOperator <- NULL
        }
      }
    }
    if (!is.null(outerOperator)) {
      txt <- .splitByChar(.rmOuterBrackets(txt), outerOperator)
      if (length(txt) > 2)
        stop (paste0("Only one outerOperator is allowed.\n",
                     "Errorneous expression is: ", txtOrig, "\n",
                     "outerOperators are: ", paste0(txt, collapse=", ")))# ("In case innerOperator==outerOperator, try using brakets to avoid this error. OK: '(a/b)/100',  NOT OK: 'a/b/100'")
      if (length(txt) == 2) {
        numbInd <- grepl("^[0-9]+$", txt)
        if (!any(numbInd))
          stop (paste0("The outerOperator must refer to a number, not variable. OK: (a/b)*100,  NOT OK: (a/b)*c\n",
                       "Errorneous expression is: ", txtOrig, "\n",
                       "These are the extracted operators: ", paste0(txt[!numbInd], collapse=", ")))
        outerAtStart <- numbInd[1]
        outerVal <- txt[numbInd]
        txt <- txt[!numbInd]
        if (outerOperator%in%c("-", "/") && grepl("[\\+\\/\\*\\-]", txt) && !grepl("^\\(|\\)$", txt))
          stop (paste0("If an outerOperator '-' or '/' is used, then the inner operation has to be surrounded by brackets. OK: '(a+b)/c',  NOT OK: 'a+b/c'\n",
                       "Errorneous expression is: ", txtOrig, "\n"))
      }
    }
    innerVal <- .splitByChar(.rmOuterBrackets(txt), innerOperator)
    # If one inner value is a number, then run the function again with innerOperator==define it as outer value and give the correspondend outerOperator
    if (length(innerVal) == 2 && any(grepl("^[0-9]+$", innerVal))) {
      res <- .getOperands(txt=txtOrig, innerOperator="?", outerOperator=innerOperator)
      res[["innerOp"]] <- innerOperator
      return (res)
    }
    return (list("hasInner"=length(innerVal)>1, "innerVal"=innerVal, "innerOp"=innerOperator,
                 "hasOuter"=!is.null(outerVal), "outerVal"=outerVal, "outerOp"=outerOperator, "outerAtStart"=outerAtStart))
  }
  .unRegex <- function (x) {
    if (x == "+")
      return ("\\+")
    if (x == "*")
      return ("\\*")
    if (x == "?")
      return ("\\?")
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
      if (length(res) == 1) {
        stop ("The operator was not between two operands. OK: 'a/b'.  NOT OK: 'ab/'")
      }
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
  # This function corrects the variance figures by a fixed scalar muliplier/divisor.
  .correctRawVarianceByOuterValue <- function (vObj, oObj) {
    #warning("resw, meinst du, man kann hier einfach A und e mit einem Faktor multiplizieren? Anhand der Berechnungsformeln denke ich, schon. 2018-07-23")
    if (is.null(oObj) || !oObj$hasOuter)
      return (vObj)
    if (length(vObj)!=4 || any(sort(names(vObj)) != c("A","e","mean","var")))
      stop ("Internal error. If oObj$hasOuter, the result places in vObj will be adapter afterwards. The names don't match to the orgininal ones.")
    if (oObj$outerAtStart) {
      vObj$var <-  eval(parse(text=paste0("(", oObj$outerVal,"^2)", oObj$outerOp, "vObj$var")))
      vObj$A <-    eval(parse(text=paste0(     oObj$outerVal,       oObj$outerOp, "vObj$A")))
      vObj$e <-    eval(parse(text=paste0(     oObj$outerVal,       oObj$outerOp, "vObj$e")))
      vObj$mean <- eval(parse(text=paste0(     oObj$outerVal,       oObj$outerOp, "vObj$mean")))
    } else {
      vObj$var <-  eval(parse(text=paste0("vObj$var",  oObj$outerOp, "(", oObj$outerVal, "^2)")))
      vObj$A <-    eval(parse(text=paste0("vObj$A",    oObj$outerOp,      oObj$outerVal)))
      vObj$e <-    eval(parse(text=paste0("vObj$e",    oObj$outerOp,      oObj$outerVal)))
      vObj$mean <- eval(parse(text=paste0("vObj$mean", oObj$outerOp,      oObj$outerVal)))
    }
    return (vObj)
  }
  # This funcion extracts Yn and Yd from a division like I((a+b)/c)
  .getYnAndYd <- function (data, col, rowFilt, na.rm, prepareDataOnly=FALSE) {
    #cat("function was called: .getYnAndYd\n")
    if (!prepareDataOnly && length(col) > 1)
      stop("Internal error. 'col' must be a character of length 1 if !prepareDataOnly.")
    if (prepareDataOnly) {
      na.rm <- FALSE
    }
    
    newCols <- NULL
    allCols <- col
    catchedErrors <- .messageQueue$new()
    
    for(col in allCols) {
      tryCatch({
        #cat(paste0("col is:", col, "\n"))
        # Prepare the result that the column has no outer operator like (a/b)*100, so that this information can be accessed, even if the colname was no formula.
        op <- list("hasInner"=FALSE, "hasOuter"=FALSE)
        # Case I() column
        if (startsWith(col,"I(") && endsWith(col,")")) {
          # In case of a division, separate counter and denominator.
          # Remove I()
          col1 <- NULL
          if (grepl("/",col)) {
            col1 <- substr(col,3,nchar(col)-1)
            # Descructure the expression into parts
            for (outerOperator in c("*", "/")) {
              op <- .getOperands(col1, innerOperator="/", outerOperator=outerOperator)
              if (op$hasOuter) break
            }
            # Create columns in case they dont exist, e.g. if '(a+b)/c' is given, create '(a+b)' in the data set.
            computeCols <- c(op$innerVal)
            if (op$hasInner && length(computeCols) != 2)
              stop (paste0("In quotients like 'I(x/y)' only two columns are allowed.\n",
                           "Errorneous expression: ", paste0(col1, collapse=", ")))
            # If there is a divisor col like I(a/b), then op$innerVal will only get 'a' and 'b', but not 'I(a/b)', which shall also be computed in case prepareDataOnly==TRUE.
            if (prepareDataOnly) {
              computeCols <- c(computeCols, col)
            }
            # If there is no division, just use the normal col.
          } else {
            computeCols <- col
          }
          # Create columns if they are missing.
          computeColsNotAvail <- computeCols[!computeCols%in%colnames(data)]
          if (length(computeColsNotAvail)>0) {
            newCols <- c(newCols, computeColsNotAvail)
            tryCatch({
              for (calcCol in computeColsNotAvail) {
                #cat(paste0("will create column: ", calcCol, "\n"))
                data[,calcCol] <- as.vector(with(data, eval(parse(text=calcCol)))) # as.vector is neccessary because otherwise it will be of class "AsIs"
              }
            }, error=function(e){
              if(grepl("unexpected", e$message)) {
                stop (paste0("The calculation syntax in a column like 'I(a+b)' is errorneous. See the error message below.\n",
                             gsub("<[^u]+: ","",e$message)), call.=FALSE)
              } else {
                stop (paste0("The calculation in a column like 'I(a+b)' is not possible, probably because a variable is missing. See the error message below.\n",
                             calcCol, ", ", e$message), call.=FALSE)
              }
            })
          }
          # Define Yn and Yd.
          Yn <- data[,computeCols[1]]
          Yd <- if (op$hasInner) data[,computeCols[2]] else numeric(length(Yn))
          filt <- if (na.rm) rowFilt & !is.na(Yn) &!is.na(Yd) else rowFilt
          # This can happen when a) no division is given, like 'a+b' or b) something like 'a/100' is given. Then 100 is the outer operator and there is no inner operator.
          if (!op$hasInner) {
            Yd <- NULL
          }
          # Case NOT I() column
        } else {
          Yn <- data[,col]
          Yd <- NULL
          filt <- if (na.rm) rowFilt & !is.na(Yn) else rowFilt
        }
        if (na.rm) filt[is.na(filt)] <- FALSE
        
      }, # try end, catch the error message and store it in the message queue called 'catchedErrors'.
      error=function(e){
        catchedErrors$add(e$message)
      })
    }
    
    # If there were errors, then display them now.
    catchedErrors <- catchedErrors$flush()
    if (length(catchedErrors) > 0) {
      # Case of 1 error
      if (length(catchedErrors) == 1)
        stop (catchedErrors)
      # Multiple errors
      cat(paste0(c("", catchedErrors, ""), collapse="\n-------------------------------------------\n"), "\n", sep="")
      stop ("Some columns like I(a/b) could not be computed. See the text output above this message to find out why.")
    }
    
    newCols <- unique(newCols[!grepl("^[0-9+]$", newCols)])
    if (prepareDataOnly) {
      return (list(Yn=NULL, Yd=NULL, notNa=NULL, op=NULL, data=data, newCols=newCols))
    } else {
      return (list(Yn=Yn,   Yd=Yd,   notNa=filt, op=op,   data=NULL, newCols=newCols))
    }
  }
}

# The method .messageQueue is used to catch errors in tryCatch 'by reference', without the '<<-' operator.
.messageQueue <- setRefClass(
  Class="messageQueue",
  
  fields=list(queue="character"),
  
  methods=list(
    # Methods to override.
    # Initialization of the class.
    initialize = function(queue) {
      if (missing(queue)) {
        .self$queue <- character()
      } else {
        .self$queue <- queue
      }
    }
    # print() method.
    ,show = function() {
      print(queue)
    }
    # own methods
    ,add = function(message) {
      .self$queue <- c(queue, message)
    }
    ,flush = function() {
      res <- queue
      .self$queue <- character()
      return (res)
    }
  )
)

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

#### Tutorial ####
# *** Tutorial ***  <- This comment must be here, otherwise the function compressor will cut this line out of the document!

# Create the dataset.
calibTutorial <- function(giveURL=FALSE){
  tutorialURL <- "https://raw.githubusercontent.com/danielhoop/R/master/tutorials/calibration_tutorial_FADN_sample_multiple_levels.R"
  if (giveURL)
    return(tutorialURL)
  tutorialTxt <- readLines(tutorialURL)
  message(paste("#### Tutorial from gitbut ####", paste0("# ", tutorialURL), "", sep="\n"))
  message(paste0(tutorialTxt, sep="\n"))
}

varEstTutorial <- function(){
  tutorialTxt <- '



'
  message(paste0(tutorialTxt, sep="\n"))
}
varEstTutorial()
