##### Info ####
# Tutorial for calibration procedure using the 'sampling' package in R.
#
# Copyright (c) Daniel Hoop, Agroscope, Switzerland.
# Version: 2018-10-16

#### Functions ####
# Read in functions -> You will need categ.to.bin() and mean.weight()
if(!exists("mean.weight")) source("https://raw.githubusercontent.com/danielhoop/R/master/func.R")

#### Create data.frame ####
# Give numer of rows and columns that should randomly be created in test data frame.
nrows <- 1000
ncols <- 5

# There are different stratification levels. E.g. nation=full population, reg=region, type=farm type
# farmCount=1 -> Is later needed for calibration.
# activ=1 -> Indicating if observation was activated.
# resp=1 -> Indicating if observation has responded (i.e. delivered data).
#
df <- data.frame( matrix(sample(1:nrows,nrows*ncols,replace=TRUE), ncol=ncols),
                  "nation"=1, "reg"=sample(1:3,nrows,replace=TRUE), "type"=sample(1:9,nrows,replace=TRUE),
                  "farmCount"=1, "activ"=sample(c(0,1,1), nrows,replace=TRUE)
)
# Change the scale of the different columns
changeCols <- grep("^X[0-9]+$", colnames(df))
df[,changeCols] <- df[,changeCols] * as.list(changeCols)
# Create 'resp' column.
df[,"resp"] <- round(runif(nrows,0,1))*df[,"activ"]
# Statification can also be combined.
df[,"regType"] <- paste(df[,"reg"],df[,"type"],sep="_")
sum(df[,"activ"] ); sum(df[,"resp"] ) # Number of selected/activated and respondend farms

#### Calculate d, w, etc. ####
su.regType <- sort(unique(df[,"regType"])) # definition of strata (region X type of farming)

# Refer to calibration literature for the meaning of the variables. See e.g. help(calib) -> References.
Nh <- tapply.fixed( df[,"farmCount"], df[,"regType"], function(x)sum(x), names.result=su.regType ) # Number of obs. in population by strata
nh <- tapply.fixed( df[,"activ"], df[,"regType"], function(x)sum(x), names.result=su.regType ) # Number of selected observations (gross-sample)
nrh <- tapply.fixed( df[,"resp"], df[,"regType"], function(x)sum(x), names.result=su.regType ) # Number of respondend observations (netto-sample)

d0_raw <- Nh/nh # sampling-design weights
w0_raw <- Nh/nrh # post-stratification weights

df[,"d0"] <- as.numeric( replace.values(names(d0_raw), d0_raw, df[,"regType"]) )
df[,"w0"] <- as.numeric( replace.values(names(w0_raw), w0_raw, df[,"regType"]) )

sum(1/df[,"d0"]); sum(nh) # Sum of inclusion probabilities is equal to the (gross-)sample size
sum(1/df[,"w0"]); sum(nrh) # Sum of total inclusion probabilities is equal to the (net-)sample size
sum(df[df[,"resp"]==1,"w0"]); sum(Nh) # Sum of sampling-design weights of responding farms is equal to the popiulation size
sum(df[df[,"activ"]==1,"d0"]); sum(Nh) # Sum of post-stratificaiton weights of the selected farms is equal to the population size

head(df)

#### Specification of calibration ####

# Which farm sample should be calibrated? "resp" -> responding farms which delivered data.
filtCalib <- df[,"resp"]==1
# List of variables to be calibrated (auciliary variables). This can be distinguished for different calibration levels. E.g. nation or type.
optVarList <- list(nation =  c("farmCount","X1", "X2"),
                   reg =     c("farmCount",      "X2"),
                   type =    c("farmCount",            "X3"),
                   regType = c("farmCount",                 "X4")
)

# Info: - calibrate the number of farms in the sample on different levels: naion, region, type, strata=region X type
#       - calibrate additionally the total of variable x1, x2 on the national level (for exampe X1 = total agricultural area, X2 = livestock unit)
#       - Calibrate additionally the total of variable X2 on the regional level, X3 on the level of farm type and X4 on the level "reg x type"

#### Prepare data for calibration ####
# Xs - Matrix of the auxiliary variables
# Now create the Xs dataset that will be optimized.
Xs <- as.list(rep(0, length(optVarList))); names(Xs) <- names(optVarList)
specialCase <- FALSE

# -> Multiply the variables to be optimized with the binary index for the stratification on each level.
for(i in names(optVarList)){

  # If already binary, to nothing.
  if( suppressWarnings(all(sort(unique(df[,i]))==c(0,1))) ) {
    binInd <- df[,i,drop=FALSE]
    specialCase <- TRUE # Prepare warning for special case that binary variable was entered.
    # Else create binaries (0,1)
  } else {
    binInd <- categ.to.bin(df[,i])
  }
  # Multiply the binary indices with the corresponding variables that will be calibrated
  # First multiplication is done "manually"
  indTimesVar <- df[,optVarList[[i]],drop=FALSE]*binInd[,1]
  # The latter will be added to the
  if(ncol(binInd)>1) for(i2 in 2:ncol(binInd)) indTimesVar <- cbind(indTimesVar, df[,optVarList[[i]],drop=FALSE]*binInd[,i2])
  Xs[[i]] <- indTimesVar
}
# Show warning for special case that is not handled correctly in all cases.
if(specialCase) warning("Binary indexes (0,1) are directly multiplied with the calibration variables to calculate Xs. If you want to avoid this, use a categorial variable that starts with 1, not 0. -> E.g. use values 1 and 2.")
# Show head of Xs on all levels.
lapply(Xs, function(x)head(x))

# Collapse Xs from list to a matrix/data.frame
Xs <- as.matrix(do.call("cbind",Xs))

# Calculate the sum of variable that represents the calibration goal.
totalPopul <- colSums(Xs)
# If the calibration goal in the population is larger than 0, but all observations have value of 0, then the optimization is infeasible.
# Therefore set to 0, also in population.
totalSample <- colSums(Xs[df[,"resp"]==1,,drop=FALSE])
totalPopul[totalSample==0] <- 0

#### Calibration ####

# The actual calibration is done here. Refer to the help files of the calib() function for further information.
require.package(sampling)
df[,c("w_calib_tmp","w_calib")] <- 0
df[filtCalib,"w_calib_tmp"] <- sampling::calib(Xs=Xs[filtCalib,], d=df[filtCalib,"w0"], q=rep(1,sum(filtCalib)), total=totalPopul, method="linear", description=FALSE, max_iter=500)
df[filtCalib,"w_calib"] <- df[filtCalib,"w_calib_tmp"]*df[filtCalib,"w0"]
df <- df[,!colnames(df)%in%"w_calib_tmp"]
# Sum of calibrated weights is identical to nrow(df) (the number of observations in the full sample) because we calibrated the column "farmCount".
sum(df[filtCalib,"w_calib"]) - nrow(df)

#### Check results ####

# Check if calibration was done properly.
levelToCheck <- "nation"
# Calculate the unweighted mean of the whole population for some variables.
m_all <- mean.weight(df[,optVarList[[levelToCheck]]], weights=NULL, index=df[,levelToCheck]); m_all
# Caluclate the weighted mean for the responding farms.
m_resp <- mean.weight(df[,optVarList[[levelToCheck]]], weights=df[,"w_calib"], index=df[,levelToCheck]); m_resp
# What is the difference? Should be zero!
round( m_all - m_resp , 2)

# *** End of tutorial ***
