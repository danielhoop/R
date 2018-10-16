##### Info ####
# Tutorial for using the variance estimation package by Daniel Hoop and Swetlana Renner.
#
# Copyright (c) Daniel Hoop, Agroscope, Switzerland.
# Version: 2018-10-16

#### Functions ####
# You will need some additional functions.
if(!exists("mean.weight")) source("https://raw.githubusercontent.com/danielhoop/R/master/func.R")
if(!exists("variance.estimate")) loadVarEstCalibFunc()
  
#### Data preparation ####
# First create a dataset as explained in the calibration tutorial. Call the function "calibTutorial" to see how.
# calibTutorial()
# Either copy all the code from the tutorial and execute the code. Then the data will be ready.
# Or run this:
source(calibTutorial(giveURL=TRUE))


#### Variance estimation ####

#### Note on divisions ####
# Notice that we use w0 as the weight (for simplicity) as opposed to the calibrated weight.
# Otherwise you would have to include the full code of the calibration tutorial.

# Define the columns to calculate imprecision measures.
# Be aware that the variance estimate of divisions is handled specially.
# IMPORTANT: You have to declare these divisons as "I(a/b)" in the "cols" argument. See below.
cols <- c("X1", "X2", "X3", "X4", "X5", "I(X1/X2)", "I((X1+X2)/X5*100)")

# For demonstration purposes we create the division variables inside the data.frame
df <- within(df, {
  X1X2 <- X1+X2
  "divErr1" <- X1/X2
  "divErr2" <- (X1+X2)/X5*100
})
cols <- c(cols, "divErr1", "divErr2", "I(X1X2/X5)")

# We prepare weights and inclusProbs as variables such that the code will be shorter afterwards
weights <- df[,"w0"]
inclusProbs <- 1/weights

# Calculate the variance with the Horvitz-Thompson esimator for the whole dataset.
v <- variance.estimate(df, cols=cols, weights=weights, inclusProbs=inclusProbs, figure="var", method="ht")
# - The variance of I(X1/X2) and divErr1 differ dramatically! Same holds true for I((X1+X2)/X5*100) and divErr2!
print(v)
# - Notice the difference! The reason is that it is the variance and thus squared!
print(v["I((X1+X2)/X5*100)"]);
100 * print(v["I(X1X2/X5)"])
# - See below. Now it's identical.
print( v["I((X1+X2)/X5*100)"] - 100^2 * v["I(X1X2/X5)"] )

# To move on, let's redifile cols without the errorneos columns.
cols <- c("X1", "X2", "X3", "X4", "X5", "I(X1/X2)", "I((X1+X2)/X5*100)")

#### Note on fixedIndex, indexOfFixedResult ####
# Suppose that not all combinations of region and type are in your data set. But you want to display them all.
# That is what the arguments "fixedIndex", "indexOfFixedResult", "indexSep" are meant for.
indexSep <- "_"
indexOfFixedResult <- paste.cols(expand.grid(sort(unique(df[,"reg"])), sort(unique(df[,"type"]))), sep=indexSep); print(indexOfFixedResult)
filt <- 1:50
# All index combinations are shown no matter if they occured in the data set.
v <- variance.estimate(df[filt,], cols=cols, weights=weights[filt], inclusProbs=inclusProbs[filt], figure="var", method="ht", index=df[filt,c("reg","type")],
                  fixedIndex = TRUE, indexOfFixedResult = indexOfFixedResult, indexSep=indexSep); print(v)

#### Note on Nmin ####
# The variance estimation is based on parametric methods assuming gaussian distribution. This is problematic if the sample size is too small.
# You can automatically set values of small samples to NA using the argument "Nmin"

# Choose all observations from reg=1, type=1, but only 5 observations from reg=1, type=2
filt <- c(which(df[,"reg"] == 1 & df[,"type"] == 1),
          which(df[,"reg"] == 1 & df[,"type"] == 2)[5])
# We set Nmin to 10, thus the result of reg=1, type=2 will be set to NA
v <- variance.estimate(df[filt,], cols=cols, weights=weights[filt], inclusProbs=inclusProbs[filt], figure="var", method="ht", index=df[filt,c("reg","type")], Nmin=10); print(v)

#### Other variance figures ####
# Calculate the standard error
v <- variance.estimate(df, cols=cols, weights=weights, inclusProbs=inclusProbs, figure="SE", index=df[,"reg"]); print(v)
# Now let's take advantage of the fact that we calibrated our weights
# The variance for all calibrated varibles (X1, X2, X3, X4) is 0 because we know that our estimator is exactely equal to the population sample!
v <- variance.estimate(df, cols=cols, weights=weights, inclusProbs=inclusProbs, figure="SE", index=df[,"reg"], method="calib", Xs=Xs); print(v)

# The confidence interval of the estimator can also be calculated.
# Assuming a large sample we specify CImultiplier of 1.96 for the 0.975 confidence interval.
v1 <- variance.estimate(df, cols=cols, weights=weights, inclusProbs=inclusProbs, figure="halfLengthCI", method="ht", CImultiplier=1.96, index=df[,"type"]); print(v1)

# Using CImultiplier for small sample sizes might be wrong, however. 
# Rather use the argument "CIprob"
# To calculate the degrees of freedom dependend on the sample size we need to provide the stratification.
# We assume that the sample was stratified on the level region x type (column "regType")
v2 <- variance.estimate(df, cols=cols, weights=weights, inclusProbs=inclusProbs, figure="halfLengthCI", method="ht", index=df[,"type"],
                       CIprob=0.975, indexStrata=df[,"regType"]); print(v2)

# Notice that the estimated confidence intervals are now slightly higher
round(v2 / v1 - 1 ,3)

#### Significant differences between years ####
# Let's assume that two years were sampled. We insert a column called "year".
# Be aware that the calibration is wrong for the individual years but we don't mind for the following lines.
# Notice that the sample doesn't have to be balanced but there needs to be an overlap between the years!
df[,c("id","year")] <- -1L
# year0
filt <- 1:floor(nrow(df)/2)
df[filt,"year"] <- 2000L
df[filt,"id"] <- 1L:length(filt)
# year1
filt <- ceiling(nrow(df)/2):nrow(df)
df[filt,"year"] <- 2001L
df[filt,"id"] <- 1L:length(filt)

# Calculate the p value of the difference from year 0 to year 1 (on the index level "reg")
# For simplicity we assume that the balanced weights and inclusion probabilities are identical to the normal weights/inclusion probabilities.
p <- variance.estimate(df, cols=cols, weights=weights, inclusProbs=inclusProbs, figure="pValue", index=df[,"reg"], method="ht",
                       deltaBetweenYears=TRUE, year0=2000, year1=2001, id=df[,"id"], year=df[,"year"],
                       weightsBalanced=weights, inclusProbsBalanced=inclusProbs); print(p)
# -> Because we worked with random numbers it's no surprise that the difference between the years is not significant in most cases.


# *** End of tutorial ***
