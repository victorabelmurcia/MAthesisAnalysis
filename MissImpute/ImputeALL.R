# This script deals with the problem of all missing values in the whole dataset at once
# The problem is solved using a technique called MICE - Multivariate Imputation by Chained Equation (Buuren, Groothuis-Oudshoorn, 2011)
# It is a state-of-the-art technique that allows for multiple imputations
# and is relatively assumption-free.
# It takes into the accoutn the nature of missing data and the measurement scale and the type of a variable.
library(mice)
source("MissImpute/ImputeHelper.R")

D = read.csv("Dane/DataInd218.csv")
wD = D[,2:dim(D)[2]] # working data - without id numbers

NAs = apply(wD, 2, numNA)