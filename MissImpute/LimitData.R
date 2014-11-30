# This script deals with the problem of all missing values in the whole dataset at once
# The problem is solved using a technique called MICE - Multivariate Imputation by Chained Equation (Buuren, Groothuis-Oudshoorn, 2011)
# It is a state-of-the-art technique that allows for multiple imputations
# and is relatively assumption-free.
# It takes into the accoutn the nature of missing data and the measurement scale and the type of a variable.
library(mice)
source("MissImpute/ImputeHelper.R")

D = read.csv("Dane/DatIndIm218.csv")
wD = D # working file

# First stage:
# Choose significant variables that will be used in analyses and therefore need to be imputed
wD = wD[,-which(names(wD)=="birth")] # because "birth" is redundant when "age" is included too
wD = wD[, -which(names(wD)=="Wwa")] # no variation - everyone resides in Warsaw
wD = wD[, -which(names(wD)=="district")] # it will not be used in analyses (except for descriptve statistics)
wD = wD[, -which(names(wD)=="children")] # almost no variation - only 5 persons have children
wD = wD[, -which(names(wD)=="work")] # it is redundant when "worktime" is included too
wD = wD[, -which(names(wD)=="occup")]
wD = wD[, -which(names(wD)=="ISCO")] # "occup" and "ISCO" are too dispersed; "ISCObroad" will be used instead
wD = wD[, -which(names(wD)=="pc")] # almost no variation; only two persons does not have a pc
wD = wD[, -which(names(wD)=="internet")] # no variation at all!!!
# There is an obvious outlier that most certeinly is a result of misunderstanding of the question by a respondent; it is recoded to NA
wD[!is.na(wD$tvtime) & D$tvtime==100 , "tvtime"] = NA

wD = wD[, -which(names(wD)=="gender")] # because gender will not be analyzed except for the descriptive statistics
wD = wD[, -which(names(wD)=="income.f")] # this factor variable will not be useful

### Now the dataset is a bit closer to be prepared for the analysis
write.csv(wD, file="Dane/LimDatInd218.csv", row.names=FALSE)
rm(list=ls())

# This is it folks!