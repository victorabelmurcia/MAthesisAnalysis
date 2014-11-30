### This script describes and replicates a scaling procedure
### that has been used to scale data on social/civic activity
### variables: from socact1 to socact14
### I employ mokken scaling (and use "mokken" package)
library(mokken)
source("Scaling/scaling-Helper.R")

D = read.csv("Dane/FullDatInd231.csv")
D.back = D # backup copy of the full dataset
D = D[,which(names(D)=="socact1"):which(names(D)=="socact14")]

# sort columns by frequency distribution
D = meanSort(D)
freq = apply(D, 2, mean) # frequency distributions