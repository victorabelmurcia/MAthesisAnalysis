### This script describes and replicates a scaling procedure
### that has been used to scale data on social/civic activity
### variables: from socact1 to socact14
### I employ mokken scaling (and use "mokken" package)
library(mokken)

D = read.csv("Dane/DataInd218.csv")
# Restrict the dataset only to civic activity (and id)
D = D[, c(1, which(names(D)=="socact1"):which(names(D)=="socact14"))]

Diff = apply(D[,2:15], 2, mean, na.rm=T) # Difficulty levels