### This script describes and replicates a scaling procedure
### that has been used to scale data on social/civic activity
### variables: from socact1 to socact14

D = read.csv("Dane/DataInd218.csv")
# Restrict the dataset only to civic activity (and id)
D = D[, c(1, which(names(D)=="socact1"):which(names(D)=="socact14"))]