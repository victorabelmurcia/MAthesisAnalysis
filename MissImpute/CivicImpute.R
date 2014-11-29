### Imputation of missing data in the civic activity set of variables
source("Scaling/scaling-Helper.R")
library(tree)

D = read.csv("Dane/DataInd218.csv")
### Helper function
numNA <- function(x) { sum(is.na(x)) }
NAs = apply(D[,118:131], 2, numNA) # Very few NAs
# So an imputation procedure will most certainly not distort the data
# I will use imputing based on classification trees
# Other civic variables will be used as predictors
# Civic activity variables are correlated so it should be an efficient approach

D = D[,c(1,118:131)]
corMat = round(cor(D[,2:15], use="pairwise.complete.obs"),3)

