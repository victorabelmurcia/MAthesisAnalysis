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
Zeros = apply(D, 1, sum) # very few respondents with no variations in answers

# Check latent monotonicity
mono = check.monotonicity(D) # no violations of monotonicity

# Nonintersection
# Pmatrix method
pmat = check.pmatrix(D)
# Restscore method
rest = check.restscore(D)

# Invariant Item Ordering
iio = check.iio(D, method="MIIO")

# all three methods indicate that socact14 should be excluded from the scale

# Scale
H = coefH(D)
Z = coefZ(D)
# automated item selection
scale = data.frame(scale.h = aisp(D), scale.ga = aisp(D, search="ga"))
names(scale) = c("scale.h", "scale.ga")
# Genetic algorithm also shows that socact14 is not a part of the dimension

# Scaling without socact14
D = D[, -which(names(D)=="socact14")]

# latent monotonicity
mono = check.monotonicity(D) # Latent monotonicity holds without exceptions

# nonintersection
# Pmatrix
pmat = check.pmatrix(D)
# restscore
rest = check.restscore(D)
# Intersection is only slightly violated

# invariant item ordering
iio = check.iio(D) # no significant violations
# HT (> 0.5) indicates strong scale

# scaling
H = coefH(D) # 95% CI for H is [0.3342 - 0.5498] - so the scale is relatively good
scale = data.frame(aisp(D), aisp(D, search="ga"))
names(scale) = c("scale.h", "scale.ga") # both algorithms yield the same solution
# The solution is purely unidimensional

# reliability
rel = check.reliability(D, LCRC=TRUE) # good reliability measures

# So the final scale of social / civic activity is composed of socact1 to socact13
civic = apply(D, 1, sum)
D = D.back
D$civic = civic

# Save the dataset
write.csv(D, file="Dane/FullDatInd231.csv", row.names=FALSE)


