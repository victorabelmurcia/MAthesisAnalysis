# This script performs basic logical imputation
# Logical imputation is for instance imputing NA for the profession in the case of a person who does not have a job etc.
source("MissImpute/ImputeHelper.R")
source("GenCompFuncs/ComputingMisc.R")

D = read.csv("Dane/DataInd231.csv")
NAs1 = apply(D, 2, numNA)

# Check NAs for eduprog
D[is.na(D$eduprog), c("edu", "eduprog")]
D$eduprog = as.character(D$eduprog)
D[D$edu=="<=highschool+tech" & is.na(D$eduprog), "eduprog"] = "no_uni_edu"
D$eduprog = as.factor(D$eduprog)
D$eduprog = factor(D$eduprog, levels(D$eduprog)[c(4,3,2,1,5,6)])

# Check NAs for occup
D[is.na(D$occup), c("work", "occup")]
D$occup = as.character(D$occup)
D[D$work=="no" & is.na(D$occup), "occup"] = "no_job"
D$occup = as.factor(D$occup)

# Check NAs for ISCO
D[is.na(D$ISCO), c("work", "ISCO")]
D$ISCO = as.character(D$ISCO)
D[D$work=="no" & is.na(D$ISCO), "ISCO"] = "no_job"
D$ISCO = as.factor(D$ISCO)

# Check NAs for ISCObroad
D[is.na(D$ISCObroad), c("work", "ISCObroad")]
D$ISCObroad = as.character(D$ISCObroad)
D[D$work=="no" & is.na(D$ISCObroad), "ISCObroad"] = "no_job"
D$ISCObroad = as.factor(D$ISCObroad)
D$ISCObroad = factor(D$ISCObroad, levels(D$ISCObroad)[c(3,2,1)])

# NAs for Training and sport
D[D$trainfreq=="never", c("trainfreq", "trainfav")]
D[D$trainfreq=="never", "trainfav"] = NA
D[is.na(D$trainfav), c("trainfreq", "trainfav")]
D$trainfav = as.character(D$trainfav)
D[D$trainfreq=="never" & is.na(D$trainfav), "trainfav"] = "no_sport"
D$trainfav = as.factor(D$trainfav)
D$trainfav = factor(D$trainfav, levels(D$trainfav)[c(2,1,3,4)])

### Additionally very extreme values in book12 and movie30 are transformed into NAs
# Get rid of extreme values in book12 and movie30
D$movie30 = OLsToNAs(D$movie30, k=7)
D$book12 = OLsToNAs(D$movie30, k=7)

NAs2 = apply(D, 2, numNA)

write.csv(D, file="Dane/DatIndIm231.csv", row.names=FALSE)
rm(list=ls())

# This is it folks!