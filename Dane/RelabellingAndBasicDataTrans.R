############################
### Labeling the dataset ###
############################
library(dplyr)
source("Dane/DataTransHelperFuncs.R")

D <- read.csv("Dane/raw178.csv")
N <- read.csv("Dane/Names.csv", header=F)
names(N) = c("id", "varname_pl", "varname_eng")

# change colnames
names(D) = as.character(N[,3])

### Separating personal variables from place variables
begin = which(names(D) == "p1")
P = D[, c(1, begin:dim(D)[2])]
D = D[,1:begin-1]
rm("begin")
write.csv(P, file="Dane/Places178.csv") # Writing place data to a separate file

# sort datasets (just in case they are sorted by id by default)
D = arrange(D, id)
N = arrange(N, id)
P = arrange(P, id)

#################
### Labelling ###
#################

levels(D$gender) = c(NA, "woman",NA,"man",NA)

### correct various answering formats for birthyear and add age variable
b = as.character(D$birth)
NAind = which(is.na(b) == 1)
short = grep("^[^19]", b)
for(i in short) {b[i] = paste("19", b[i], sep="")}
D$birth = as.numeric(b)
rm(list=c("NAind", "short", "b"))
D$age = 2014 - D$birth

# recode marital status
levels(D$marital) = c("nf_rel", "nf_rel", "nf_rel", "single", "nf_rel", "married")
D$marital = factor(D$marital, levels(D$marital)[c(2,1,3)])

# recode Warsaw-related variables
levels(D$Wwa) = c("no", "yes")
D$timeWwa = as.character(D$timeWwa) # this is recoded partially by hand
D[14, 7] = as.character(D$age[14]) # this is NA :(
D[49, 7] = "5"
b = D$timeWwa
b = gsub("[aA-zZ ąćęłńóśźżĄĆĘŁŃÓŚŹŻ,.]*", "", b)
b = as.numeric(b)
D$timeWwa = as.numeric(b)
rm("b")

# recode demographics, family, dog, occupation, education
levels(D$children) = c(NA, "no", "yes")
levels(D$dog) = c(NA, "no", "yes")
levels(D$edutime) = c(NA, "partial", "full")
levels(D$work) = c(NA, "no", "yes")
levels(D$worktime) = c("no_job", "flexible", "full", "part")
D$worktime = factor(D$worktime, levels(D$worktime)[c(1,4,2,3)])
D$edu = factor(D$edu, levels(D$edu)[c(1,2,6,3,7,5,4)])
levels(D$edu) = c(NA, rep("<=highschool+tech",3), "BA", "MA+", "MA+")

# Recoding of study programme (eduprog) is done by hand in a spreadsheet
# It had to be somewhat arbitrary - eduprog2.csv contains all decisions I made
write.csv(D[,c(1,11)], file="Dane/eduprog.csv")
# Recode and group by hand
eduprog = read.csv("Dane/eduprog2.csv")
D = arrange(D, id)
eduprog = arrange(eduprog, id)
D$eduprog = eduprog$eduprog2
rm("eduprog")
# combine art and human/lang; combine med/bio with STEM (otherwisise too little classes)
# leisure is still very small, but it seems it can't be added anywhere in any reasonable way
levels(D$eduprog)[c(1,5)] = c("human/lang", "STEM")

# Recode occupation
write.csv(D[,c(1,15)], file="Dane/occupation.csv")
# Recode and group by hand
occup = read.csv("Dane/occupation2.csv")
D = arrange(D, id)
occup = arrange(occup, id)
D$occup = occup$occup2 # My own classification according to the type of the job
D$ISCO = occup$ISCO # quasi-ISCO-08 classification
rm("occup")
# very small classes in occup - rather not very useful variable
# ISCO looks a bit better, but also not too good
# I make additional classification with much broader classes based on ISCO
D$ISCObroad = D$ISCO
levels(D$ISCObroad)[c(1,2,6,7)] = "low:phys/sale/serv/assoc_pro"
levels(D$ISCObroad)[c(2:4)] = "high:mng/pro/self"

### Parental Education ###
levels(D$fatheredu) = c(NA, "<=med", "don't_know", "<=med", "<=med", rep("high", 3), "<=med")
D$fatheredu = factor(D$fatheredu, levels(D$fatheredu)[c(2,1,3)])
levels(D$motheredu) = c(NA, rep("<=med", 3), rep("high", 3), "<=med")
levels(D$grandedu) = c(NA, "<=med", "don't_know", rep("<=med", 2), rep("high", 3), "<=med")
D$grandedu = factor(D$grandedu, levels(D$grandedu)[c(2,1,3)])

### Income, financial situation, possessions, home ownership, etc. ###
# Two variables are created - a continuous one (it assumes that more than 10 000 is just 10 000 - it happesn only for two cases)
# The second one is a typical factor
D$income.f = D$income # this is the factor one
levels(D$income.f) = c(NA, "1-2", "2-3", "3-4", "4-5", "5-6", "6-7", "7-8", "8-9", "9-10", "0-1", NA, "10+")
D$income.f = factor(D$income.f, levels(D$income.f)[c(10, 1:9, 11)])
# classes are small so I create additional factor with broader classes
D$incomeclass = D$income.f
levels(D$incomeclass) = c(rep("low", 2), rep("mid/high", 9))
# now for the continuous income indicator
D$income = as.numeric(D$income.f) - 1
levels(D$cars) = c(NA, rep("1car+", 2), "no_car", "1car+")
D$cars = factor(D$cars, levels(D$cars)[c(2,1)])
# very small classes in hometype, so I combine everything except for the apartment into one category called "other"
levels(D$hometype) = c(NA, rep("other", 3), "apartment", "other")
levels(D$homestatus) = c(NA, "other", rep("family",2), "own", rep("other",4),"other", rep("family",2), "other", rep("family",11),rep("other",2), rep("family",7), "rent", "other", "family")

levels(D$tv) = c(NA, "no_tv", "tv")
levels(D$pc) = c(NA, "no_pc", "pc") # almost no variability
levels(D$tablet) = c(NA,"no_tablet", "tablet")
levels(D$internet) = c(NA, "everyday") # no variability
levels(D$tvtime) = c(NA,0,0,.1,.25,.5,0,1,10,1.5,14,1.5,1.5,1,2,20,2.5,2,3,4,5,6,7,9,.25,0,.25,1,0)
D$tvtime = as.numeric(as.character(D$tvtime))
# From internetuse I derive only information about having a personal blog, website etc.
# and also information concerning following particular blogs / websites
b = as.character(D$internetuse)
blog = grep("Prowadzenie własnego bloga / strony", b)
b = rep(0, dim(D)[1])
b[blog] = 1
D$internetuse = as.factor(b)
levels(D$internetuse) = c("no_blog/web", "blog/web")
rm(list=c("b", "blog"))

# Check if factor levels ordering for tv.a1 to tv.a8 are same
for(i in 31:37) {print(all.equal(levels(D[,30]), levels(D[,i])))}
# Recode factor levels in tv.a1 to tv.7
# I use on of the functions from the DataTransHelperFuncs.R
D[,30:36] = varSetRecode(30:36, D, c(NA,4,6,2,5,3,7,1), c(7,3,5,1,4,2,6), TRUE)
# tv.a8 is recoded separately as it got different factor ordering by default
levels(D[,37]) = c(NA,4,6,5,3,7,1)
D[,37] = factor(D[,37], levels(D[,37])[c(6,4,1,3,2,5)])
D[,37] = as.numeric(as.character(D[,37]))

# Check if factor levels ordering for tv.b1. to tv.b9 are same
for(i in 39:46) {print(all.equal(levels(D[,38]), levels(D[,i])))}
# Recode factor levels in tv.b1 - tv.b3 and tv.b5 - tv.b9
# Not recognizing a tv station is considered NA
D[,c(38:40,42:46)] = varSetRecode(c(38:40,42:46), D, c(NA,4,6,2,NA,5,3,7,1), c(7,3,5,1,4,2,6), TRUE)
levels(D[,41]) = c(NA,4,2,NA,5,3,7,1)
D[,41] = factor(D[,41], levels(D[,41])[c(6,2,4,1,3,5)])
D[,41] = as.numeric(as.character(D[,41]))