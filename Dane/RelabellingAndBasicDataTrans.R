############################
### Labeling the dataset ###
############################
library(dplyr)
source("Dane/DataTransHelperFuncs.R")

D <- read.csv("Dane/raw227.csv")
N <- read.csv("Dane/Names.csv", header=F)
names(N) = c("id", "varname_pl", "varname_eng")

# change colnames
names(D) = as.character(N[,3])
rm("N")

### Separating personal variables from place variables
begin = which(names(D) == "p1")
P = D[, c(1, begin:dim(D)[2])]
D = D[,1:begin-1]
rm("begin")
write.csv(P, file="Dane/Places227.csv") # Writing place data to a separate file
rm("P")

# sort datasets (just in case they are sorted by id by default)
D = arrange(D, id)

#################
### Labelling ###
#################

levels(D$gender) = c(NA, "woman",NA,"man",NA)

### correct various answering formats for birthyear and add age variable
b = as.character(D$birth)
short = grep("^[^1]", b)
for(i in short) {b[i] = paste("19", b[i], sep="")}
D$birth = as.numeric(b)
rm(list=c("short", "b"))
D$age = 2014 - D$birth

# recode marital status
levels(D$marital) = c(NA, rep("nf_rel", 3), "single", "nf_rel", "married")
D$marital = factor(D$marital, levels(D$marital)[c(2,1,3)])

# recode Warsaw-related variables
levels(D$Wwa) = c("no", "yes")
b = as.character(D$timeWwa)
b = gsub("[[1-9] miesi]", "1", b)
for(i in 1:length(b)) { if(grepl("urodzen", b[i])) b[i]=D$age[i] }
b = gsub("piąty", "5", b)
b = gsub("[aA-zZ ąćęłńóśźżĄĆĘŁŃÓŚŹŻ,.]*", "", b)
b = as.numeric(b)
D$timeWwa = b
rm("b")

# recode demographics, family, dog, occupation, education
levels(D$children) = c(NA, "no", "yes")
levels(D$dog) = c(NA, "no", "yes")
levels(D$edutime) = c(NA, "no_fl_edu", "fl_edu")
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
levels(D$eduprog)[c(1,5,7)] = c("human/lang", "STEM/med", "STEM/med")

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
levels(D$motheredu) = c(NA, "<=med", "don't_know", rep("<=med", 2), rep("high",3), "<=med")
D$motheredu = factor(D$motheredu, levels(D$motheredu)[c(2,1,3)])
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
levels(D$hometype) = c(NA, rep("other", 4), "apartment", "other")
b = as.character(D$homestatus)
rodz = grep("[rR]odzi[nc]|partne|[tT]eś|[żŻ]on|[mM][ąę]ż|[dD]iad|[bB]ab|[mM]am[ay]", b)
wlasn = grep("właś|w[lł]asn", b)
wlasn = wlasn[!wlasn %in% rodz]
wynajm = grep("[wW]ynajm", b)
b[-c(wynajm,wlasn,rodz)] = "other"
b[rodz] = "family"
b[wlasn] = "own"
b[wynajm] = "rent"
D$homestatus = as.factor(b)
rm(list=c("b", "rodz", "wynajm", "wlasn"))
D$homestatus = factor(D$homestatus, levels(D$homestatus)[c(4,3,1,2)])

levels(D$tv) = c(NA, "no_tv", "tv")
levels(D$pc) = c(NA, "no_pc", "pc") # almost no variability
levels(D$tablet) = c(NA,"no_tablet", "tablet")
levels(D$internet) = c(NA, "everyday") # no variability
b = as.character(D$tvtime)
b = gsub("[0-9] do|[0-9]-", "", b)
b = gsub("[nN]ie ogl", "0", b)
b = gsub(",", ".", b)
for(i in 1:length(b)) {
      if(grepl("[mM]inu",b[i])) {
            val = as.numeric(gsub("[[aA-zZ] ąćęłńóśźżĄĆĘŁŃÓŚŹŻ,.+-?]", "", b[i]))
            b[i] = as.character(val/60)
      }
}
b = gsub("[aA-zZ]* *", "", b)
b = gsub("-", "0", b)
b = gsub("[ąćęłńóśźżĄĆĘŁŃÓŚŹŻ+?]*", "", b)
b = as.numeric(b)
D$tvtime = b
rm(list=c("b", "val"))
# From internetuse I derive only information about having a personal blog, website etc.
# and also information concerning following particular blogs / websites
b = as.character(D$internetuse)
blog = grep("Prowadzenie własnego bloga / strony", b)
b = rep(0, dim(D)[1])
b[blog] = 1
D$internetuse = as.factor(b)
levels(D$internetuse) = c("no_blog/web", "blog/web")
rm(list=c("b", "blog"))

# TV watching related variables
# Check if factor levels ordering for tv.a1 to tv.a8 are same
for(i in 31:37) {print(all.equal(levels(D[,30]), levels(D[,i])))}
# Recode factor levels in tv.a1 to tv.7
# I use on of the functions from the DataTransHelperFuncs.R
D[,30:36] = varSetRecode(30:36, D, c(NA,4,6,2,5,3,7,1), numeric=TRUE)
# tv.a8 is recoded separately as it got different factor ordering by default
levels(D[,37]) = c(NA,4,6,5,3,7,1)
D[,37] = as.numeric(as.character(D[,37]))

# Check if factor levels ordering for tv.b1. to tv.b9 are same
for(i in 39:46) {print(all.equal(levels(D[,38]), levels(D[,i])))}
# Recode factor levels in tv.b1 - tv.b3 and tv.b5 - tv.b9
# Not recognizing a tv station is considered NA
# 999 is used in imputation
D[,c(38:40,42:46)] = varSetRecode(c(38:40,42:46), D, c(NA,4,6,2,999,5,3,7,1), numeric=TRUE)
levels(D[,41]) = c(NA,4,2,999,5,3,7,1)
D[,41] = as.numeric(as.character(D[,41]))
for(i in 38:46) { D[,i][D[,i] == 999] = mean(D[,i][D[,i] != 999], na.rm=T) }

# Movie watching related variables
levels(D$moviefreq) = c(NA,"often", "sometimes", "often", rep("rarely",2))
# Check if factor levels ordering for tv.b1. to tv.b9 are same
for(i in 49:57) {print(all.equal(levels(D[,48]), levels(D[,i])))}
# Recode factor levels in movie.a1 to movie.a10
D[,48:57] = varSetRecode(48:57, D, c(NA,4,6,2,5,3,7,1), numeric=TRUE)
b = as.character(D$movie30)
blad = grep("[0-9]{3,3}|tysi", b)
b[blad] = NA
b = gsub("[0-9][-/]", "", b)
slowne = grep("[0-9]* [fF]ilm", b)
b[slowne] = sub("[0-9]", "", b[slowne])
b = gsub("[aA-zZ ąćęłńóśźżĄĆĘŁŃÓŚŹŻ+?()-]*", "", b)
b = as.numeric(b)
D$movie30 = b
rm(list=c("b", "blad", "slowne"))

# Book reading related variables
levels(D$bookfreq) = c(NA,"often", "sometimes", "often", rep("rarely", 2))
# Check if factor levels ordering for book.a1. to book.a10 are same
for(i in 61:69) {print(all.equal(levels(D[,60]), levels(D[,i])))}
# Recode factor levels in book.a1 to book.a10
D[,60:69] = varSetRecode(60:69, D, c(NA,4,6,2,5,3,7,1), numeric=TRUE)

# Press related variables
levels(D$pressfreq) = c(NA, "often", "sometimes", "often", rep("rarely", 2))
# Check if factor levels ordering for press.a1. to press.a7 are same
for(i in 73:78) {print(all.equal(levels(D[,72]), levels(D[,i])))}
# Recode  factor levels in press.a1 to press.a7
D[,72:78] = varSetRecode(72:78, D, c(NA,4,6,2,5,3,7,1), numeric=TRUE)
# Check if factor levels ordering for press.b1. to press.b10 are same
for(i in 80:88) {print(all.equal(levels(D[,79]), levels(D[,i])))}
# Recode factor levels in press.b1 to press.b10
# value of 999 is used for imputation
D[,79:88] = varSetRecode(79:88, D, c(NA,4,6,2,999,5,3,7,1), numeric=TRUE)
for(i in 79:88) D[,i][D[,i] == 999] = mean(D[,i][D[,i] != 999], na.rm=TRUE)

# Music preferences
levels(D$musicfreq) = c(NA,"everyday", rep("not_everyday", 4))
# Check if factor levels ordering for music.a1 to music.a14 are same
for(i in 91:103) {print(all.equal(levels(D[,90]), levels(D[,i])))}
# Recode factor levels in music.a1 to music.a14
D[,90:103] = varSetRecode(90:103, D, c(NA,4,6,2,5,3,7,1), numeric=TRUE)

# Various cultural activities
levels(D$theater) = c(NA, "regularly", "rarely", rep("regularly",2), rep("rarely", 2))
levels(D$opera) = c(NA, "regularly", "never", rep("regularly", 2), rep("rarely", 2))
levels(D$cinema) = c(NA, "sometimes", "rarely", rep("regularly",2), rep("rarely", 2))
levels(D$art) = c(NA, "sometimes", "rarely", rep("regularly",2), rep("rarely", 2))
levels(D$livemusic) = c(NA,"sometimes", "rarely", rep("regularly",2), rep("rarely",2))
levels(D$club) = c(NA,"sometimes", "never", rep("regularly",2), rep("rarely",2))
levels(D$sportshow) = c(NA,"regularly", "never", rep("regularly",2), rep("rarely",2))

# Sport activities
levels(D$trainfreq) = c(NA, "regularly", "sometimes", "regularly", "sometimes", "never")
# Recode respondents' favourite sport activity
write.csv(D[,c(1,114)], file="Dane/trainfav.csv")
# Recode and group by hand
trainfav2 = read.csv("Dane/trainfav2.csv")
D = arrange(D, id)
trainfav2 = arrange(trainfav2, id)
D$trainfav = trainfav2$trainfav2
levels(D$trainfav)[1:2] = rep("rival",2)

# Amount of books, cd's and works of art in possession
b = as.character(D$bookquant)
b = gsub("[0-9]*-", "", b)
b = gsub("tysi", "1500", b)
b = gsub("set", "150", b)
b = gsub("dziesi", "50", b)
b = gsub("[aA-zZ ąćęłńóśźżĄĆĘŁŃÓŚŹŻ+?(),.><]*", "", b)
b = as.numeric(b)
b = cut(b, c(-1,99,999,Inf), labels=c("tens", "hundreds", "thousands"))

b = as.character(D$cdquant)
b = gsub("[0-9]*-", "", b)
b = gsub("set", "150", b)
b = gsub("dziesi", "50", b)
b = gsub("na[śs]ci", "15", b)
b = gsub("-", "0", b)
b = gsub("[aA-zZ ąćęłńóśźżĄĆĘŁŃÓŚŹŻ+?(),.><]*", "", b)
b = as.numeric(b)
b = cut(b, c(-1,19,99,Inf), labels=c("few", "tens", "hundreds"))

b = as.character(D$artquant)
b = gsub("[0-9]*-", "", b)
b = gsub("kilkana[śs]", "15", b)
b = gsub("kilka", "5", b)
b = gsub("[aA-zZ ąćęłńóśźżĄĆĘŁŃÓŚŹŻ+?(),.><]*", "", b)
b = as.numeric(b)
b = cut(b, c(-1,0,10,Inf), labels=c("zero", "few", "tens+"))

# Social/civic activity
# Check if factor levels ordering for socact1. to socact14 are same
for(i in 119:131) {print(all.equal(levels(D[,118]), levels(D[,i])))}
# Recode factor levels in socact1 to socact14
D[,118:131] = varSetRecode(118:131, D, c(NA,0,1), numeric=TRUE)

# Social capital - known persons (Resource Generator model)
# Check if factor levels ordering for scknow. to scknow15 are same
for(i in 133:146) {print(all.equal(levels(D[,132]), levels(D[,i])))}
# Recode factor levels in scknow1 to scknow15
D[,132:146] = varSetRecode(132:146, D, c(NA,0,1,2,4,3), numeric=TRUE)

# Social capital - getting help (Resource Generator model)
# Check if factor levels ordering for schelp1. to schelp15 are same
for(i in 148:161) {print(all.equal(levels(D[,147]), levels(D[,i])))}
# Recode factor levels in scknow1 to shelp12 and for schelp15
D[,c(147:158, 161)] = varSetRecode(c(147:158, 161), D, c(NA,0,1,2,4,3), numeric=TRUE)
# Recode factor levels in scknow13 and scknow14
D[,159:160] = varSetRecode(159:160, D, c(NA,0,2,4,3), numeric=TRUE)

# Place attachment towards Warsaw
# General unidimensional scale of place attachment
# Check if factor levels ordering for attgen1 to attgen9 are same
for(i in 163:170) {print(all.equal(levels(D[,162]), levels(D[,i])))}
# Recode factor levels in attgen1 to attgen9
D[,162:170] = varSetRecode(162:170, D, c(NA,2,4,3,1,5), numeric=TRUE)

# Multidimensional scale of place attachment
# Check if factor levels ordering for attgen1 to attgen9 are same
for(i in 172:188) {print(all.equal(levels(D[,171]), levels(D[,i])))}
# Recode factor levels in attmult1 to attmult18
D[,171:188] = varSetRecode(171:188, D, c(NA,2,4,3,1,5))

### Get rid of respondents that are either too old or do not live in Warsaw
D = D[D$age<=35, ]
D = D[D$Wwa == "yes", ]

### Save the date
write.csv(D, file="Dane/DataInd.csv")

# This is it folks!