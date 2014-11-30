# This script deals with the problem of these missing values
# that can not be replaced using simple logcial imputation.
# The problem is solved using a technique called MICE - Multivariate Imputation by Chained Equation (Buuren, Groothuis-Oudshoorn, 2011)
# It is a state-of-the-art technique that allows for multiple imputations
# which is relatively assumption-free.
# It takes into the account the nature of missing data and the measurement scale and the type of a variable and proceed iteratively yielding an optimal solution.
# Moreover it allows computing uncertainity measure for each imputed value,
# because it generates several parallel imputation schemes.

library(mice)
library(dplyr)
library(lattice)
source("MissImpute/ImputeHelper.R")
source("GenCompFuncs/ComputingMisc.R")

D = read.csv("Dane/LimDatInd231.csv")
NAs = apply(D, 2, numNA)
print(NAs) # number of NAs for each variable

# For almost every variable there is realtively few NAs, what suggests that the missing values are due to some purely random process
# (such as overlooking an answer field in the questionnaire)
# Only variables related to tv stations and tv programmes (and press) have more NAs.
# Most certainly it is due to the fact, that quite many respondents do not watch tv at all or do it very rarely (or do not read press).
# Therefore some of them did not give any answers, while some did it any way.
# Moreover, in many cases respondents with 0 tv wathing time gave answeres to some tv questions and to some not.
# Moreover, some of the NAs are due to the lack of knowledge of a tv station (or a press title)
# This is problematic, since there is no simple logical rule that could be useful in resolving this problem. Thus, it seems that best way to cope with it is to apply
# some adavanced imputation technique such as MICE.
# The following command shows the pattern of NAs among respondent with 0's on tvtime:
D[!is.na(D$tvtime) & D$tvtime==0,(which(names(D)=="tv.a1")-1):which(names(D)=="tv.b9")]

# Also income data has slightly higher fraction of NA. This is of course due to the fact that some respondents did not want to reveal their financial situation. 

# The MICE imputation process will be split into several separate procedures. Since in most of cases there are relatively few NAs, every meaningfully separate set of variables (such as attachment or social capital indicators) will have its own imputation procedure.
# Only data on cultural preferences in terms of tv, books, cinema, press and music will be imputed jointly in order to provide information that is sufficient to deal with NAs in tv section.

D = arrange(D, id) # sort the dataset by id.

# In general, MICE algorithm will be used to generate 20 parallel datasets with imputed values. This will give higher chances of getting an optimal solution and moreover will allow estimating uncertainity of every of replacement.

# MICE imputation - place attachment scales
# method of imputation - predictive mean matching (pmm)
Att = D[,which(names(D)=="attgen1"):which(names(D)=="attmult18")]
AttNAs = apply(Att, 2, numNA) # number of NAs in Att; there are very few of them
AttImp = mice(Att, m=20, seed=101, method="pmm") # seed sets up a pseudo-random numbers generator
AttImpL = actualImp(AttImp$imp)
AttImpErr = ImpOut(AttImpL, Att)[[1]] # Uncertainity of imputed values
AttImpVal = ImpOut(AttImpL, Att)[[2]] # Dominant imputed values
Att = mapImpToData(AttImpVal, Att)  # Map imputed values back to the dataset
AttRMSE = meanImpRMSE(AttImp, Att)
AttSE = apply(Att, 2, se, na.rm=TRUE)
#########################################
# Plot RMSE against SE and entropy bars
#########################################
# Setting up a plot
par(mfrow=c(2,1))
barplot(AttImpErr[,3], ylim=c(0,1), col="gray15", las=3, ylab="Entropia względna")
# Here the RMSE / SE plot begins
plot(AttSE, type="l", ylim=c(0,.15), xlim=c(0,30), ylab="SE / RMSE", xlab="", xaxt="n", lwd=2, lty=1)
axis(1, at=1:length(AttSE), labels=names(AttSE), las=3)
abline(v=9.5, lty=2)
lines(AttRMSE, lwd=2, lty=2)
text(4,.14, "Przywiązanie - jedenwymiar", cex=.7)
text(20,.14, "Przywiązanie - 3 wymiary", cex=.7)
legend("topright", c("SE", "RMSE"), lty=c(1,2), cex=.7)
# Here the plot ends
par(mfrow=c(1,1))
##################################################

# MICE imputation - social capital scales (knowing and getting help - Resource Generator)
# Method: predictive mean meathing (pmm)
SC = D[,which(names(D)=="scknow1"):which(names(D)=="schelp15")]
SCNAs = apply(SC, 2, numNA) # number of NAs in SC; there are very few of them
SCImp = mice(SC, m=20, seed=1717, method="pmm")
SCImpL = actualImp(SCImp$imp)
SCImpErr = ImpOut(SCImpL, SC)[[1]] # Uncertainity of imputed values
SCImpVal = ImpOut(SCImpL, SC)[[2]] # Dominant imputed values
SC = mapImpToData(SCImpVal, SC)  # Map imputed values back to the dataset
SCRMSE = meanImpRMSE(SCImp, SC)
SCSE = apply(SC, 2, se, na.rm=TRUE)
#########################################
# Plot RMSE against SE and entropy bars
#########################################
# Setting up a plot
par(mfrow=c(2,1))
barplot(SCImpErr[,3], ylim=c(0,1), col="gray15", las=3, ylab="Entropia względna")
# Here the RMSE / SE plot begins
plot(SCSE, type="l", ylim=c(0,.15), xlim=c(0,length(SCSE)), ylab="SE / RMSE", xlab="", xaxt="n", lwd=2, lty=1)
axis(1, at=1:length(SCSE), labels=names(SCSE), las=3)
abline(v=15.5, lty=2)
lines(SCRMSE, lwd=2, lty=2)
text(8,.14, "Kapitał - znajomość", cex=.7)
text(22,.14, "Kapitał - pomoc", cex=.7)
legend("topright", c("SE", "RMSE"), lty=c(1,2), cex=.7)
# Here the plot ends
par(mfrow=c(1,1))
##################################################


# MICE imputation - social / civic activity
# Method: logistic regression (since the data is binary)
Act = D[,which(names(D)=="socact1"):which(names(D)=="socact14")]
# for technical reasons the data has to be transformed into factors
# in order to use logistic regression as the imputation method
for(i in 1:dim(Act)[2]) Act[,i] = as.factor(Act[,i])
ActNAs = apply(Act, 2, numNA) # number of NAs in Act; there are very few of them
ActImp = mice(Act, m=20, seed=2515, method="logreg")
ActImpL = actualImp(ActImp$imp)
ActImpErr = ImpOut(ActImpL, Act)[[1]] # Uncertainity of imputed values
ActImpVal = ImpOut(ActImpL, Act)[[2]] # Dominant imputed values
Act = mapImpToData(ActImpVal, Act)  # Map imputed values back to the dataset
# Transfrom Act back to numeric
for(i in 1:dim(Act)[2]) Act[,i] = as.numeric(as.character(Act[,i]))
ActRMSE = meanImpRMSE(ActImp, Act, factor_imp=TRUE)
ActSE = apply(Act, 2, se, na.rm=TRUE)
#########################################
# Plot RMSE against SE and entropy bars
#########################################
# Setting up a plot
par(mfrow=c(2,1))
barplot(ActImpErr[,3], ylim=c(0,1), col="gray15", las=3, ylab="Entropia względna")
# Here the RMSE / SE plot begins
plot(ActSE, type="l", ylim=c(0,.05), xlim=c(0,length(ActSE)), ylab="SE / RMSE", xlab="", xaxt="n", lwd=2, lty=1)
axis(1, at=1:length(ActSE), labels=names(ActSE), las=3)
abline(v=15.5, lty=2)
lines(ActRMSE, lwd=2, lty=2)
text(7,.045, "Aktywność społeczna / obywatelska", cex=.7)
legend("topright", c("SE", "RMSE"), lty=c(1,2), cex=.7)
# Here the plot ends
par(mfrow=c(1,1))
##################################################

# MICE imputation for press, tv, cinema and books
# Method: predictive mean matching (pmm)
vars = c(20:37,  39:49, 51:61, 63:79, 81:94)
Cult = D[,vars]
CultNAs = apply(Cult, 2, numNA) # number of NAs in Cult;
# There is much more of them than in the previous cases
CultImp = mice(Cult, m=20, seed=9999, method="pmm")
CultImpL = actualImp(CultImp$imp)
CultImpErr = ImpOut(CultImpL, Cult)[[1]] # Uncertainity of imputed values
CultImpVal = ImpOut(CultImpL, Cult)[[2]] # Dominant imputed values
Cult = mapImpToData(CultImpVal, Cult)  # Map imputed values back to the dataset
CultRMSE = meanImpRMSE(CultImp, Cult)
CultSE = apply(Cult, 2, se, na.rm=TRUE)
#########################################
# Plot RMSE against SE and entropy bars
# - tv preferences
#########################################
# Setting up a plot
par(mfrow=c(2,1))
barplot(CultImpErr[1:18,3], ylim=c(0,1), col="gray15", las=3, ylab="Entropia względna")
# Here the RMSE / SE plot begins
plot(CultSE[1:18], type="l", ylim=c(0,.2), xlim=c(0,length(CultSE[1:18])), ylab="SE / RMSE", xlab="", xaxt="n", lwd=2, lty=1)
axis(1, at=1:length(CultSE[1:18]), labels=names(CultSE[1:18]), las=3)
lines(CultRMSE[1:18], lwd=2, lty=2)
text(8,.19, "Preferencje - TV", cex=.7)
legend("topright", c("SE", "RMSE"), lty=c(1,2), cex=.7)
# Here the plot ends
par(mfrow=c(1,1))
##################################################
# Plot RMSE against SE and entropy bars
# - Movie / cinema preferences
#########################################
# Setting up a plot
par(mfrow=c(2,1))
barplot(CultImpErr[19:26,3], ylim=c(0,1), col="gray15", las=3, ylab="Entropia względna")
# Here the RMSE / SE plot begins
plot(CultSE[19:29], type="l", ylim=c(0,.3), xlim=c(0,length(CultSE[19:29])), ylab="SE / RMSE", xlab="", xaxt="n", lwd=2, lty=1)
axis(1, at=1:length(CultSE[19:29]), labels=names(CultSE[19:29]), las=3)
lines(CultRMSE[19:29], lwd=2, lty=2)
text(5,.27, "Preferencje - filmy / kino", cex=.7)
legend("topleft", c("SE", "RMSE"), lty=c(1,2), cex=.7)
# Here the plot ends
par(mfrow=c(1,1))
##################################################
# Plot RMSE against SE and entropy bars
# - Literary / book preferences
#########################################
# Setting up a plot
par(mfrow=c(2,1))
barplot(CultImpErr[30:37,3], ylim=c(0,1), col="gray15", las=3, ylab="Entropia względna")
# Here the RMSE / SE plot begins
plot(CultSE[30:40], type="l", ylim=c(0,.3), xlim=c(0,length(CultSE[30:40])), ylab="SE / RMSE", xlab="", xaxt="n", lwd=2, lty=1)
axis(1, at=1:length(CultSE[30:40]), labels=names(CultSE[30:40]), las=3)
lines(CultRMSE[30:40], lwd=2, lty=2)
text(6,.27, "Preferencje - książki / literatura", cex=.7)
legend("topleft", c("SE", "RMSE"), lty=c(1,2), cex=.7)
# Here the plot ends
par(mfrow=c(1,1))
##################################################
# Plot RMSE against SE and entropy bars
# - Press preferences
#########################################
# Setting up a plot
par(mfrow=c(2,1))
barplot(CultImpErr[41:54,3], ylim=c(0,1), col="gray15", las=3, ylab="Entropia względna")
# Here the RMSE / SE plot begins
plot(CultSE[41:57], type="l", ylim=c(0,.3), xlim=c(0,length(CultSE[41:57])), ylab="SE / RMSE", xlab="", xaxt="n", lwd=2, lty=1)
axis(1, at=1:length(CultSE[41:57]), labels=names(CultSE[41:57]), las=3)
lines(CultRMSE[41:57], lwd=2, lty=2)
text(9,.27, "Preferencje - prasa", cex=.7)
legend("topleft", c("SE", "RMSE"), lty=c(1,2), cex=.7)
# Here the plot ends
par(mfrow=c(1,1))
##################################################
# Plot RMSE against SE and entropy bars
# - music preferences
#########################################
# Setting up a plot
par(mfrow=c(2,1))
barplot(CultImpErr[58:68,3], ylim=c(0,1), col="gray15", las=3, ylab="Entropia względna")
# Here the RMSE / SE plot begins
plot(CultSE[58:71], type="l", ylim=c(0,.3), xlim=c(0,length(CultSE[58:71])), ylab="SE / RMSE", xlab="", xaxt="n", lwd=2, lty=1)
axis(1, at=1:length(CultSE[58:71]), labels=names(CultSE[58:71]), las=3)
lines(CultRMSE[58:71], lwd=2, lty=2)
text(7,.27, "Preferencje -muzyka", cex=.7)
legend("topleft", c("SE", "RMSE"), lty=c(1,2), cex=.7)
# Here the plot ends
par(mfrow=c(1,1))
##################################################


# MICE imputation for demographics, socio-economics and other variables
# (mostly categorical)
# Method: various (depending on a variable type)
vars = c(2:19, 38, 50, 62, 80,95:108, 180:182)
Gen = D[, vars]
GenNAs = apply(Gen, 2, numNA) # number of NAs in Cult; There is not many
N = names(GenNAs[GenNAs != 0])    # variables with missing data
Gen = D[,N]
methods = c("lda", "pmm", "polr", "lda", "logreg", "pmm", "pmm", "logreg", "polr", "polr", "logreg", "polr", "polr", "polr", "polr", "polr", "pmm", "pmm", "lda", "polr", "polr", "polr", "pmm", "lda", "polr")
GenImp = mice(Gen, m=20, seed=1050, method=methods)
GenImpL = actualImp(GenImp$imp)
GenImpErr = ImpOut(GenImpL, Gen)[[1]] # Uncertainity of imputed values
GenImpVal = ImpOut(GenImpL, Gen)[[2]] # Dominant imputed values
Gen = mapImpToData(GenImpVal, Gen)  # Map imputed values back to the dataset
##################################
# plot of entropy of imputations #
##################################
barplot(GenImpErr[,3], col="gray15", ylim=c(0,1), las=3, cex.axis=.7, cex=.7)

# combine all the datasets back together
wD = D
indGen = which(names(wD) %in% names(Gen))
indCult = which(names(wD) %in% names(Cult))
indAct = which(names(wD) %in% names(Act))
indSC = which(names(wD) %in% names(SC))
indAtt = which(names(wD) %in% names(Att))
wD[, indGen] = Gen
wD[, indCult] = Cult
wD[, indAct] = Act
wD[, indSC] = SC
wD[, indAtt] = Att

# Save the workspace (in order not to repeat the computationally heavy analysis)
save.image("MissImpute/MICEimput.RData")

# Save the dataset
write.csv(wD, file="Dane/FullDatInd231.csv", row.names=FALSE)
rm(list=ls())

# This is it folks!