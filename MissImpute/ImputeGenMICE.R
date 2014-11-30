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
source("MissImpute/ImputeHelper.R")

D = read.csv("Dane/LimDatInd218.csv")
NAs = apply(D, 2, numNA)
print(NAs) # number of NAs for each variable

# For almost every variable there is realtively few NAs, what suggests that the missing values are due to some purely random process
# (such as overlooking an answer field in the questionnaire)
# Only variables related to tv stations and tv programmes have more NAs.
# Most certainly it is due to the fact, that quite many respondents do not watch tv at all or do it very rarely.
# Therefore some of them did not give any answers, while some did it any way.
# Moreover, in many cases respondents with 0 tv wathing time gave answeres to some tv questions and to some not.
# This is problematic, since there is no simple logical rule that could be useful in resolving this problem. Thus, it seems that best way to cope with it is to apply
# some adavanced imputation technique such as MICE.
# The following command shows the pattern of NAs among respondent with 0's on tvtime:
D[!is.na(D$tvtime) & D$tvtime==0,(which(names(D)=="tv.a1")-1):which(names(D)=="tv.b9")]

# Also income data has slightly higher fraction of NA. This is of course due to the fact that some respondents did not want to reveal their financial situation. 

# The MICE imputation process will be split into several separate procedures. Since in most of cases there are relatively few NAs, every meaningfully separate set of variables (such as attachment or social capital indicators) will have its own imputation procedure.
# Only data on cultural preferences in terms of tv, books, cinema, press and music will be imputed jointly in order to provide information that is sufficient to deal with NAs in tv section.

D = arrange(D, id) # sort the dataset by id.

# In general, MICE algorithm will be used to generate 10 parallel datasets with imputed values. This will give higher chances of getting an optimal solution and moreover will allow estimating uncertainity of every of replacement.

# MICE imputation - place attachment scales
Att = D[,which(names(D)=="attgen1"):which(names(D)=="attmult18")]
AttNAs = apply(Att, 2, numNA) # number of NAs in Att; there are very few of them
AttImp = mice(Att, m=10, seed=101)

