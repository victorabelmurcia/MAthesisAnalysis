D <- read.csv("Dane/raw150.csv")    # loading raw dataset (150 obs.)
N <- names(D)     # Column names vector
N = data.frame(Name1 = N)

write.csv(N, file="Dane/Names.csv")      # Writing a file to modify col names by hand
rm(list=ls())