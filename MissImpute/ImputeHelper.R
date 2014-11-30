# Helper function that computes sum or fraction of NAs for every variable in a dataset
numNA <- function(x, frac=FALSE) { 
      if(frac) return(mean(is.na(x)))
      else return(sum(is.na(x)) )
}

# Helper function that get rids of imputation lists of variables with no NA
actualImp <- function(L) {
      List = list()
      len = length(L)
      for(i in 1:len) {
            if(!is.null(L[[i]])) List = append(List, L[i])
      }
      return(List)
}

# Dominant value of a vector
domin <- function(x) {
      Tab = table(x)
      mode = 0
      uni = 0
      if(is.numeric(x)) {
            uni = sort(unique(x))
            mode = which(Tab==max(Tab))[[1]]
            mode = uni[mode]
            return(mode)
      }
      else {
            mode = which(Tab==max(Tab))[[1]]
            return(names(Tab[mode]))
      }
}

# Helper function that computes uncertainity (relative entropy) of every imputation
# and dominant impute values for every NA
ImpOut <- function(L, D) {
      # Inner helper function that computes relative entropy
      relEnt <- function(x, k) {
            probs = as.numeric(prop.table(table(x)))
            H = -sum(probs*log(probs,2))
            return(H / log(k, 2))
      }
      # Inner helper function that makes a data.frame for storing error estimates
      makeDF <- function(N, colnam) {
            DF = matrix(nrow=length(N), ncol=length(colnam), dimnames=list(N,colnam))
            return(DF)
      }
      N = names(L)
      colnam = c("NAs", "MinEnt", "AvgEnt", "MaxEnt")
      errDF = makeDF(N, colnam)
      for(n in N) {
            l = L[[n]]
            k = unique(D[,n])
            k = length(k[!is.na(k)])
            errDF[n,1] = length(rownames(l))
            Ent = apply(l, 1, relEnt, k)
            errDF[n,2:4] = c(min(Ent), mean(Ent), max(Ent))
      }
      rownam = vector(mode="character", length=0)
      for(i in 1:dim(errDF)[1]) {
            rownam = append(rownam, rep(rownames(errDF)[i], errDF[i,1]))
      }
      colnam = c("id", "imp_val")
      impDF = makeDF(rownam, colnam)
      for(n in N) {
            obs = which(rownames(impDF) == n)
            impDF[obs,1] = as.numeric(rownames(L[[n]]))
            impDF[obs,2] = apply(L[[n]], 1, domin)
      }
      output = list(errDF, impDF)
      names(output) = c("errDF", "impDF")
      return(output)
}

# Map imputed values back to the dataset
mapImpToData <- function(impDF, D) {
      for(i in 1:dim(impDF)[1]) {
            var = rownames(impDF)[i]
            id = impDF[i,1]
            imp_val = impDF[i,2]
            D[id, var] = imp_val
      }
      return(D)
}