# These are various computing functions of general utility

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

OLsToNAs <- function(x, k=5) {
      if(!is.numeric(x)) stop("data is not numeric")
      sigma = sd(x, na.rm=T)
      mu = mean(x, na.rm=T)
      x[abs(x-mu) > (k*sigma)] = NA
      return(x)
}