# Helper function that computes sum or fraction of NAs for every variable in a dataset
numNA <- function(x, frac=FALSE) { 
      if(frac) return(mean(is.na(x)))
      else return(sum(is.na(x)) )
}