meanSort <-function(DF) {
      rDF = DF
      cols = dim(DF)[2]
      means = apply(DF, 2, mean, na.rm=TRUE)
      id = 1:cols
      ctrl = data.frame(id=id, xbar=means)
      ctrl = ctrl[order(means), ]
      for(i in 1:cols) {
            rDF[, i] = DF[, ctrl[i,1]]
            names(rDF)[i] = names(DF)[ctrl[i,1]]
      }
      return(rDF)
}