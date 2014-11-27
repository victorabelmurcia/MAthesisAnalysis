##################################################################
### This file contains helper functions for the script called: ###
###               "RelabellingAndBasicDataTrans.R              ###
##################################################################

# The following function can be used to automatically
# change and sort factor levels in a set of variables of the same structure

varSetRecode <- function(cols, data, newlevels, ordering=NULL, numeric=FALSE) {
      # cols should be integer (numeric) vector indicating columns to be recoded
      # data is the dataset of interest
      # newlevels must be a vector of new levels (character)
      # ordering is the vector of integers (numeric) indicating the new levels ordering
      # by default it does not change the ordering
      if(is.null(ordering)) ordering = 1:length(levels(data[,cols[1]]))
      N = names(data)[cols]
      varSet = data[,cols]
      for(i in 1:length(cols)) {
            col = cols[i]
            levels(varSet[,i]) = newlevels
            varSet[,i] = factor(varSet[,i], levels(varSet[,i])[ordering])
            if(numeric) {varSet[,i] = as.numeric(as.character(varSet[,i]))}
      }
      names(varSet) = N
      return(varSet)
}