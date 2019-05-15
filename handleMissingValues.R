handleMissingValues <- function(data) {
  nonNAIndex=which(!is.na(data))
  sumofNonNa=sum(data[nonNAIndex])
  meanofNonNa=sumofNonNa/length(nonNAIndex)
  NAIndex=which(is.na(data))
  data[NAIndex]<-meanofNonNa
  return(as.integer(data)) 
}
