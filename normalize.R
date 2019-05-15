normalize <- function(data) {
  min_d<-as.integer(min(data))
  diff<-as.integer(max(data)-min(data))
  for (i in 1:length(data)) {
    data[i]<-(data[i]-min_d)/diff
  }
  return(data) 
}
