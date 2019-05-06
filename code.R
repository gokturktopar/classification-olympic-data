
#read Data
source("handleMissingValues.R")
olympicdata<-read.table("athlete_events.csv", header = TRUE, sep = ",", dec = ",")
#Handle Missin Values
olympicdata$Height=handleMissingValues(olympicdata$Height)
olympicdata$Weight<-as.numeric(levels(olympicdata$Weight))[olympicdata$Weight]
olympicdata$Weight=handleMissingValues(olympicdata$Weight)
View(olympicdata)

