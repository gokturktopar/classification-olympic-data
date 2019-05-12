
#read Data
source("handleMissingValues.R")
olympicdata<-read.table("athlete_events.csv", header = TRUE, sep = ",", dec = ",")
olympicdata$Height=handleMissingValues(olympicdata$Height)
#Handling missing values
olympicdata$Weight<-as.numeric(levels(olympicdata$Weight))[olympicdata$Weight]
olympicdata$Weight=handleMissingValues(olympicdata$Weight)
olympicdata$Age=handleMissingValues(olympicdata$Age)
nonMedal<-which(is.na(olympicdata$Medal))
olympicdata$Medal<-as.character(levels(olympicdata$Medal))[olympicdata$Medal]
olympicdata$Medal[nonMedal]<-"Non"
View(olympicdata)

#normalization

#continuous to categorical
must_convert<-sapply(olympicdata,is.factor)       # logical vector telling if a variable needs to be displayed as numeric
M2<-sapply(olympicdata[,must_convert],unclass)    # data.frame of all categorical variables now displayed as numeric
olympicdata<-cbind(olympicdata[,!must_convert],M2)  


