#set working dir
setwd("C:/Users/pico/Desktop/gokturk/4.sýnýf/machineLearning/proje/120-years-of-olympic-history-athletes-and-results (1)")
#read Data
source("handleMissingValues.R")
olympicdata<-read.table("athlete_events.csv", header = TRUE, sep = ",", dec = ",")
olympicdata$Height=handleMissingValues(olympicdata$Height)
View(olympicdata$Weight)
olympicdata$Weight<-as.numeric(levels(olympicdata$Weight))[olympicdata$Weight]
olympicdata$Weight=handleMissingValues(olympicdata$Weight)
View(olympicdata)
View(olympicdata$Weight)
