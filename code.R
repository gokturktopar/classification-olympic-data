library(ggvis) # for scatterplots 

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


#visualization
olympicdata %>% ggvis(~Sex, ~Sport, fill = ~Medal) %>% layer_points()

table(iris$Medal) 

#algorithm
library(class) # Contains the "knn" function
library(ISLR)
set.seed(498593) #Set the seed for reproducibility
selectedCol<-c("Sex","Age", "Height","Weight", "Team","NOC","Games","Year","Season","City","Sport","Event","Medal")
selectedColvector<-match(selectedCol,colnames(olympicdata))
selectedColvector<-selectedColvector[!is.na(selectedColvector)]
Smarket_subset<-olympicdata[,selectedColvector]
sm_sample <- sample(1:nrow(Smarket_subset), size=nrow(Smarket_subset)*0.75)
sm_train <- Smarket_subset[sm_sample, ] #Select the 75% of rows
sm_test <- Smarket_subset[-sm_sample, ] #Select the 25% of rows
sm_acc <- numeric() #holding variable
for(i in 1:5){
  #Apply knn with k = i
  predict <- knn(train=sm_train[,-length(selectedColvector)], test=sm_test[,-length(selectedColvector)], cl=sm_train$Medal, k=i)
  sm_acc <- c(sm_acc, mean(predict==sm_test$Medal))
}
plot(1-sm_acc, type="l", ylab="Error Rate",  xlab="K", main="Error Rate for Smarket with varying K")

