
library(lattice)
library(caret)
#(ggvis)
#set working dir
setwd("C:/Users/pico/Desktop/gokturk/4.sınıf/machineLearning/proje/120-years-of-olympic-history-athletes-and-results (1)")

#read Data
source("handleMissingValues.R")
source("normalize.R")
olympicdata<-read.table("athlete_events.csv", header = TRUE, sep = ",", dec = ",")
olympicdata$Height=handleMissingValues(olympicdata$Height)
#Handling missing values
olympicdata$Weight<-as.numeric(levels(olympicdata$Weight))[olympicdata$Weight]
olympicdata$Weight=handleMissingValues(olympicdata$Weight)
olympicdata$Age=handleMissingValues(olympicdata$Age)
nonMedal<-which(is.na(olympicdata$Medal))
olympicdata$Medal<-as.character(levels(olympicdata$Medal))[olympicdata$Medal]
olympicdata$Medal[nonMedal]<-"Non"


#categorical to numeric
must_convert<-sapply(olympicdata,is.factor)       # logical vector telling if a variable needs to be displayed as numeric
M2<-sapply(olympicdata[,must_convert],unclass)    # data.frame of all categorical variables now displayed as numeric
olympicdata<-cbind(olympicdata[,!must_convert],M2)  
olympicdata<-olympicdata[,c(2,3,4,5,15,8,9,10,11,12,13,14,6)]




#save dataframe
library(gridExtra)
outdata <- subset(olympicdata, subset=ID<10)
png("datarafme.png", height = 50*nrow(outdata), width = 200*ncol(outdata))
grid.table(outdata)
dev.off()
dd<-olympicdata[1:20,]

#pdf("test.pdf", height=11, width=10)
outdata <-  count(olympicdata$Sport)
png("sportypes.png")
grid.table(outdata$x)
dev.off()


#visualization data
library(ggvis)
library(plyr)
count(olympicdata$Sport)$x
count(olympicdata$Sex=="M")
mean(olympicdata[olympicdata$Sex=="M",]$Age)
mean(olympicdata[olympicdata$Sex=="F",]$Age)
summary(olympicdata)
#Number of male and female Olympians over age
df2 <- count(olympicdata, c('Sex','Age'))
ggplot(df2, aes(x=Age, y=freq, group=Sex, color=Sex)) +
geom_point(size=2) +
geom_line(size=0.8)  +
scale_color_manual(values=c("#DF0024","#009F3D")) +
labs(title = "Number of Male and Female Athletes over Age",y="Athlete") +

theme(plot.title = element_text(hjust = 5),panel.border = element_blank(),

panel.background = element_rect(fill = "#cee1e8", color = NA),
# Change axis line
axis.line = element_line(colour = "black"))


#Number of male and female Olympians over year
df2 <- count(olympicdata, c('Sex','Year'))
ggplot(df2, aes(x=Year, y=freq, group=Sex, color=Sex)) +
geom_point(size=2) +
geom_line(size=0.8)  +
scale_color_manual(values=c("#DF0024","#009F3D")) +
labs(title = "Number of Male and Female Athletes over Year",y="Athlete") +

theme(plot.title = element_text(hjust = 5),panel.border = element_blank(),

panel.background = element_rect(fill = "#cee1e8", color = NA),
# Change axis line
axis.line = element_line(colour = "black"))

#Number of male and female Olympians over medal
df2 <- count(olympicdata, c('Sex','Medal'))
df2 %>% ggvis(~freq, ~Medal, fill = ~Sex) %>% layer_points()

#Number of medal Olympians over sports
#gold medal
df2 <- count(olympicdata, c('Sport','Medal'))
df2gold<-subset(df2,Medal=="Gold")
df2gold<-df2gold[order(-df2gold$freq),]
ggplot(df2gold[1:10,], aes(x=Sport,y=freq)) + geom_bar(stat = "identity",fill = "#FFD700")
#silver
df2 <- count(olympicdata, c('Sport','Medal'))
df2silver<-subset(df2,Medal=="Silver")
df2silver<-df2silver[order(-df2silver$freq),]
ggplot(df2silver[1:10,], aes(x=Sport,y=freq)) + geom_bar(stat = "identity",fill = "#C0C0C0")
#bronze
df2 <- count(olympicdata, c('Sport','Medal'))
df2bronze<-subset(df2,Medal=="Silver")
v<-df2gold[order(-df2bronze$freq),]
ggplot(df2bronze[1:10,], aes(x=Sport,y=freq)) + geom_bar(stat = "identity",fill = "#A77044")

#Number of medal Olympians over Countries
#Most golden medal countries
df2 <- count(olympicdata, c('NOC','Medal'))
df2gold<-subset(df2,Medal=="Gold")
df2gold<-df2gold[order(-df2gold$freq),]
ggplot(df2gold[1:10,], aes(x=NOC,y=freq)) + geom_bar(stat = "identity",fill = "#FFD700")

#number of having medal base on  ages
df2 <- count(olympicdata, c('Age','Medal'))

df2age<-df2[order(-df2$freq),]
ggplot(df2age[1:10,], aes(x=Age,y=freq)) + geom_bar(stat = "identity",fill = "#0085C7")



#normalization

forn<-ncol(olympicdata)-1
  for (i in 1:forn) {
  
  olympicdata[,i]<-normalize(olympicdata[,i])
  
  }




#algorithm
library(class) # Contains the "knn" function
library(ISLR)
set.seed(498593) #Set the seed for reproducibility
 #  selectedCol<-c("Sex","Age", "Height","Weight", "Team","NOC","Games","Year","Season","City","Sport","Event","Medal")
    #selectedCol<-c( "Sex","Age", "Height","Weight","NOC","Season","Sport","Event","Medal")
    selectedCol<-c("Sex","Age", "Height","Weight","NOC","Sport","Event","Medal")
   # selectedCol<-c("Name","Sex","Age", "Height","Weight", "Team","NOC","Games","Year","Season","City","Sport","Event","Medal")
                                
selectedColvector<-match(selectedCol,colnames(olympicdata))
selectedColvector<-selectedColvector[!is.na(selectedColvector)]
Smarket_subset<-olympicdata[,selectedColvector]
sm_sample <- sample(1:nrow(Smarket_subset), size=nrow(Smarket_subset)*0.75)
sm_train <- Smarket_subset[sm_sample, ] #Select the 75% of rows
sm_test <- Smarket_subset[-sm_sample, ] #Select the 25% of rows
sm_acc <- numeric() #holding variable
#time
  g <- rnorm(100000)
  h <- rep(NA, 100000)
  # Start the clock!
  ptm <- proc.time()
  
  for(i in 1:10){
    #Apply knn with k = i
    predict <- knn(train=sm_train[,-length(selectedColvector)], test=sm_test[,-length(selectedColvector)], cl=sm_train$Medal, k=i)
    sm_acc <- c(sm_acc, mean(predict==sm_test$Medal))
  }
  
  # Stop the clock
  proc.time() - ptm

plot(1-sm_acc, type="l", ylab="Error Rate",  xlab="K", main="Error Rate for Olympic Data with Varying K")

selectedK<-which(max(sm_acc)==sm_acc)
minErrorRate<-sm_acc[selectedK]
selectedK
minErrorRate
predict <- knn(train=sm_train[,-length(selectedColvector)], test=sm_test[,-length(selectedColvector)], cl=sm_train$Medal, k=10)

table(predict,sm_test$Medal)
prop.table(table(predict,sm_test$Medal))
ftrain<-factor(sm_test$Medal)
confusionMatrix(predict,ftrain,mode = "prec_recall")


#precision and recall calculation
tbl_2_1 <- table(predict,ftrain)
precision(tbl_2_1,na_rm=FALSE)
recall(tbl_2_1,na_rm=FALSE)
F_meas(tbl_2_1)


#cross validation
trial_sum <- numeric(20)
trial_n <- numeric(20)
#time
g <- rnorm(100000)
h <- rep(NA, 100000)
# Start the clock!
ptm <- proc.time()
  for(i in 1:10){
  
  ir_sample <- sample(1:nrow(olympicdata), size=nrow(olympicdata)*0.7)
  ir_train <- olympicdata[ir_sample,]
  ir_test <- olympicdata[-ir_sample,]
  test_size <- nrow(ir_test)
  
    for(j in 1:5){
    x<-length(olympicdata)
    # predict <- knn(train=ir_train[,-length(selectedColvector)], test=ir_test[,-length(selectedColvector)], cl=sm_train$Medal, k=j)
    predict <- knn(ir_train[,-x], ir_test[,-x], ir_train$Medal, k=j)
    trial_sum[j] <- trial_sum[j] + sum(predict==ir_test$Medal)
    trial_n[j] <- trial_n[j] + test_size
    }
  }  

# Stop the clock
proc.time() - ptm
plot(1-trial_sum[1:5] / trial_n[1:5], type="l", ylab="Error Rate",xlab="K",main="Error Rate for Olympic With Varying K (10 Samples)")
trial_sum[1:5] / trial_n[1:5]
