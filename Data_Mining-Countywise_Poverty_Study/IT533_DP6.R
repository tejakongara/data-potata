install.packages("car")
install.packages("mlbench")
install.packages("mboost")
install.packages("textir")
install.packages("class")
install.packages("e1071")

county=read.csv(file.choose())
county<-na.omit(county)
str(county)
county # Here we observe that the dataset consists of 3220 rows.
county$State = as.numeric(county$State)
county$County = as.numeric(county$County)
par(mfrow=c(1,3), mai=c(.3,.6,.1,.1))
plot(income~poverty,data=county,col=c(grey(.2),2:6))
plot(unemployment~poverty,data=county,col=c(grey(.2),2:6))
plot(drive~poverty,data=county,col=c(grey(.2),2:6))
#Using nt=3120 training cases for finding the nearest neighbors for
#the remaining 100 cases.
n=length(county$Poverty)
nt=3120
set.seed(1)
train<-sample(1:n,nt)
#Using Normalization to standardize the data
x=county[,c(4,3)]
x[,1]=(x[,1]-mean(x[,1]))/sd(x[,1])
x[,2]=(x[,2]-mean(x[,2]))/sd(x[,2])
x[1:3,]
# Applying kNN Algorithm
library(class)
nearest1 <- knn(train=x[train,],test=x[-train,],cl=county$Poverty[train],k=1)
nearest5 <- knn(train=x[train,],test=x[-train,],cl=county$Poverty[train],k=5)
data.frame(county$Poverty[-train],nearest1,nearest5)
# Plotting to study the variations
par(mfrow=c(1,2))
#plot for k=1
plot(x[train,],col=county$poverty[train],cex=.8,main="1-nearest neighbor")
points(x[-train,],bg=nearest1,pch=21,col=grey(.9),cex=1.25)
#plot for k=5
plot(x[train,],col=county$poverty[train],cex=.8,main="5-nearest neighbor")
points(x[-train,],bg=nearest5,pch=21,col=grey(.9),cex=1.25)
# Calculating proportions of correct classifications on training set
pcorrn1=100*sum(county$Poverty[-train]==nearest1)/(n-nt)
pcorrn5=100*sum(county$Poverty[-train]==nearest5)/(n-nt)
pcorrn1
pcorrn5
#cross-validation(leave one out)
pcorr=dim(10)
for (k in 1:10) {
  pred=knn.cv(x,county$Poverty,k)
  pcorr[k]=100*sum(county$Poverty==pred)/n
}
pcorr

###Naive Bayes Algorithm
library(mlbench)
#Loading the dataset
county1=read.csv(file.choose())
#Dividing the dataset into test and training sets
county1[sapply(county1, is.numeric)]<- lapply(county1[sapply(county1, is.numeric)], as.factor)
#Plotting barplots
plot(as.factor(county1[,18]))
title(main="County Data", xlab="Poverty", ylab = "factors")
county1[,"train"]<-ifelse(runif(nrow(county1))<0.80,1,0)
county1$train = as.factor(county1$train)
str(county1)
#Determining the column number of train/test indicator
trainCol<-grep('train',names(county1))
#Separating the test and training sets and modeling the data
trainC1<-county1[county1$train==1,-trainCol]
testC1<-county1[county1$train==1,-trainCol]
str(county1)
## Implementing the Naive Bayes Algorithm
library(e1071)
nb1 <-naiveBayes(poverty~.,data = trainC1)
nb1
summary(nb1)
str(nb1)
nb1_test_predict<-predict(nb1,testC1[,-18])
# Building the confusion matrix
table(pred=nb1_test_predict,true=testC1$Poverty)

### Explanations given in attached word document