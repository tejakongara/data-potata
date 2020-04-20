# Sree Teja Kongara

install.packages("tree")
install.packages("party")
install.packages("rpart")
install.packages("car")
install.packages("mlbench")
install.packages("mboost")
install.packages("textir")
install.packages("class")
install.packages("e1071")
install.packages("randomForest")
install.packages("h2o")

library(tree)
county = read.csv(file.choose())
county <- na.omit(county)
#Considering the first 100 rows due to the size of the dataset and keeping in mind previous processing errors
county1 <- head(county,100)
str(county1)
county1
## Building a decision tree using the function tree()
# Splitting the dataset into training(70%) and test(30%) subsets
X <- sample(2, nrow(county1), replace=TRUE, prob=c(0.7, 0.3))
trainXtree <- county1[X==1,]
testXtree <- county1[X==2,]

XFormula <- poverty ~ income+unemployment+drive
county_tree <- tree(XFormula, data=trainXtree)
summary(county_tree)
print(county_tree)
plot(county_tree)
text(county_tree)
# This gives us the decision tree
testPredict <- predict(county_tree, newdata1 = testXtree) 
## table(testPred, testDataTree$Species) ## error
show(testPredict)

### Setting up a Random Forest Model
# Splitting the dataset into training(70%) and test(30%) subsets
Y <- sample(2, nrow(county1), replace=TRUE, prob=c(0.7, 0.3))
trainXtree <- county1[Y==1,]
testXtree <- county1[Y==2,]

library(randomForest)
rfc <- randomForest(poverty~ income+unemployment+drive, data=trainXtree, ntree=50, proximity=TRUE)
table(predict(rfc), trainXtree$Poverty)
print(rfc)
attributes(rfc)
## After this, we plot the error rates with various other trees.
plot(rfc)
## Here importance of variables can be gauged with functions importance() and varImpPlot()
importance(rfc)
varImpPlot(rfc)
## Now the built random forest is tested with test data, and the result is then checked with functionstable() and margin().
## The margin of a data point is the proportion of votes for the correct class minus maximum proportion of votes for other classes.
## Here positive margin correlates to correct classification.
countyPred<-predict(rfc, newdata2 = testXtree)
table(countyPred, testXtree$Poverty)
plot(margin(rfc, testXtree$Poverty))

### Answers and observations in attached Word file.
