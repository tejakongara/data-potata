#1. Data Transformation: Variables chosen: Poverty, MeanCommute, Unemployment

# By transforming these variables to categorical format, we make it easy to understand the poverty levels, mean commute distances and unemployment levels of counties into three simple categories. This improves the readability and reduces the processing time of the dataset. 
#Transforming variable Poverty to categorical
data1 <- read.csv("County17.csv")
is.numeric(data1$Poverty)
data1$Poverty = cut(data1$Poverty, br=c(-1,12,35,99), labels = c("low","mod","high"))
is.factor(data1$Poverty)
table(data1$Poverty)
#Transforming variable MeanCommute to categorical
is.numeric(data1$MeanCommute)
data1$MeanCommute = cut(data1$MeanCommute, br=c(-1,5,15,50), labels = c("walkable","carpoolPossible","mustDrive"))
is.factor(data1$MeanCommute)
table(data1$MeanCommute)
#Transforming variable Unemployment to categorical
is.numeric(data1$Unemployment)
data1$Unemployment = cut(data1$Unemployment, br=c(-1,7,15,35), labels = c("low","moderate","high"))
is.factor(data1$Unemployment)
table(data1$Unemployment)
###################################################################################################

#2. Data Substitution: The dataset we are using is highly curated and is therefore devoid of missing, implausible or duplicate values. However let's attempt data substitution on some values of the variable State, to improve readability.
# Note - This particular dataset has only 2 non-numerical variables (State, County). Substituting data on numeric values with other numeric values would corrupt the data, which is undesirable. This is why we have chosen three examples from the non-numerical State variable to demonstrate substitution. 

# Substituting (IN,IL,CA) values in place of (Indiana, Illinois, California) in variable State
unique(data1$State)
data1$State <-as.character(data1$State) #Converting to character datatype for easier conversion than with factor
data1$State[data1$State== "Indiana"]<-"IN"
data1$State[data1$State== "Illinois"]<-"IL"
data1$State[data1$State== "California"]<-"CA"
data1$State<-as.factor(data1$State) #Turning variable back into data type factor
unique(data1$State)
###################################################################################################

#3. Data Reduction: The dataset we are using has no duplicate variables, except for CountyID and County. Attempting to remove the variable County to make the dataset leaner and less time consuming during processing.
# Also, the variable FamilyWork has negligible values and is very similar to SelfEmployed. For brevity's sake and to make our dataset more understandable, let's remove such negligible variables.  
data1$County <-NULL
data1$FamilyWork <- NULL
unique(data1)
# Binning: Using binning to segregate the values in variable Drive, showing counties with least to highest driving populations. This enables easier analysis and helps make sense of numbers better by dividing all values of the variable Drive into categories.
drive <- data1$Drive
cut(drive,3, labels = c("low","medium","high"))
###################################################################################################

#4. Important Analytical Questions - To help make better business/organizational decisions
#a. Do higher income levels translate into greater driving populations across counties?
#b. Do higher MeanCommute values result in greater preference for driving among people?
#c. What is the correlation between Poverty levels and Unemployment levels in counties?

