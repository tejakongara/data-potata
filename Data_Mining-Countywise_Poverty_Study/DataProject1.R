
# Q1: This dataset describes the demographics of all counties across the USA. The data was collected as part of the American Community Survey (ACS) by the US Census Bureau in the year 2017. The analysis of this data would be useful for governments for assessment of criteria such as public transportation, corporations and businesses (especially public transport companies/car companies etc) for business expansion purposes, among other uses.
# Q2: This dataset contains data related to each county showing total population and it’s segregation based on gender, ethnicity, voting eligibility, income, nature of employment of professionals, unemployment, poverty and transportation preferences for work commute. The total data available is that of 1955 unique counties in the United States and a population of approximately 324 million.
# Q3: County, Employed, Drive, Income are the four most important attributes in the dataset. Their importance is because of the first three attributes County, Employed and Drive are independent attributes which affect the Income attribute which is a dependent attribute. The entire data in the dataset is based around these 4 attributes. Their datatypes are: County - Factor Employed - Numeric Drive - Double Income - Numeric 

getwd()
projdata<-read.csv("./county17.csv")

is.character(projdata$County)
is.factor(projdata$County)

is.numeric(projdata$Income)
is.numeric(projdata$Employed)

is.numeric(projdata$drive)
is.double(projdata$Drive)

projdata$County = as.numeric(projdata$County)
projdata$Drive = as.numeric(projdata$Drive)
is.numeric(projdata$County)

# Question 4********
# Finding mean, median, mode, standard deviation and IQR for attribute Employed
mean(projdata$Employed)
median(projdata$Employed)
mode(projdata$Employed)
sd(projdata$Employed)
IQR(projdata$Employed)

# Finding mean, median, mode, standard deviation and IQR for attribute County 
mean(projdata$County)
median(projdata$County)
mode(projdata$County)
sd(projdata$County)
IQR(projdata$County)

# These measurements are important for understanding if the values of the attribute are accurate or if they have outliers which may be corrupting the overall calculations. 

# Question 5********
# Considering Income as the most important dependent attribute
# Considering independent attributes 'Employed' and 'Drive' to calculate distance with Income
ProjDist1<-data.frame(projdata$Employed[1:10], projdata$Income[1:10])
ProjDist2<-data.frame(projdata$Drive[1:10], projdata$Income[1:10])

# Calculating Manhattan distance between attributes
dist(ProjDist1, method = 'manhattan')
dist(ProjDist2, method = 'manhattan')

# Calculating Euclidean distance between attributes
dist(ProjDist1, method = 'euclidean')
dist(ProjDist2, method = 'euclidean')

# Here we can clearly see that the distance between attributes Drive and Income is less.
# This means that the independent attribute 'Drive' is closely related to the dependent attribute 'Income'.
# This is important since it shows that people who drive are more likely to command higher incomes than people who are only employed.


