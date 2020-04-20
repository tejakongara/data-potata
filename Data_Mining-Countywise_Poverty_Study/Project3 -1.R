# Sree Teja Kongara

#### Question1. For choosing attributes, a basic Cosine Similarity test was performed on the attributes Income, Drive, Poverty, Unemployment and PrivateWork. These attributes were initially chosen since a quick overview of the dataset reveals an emphasis on employment, employment type, income and preferred modes of transportation among people of these counties. Thus, the attributes with the greatest significance among these categories were chosen.
install.packages('nutshell')
install.packages('lattice')
install.packages('MASS')
install.packages('OneR') 
install.packages('coop')
library('nutshell')
library('lattice')
library('MASS')
library('OneR')
library('coop')

getwd()
county <- read.csv("/users/teja/documents/dm/county17.csv")
income = county$Income
drive = county$Drive
commute = county$MeanCommute
poverty = county$Poverty
unemployment = county$Unemployment
private = county$PrivateWork

#Cosine Similarity Test
cosine(income, drive) # Using inverse cosine calculator, gives: 17.64951574 degrees
cosine(income, poverty) # Using inverse cosine calculator, gives: 39.66113216 degrees
cosine(drive, poverty) # Using inverse cosine calculator, gives: 26.27138498 degrees
cosine(income, unemployment) # Using inverse cosine calculator, gives: 39.70201412 degrees
cosine(drive, unemployment) # Using inverse cosine calculator, gives: 29.50744537 degrees
cosine(drive, commute) # Using inverse cosine calculator, gives: 13.49583197 degrees
cosine(private,drive) # Using inverse cosine calculator, gives: 6.28078532 degrees
cosine(poverty, unemployment) # Using inverse cosine calculator, gives: 0.9396648 degrees

# The Cosine Similarity test showed the following relations - From Lowest Angle to Highest: (PrivateWork, Drive) = 6 degrees, (Drive,MeanCommute) = 13 degrees, (Poverty, Unemployment) = 20 degrees, (Income, Drive) = 17 degrees, among others.

#Correlation Test
cor(drive,income, use="all.obs",method=c("pearson"))
cor(income,poverty, use="all.obs",method=c("pearson"))
cor(drive,poverty, use="all.obs",method=c("pearson"))
cor(income,unemployment, use="all.obs",method=c("pearson"))
cor(drive,unemployment, use="all.obs",method=c("pearson"))
cor(drive,commute, use="all.obs",method=c("pearson"))
cor(private,drive, use="all.obs",method=c("pearson"))
cor(poverty,unemployment, use="all.obs",method=c("pearson"))

# A Correlation test was performed on these relationships - From Strong relationship to Weak: (Income, Poverty) = -0.7645, (Poverty,Unemployment) = -0.7308, (PrivateWork, Drive) = 0.3850, (Drive, MeanCommute) = 0.2201

#### Question2. Based on the values of both Cosine similarity and Correlation, it can be judged that the attributes Poverty and Unemployment (Cosine:20 degrees (Close) and Correlation: 0.7308 (Strong, Positive correlation)) have the closest association, followed by PrivateWork and Drive (Cosine: 6 degrees (Closest) and Correlation: 0.3850724 (Moderate, Positive correlation)).
# Therefore, Unemployment and Poverty were considered as the most closely related attributes, with closeness in terms of angle as well as a strong positive correlation, which means that an increase in unemployment levels in the counties resulted in an increase in the poverty levels.

#### Question3. Some surprises included relationships between (Income, Drive), (Drive, Poverty), (Drive, Unemployment) which were considerably weaker when compared to the relationship between Unemployment and Poverty. This resulted in going back and picking other attributes, of which the relationship between Poverty and Unemployment was found to be strongest(using both Cosine similarity and Correlation methods).

#### Question4. We can use the Correlation method to predict future values of an attribute in relation to another. For example, the attributes Unemployment and Poverty share a Strong, Positive correlation of approximately 0.7308. Using this we can predict that as Unemployment levels increase in a county, the Poverty levels also tend to increase.

plot(poverty,unemployment, main="Scatterplot poverty vs. unemployment ",xlab="Poverty",ylab="Unemployment")

# A scatterplot of attributes Poverty and Unemployment shows that as unemployment levels increase, so do the poverty levels in counties. 

