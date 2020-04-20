county <- read.csv(file.choose()) 
str(county)

#Converting all non-numeric variables into numeric, for establishing correlation matrix
c1 = county
c1$State = as.numeric(c1$State)
c1$County = as.numeric(c1$County)
str(c1)
View(c1)
c1.cor=cor(c1)
c1.cor
print(c1.cor[,18])

### Q1. Explain what correlations your analysis is showing and which two or three variables appear to be correlated to one another 
### AND to the target variable in AT LEAST two sentences.
# Here we can observe that Poverty has a high correlation coefficient with other variables(~0.730)
# Income, Unemployment and IncomePerCap also have strong correlations among each other (~ +/- 0.750)
# Thus, as per our correlation table if Poverty is the dependent variable,
# then, Income, Unemployment and Poverty are independent variables having strong correlations with the dependent variable.

### Q2. Set up up a best subset regression table and check how many of the variables from your calculation above should be combined
### in a linear regression to produce an optimal model.
library(leaps)
df1 <- county[,c(18,14,37)]
x=df1[,2:3] # the predictor variables
y=df1[,1]   # the target variable (Poverty)
out=summary(regsubsets(x,y,nbest=2,nvmax=ncol(X)))
tab=cbind(out$which,out$rsq,out$adjr2)
tab
# The table shows us that if we run a basic linear regression with the variables Income and Unemployment then we get a
# maximum fidelity of 74% which is good enough.

### Q3. Build and run your multiple regression model and test its quality with leave-one-out cross-validation.
PIUmod=lm(poverty~income+unemployment,data=c2)
summary(PIUmod)
# Testing the quality of the model with leave one-out cross validation
c2 = county
diff=dim(n)
percdiff=dim(n)
for(k in 1:n) {
  train1=c(1:n)
  train=train1[train1!=k]
  regIU=lm(poverty~income+unemployment, data=c2[train,])
  predIU=predict(regIU,newdat=c2[-train,])
  obs=c2$Poverty[-train]
  diff[k]=obs-predIU
  percdiff[k]=abs(diff[k])/obs
}
me=mean(diff)
rmse=sqrt(mean(diff**2))
mape=100*(mean(percdiff))
me   # mean error
rmse # root mean square error
mape # mean absolute percent error
# The above values shows that both the test set and training set differ when tested with our model. 
# The mean error and root mean square error are within acceptable limits, but the mean absolute percent error is high,
# this shows that the model is not the best fit for our chosen dataset.

### Q4. Would a logistic regression model work for your dataset? How would you have to transform your target variable to set it up?
### Would you have to add an extra binary attribute to your dataset?
# A logistic regression model would work for the given dataset.
# For this model to be applied successfully the target variable should be converted into binary type. If this is done,
# It is not necessary to add another binary attribute.

### Q5. Try setting up a logistic model. If you get error messages, research them, include them, 
### and explain why you are getting these errors and what you have done to address them.

# Setting up a logistic model
# Finding standard deviation to each variable in the dataset.
sapply(df1,sd)
# Setting up a two-way contigency table
df1$newPov <-  0+(df1$Poverty>8.3)
df1
xtabs(~newPov, data = df1)

xtabs(~newPov + income, data = df1)
county_logist <- glm(newPov ~ income+unemployment , data =df1, family = "binomial")
summary(county_logist)
# This shows that for every 1 unit change in income, the likelihood of poverty in a county decreases by 0.0001320
# This also shows that for every 1 unit change in unemployment, the likelihood of poverty in a county increases by 0.5077

# Obtaining confidence intervals for the coefficient estimates
# CI's using profile log-likelihood
confint(county_logist)
# CI's using standard errors
confint.default(county_logist)
# This shows that if the same population is sampled on numerous occassions,
# and interval estimates are made, the resulting intervals would
# bracket the true population in approx 97.5% of the cases.

