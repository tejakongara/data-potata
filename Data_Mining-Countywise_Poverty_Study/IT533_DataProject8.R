# Sree Teja Kongara

install.packages('randomForest')
install.packages('nnet')
install.packages("neuralnet")

library(neuralnet)
library(ggplot2)
library(nnet)
library(dplyr)
library(reshape2)

county = read.csv(file.choose())
county <- na.omit(county)
county$State = as.numeric(county$State)
county$County = as.numeric(county$County)
str(county)
View(county)

summary(county$Poverty) # Determining quantiles
hist(county$Poverty) # Creating histogram

# Establishing a quick correlation table to select attributes with closest correlation to Poverty
county.cor <- cor(county, y = county$Poverty)
sort(county.cor)
# Here we see the attributes with strongest positive or negative correlation are:
# ChildPoverty, Income, IncomePerCap and Unemployment.

# Bucketizing Poverty into a categorical class atrribute
cats <- c(2,7,14,20,40,66)
catLabels <- c("1","2","3","4","5")
county$Poverty.b <- cut(county$Poverty, breaks=cats, labels=catLabels, right = FALSE, include.lowest = TRUE)
summary(county$Poverty.b)

# Creating a subset dataframe with these attributes only. Limiting tuples to 1000 for easier processing
county1 <- county[c(1:1000),c("ChildPoverty","Income","IncomePerCap","Unemployment","Poverty.b")]
# Splitting dataset into training and test data
set.seed(1234) # makes the result reproducible in repeated runs
x <- sample(2, nrow(county1), replace= TRUE, prob=c(0.67,0.34))
ctrain <- county1[x==1,]
ctest <- county1[x==2,]

### 1 - Running Neural Network Analyses on both Train and Test datasets
labels <- class.ind(as.factor(ctrain$Poverty.b))
standardizer <- function(x){(x-min(x))/(max(x)-min(x))}
ctrain[,1:4] <- lapply(ctrain[,1:4], standardizer)
ctrain
# Reviewing data
pre_process_county <- cbind(ctrain[,1:4], labels)
pre_process_county$ChildPoverty = pre_process_county$'ChildPoverty'
pre_process_county$Income = pre_process_county$'Income'
pre_process_county$IncomePerCap = pre_process_county$'IncomePerCap'
pre_process_county$Unemployment = pre_process_county$'Unemployment'
pre_process_county

countyForm <- as.formula("1+2+3 ~ ChildPoverty+Income+IncomePerCap+Unemployment")
county_NN <- neuralnet(countyForm, data=pre_process_county, hidden = c(12,11), act.fct = "tanh", linear.output = FALSE)

# Calculating accuracy of the model
county_p <-  neuralnet::compute(county_NN, pre_process_county[, 1:4])
original_values <- max.col(pre_process_county[, 5:7])
pr.nn_2 <- max.col(county_p$net.result)
print(paste("Model Accuracy: ", round(mean(pr.nn_2==original_values)*100, 2), "%.", sep = ""))

# The Model Accuracy obtained is: 79.48%

### 2 - Tuning Neural Network
countyForm <- as.formula("1+2+3 ~ ChildPoverty+Income+IncomePerCap+Unemployment")
county_NN <- neuralnet(countyForm, data=pre_process_county, hidden = c(16,12), act.fct = "tanh", linear.output = FALSE)

ctest[,1:4]<- lapply(ctest[,1:4], standardizer)
ctest

county_p <-  neuralnet::compute(county_net, ctest[,1:4])
original_valueswin <- max.col(pre_process_county[, 5:7])
pr.nn_2 <- max.col(pre_process_county$net.result)
print(paste("Model Accuracy: ", round(mean(pr.nn_2==original_values)*100, 2), "%.", sep = ""))

# The revised accuracy  obtained is : 47.25%
#  This is closer to our Random Forest accuracy of 44.80%

### 3 - Gradient Descent
#Load libraries
install.packages("highcharter")
library(dplyr)
library(highcharter)

#Scaling length variables from iris dataset.

county_demo <- county[,c("Unemployment","ChildPoverty")] %>%
  mutate(Unemployment = as.numeric(scale(Unemployment)),
         ChildPoverty = as.numeric(scale(ChildPoverty))) %>%
  select(Unemployment,ChildPoverty)

#Fit a simple linear model to compare coefficients.

regression <- lm(county_demo$Unemployment~county_demo$ChildPoverty)

coef(regression)

county_demo_reg <- county_demo

county_demo_reg$reg <- predict(regression,county_demo)

#Plot the model with highcharter

highchart() %>%
  hc_add_series(data = county_demo_reg, type = "scatter", hcaes(x = Unemployment, y = ChildPoverty), name = "Unemployment vs ChildPoverty") %>%
  hc_add_series(data = county_demo_reg, type = "line", hcaes(x = Unemployment, y = reg), name = "Linear Regression") %>%
  hc_title(text = "Linear Regression")

library(tidyr)


set.seed(135) 

#Auxiliary function:  y = mx + b

reg <- function(m,b,x)  return(m * x + b)


#Starting point
b <- runif(1)
m <- runif(1)


#Gradient descent function

gradient_desc <- function(b, m, data, learning_rate = 0.01){ # Small steps
  
  # Column names = Code easier to understand
  
  colnames(data) <- c("x","y")
  
  
  #Values for first iteration
  
  b_iter <- 0     
  m_iter <- 0
  n <- nrow(data)
  
  # Compute the gradient for Mean Squared Error function
  
  for(i in 1:n){
    
    # Partial derivative for b
    
    b_iter <- b_iter + (-2/n) * (data$y[i] - ((m * data$x[i]) + b))
    
    # Partial derivative for m
    
    m_iter <- m_iter + (-2/n) * data$x[i] * (data$y[i] - ((m * data$x[i]) + b))
    
  }
  
  
  # Move to the OPPOSITE direction of the derivative
  
  new_b <- b - (learning_rate * b_iter)
  new_m <- m - (learning_rate * m_iter)
  
  # Replace values and return
  
  new <- list(new_b,new_m)
  
  return(new)
  
}

# Store some values to make the motion plot

vect_m <- m
vect_b <- b


# Iterate to obtain better parameters

for(i in 1:1000){
  if(i %in% c(1,100,250,500)){ # I keep some values in the iteration for the plot
    vect_m <- c(vect_m,m)
    vect_b <- c(vect_b,b)
  } 
  x <- gradient_desc(b,m,iris_demo)
  b <- x[[1]]
  m <- x[[2]]
}
### Investigating our coefficients

print(paste0("m = ", m))
## [1] "m = 0.871753774273602"
print(paste0("b = ", b))
## [1] "b = 5.52239677041512e-10"
# The difference in the coefficients is minimal.

# We can see how the iterations work in the next plot:

#Computing new values

county_demo$preit    <- reg(vect_m[1],vect_b[1],county_demo$Unemployment)
county_demo$it1      <- reg(vect_m[2],vect_b[2],iris_demo$Unemployment)
county_demo$it100    <- reg(vect_m[3],vect_b[3],iris_demo$Unemployment)
county_demo$it250    <- reg(vect_m[4],vect_b[4],iris_demo$Unemployment)
county_demo$it500    <- reg(vect_m[5],vect_b[5],iris_demo$Unemployment)
county_demo$finalit  <- reg(m,b,county_demo$Unemployment)


county_gathered <- county_demo %>% gather(key = gr, value = val, preit:finalit) %>%
  select(-ChildPoverty) %>% 
  distinct()


county_start <- county_gathered %>%
  filter(gr == "preit")


county_seq <- county_gathered %>%
  group_by(Unemployment) %>%
  do(sequence = list_parse(select(., y = val)))


county_data <- left_join(county_start, county_seq)

#Motion Plot

cc2 <- highchart() %>%
  hc_add_series(data = county_data, type = "line", hcaes(x = Unemployment, y = val), name = "Gradient Descent") %>%
  hc_motion(enabled = TRUE, series = 0, startIndex = 0,
            labels = c("Iteration 1","Iteration 100","Iteration 250","Iteration 500","Final Iteration")) %>%
  hc_add_series(data = county_demo_reg, type = "scatter", hcaes(x = Unemployment, y = ChildPoverty), name = "Unemployment VS ChildPoverty") %>%
  hc_title(text = "Gradient Descent Iterations")

cc2
# Observations mentioned in attached .docx file
