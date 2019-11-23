# install and load packages
install.packages("MASS")
install.packages("ggplot2")
library(MASS)
library(caTools)
library(ggplot2)

# set working directory
setwd("~/Desktop")

# import data 
fb_data <- read.csv('bigsky2019.csv')
fb_data <- fb_data[,3:15]

# view data
fb_data

wins_mean <- mean(fb_data$Wins)

# split data into training and testing 
split <- sample.split(fb_data$PPG, SplitRatio = 0.8)
training_set <- subset(fb_data, split == TRUE)
test_set <- subset(fb_data, split == FALSE)

# create linear model
fb_model <- lm(formula = Wins ~ PPG + PPG_Against + Rushing_Offense + Rushing_defense +
                 Passing_Offense + Passing_Defense, data = fb_data)

# view accuracy of model
summary(fb_model)

summary(fb_model)$coefficient

'p-value is .004123
Rushing_offense, Rushing_defend, Passing_Offense and Passing_defense variables have very low
significance in linear regression model'

# Create alternate model
fb_model_simple <- lm(formula = Wins ~ PPG + PPG_Against, data = fb_data)

# view accuracy of model
summary(fb_model_simple)
confint(fb_model_simple)
summary(fb_model_simple)$coefficients

'Second model with less variables has significantly better p-value (2.202e-06) in comparison
to the model which includes passing and rushing statistics'

'PPG and PPG_Against variables both of high significance to model'

# calculate Residual Standard error
sigma(fb_model_simple)/wins_mean

'92% of variance can be predicted by points and points against
  p-value 2.02e-06 highly significant, meaning predictor variables significantly correlated
    to outcome of wins
  RSE 0.6564 corresponding to 13.7% rate of error'

# run linear model
wins_predictions <- predict(fb_model_simple, newdata = test_set)

plot(wins_predictions)
