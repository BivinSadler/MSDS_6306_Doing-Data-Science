# Import necessary libraries
library(tidyverse)

# Read in cars data set
cars <- read.csv(file.choose(),header = TRUE)

# Always important to look at the basic structure first



# I see car information: 2 factor columns, 3 num columns, and 4 int columns
summary(cars) # I see two NAs in Horsepower

# Scatter plot to inspect general trend
cars %>% ggplot(aes(x=Weight, y=MPG)) + geom_point() + ggtitle("Weight vs MPG")

# Use lm to create a linear regression model
fit <- lm(MPG~Weight, data=cars)

# Scatter plot with LR model overlay
cars %>% ggplot(aes(x=Weight, y=MPG)) + geom_point() + ggtitle("LR Model: Weight vs MPG") + geom_smooth(method = "lm")

# The long way to calculate the p-value
# Pull out intercept and slope values for later
beta_0_hat <- fit$coefficients[1]
beta_1_hat <- fit$coefficients[2]

# Pull out SE for intercept and slope
SE_beta_0_hat <- summary(fit)$coefficients[1,2]
SE_beta_1_hat <- summary(fit)$coefficients[2,2]

#Intercept
tstat <- beta_0_hat/SE_beta_0_hat #beta_0_hat / SE(beta_0_hat)
pvalue <- (1-pt(tstat,length(cars$MPG)-2)) * 2 # Mult by 2 since 2 sided test
tstat
pvalue

#Slope
tstat <- beta_1_hat/SE_beta_1_hat #beta_1_hat / SE(beta_1_hat)
pvalue <- (pt(tstat,length(cars)-2)) * 2 # Mult by 2 since 2 sided test
tstat
pvalue

# The easy way to get the p-values and confidence intervals
summary(fit)
confint(fit)


########################################## Question 2 #############################################

# LOOCV ... Explicit Coding

# Model 1

pred_error_sq <- c(0)
for(i in 1:dim(cars)[1]) {
 cars_train <- cars[-i,]
  fit <- lm(MPG ~ Weight,data = cars_train) # leave i'th observation out
  mpg_i <- predict(fit, data.frame(Weight = cars[i,6])) # predict i'th observation
  pred_error_sq <- pred_error_sq + (cars[i,2] - mpg_i)^2 # cumulate squared prediction errors
}

SSE = var(cars$MPG) * (393)

R_squared <- 1 - (pred_error_sq/SSE) # Measure for goodness of fit
R_squared

MSE = pred_error_sq / 394
MSE

RMSE = sqrt(pred_error_sq/394)
RMSE

# Model 2

pred_error_sq <- c(0)
for(i in 1:dim(cars)[1]) {
  cars_train <- cars[-i,]
  fit <- lm(MPG ~ Weight + I(Weight^2),data = cars_train) # leave i'th observation out
  mpg_i <- predict(fit, data.frame(Weight = cars[i,6])) # predict i'th observation
  pred_error_sq <- pred_error_sq + (cars[i,2] - mpg_i)^2 # cumulate squared prediction errors
}

SSE = var(cars$MPG) * (393)

R_squared <- 1 - (pred_error_sq/SSE) # Measure for goodness of fit
R_squared

MSE = pred_error_sq / 394
MSE

RMSE = sqrt(pred_error_sq/394)
RMSE



# LOOCV Using the caret Package

library(caret)

train(MPG ~ Weight, method = "lm", data = cars, trControl = trainControl(method = "LOOCV"))

train(MPG ~ Weight + I(Weight^2), method = "lm", data = cars, trControl = trainControl(method = "LOOCV"))



# Using model 2 let's estimate the mean mpg of the subpopulaiton of cars that weigh 2000lbs
fit <- lm(MPG ~ Weight + I(Weight^2),data = cars)
car2000 <- data.frame(Weight = 2000)
car2000_conf <- predict(fit, newdata = car2000, interval = "confidence")
car2000_conf

# Using model 2 let's estimate the mpg of an individual car that weighs 2000lbs
fit <- lm(MPG ~ Weight + I(Weight^2),data = cars)
car2000 <- data.frame(Weight = 2000)
car2000_pred <- predict(fit, newdata = car2000, interval = "prediction")
car2000_pred

# Plot predicted mean on our graph
cars %>% ggplot(aes(x = Weight, y = MPG)) + geom_point() + geom_line(data = cars, aes( x = Weight, y = preds, col = "red", size = 1)) + ggtitle("LR Model: Weight + Weight^2 vs MPG") + scale_color_discrete(name = "Predicted") + geom_point(aes(x=2000, y=floor(car2000_pred[1])), color='blue', shape='square', size=3)


############################################# Question 3 #############################################

# How many values are missing?
summary(cars$Horsepower)

# What rows are missing values?
missingIdx <- which(is.na(cars$Horsepower))
missingIdx

# We can't use MPG so let's look at other relationships for hints
plot(cars[,-c(1,2)])

# Weight, Acceleration, and Displacement look promising, let's zoom in
cars %>% ggplot(aes(x=Weight, y=Horsepower)) + geom_point() # Increasing SD?
cars %>% ggplot(aes(x=Displacement, y=Horsepower)) + geom_point()
cars %>% ggplot(aes(x=Acceleration, y=Horsepower)) + geom_point()

# Dispalcement and Acceleration looks good, let's try a first order wth Acceleration
fit = lm(Horsepower~Acceleration, data=cars)
summary(fit)
confint(fit)

# Scatter plot with line of predicted mean values
cars %>% ggplot(aes(x = Acceleration, y = Horsepower)) + geom_point() + geom_smooth(method = "lm") + ggtitle("LR Model: Acceleration vs Horsepower")

# Get accelerration values for missing horsepower rows
acc1 <- cars[missingIdx[1],]$Acceleration
acc2 <- cars[missingIdx[2],]$Acceleration

# Create two lists to hold our two predictor values
missingAcc <- c(acc1, acc2)

# Create data frame with correct column names
missingData <- data.frame(Acceleration = missingAcc)

# Predict horsepower
predHorse <- predict(fit, newdata = missingData)

# Insert our predicted horsepower
cars[missingIdx[1],]$Horsepower <- predHorse[1]
cars[missingIdx[2],]$Horsepower <- predHorse[2]

# Sanity check we have no more missing values
summary(cars)

# This has at least one curve, let's try a second order
cars <- cars %>% mutate(AccSquared = Acceleration^2)
squared_fit = lm(Horsepower~Acceleration+AccSquared, data=cars)
summary(squared_fit)
confint(squared_fit)

# Predict mean value for each x value
squared_preds <- predict(squared_fit)

# Calculate MPSE
squared_MSPE = mean((cars$Horsepower - squared_preds)^2)
print(paste("MSPE:", squared_MSPE))

# Scatter plot with line of predicted mean values for fixed weight
cars %>% ggplot(aes(x = Acceleration, y = Horsepower)) + geom_point() + geom_line(data = cars, aes( x = Acceleration, y = squared_preds, col = "red")) + ggtitle("LR Model: Acceleration + Acceleration^2 vs Horsepower") + scale_color_discrete(name = "Predicted")

# We are getting close, looks like we need another bend near 15, let's try a 3rd order
cars <- cars %>% mutate(AccCubed = Acceleration^3)

# Build another model
cubed_fit = lm(Horsepower~Acceleration+AccSquared+AccCubed, data=cars)
summary(cubed_fit)
confint(cubed_fit)

# Predict mean value for each x value
cubed_preds <- predict(cubed_fit)

# Calculate MPSE
cubed_MSPE = mean((cars$Horsepower - cubed_preds)^2)
print(paste("MSPE:", cubed_MSPE))

# Scatter plot with line of predicted mean values for fixed weight
cars %>% ggplot(aes(x = Acceleration, y = Horsepower)) + geom_point() + geom_line(data = cars, aes( x = Acceleration, y = cubed_preds, col = "red")) + ggtitle("LR Model: Acc + Acc^2 + Acc^3 vs Horsepower") + scale_color_discrete(name = "Predicted")


# hmmm, accCubed includes zero, better take that back out, let's try adding another feature
multi_fit = lm(Horsepower~Acceleration+AccSquared+Displacement, data=cars)
summary(multi_fit)
confint(multi_fit)

# Predict mean value for each x value
multi_preds = predict(multi_fit)

# Calculate MPSE
multi_MSPE = mean((cars$Horsepower - multi_preds)^2)
print(paste("MSPE:", multi_MSPE))

# Huge improvement! Let's press our luck
last_fit = lm(Horsepower~Acceleration+AccSquared+Displacement+Weight, data=cars)
summary(last_fit)
confint(last_fit)

# Predict mean value for each x value
last_preds = predict(last_fit)

# Calculate MPSE
last_MSPE = mean((cars$Horsepower - last_preds)^2)
print(paste("MSPE:", last_MSPE))

# Ok, I'm happy with that score. Unfortunately, the shape is now a hyperplane which we can't plot.
# On to the final question
# Scatter plot
cars %>% ggplot(aes(x = Horsepower, y = MPG)) + geom_point()

# That looks like it decays exponentially so lets try a second order fit
cars <- cars %>% mutate(HorseSquared = Horsepower^2)
horse_fit = lm(MPG~Horsepower+HorseSquared, data=cars)
summary(horse_fit)
confint(horse_fit)

# Predict mean value for each x value
horse_preds = predict(horse_fit)

# Calculate MPSE
horse_MSPE = mean((cars$MPG - horse_preds)^2)
print(paste("MSPE:", horse_MSPE))

# Scatter plot with line of predicted mean values for fixed weight
cars %>% ggplot(aes(x = Horsepower, y = MPG)) + geom_point() + geom_line(data = cars, aes( x = Horsepower, y = horse_preds, col = "red")) + ggtitle("LR Model: Horsepower + Horsepower^2 vs MPG") + scale_color_discrete(name = "Predicted")

# Ohhhh so close, lets try a third order
cars <- cars %>% mutate(HorseCubed = Horsepower^3)
horseCubed_fit = lm(MPG~Horsepower+HorseSquared+HorseCubed, data=cars)
summary(horseCubed_fit)
confint(horseCubed_fit)

# Predict mean value for each x value
horseCubed_preds = predict(horseCubed_fit)

# Calculate MPSE
horseCubed_MSPE = mean((cars$MPG - horseCubed_preds)^2)
print(paste("MSPE:", horseCubed_MSPE))

# Scatter plot with line of predicted mean values for fixed weight
cars %>% ggplot(aes(x = Horsepower, y = MPG)) + geom_point() + geom_line(data = cars, aes( x = Horsepower, y = horseCubed_preds, col = "red")) + ggtitle("LR Model: Horsepower + Horsepower^2 + Horsepower^3 vs MPG") + scale_color_discrete(name = "Predicted")

# Ahhhhh, we are going backwards. Let's try bringing back in other features
multiMPG_fit = lm(MPG~Horsepower+HorseSquared+Weight+Acceleration, data=cars)
summary(multiMPG_fit)
confint(multiMPG_fit)

# Predict mean value for each x value
multiMPG_preds = predict(multiMPG_fit)

# Calculate MPSE
multiMPG_MSPE = mean((cars$MPG - multiMPG_preds)^2)
print(paste("MSPE:", multiMPG_MSPE))

# Winner! Ohhhh no, I don't have values for my other features :sad-face:
# Fine!
horse250 <- predict(horse_fit, newdata = data.frame(Horsepower = 250, HorseSquared = 250^2), interval = "confidence")
horse250

# Plot predicted mean on our graph
cars %>% ggplot(aes(x = Horsepower, y = MPG)) + geom_point() + geom_line(data = cars, aes( x = Horsepower, y = horse_preds, col = "red")) + ggtitle("LR Model: Horsepower + Horsepower^2 vs MPG") + scale_color_discrete(name = "Predicted") + geom_point(aes(x=250, y=floor(horse250[1])), color='blue', shape='square', size=3)

# Yikes! Careful this is extrapolating. We can give a mean mpg value but it comes with a caution
