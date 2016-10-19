install.packages('stats')
install.packages("e1071") ## For Support Vector Machine 

library('e1071')

## Reading Files 
Data1 <- read.csv("SVM_Practice.csv", header = TRUE)

## Plot them in a 2D Plane
plot(Data1,pch = 20)

## Applying Linear Regression Model 

model <- lm(Data1$Y ~ Data1$X , Data1)

abline(model)

PredictedY <- predict(model, Data1)

points(Data1$X, PredictedY, col= "blue", pch= 4)

## Calculating Errors in Linear Regression Model

## Function to calculate Root Mean Square Error
rmse <- function(error)
{
  sqrt(mean(error^2))
}

error <- model$residuals

predictRMSE <- rmse(error)

## Applying Support Vector Machine Algorithm

model2 <- svm(Data1$Y ~ Data1$X , Data1)

predictedY2 <- predict(model2, Data1)

points(Data1$X, predictedY2, col = "red", pch = 7)

## Error calculation 

error1 <- model2$residuals

predictRMSE2 <- rmse(error1)

## Tuning SVM to reduce error 

## Epsilon is 0.1 by default , we are giving it a range of values from 0 to 1 and interval of 0.1.

tuneResult <- tune(svm, Data1$Y ~ Data1$X, data = Data1,
                   ranges = list(epsilon = seq(0,1,0.1), cost = 2^(2:9))
)
print(tuneResult)
plot(tuneResult)

## Darker Portion of Plot depicts better modelling and ranging epsilon accordingly

tuneResult <- tune(svm, Data1$Y ~ Data1$X, data = Data1,
                   ranges = list(epsilon = seq(0,0.02,0.01), cost = 2^(2:9))
)
print(tuneResult)
plot(tuneResult)

## Putting them in a tuned Model 
## R has a in-builr function to estimate the best model 

tunedmodel <- tuneResult$best.model
tunedmodelY <- predict(tunedmodel,Data1)

error3 <- Data1$Y - tunedmodelY ## similar to 'error3 <- tunedmodel$residuals'

tunedmodelRMSE <- rmse(error3)

points(Data1$X, tunedmodelY, col = "green", pch = 10)
