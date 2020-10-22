###  Chapter 3: Linear Regression Lab

### Load/install packages

#install.packages("MASS")
#install.packages("ISLR")
#install.packages("car")
library(MASS)
library(ISLR)

### Simple Linear Regression

# preview data
fix(Boston)
# print column names
names(Boston)
# find out more
?Boston

# lm() function to fit simple linear regression, medv = response, lstat = predictor
lm.fit <- lm(medv~lstat, data=Boston)

lm.fit
summary(lm.fit)

# see other info stored in lm.fit
names(lm.fit)
coef(lm.fit)

# confidence intervals
confint(lm.fit)

# show confidence interval for estimates of 5, 10 ,15
predict(lm.fit,data.frame(lstat=c(5,10,15)),interval = "confidence")
# show prediction interval for estimates of 5, 10 ,15
predict(lm.fit,data.frame(lstat=c(5,10,15)),interval = "prediction")

# plotting lstat vs medv and least squares regression line
plot(Boston$lstat,Boston$medv)
abline(lm.fit, lwd=3,col="red")

# convert plot into 2x2 grid
par(mfrow=c(2,2))
plot(lm.fit)

# plot residuals and studentized residuals
plot(predict(lm.fit), residuals(lm.fit))
plot(predict(lm.fit), rstudent(lm.fit))

# Leverage statistics computed for all predictors and the index of the largest element of a vector
plot(hatvalues(lm.fit))
which.max(hatvalues(lm.fit))

### Multiple Linear Regression

# composite lstat + data
lm.fit <- lm(medv~lstat+age, data=Boston)
summary(lm.fit)

# all variables
lm.fit <- lm(medv~., data=Boston)
summary(lm.fit)

# variance inflation factors
library(car)
vif(lm.fit)

# all variables except age
lm.fit1 <- lm(medv~.-age, data=Boston)
summary(lm.fit1)
# or update
lm.fit1 <- update(lm.fit,~.-age)

### Interaction Terms

# lstat:black -> include an interaction term between lstat and black
# lstat*age -> include: lstat, age, and the interaction term lstat x age

summary(lm(medv~lstat*age, data=Boston))

### Non-linear Transformation of the Predictors

lm.fit2 <- lm(medv~lstat+I(lstat^2), data=Boston)
summary(lm.fit2)

# Analysis of variance
lm.fit <- lm(medv~lstat, data=Boston)
anova(lm.fit,lm.fit2)

# convert plot into 2x2 grid
par(mfrow=c(2,2))
plot(lm.fit2)

# poly() to create a polynomial fit
lm.fit5 <- lm(medv~poly(lstat,5), data=Boston)
summary(lm.fit5)

# log transformation
summary(lm(medv~log(rm), data=Boston))

### Qualitative Predictors

# Inspect car data
fix(Carseats)
names(Carseats)

lm.fit <- lm(Sales~.+Income:Advertising+Price:Age, data = Carseats)
summary(lm.fit)
contrasts(Carseats$ShelveLoc)

### Writing functions

LoadLibraries <- function(){
  library(MASS)
  library(ISLR)
  print("Libraries are loaded")
}
LoadLibraries()

















