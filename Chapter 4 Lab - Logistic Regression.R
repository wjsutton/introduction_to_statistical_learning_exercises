###  Chapter 4 Lab: Logistic Regression, LDA, QDA, and KNN 

### Load/install packages

#install.packages("ISLR")
#install.packages("MASS")
library(ISLR)
library(MASS)

# Data summary
names(Smarket)
dim(Smarket)
summary(Smarket)

# correlation matrix
pairs(Smarket)
cor(Smarket[,-9])

# plot Volume
attach(Smarket)
plot(Volume)

### Logistic Regression
# Predict: Direction (up/down) of stock market
# Using: Lag1-Lag5 & Volume
# Model: glm - generalized linear model
# Method: Binomial - for logistic regression

glm.fits <- glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, 
                data = Smarket,
                family = binomial)
summary(glm.fits)

# Smallest p-value is with Lag1 (0.145)
# indicates if market up yesterday, less likely to be up today
# however p-value of 0.145 is still relatively large and not a clear indicator

# coefficients of model
coef(glm.fits)
summary(glm.fits)$coef
# p-values of model
summary(glm.fits)$coef[,4]

# Give list of predictions based on glm.fits model
glm.probs <- predict(glm.fits, type="response")
glm.probs[1:10]
contrasts(Direction)

# create a list of up/down based on probabilities
glm.pred <- rep("Down",1250)
glm.pred[glm.probs>.5] <- "Up"

# Create confusion matrix of outcomes
table(glm.pred, Direction)
(507+145)/1250
mean(glm.pred == Direction)

train <- (Year<2005)
Smarket.2005 <- Smarket[!train,]
dim(Smarket.2005)
Direction.2005 <- Direction[!train]

# Running model training on pre 2005 data 
glm.fits <- glm(Direction ~ Lag1+Lag2+Lag3+Lag4+Lag5+Volume,
                data = Smarket,
                family = binomial,
                subset = train)
glm.probs <- predict(glm.fits, Smarket.2005, type = "response")

glm.pred <- rep("Down",252)
glm.pred[glm.probs>.5] <- "Up"
table(glm.pred, Direction.2005)

mean(glm.pred == Direction.2005)
mean(glm.pred != Direction.2005)

# Rerunning model with less variables
glm.fits <- glm(Direction ~ Lag1+Lag2,
                data = Smarket,
                family = binomial,
                subset = train)

glm.probs <- predict(glm.fits, Smarket.2005, type = "response")
glm.pred <- rep("Down",252)
glm.pred[glm.probs>.5] <- "Up"
table(glm.pred, Direction.2005)

mean(glm.pred == Direction.2005)

# Predict for particular data points
predict(glm.fits, newdata = data.frame(Lag1=c(1.2,1.5),Lag2=c(1.1,-0.8)), type = "response")

### Linear Discriminant Analysis

library(MASS)
lda.fit <- lda(Direction ~ Lag1+Lag2,
               data = Smarket,
               subset = train)
lda.fit
plot(lda.fit)


lda.pred <- predict(lda.fit, Smarket.2005)
names(lda.pred)
lda.class <- lda.pred$class
table(lda.class,Direction.2005)
mean(lda.class == Direction.2005)

sum(lda.pred$posterior[,1]>=.5)
sum(lda.pred$posterior[,1]<=.5)
lda.pred$posterior[1:20,1]
lda.class[1:20]

sum(lda.pred$posterior[,1]>.9)


### Quadratic Discriminant Analysis

qda.fit <- qda(Direction ~ Lag1+Lag2,
               data = Smarket,
               subset = train)

qda.class <- predict(qda.fit, Smarket.2005)$class
table(qda.class,Direction.2005)
mean(qda.class == Direction.2005)


# K-nearest neighbors

library(class)
train.X <- cbind(Lag1, Lag2)[train,]
test.X <- cbind(Lag1, Lag2)[!train,]
train.Direction <- Direction[train]

set.seed(1)
# Testing k = 1
knn.pred <- knn(train.X,test.X,train.Direction, k = 1)
table(knn.pred, Direction.2005)
mean(knn.pred == Direction.2005)

# k = 3
knn.pred <- knn(train.X,test.X,train.Direction, k =3)
table(knn.pred, Direction.2005)
mean(knn.pred == Direction.2005)


### An Application to Caravan Insurance Data

library(ISLR)
library(class)

dim(Caravan)
attach(Caravan)
# Purchased caravan insurance
summary(Purchase)
348/5822

# standardizing variables for KNN
# standard deviation of one 
# and a mean of zero
standardized.X <- scale(Caravan[,-86])
var(Caravan[,1])
var(Caravan[,2])

var(standardized.X[,1])
var(standardized.X[,2])

# define train and test datasets
test <- 1:1000
train.X <- standardized.X[-test,]
test.X <- standardized.X[test,]

train.Y <- Purchase[-test]
test.Y <- Purchase[test]

set.seed(1)
knn.pred <- knn(train.X,test.X,train.Y,k=1)
mean(test.Y!=knn.pred)
mean(test.Y!='No')

# KNN error rate is 12%, actual is 6%

table(knn.pred,test.Y)
9/(68+9)

# k = 3
knn.pred <- knn(train.X,test.X,train.Y,k=3)
table(knn.pred,test.Y)
5/26

# k = 5
knn.pred <- knn(train.X,test.X,train.Y,k=5)
table(knn.pred,test.Y)
4/15

# Compare to logistic regression model

glm.fits <- glm(Purchase~.
                ,data = Caravan
                ,family = binomial
                ,subset = -test)

glm.probs <- predict(glm.fits, Caravan[test,], type = "response")
glm.pred <- rep("No",1000)
glm.pred[glm.probs>.5] <- "Yes"

table(glm.pred,test.Y)

glm.pred <- rep("No",1000)
glm.pred[glm.probs>.25] <- "Yes"

table(glm.pred,test.Y)
11/33







