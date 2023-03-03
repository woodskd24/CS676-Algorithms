library(tidyr)
library(dplyr)
library(readr)
library(readxl)
library(tibble)
library(writexl)
library(stringr)
library(ggplot2)
library(summarytools)
library(ramify)


iris <- read_csv("iris.data.csv")

##Prep
install.packages('class')
library(class)
install.packages('caret')
library(caret)
library("mlbench")
library("tibble")
library("rpart")

train <- rbind(iris3[1:25,,1], iris3[1:25,,2], iris3[1:25,,3])
test <- rbind(iris3[26:50,,1], iris3[26:50,,2], iris3[26:50,,3])
cl <- factor(c(rep("s",25), rep("c",25), rep("v",25)))
knn3Train(train, test, cl, k = 5, prob = TRUE)

##Q6.4 (A & L)
fit=knn3Train(train, test, cl, k = 5, prob = TRUE)
cm = as.matrix(table(Actual = cl, Predicted = fit))
sum(diag(cm))/length(cl)

##B & M
fit=knn3Train(train, test, cl, k = 3, prob = TRUE)
cm = as.matrix(table(Actual = cl, Predicted = fit))
sum(diag(cm))/length(cl)

##C
fit=knn3Train(train, test, cl, k = 5, prob = TRUE)
cm = as.matrix(table(Actual = cl, Predicted = fit))
sum(diag(cm))/length(cl)

##D
manhattan_dist <- function(a, b){
  dist <- abs(a-b)
  dist <- sum(dist)
  return(dist)
}

a <- train
b <- test

manhattan_dist(a, b)

##E & N

fit=knn3Train(train, test, cl, k = 5, prob = 1)
cm = as.matrix(table(Actual = cl, Predicted = fit))
sum(diag(cm))/length(cl)

#F & O

fit=knn3Train(train, test, cl, k = 5, prob = 2)
cm = as.matrix(table(Actual = cl, Predicted = fit))
sum(diag(cm))/length(cl)

#I
min_max_norm <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

iris_norm <- as.data.frame(lapply(iris[1:4], min_max_norm))

head(iris_norm)

#J
train <- rbind(iris_norm[1:25,,1], iris_norm[1:25,,2], iris_norm[1:25,,3])
test <- rbind(iris_norm[26:50,,1], iris_norm[26:50,,2], iris_norm[26:50,,3])
cl <- factor(c(rep("s",25), rep("c",25), rep("v",25)))
fit=knn3Train(train, test, cl, k = 5, prob = 2)
cm = as.matrix(table(Actual = cl, Predicted = fit))
sum(diag(cm))/length(cl)

#K
minkowski_distance = function(a,b,p){
  if(p<=0){
    stop('p must be higher than 0') 
  }
  
  if(length(a)== length(b)){
    sum(abs(a-b)^p)^(1/p)
  }else{
    stop('Vectors must be of the same length')
    
  }
}

minkowski_distance

minkowski_distance(1:10, 11:20, 2)

library(tidyr)
library(dplyr)
library(readr)
library(readxl)
library(tibble)
library(writexl)
library(stringr)
library(ggplot2)
library(summarytools)
library(ramify)


x <- c(5,6,4,3,4,3,1)
y <- c(2,5,2,3,7,6,8)
g <- c(4,3,3,3,2,2,1)

df <- data.frame(x,y,g)

print(df)

attach(df)

model <- lm(y ~ x, data=df)
new <- data.frame(x=c(2), y=c(4))
predict(model, newdata = new)

model <- lm(y ~ x, data=df)
new <- data.frame(x=c(2), y=c(4))
predict(model, newdata = new)

model <- lm(y ~ x, data=df)
new <- data.frame(x=c(2), y=c(4))
predict(model, newdata = new)



