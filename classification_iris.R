library(psych)
data(iris)
View(iris)
attach(iris)

lapply(iris, function(x) sum(is.na(x)))

str(iris)

summary(iris)

describe(iris)

boxplot(Sepal.Length)
hist(Sepal.Length)

boxplot(Sepal.Width, horizontal = TRUE)
hist(Sepal.Width)

boxplot(Petal.Length, horizontal = T)
hist(Petal.Length)

boxplot(Petal.Width, horizontal = T)
hist(Petal.Width)

################################################################
#SVM
library(caTools)
split <- sample.split(iris$Species,0.75)
iris_train1 <- iris[split,]
iris_test1 <- iris[!split,]


library(kernlab)
library(caret)
iris_r <- ksvm(Species~., data = iris_train1,kernel = 'rbfdot')
pred_r <- predict(iris_r, iris_test1)
mean(pred_r == iris_test1$Species)
library(gmodels)
CrossTable(pred_r,iris_test1$Species)

###################################################################
#using KNN
iris_train <- iris[split,1:4]
iris_test <- iris[!split,1:4]

iris_train_l <- iris[split,5]
iris_test_l <- iris[!split,5]

library(class)
iris_k <- knn(train = iris_train,test = iris_test,cl = iris_train_l,k=11 )
CrossTable(iris_k, iris_test_l)
mean(iris_k == iris_test_l)


###################################################################
#using Naive Bayes
library(e1071)
iris_n <- naiveBayes(iris_train, iris_train_l)

pred_n <- predict(iris_n, iris_test)
CrossTable(pred_n, iris_test_l)
mean(pred_n == iris_test_l)

####################################################################