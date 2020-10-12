trainingSet <- read.csv("C:/Users/Jay/Documents/A4DATA/40153430_train.CSV", header=FALSE)
train_T = trainingSet[,2:5]
testingSet <- read.csv("C:/Users/Jay/Documents/A4DATA/40153430_test.CSV", header = FALSE)
library(class)
library(ISLR)
library(caret)

#Splitting the "Training" data set into training and test datasets
training_training <- trainingSet[1:3200,]
training_test <- trainingSet[3201:3999,]
normalise_data <- function(x){(x-min(x))/(max(x)-min(x))}

training_training <- training_training[order(runif(nrow(training_training))),]
training_test <- training_test[order(runif(nrow(training_test))),]

#Normalising the data
train_normal <- as.data.frame(lapply(training_training[,-1], normalise_data))
test_normal <- as.data.frame(lapply(training_test[,-1], normalise_data))

train_x <- train_normal[,2:5]
test_x <- test_normal[,2:5]

train_y <- train_normal[,1]
test_y <- test_normal[,1]

#kNN for k=1 
kNN1 <- knn(train_x,test_x,train_y, k=1)
 table(kNN1, test_y)
accuracyKNN1 <- mean(test_y == kNN1)
accuracyKNN1

plot(accuracyKNN1)

#kNN for k=3
kNN3 <- knn(train_x,test_x,train_y, k=3)
table(kNN3, test_y)
accuracyKNN3 <- mean(train_y == kNN3)
accuracyKNN3

#kNN for k=5
kNN5 <- knn(train_x,test_x,train_y, k=5)
table(kNN5, test_y)
accuracyKNN5 <- mean(train_y == kNN5)
accuracyKNN5

#kNN for k=9
kNN9 <- knn(train_x,test_x,train_y, k=9)
table(kNN9, test_y)
accuraccyKNN9 <- mean(train_y == kNN9)
accuraccyKNN9

#kNN for k=17
kNN17 <- knn(train_x,test_x,train_y, k=17)
table(kNN17, test_y)
accuracyKNN17 <- mean(train_y == kNN17)
accuracyKNN17

#kNN for k=33
kNN33 <- knn(train_x, test_x, train_y, k=33)
table(kNN33,test_y)
accuracyKNN33 <- mean(train_y == kNN33)
accuracyKNN33


#TASK 2.2

train_normal$folds <- cut(seq(1,nrow(train_normal)),breaks=5,labels=FALSE)
feature<- cbind(train_normal$V2,train_normal$V3,train_normal$V4,train_normal$V5)

accuracy_vector <- vector(length =5)
#KNN k=1
for (i in 1:5)
{
  train_it_fold = train_normal[train_normal$folds!=i,]
  validation_it_fold = train_normal[train_normal$folds==i,]
  
  pred = knn(train_it_fold[,feature], validation_it_fold[,feature],train_it_fold$V2, k=1)
  correctList <- pred == validation_it_fold$V2
  nrCorrect <- nrow(validation_it_fold[correctList,])
  
  
    acc<- nrCorrect/nrow(validation_it_fold)
    error <- 1- acc
    print(acc)
    print(err)
    accuracy_vector[i] <- acc
}
ACCk1 <- mean(accuracy_vector)
ACCk1
errK1 <- 1-ACCk1
errK1
#kNN k=3
for (i in 1:5)
{
  train_it_fold = train_normal[train_normal$folds!=i,]
  validation_it_fold = train_normal[train_normal$folds==i,]
  
  pred = knn(train_it_fold[,feature], validation_it_fold[,feature],train_it_fold$V2, k=3)
  correctList <- pred == validation_it_fold$V2
  nrCorrect <- nrow(validation_it_fold[correctList,])
  
  acc<- nrCorrect/nrow(validation_it_fold)
  error <- 1- acc
  print(acc)
  print(err)
  accuracy_vector[i] <- acc
}

ACCk3 <- mean(accuracy_vector)
ACCk3
errk3 <- 1-ACCk3
errk3

#KNN k= 5
for (i in 1:5)
{
  train_it_fold = train_normal[train_normal$folds!=i,]
  validation_it_fold = train_normal[train_normal$folds==i,]
  
  pred = knn(train_it_fold[,feature], validation_it_fold[,feature],train_it_fold$V2, k=5)
  correctList <- pred == validation_it_fold$V2
  nrCorrect <- nrow(validation_it_fold[correctList,])
  
  
  acc<- nrCorrect/nrow(validation_it_fold)
  error <- 1- acc
  print(acc)
  print(err)
  accuracy_vector[i] <- acc
}

ACCk5 <- mean(accuracy_vector)
ACCk5
errK5 <- 1-ACCk5
#KNN for k=9
for (i in 1:5)
{
  train_it_fold = train_normal[train_normal$folds!=i,]
  validation_it_fold = train_normal[train_normal$folds==i,]
  
  pred = knn(train_it_fold[,feature], validation_it_fold[,feature],train_it_fold$V2, k=9)
  correctList <- pred == validation_it_fold$V2
  nrCorrect <- nrow(validation_it_fold[correctList,])
  
  
  acc<- nrCorrect/nrow(validation_it_fold)
  error <- 1- acc
  print(acc)
  print(err)
  accuracy_vector[i] <- acc
}
ACCk9 <- mean(accuracy_vector)
ACCk9
errK9 <- 1-ACCk9
#KNN for k=17
for (i in 1:5)
{
  train_it_fold = train_normal[train_normal$folds!=i,]
  validation_it_fold = train_normal[train_normal$folds==i,]
  
  pred = knn(train_it_fold[,feature], validation_it_fold[,feature],train_it_fold$V2, k=17)
  correctList <- pred == validation_it_fold$V2
  nrCorrect <- nrow(validation_it_fold[correctList,])
  
  
  acc<- nrCorrect/nrow(validation_it_fold)
  error <- 1- acc
  print(acc)
  print(err)
  accuracy_vector[i] <- acc
}
ACCk17<- mean(accuracy_vector)
ACCk17
errK17 <- 1-ACCk17

#KNN for k=33
for (i in 1:5)
{
  train_it_fold = train_normal[train_normal$folds!=i,]
  validation_it_fold = train_normal[train_normal$folds==i,]
  
  pred = knn(train_it_fold[,feature], validation_it_fold[,feature],train_it_fold$V2, k=33)
  correctList <- pred == validation_it_fold$V2
  nrCorrect <- nrow(validation_it_fold[correctList,])
  
  
  acc<- nrCorrect/nrow(validation_it_fold)
  error <- 1- acc
  print(acc)
  print(err)
  accuracy_vector[i] <- acc
}
ACCk33<- mean(accuracy_vector)
ACCk33
errK33 <- 1-ACCk33
