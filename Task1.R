data <- read.csv("C:/Users/Jay/Documents/features/40153430-all-features.CSV", header= FALSE)
library(datasets)
nrow(data)
head(data)
summary(data)

#Dummy column to discriminate between digit and nondigit
data$dummy.V1 <-0
data$dummy.V1
data$dummy.V1[data$V1 == 100] <- 1
data$dummy.V1[data$V1 == 200] <- 1
data$dummy.V1[data$V1 == 300] <- 1

data_shuffled <- data[sample(nrow(data)),]
head(data_shuffled)

#first 80% will be training data
training_data = data_shuffled[1:128,]
testing_data = data_shuffled[129:160,]

#TASK1.1
V2 <- data_shuffled[,2]
V2

glmfit <- glm(dummy.V1 ~ V2, data = training_data, 
              family = 'binomial')
summary(glmfit)

#Assuming a p>0.5 cut off, calculate accuracy on the training data
x.values = training_data[,2]
training_data[["Pred_Val"]] = predict(glmfit,training_data,type = "response")
training_data[["pred_class"]] = 0
training_data[["pred_class"]][training_data[["Pred_val"]] > 0.5] = 1
correctItems = training_data[["pred_class"]] == training_data[["dummy.V1"]] 

#Proportion correct
nrow(training_data[correctItems,])/nrow(training_data)
#Proportion incorrect
nrow(training_data[!correctItems,])/nrow(training_data)

#Calculate accuracy on the test data, assuming a p>0.5 cut off
remove(x.values)
x.values = testing_data[,2]

testing_data[["Pred_val"]] = predict(glmfit, testing_data, type="response")
testing_data[["pred_class"]] = 0
testing_data[["pred_class"]][testing_data[["Pred_val"]] > 0.5] = 1

correctItems = testing_data[["pred_class"]] == testing_data[["dummy.V1"]] 

# proportion correct:
nrow(testing_data[correctItems,])/nrow(testing_data)

# proportion incorrect:
nrow(testing_data[!correctItems,])/nrow(testing_data)

#TASK 1.2
V3 <- data_shuffled[,3]
V4<- data_shuffled[,4]
V5<- data_shuffled[,5]
V6<- data_shuffled[,6]
V7<- data_shuffled[,7]
V8<- data_shuffled[,8]
V9<- data_shuffled[,9]
V10<- data_shuffled[,10]
V11 <- data_shuffled[,11]

glm2 <- glm(dummy.V1~V2+V3+V4+V5+V6+V7+V8+V9+V10+V11, 
            data = training_data, 
              family = 'binomial')

summary(glm2)
remove(x.values)
x.values = V2+V3+V4+V5+V6+V7+V8+V9+V10+V11
training_data[["Pred_Val"]] = predict(glm2,training_data,type = "response")
training_data[["pred_class"]] = 0
training_data[["pred_class"]][training_data[["Pred_val"]] > 0.5] = 1
correctItems = training_data[["pred_class"]] == training_data[["dummy.V1"]] 

#Proportion correct
nrow(training_data[correctItems,])/nrow(training_data)
#Proportion incorrect
nrow(training_data[!correctItems,])/nrow(training_data)

remove(x.values)
x.values = V2+V3+V4+V5+V6+V7+V8+V9+V10+V11

testing_data[["Pred_val"]] = predict(glm2, testing_data, type="response")
testing_data[["pred_class"]] = 0
testing_data[["pred_class"]][testing_data[["Pred_val"]] > 0.5] = 1

correctItems = testing_data[["pred_class"]] == testing_data[["dummy.V1"]] 

# proportion correct:
nrow(testing_data[correctItems,])/nrow(testing_data)

# proportion incorrect:
nrow(testing_data[!correctItems,])/nrow(testing_data)


#TASK 1.3
library(ISLR)
library(boot)
library(caret)

#Creating fold with k=5, and then performing cross validation
folds <- createFolds(training_data$dummy.V1, k=5)
CV <- lapply(folds, function(x){
  training_fold = training_data[-x,]
  test_fold = training_data[x,]
  glmfit <- glm(dummy.V1 ~ V2, data = training_fold, 
                family = 'binomial')
  training_fold[["Pred_Val"]] = predict(glmfit,training_fold,type = "response")
  training_fold[["pred_class"]] = 0
  training_fold[["pred_class"]][training_fold[["Pred_val"]] > 0.5] = 1
  correctItems = training_fold[["pred_class"]] == training_fold[["dummy.V1"]]
  nrow(training_fold[correctItems,])/nrow(training_fold)
})

folds2 <- createFolds(testing_data$dummy.V1, k=5)
CV2 <- lapply(folds2, function(x){
  testing_fold = testing_data[-x,]
  test_fold = testing_data[x,]
  glmfit <- glm(dummy.V1 ~ V2, data = testing_fold, 
                family = 'binomial')
  testing_fold[["Pred_Val"]] = predict(glmfit,testing_fold,type = "response")
  testing_fold[["pred_class"]] = 0
  testing_fold[["pred_class"]][testing_fold[["Pred_val"]] > 0.5] = 1
  correctItems = testing_fold[["pred_class"]] == testing_fold[["dummy.V1"]]
  nrow(testing_fold[correctItems,])/nrow(testing_fold)
})

folds3 <- createFolds(training_data$dummy.V1, k=5)
CV3 <- lapply(folds3, function(x){
  training_fold = training_data[-x,]
  test_fold = training_data[x,]
  glmfit <- glm(dummy.V1 ~ V2+V3+V4+V5+V6+V7+V8+V9+V10+V11
                , data = training_fold, 
                family = 'binomial')
  training_fold[["Pred_Val"]] = predict(glmfit,training_fold,type = "response")
  training_fold[["pred_class"]] = 0
  training_fold[["pred_class"]][training_fold[["Pred_val"]] > 0.5] = 1
  correctItems = training_fold[["pred_class"]] == training_fold[["dummy.V1"]]
  nrow(training_fold[correctItems,])/nrow(training_fold)
})

folds4 <- createFolds(testing_data$dummy.V1, k=5)
CV4 <- lapply(folds4, function(x){
  testing_fold = testing_data[-x,]
  test_fold = testing_data[x,]
  glmfit <- glm(dummy.V1 ~ V2+V3+V4+V5+V6+V7+V8+V9+V10+V11
                , data = testing_fold, 
                family = 'binomial')
  testing_fold[["Pred_Val"]] = predict(glmfit,testing_fold,type = "response")
  testing_fold[["pred_class"]] = 0
  testing_fold[["pred_class"]][testing_fold[["Pred_val"]] > 0.5] = 1
  correctItems = testing_fold[["pred_class"]] == testing_fold[["dummy.V1"]]
  nrow(testing_fold[correctItems,])/nrow(testing_fold)
})

