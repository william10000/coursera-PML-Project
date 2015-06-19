# Practical Machine Learning Project
# William Wan
# started on 2015-06-18

# main.R

# see here for github.io pages https://class.coursera.org/predmachlearn-015/forum/thread?thread_id=67

# What you should submit
# 
# The goal of your project is to predict the manner in which they did the
# exercise. This is the "classe" variable in the training set. You may use any
# of the other variables to predict with. You should create a report describing
# how you built your model, how you used cross validation, what you think the
# expected out of sample error is, and why you made the choices you did. You
# will also use your prediction model to predict 20 different test cases.
# 
# 1. Your submission should consist of a link to a Github repo with your R
# markdown and compiled HTML file describing your analysis. Please constrain the
# text of the writeup to < 2000 words and the number of figures to be less than
# 5. It will make it easier for the graders if you submit a repo with a gh-pages
# branch so the HTML page can be viewed online (and you always want to make it
# easy on graders :-). 
# 2. You should also apply your machine learning algorithm
# to the 20 test cases available in the test data above. Please submit your
# predictions in appropriate format to the programming assignment for automated
# grading. See the programming assignment for additional details.
# 

# strategy - 
  # remove unnecessary columns - eg. date
  # remove columns with all NA

# Notes
# train$classe contains values of correct movement A-E
#  enable multi-core processing

library(dplyr)
library(caret)


library(doParallel)
cl <- makeCluster(detectCores())
registerDoParallel(cl) 


# load datasets
train_org <- read.csv("pml-training.csv")
test <- read.csv("pml-testing.csv")

train <- train_org # work with a copy of the training set so that we don't have to waste time reloading the original from disk

class_names <- names(train_org) # get list of factor names

col_na_train <- colSums(!is.na(train_org)) # find number of NAs in training set
col_na_test <- colSums(!is.na(test)) # find number of NAs in test set


##### remove NA columns from train dataset
dont_use <- names(col_na_test[!(col_na_test>0)]) # create vector of names with nearly all NAs using test set


for (i in 1 : length(dont_use)) {
  train[, dont_use[i]] <- NULL
}

##### remove unnecessary predictors name, row number, time stamp etc. - some were el
train$raw_timestamp_part_1  <- NULL
train$new_window <- NULL # these are mostly "no" - 19216 vs. 406 yes
train$cvtd_timestamp <- NULL

# these were eliminated after looking at varImp and checking histograms of "important' variables
train$X <- NULL
train$num_window <- NULL

# create new training and validation sets
folds <- createFolds(y = train$classe, k = 10, list = TRUE)

# train on folds - run multiple times after checking varImp and histograms of important variables
modFit1 <- train(classe ~ ., data = train[folds[1]$Fold01, ], method = "rf") # started 15:48 done before 16:03

modFit2 <- train(classe ~ ., data = train[folds[2]$Fold02, ], method = "rf") # started 16:05 done before 16:07

modFit3 <- train(classe ~ ., data = train[folds[3]$Fold03, ], method = "rf") # started 22:02 done before 22:04

# check importance of variables to see if anything looks out of place
varImp(modFit1)
varImp(modFit2)
varImp(modFit3)

# this is not real cross validation - will do if I have time
# predict on validation test sets
pred_train1 <- predict(modFit1, train[folds[4]$Fold04,])
pred_train2 <- predict(modFit2, train[folds[5]$Fold05,])
pred_train3 <- predict(modFit3, train[folds[6]$Fold06,])

# calculate out of sample errors
oose1 <- (pred_train1 == train[folds[4]$Fold04,]$classe)/nrow(pred_train1)
oose2 <- (pred_train2 == train[folds[5]$Fold04,]$classe)/nrow(pred_train2)
oose2 <- (pred_train3 == train[folds[6]$Fold04,]$classe)/nrow(pred_train3)

oose_tot <- mean(c(oose1, oose2, oose3))

# predict on test set using different models - use majority vote - hope at least 2/3 are alike
pred_test1 <- predict(modelFit1, test)
pred_test2 <- predict(modelFit2, test)
pred_test3 <- predict(modelFit3, test)

# missClass = function(values,prediction){sum(((prediction > 0.5)*1) != values)/length(values)}
# 
# missClass(trainSA$chd, pred_train) # 0.27
# missClass(testSA$chd, pred_test) # 0.31



# put predictions here in the answers
answers <- as.character()

pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

pml_write_files(answers)
