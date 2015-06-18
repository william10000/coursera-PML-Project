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

train$new_window <- NULL # these are mostly "no" - 19216 vs. 406 yes
train$cvtd_timestamp <- NULL

# create new training and validation sets





