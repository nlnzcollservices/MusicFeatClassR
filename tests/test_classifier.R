library(testthat)
library(randomForest)
library(caret)
library(data.table)
library(stringr)
library(e1071)
library(nnet)

# Correct values test cases
test_that("train_classifier with correct values returns expected results", {
  fold_path<-file.path(getwd(),'files')
  csv_file_name<-"extracted_features_correct.csv"
  csv_file_path<-file.path(fold_path, csv_file_name)
  feature_data<-read.csv(csv_file_path)

  results_rf <- train_classifier(feature_data, "label_code", "RandomForest")

  expect_type(results_rf, "list")
  expect_named(results_rf, c("accuracy", "confusion_matrix", "trained_model","predictions"))
  expect_type(results_rf$accuracy, "double")
  expect_s3_class(results_rf$confusion_matrix, "confusionMatrix")
  expect_s3_class(results_rf, "train_classifier")

})

# Incorrect values test cases
test_that("train_classifier with incorrect values returns an appropriate error messages", {
  feature_data<-"Random string"
  expect_error(train_classifier(feature_data, "label_code", "RandomForest"),"Input 'dataframe' must be a data frame.")
  fold_path<-file.path(getwd(), 'files')
    csv_file_name<-"extracted_features_empty.csv"
    csv_file_path<-file.path(fold_path, csv_file_name)
    feature_data<-read.csv(csv_file_path)

    expect_error(train_classifier(feature_data, "label_code", "RandomForest"),
                 "The data frame is empty.")
    csv_file_name<-"extracted_features_correct.csv"
    csv_file_path<-file.path(fold_path, csv_file_name)
    feature_data<-read.csv(csv_file_path)


  # Test an incorrect target variable
  expect_error(train_classifier(feature_data, 2, "RandomForest"),
               "Input 'target_variable' must be a character string.")

  expect_error(train_classifier(feature_data, "label_code", " Kneighbour is not supported. Avilable classifiers are  RandomForest, LogisticRegression, SupportVectorMachine, NaiveBayes ."))
})
