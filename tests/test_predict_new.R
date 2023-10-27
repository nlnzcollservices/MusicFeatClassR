library(testthat)
library(sloop)


# Create a test data directory with sample audio and XML files for testing
test_dir<-file.path(getwd())
test_csv_file <- file.path(test_dir, "test_output.csv")
test_root_dir<-file.path(test_dir, "music", "IE1005520")
model_path<-file.path(test_dir, "model.rds")
trained_model<-readRDS(model_path)
context("my_predictor Tests")

test_that("Test my_predictor with valid input", {

  feature_data <- my_predictor(test_root_dir, trained_model)
  expect_is(feature_data, "data.frame")
  expect_equal(nrow(feature_data), 2)
  expect_true("ie_num" %in% colnames(feature_data))
  expect_true("title" %in% colnames(feature_data))
  expect_true("audio_file" %in% colnames(feature_data))
  expect_true("label_code" %in% colnames(feature_data))
})

test_that("Test my_predictor with missing music directory", {
  expect_error(my_predictor(NULL, trained_model), "The root directory path must be provided.")
})

test_that("Test my_predictor with string instead of trained model", {

  trained_model <- "Mock Model"
  expect_error(my_predictor(test_root_dir, trained_model), "The 'trained_model' must be a S3 class object.")
})


