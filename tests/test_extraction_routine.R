library(testthat)
library(xml2)
library(dplyr)
library(purrr)
library(tuneR)
library(data.table)

test_dir<-file.path(getwd())
test_csv_file <- file.path(test_dir, "test_output.csv")
test_label_file<-file.path(test_dir, "files","ies_mms_label.txt")
test_root_dir<-file.path(test_dir, "music")

test_that("rpipeline runs without errors", {
  result <- rpipeline(test_label_file, test_csv_file, test_root_dir)
  expect_true(file.exists(test_csv_file))
  expect_type(result$feature_data, "list")
  expect_equal(nrow(result$feature_data), 2)
  if (file.exists(test_csv_file)) {
    file.remove(test_csv_file)
  }
})



