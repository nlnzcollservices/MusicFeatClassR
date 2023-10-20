library(testthat)
library(tuneR)


test_audio_file <- file.path(getwd(),"music","IE1005520","REP74107552","FL74107553_1294987_PM_S_2.mp3")

test_that("extract_audio_features extracts correctly", {
  if (!file.exists(test_audio_file)) {

    simple_audio <- sine(440, duration = 5, samp.rate = 44100)
    writeWave(simple_audio, test_audio_file)
  }
  features <- extract_audio_features(test_audio_file)
  expect_type(features, "list")
  expect_true("duration" %in% colnames(features))
  expect_true("sample_rate" %in% colnames(features))
  expect_true("num_channels" %in% colnames(features))
  expect_true("amplitude" %in% colnames(features))
  expect_true(features$duration > 0)
  expect_true(features$sample_rate > 0)
  expect_true(features$num_channels > 0)
  expect_true(features$amplitude >= 0)
})
