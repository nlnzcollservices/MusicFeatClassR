library(testthat)
library(xml2)


test_xml_file <- file.path(getwd(),"music","IE1005520","ie.xml")


test_that("extract_metadata correctly extracts metadata from XML", {

  metadata <- extract_metadata(test_xml_file)
  expect_type(metadata, "list")
  expect_true("xml_title" %in% colnames(metadata))
  expect_true("xml_creators" %in% colnames(metadata))
  expect_true("xml_type" %in% colnames(metadata))
  expect_true("xml_publisher" %in% colnames(metadata))
  expect_true("xml_date" %in% colnames(metadata))
  expect_true("xml_language" %in% colnames(metadata))
  expect_true("xml_identifier" %in% colnames(metadata))
  expect_true("xml_bibliographicCitation" %in% colnames(metadata))
  expect_equal(nrow(metadata), 1)

  expect_match(metadata$xml_title[1], "*Māoritanga*")
  expect_match(metadata$xml_creators[1], "Maniapoto Voices.")
  expect_match(metadata$xml_type[1], "Sound Recordings")
  expect_match(metadata$xml_publisher[1], "*Auckland*")
  expect_match(metadata$xml_date[1], "\\[1966\\?\\]")
  expect_match(metadata$xml_language[1], "mao")
  expect_match(metadata$xml_identifier[1], "IE1005520")
  expect_match(metadata$xml_bibliographicCitation[1], "Māoritanga")
})
