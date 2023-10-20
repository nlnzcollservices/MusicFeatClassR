#' XML metadata extraction
#'
#' This function extracts metadata from mets.xml file
#'
#' @param label_file_path Path to the label file.
#' @param csv_file_path   Path to the CSV output file.
#' @param root_dir        Root directory for data processing.
#'
#' @export
#'
#' @return df row of processed data
#'
#' @examples
#' label_file_path <- "path/to/label/file.csv"
#' csv_file_path <- "path/to/output.csv"
#' root_dir <- "path/to/root/directory"
#' result <- run_pipeline(label_file_path, csv_file_path, root_dir)
#'
#' @importFrom dplyr filter bind_rows
#'
extract_metadata <- function(xml_file) {
  xml_doc <- read_xml(xml_file, encoding = "UTF-8")
  xml_records <- xml_doc %>%
    xml_find_all("//mets:dmdSec/mets:mdWrap/mets:xmlData/dc:record")

  xml_data <- data.frame(
    xml_title = character(),
    xml_creators = character(),
    xml_type = character(),
    xml_publisher = character(),
    xml_date = character(),
    xml_language = character(),
    xml_identifier = character(),
    xml_bibliographicCitation = character(),
    stringsAsFactors = FALSE # Set stringsAsFactors to FALSE
  )

  for (xml_record in xml_records) {
    # Clean the xml_data data frame for each record
    xml_data <- data.frame(
      xml_title = character(),
      xml_creators = character(),
      xml_type = character(),
      xml_publisher = character(),
      xml_date = character(),
      xml_language = character(),
      xml_identifier = character(),
      xml_bibliographicCitation = character(),
      stringsAsFactors = FALSE # Set stringsAsFactors to FALSE
    )

    xml_title <- NA
    xml_creators <- NA
    xml_type <- NA
    xml_publisher <- NA
    xml_date <- NA
    xml_language <- NA
    xml_identifier <- NA
    xml_bibliographicCitation <- NA

    if (length(xml_find_all(xml_record, ".//dc:title")) > 0)
      xml_title <- paste(xml_text(xml_find_first(xml_record, ".//dc:title")), collapse = "||")

    if (length(xml_find_all(xml_record, ".//dc:creator")) > 0)
      xml_creators <- paste(xml_text(xml_find_all(xml_record, ".//dc:creator")), collapse = "||")

    if (length(xml_find_all(xml_record, ".//dc:type")) > 0)
      xml_type <- paste(xml_text(xml_find_all(xml_record, ".//dc:type")), collapse = "||")

    if (length(xml_find_all(xml_record, ".//dc:publisher")) > 0)
      xml_publisher <- paste(xml_text(xml_find_all(xml_record, ".//dc:publisher")), collapse = "||")

    if (length(xml_find_all(xml_record, ".//dc:date")) > 0)
      xml_date <- paste(xml_text(xml_find_all(xml_record, ".//dc:date")), collapse = "||")

    if (length(xml_find_all(xml_record, ".//dc:language")) > 0)
      xml_language <- paste(xml_text(xml_find_all(xml_record, ".//dc:language")), collapse = "||")

    if (length(xml_find_all(xml_record, ".//dc:identifier")) > 0)
      xml_identifier <- paste(xml_text(xml_find_all(xml_record, ".//dc:identifier")), collapse = "||")

    if (length(xml_find_all(xml_record, ".//dcterms:bibliographicCitation")) > 0)
      xml_bibliographicCitation <- paste(xml_text(xml_find_all(xml_record, ".//dcterms:bibliographicCitation")), collapse = "||")

    # Create a temporary data frame for the current record
    temp_data <- data.frame(
      xml_title = xml_title,
      xml_creators = xml_creators,
      xml_type = xml_type,
      xml_publisher = xml_publisher,
      xml_date = xml_date,
      xml_language = xml_language,
      xml_identifier = xml_identifier,
      xml_bibliographicCitation = xml_bibliographicCitation,
      stringsAsFactors = FALSE # Set stringsAsFactors to FALSE
    )

    # Append the temporary data frame to the final data frame
    xml_data <- rbind(xml_data, temp_data)
  }

  return(xml_data)
}
