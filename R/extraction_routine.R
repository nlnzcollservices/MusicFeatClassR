#' This function run feature extraction process.
#'
#' @param label_file_path Path to the label file.
#' @param csv_file_path   Path to the CSV output file.
#' @param root_dir        Root directory for data processing.
#'
#' @export
#'
#' @return list, containing data frame and input parameters
#'
#' @examples
#' label_file_path <- "path/to/label/file.csv"
#' csv_file_path <- "path/to/output.csv"
#' root_dir <- "path/to/root/directory"
#' result <- run_pipeline(label_file_path, csv_file_path, root_dir)
#'
#' @importFrom dplyr filter bind_rows
#'



rpipeline <- function(label_file_path, csv_file_path, root_dir) {

  flag_prev<-FALSE

  if (is.null(label_file_path) ||
      is.null(csv_file_path) || is.null(root_dir)) {
    stop("All paths (label_file_path, csv_file_path, root_dir) must be provided.\n")
  }

  if (!file.exists(label_file_path)) {
    stop("Labels file paths are incorrect. Please verify and try again.\n")
  }

  if (!dir.exists(root_dir)) {
    stop("The music directory does not exist.\n")
  }


  previous_ie <- NA
  flag <- TRUE

  if (file.exists(csv_file_path)) {
    previous_data <- read.csv(csv_file_path, stringsAsFactors = FALSE)
    flag_prev<-TRUE
    if (nrow(previous_data) > 0) {
      previous_ie <- tail(previous_data$"xml_identifier", 1)
      flag <- FALSE
    }
  }

  column_names <-
    c("mms",
      "ie_number",
      "label_code",
      "label1",
      "label2",
      "label3",
      "label4",
      "label5")
  label_data <-
    read.csv(
      label_file_path,
      sep = "|",
      header = FALSE,
      col.names = column_names
    )

  subdirs <- list.files(root_dir, full.names = TRUE)
  feature_data <- data.frame()
  file_types <- c("mp3", "vaw")
  file_type <- basename(root_dir)
  file_type <- "mp3"
  file_type_dir <- root_dir
  file_type_dir
  ie_folders <-
    list.dirs(file_type_dir, full.names = FALSE, recursive = FALSE)
  print(ie_folders)

  for (ie_folder in ie_folders) {
    print(ie_folder)
    ie_number <- rep(ie_folder)
    ie_number -> ie_number_to_find
    print("IE number")

    filtered_row <-
      label_data %>% filter(ie_number == ie_number_to_find)

    if (flag == FALSE && !is.na(previous_ie)) {
      if (ie_number == previous_ie) {
        flag <- TRUE
      }
    }
    if (flag == TRUE) {
      rep_folders <-
        list.dirs(
          file.path(file_type_dir, ie_folder),
          full.names = FALSE,
          recursive = FALSE
        )
      print(rep_folders)
      rep_folder <- rep_folders[1]
      rep_number <- basename(rep_folders[1])
      rep_folders
      xml_file <- file.path(file_type_dir, ie_folder, "ie.xml")
      xml_data <- extract_metadata(xml_file)
      audio_files <-
        list.files(
          file.path(file_type_dir, ie_folder, rep_folder),
          pattern = "\\.(mp3|vaw)$",
          full.names = TRUE
        )
      print(audio_files)

      for (audio_file in audio_files) {

        full_row <- NaN
        feature_table <- extract_audio_features(audio_file)
        print(feature_table)
        feature_row <- data.frame(
          file_type = file_type,
          ie_number = ie_number,
          rep_number = rep_number,
          file_name = basename(audio_file)
        )
        print(feature_table)
        print(feature_row)
        print(xml_data)
        full_row <-
          cbind(feature_row, feature_table, xml_data, filtered_row)
        feature_data <- bind_rows(feature_data, full_row)
        write.csv(feature_data, csv_file_path)
      }
    }
  }
  if (flag_prev == TRUE){
    feature_data<-bind_rows(previous_data, feature_data)
  }
  my_obj <- list(
    label_file_path = label_file_path,
    csv_file_path = csv_file_path,
    root_dir = root_dir,
    feature_data = feature_data
  )

  class(my_obj) <- "rpipeline"
  return (my_obj)

}
