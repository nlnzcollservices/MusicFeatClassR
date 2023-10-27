my_predictor <- function(music_dir, trained_model) {
  if (is.null(music_dir)) {
    stop("The root directory path must be provided.")
  }

  if (!dir.exists(music_dir)) {
    stop("The music directory does not exist.")
  }

  if (otype(trained_model)!="S3") {
    stop("The 'trained_model' must be a S3 class object.")
  }

  ie_folder <- music_dir
  ie_number <- basename(ie_folder)
  rep_folder <- list.dirs(music_dir)
  rep_number <- basename(list.dirs(music_dir))[3]
  xml_file <- file.path(ie_folder, "ie.xml")
  xml_data <- extract_metadata(xml_file)
  audio_files <-
    list.files(
      file.path(rep_folder),
      pattern = "\\.(mp3|vaw)$",
      full.names = TRUE
    )


  feature_rows <- data.frame()

  for (audio_file in audio_files) {
    feature_table_audio <- extract_audio_features(audio_file)
    feature_table_xml <- extract_metadata(xml_file)

    # Merge audio and XML features
    feature_table <- cbind(feature_table_audio, feature_table_xml)

    # Predict the label code using the trained model
    label_code <- predict(trained_model, newdata = feature_table)

    # Create a data frame with the extracted data for the current audio file
    feature_row <- data.frame(
      ie_num = ie_number,
      title = feature_table$xml_title,
      audio_file = audio_file,
      label_code = label_code
    )

      feature_rows <- rbind(feature_rows, feature_row)
  }

  return(feature_rows)
}
