#' Extract features from mp3 file
#'
#' @param audio_file Path to the audio file
#'
#' @export
#'
#' @return A matrix of audio features
#'
# Function to extract audio features from music file
extract_audio_features <- function(file_path) {

  audio <- readMP3(file_path)

  features <- data.table(
    duration = length(audio) / audio@samp.rate,
    sample_rate = audio@samp.rate,
    num_channels = length(audio),
    amplitude = max(abs(audio@left))
  )

  return(features)
}


