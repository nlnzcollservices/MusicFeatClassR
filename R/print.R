# Print Method for rpipeline
#'
#' This custom print method displays the details of an rpipeline object.
#'
#' @param x An object of class rpipeline.
#' @param ... Additional arguments (not used).
#'
#' @export
#'
#' @keywords data
print.rpipeline <- function(x, ...) {
  cat("Printing paths:\n")
  cat("Label File Path:", x$label_file_path, "\n")
  cat("CSV File Path:", x$csv_file_path, "\n")
  cat("Root Directory:", x$root_dir, "\n")
  cat("\n")
  invisible()
}

#' Print method for Classifier Results
#'
#' This custom print method displays predictions.
#'
#' @param results A list containing the classifier results, including accuracy, confusion matrix, and trained model.
#'
#' @return None (the function prints details to the console).
#'
#' @keywords print
#'
print.train_classifier <- function(results) {
  cat("\nPredictions:\n", file = stdout())
  print(head(results$predictions, 30), 30)
}


