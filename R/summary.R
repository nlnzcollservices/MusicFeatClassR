# Custom Summary Function for rpipeline
#'
#' This custom summary function provides a summary of an rpipeline object.
#'
#' @param object An object of class rpipeline.
#' @param ... Additional arguments (not used).
#'
#' @return A data frame containing a summary of the rpipeline object.
#'
#' @export
#'
#' @keywords data
summary.rpipeline <- function(object, ...) {
  cat("Summary of rpipeline object\n")
  print(summary(object$feature_data))
  cat("\n")
  invisible()
}

#' Summary Function for Random Forest Classifier Results
#'
#' This custom summary function provides a summary of the results from a random forest classifier, including accuracy,
#' the confusion matrix, and the trained model.
#'
#' @param results A list containing the classifier results, including accuracy, confusion matrix, and trained model.
#'
#' @return None (the function prints the summary to the console).
#'
#' @keywords summary
summary.train_classifier <- function(results) {
  cat("Summary of Random Forest Classifier Results\n")
  cat("\nAccuracy:", results$accuracy, "\n")

  cat("\nConfusion Matrix:\n")
  print(results$confusion_matrix)

  cat("\nTrained Model Summary:\n")
  print(results$trained_model)
}

