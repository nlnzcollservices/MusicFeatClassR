#' Visualize Confusion Matrix
#'
#' This function visualizes the confusion matrix from classifier results using ggplot2.
#'
#' @param results A list containing classifier results with a confusion_matrix.
#'
#' @return A ggplot2 object representing the confusion matrix visualization.
#'
#' @examples
#' # Example usage:
#' library(caret)
#' cm <- confusionMatrix(factor(c(0, 1, 0, 1)), factor(c(0, 1, 1, 0)))
#' results <- list(
#'   accuracy = 0.85,
#'   confusion_matrix = cm,
#'   trained_model = randomForest(Species ~ ., data = iris)
#' )
#' visualize_confusion_matrix(results)
#'
#' @export
#'
#' @keywords visualization
plot.train_classifier <- function(results) {
  if (!"confusion_matrix" %in% names(results)) {
    stop("Results list must contain a 'confusion_matrix' element.")
  }

  confusion_matrix <- as.data.frame(results$confusion_matrix$table)
  colnames(confusion_matrix) <- c("Actual", "Predicted", "Count")

  ggplot2::ggplot(data = confusion_matrix,
                  ggplot2::aes(x = factor(Actual), y = factor(Predicted))) +
    geom_tile(ggplot2::aes(fill = Count), width = 0.9) +
    geom_text(ggplot2::aes(label = sprintf("%d", Count)), vjust = 1) +
    labs(title = "Confusion Matrix",
         x = "Actual",
         y = "Predicted") +
    scale_fill_gradient(low = "white", high = "skyblue") +
    theme_minimal()
}

#' Visualize Correlation Heatmap
#'
#' This function visualizes the correlation between label_code and other numeric variables in a dataframe.
#'
#' @param dataframe The input dataframe.
#'
#' @return A ggplot2 object representing the correlation heatmap.
#'
#' @examples
#' # Example usage:
#' df <- read.csv("your_data.csv")
#' visualize_correlation(df)
#'
#' @export
#'
#' @keywords visualization
#'
plot.rpipeline <- function(mydata) {
  # Select the relevant columns
  dataframe <-mydata$feature_data
  numeric_vars <- c("amplitude", "duration", "sample_rate", "num_channels")
  df_numeric <- dataframe[, numeric_vars]

  # Calculate the correlation matrix
  correlation_matrix <- cor(df_numeric)

  # Create a ggplot2 heatmap
  ggplot2::ggplot(data = melt(correlation_matrix), ggplot2::aes(x = Var1, y = Var2, fill = value)) +
    ggplot2::geom_tile() +
    ggplot2::scale_fill_gradient2(low = "skyblue3", mid = "white", high = "darkred", midpoint = 0) +
    ggplot2::labs(title = "Correlation Heatmap",
                  x = "Variable",
                  y = "Variable",
                  fill = "Correlation") +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
}

