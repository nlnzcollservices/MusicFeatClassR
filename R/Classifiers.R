#' Train Classifier
#'
#' This function trains a random forest classifier on a given dataset and returns multiple results.
#'
#' @param feature_data The input dataframe containing the dataset.
#' @param target_variable The name of the target variable.
#' @param classif Classifier name to use.
#' @param train_percentage The percentage of data to be used for training (default is 0.7).
#'
#' @return A list containing the accuracy, confusion matrix, and trained model.
#'
#' @examples
#'feature_data <- read.csv("data.csv")
#' target_variable <- "label_code"
#' calssif<-"RandomForest"#(or "LogisticRegression")
#' results <- train_random_forest_classifier(feature_data, target_variable)
#'
#' @importFrom randomForest randomForest
#' @importFrom caret createDataPartition confusionMatrix
#'
#' @export
#'
#' @keywords machine learning
#'
train_classifier <-
  function(feature_data,
           target_variable,
           classif,
           train_percentage = 0.7,
           save_trained_model=TRUE) {
           train_percentage = 0.7) {
    if (!is.data.frame(feature_data)) {
      stop("Input 'dataframe' must be a data frame.")
    }
    if (nrow(feature_data) == 0) {
      stop("The data frame is empty.")
    }

    if (!is.character(target_variable)){
      stop("Input 'target_variable' must be a character string.")


    }

    supported_classifiers <- c("RandomForest", "LogisticRegression","SupportVectorMachine","NaiveBayes")
    dataframe_filtered <-
      feature_data[complete.cases(feature_data[, "label_code"]),]
    dataframe_filtered <-
      dataframe_filtered[dataframe_filtered[, "label_code"] != "",]

    target <- dataframe_filtered[, "label_code"]
    class_counts <- table(target)

    class_counts_filtered <- class_counts[class_counts > 30]
    names(class_counts_filtered)
    dataframe_filtered <-
      dataframe_filtered[dataframe_filtered$label_code %in% names(class_counts_filtered),]
    dataframe_filtered$xml_date <-
      str_extract(dataframe_filtered$xml_date, "\\d{4}")
    dataframe_filtered$xml_date <-
      as.factor(dataframe_filtered$xml_date)
    target <- dataframe_filtered[, "label_code"]

    my_data_set <-
      dataframe_filtered[, c("sample_rate", "duration", "num_channels", "amplitude")]

    train_indices <-
      createDataPartition(target, p = train_percentage, list = FALSE)
    train_data <- my_data_set[train_indices,]
    test_data <- my_data_set[-train_indices,]
    train_target <- target[train_indices]
    test_target <- target[-train_indices]

    train_target <- as.factor(train_target)
    test_target <- as.factor(test_target)

    if (classif %in% supported_classifiers) {
      if (classif == "RandomForest") {
        my_model <- randomForest(train_target ~ ., data = train_data)
      } else if (classif == "SupportVectorMachine") {
        my_model <-
          svm(train_target ~ ., data = train_data, kernel = "linear")
      } else if (classif == "LogisticRegression") {
      my_model <-
        multinom(train_target ~ ., data = train_data)
      } else if (classif == "NaiveBayes") {
        my_model <-
          naiveBayes(train_target ~ ., data = train_data)
      }

      #   # ...
      #   # Here place for other classifiers
      #   # ...



      predictions <- predict(my_model, newdata = test_data)
      confusion_matrix <- confusionMatrix(predictions, test_target)
      confusion_table <- confusion_matrix$table
      accuracy <- sum(diag(confusion_table)) / sum(confusion_table)


      results <- list(
        accuracy = accuracy,
        confusion_matrix = confusion_matrix,
        trained_model = my_model,
        predictions = predictions
      )
      class(results) <- "train_classifier"
      return(results)
     } else {

       stop(paste(classif, "is not supported. Avilable classifiers are ", paste(supported_classifiers, collapse=", "), "."))
   }
  }

