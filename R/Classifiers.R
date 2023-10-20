  #' Train Classifier
  #'
  #' This function trains a random forest classifier on a given dataset and returns multiple results.
  #'
  #' @param feature_data The input dataframe containing the dataset.
  #' @param classif Classifier name to use.
  #' @param train_percentage The percentage of data to be used for training (default is 0.7).
  #'
  #' @return A list containing the accuracy, confusion matrix, and trained model.
  #'
  #' @examples
  #'feature_data <- read.csv("data.csv")
  #' classif<-"RandomForest"#(or "LogisticRegression")
  #' results <- train_classifier(feature_data, classifier, classifier parameters)
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
             classif,
             classif_params =list(),
             train_percentage = 0.7) {
      if (!is.data.frame(feature_data)) {
        stop("Input 'dataframe' must be a data frame.")
      }
      if (nrow(feature_data) == 0) {
        stop("The data frame is empty.")
      }

      if (!is.list(classif_params)) {
        stop("Input 'classif_params' must be a list.")
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
          my_model <- do.call(randomForest, c(list(formula = train_target ~ ., data = train_data), classif_params))
        } else if (classif == "SupportVectorMachine") {
          my_model <- do.call(svm, c(list(formula = train_target ~ ., data = train_data), classif_params))
        } else if (classif == "LogisticRegression") {
          my_model <- do.call(multinom, c(list(formula = train_target ~ ., data = train_data), classif_params))
        } else if (classif == "NaiveBayes") {
          my_model <- do.call(naiveBayes, c(list(formula = train_target ~ ., data = train_data), classif_params))

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


