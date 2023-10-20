library(randomForest)
library(caret)
library(data.table)
library(stringr)
library(e1071)
library(nnet)
library(Rcpp)

csv_file_name<-"extracted_features_original.csv"
csv_files_path<-file.path(getwd(), "inst","csv")
csv_file_path<-file.path(csv_files_path, csv_file_name)
feature_data<-read.csv(csv_file_path)
result <- train_classifier(feature_data, target_variable = "label_code",classif = "RandomForest")
print(result)
summary(result)
names(result)
sloop::otype(result)
result$accuracy
result$trained_model
result$confusion_matrix$table
#sourceCpp(file.path(getwd(),"src","get_metrics.cpp"))
classificationMetrics(result$confusion_matrix$table)
plot(result)
