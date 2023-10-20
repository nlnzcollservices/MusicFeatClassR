## ----setup, include = FALSE, warning = FALSE----------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----message = FALSE, warning=FALSE-------------------------------------------
library(xml2)
library(dplyr)
library(purrr)
library(tuneR)
library(data.table)
library(reshape2)
library(MusicFeatClassR) # the package


## -----------------------------------------------------------------------------
csv_file_name<-"extracted_features.csv"
label_file<-"ies_mms_label.txt"
vignettes_dir<-getwd()
parent_dir<-dirname(vignettes_dir)

csv_files_path<-file.path(parent_dir, "inst","csv")
csv_file_path<- file.path(csv_files_path, csv_file_name)
label_file_path<-file.path(csv_files_path, label_file)
filesdir <- file.path(parent_dir,'files','mp3')

## -----------------------------------------------------------------------------
mydata<-rpipeline(label_file_path, csv_file_path, filesdir)
head(mydata)


## -----------------------------------------------------------------------------

print(mydata)


## -----------------------------------------------------------------------------

summary(mydata)


## -----------------------------------------------------------------------------

plot(mydata)


## ----message = FALSE, warning=FALSE-------------------------------------------
library(randomForest)
library(caret)
library(data.table)
library(stringr)
library(e1071)
library(nnet)
library(Rcpp)
library(MusicFeatClassR)
csv_file_name<-"extracted_features_original.csv"
vignettes_dir<-getwd()
parent_dir<-dirname(vignettes_dir)
csv_files_path<-file.path(parent_dir, "inst","csv")
csv_file_path<-file.path(csv_files_path, csv_file_name)
feature_data<-read.csv(csv_file_path)
result <- train_classifier(feature_data ,classif = "RandomForest")
model_path<-file.path(parent_dir, "inst", "model.rds")
#the model can be saved
saveRDS(result$trained_model, file = model_path)


## -----------------------------------------------------------------------------

print(result)


## -----------------------------------------------------------------------------
summary(result)

## -----------------------------------------------------------------------------

plot(result)


## -----------------------------------------------------------------------------

#sourceCpp(parent_dir,"src","get_metrics.cpp")
classificationMetrics(result$confusion_matrix$table)


## -----------------------------------------------------------------------------

