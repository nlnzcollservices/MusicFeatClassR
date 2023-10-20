library(xml2)
library(dplyr)
library(purrr)
library(tuneR)
library(data.table)


csv_file_name<-"extracted_features11.csv"
label_file<-"ies_mms_label.txt"
csv_files_path<-file.path(getwd(), "inst","csv")
csv_file_path<- file.path(csv_files_path, csv_file_name)
label_file_path<-file.path(csv_files_path, label_file)
filesdir <- file.path(getwd(),'tests','music')
filesdir
mydata<-rpipeline(label_file_path, csv_file_path, filesdir)
print(mydata)
summary(mydata)
plot(mydata)
mydata$feature_data[, numeric_vars]
mydata$label_code
mydata$amplitude
mydata$feature_data$amplitude
