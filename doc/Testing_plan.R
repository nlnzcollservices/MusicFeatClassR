## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----warning=FALSE------------------------------------------------------------
library(MusicFeatClassR)
library(testthat)
current_dir<-getwd()
root_dir<-dirname(current_dir)
test_dir(file.path(root_dir,"tests"))

