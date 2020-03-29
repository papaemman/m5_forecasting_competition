#############################
#                           #
#      Main R script        #
#                           #
#############################


## 00. Load packages ----

# Utils
library(tictoc)

# Data wrangling
library(plyr)
library(dplyr)

# Parallel computations
library(doSNOW)
library(foreach)

# Time series manipulations and forecasting
library(zoo)
library(forecast)
library(smooth)
library(prophet)

# Machine Learning
library(randomForest)
library(RSNNS)

# Visualization
library(ggplot2)
library(plotly)


## 01. Define global variables ----
opts <- list()

opts$user_name <- "panagiotis"




