#####################################################################
#                                                                   #
#  M5 accuracy: mlr3learners.lightgbm R package [Score: 0.60389]    #
#                                                                   #
#####################################################################








# I am introducing here the mlr3learners.lightgbm R package https://github.com/mlr3learners/mlr3learners.lightgbm
# It supports also GPU; more information on how it works can be found in the package vignettes 

# Some ideas and code were taken from MichaelMayers great notebook M5 Forecast: attack of the data.table.
# I am a big fan of the data.table R package and using it for quite a while now, however,
# MichaelMayers introduced some nice data wrangling tricks with data.table I wasn't aware of yet.
# If you like this work or even fork this notebook, please be so kind and give an upvote!

## 01. Data Import ----

datadir <- "../input/m5-forecasting-accuracy/"

# read csv files
for (i in list.files(datadir, pattern = "\\.csv$")) {
  assign(
    x = i,
    value = data.table::fread(
      file = paste0(datadir, i),
      header = T,
      stringsAsFactors = T
    )
  )
}

## 02. Merging/ Feature creation ----

# add validation dummies
sales_train_validation.csv[, (paste0("d_", 1914:1969)) := NA_integer_]

dataset <- data.table::melt.data.table(
  data = sales_train_validation.csv,
  id.vars = colnames(sales_train_validation.csv)[grepl("id", colnames(sales_train_validation.csv))],
  measure.vars = colnames(sales_train_validation.csv)[grepl("^d_", colnames(sales_train_validation.csv))],
  variable.name = "d",
  value.name = "demand",
  na.rm = FALSE
)
rm(sales_train_validation.csv)
gc()

# make data smaller
dataset[, ("d") := as.integer(gsub("^d_", "", get("d")))]
gc()

# ensure order
data.table::setorder(
  x = dataset,
  cols = "d"
)
gc()


# define indices for prediction/evaluation, training and testing set
pr <- 1913
eva <- 1941
tra <- (pr - 1.5 * 366) # use the last 1.5 yrs for training
val <- (tra - 90) # use the quater year before for validation

# reduce data
dataset <- dataset[get("d") >= val, ]
gc();gc()


stopifnot(!is.unsorted(dataset$d))

calendar.csv[, `:=` (weekend = ifelse(get("weekday") %in% c("Saturday", "Sunday"), 1L, 0L),
                     # create weekend feature
                     d = as.integer(gsub("^d_", "", get("d"))),
                     day = as.integer(substr(get("date"), start = 9, stop = 10)),
                     date = factor(as.Date(get("date"))),
                     event_name_1 = as.integer(factor(get("event_name_1"))),
                     event_type_1 = as.integer(factor(get("event_type_1"))),
                     event_name_2 = as.integer(factor(get("event_name_2"))),
                     event_type_2 = as.integer(factor(get("event_type_2"))))][
                       , `:=` (date = NULL,
                               weekday = NULL)
                       ]

# merge calendar to dataset
dataset <- calendar.csv[get("d") >= val, ][dataset, on = "d"]
rm(calendar.csv)
gc()


# create some more features
sell_prices.csv[, `:=` (sell_price_rel_lag1 = (sell_price / dplyr::lag(sell_price) - 1),
                        sell_price_rel_lag7 = (sell_price / dplyr::lag(sell_price, 7) - 1),
                        sell_price_rel_lag30 = (sell_price / dplyr::lag(sell_price, 30) - 1),
                        sell_price_sdr7 = (RcppRoll::roll_sdr(sell_price, n = 7))
), by = c("store_id", "item_id")]
gc()


# merge prices to dataset
dataset <- sell_prices.csv[dataset, on = c("store_id", "item_id", "wm_yr_wk")][, wm_yr_wk := NULL]
rm(sell_prices.csv)
gc()

# create more features (slightly modified code from https://www.kaggle.com/mayer79/m5-forecast-attack-of-the-data-table)
agg <- dataset[, .(agg_mean = mean(demand, na.rm = TRUE)), keyby = c("item_id", "d")]
agg[, lag_t28 := dplyr::lag(agg_mean, 28), keyby = "item_id"
    ][
      , rolling_mean_t30r := RcppRoll::roll_meanr(lag_t28, 30), keyby = "item_id"
      ][
        , rolling_sd_t30r := RcppRoll::roll_sdr(lag_t28, 30), keyby = "item_id"
        ]
dataset[agg, `:=` (rolling_mean_t30r_item = i.rolling_mean_t30r,
                   rolling_sd_t30r_item = i.rolling_sd_t30r
), on = c("item_id", "d")]
rm(agg)
gc()


## 03. Install the mlr3learners.lightgbm package from github ----


devtools::install_github("mlr3learners/mlr3learners.lightgbm", upgrade = "never")

library(mlr3)
library(mlr3learners.lightgbm)


## 04. Prepare and train the learner


nrounds <- 6500

dim(dataset)

split <- list()
# 1.5 years training
split$train_index <- which(dataset[, get("d")] >= tra &
                             dataset[, get("d")] <= pr)
# 1/4 year validation
split$validation_index <- which(dataset[, get("d")] >= val &
                                  dataset[, get("d")] < tra)
split$test_index <- which(dataset[, get("d")] > pr &
                            dataset[, get("d")] <= eva)
split$evaluation_index <- which(dataset[, get("d")] > eva)

# create the task
vec <- setdiff(colnames(dataset), c("item_id", "id"))
task <- mlr3::TaskRegr$new(
  id = "m5",
  backend = dataset[, (vec), with = F],
  target = "demand"
)

# get sets with test/validati and evaluation ids
test <- dataset[split$test_index, c("id", "d", "demand"), with = F]
evaluation <- dataset[split$evaluation_index, c("id", "d", "demand"), with = F]
rm(dataset)
gc()

# instanciate the learner
learner <- mlr3::lrn("regr.lightgbm")

# define learning arguments
learner$nrounds <- nrounds
# switch off inner cross validation
learner$lgb_learner$nrounds_by_cv <- FALSE

# define learner parameters  
learner$param_set$values <- list(
  "objective" = "regression"
  ,"learning_rate" = 0.1
  , "seed" = 17L
  , "metric" = "rmse"
  , "bagging_fraction" = 0.3
  , "bagging_freq" = 5
  , "feature_fraction" = 0.7
  , "force_col_wise" = TRUE
  #, "device_type" = "gpu"
  #, "max_bin" = 63L
  #, "num_threads" = 6L
)

# train the learner
learner$train(task, row_ids = split$train_index)
gc()

learner$model$current_iter()


## 04. Evaluate the learner on validation set ----


learner$importance()
imp <- learner$lgb_learner$importance2()
imp$importance
imp$plot

predictions <- learner$predict(task, row_ids = split$validation_index)
head(predictions$response)

predictions$score(mlr3::msr("regr.rmse"))


## 05. Predict final sets ----

# add test (= M5 validation) predictions
predictions <- learner$predict(task, row_ids = split$test_index)
preds <- ifelse(
  predictions$response < 0, 0, predictions$response
)

test[, ("demand") := preds]
gc()

predictions <- learner$predict(task, row_ids = split$evaluation_index)
preds <- ifelse(
  predictions$response < 0, 0, predictions$response
)
evaluation[, ("demand") := preds]
gc()

# make data wide again
submit <- data.table::dcast.data.table(
  data = test[, ("demand") := round(get("demand"), 6)],
  formula = id ~ d,
  value.var = "demand"
)
colnames(submit)[2:ncol(submit)] <- paste0("F", 1:(ncol(submit) - 1))

submit2 <- data.table::dcast.data.table(
  data = evaluation[, ("demand") := round(get("demand"), 6)],
  formula = id ~ d,
  value.var = "demand"
)
colnames(submit2)[2:ncol(submit2)] <- paste0("F", 1:(ncol(submit2) - 1))
submit2[, ("id") := gsub("validation", "evaluation", get("id"))]

data.table::fwrite(
  rbind(submit, submit2),
  paste0("submit_",
         Sys.Date(), "_", gsub(":", "", substr(Sys.time(),
                                               start = 12,
                                               stop = 19)), ".csv"))

