################################################################
#                                                              #
# Time Series Machine Learning (and Feature Engineering) in R  #
#                                                              #
################################################################

# Tutorial: https://www.business-science.io/time-series/2020/03/18/time-series-machine-learning.html

## 00. Prerequisites ----
Sys.setlocale("LC_TIME", "C")
sessionInfo()

library(workflows)
library(parsnip)
library(recipes)
library(yardstick)
library(glmnet)
library(tidyverse)
library(tidyquant)
library(timetk) # Use >= 0.1.3 version


## 01. Data ----

# Read data
bikes <- read_csv(file = "data/additional_data/Bike-Sharing-Dataset/day.csv")
head(bikes)

# Select date and count
bikes_tbl <- bikes %>%
  select(dteday, cnt) %>%
  rename(date  = dteday, value = cnt)

# Visualize data and training/testing regions
bikes_tbl %>%
  ggplot(aes(x = date, y = value)) +
  geom_rect(xmin = as.numeric(ymd("2012-07-01")),
            xmax = as.numeric(ymd("2013-01-01")),
            ymin = 0, ymax = 10000,
            fill = palette_light()[[4]], alpha = 0.01) +
  annotate("text", x = ymd("2011-10-01"), y = 7800,
           color = palette_light()[[1]], label = "Train Region") +
  annotate("text", x = ymd("2012-10-01"), y = 1550,
           color = palette_light()[[1]], label = "Test Region") +
  geom_point(alpha = 0.5, color = palette_light()[[1]]) +
  labs(title = "Bikes Sharing Dataset: Daily Scale", x = "") +
  theme_tq()

# Note: A visualization will help understand how we plan to tackle the problem of forecasting the data.
# We’ll split the data into two regions: a training region and a testing region.

## 02. Modeling ----

# Split into training and test sets
train_tbl <- bikes_tbl %>% filter(date < ymd("2012-07-01"))
test_tbl  <- bikes_tbl %>% filter(date >= ymd("2012-07-01"))

dim(train_tbl)
dim(test_tbl)

train_tbl

## Recipe Preprocessing Specification

# Add time series signature
recipe_spec_timeseries <- recipes::recipe(value ~ ., data = train_tbl) %>%
  timetk::step_timeseries_signature(date) 

# Prepare and Bake the recipe
temp_train_tbl <- bake(prep(recipe_spec_timeseries), new_data = train_tbl)
dim(temp_train_tbl)
str(temp_train_data)
View(temp_train_tbl)

# Note: When we apply the prepared recipe prep() using the bake() function, we go from 2 features to 29 features! 
# Yes, 25+ new columns were added from the timestamp “date” feature. 


## 03. Building Engineered Features on Top of our Recipe ----

# Next is where the magic happens. 
# I apply various preprocessing steps to improve the modeling behavior to go from 29 features to 225 engineered features!


recipe_spec_final <- recipe_spec_timeseries %>%
  step_rm(date) %>%
  step_rm(contains("iso"), 
          contains("second"), contains("minute"), contains("hour"),
          contains("am.pm"), contains("xts")) %>%
  step_normalize(contains("index.num"), date_year) %>%
  step_interact(~ date_month.lbl * date_day) %>%
  step_interact(~ date_month.lbl * date_mweek) %>%
  step_interact(~ date_month.lbl * date_wday.lbl * date_yday) %>%
  step_dummy(contains("lbl"), one_hot = TRUE) 

temp_train_data <- bake(prep(recipe_spec_final), new_data = train_tbl)
dim(temp_train_data)
View(temp_train_data)


## 04. Model specification ----
model_spec_glmnet <- linear_reg(mode = "regression", penalty = 10, mixture = 0.7) %>%
  set_engine("glmnet")


## 05. Workflow ----

workflow_glmnet <- workflow() %>%
  add_recipe(recipe_spec_final) %>%
  add_model(model_spec_glmnet)

workflow_glmnet


## 06. Training ----

workflow_trained <- workflow_glmnet %>% fit(data = train_tbl)


## 07. Visualize the Test (Validation) Forecast ----

prediction_tbl <- workflow_trained %>% 
  predict(test_tbl) %>%
  bind_cols(test_tbl) 

prediction_tbl

## Visualize the results

ggplot(aes(x = date), data = bikes_tbl) +
  geom_rect(xmin = as.numeric(ymd("2012-07-01")),
            xmax = as.numeric(ymd("2013-01-01")),
            ymin = 0, ymax = 10000,
            fill = palette_light()[[4]], alpha = 0.01) +
  annotate("text", x = ymd("2011-10-01"), y = 7800,
           color = palette_light()[[1]], label = "Train Region") +
  annotate("text", x = ymd("2012-10-01"), y = 1550,
           color = palette_light()[[1]], label = "Test Region") + 
  geom_point(aes(x = date, y = value),  
             alpha = 0.5, color = palette_light()[[1]]) +
  # Add predictions
  geom_point(aes(x = date, y = .pred), data = prediction_tbl, 
             alpha = 0.5, color = palette_light()[[2]]) +
  theme_tq() +
  labs(title = "GLM: Out-Of-Sample Forecast")


## 08. Validation Accuracy (Out of Sample) ----

# Calculating forecast error
prediction_tbl %>% metrics(value, .pred)


# Visualize the residuals

prediction_tbl %>%
  ggplot(aes(x = date, y = value - .pred)) +
  geom_hline(yintercept = 0, color = "black") +
  geom_point(color = palette_light()[[1]], alpha = 0.5) +
  geom_smooth(span = 0.05, color = "red") +
  geom_smooth(span = 1.00, se = FALSE) +
  theme_tq() +
  labs(title = "GLM Model Residuals, Out-of-Sample", x = "") +
  scale_y_continuous(limits = c(-5000, 5000))

# Note: The residuals show that the model predicts low in October and high in December.

## Next steps:
# At this point you might go back to the model and try tweaking features using interactions or polynomial terms,
# adding other features that may be known in the future (e.g. temperature of day can be forecasted relatively accurately within 7 days),
# or try a completely different modeling technique with the hope of better predictions on the test set.


## 09. Forecasting future data ----

# Let’s use our model to predict what are the expected future values for the next six months.
# The first step is to create the date sequence. 
# Let’s use tk_get_timeseries_summary() to review the summary of the dates from the original dataset, “bikes”.

# Extract bikes index
idx <- bikes_tbl %>% timetk::tk_index()

# Get time series summary from index (general summary and periodicity information)
bikes_summary <- idx %>% timetk::tk_get_timeseries_summary()


# Note:
# From the summary, we know that the data is 100% regular because the median and mean differences are 86400 seconds or 1 day.
# We don’t need to do any special inspections when we use tk_make_future_timeseries(). 
# If the data was irregular, meaning weekends or holidays were excluded, you’d want to account for this. 
# Otherwise your forecast would be inaccurate.

## Create the future observations
idx_future <- idx %>% tk_make_future_timeseries(n_future = 180)
future_tbl <- tibble(date = idx_future) 
future_tbl


## Retrain the model specification on the full data set, then predict the next 6-months.

future_predictions_tbl <- workflow_glmnet %>% 
  fit(data = bikes_tbl) %>%
  predict(future_tbl) %>%
  bind_cols(future_tbl)

 
## Visualize the forecast.

bikes_tbl %>%
  ggplot(aes(x = date, y = value)) +
  geom_rect(xmin = as.numeric(ymd("2012-07-01")),
            xmax = as.numeric(ymd("2013-01-01")),
            ymin = 0, ymax = 10000,
            fill = palette_light()[[4]], alpha = 0.01) +
  geom_rect(xmin = as.numeric(ymd("2013-01-01")),
            xmax = as.numeric(ymd("2013-07-01")),
            ymin = 0, ymax = 10000,
            fill = palette_light()[[3]], alpha = 0.01) +
  annotate("text", x = ymd("2011-10-01"), y = 7800,
           color = palette_light()[[1]], label = "Train Region") +
  annotate("text", x = ymd("2012-10-01"), y = 1550,
           color = palette_light()[[1]], label = "Test Region") +
  annotate("text", x = ymd("2013-4-01"), y = 1550,
           color = palette_light()[[1]], label = "Forecast Region") +
  geom_point(alpha = 0.5, color = palette_light()[[1]]) +
  # future data
  geom_point(aes(x = date, y = .pred), data = future_predictions_tbl,
             alpha = 0.5, color = palette_light()[[2]]) +
  geom_smooth(aes(x = date, y = .pred), data = future_predictions_tbl,
              method = 'loess') + 
  labs(title = "Bikes Sharing Dataset: 6-Month Forecast", x = "") +
  theme_tq()



## 10. Forecast Error ----

# A forecast is never perfect.
# We need prediction intervals to account for the variance from the model predictions to the actual data.

# Calculate standard deviation of residuals
test_resid_sd <- prediction_tbl %>%
  summarize(stdev = sd(value - .pred))

future_predictions_tbl <- future_predictions_tbl %>%
  mutate(
    lo.95 = .pred - 1.96 * test_resid_sd$stdev,
    lo.80 = .pred - 1.28 * test_resid_sd$stdev,
    hi.80 = .pred + 1.28 * test_resid_sd$stdev,
    hi.95 = .pred + 1.96 * test_resid_sd$stdev
  )


# Plot the forecast with the prediction intervals
bikes_tbl %>%
  ggplot(aes(x = date, y = value)) +
  geom_point(alpha = 0.5, color = palette_light()[[1]]) +
  geom_ribbon(aes(y = .pred, ymin = lo.95, ymax = hi.95), 
              data = future_predictions_tbl, 
              fill = "#D5DBFF", color = NA, size = 0) +
  geom_ribbon(aes(y = .pred, ymin = lo.80, ymax = hi.80, fill = key), 
              data = future_predictions_tbl,
              fill = "#596DD5", color = NA, size = 0, alpha = 0.8) +
  geom_point(aes(x = date, y = .pred), data = future_predictions_tbl,
             alpha = 0.5, color = palette_light()[[2]]) +
  geom_smooth(aes(x = date, y = .pred), data = future_predictions_tbl,
              method = 'loess', color = "white") + 
  labs(title = "Bikes Sharing Dataset: 6-Month Forecast with Prediction Intervals", x = "") +
  theme_tq()






