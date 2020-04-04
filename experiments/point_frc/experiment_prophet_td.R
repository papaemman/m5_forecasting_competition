######################################################
#                                                    #
#  Experiment: Prophet facebook package / Top down   #
#                                                    #
######################################################

# Source dependencies 
lapply(X = paste0("modules/", list.files(path = "modules/")), FUN = source)


## Define methods names to be passed as argument to wrapper_frc_methods() function
b_names <- c("prophet")


## Import datasets
sales <- read.csv("data/raw/sales_train_validation.csv")
calendar <- read.csv("data/raw/calendar.csv")

time_series_b <- readRDS(file = "data/processed/time_series_b_uncertainty.rds")
# time_series_b <- readRDS(file = "data/processed/time_series_b.rds")


stat_total <- readRDS("data/processed/stat_total_complete.rds")
head(stat_total)


## Aggregation level 1 ----


tsid <- stat_total %>% filter(Hlevel==1) %>% pull(tsid) # Get tsid for aggregation level 1

frc <- prophet_frc(x = time_series_b[[tsid]]$x, h = 28)

plot_forecastings_one_ts(time_series = time_series_b[[1]]$x, frc = frc)


## Aggregation level 2 ----

tsids <- stat_total %>% filter(Hlevel==2) %>% pull(tsid) # Get tsid for aggregation level 1
tsids

frc <- lapply(X = c(tsids[1]:tsids[length(tsids)]), 
              FUN = function(x) {prophet_frc(x = time_series_b[[x]]$x, h = 28)})


plot_forecastings_one_ts(time_series = time_series_b[[2]]$x, frc = frc[[1]])


## Aggregation level 3 ----
tsids <- stat_total %>% filter(Hlevel==3) %>% pull(tsid) # Get tsid for aggregation level 1
tsids

frc <- lapply(X = c(tsids[1]:tsids[length(tsids)]), 
              FUN = function(x) {prophet_frc(x = time_series_b[[x]]$x, h = 28)})

length(frc)

id <- 5
plot_forecastings_one_ts(time_series = time_series_b[[tsids[id]]]$x, frc = frc[[id]])





