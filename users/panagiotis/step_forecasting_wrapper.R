time_series_b <- readRDS("data/processed/time_series_b.rds")
x <- time_series_b[[6789]]$x
plot(x, type = "l")

length(x)




## 1. rep() h times
input = c(1,2,3,4,5,6,7,1,2,3,4,5,6,7)

RF_local(input = input, fh = 7, ni = 6)

## 2. forecasing h = 1, retrain, foreacasting again

step_forecasting_wrapper <- function(input, fh = 5){
  
  frc <- c()
  for (i in 1:fh) {
    input <- c(input, RF_local(input = input, fh = 1, ni = 6))
  }
  
  return(tail(input, fh))
}

RF_local(input = input, fh = 7, ni = 6)
step_forecasting_wrapper(input = input, fh = 7)




# MLP_local_v2
step_forecasting_wrapper <- function(input, fh = 5){
  
  frc <- c()
  for (i in 1:fh) {
    input <- c(input, MLP_local(input = input, fh = 1, ni = 6))
  }
  
  return(tail(input, fh))
}
