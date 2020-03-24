###################################
#                                 #
#  Create time_series_b object    #
#                                 #
###################################

# Note: Estimate multiple statistics for each time series

# Import time_series_b object
time_series_b <- readRDS("data/processed/time_series_b.rds")


# Start cluster for Parallel computations
cl = registerDoSNOW(makeCluster(spec = 6, type = "SOCK"))

stat_total <- foreach(tsi=1:length(time_series_b), .combine='rbind') %dopar% statistics(tsi)

View(stat_total)

# Save stat_total
saveRDS(object = stat_total, file = "data/processed/stat_total.rds")
write.csv(x = stat_total, file = "data/processed/stat_total.rds")
