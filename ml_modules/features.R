#####################################
#                                   #
#   Define features for training    # 
#                                   #
#####################################


## Features ----

features <- c(
  
  # "id",
  
  # "item_id",  "dept_id", "cat_id",
  
  # "store_id", "state_id", 
  
  # "d", 
  
  # "sales",
  
  "wday", "month", "year",
  
  "event_name_1", "event_type_1", 
  
  "event_name_2", "event_type_2",
  
  "snap_CA", "snap_TX", "snap_WI",   
  
  "day_month", "day_quarter", "day_year",
  "week_month", "week_quarter", "week_year", "weekend",
  "month_quarter", "quart", "semester",
  
  "wday_s1", "wday_c1", "wday_s2" , "wday_c2", "wday_s3", "wday_c3",
  "month_s1", "month_c1", "month_s2", "month_c2", "month_s3","month_c3",
  
  "wday_rbf_1", "wday_rbf_2", "wday_rbf_3", "wday_rbf_4", "wday_rbf_5", "wday_rbf_6", "wday_rbf_7",
  
  "month_rbf_1", "month_rbf_2", "month_rbf_3", "month_rbf_4", "month_rbf_5", "month_rbf_6", "month_rbf_7" ,"month_rbf_8", "month_rbf_9",
  "month_rbf_10", "month_rbf_11", "month_rbf_12",
  
  "sporting_event", "cultural_event", "national_event", "religious_event",
  
  "sporting_value", "cultural_value", "national_value", "religious_value",
  
  "has_event", "has_event_lead_t1", "has_event_lead_t2", "has_event_lead_t3", "week_events",
  
  "sell_price", "sell_price_diff", "sell_price_rel_diff", "sell_price_cumrel", "sell_price_roll_sd7",
  
  # "nb_stores", 
  
  "unique_sell_prices", "sell_price_changes", "mean_sell_price", "min_sell_price", "max_sell_price", "max_diff",
  
  "sell_price_to_mean", "sell_price_to_min", "sell_price_to_max", "sell_price_diff_to_max_diff",
  
  "diff_from_mean_price", "rel_diff_from_mean_price", "best_price",
  
  "sell_price_to_min_lead_1", "sell_price_to_min_lead_2", "sell_price_to_max_lead_1", "sell_price_to_max_lead_2",
  
  
  # "total_sales", "total_state_sales", "total_store_sales", "total_cat_sales", "total_dept_sales",           
  # "total_state_cat_sales", "total_state_dept_sales", "total_store_cat_sales", "total_store_dept_sales",   
  # "total_item_sales", "total_item_state_sales",
  
  "snap",
  
  "lngth", "ADI", "CV2", "pz", "Low25", "Mean", "Median", "Up25", "Max", "dollar_sales",
  
  # "Type",
  "type_1", "type_2", "type_3", "type_4",
  
  
  "lag_t1", "lag_t2", "lag_t3", "lag_t4", "lag_t5", "lag_t6", "lag_t7", "lag_t8",
  "lag_t14", "lag_t21", "lag_t28", "lag_t35", "lag_t42", "lag_t49",
  
  "mean_last_month", "mean_last_2_monts", "mean_previous_month",
  
  "rolling_mean_lag1_t7", "rolling_mean_lag1_t120",
  
  "rolling_mean_lag7_t7", "rolling_mean_lag7_t30", "rolling_mean_lag7_t60" ,
  "rolling_mean_lag7_t90", "rolling_mean_lag7_t120", "rolling_mean_lag7_t180", 
  
  "rolling_sd_lag7_t7", "rolling_sd_lag7_t30", 
  "rolling_sum_lag7_t7","rolling_min_lag7_t7",
  
  "rolling_min_lag7_t30", "rolling_max_lag7_t7",
  
  "rolling_max_lag7_t30", "rolling_mean_lag28_t7",
  
  "rolling_mean_lag28_t30", "rolling_mean_lag28_t60", "rolling_mean_lag28_t90", "rolling_mean_lag28_t120", "rolling_mean_lag28_t180" ,   
  "rolling_sd_lag28_t7", "rolling_sd_lag28_t30", 
  "rolling_sum_lag28_t7", 
  
  "rolling_min_lag28_t7", "rolling_min_lag28_t30",
  "rolling_max_lag28_t7", "rolling_max_lag28_t30",
  
  "lag_t7_total_sales", "lag_t7_total_state_sales", "lag_t7_total_store_sales",                  
  "lag_t7_total_cat_sales", "lag_t7_total_dept_sales", "lag_t7_total_state_cat_sales",               
  "lag_t7_total_state_dept_sales", "lag_t7_total_store_cat_sales", "lag_t7_total_store_dept_sales",              
  "lag_t7_total_item_sales", "lag_t7_total_item_state_sales",
  
  "lag_t28_total_sales", "lag_t28_total_state_sales", "lag_t28_total_store_sales", "lag_t28_total_cat_sales",                    
  "lag_t28_total_dept_sales", "lag_t28_total_state_cat_sales", "lag_t28_total_state_dept_sales",             
  "lag_t28_total_store_cat_sales", "lag_t28_total_store_dept_sales", "lag_t28_total_item_sales" ,                  
  "lag_t28_total_item_state_sales",
  
  "mean_last_total_sales", "mean_last_total_state_sales", "mean_last_total_store_sales",
  "mean_last_total_cat_sales", "mean_last_total_dept_sales","mean_last_total_state_cat_sales",
  "mean_last_total_state_dept_sales","mean_last_total_store_cat_sales",            
  "mean_last_total_store_dept_sales", "mean_last_total_item_sales", "mean_last_total_item_state_sales",           
  
  "rolling_mean_lag_t28_total_sales", "rolling_mean_lag_t28_total_state_sales", "rolling_mean_lag_t28_total_store_sales",     
  "rolling_mean_lag_t28_total_cat_sales", "rolling_mean_lag_t28_total_dept_sales", "rolling_mean_lag_t28_total_state_cat_sales", 
  "rolling_mean_lag_t28_total_state_dept_sales" ,"rolling_mean_lag_t28_total_store_cat_sales", "rolling_mean_lag_t28_total_store_dept_sales",
  "rolling_mean_lag_t28_total_item_sales", "rolling_mean_lag_t28_total_item_state_sales",
  
  
  "enc_state_mean", "enc_state_sd", "enc_store_mean", "enc_store_sd", "enc_cat_mean", "enc_cat_sd",           
  "enc_dept_mean", "enc_dept_sd", "enc_state_cat_mean", "enc_state_cat_sd", "enc_state_dept_mean", "enc_state_dept_sd",     
  "enc_store_cat_mean", "enc_store_cat_sd", "enc_store_dept_mean", "enc_store_dept_sd", 
  "enc_item_mean", "enc_item_sd", "enc_item_max",
  "enc_item_state_mean", "enc_item_state_sd", "enc_item_state_max"    
  
  # "orig", "rleLength" 
  
)


# Categoricals ----

categoricals <- c(
  
  # "store_id", "state_id",
  
  "wday", "month", "year",
  
  "event_name_1", "event_type_1", "event_name_2", "event_type_2",
  
  "snap_CA", "snap_TX", "snap_WI",   
  
  "day_month", "day_quarter", "day_year",
  
  "week_month", "week_quarter", "week_year", "weekend",
  
  "month_quarter", "quart", "semester",
  
  "sporting_event", "cultural_event", "national_event", "religious_event",
  
  "has_event", "has_event_lead_t1", "has_event_lead_t2", "has_event_lead_t3", "week_events",
  
  "best_price",
  
  "snap",
  
  # "Type", 
  "type_1", "type_2", "type_3", "type_4"
  
  # "Low25", "Mean", "Median", "Up25", "Max"
  
)
