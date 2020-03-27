####################################################
#                                                  #
#  Export forecastings file in kaggle's format     #
#                                                  # 
####################################################


## Note: Example dataset with forecastings (frc_total) to pass as argument in evaluate_experiment(frc_total, b_names) function:

## Forecastings
# frc_total <- readRDS("data/forecastings/frc_top_12_item_ids.RDS")
# View(frc_total)

## benchmark names
# b_names <- c("Naive", "sNaive", "SES", "MA", 
#              "Croston", "optCroston","SBA", "TSB", 
#              "ADIDA", "iMAPA",
#              "ES_bu", "ARIMA_bu",
#              "MLP_l", "RF_l", "MLP_g", "RF_g",
#              "ES_td","ESX","ARIMA_td","ARIMAX", # ARIMAX can't fit in the data (remove it from the b_names vector )
#              "Com_b","Com_t","Com_tb","Com_lg")

# b_names <- setdiff(colnames(frc_total), c("item_id", "dept_id", "cat_id", "store_id", "state_id", "fh"))



## 01. Function: Make submission files (as many as the columns in forcastings datasets) and saves them in data/submissions directory.

make_submission_files <- function(frc_total, b_names, submission_file_name_prefix = "BF"){
  
  # Message
  # b_names <- setdiff(colnames(frc_total), c("item_id", "dept_id", "cat_id", "store_id", "state_id", "fh"))
  
  print(paste("There are forecastings for", length(b_names), "methods in this dataset."))
  print(paste(length(b_names), " submission files will be created!"))
  
  
  # For every method (mid) in frc_total (column), create a submission file with the appropriate format
  for (mid in 1:length(b_names)){ # mid: method id
    
    # Get submission file for method id
    submission <- frc_total[,c("item_id", "store_id", b_names[mid], "fh")]
    colnames(submission)[1:2] <- c("Agg_Level_1", "Agg_Level_2")
    
    # Create new columns F1, F2, ..., F28
    submission$F7 = submission$F6 = submission$F5 = submission$F4 = submission$F3 = submission$F2 = submission$F1 <- NA
    submission$F14 = submission$F13 = submission$F12 = submission$F11 = submission$F10 = submission$F9 = submission$F8 <- NA
    submission$F21 = submission$F20 = submission$F19 = submission$F18 = submission$F17 = submission$F16 = submission$F15 <- NA
    submission$F28 = submission$F27 = submission$F26 = submission$F25 = submission$F24 = submission$F23 = submission$F22 <- NA
    
    l1_unique <- unique(submission$Agg_Level_1)  # Unique item ids
    l2_unique <- unique(submission$Agg_Level_2)  # Unique store ids
    frc <- NULL
    
    
    # Transpose the appropriate column (3) to row
    
    for (l2 in l2_unique){   # l2 = l2_unique[1]
      for (l1 in l1_unique){ # l1 = l1_unique[1]
        
        # Print store_id and item id repeatetively at the same line
        cat("\r", paste("store_id:", l2, "| item_id:", l1))
        
        temp <- submission[(submission$Agg_Level_1==l1)&(submission$Agg_Level_2==l2),]
        temp[1, 5:32] <- temp[,3]
        frc <- rbind(frc, data.frame(l1, l2, temp[1, 5:32]))
      }
    }
    
    # Create data frame with the rigth format (columns and column names)
    colnames(frc)[1:2] <- c("Agg_Level_1", "Agg_Level_2")
    frc$id <- paste0(frc$Agg_Level_1,"_",frc$Agg_Level_2,"_validation")
    frc <- frc[,c("id", colnames(frc)[3:30])]
    
    # Save sumbisssion file
    write.csv(x = frc, 
              file = paste0("data/submissions/",submission_file_name_prefix, "_", b_names[mid], "_submission",".csv"),
              row.names = FALSE,) 
  }
  
}


# Test
# make_submission_files(frc_total = frc_total, b_names, submission_file_name_prefix = "exp")



## 02. Function: rbind_submission_file(submission_file), transform the submission file in the correct format ----

rbind_submission_file <- function(submission_file){
  
  submission_file <- rbind(submission_file, submission_file)
  submission_file$id[30491:60980] <- gsub(x = submission_file$id[30491:60980], pattern = "validation", replacement = "evaluation")
  
  return(submission_file)
}

