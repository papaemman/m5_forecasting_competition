##############################
#                            #
#   Check submission files   #
#                            #
##############################

## Guidelines for Submission File: 

# Each row contains an id that is a concatenation of an item_id and a store_id, 
# which is either validation (corresponding to the Public leaderboard), or evaluation (corresponding to the Private leaderboard).
# You are predicting 28 forecast days (F1-F28) of items sold for each row. 
# For the validation rows, this corresponds to d_1914 - d_1941,
# and for the evaluation rows, this corresponds to d_1942 - d_1969.

# Note that a month before the competition close, the ground truth for the validation rows will be provided.


## 01. Sample submission file
sample_submission <- read.csv("data/submissions/sample_submission.csv", stringsAsFactors = F)

str(sample_submission)
dim(sample_submission) # 60980    29
head(sample_submission)
colnames(sample_submission)


## 02. Output of make_submission_files() function
naive_submission <- read.csv("data/submissions/exp_Naive_submission.csv", stringsAsFactors = F)

prophet_submission_div_2 <- read.csv("data/submissions/exp_prophet_submission_final_div_2.csv", stringsAsFactors = F)
View(prophet_submission_div_2)
dim(prophet_submission_div_2)

str(naive_submission)
dim(naive_submission)

colnames(naive_submission)


head(sample_submission$id)
length(sample_submission$id)
tail(sample_submission)

head(naive_submission$id)
length(naive_submission$id)
tail(naive_submission)

submission_file <- naive_submission

# Conclusion: rbind_submission_file() function must be used in every submission file, prior the submission.

