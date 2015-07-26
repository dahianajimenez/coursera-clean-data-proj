
# Create new project folder to hold copy of the original data and results
if(!file.exists("./projectsamsung")) {
    dir.create("./projectsamsung")
    setwd("./projectsamsung")
}

download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip", destfile="UCI HAR Dataset.zip")

unzip("UCI HAR Dataset.zip")
setwd("UCI HAR Dataset")


# Install required packages
if (!require("dplyr"))
    install.packages("dplyr")
library(dplyr)


# Create dataframes for TRAIN subject, activity, and features variables. Then combine them in one dataframe.
train_subject <- read.table("./train/subject_train.txt", col.names = "subject")
train_activity <- read.table("./train/y_train.txt", col.names = "activity")
train_features <- read.table("./train/X_train.txt")

train_df <- tbl_df(bind_cols(train_subject, train_activity, train_features))


# Create dataframes for TEST subject, activity, and features variables. Then combine them in one dataframe.
test_subject <- read.table("./test/subject_test.txt", col.names = "subject")
test_activity <- read.table("./test/y_test.txt", col.names = "activity")
test_features <- read.table("./test/X_test.txt")

test_df <- tbl_df(bind_cols(test_subject, test_activity, test_features))


# Combine train and test dataframes
combined_df <- bind_rows(train_df, test_df) %>%
    
    # Replace numerical representations of activities with descriptive ones (reference file "activity_labels.txt")
    mutate(activity = replace(activity, activity == 1, "Walking")) %>%
    mutate(activity = replace(activity, activity == 2, "Walking_Upstairs")) %>%
    mutate(activity = replace(activity, activity == 3, "Walking_Downstairs")) %>%
    mutate(activity = replace(activity, activity == 4, "Sitting")) %>%
    mutate(activity = replace(activity, activity == 5, "Standing")) %>%
    mutate(activity = replace(activity, activity == 6, "Laying"))


# File "features.txt" contains the column names of the features. Some of those names are duplicated. Make them unique.
feat_col_names <- read.table("features.txt")
feat_col_names_unique <- make.unique(as.vector(feat_col_names[, 2]))
colnames(combined_df)[3:563] <- feat_col_names_unique


# For features referring to means or standard deviations, create a dataframe with their average per activity per subject
sd_mean_averages_df <- combined_df %>%
    select(subject, activity, matches("std|mean")) %>%
    group_by(subject, activity) %>%
    summarise_each(funs(mean))


# For column names of features:
# Add string "AVG."
# Rename to reflect naming conventions
# Remove triple and douple ellipsis, and periods at end
sma_col_names <- colnames(sd_mean_averages_df)

sma_col_names <- as.vector(c("subject", "activity", sapply(colnames(sd_mean_averages_df)[3:ncol(sd_mean_averages_df)], function(x) {paste("AVG", x, sep=".")})))
sma_col_names <- make.names(sma_col_names)
sma_col_names <- gsub("\\.\\.\\.|\\.\\.", ".", sma_col_names)
sma_col_names <- gsub("\\.$", '', sma_col_names)

colnames(sd_mean_averages_df) <- sma_col_names


# Export the averages
write.table(sd_mean_averages_df, "../sd_mean_averages.txt", row.names = FALSE)


# NOTE: to read "sd_mean_averages.txt":
# read.table("sd_mean_averages.txt", header=TRUE)
