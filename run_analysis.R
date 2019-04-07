#         Summary
#   Using data collected from Samsung Galaxy S smartphone and save results to "Tidy_Data_Set.txt".

library(dplyr)



# Stage 1: Get data

# Stage 1.1: Downloading zipped data file
DataUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
DataFile <- "UCI HAR Dataset.zip"

if (!file.exists(DataFile)) {
  download.file(DataUrl, DataFile, mode = "wb")
}

# Stage 1.2: Unzipping file containing data if data directory doesn't already exist
dataPath <- "UCI HAR Dataset"
if (!file.exists(dataPath)) {
  unzip(DataFile)
}


# Stage 2: Read data

# Stage 2.1: Reading training data
trainingSubjects <- read.table(file.path(dataPath, "train", "subject_train.txt"))
trainingValues <- read.table(file.path(dataPath, "train", "X_train.txt"))
trainingActivity <- read.table(file.path(dataPath, "train", "y_train.txt"))

# Stage 2.2: Reading test data
testSubjects <- read.table(file.path(dataPath, "test", "subject_test.txt"))
testValues <- read.table(file.path(dataPath, "test", "X_test.txt"))
testActivity <- read.table(file.path(dataPath, "test", "y_test.txt"))

# Stage 2.3: Reading features
features <- read.table(file.path(dataPath, "features.txt"), as.is = TRUE)

# Stage 2.4 Reading activity labels
activities <- read.table(file.path(dataPath, "activity_labels.txt"))
colnames(activities) <- c("activityId", "activityLabel")


# Stage 3: Merge training and test sets into single data set

# Stage 3.1: Merging single data tables into combined data table
humanActivity <- rbind(
  cbind(trainingSubjects, trainingValues, trainingActivity),
  cbind(testSubjects, testValues, testActivity)
)

# Stage 3.2: Clearing individual data tables to save memory
rm(trainingSubjects, trainingValues, trainingActivity, 
   testSubjects, testValues, testActivity)

# Stage 3.3: Assigning column names
colnames(humanActivity) <- c("subject", features[, 2], "activity")


# Stage 4: Extract only the measurements on the mean and standard deviation
#          for each measurement

# Stage 4.1: Establishing the number of columns of data set to keep based on column name
#         and keep the data in these columns only
columnsToKeep <- grepl("subject|activity|mean|std", colnames(humanActivity))
humanActivity <- humanActivity[, columnsToKeep]

# Stage 5: Use descriptive activity names to name the activities in the data set

# Stage 5.1: Replacing activity values with named factor levels
humanActivity$activity <- factor(humanActivity$activity, 
levels = activities[, 1], labels = activities[, 2])


# Stage 6: Appropriately label the data set with descriptive variable names

# Stage 6.1: Getting column names
humanActivityCols <- colnames(humanActivity)

# Stage 6.2: Removing special characters
humanActivityCols <- gsub("[\\(\\)-]", "", humanActivityCols)

# Stage 6.3: Expanding abbreviations and clean up names
humanActivityCols <- gsub("^f", "frequencyDomain", humanActivityCols)
humanActivityCols <- gsub("^t", "timeDomain", humanActivityCols)
humanActivityCols <- gsub("Acc", "Accelerometer", humanActivityCols)
humanActivityCols <- gsub("Gyro", "Gyroscope", humanActivityCols)
humanActivityCols <- gsub("Mag", "Magnitude", humanActivityCols)
humanActivityCols <- gsub("Freq", "Frequency", humanActivityCols)
humanActivityCols <- gsub("mean", "Mean", humanActivityCols)
humanActivityCols <- gsub("std", "StandardDeviation", humanActivityCols)

# Stage 6.4: Correcting typo
humanActivityCols <- gsub("BodyBody", "Body", humanActivityCols)

# Stage 6.5: Using new labels as column names
colnames(humanActivity) <- humanActivityCols


# Stage 7: Create a second, independent tidy set with the average of each
#          variable for each activity and each subject

# Stage 7.1: Grouping by subject and activity and summarising statified by mean
humanActivityMeans <- humanActivity %>% 
group_by(subject, activity) %>%
summarise_each(funs(mean))

# Stage 7.2 Storing result to file "Tidy_Data_Set.txt"
write.table(humanActivityMeans, "Tidy_Data_Set.txt", row.names = FALSE, 
quote = FALSE)
