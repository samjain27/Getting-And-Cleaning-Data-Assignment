##Downloading data

library(data.table)
fileurl = 'https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip'
setwd("~/Data Cleaning Assignment")
if (!file.exists('./UCI HAR Dataset.zip')){
  download.file(fileurl,'~/UCI HAR Dataset/UCI HAR Dataset.zip', mode = 'wb')
  unzip("UCI HAR Dataset.zip", exdir = './')
}
##Reading data 

uci_hard_dir <- "~/Data Cleaning Assignment/UCI HAR Dataset"
feature_file <- paste(uci_hard_dir, "/features.txt", sep = "")
activity_labels_file <- paste(uci_hard_dir, "/activity_labels.txt", sep = "")
x_train_file <- paste(uci_hard_dir, "/train/X_train.txt", sep = "")
y_train_file <- paste(uci_hard_dir, "/train/y_train.txt", sep = "")
subject_train_file <- paste(uci_hard_dir, "/train/subject_train.txt", sep = "")
x_test_file  <- paste(uci_hard_dir, "/test/X_test.txt", sep = "")
y_test_file  <- paste(uci_hard_dir, "/test/y_test.txt", sep = "")
subject_test_file <- paste(uci_hard_dir, "/test/subject_test.txt", sep = "")

# Load raw data
features <- read.table(feature_file, colClasses = c("character"))
activity.labels <- read.table(activity_labels_file, col.names = c("ActivityId", "Activity"))
data.train.x <- read.table(x_train_file)
data.train.activity <- read.table(y_train_file)
data.train.subject <- read.table(subject_train_file)
data.test.x <- read.table(x_test_file)
data.test.activity <- read.table(y_test_file)
data.test.subject <- read.table(subject_test_file)

##converting data

features <- as.character(features[,2])

data.train <-  data.frame(data.train.subject, data.train.activity, data.train.x)
names(data.train) <- c(c('subject', 'activity'), features)

data.test <-  data.frame(data.test.subject, data.test.activity, data.test.x)
names(data.test) <- c(c('subject', 'activity'), features)

##Above Function Combines X_train, y_train, subject_train, X_test, y_test and subject_test 
##into a new data frame data.test, and renames the column name of the new data frame.

##Merging the training and the test sets to create one data set.
data.all <- rbind(data.train, data.test)

##Extracting only the measurements on the mean and standard deviation for each measurement
##Select all the columns that represent mean or standard deviation of the measurements with grep order.
col.select <- grep('mean|std', features)
data.sub <- data.all[,c(1,2,col.select + 2)]

##Uses descriptive activity names to name the activities in the data set
##Replacing numeric labels of activity in column 2 of the data frame (from 1 to 6) 
##by descriptive strings which come from the file activity_labels.txt.
activity.labels <- as.character(activity.labels[,2])
data.sub$activity <- activity.labels[data.sub$activity]

##Rename the colname of data.sub with gsub order.
##Appropriately labels the data set with descriptive variable names

name.new <- names(data.sub)
name.new <- gsub("[(][)]", "", name.new)
name.new <- gsub("^t", "TimeDomain_", name.new)
name.new <- gsub("^f", "FrequencyDomain_", name.new)
name.new <- gsub("Acc", "Accelerometer", name.new)
name.new <- gsub("Gyro", "Gyroscope", name.new)
name.new <- gsub("Mag", "Magnitude", name.new)
name.new <- gsub("-mean-", "_Mean_", name.new)
name.new <- gsub("-std-", "_StandardDeviation_", name.new)
name.new <- gsub("-", "_", name.new)
names(data.sub) <- name.new

##Creates a second,independent tidy data set 
##with the average of each variable for each activity and each subject

data.tidy <- aggregate(data.sub[,3:81], by = list(activity = data.sub$activity, subject = data.sub$subject),FUN = mean)
write.table(x = data.tidy, file = "data_tidy.txt", row.names = FALSE)
##print data with 
##data_tidy