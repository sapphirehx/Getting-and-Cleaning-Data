#download and unzip data files
html = 'https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip'
setwd("F:/R workdir/Getting and Cleaning Data/week4/")
f = './dataset.zip'
download.file(html,f,mode = 'wb')
unzip(f)
setwd('./UCI HAR Dataset/')

#load packages needed
library(data.table)
library(plyr)
library(dplyr)
library(tidyr)

#read data sets
training_set <- data.table(read.table('./train/X_train.txt'))
test_set <- data.table(read.table('./test/X_test.txt'))
subject_train <- data.table(read.table('./train/subject_train.txt'))
subject_test <- data.table(read.table('./test/subject_test.txt'))
training_lable <- data.table(read.table('./train/y_train.txt'))
test_lable <- data.table(read.table('./test/y_test.txt'))
features <- data.table(read.table('./features.txt'))
activity_labels <- data.table(read.table('./activity_labels.txt'))

# 1. Merges the training and the test sets to create one data set.
# subject and label as the 1st and 2nd columns
training_set_sl <- cbind('subject' = subject_train, 'lable' = training_lable, training_set)
test_set_sl <- cbind('subject' = subject_test, 'lable' = test_lable, test_set)
#merge two data sets
meterdata <- rbind(training_set_sl, test_set_sl)

# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
# tells which column is the mean and std
meanStdColumns <- grep("mean\\(\\)|std\\(\\)", features$V2, value = FALSE)

# extracts mean and std column
#since 2 columns were added at front, all indices should add 2
sub_meterdata <- select(meterdata, 1, 2, meanStdColumns+2)
# adding corresponding column names
names(sub_meterdata) <- c('subject','activity',as.character(features$V2[meanStdColumns]))

#3. Uses descriptive activity names to name the activities in the data set
# left join sub_meterdata and activity_lable to add the activity names
des_meter <- left_join(sub_meterdata,activity_labels,by = c('activity' = 'V1'))
# drop undescriptive activity number and rename column
des_meter <- select(des_meter, -2, 'activity' = V2)

#4. Appropriately labels the data set with descriptive variable names.
# variable names named as features
# change -mean() to Mean
# change -std() to Std
# remove -
names(des_meter) <- gsub('-mean\\(\\)','Mean',names(des_meter))
names(des_meter) <- gsub('-std\\(\\)','Std',names(des_meter))
names(des_meter) <- gsub('-','',names(des_meter))


#5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
avg_meter <- ddply(des_meter, .(subject,activity), function(x) colMeans(x[,2:(length(des_meter)-1)]))
write.table(avg_meter, "avg_meter.txt", row.name=FALSE)



