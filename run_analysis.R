## Getting and Cleaning Data Course Project (Coursera)

# Project Description:
# The purpose of this project is to demonstrate my ability to collect, work with, and clean a data set. 
# The goal is to prepare tidy data that can be used for later analysis.

# Data Set:
# The UCI Human Activity Recognition Using Smartphones Data Set is downloaded from:
# https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
# The description of the data set is here:
# http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones

# This script will do the folowing steps:
# 1. Merges the training and the test sets to create one data set.
# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
# 3. Use descriptive activity names to name the activities in the data set.
# 4. Appropriately labels the data set with descriptive variable names.
# 5. From the data set in step 4, create a second, independent tidy data set 
#    with the average of each variable for each activity and each subject.

# Clean up workspace
rm(list=ls())

# 1. Merges the training and the test sets to create one data set.

# Set Working Directory to the location where UCI HAR Dataset was unzipped
setwd("/Users/a.rack.C3/Documents/UCI HAR Dataset/")

# Read in the data from files
features = read.table("./features.txt", header = FALSE)
activityType = read.table("./activity_labels.txt", header = FALSE)
subjectTrain = read.table("./train/subject_train.txt", header = FALSE)
xTrain = read.table("./train/X_train.txt", header = FALSE)
yTrain = read.table("./train/y_train.txt", header = FALSE)

# Assign Column Names to the Train Data
colnames(activityType) = c("activityId", "activityType")
colnames(subjectTrain) = "subjectId"
colnames(xTrain) = features[,2]
colnames(yTrain) = "activityId"

# Create Training Dataset by Merging
trainingData = cbind(yTrain, subjectTrain, xTrain)

# Read in the Test Data
subjectTest = read.table("./test/subject_test.txt", header = FALSE)
xTest = read.table("./test/X_test.txt", header = FALSE)
yTest = read.table("./test/y_test.txt", header = FALSE)

# Assign Column Names to the Test Data
colnames(subjectTest) = "subjectId"
colnames(xTest) = features[,2]
colnames(yTest) = "activityId"

# Create Test Dataset by Merging
testData = cbind(yTest, subjectTest, xTest)

# Create One Dataset by Merging Training and Test Datasets
oneData = rbind(trainingData, testData)


# 2. Extracts only the measurements on the mean and standard deviation for each measurement.

# Create a vector of column names from oneData
colNames = colnames(oneData)

# Create a logical vector, that contains TRUE values for column names with "mean()" and "std()" at the end and FALSE for others
logicalVector = (grepl("subject..",colNames) | grepl("activity..",colNames) |  grepl("-mean..",colNames) & !grepl("-meanFreq..",colNames) & !grepl("mean..-",colNames) | grepl("-std..",colNames) & !grepl("-std()..-",colNames))

# Subset oneData based on logicalVector
oneData = oneData[logicalVector==TRUE]


# 3. Use descriptive activity names to name the activities in the data set.

# Merge oneData with activityType table to get descriptive activity names, and update colNames with the new column
oneData = merge(oneData, activityType, by="activityId", all.x=TRUE)
colNames = colnames(oneData)


# 4. Appropriately labels the data set with descriptive variable names.

# Cleaning up the variable names
for (i in 1:length(colNames))
{
  colNames[i] = gsub("\\()","",colNames[i])
  colNames[i] = gsub("-std$","StdDev",colNames[i])
  colNames[i] = gsub("-mean","Mean",colNames[i])
  colNames[i] = gsub("^(t)","time",colNames[i])
  colNames[i] = gsub("^(f)","freq",colNames[i])
  colNames[i] = gsub("([Gg]ravity)","Gravity",colNames[i])
  colNames[i] = gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",colNames[i])
  colNames[i] = gsub("[Gg]yro","Gyro",colNames[i])
  colNames[i] = gsub("AccMag","AccMagnitude",colNames[i])
  colNames[i] = gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",colNames[i])
  colNames[i] = gsub("JerkMag","JerkMagnitude",colNames[i])
  colNames[i] = gsub("GyroMag","GyroMagnitude",colNames[i])
}

#Assign the descriptive names to oneData
colnames(oneData) = colNames


# 5. From the data set in step 4, create a second, independent tidy data set 
#    with the average of each variable for each activity and each subject.

# Create a new table without activityType column
no_activityType = oneData[ ,names(oneData) !="activityType"]

# Summarizing the no_activityType table to include only the average of each variable
meanData  = aggregate(no_activityType[,names(no_activityType) != c('activityId','subjectId')],by=list(activityId=no_activityType$activityId,subjectId = no_activityType$subjectId),mean)

# Merging meanData with activity Type table to tidyData, to include descriptive activity names
tidyData = merge(meanData, activityType, by="activityId", all.x=TRUE)

# Export tidyData
write.table(tidyData, "./tidyData.txt", row.names = TRUE, sep="\t")



