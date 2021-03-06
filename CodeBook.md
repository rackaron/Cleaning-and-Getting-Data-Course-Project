# Getting and Cleaning Data Course Project

The purpose of this project is to demonstrate my ability to collect, work with, and clean a data set. The goal is to prepare tidy data that can be used for later analysis.

## Subject of the Project:
One of the most exciting areas in all of data science right now is wearable computing. Companies like Fitbit, Nike, and Jawbone Up are racing to develop the most advanced algorithms to attract new users. The data linked to from the course website represent data collected from the accelerometers from the Samsung Galaxy S smartphone.
 A full description is available at the site where the data was obtained:

http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones

## Data Source
Here are the data for the project:

https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

## Description of the Data: Human Activity Recognition Using Smartphones Data Set
### Abstract: 
Human Activity Recognition database built from the recordings of 30 subjects performing activities of daily living (ADL) while carrying a waist-mounted smartphone with embedded inertial sensors.
### Source:
Jorge L. Reyes-Ortiz(1,2), Davide Anguita(1), Alessandro Ghio(1), Luca Oneto(1) and Xavier Parra(2)
1 - Smartlab - Non-Linear Complex Systems Laboratory
DITEN - Università degli Studi di Genova, Genoa (I-16145), Italy. 
2 - CETpD - Technical Research Centre for Dependency Care and Autonomous Living
Universitat Politècnica de Catalunya (BarcelonaTech). Vilanova i la Geltrú (08800), Spain
activityrecognition '@' smartlab.ws

## Data Set Information:

The experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, we captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. The experiments have been video-recorded to label the data manually. The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data. 

The sensor signals (accelerometer and gyroscope) were pre-processed by applying noise filters and then sampled in fixed-width sliding windows of 2.56 sec and 50% overlap (128 readings/window). The sensor acceleration signal, which has gravitational and body motion components, was separated using a Butterworth low-pass filter into body acceleration and gravity. The gravitational force is assumed to have only low frequency components, therefore a filter with 0.3 Hz cutoff frequency was used. From each window, a vector of features was obtained by calculating variables from the time and frequency domain.

## Attribute Information:

### For each record in the dataset it is provided: 
- Triaxial acceleration from the accelerometer (total acceleration) and the estimated body acceleration. 
- Triaxial Angular velocity from the gyroscope. 
- A 561-feature vector with time and frequency domain variables. 
- Its activity label. 
- An identifier of the subject who carried out the experiment.

### The dataset includes the following files:
- 'README.txt'
- 'features_info.txt': Shows information about the variables used on the feature vector.
- 'features.txt': List of all features.
- 'activity_labels.txt': Links the class labels with their activity name.
- 'train/X_train.txt': Training set.
- 'train/y_train.txt': Training labels.
- 'test/X_test.txt': Test set.
- 'test/y_test.txt': Test labels.

The following files are available for the train and test data. Their descriptions are equivalent. 
- 'train/subject_train.txt': Each row identifies the subject who performed the activity for each window sample. Its range is from 1 to 30. 
- 'train/Inertial Signals/total_acc_x_train.txt': The acceleration signal from the smartphone accelerometer X axis in standard gravity units 'g'. Every row shows a 128 element vector. The same description applies for the 'total_acc_x_train.txt' and 'total_acc_z_train.txt' files for the Y and Z axis. 
- 'train/Inertial Signals/body_acc_x_train.txt': The body acceleration signal obtained by subtracting the gravity from the total acceleration. 
- 'train/Inertial Signals/body_gyro_x_train.txt': The angular velocity vector measured by the gyroscope for each window sample. The units are radians/second. 

## Notes: 

- Features are normalized and bounded within [-1,1].
- Each feature vector is a row on the text file.

For more information about this dataset contact: activityrecognition@smartlab.ws

## License:

Use of this dataset in publications must be acknowledged by referencing the following publication [1] 

[1] Davide Anguita, Alessandro Ghio, Luca Oneto, Xavier Parra and Jorge L. Reyes-Ortiz. Human Activity Recognition on Smartphones using a Multiclass Hardware-Friendly Support Vector Machine. International Workshop of Ambient Assisted Living (IWAAL 2012). Vitoria-Gasteiz, Spain. Dec 2012

This dataset is distributed AS-IS and no responsibility implied or explicit can be addressed to the authors or their institutions for its use or misuse. Any commercial use is prohibited.

Jorge L. Reyes-Ortiz, Alessandro Ghio, Luca Oneto, Davide Anguita. November 2012.

## This script will do the folowing steps:
1. Merges the training and the test sets to create one data set.
2. Extracts only the measurements on the mean and standard deviation for each measurement.
3. Use descriptive activity names to name the activities in the data set.
4. Appropriately labels the data set with descriptive variable names.
5. From the data set in step 4, create a second, independent tidy data set 
   with the average of each variable for each activity and each subject.

### Clean up workspace
rm(list=ls())

## 1. Merges the training and the test sets to create one data set.

#### Set Working Directory to the location where UCI HAR Dataset was unzipped:
    setwd("/Users/a.rack.C3/Documents/UCI HAR Dataset/")

#### Read in the data from files
    features = read.table("./features.txt", header = FALSE)
    activityType = read.table("./activity_labels.txt", header = FALSE)
    subjectTrain = read.table("./train/subject_train.txt", header = FALSE)
    xTrain = read.table("./train/X_train.txt", header = FALSE)
    yTrain = read.table("./train/y_train.txt", header = FALSE)

#### Assign Column Names to the Train Data
    colnames(activityType) = c("activityId", "activityType")
    colnames(subjectTrain) = "subjectId"
    colnames(xTrain) = features[,2]
    colnames(yTrain) = "activityId"

#### Create Training Dataset by Merging
    trainingData = cbind(yTrain, subjectTrain, xTrain)

#### Read in the Test Data
    subjectTest = read.table("./test/subject_test.txt", header = FALSE)
    xTest = read.table("./test/X_test.txt", header = FALSE)
    yTest = read.table("./test/y_test.txt", header = FALSE)

#### Assign Column Names to the Test Data
    colnames(subjectTest) = "subjectId"
    colnames(xTest) = features[,2]
    colnames(yTest) = "activityId"

#### Create Test Dataset by Merging
    testData = cbind(yTest, subjectTest, xTest)

#### Create One Dataset by Merging Training and Test Datasets
    oneData = rbind(trainingData, testData)

## 2. Extracts only the measurements on the mean and standard deviation for each measurement.

#### Create a vector of column names from oneData
    colNames = colnames(oneData)

#### Create a logical vector, that contains TRUE values for column names with "mean()" and "std()" at the end and FALSE for others
    logicalVector = (grepl("subject..",colNames) | grepl("activity..",colNames) |  grepl("-mean..",colNames) & !grepl("-meanFreq..",colNames) & !grepl("mean..-",colNames) | grepl("-std..",colNames) & !grepl("-std()..-",colNames))

#### Subset oneData based on logicalVector
    oneData = oneData[logicalVector==TRUE]


## 3. Use descriptive activity names to name the activities in the data set.

#### Merge oneData with activityType table to get descriptive activity names, and update colNames with the new column
    oneData = merge(oneData, activityType, by="activityId", all.x=TRUE)
    colNames = colnames(oneData)


## 4. Appropriately labels the data set with descriptive variable names.

#### Cleaning up the variable names
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

#### Assign the descriptive names to oneData
colnames(oneData) = colNames


## 5. From the data set in step 4, create a second, independent tidy data set with the average of each variable for each activity and each subject.

#### Create a new table without activityType column
    no_activityType = oneData[ ,names(oneData) !="activityType"]

#### Summarizing the no_activityType table to include only the average of each variable
    meanData  = aggregate(no_activityType[,names(no_activityType) != c('activityId','subjectId')],by=list(activityId=no_activityType$activityId,subjectId = no_activityType$subjectId),mean)

#### Merging meanData with activity Type table to tidyData, to include descriptive activity names
    tidyData = merge(meanData, activityType, by="activityId", all.x=TRUE)

#### Export tidyData
    write.table(tidyData, "./tidyData.txt", row.names = FALSE, sep="\t")


The tidy data set is a set of variables for each activity and each subject. 10299 observations are segmented into 180 groups (30 subjects and 6 activities) with 18 mean and standard deviation features averaged for each group. 
The resulting data table has 181 rows and 21 columns. 

The first row is the header containing the column names.

The columns are: 
* subjectId (one of the 30 volunteers)
* activityId (one of the 6 activities)
* activityType (one of the 6 activities named descriptive)
* 9 Mean variables 
* 9 Standard Deviation variables

