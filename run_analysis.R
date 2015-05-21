# Getting and Cleaning Data
# Course Project
# 150512

library(data.table)
setwd("./Courses/data")

# 1. Merge the training and the test sets to create one data set.

# Read the training data files.

# This file identifies the 30 subjects that did each activity; 7352 obs.
subjectTrain <- read.table("./UCI HAR Dataset/train/subject_train.txt", header = FALSE)
str(subjectTrain)
table(subjectTrain$V1)

# This file has all the activities that the 30 subjects did; n=7352.

activityTrain <- read.table("UCI HAR Dataset/train/y_train.txt", header = FALSE)
str(activityTrain)

# This file has 561 observational features recorded for each activity (n=7352)
featuresTrain <- read.table("UCI HAR Dataset/train/X_train.txt", header = FALSE)
str(featuresTrain)

# Repeat process for the test data files, n=2947, 9 subjects.

subjectTest <- read.table("UCI HAR Dataset/test/subject_test.txt", header = FALSE)
activityTest <- read.table("UCI HAR Dataset/test/y_test.txt", header = FALSE)
featuresTest <- read.table("UCI HAR Dataset/test/X_test.txt", header = FALSE)
str(subjectTest)
str(activityTest)
table(subjectTest$V1)
str(featuresTest)

# Merge the training and test data by subject, activity and features using rbind.

subject <- rbind(subjectTrain, subjectTest)
activity <- rbind(activityTrain, activityTest)
features <- rbind(featuresTrain, featuresTest)

# The datasets currently have nondescript V1, V2 etc. 
# Add meaningful labels to the merged datasets.

featureNames <- read.table("UCI HAR Dataset/features.txt")
head(featureNames)
str(featureNames)
# Add 561 labels to the features table.
colnames(features) <- t(featureNames[2])
str(features)

colnames(activity) <- "Activity"
colnames(subject) <- "Subject"
str(activity)
str(subject)

# 2. Extract only the measurements on the mean and standard deviation for each measurement. 

featuresMeanSD <- grep(".*mean.*|.*std.*", names(features), ignore.case=TRUE)
str(featuresMeanSD)
head(featuresMeanSD)

# Merge all files together on the column name.

mergedData <- cbind(features,activity,subject)
str(mergedData)

# Create a vector with the features that are mean or SD plus subject and activity IDs.

keepCols <- c(featuresMeanSD, 562, 563)

finalData <- mergedData[,keepCols]
dim(finalData)

# 3. Use descriptive activity names to name the activities in the data set

# Six activities as text.
activityLabels <- read.table("./UCI HAR Dataset/activity_labels.txt", header = FALSE)
str(activityLabels)

# Make activity a factor that can accept text values.

finalData$Activity <- as.character(finalData$Activity)
for (i in 1:6){
  finalData$Activity[finalData$Activity == i] <- as.character(activityLabels[i,2])
}
table(finalData$Activity)
table(finalData$Subject)

# 4. Appropriately label the data set with descriptive variable names. 

# Use gsub to search and replace text strings in names of finalData.

names(finalData)<-gsub("-mean()", "Mean", names(finalData))
names(finalData)<-gsub("-std()", "SD", names(finalData))
names(finalData)<-gsub("BodyBody", "Body", names(finalData))
names(finalData)<-gsub("^t", "Time", names(finalData))
names(finalData)<-gsub("^f", "Freq", names(finalData))
names(finalData)<-gsub("angle", "Angle", names(finalData))
names(finalData)<-gsub("gravity", "g ", names(finalData))
names(finalData)

# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

tidyData <- aggregate(. ~Subject + Activity, finalData, mean)
tidyData <- tidyData[order(tidyData$Subject,tidyData$Activity),]
write.table(tidyData, file = "Tidy.txt", row.names = FALSE)
