# Coursera
# Getting and Cleaning Data
# Course Project
# 150512

# 1. Merge the training and the test sets to create one data set.

# subject_train.txt identifies the 30 subjects that did each activity; 7352 obs.
# y_train.txt has all the activities that the 30 subjects did; n=7352.
# X_train.txt has 561 observational features recorded for each activity (n=7352)

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
# subject_test.txt has 2947 rows for 9 subjects.
# y_test.txt has the activity types that correspond to each subject row.
# X_test.txt has the feature measurements of each activity.

subjectTest <- read.table("UCI HAR Dataset/test/subject_test.txt", header = FALSE)
activityTest <- read.table("UCI HAR Dataset/test/y_test.txt", header = FALSE)
featuresTest <- read.table("UCI HAR Dataset/test/X_test.txt", header = FALSE)
str(subjectTest)
str(activityTest)
table(subjectTest$V1)
str(featuresTest)

# Merge the training and test data by subject, activity and features using row bind.

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

# Create a vector with the features that are mean or SD plus the subject and activity IDs.

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

# Codebook
 [1] "Subject"                         

 [2] "Activity" 
  LAYING            
  SITTING           
  STANDING            
  WALKING 
  WALKING_DOWNSTAIRS 
  WALKING_UPSTAIRS 

Observational features (mean and sd) measured in X, Y and Z axes:
 [3] "TimeBodyAccMean()-X"             "TimeBodyAccMean()-Y"            
 [5] "TimeBodyAccMean()-Z"             "TimeBodyAccSD()-X"              
 [7] "TimeBodyAccSD()-Y"               "TimeBodyAccSD()-Z"              
 [9] "TimeGravityAccMean()-X"          "TimeGravityAccMean()-Y"         
[11] "TimeGravityAccMean()-Z"          "TimeGravityAccSD()-X"           
[13] "TimeGravityAccSD()-Y"            "TimeGravityAccSD()-Z"           
[15] "TimeBodyAccJerkMean()-X"         "TimeBodyAccJerkMean()-Y"        
[17] "TimeBodyAccJerkMean()-Z"         "TimeBodyAccJerkSD()-X"          
[19] "TimeBodyAccJerkSD()-Y"           "TimeBodyAccJerkSD()-Z"          
[21] "TimeBodyGyroMean()-X"            "TimeBodyGyroMean()-Y"           
[23] "TimeBodyGyroMean()-Z"            "TimeBodyGyroSD()-X"             
[25] "TimeBodyGyroSD()-Y"              "TimeBodyGyroSD()-Z"             
[27] "TimeBodyGyroJerkMean()-X"        "TimeBodyGyroJerkMean()-Y"       
[29] "TimeBodyGyroJerkMean()-Z"        "TimeBodyGyroJerkSD()-X"         
[31] "TimeBodyGyroJerkSD()-Y"          "TimeBodyGyroJerkSD()-Z"         
[33] "TimeBodyAccMagMean()"            "TimeBodyAccMagSD()"             
[35] "TimeGravityAccMagMean()"         "TimeGravityAccMagSD()"          
[37] "TimeBodyAccJerkMagMean()"        "TimeBodyAccJerkMagSD()"         
[39] "TimeBodyGyroMagMean()"           "TimeBodyGyroMagSD()"            
[41] "TimeBodyGyroJerkMagMean()"       "TimeBodyGyroJerkMagSD()"        
[43] "FreqBodyAccMean()-X"             "FreqBodyAccMean()-Y"            
[45] "FreqBodyAccMean()-Z"             "FreqBodyAccSD()-X"              
[47] "FreqBodyAccSD()-Y"               "FreqBodyAccSD()-Z"              
[49] "FreqBodyAccMeanFreq()-X"         "FreqBodyAccMeanFreq()-Y"        
[51] "FreqBodyAccMeanFreq()-Z"         "FreqBodyAccJerkMean()-X"        
[53] "FreqBodyAccJerkMean()-Y"         "FreqBodyAccJerkMean()-Z"        
[55] "FreqBodyAccJerkSD()-X"           "FreqBodyAccJerkSD()-Y"          
[57] "FreqBodyAccJerkSD()-Z"           "FreqBodyAccJerkMeanFreq()-X"    
[59] "FreqBodyAccJerkMeanFreq()-Y"     "FreqBodyAccJerkMeanFreq()-Z"    
[61] "FreqBodyGyroMean()-X"            "FreqBodyGyroMean()-Y"           
[63] "FreqBodyGyroMean()-Z"            "FreqBodyGyroSD()-X"             
[65] "FreqBodyGyroSD()-Y"              "FreqBodyGyroSD()-Z"             
[67] "FreqBodyGyroMeanFreq()-X"        "FreqBodyGyroMeanFreq()-Y"       
[69] "FreqBodyGyroMeanFreq()-Z"        "FreqBodyAccMagMean()"           
[71] "FreqBodyAccMagSD()"              "FreqBodyAccMagMeanFreq()"       
[73] "FreqBodyAccJerkMagMean()"        "FreqBodyAccJerkMagSD()"         
[75] "FreqBodyAccJerkMagMeanFreq()"    "FreqBodyGyroMagMean()"          
[77] "FreqBodyGyroMagSD()"             "FreqBodyGyroMagMeanFreq()"      
[79] "FreqBodyGyroJerkMagMean()"       "FreqBodyGyroJerkMagSD()"        
[81] "FreqBodyGyroJerkMagMeanFreq()"   "Angle(tBodyAccMean,g )"         
[83] "Angle(tBodyAccJerkMean),g Mean)" "Angle(tBodyGyroMean,g Mean)"    
[85] "Angle(tBodyGyroJerkMean,g Mean)" "Angle(X,g Mean)"                
[87] "Angle(Y,g Mean)"                 "Angle(Z,g Mean)"  

# References

[1] Davide Anguita, Alessandro Ghio, Luca Oneto, Xavier Parra and Jorge L. Reyes-Ortiz. Human Activity Recognition on Smartphones using a Multiclass Hardware-Friendly Support Vector Machine. International Workshop of Ambient Assisted Living (IWAAL 2012). Vitoria-Gasteiz, Spain. Dec 2012
