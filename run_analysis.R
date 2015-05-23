library(data.table)
library(plyr)

#Read the data
featureNames <- read.table("./UCI HAR Dataset/features.txt")
activityLabels <- read.table("./UCI HAR Dataset/activity_labels.txt", header = FALSE, na.strings="NA")
subjectTrain <- read.table("./UCI HAR Dataset/train/subject_train.txt", header = FALSE, na.strings="NA")
yTrain <- read.table("./UCI HAR Dataset/train/y_train.txt", header = FALSE, na.strings="NA")
xTrain <- read.table("./UCI HAR Dataset/train/X_train.txt", header = FALSE, na.strings="NA")
subjectTest <- read.table("./UCI HAR Dataset/test/subject_test.txt", header = FALSE, na.strings="NA")
yTest <- read.table("./UCI HAR Dataset/test/y_test.txt", header = FALSE, na.strings="NA")
xTest <- read.table("./UCI HAR Dataset/test/X_test.txt", header = FALSE, na.strings="NA")

#Part1 : Merging all the datasets
subject <- rbind(subjectTrain, subjectTest)
activity <- rbind(yTrain, yTest)
features <- rbind(xTrain, xTest)
colnames(features) <- t(featureNames[2])
colnames(activity) <- "Activity"
colnames(subject) <- "Subject"
completeData <- cbind(features,activity,subject)

#Step2: Mean and std dev
columnsWithMeanSTD <- grep(".*Mean.*|.*Std.*", names(completeData), ignore.case=TRUE)
requiredColumns <- c(columnsWithMeanSTD, 562, 563)
#dim(completeData)
extractedData <- completeData[,requiredColumns]
#dim(extractedData)

#Step 3 : use descriptive names
extractedData$Activity <- as.character(extractedData$Activity)
for (i in 1:6){
  extractedData$Activity[extractedData$Activity == i] <- as.character(activityLabels[i,2])
}
extractedData$Activity <- as.factor(extractedData$Activity)

#Step 4: names(extractedData)<-gsub("Acc", "Accelerometer", names(extractedData))
names(extractedData)<-gsub("Gyro", "Gyroscope", names(extractedData))
names(extractedData)<-gsub("BodyBody", "Body", names(extractedData))
names(extractedData)<-gsub("Mag", "Magnitude", names(extractedData))
names(extractedData)<-gsub("^t", "Time", names(extractedData))
names(extractedData)<-gsub("^f", "Frequency", names(extractedData))
names(extractedData)<-gsub("tBody", "TimeBody", names(extractedData))
names(extractedData)<-gsub("-mean()", "Mean", names(extractedData), ignore.case = TRUE)
names(extractedData)<-gsub("-std()", "STD", names(extractedData), ignore.case = TRUE)
names(extractedData)<-gsub("-freq()", "Frequency", names(extractedData), ignore.case = TRUE)
names(extractedData)<-gsub("angle", "Angle", names(extractedData))
names(extractedData)<-gsub("gravity", "Gravity", names(extractedData))

#Step 5: create tidy dataset

extractedData$Subject <- as.factor(extractedData$Subject)
extractedData <- data.table(extractedData)
tidyData <- aggregate(. ~Subject + Activity, extractedData, mean)
tidyData <- tidyData[order(tidyData$Subject,tidyData$Activity),]
write.table(tidyData, file = "Tidy.txt", row.names = FALSE)
