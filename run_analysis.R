#Read the files
library(data.table)

#Read "features.txt" and name the columns as "n" and "features"
features <- read.table("UCI HAR Dataset/features.txt", col.names = c("n", "features"))
head(features)

#Read "activity_labels.txt" and name the columns as "code" and "activity"
activityLabels <- read.table("UCI HAR Dataset/activity_labels.txt", col.names = c("code", "activity"))
head(activityLabels)

#Read "subject_test.txt" and name column as "subject"
subjectsTest <- read.table("UCI HAR Dataset/test/subject_test.txt", col.names = "subjects")
head(subjectsTest)

#Read "subject_test.txt" and name column as "subject"
subjectsTrain <- read.table("UCI HAR Dataset/train/subject_train.txt", col.names = "subjects")
head(subjectsTrain)

#Read test data and name columns
x_test <- read.table("UCI HAR Dataset/test/X_test.txt", col.names = features$features)
y_test <- read.table("UCI HAR Dataset/test/y_test.txt", col.names = "code")
head(x_test)
head(y_test)

#Read train data and name columns
x_train <- read.table("UCI HAR Dataset/train/X_train.txt", col.names = features$features)
y_train <- read.table("UCI HAR Dataset/train/y_train.txt", col.names = "code")
head(x_train)
head(y_train)

#Merge training and test data
X <- rbind(x_train, x_test)
Y <- rbind(y_train, y_test)
head(X)
head(Y)

#Merge tarin and test subjects
subjects <- rbind(subjectsTrain, subjectsTest)
head(subjects)

#Merge data
samsungData <- cbind(subjects, Y, X)
head(samsungData)

#Extracts only the measurements on the mean and standard deviation for each measurement. 
MeanStd <- samsungData[grep("[Mm]ean|[Ss]td", names(samsungData))]
head(MeanStd)

#Uses descriptive activity names to name the activities in the data set
samsungData$code <- activityLabels[samsungData$code, 2]
head(samsungData)

#Appropriately labels the data set with descriptive variable names.
names(samsungData)
names(samsungData)<-gsub("code", "Activity", names(samsungData))
names(samsungData)<-gsub("Acc", "Accelerometer", names(samsungData))
names(samsungData)<-gsub("Gyro", "Gyroscope", names(samsungData))
names(samsungData)<-gsub("BodyBody", "Body", names(samsungData))
names(samsungData)<-gsub("Mag", "Magnitude", names(samsungData))
names(samsungData)<-gsub("^t", "Time", names(samsungData))
names(samsungData)<-gsub("^f", "Frequency", names(samsungData))
names(samsungData)<-gsub("tBody", "TimeBody", names(samsungData))
names(samsungData)<-gsub("-mean()", "Mean", names(samsungData))
names(samsungData)<-gsub("-std()", "std", names(samsungData))
names(samsungData)<-gsub("-freq()", "Frequency", names(samsungData))

#Creates an independent tidy data set with the average of each 
#variable for each activity and each subject.
library(plyr)
TidyData <- aggregate(. ~subjects + Activity, samsungData, mean)
write.table(TidyData, file = "tidydata.txt",row.name=FALSE)

#Create codebook
library(dataMaid)
makeCodebook(TidyData)
