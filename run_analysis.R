##Downloading Data if not in system  and unzipping
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
if (!file.exists("datafile.zip")) {
  download.file(fileUrl,destfile = "datafile.zip")
  }

unzipfolder <- "UCI HAR Dataset"
if (!file.exists(unzipfolder)) {
  unzip("datafile.zip")
}

##Merging training and test sets to create one data set

#loading activity labes and features
features <- read.table("./UCI HAR Dataset/features.txt", as.is = TRUE)
colnames(features)<-c("index", "featureNames")

activities <- read.table("./UCI HAR Dataset/activity_labels.txt")
colnames(activities) <- c("classLabels", "activityName")


# read test data
testSubjects <- read.table( "./UCI HAR Dataset/test/subject_test.txt")
testValues <- read.table("./UCI HAR Dataset/test/X_test.txt")
testActivity <- read.table("./UCI HAR Dataset/test/y_test.txt")
test <- cbind(testSubjects, testValues, testActivity)

#read training data
trainSubjects <- read.table( "./UCI HAR Dataset/train/subject_train.txt")
trainValues <- read.table("./UCI HAR Dataset/train/X_train.txt")
trainActivity <- read.table("./UCI HAR Dataset/train/y_train.txt")
train <- cbind(trainSubjects, trainValues, trainActivity)

#Merge datasets 
MergedData <- rbind(train,test)
colnames(MergedData) <- c("subject", features[, 2], "activity")



##Extract only the measurements on the mean and standard
##  deviation for each measurement

# determine columns of data set to keep based on column name...
columnsRequired <- grepl("subject|activity|mean|std", 
                       colnames(MergedData))
MergedData <- MergedData[,columnsRequired]



##Use descriptive activity names to name the activities 
##  in the data set

# replace activity values with named factor levels
MergedData$activity <- factor(MergedData$activity,
                              levels = activities[, 1], 
                              labels = activities[, 2])

##Appropriately label the data set with descriptive 
##  variable names

datacolumns <- colnames(MergedData)

datacolumns <- gsub("[()-]", "", datacolumns)
datacolumns <- gsub("Freq", "Frequency", datacolumns)
datacolumns <- gsub("^f", "Frequency", datacolumns)
datacolumns <- gsub("^t", "Time", datacolumns)
datacolumns <- gsub("Acc", "Accelerometer", datacolumns)
datacolumns <- gsub("Gyro", "Gyroscope", datacolumns)
datacolumns <- gsub("Mag", "Magnitude", datacolumns)
datacolumns <- gsub("mean", "Mean", datacolumns)
datacolumns <- gsub("std", "StandardDeviation", datacolumns)
datacolumns <- gsub("BodyBody", "Body", datacolumns)

colnames(MergedData) <- datacolumns

##Create a second, independent tidy set with the average 
##  of each variable for each activity and each subject

# group by subject and activity and summarise using mean
DataMean <- MergedData %>% 
  group_by(subject, activity) %>%
  summarise_each(funs(mean))

# output to file "tidy_data.txt"
write.table(DataMean, "tidy_data.txt",
            row.names = FALSE, 
            quote = FALSE)


