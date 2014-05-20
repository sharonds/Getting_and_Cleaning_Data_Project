################################################################################
#  Merges the training and the test sets to create one data set

##Load Train Data
train.Data <- read.table("./data/train/X_train.txt")
train.Label <- read.table("./data/train/y_train.txt")
train.Subject <- read.table("./data/train/subject_train.txt")

##Load test Data
test.Data <- read.table("./data/test/X_test.txt")
test.Label <- read.table("./data/test/y_test.txt")
test.Subject <- read.table("./data/test/subject_test.txt")

##Merging the data
merged.Data <- rbind(train.Data, test.Data)
merged.Label <- rbind(train.Label, test.Label)
merged.Subject <- rbind(train.Subject, test.Subject)

################################################################################

#  Extracts only the measurements on the mean and standard deviation for each measurement
features <- read.table("./data/features.txt")
meanStd <- grep("mean\\(\\)|std\\(\\)", features[, 2])
merged.Data <- merged.Data[, meanStd]
names(merged.Data) <- gsub("\\(\\)", "", features[meanStd, 2]) # remove "()"


################################################################################

# Uses descriptive activity names to name the activities in the data set
activity <- read.table("./data/activity_labels.txt")
activity[, 2] <- tolower(gsub("_", "", activity[, 2]))

activity.Label <- activity[merged.Label[, 1], 2]
merged.Label[, 1] <- activity.Label
names(merged.Label) <- "activity"

################################################################################

# Appropriately labels the data set with descriptive activity names

names(merged.Subject) <- "subject"
dataSet <- cbind(merged.Subject, merged.Label, merged.Data)


################################################################################

# Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 

subjectLen <- length(table(merged.Subject)) 
activityLen <- dim(activity)[1] 
columnLen <- dim(dataSet)[2]
data.final <- matrix(NA, nrow=subjectLen*activityLen, ncol=columnLen)
data.final <- as.data.frame(data.final)
colnames(data.final) <- colnames(dataSet)
row <- 1
for(i in 1:subjectLen) {
  for(ii in 1:activityLen) {
    data.final[row, 1] <- sort(unique(merged.Subject)[, 1])[i]
    data.final[row, 2] <- activity[ii, 2]
    a <- i == dataSet$subject
    b <- activity[ii, 2] == dataSet$activity
    data.final[row, 3:columnLen] <- colMeans(dataSet[a&b, 3:columnLen])
    row <- row + 1
  }
}

################################################################################
# creates dity data file
write.table(data.final, "tidy_data.txt") 

