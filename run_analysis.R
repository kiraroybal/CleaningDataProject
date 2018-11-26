## Getting and Cleaning Data Project Script
## Kira Roybal
## 11/25/2018

# Download the UCI HAR dataset
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(url, destfile = "samsung.zip", method = "curl")

# Open all test data files
testSet <- read.table("./test/X_test.txt", header = FALSE)
testLabels <- read.table("./test/y_test.txt", header = FALSE)
testSubject <- read.table("./test/subject_test.txt", header = FALSE)
testAccX <- read.table("./test/Inertial Signals/body_acc_x_test.txt", 
                       header = FALSE)
testAccY <- read.table("./test/Inertial Signals/body_acc_y_test.txt", 
                       header = FALSE)
testAccZ <- read.table("./test/Inertial Signals/body_acc_z_test.txt", 
                       header = FALSE)
testGyroX <- read.table("./test/Inertial Signals/body_gyro_x_test.txt", 
                        header = FALSE)
testGyroY <- read.table("./test/Inertial Signals/body_gyro_y_test.txt", 
                        header = FALSE)
testGyroZ <- read.table("./test/Inertial Signals/body_gyro_z_test.txt", 
                        header = FALSE)
testTotalX <- read.table("./test/Inertial Signals/total_acc_x_test.txt", 
                       header = FALSE)
testTotalY <- read.table("./test/Inertial Signals/total_acc_y_test.txt", 
                       header = FALSE)
testTotalZ <- read.table("./test/Inertial Signals/total_acc_z_test.txt", 
                       header = FALSE)

# Open all training data files
trainSet <- read.table("./train/X_train.txt", header = FALSE)
trainLabels <- read.table("./train/y_train.txt", header = FALSE)
trainSubject <- read.table("./train/subject_train.txt", header = FALSE)
trainAccX <- read.table("./train/Inertial Signals/body_acc_x_train.txt", 
                       header = FALSE)
trainAccY <- read.table("./train/Inertial Signals/body_acc_y_train.txt", 
                       header = FALSE)
trainAccZ <- read.table("./train/Inertial Signals/body_acc_z_train.txt", 
                       header = FALSE)
trainGyroX <- read.table("./train/Inertial Signals/body_gyro_x_train.txt", 
                        header = FALSE)
trainGyroY <- read.table("./train/Inertial Signals/body_gyro_y_train.txt", 
                        header = FALSE)
trainGyroZ <- read.table("./train/Inertial Signals/body_gyro_z_train.txt", 
                        header = FALSE)
trainTotalX <- read.table("./train/Inertial Signals/total_acc_x_train.txt", 
                        header = FALSE)
trainTotalY <- read.table("./train/Inertial Signals/total_acc_y_train.txt", 
                        header = FALSE)
trainTotalZ <- read.table("./train/Inertial Signals/total_acc_z_train.txt", 
                        header = FALSE)

# Open feature names
features <- read.table("features.txt", header = FALSE)
features <- select(features, V2)

# Rename variable in testSet and trainSet
for (i in 1:516) {
    colnames(testSet)[i] <- as.character(features[i,])
    colnames(trainSet)[i] <- as.character(features[i,])
}

# Add in a column to specify either test or training group
library(dplyr)
testSet <- mutate(testSet, group = "test")
trainSet <- mutate(trainSet, group = "train")

# Combine the test and training sets (1)
allTest <- cbind(testSubject, testLabels, testSet)
allTrain <- cbind(trainSubject, trainLabels, trainSet)
all <- rbind(allTest, allTrain)
colnames(all)[1] <- "subject"
colnames(all)[2] <- "label"

# Extract only mean and standard deviation measurements (2)
all <- select(all, subject, label, group, contains("mean"), contains("std"))

# Rename the activities (labels) (3)
for (i in 1:dim(all)[1]) {
    if (all[i,2] == 1) {all[i,2] <- "walking"}
    if (all[i,2] == 2) {all[i,2] <- "walking upstairs"}
    if (all[i,2] == 3) {all[i,2] <- "walking downstairs"}
    if (all[i,2] == 4) {all[i,2] <- "sitting"}
    if (all[i,2] == 5) {all[i,2] <- "standing"}
    else {all[i,2] <- "laying"}
}

# Rename the variables (4)
colnames(all) <- gsub("-mean", "Mean", colnames(all))
colnames(all) <- gsub("-std", "StDev", colnames(all))
colnames(all) <- gsub("-X", "X", colnames(all))
colnames(all) <- gsub("-Y", "Y", colnames(all))
colnames(all) <- gsub("-Z", "Z", colnames(all))
colnames(all) <- gsub("\\()", "", colnames(all))
colnames(all) <- gsub("^t", "Time", colnames(all))
colnames(all) <- gsub("^f", "Freq", colnames(all))

View(all)


##### Create a 2nd tidy data set (5) #####
newdata <- data.frame(matrix(ncol=dim(all)[2], nrow=30))
all <- arrange(all, subject)
j <- 1
for (i in all) {
    newdata[,j] <- tapply(i, all$subject, mean)
    j <- j + 1
}

# Create headers for the variables
for (i in 1:dim(newdata)[2]) {
    colnames(newdata)[i] <- paste0("Avg", colnames(all)[i])
}

# Remove the label and group columns
newdata <- newdata[-c(2,3)]
# Rename the subject column
colnames(newdata) <- gsub("Avgsubject", "subject", colnames(newdata))

View(newdata)