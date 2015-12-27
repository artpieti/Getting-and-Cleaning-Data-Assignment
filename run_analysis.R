# Getting and Cleaning Data course assignment
# By Arttu Pietila

run_analysis <- function() {
      library(data.table)

# Load in the data from the test and train folders
      subject_test <- read.table("UCI HAR Dataset/test/subject_test.txt")
      X_test <- read.table("UCI HAR Dataset/test/X_test.txt")
      y_test <- read.table("UCI HAR Dataset/test/y_test.txt")
      
      subject_train <- read.table("UCI HAR Dataset/train/subject_train.txt")
      X_train <- read.table("UCI HAR Dataset/train/X_train.txt")
      y_train <- read.table("UCI HAR Dataset/train/y_train.txt")
      
# Merge the data tables
      subject <- rbind(subject_test, subject_train)
      X <- rbind(X_test, X_train)
      y <- rbind(y_test, y_train)
      
# Load the descrpitive activity names
      features <- read.table("UCI HAR Dataset/features.txt", 
                             col.names=c("featureId", "featureLabel"))
      activities <- read.table("UCI HAR Dataset/activity_labels.txt", 
                               col.names=c("activityId", "activityLabel"))
      
# Select the required features
      toMatch <- c("-mean", "-std")
      meanStdFeatures <- grep("-mean\\(\\)|-std\\(\\)", features$featureLabel)
      X <- X[, meanStdFeatures]
      
# Label the data
      names(subject) <- "subjectId"
      names(X) <- gsub("\\(|\\)", "", features$featureLabel[meanStdFeatures])
      names(y) = "activityId"
      activity <- merge(y, activities, by="activityId")$activityLabel

# Merge subject, X and y data tables into one data table and write it into a text file
      data <- cbind(subject, X, activity)
      write.table(data, "tidy_data.txt", row.names = FALSE)

# Create a data table of averages for each activity and each subject 
      DT <- data.table(data)
      avgsData <- DT[, lapply(.SD, mean), by=c("subjectId", "activity")]
      write.table(avgsData, "avgtidy_data.txt")
}

