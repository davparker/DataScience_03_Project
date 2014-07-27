# Author:   David Parker
# Course:   Getting and Cleaning Data
# School:   John Hopkins University
# Provider: Coursera

# License:
#     ========
#     Use of this dataset in publications must be acknowledged by referencing the following publication [1] 
# 
# [1] Davide Anguita, Alessandro Ghio, Luca Oneto, Xavier Parra and Jorge L.
# Reyes-Ortiz. Human Activity Recognition on Smartphones using a Multiclass
# Hardware-Friendly Support Vector Machine. International Workshop of Ambient
# Assisted Living (IWAAL 2012). Vitoria-Gasteiz, Spain. Dec 2012
# 
# This dataset is distributed AS-IS and no responsibility implied or explicit
# can be addressed to the authors or their institutions for its use or misuse.
# Any commercial use is prohibited.
# 
# Jorge L. Reyes-Ortiz, Alessandro Ghio, Luca Oneto, Davide Anguita. November 2012.

# Here are the data for the project:
# 
# https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
# 
# You should create one R script called run_analysis.R that does the following. 
# 1) Merges the training and the test sets to create one data set. 
# 2) Extracts only the measurements on the mean and standard deviation for each measurement. 
# 3) Uses descriptive activity names to name the activities in the data set 
# 4) Appropriately labels the data set with descriptive variable names. 
# 5) Creates a second, independent tidy data set with the average of each variable for each
#    activity and each subject.


# Preliminary activities, acquiring the data, reading it into data frames
# Step 5 utilizes ddply in the plyr library
require(plyr)

# Assumes the working directory is set to project directory!
# Aquire "Human Activity Recognition Using Smartphones Data Set"
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
if (!file.exists("dataset.zip")) {
    download.file(fileUrl, "dataset.zip")
    unzip("dataset.zip")
}

# Read in actvityLabels
xActivityFile <- "./UCI HAR Dataset/activity_labels.txt"
xActivityLabels <- read.table(xActivityFile, header = FALSE, colClasses = "character")

# Read in x feature labels i.e. column names | common to Train & Test
xFeatures <- "./UCI HAR Dataset/features.txt"
xColNames <- read.table(xFeatures, header = FALSE, colClasses = "character")

# Read in test data
xTestFile    <- "./UCI HAR Dataset/test/x_test.txt"
yTestFile    <- "./UCI HAR Dataset/test/y_test.txt"
sTestFile    <- "./UCI HAR Dataset/test/subject_test.txt"
# Load xTestData using column names from above
xTestData    <- read.table(xTestFile, header = FALSE, col.names = xColNames$V2, colClasses = "numeric")
yTestData    <- read.table(yTestFile, header = FALSE, colClasses = "character")
TestSubject  <- read.table(sTestFile, header = FALSE, colClasses = "character")

# Read in train data
xTrainFile    <- "./UCI HAR Dataset/train/x_train.txt"
yTrainFile    <- "./UCI HAR Dataset/train/y_train.txt"
sTrainFile    <- "./UCI HAR Dataset/train/subject_train.txt"
# load xTrainData using column names from above
xTrainData    <- read.table(xTrainFile, header = FALSE, col.names = xColNames$V2, colClasses = "numeric")
yTrainData    <- read.table(yTrainFile, header = FALSE, colClasses = "character")
TrainSubject  <- read.table(sTrainFile, header = FALSE, colClasses = "character")


# Step 1
# Merges the training and the test sets to create one data set. 
# Verify test data row counts
nrow(xTestData)  # ecpect 2947
identical(nrow(xTestData), nrow(yTestData))  # expect TRUE
identical(nrow(xTestData), nrow(TestSubject))  # expect TRUE

# Create one test data frame binding by columns
xTestData <- cbind(xTestData, TestSubject)
xTestData <- cbind(xTestData, yTestData)

# Verify train data row counts
nrow(xTrainData)  # expect 7352
identical(nrow(xTrainData), nrow(yTrainData))    # expect TRUE
identical(nrow(xTrainData), nrow(TrainSubject))  # expect TRUE

# Create on train data frame binding by columns
xTrainData <- cbind(xTrainData, TrainSubject)
xTrainData <- cbind(xTrainData, yTrainData)

# Verify colnames prior to merging
identical(colnames(xTestData), colnames(xTrainData))  # expect TRUE

# Merge all the data into one data frame
xMergeData <- rbind(xTrainData, xTestData)

# Verify the rowcount of Merged Data equal the sum of Test + Train
identical(nrow(xMergeData), (nrow(xTestData) + nrow(xTrainData) ) )  # expect TRUE


# Step 2
# Extracts only the measurements on the mean and standard deviation for each measurement.

# Gets all column indicies for standard deviation, mean, generic V1 V1.1 added by Subject & yData
# Note: columns with meanFreq in their names get selected here, pulled in with the other mean
# columns, but these will be dropped at the end of this step. Easier that way.
xMeanStdCol <- grepl(".std|.mean|^V", colnames(xMergeData) )
sum(xMeanStdCol)  # sum of filtered column indicies | expect 81

# Extracting data using filtered column indicies into a new dataset: xMeanStdData 
xMeanStdData <- xMergeData[, xMeanStdCol]

# Now to drop columns with meanFreq in their names.
cnames <- colnames(xMeanStdData)  # grab column names
xMeanFreqCol <- grepl(".Freq", cnames)  # return a logical vector of indicies to drop

sum(xMeanFreqCol)  # expect 13 | columns to drop
colDrop <- cnames[xMeanFreqCol]  # returns a vector with names to be dropped 

# recreate data frame by subsetting on column names NOT to be dropped
xMeanStdData <- xMeanStdData[, !(colnames(xMeanStdData) %in% colDrop)]  

# Verifying tidier dataset
ncol(xMeanStdData)  # expect 68
nrow(xMeanStdData)  # expect 10299
colnames(xMeanStdData)
# Examine sample data head & tail | first & last columns
head(xMeanStdData[, c(1, 2, 3, 4, 5, 6, 66, 67, 68)])
tail(xMeanStdData[, c(1, 2, 3, 4, 5, 6, 66, 67, 68)])


# Step 3
# Uses descriptive activity names to name the activities in the data set

colnames(xActivityLabels)  # review Activity Labels column names for merging
# Update the merged yTestData activity number column with the Activity Labels
xMeanStdData <- merge(xMeanStdData, xActivityLabels, by.x = "V1.1", by.y = "V1", all = TRUE)
ncol(xMeanStdData)  # expect 69 (1 new column)
colnames(xMeanStdData)
# Examine sample data head & tail | first & last columns
head(xMeanStdData[, c(1, 2, 3, 4, 5, 6, 66, 67, 68, 69)])
tail(xMeanStdData[, c(1, 2, 3, 4, 5, 6, 66, 67, 68, 69)])

# We no longer need V1.1 as the textual data was merged with its corresponding activity.  
xMeanStdData$V1.1 <- NULL
ncol(xMeanStdData)  # expect 68 - we basically replaced Activity number with its name


# Step 4
# Appropriately labels the data set with descriptive variable names.

# All but the last 2 column labels have descriptive names read in during read.table on xDataFiles
# Refer to xActivityLabels
colnames(xMeanStdData)  # review column names

# Rename the generic V & V2 to appropriate Subject and Activity labels.  
colnames(xMeanStdData)[67] <- "Subject.ID"
colnames(xMeanStdData)[68] <- "Activity"

# Convert Subject.ID to numeric for proper sorting in Step 5
is.numeric(as.numeric(xMeanStdData$Subject.ID)) # test validity | expect TRUE
xMeanStdData$Subject.ID <- as.numeric(as.character(xMeanStdData$Subject.ID))

# reorder columns placing Subject.ID and Activity 1st
xMeanStdData <- xMeanStdData[,c(67,68,1:66)]

colnames(xMeanStdData)


# Step 5

# Creates a second, independent tidy data set with the average of each variable.
# Utilize function ddply from the plyr library. This function accomplishes 2 tasks.  
# 1. This breaks down the tidy dataset xMeanStdData created above into a tidier
#    dataset by Subject.ID and Activity.
# 2. Summarizes the 66 measured variables for standard deviation and means by
#    way of an anonymous function taking the means for those variables.
# The resultant new dataframe TidyDataSet is sorted by Subject.ID and Activity.  
TidyDataSet <-ddply(xMeanStdData, c("Subject.ID","Activity"), 
                    function(x) colMeans(x[c(colnames(xMeanStdData)[3:68])]))
#
dim(TidyDataSet)  
# [1] 180  68

head(TidyDataSet,12)

# write TidyDataSet to TidyData.txt into the working directory
write.table(TidyDataSet, file = "TidyData.txt", sep = ",", eol = "\r", row.names = FALSE, col.names = TRUE)

file.info("TidyData.txt")
# TidyData.txt size 224153

# write xMeanStdData to MeanStdData.txt  into the working directory
write.table(xMeanStdData, file = "MeanStdData.txt", sep = ",", eol = "\r", row.names = FALSE, col.names = TRUE)


# The Tidy Dataset is completed.  
# The filename is TidyData.txt @ https://github.com/davparker/DataScience_03_Project/blob/master/TidyData.txt).
# Located in the working directory of the script.  

