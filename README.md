DataScience_03_Project
======================  
  
Coursera Data Science Specialization Track class 03 Course Project  
---
title: "Coursera Project Tidy Data"
author: "David Parker"
date: "Sunday, July 27, 2014"
output: html_document
---

### Objective:
To create a "Tidy" dataset from data published in an experiment using wearable smart devices. This is a "Data Science, Wearable Computing" experiment. The data from this experiment, titled "Human Activity Recognition on Smartphones using a Multiclass Hardware-Friendly Support Vector Machine" [1], is publicly avaible, cited below in this document.

### Process:
A single R script handes all the processing. It is located [here](https://github.com/davparker/DataScience_03_Project/blob/master/run_analysis.R).  
  
In preperation to aquire "Human Activity Recognition Using Smartphones Data Set", the script first checks to see if a copy is present in the working directory. If not it downloads it from:  
[https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip](https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip)  
  
The script then unzips the data into the working directory. Next, the script reads in the datasets in the following order:  
First gathers data common to both test and train:  
+ Read *actvity_labels.txt* into, dataframe: __xActivityLabels__  
+ Read *features.txt* labels into, dataframe: __xColNames__ (these are column names common to Train & Test)  
Then collects data for test subjects:  
+ Read *x_test.txt* variables into dataframe: __xTestData__ (using column names above)  
+ Read *y_test.txt* activities into dataframe: __yTestData__  
+ Read *subject_test.txt* subjects into datafrae: __TestSubject__  
Next collects data for train subjects:  
+ Read *x_train.txt* variables into dataframe: __xTrainData__ (using column names above)  
+ Read *y_train.txt* activities into dataframe: __yTrainData__  
+ Read *subject_train.txt* subjects into datafrae: __TrainSubject__  
  
Now the steps for processing the data begins:    
  
###Step 1  
__Merges the training and the test sets to create one data set__.    
Both the Test and Train datasets should contain the same number of rows.  
Verify test data row counts.  
```{r}  
nrow(xTestData)  # ecpect 2947  
identical(nrow(xTestData), nrow(yTestData))  # expect TRUE  
identical(nrow(xTestData), nrow(TestSubject))  # expect TRUE  
```  
  
The 3 tables in test have the same number of rows, _2947_.  
Create one test data frame combining columns from xTestData, TestSubject, yTestData.  
```{r}  
xTestData <- cbind(xTestData, TestSubject)   
xTestData <- cbind(xTestData, yTestData)   
```  
Verify train data row counts  
```{r}  
nrow(xTrainData)  # expect 7352  
identical(nrow(xTrainData), nrow(yTrainData))    # expect TRUE  
identical(nrow(xTrainData), nrow(TrainSubject))  # expect TRUE  
```  
The 3 tables in train have the same number of rows, _7352_.  
Create one train data frame combining xTrainData, TrainSubject, yTrainData,  
```{r}  
xTrainData <- cbind(xTrainData, TrainSubject)  
xTrainData <- cbind(xTrainData, yTrainData)  
```  
Verify colnames prior to merging.  
```{r}  
identical(colnames(xTestData), colnames(xTrainData))  # expect TRUE  
```  
Merge all the data into one data frame.  
```{r}  
xMergeData <- rbind(xTrainData, xTestData)  
```  
  
Verify the rowcount of Merged Data equal the sum of Test + Train.  
```{r}  
identical(nrow(xMergeData), (nrow(xTestData) + nrow(xTrainData) ) )  # expect TRUE  
```  
  
###Step 2  
Extracts only the measurements on the mean and standard deviation for each measurement.  
  
Gets all column _indicies_ for standard deviation, mean, generic _V1_ _V1.1_ added by Subject & yData.  
_Note: columns with meanFreq in their names get selected here, pulled in with the other mean columns, but these will be dropped at the end of this step. Easier that way._    
```{r}  
xMeanStdCol <- grepl(".std|.mean|^V", colnames(xMergeData) )  
sum(xMeanStdCol)  # sum of filtered column indicies | expect 81  
```  
  
Extracting data using filtered column indicies into a new dataset: __xMeanStdData__  
```{r}  
xMeanStdData <- xMergeData[, xMeanStdCol]  
```  
Now to drop columns with _meanFreq_ in their names.  
```{r}  
cnames <- colnames(xMeanStdData)  # grab column names  
xMeanFreqCol <- grepl(".Freq", cnames)  # return a logical vector of indicies to drop  
```  
```{r}  
sum(xMeanFreqCol)  # expect 13 | columns to drop  
colDrop <- cnames[xMeanFreqCol]  # returns a vector with names to be dropped  
```  
  
Recreate data frame by subsetting on column names NOT to be dropped.  
```{r}  
xMeanStdData <- xMeanStdData[, !(colnames(xMeanStdData) %in% colDrop)]  
```  
Verifying tidier dataset.  
```{r}  
ncol(xMeanStdData)  # expect 68 columns  
nrow(xMeanStdData)  # expect 10299  
colnames(xMeanStdData)  
```  
Examine sample data head & tail | first & last columns.  
```{r}  
head(xMeanStdData[, c(1, 2, 3, 4, 5, 6, 66, 67, 68)])  
tail(xMeanStdData[, c(1, 2, 3, 4, 5, 6, 66, 67, 68)])  
```  
  
###Step 3  
Use descriptive activity names to name the activities in the data set.  
```{r}  
colnames(xActivityLabels)  # review Activity Labels column names for merging  
```  

__Update__ the merged _yTestData_ activity number column with the _xActivityLabels_.  
```{r}  
xMeanStdData <- merge(xMeanStdData, xActivityLabels, by.x = "V1.1", by.y = "V1", all = TRUE)  
ncol(xMeanStdData)  # expect 69 (1 new column)  
colnames(xMeanStdData)  
```  
Examine sample data head & tail | first & last columns.  
```{r}  
head(xMeanStdData[, c(1, 2, 3, 4, 5, 6, 66, 67, 68, 69)])  
tail(xMeanStdData[, c(1, 2, 3, 4, 5, 6, 66, 67, 68, 69)])  
```  
We no longer need V1.1 as the textual data was merged with its corresponding activity.  
```{r}  
xMeanStdData$V1.1 <- NULL  
ncol(xMeanStdData)  # expect 68 - we basically replaced Activity number with its name  
```  
  
###Step 4  
Appropriately labels the data set with descriptive variable names.  
  
All but the last 2 column labels have descriptive names read in during _read.table_ on _xDataFiles_.  
_Refer to xActivityLabels_  
```{r}  
colnames(xMeanStdData)  # review column names  
```  
Rename the generic V & V2 to appropriate __Subject__ and __Activity__ labels.  
```{r}  
colnames(xMeanStdData)[67] <- "Subject.ID"  
colnames(xMeanStdData)[68] <- "Activity"  
```  
Convert Subject.ID to numeric for proper sorting in Step 5.  
```{r}  
is.numeric(as.numeric(xMeanStdData$Subject.ID)) # test validity | expect TRUE  
xMeanStdData$Subject.ID <- as.numeric(as.character(xMeanStdData$Subject.ID))  
```  
Reorder columns placing Subject.ID and Activity 1st.  
These are not measured variables. They are the Activities the measured variables will be summarized to.  
```{r}  
xMeanStdData <- xMeanStdData[,c(67,68,1:66)]  
colnames(xMeanStdData)  # observe new column arrangement  
``` 
  
###Step 5  
Creates a second, independent tidy data set with the average of each variable for each _Activity_ and each _Subject_.  
Utilize function _ddply_ from the _plyr_ library. This function accomplishes 2 tasks.  
1. This breaks down the tidy dataset _xMeanStdData_ created above into a __tidier__ dataset by _Subject.ID_ and _Activity_.  
2. Summarizes the 66 measured variables for standard deviation and means by way of an _anonymous function_ taking the means for those variables.  
The resultant new dataframe __TidyDataSet__ is sorted by _Subject.ID_ and _Activity_.  

```{r}  
TidyDataSet <-ddply(xMeanStdData, c("Subject.ID","Activity"),  
                    function(x) colMeans(x[c(colnames(xMeanStdData)[3:68])]))  
```  
Examine specifics for resulting __Tidy__ dataset.  
```{r}  
dim(TidyDataSet)  
# [1] 180  68  
```  
```{r}  
head(TidyDataSet,12)  
```  
  
Write __TidyDataSet__ to __TidyData.csv__ into the _working directory_.  
```{r}  
write.csv(TidyDataSet, file = "TidyData.csv",row.names=FALSE)  
```  
  
Retrieve the file info.  
```{r}  
file.info("TidyData.csv")  
```
_TidyData.csv size is 224334_  
  
Write xMeanStdData to MeanStdData.csv  into the working directory.  
```{r}  
write.csv(xMeanStdData, file = "MeanStdData.csv",row.names=FALSE)  
```  

###The Tidy Dataset is completed.  
###The filename is [__TidyData.csv__](https://github.com/davparker/DataScience_03_Project/blob/master/TidyData.csv) located in the working directory of the script.  
  
  
  
  
  
  
[1] Davide Anguita, Alessandro Ghio, Luca Oneto, Xavier Parra and Jorge L. Reyes-Ortiz. Human Activity Recognition on Smartphones using a Multiclass Hardware-Friendly Support Vector Machine. International Workshop of Ambient Assisted Living (IWAAL 2012). Vitoria-Gasteiz, Spain. Dec 2012
[__Citation__](http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones)