Getting and Cleaning Data Project

Description
Additional information about the variables, data and transformations used in this course project for the Johns Hopkins Getting and Cleaning Data course.

Source Data
You have to download source data from link below and unzip it to working directory of R Studio.
You have to perform R script.

About source data

As source data for work was used Human Activity Recognition Using Smartphones Data Set. A full description is available at the site where the data was obtained: http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones Here are the data for the project: https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

About R script

File with R code "run_analysis.R" performs the following 6 steps 

0. Downloading Dataset into data folder and naming it as Dataset.zip
0.1 Unziping the downloaded data set

1. Merging the training and the test sets to create one data set.
1.1 Reading files
1.1.1 Reading trainings tables
1.1.2 Reading testing tables
1.1.3 Reading feature table
1.1.4 Reading activity labels
1.2 Assigning column names
1.3 Merging all data in one set

2. Extracting only the measurements on the mean and standard deviation for each measurement
2.1 Reading column names
2.2 Create vector for defining ID, mean and standard deviation
2.3 Making subset from MergingInOne

3. Using descriptive activity names to name the activities in the data set

4. Appropriately labeling the data set with descriptive variable names
Creating a second, independent tidy data set with the average of each variable for each activity and each subject

5. From the data set in step 4, creating a second, independent tidy data set with the average of each variable for each activity and each subject.

5.1 Making second tidy data set
5.2 Writing second tidy data set in txt file

Please Note: The code takes for granted all the data is present in the same folder, un-compressed and without names altered.

About variables:

x_train, y_train, x_test, y_test, subject_train and subject_test contain the data from the downloaded files.
features contains the data from features.txt
activityLabels contains the data from activity_labels.txt
mrg_train contains the column bind values from x_train, y_train and subject_train
mrg_test contains the column bind values from x_test, y_test and subject_test
MergingInOne contains the merged dataset from mrg_train and mrg_test.
column contains the column names of MergingInOne
mean_and_sd contains the logical values for the matching columns
resultForMeanAndSd contains the subset of MergingInOne where mean_and_sd is TRUE
descriptiveActivity contains the descriptive activity names for the activities in the data set.
secTidySet contains the average of each variable for each activity and each subject