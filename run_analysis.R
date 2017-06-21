library(dplyr)


#0 Downloading Dataset into data folder and naming it as Dataset.zip

if(!file.exists("./data")){dir.create("./data")}
fileURL<- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileURL, destfile = "./data/Dataset.zip")

  #0.1 Unziping the downloaded data set
  unzip(zipfile = "./data/Dataset.zip", exdir = "./data")

#1 Merging the training and the test sets to create one data set.

  #1.1 Reading files

    #1.1.1 Reading trainings tables
    x_train<- read.table("./data/UCI HAR Dataset/train/X_train.txt")
    y_train<- read.table("./data/UCI HAR Dataset/train/y_train.txt")
    subject_train<- read.table("./data/UCI HAR Dataset/train/subject_train.txt")
    
    
    #1.1.2 Reading testing tables
    x_test<- read.table("./data/UCI HAR Dataset/test/X_test.txt")
    y_test<- read.table("./data/UCI HAR Dataset/test/y_test.txt")
    subject_test<- read.table("./data/UCI HAR Dataset/test/subject_test.txt")
    
    #1.1.3 Reading feature table
    features<- read.table("./data/UCI HAR Dataset/features.txt")
    
    #1.1.4 Reading activity labels:
    activityLabels<- read.table("./data/UCI HAR Dataset/activity_labels.txt")
    
    
  #1.2 Assigning Column names
    colnames(x_train)<- features[,2]
    colnames(y_train)<- "activityId"
    colnames(subject_train)<- "subjectId"
    
    colnames(x_test)<- features[,2]
    colnames(y_test)<- "activityId"
    colnames(subject_test)<- "subjectId"
    
    colnames(activityLabels)<- c("activityId", "activityType")

  #1.3 Merging all data into one set
    mrg_train<- cbind(y_train, subject_train, x_train)
    mrg_test<- cbind(y_test, subject_test, x_test)
    MergingInOne<- rbind(mrg_train,mrg_test)
    
    
#2 Extracting only the measurements on the mean and standard deviation for each measurement
    
    #2.1 Reading column names
    column<- colnames(MergingInOne)
    
    #2.2 Create vector for defining ID, mean & standard deviation:
    mean_and_sd<- (grepl("activityId", column)| grepl("subjectId", column)| 
                   grepl("mean..", column)| grepl("std..", column))
    #2.3 Making subset from MergingInOne
    resultForMeanAndSd<- MergingInOne[,mean_and_sd== TRUE]
    
#3 Using Descriptive activity names to name the activities in the dataset:
    
    descriptiveActivity<- merge(resultForMeanAndSd, activityLabels, by='activityId', all.x= TRUE)
        
#4 Appropriately labels the data set with descriptive variable names.
    #This was done in previos steps =) See 1.3, 2.2, 2.3.
    
#5 From the data set in step 4, creates a second, independent tidy data set with the average
#of each variable for each activity and each subject. 
  
    #5.1 Making second tidy data set   
    secTidySet<- aggregate(. ~subjectId + activityId, descriptiveActivity, mean)
    secTidySet <- secTidySet[order(secTidySet$subjectId, secTidySet$activityId),]
    
    # 5.2 Writing second tidy data set into a txt file
    write.table(secTidySet, "secTidySet.txt", row.name=FALSE)
      