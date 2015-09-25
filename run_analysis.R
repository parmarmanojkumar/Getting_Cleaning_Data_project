#--------
#Title : Assignment_script
#Date : 25/09/2015
#Author : Manojkumar Parmar
#---------------
#step0: setting up environment
cat("\014")
rm(list=ls())
getwd()
readline(prompt = "Pause. Press <Enter> to continue...")
cat("\014")

#step1 : Download data and put it in folder if it is not there

if(!file.exists("./assignmentdata")){
        dir.create("./assignmentdata")
}
downloadUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
if(!file.exists("./assignmentdata/downloadeddata.zip")){
        download.file(downloadUrl,destfile="./assignmentdata/downloadeddata.zip",method="curl")
}

#step2 : Unzip file

unzip(zipfile="./assignmentdata/downloadeddata.zip",exdir="./assignmentdata")

#step3 : get the list of files from unzipped version to process it further.

datafilepath<- file.path("./assignmentdata" , "UCI HAR Dataset")
filelist<-list.files(datafilepath, recursive=T, full.names = T)

#step4 : Start reading data

#step4.1 : read y_test.txt and y_train.txt 
TestActivity  <- read.table(filelist[16],header = F)
TrainActivity <- read.table(filelist[28],header = F)

#step4.2 : read subject_train.txt and read subject_test.txt
TestSubject <- read.table(filelist[14], header = F)
TrainSubject <- read.table(filelist[26], header = F)

#step4.3 : read x_test.txt and x_train.txt
TestFeatures <- read.table(filelist[15], header = F)
TrainFeatures <- read.table(filelist[27], header = F)

#step5 : Merge training and test data
MergeActivity <- rbind(TrainActivity,TestActivity)
MergeSubject <- rbind(TrainSubject,TestSubject)
MergeFeatures <- rbind(TrainFeatures,TestFeatures)

#step6 : naming of varibales by reading from feature.txt
names(MergeSubject) <- "Subject"
names(MergeActivity) <- "Activity"
FeaturesLabel <- read.table(filelist[3], header = F)
FeaturesLabel <- FeaturesLabel$V2
names(MergeFeatures) <- FeaturesLabel

#step7 : Combine all data to make final dataframe
FinalData <- cbind(MergeSubject,MergeActivity)
FinalData <- cbind(MergeFeatures, FinalData)

#step8 : extract mean and standard deviation for each measurement from final data
#used regular expression to extract labels name which pattern mean() or std()
ExtractLabels <- as.character(FeaturesLabel[grep("mean\\(\\)|std\\(\\)",FeaturesLabel)])
ExtractLabels <- c(ExtractLabels,"Subject", "Activity")

#step9 : taking subset of data based on extraction labels
TidyData<-subset(FinalData,select=ExtractLabels)

#step10 : make variables more discriptive

#step10.1 : replace activity variable number with discription from activity_labels.txt
ActivityLabels <- read.table(filelist[1], header = F)
#convert activity into factors for replacement 
TidyData$Activity <- factor(TidyData$Activity)
TidyData$Activity<- factor(TidyData$Activity,labels=as.character(ActivityLabels$V2))

#step10.2 : expand & correct name of variables using regex
# t for time, f for frequncy,ACC for accelerometer, gyro for gyroscope, mag for magnitude

names(TidyData) <- gsub("^t", "time", names(TidyData))
names(TidyData) <- gsub("^f", "frequency", names(TidyData))
names(TidyData) <- gsub("Acc", "Accelerometer", names(TidyData))
names(TidyData) <- gsub("Gyro", "Gyroscope", names(TidyData))
names(TidyData) <- gsub("Mag", "Magnitude", names(TidyData))
names(TidyData) <- gsub("BodyBody", "Body", names(TidyData))

#step11 : Create final tiday dataset by taking average for each activity for each subject for all variables.

library(plyr);
TidyDataSet <- aggregate(. ~Subject + Activity, TidyData, mean)
TidyDataSet <- TidyDataSet[order(TidyDataSet$Subject,TidyDataSet$Activity),]
write.table(TidyDataSet, file = "tidydataset.txt",row.name=F)

#step12 : build codebook
library(knitr)
knit2html("codebook.Rmd")