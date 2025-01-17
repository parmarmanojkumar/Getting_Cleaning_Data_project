Getting-and-Cleaning-Data-Project
=================================
##Instructions for project
The purpose of this project is to demonstrate your ability to collect, work with, and clean a data set. The goal is to prepare tidy data that can be used for later analysis. You will be graded by your peers on a series of yes/no questions related to the project. You will be required to submit: 1) a tidy data set as described below, 2) a link to a Github repository with your script for performing the analysis, and 3) a code book that describes the variables, the data, and any transformations or work that you performed to clean up the data called CodeBook.md. You should also include a README.md in the repo with your scripts. This repo explains how all of the scripts work and how they are connected. 

One of the most exciting areas in all of data science right now is wearable computing - see for example this article . Companies like Fitbit, Nike, and Jawbone Up are racing to develop the most advanced algorithms to attract new users. The data linked to from the course website represent data collected from the accelerometers from the Samsung Galaxy S smartphone. A full description is available at the site where the data was obtained: 

http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones

Here are the data for the project:

https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 

You should create one R script called run_analysis.R that does the following. 

1. Merges the training and the test sets to create one data set.
2. Extracts only the measurements on the mean and standard deviation for each measurement. 
3. Uses descriptive activity names to name the activities in the data set
4. Appropriately labels the data set with descriptive variable names.
5. From the data set in step 4, creates a second, independent tidy data set with the average 
   of each variable for each activity and each subject.

##Tools used to produce this project
- This project is built by `knirt` package instllled on RStudio with RMD (R Markdown) format.

- The `codebook.md` files will be  automatically  produced by `run_analysis.R`

##Steps to produce this project

- Plsease see the details in `run_analysis.R` and `codebook.md`.

- `run_analysis.R` file contains the steps to produce this project.

- The final tidy data is in `tidydataset.txt`. It can be loaded by `Data<-read.table("tidydataset.txt", sep=" ", head=TRUE)`

- The codebok is in `codebook.md` . It gives the descriptions of the variables in the data frame prouduced by this project.

##Steps to reproduce this project

-  Open Rstudio to open R file  `run_analysis.R` to build the Project. then `codebook.md` and `tidydataset.txt` will be produced.

##Steps used for analysis
- Step00.0 : setting up environment
- Step01.0 : Download data and put it in folder if it is not there
- Step02.0 : Unzip file
- Step03.0 : get the list of files from unzipped version to process it further.
- Step04.0 : Start reading data
- Step04.1 : read `y_test.txt` and `y_train.txt` 
- Step04.2 : read `subject_train.txt` and read `subject_test.txt`
- Step04.3 : read `x_test.txt` and `x_train.txt`
- Step05.0 : Merge training and test data
- Step06.0 : naming of varibales by reading from `feature.txt`
- Step07.0 : Combine all data to make final dataframe
- Step08.0 : extract mean and standard deviation for each measurement from final data
- Step09.0 : taking subset of data based on extraction labels
- Step10.0 : make variables more discriptive
- Step10.1 : replace activity variable number with discription from `activity_labels.txt`
- Step10.2 : expand & correct name of variables using regex
- Step11.0 : Create final tiday dataset by taking average for each activity for each subject for all variables
- Step12.0 : build codebook