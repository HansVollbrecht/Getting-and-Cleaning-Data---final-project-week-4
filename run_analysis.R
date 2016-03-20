#assumption - the dataset has been downloaded adn stored already with the nam data_set.zip

library(dplyr)

#set working directory
setwd("~/Documents/R-Courses/Getting and Cleaning Data/Week 4")

#unzip data into new directory named "data
unzip("data_set.zip", exdir = "data")

#################TODOS###############################################
# 1.	Merges the training and the test sets to create one data set.
# 2.	Extracts only the measurements on the mean and standard deviation for each measurement. 
# 3.	Uses descriptive activity names to name the activities in the data set
# 4.	Appropriately labels the data set with descriptive variable names. 
# 5.	From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.


# 1. Merge training and test data sets

#read test data
#dimensions X_test: 2947  561 -> measurements with 561 variables
X_test <- read.table(file = "data/UCI HAR Dataset/test/X_test.txt",header = FALSE)

#dimensions y_test: 2947 1 -> activity labels (1-6) per measurement
y_test <- read.table(file = "data/UCI HAR Dataset/test/y_test.txt",header = FALSE)

#dimensions subject_test: 2947 1 -> which of 30% (9 persons) of test participants was measured
subject_test <- read.table(file = "data/UCI HAR Dataset/test/subject_test.txt",header = FALSE)

#dim body_acc_x-z_test: 2947 128 -> acceleration signal x-z axis 128 element vector
# body_acc_x_test <- read.table(file = "data/UCI HAR Dataset/test/Inertial Signals/body_acc_x_test.txt",header = FALSE)
# body_acc_y_test <- read.table(file = "data/UCI HAR Dataset/test/Inertial Signals/body_acc_y_test.txt",header = FALSE)
# body_acc_z_test <- read.table(file = "data/UCI HAR Dataset/test/Inertial Signals/body_acc_z_test.txt",header = FALSE)

#dims body_gyros_x-z_test: 2947 1 -> gyroscope x-z data with 128 elements vecotrs
# body_gyro_x_test <- read.table(file = "data/UCI HAR Dataset/test/Inertial Signals/body_gyro_x_test.txt",header = FALSE)
# body_gyro_y_test <- read.table(file = "data/UCI HAR Dataset/test/Inertial Signals/body_gyro_y_test.txt",header = FALSE)
# body_gyro_z_test <- read.table(file = "data/UCI HAR Dataset/test/Inertial Signals/body_gyro_z_test.txt",header = FALSE)

#read column headers for 561 measures
column.names <- read.table(file = "data/UCI HAR Dataset/features.txt", header = FALSE, stringsAsFactors = FALSE)

#read training data
#dim(X_train): 7352 561
X_train <- read.table(file = "data/UCI HAR Dataset/train/X_train.txt")

#dimensions subject_train: 7352 1 -> which 70% (21 persons) of training participants was measured
subject_train <- read.table(file = "data/UCI HAR Dataset/train/subject_train.txt",header = FALSE)

#dimensions y_train: 2947 1 -> activity labels (1-6) per measurement
y_train <- read.table(file = "data/UCI HAR Dataset/train/y_train.txt",header = FALSE)


#combine training and test data frames for measurements
X_all <- rbind(X_test,X_train)
colnames(X_all)<-t(column.names[2]) #names are in second column and need to be transposed

#combine training and test data frames for subjects
subjects_all <- rbind(subject_test,subject_train)

#combine activity labels for test and training data frames
y_all <- rbind(y_test,y_train)

#-----------------------------------------------------------------------------
# 2.	Extracts only the measurements on the mean and standard deviation for each measurement. 

#retrieve  columns with "means" in their column names and retrieve columns with
#"std" (standard deviation) in their column name
relevant <- X_all[,grepl("[Mm]ean|std",names(X_all))]


#------------------------------------------------------------------------------
# 3.	Uses descriptive activity names to name the activities in the data set
#get activity mapping 
activities <- read.table("data/UCI HAR Dataset/activity_labels.txt", header = FALSE)

#remove underscores and convert to lowercase to clean up activity names
activities$V2<-gsub("_",tolower(activities$V2),replacement = "")

#assigning activities to observations
y_all <- merge(y_all,activities) 
#keep only the activity labels
y_all <- y_all[,2]

#------------------------------------------------------------------------------
# 4.	Appropriately labels the data set with descriptive variable names. 
#list with old values and the new col names
nval<-c("gravity","body","standarddeviation","frequency","acceleration","and","magnitude","of","","")
old <-c("tGravity","[f|t]Body","std","Freq","Acc",",","Mag","\\(","\\)","-")

#replace old vals with new vals
n <- colnames(relevant)
n <-sub(replacement ="", pattern =  "\\(\\)",x=n)
for (i in 1:length(old)) {
  n <- gsub(old[i],n,replacement=nval[i])
}

#convert to lowercase and assign to columns
colnames(relevant)<- tolower(n)

#-------------------------------------------------------------------------------
# 5.	From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
#combine subject IDs, activities and variables
tidy_means <- cbind(subjects_all,y_all,relevant)
colnames(tidy_means) <- c("subject","actvities",colnames(relevant))

#group data
tidy_means <- group_by(tidy_means, subject, actvities)

#summarize by subjects
subject_means<-aggregate(tidy_means[-2],list(tidy_means$subject),mean)

#summarize by activity
activity_means<-aggregate(tidy_means[-2],list(tidy_means$actvities),mean)

#combine subject and activity means
tidy_means <- rbind(activity_means,subject_means)

#save the data.frame
write.csv(tidy_means, file = "tidy_means.csv")
