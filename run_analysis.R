
# ===============================================================================
# getting data 
# ===============================================================================

fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl,destfile = "data.zip")
unzip("data.zip", list = T)

# ===============================================================================
# Reading data (Packages )
# ===============================================================================

library(tidyverse)
library(data.table)

feature <- read.table("features.txt",header = F)

# ====================================================================
# reading data from folder 
# =====================================================================

## Reading "Training" Data

activity_names<-read.table("activity_labels.txt", header = FALSE)
features_train<-read.table("./train/X_train.txt",header = FALSE)
activity_train<-read.table("./train/Y_train.txt",header=FALSE)
subjects_train<-read.table("./train/subject_train.txt",header=FALSE)

bodyAccX_train<-read.table("./train/Inertial Signals/body_acc_x_train.txt",header=FALSE)
bodyAccY_train<-read.table("./train/Inertial Signals/body_acc_y_train.txt",header=FALSE)
bodyAccZ_train<-read.table("./train/Inertial Signals/body_acc_z_train.txt",header=FALSE)
bodyGyroX_train<-read.table("./train/Inertial Signals/body_gyro_x_train.txt",header=FALSE)
bodyGyroY_train<-read.table("./train/Inertial Signals/body_gyro_y_train.txt",header=FALSE)
bodyGyroZ_train<-read.table("./train/Inertial Signals/body_gyro_z_train.txt",header=FALSE)
totalAccX_train<-read.table("./train/Inertial Signals/total_acc_x_train.txt",header=FALSE)
totalAccY_train<-read.table("./train/Inertial Signals/total_acc_y_train.txt",header=FALSE)
totalAccZ_train<-read.table("./train/Inertial Signals/total_acc_z_train.txt",header=FALSE)

## Reading "Test" Data

features_test<-read.table("./test/X_test.txt",header = FALSE)
activity_test<-read.table("./test/Y_test.txt",header=FALSE)
subjects_test<-read.table("./test/subject_test.txt",header=FALSE)

bodyAccX_test<-read.table("./test/Inertial Signals/body_acc_x_test.txt",header=FALSE)
bodyAccY_test<-read.table("./test/Inertial Signals/body_acc_y_test.txt",header=FALSE)
bodyAccZ_test<-read.table("./test/Inertial Signals/body_acc_z_test.txt",header=FALSE)
bodyGyroX_test<-read.table("./test/Inertial Signals/body_gyro_x_test.txt",header=FALSE)
bodyGyroY_test<-read.table("./test/Inertial Signals/body_gyro_y_test.txt",header=FALSE)
bodyGyroZ_test<-read.table("./test/Inertial Signals/body_gyro_z_test.txt",header=FALSE)
totalAccX_test<-read.table("./test/Inertial Signals/total_acc_x_test.txt",header=FALSE)
totalAccY_test<-read.table("./test/Inertial Signals/total_acc_y_test.txt",header=FALSE)
totalAccZ_test<-read.table("./test/Inertial Signals/total_acc_z_test.txt",header=FALSE)

# =======================================================================================
# Renaming Datasets
# =======================================================================================

#rename colomuns in train set
names(features_train)<-feature$V2
#rename colomuns in test set
names(features_test)<-feature$V2
#rename colomun in activitys set in train
names(activity_train)<-"activity"
#rename colomun in activitys set in test
names(activity_test)<-"activity"
#rename colomun in subject set in train
names(subjects_train)<-"subject"
#rename colomun in subject set in test
names(subjects_test)<-"subject"

# Merging subjects, Activities, and Features ("Train")
subactfeat_train <- cbind(subjects_train,activity_train,features_train)
# Merging subjects, Activities, and Features ("Test")
subactfeat_test <- cbind(subjects_test,activity_test,features_test)

# ==========================================================================
# Answering the Questions
# ==========================================================================

# Q1: Merging Training & Test datasets 

train_test<-rbind(subactfeat_train,subactfeat_test)

# removing the duplicated column names

train_test <- train_test[ , !duplicated(colnames(train_test))]

# Q2: Extracting only the measurements on the mean and standard deviation for each measurement 

mean_sd <- train_test%>% select(matches("mean|std"))

# Q3 : adding descriptive activies names the combined data 

detail_train_test<-train_test%>% 
  arrange(activity) %>% 
  mutate(activity = as.character(factor(activity, levels=1:6, 
                                         labels= activity_names$V2)))

#Q4 : data set with descriptive variable names.
names(detail_train_test)<-gsub("tBodyAcc-","Body acceleration signal in time domain (from the accelerometer)",names(detail_train_test))
names(detail_train_test)<-gsub("tBodyAccMag-","Body acceleration signal in time domain applied to Fast Fourier Transform(from the accelerometer)",names(detail_train_test))
names(detail_train_test)<-gsub("tBodyAccJerk-","Body acceleration jerk signal in time domain (from the accelerometer)",names(detail_train_test))
names(detail_train_test)<-gsub("tBodyAccJerkMag-","Body acceleration jerk signal in time domain applied to Fast Fourrier Transform (from the accelerometer)",names(detail_train_test))
names(detail_train_test)<-gsub("tGravityAcc-","Gravity acceleration signal in time domain (from the accelerometer)",names(detail_train_test))
names(detail_train_test)<-gsub("tGravityAccMag-","Gravity acceleration signal in time domain applied to Fast Fourier Transform(from the accelerometer)",names(detail_train_test))
names(detail_train_test)<-gsub("tBodyGyro-","Body acceleration signal in time domain (from the gyroscope)",names(detail_train_test))
names(detail_train_test)<-gsub("tBodyGyroMag-","Body acceleration signal in time domain applied to Fast Fourrier Transform(from the gyroscope)",names(detail_train_test))
names(detail_train_test)<-gsub("tBodyGyroJerk-","Body acceleration jerk signal in time domain (from the gyroscope)",names(detail_train_test))
names(detail_train_test)<-gsub("tBodyGyroJerkMag-","Body acceleration jerk signal in time domain applied to Fast Fourrier Transform(from the gyroscope)",names(detail_train_test))
names(detail_train_test)<-gsub("fBodyAcc-","Body acceleration signal in frequence domain (from the accelerometer)",names(detail_train_test))
names(detail_train_test)<-gsub("fBodyAccMag-","Body acceleration signal in frequence domain applied to Fast Fourier Transform(from the accelerometer)",names(detail_train_test))
names(detail_train_test)<-gsub("fBodyAccJerk-","Body acceleration jerk signal in frequence domain (from the accelerometer)",names(detail_train_test))
names(detail_train_test)<-gsub("fBodyGyro-","Body acceleration signal in frequence domain (from the gyroscope)",names(detail_train_test))
names(detail_train_test)<-gsub("fBodyAccJerkMag-","Body acceleration jerk signal in frequence domain applied to Fast Fourrier Transform (from the accelerometer)",names(detail_train_test))
names(detail_train_test)<-gsub("fBodyGyroMag-","Body acceleration signal in frequence domain applied to Fast Fourier Transform (from the gyroscope)",names(detail_train_test))
names(detail_train_test)<-gsub("mean()", "MEAN", names(detail_train_test))
names(detail_train_test)<-gsub("std()", "SD", names(detail_train_test))

#Q5 : Tidy data with the "average" of "each variable" for "each activity" and "each subject"
tidydata<-detail_train_test%>%  group_by(subject,activity)%>%   summarise_all(mean)

# writing a Tidydataset into Text file 

write.table(tidydata, "MyTidyData.txt", row.name=FALSE)

# =============================== 0000 =====================================

