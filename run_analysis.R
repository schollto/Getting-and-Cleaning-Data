temp<-tempfile(fileext=".zip")
#Download hte file data from the url
download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip",temp)
#Unzip the file

unzip(temp, list=T)

#Load activity labels + features

features<-read.table(unz(temp, "UCI HAR Dataset/features.txt"), header=F)
features<-as.character(features[,2])
activityLabels<-read.table(unz(temp, "UCI HAR Dataset/activity_labels.txt"), header=F)
activityLabels<-as.character(activityLabels[,2])
dataTrainX<-read.table(unz(temp, "UCI HAR Dataset/train/X_train.txt"), header=F)
dataTrainY<-read.table(unz(temp, "UCI HAR Dataset/train/y_train.txt"), header=F)
dataTrainSubject<-read.table(unz(temp, "UCI HAR Dataset/train/subject_train.txt"), header=F)
dataTestX<-read.table(unz(temp, "UCI HAR Dataset/test/X_test.txt"), header=F)
dataTestY<-read.table(unz(temp, "UCI HAR Dataset/test/y_test.txt"), header=F)
dataTestSubject<-read.table(unz(temp, "UCI HAR Dataset/test/subject_test.txt"), header=F)


#Merge the data sets and call Combined
dataTraining<-data.frame(dataTrainSubject, dataTrainY, dataTrainX)
dataTesting<-data.frame(dataTestSubject, dataTestY, dataTestX)
# rename the training and test data sets
names(dataTraining)<-c(c('subject', 'activity'), features)
names(dataTesting)<-c(c('subject', 'activity'), features)
#1. Merge the training and the test sets to create one data set
Combined<-rbind(dataTraining, dataTesting)
#2. Extracts only the measurements on the mean and standard deviation for each measurement.
Combinedact<-Combined[,which(colnames(data) %in% c("subject", "activity", grep("mean\\(\\)|std\\(\\)", colnames(data), value=TRUE)))]

#3. Use descriptive activity names to name the activities in the data set
Combinedact$activity<-activityLabels[Combinedact$activity]

#4. Appropriately label the data set with descriptive variable names


unique(gsub("\\-(mean|std)\\(\\).*", "", names(Combined)[-c(1:2)]))

names(Combined)[-c(1:2)]<-gsub("^t", "Time", names(Combined)[-c(1:2)])
names(Combined)[-c(1:2)]<-gsub("^f", "Frequency", names(Combined)[-c(1:2)])
names(Combined)[-c(1:2)]<-gsub("Acc", "Accelerometer", names(Combined)[-c(1:2)])
names(Combined)[-c(1:2)]<-gsub("Gyro", "Gyroscope", names(Combined)[-c(1:2)])
names(Combined)[-c(1:2)]<-gsub("Mag", "Magnitude", names(Combined)[-c(1:2)])
names(Combined)[-c(1:2)]<-gsub("BodyBody", "Body", names(Combined)[-c(1:2)])

#5. From the data set in step 4, create a second, independent tidy data set with the average of each variable for each activity and each subject
tidy<-aggregate(. ~ subject + activity, Combined, mean)
tidy<-tidy[order(tidy$subject,tidy$activity),]

write.csv(tidy, file = "tidy.csv")


