library(reshape2)
filename<-"getdata_dataset.zip"

#Download an unzip the data
if(!file.exist(filename)){
        fileURL<-"https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip "
        download.file(fileURL,filename,method="curl")
}

if (!file.exists("UCI HAR Dataset")){
        unzip(filename)
}

#1.- merge the training and the test sets to create one data set

#first load hte features and the activity labels

activityLabels<-read.table("UCI HAR Dataset/activity_labels.txt")
features<-read.table("UCI HAR Dataset/features.txt")
activityLabels[,2]<-as.character(activityLabels[,2])
features[,2]<-as.character(features[,2])


#then filter the data that we need
featuresWeWant<-grep(".*mean.*|.*std.*",features[,2])
featuresNames<-features[featuresWeWant,2]

#2.-Extracts only the measurements on the mean and standard deviation for each measurement.
featuresNames<-gsub("-mean","Mean",featuresNames)
featuresNames<-gsub("-std","Std",featuresNames)
featuresNames<-gsub("[-()]","",featuresNames)

#then load the data

subjectTest<-read.table("UCI HAR Dataset/test/subject_test.txt")
xTest<-read.table("UCI HAR Dataset/test/X_test.txt")[featuresWeWant]
yTest<-read.table("UCI HAR Dataset/test/y_test.txt")
dataTest<-cbind(subjectTest,yTest,xTest)

subjectTrain<-read.table("UCI HAR Dataset/train/subject_train.txt")
xTrain<-read.table("UCI HAR Dataset/train/X_train.txt")[featuresWeWant]
yTrain<-read.table("UCI HAR Dataset/train/y_train.txt")
dataTrain<-cbind(subjectTrain,yTrain,xTrain)

#3.-Uses descriptive activity names to name the activities in the data set
combineData<-rbind(dataTest,dataTrain)
colnames(combineData)<-c("sujeto","Actividades",featuresNames)

#4.-Appropriately labels the data set with descriptive variable names.

combineData$Actividad<-factor(combineData$Actividad, levels = activityLabels[,1], labels = activityLabels[,2])

combineData$sujeto <- as.factor(combineData$sujeto)

combineDataMealt <- melt(combineData, id = c("sujeto", "Actividad"))
combineDataMean <- dcast(combineDataMealt, sujeto + Actividad ~ variable, mean)


#5.-From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
write.table(combineDataMean, "tidy.txt", row.names = FALSE, quote = FALSE)


