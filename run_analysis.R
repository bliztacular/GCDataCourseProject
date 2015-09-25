#This script, called run_analysis.R, will do the following: 
# 1. Merge the training and the test sets to create one data set.
# 2. Extract only the measurements on the mean and standard deviation for each measurement. 
# 3. Use descriptive activity names to name the activities in the data set
# 4. Appropriately label the data set with descriptive variable names. 
# 5. From the data set in step 4, create a second, independent tidy data set with the average of each variable for each activity and each subject.

# 1. Merge the training and test sets to create one data set.

setwd("~/Downloads/UCI HAR Dataset") # set working directory

# read in features, activity type etc for training data
features     = read.table('./features.txt',header=FALSE);  
activityType = read.table('./activity_labels.txt',header=FALSE); 
subjectTrain = read.table('./train/subject_train.txt',header=FALSE); 
xTrain       = read.table('./train/x_train.txt',header=FALSE); 
yTrain       = read.table('./train/y_train.txt',header=FALSE);

# assign column names to the data
colnames(activityType)  = c('activityId','activityType');
colnames(subjectTrain)  = "subjectId";
colnames(xTrain)        = features[,2]; 
colnames(yTrain)        = "activityId";

# merge ytrain, subjecttrain, xtrain to create final training set

dataTraining = cbind(yTrain, subjectTrain, xTrain);

# read in features, activity type etc for test data

subjectTest = read.table('./test/subject_test.txt',header=FALSE); 
xTest       = read.table('./test/x_test.txt',header=FALSE); 
yTest       = read.table('./test/y_test.txt',header=FALSE);

# assign column names to the data
colnames(activityType)  = c('activityId','activityType');
colnames(subjectTest)  = "subjectId";
colnames(xTest)        = features[,2]; 
colnames(yTest)        = "activityId";

# merge ytrain, subjecttrain, xtrain to create final test set

dataTest = cbind(yTest, subjectTest, xTest);

#combine test and training data

mergeData = rbind(dataTraining, dataTest);

# 2. Extract only the measurements on the mean and standard deviation for each measurement.

# make a vector to name the columns 
colNames = colnames(mergeData)
# make a vector to match measurements on the mean and standard deviation for each measurement
logicalVector = (grepl("activity..",colNames) | grepl("subject..",colNames) | grepl("-mean..",colNames) & !grepl("-meanFreq..",colNames) & !grepl("mean..-",colNames) | grepl("-std..",colNames) & !grepl("-std()..-",colNames));
# subset the data to select only values where the logical vector returns true
mergeData = mergeData[logicalVector==TRUE];

# 3. Use descriptive activity names to name the activities in the data set

# using activity names from activity table and merge with finaldata

mergeData = merge(finalData,activityType,by='activityId',all.x=TRUE);

# now the column names are different so we need to update them

colNames  = colnames(mergeData); 

# 4. Appropriately label the data set with descriptive variable names.

for (i in 1:length(colNames)) 
{
        colNames[i] = gsub("\\()","",colNames[i])
        colNames[i] = gsub("-std$","StdDev",colNames[i])
        colNames[i] = gsub("-mean","Mean",colNames[i])
        colNames[i] = gsub("^(t)","time",colNames[i])
        colNames[i] = gsub("^(f)","freq",colNames[i])
        colNames[i] = gsub("([Gg]ravity)","Gravity",colNames[i])
        colNames[i] = gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",colNames[i])
        colNames[i] = gsub("[Gg]yro","Gyro",colNames[i])
        colNames[i] = gsub("AccMag","AccMagnitude",colNames[i])
        colNames[i] = gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",colNames[i])
        colNames[i] = gsub("JerkMag","JerkMagnitude",colNames[i])
        colNames[i] = gsub("GyroMag","GyroMagnitude",colNames[i])
};

# one final column name reassignment now that we've prettied them up

colNames  = colnames(mergeData); 

# 5. Create a second, independent tidy data set with the average of each variable for each activity and each subject. 

# first we make a data table without activity type included
finalDataWOActivityType  = mergeData[,names(mergeData) != 'activityType'];

# pivot the data table to only include means
tidyData    = aggregate(finalDataWOActivityType[,names(finalDataWOActivityType) != c('activityId','subjectId')],by=list(activityId=finalDataWOActivityType$activityId,subjectId = finalDataWOActivityType$subjectId),mean);

# merge the tidyData with activityType to include descriptive acitvity names
tidyData    = merge(tidyData,activityType,by='activityId',all.x=TRUE);

# Export the tidyData set 
write.table(tidyData, './tidyData.txt',row.names=TRUE,sep='\t')











