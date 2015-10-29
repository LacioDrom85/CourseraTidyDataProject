
# PROJECT:

#####################################################################################################
#                                                                                                   #
# You should create one R script called run_analysis.R that does the following.                     #
#                                                                                                   #
# 1. Merges the training and the test sets to create one data set.                                  #
# 2. Extracts only the measurements on the mean and standard deviation for each measurement.        #
# 3. Uses descriptive activity names to name the activities in the data set.                        #
# 4. Appropriately labels the data set with descriptive variable names.                             #
#                                                                                                   #
# From the data set in step 4, creates a second, independent tidy data set with the average of      #
# each variable for each activity and each subject.                                                 #
#                                                                                                   #
#####################################################################################################

directory <- setwd("C:/Users/Francesco/OneDrive/Coursera/03 Getting and Cleaning Data/Week 3/Quiz and Assignment")

### 1. Merges the training and the test sets to create one data set.

# Download the file.
if(!file.exists("./data")){dir.create("./data")}
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl,destfile="./data/Dataset.zip") # Use without the curl option.

# Extraxt files (in the "exdir") from the zip compression (from "zipfile").
unzip(zipfile="./data/Dataset.zip",exdir="./data")

path_rf <- file.path("./data" , "UCI HAR Dataset") # Aggiorno la path dei dati
list.files(path_rf, recursive=TRUE)                # Vedo i files presenti nella path salvata.

# Open the datasets: there are two samples: test sample (2947 obs) and train sample (7352 obs).

# subject_train.txt: Each row identifies the subject who performed the activity for each window sample.
dataSubjectTrain <- read.table(file.path(path_rf, "train", "subject_train.txt"), header = FALSE)
dataSubjectTest  <- read.table(file.path(path_rf, "test" , "subject_test.txt"), header = FALSE)

str(dataSubjectTrain) # ID subjects that perform the acticity in the training sample
str(dataSubjectTest)  # ID subjects that perform the acticity in the test sample

# Training set: list of features for each activitie performed by the subject. 
dataFeaturesTest  <- read.table(file.path(path_rf, "test" , "X_test.txt" ), header = FALSE)
dataFeaturesTrain <- read.table(file.path(path_rf, "train", "X_train.txt"), header = FALSE)

str(dataFeaturesTest)   # ID features for each activity in the test sample
str(dataFeaturesTrain)  # ID features for each activity in the training sample

# Training label: lebels list of activities performed by the subject.
dataActivityTest  <- read.table(file.path(path_rf, "test" , "Y_test.txt" ), header = FALSE)
dataActivityTrain <- read.table(file.path(path_rf, "train", "Y_train.txt"), header = FALSE)

str(dataActivityTest)   # ID activity performed in test sample
str(dataActivityTrain)  # ID activity performed in training sample

# Append training and test (samples) datasets
dataSubject <- rbind(dataSubjectTrain, dataSubjectTest)
dataActivity<- rbind(dataActivityTrain, dataActivityTest)
dataFeatures<- rbind(dataFeaturesTrain, dataFeaturesTest)

# Assign label at variables in the datasets (both the data have only one variable).
names(dataSubject) <- c("subject")
names(dataActivity) <- c("activity")

# Dataset that includes the name of variables. The values are in the Furures data created.
dataFeaturesNames <- read.table(file.path(path_rf, "features.txt"), head=FALSE)

# Assign at each variable in the dataset Fetures the corrisponding name includes in FeaturesNames
names(dataFeatures)<- dataFeaturesNames$V2

# Merge the list of subjects that perform the activities and the activity performed by the subject,
# Add also the features linked to each activity performed by the subject.
# Data is the merged dataset.
dataCombine <- cbind(dataSubject, dataActivity)
Data <- cbind(dataFeatures, dataCombine)


### 2. Extracts only the measurements on the mean and standard deviation for each measurement.

# From the list of the feature names, we extract only the mean and standard deviation.
# We use the function grep. Among the variables in V2 of FeaturesNames, grep function
# extracts all the variables that contain the word "mean" or "std"
subdataFeaturesNames <- dataFeaturesNames$V2[grep("mean\\(\\)|std\\(\\)", dataFeaturesNames$V2)]

# List of variables of sub-dataset: list of variables with "mean" and "std", plus "subject" and "activity"
selectedNames <- c(as.character(subdataFeaturesNames), "subject", "activity")

# Select only the variables means and stds from the Data.
# We use subset function, which return subsets of vectors, matrices or data frames which meet conditions.
# Here the condiction is the name of variables, according to "selectedNames"
# Alternative: Data_meanstd <- Data[,grepl("mean\\(\\)|std\\(\\)|Subject|Id", names(Data))]

Data_meanstd <- subset(Data, select = selectedNames)
str(Data_meanstd) # Check that the dataset actually contains only mean and std values.


### 3. Use descriptive activity names to name the activities in the data set

# Open the dataset with the name activities (2 variables: code and label of activity)
activityLabels <- read.table(file.path(path_rf, "activity_labels.txt"), header = FALSE)
names(activityLabels) <- c("activity", "activity_name")

# Match the values of the activities with the corresponding labels.
Data_meanstd2 <- join(Data_meanstd, activityLabels, by = "activity", match = "first")
head(Data_meanstd$activity, 20)       # Check
head(Data_meanstd2$activity_name, 20)
Data_meanstd2 <- Data_meanstd2[,-1]   # Delete the first column of data, namely, the code of activities


### 4. Appropriately labels the data set with descriptive names.
names(Data_meanstd2)
names(Data_meanstd2) <- gsub("^t", "time", names(Data_meanstd2)) # Replace "t" with "time" in the name of variables.
names(Data_meanstd2) <- gsub("^f", "frequency", names(Data_meanstd2)) # Replace "f" with "frequency"
names(Data_meanstd2) <- gsub("Acc", "Accelerometer", names(Data_meanstd2))
names(Data_meanstd2) <- gsub("Gyro", "Gyroscope", names(Data_meanstd2))
names(Data_meanstd2) <- gsub("Mag", "Magnitude", names(Data_meanstd2))
names(Data_meanstd2) <- gsub("BodyBody", "Body", names(Data_meanstd2))

library(plyr)

### 5. From the data set in step 4, creates a second, independent tidy data set 
### with the average of each variable for each activity and each subject 

# Starting from the dataset of point 4, and by the variable "subject" and "activity",
# we compute the mean per colonna (numcolwise function)
Finaldata <- ddply(Data_meanstd2, c("subject","activity"), numcolwise(mean))
head(Finaldata, 20) # Check

# Improve column names
Finaldataheaders <- names (Finaldata) # Save the names of variables

# Function that add the suffix at variables
addSuffix <- function(x, suffix) {
  if (!(x %in% c("subject","activity"))) {
    paste(x, suffix, sep="")
  }
  else{
    x
  }
}

Finaldataheaders <- sapply(Finaldataheaders, addSuffix, ".mean") # Add suffix at variable names
names(Finaldata) <- Finaldataheaders # Assign improved name at each column

# Generate tiny data
write.table(Finaldata, file = "tidydata.txt", row.name=FALSE)
