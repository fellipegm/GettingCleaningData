rm(list=ls())
# Set working directory
setwd('/media/fellipegm/Data/Drive/07 Projetos/DataScience_JohnsHopkins/3.GetAndCleaning/Week4/Assignment/')


# Download the dataset
if (!file.exists("data.zip")) {
        download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip",
                      destfile = "data.zip")
}

# Unzip the dataset
if (!dir.exists("./UCI HAR Dataset/")){
        unzip(zipfile = "./data.zip")
}

# Load the datasets
featNames <- read.table("./UCI HAR Dataset/features.txt")
featNames <- gsub(pattern = "[-,]", replacement = "_", x = featNames[,2])
featNames <- gsub(pattern = "[\\(\\)]", replacement = "", x = featNames)

xtrain <- read.table("./UCI HAR Dataset/train/X_train.txt", 
                   header = FALSE,
                   col.names = featNames, 
                   stringsAsFactors = FALSE)
ytrain <- read.table("./UCI HAR Dataset/train/y_train.txt", 
                     header = FALSE, 
                     col.names = "activity",
                     stringsAsFactors = FALSE)
indvtrain <- read.table("./UCI HAR Dataset/train/subject_train.txt",
                        header = FALSE,
                        col.names = "subject",
                        stringsAsFactors = FALSE)

xtest <- read.table("./UCI HAR Dataset/test/X_test.txt", 
                     header = FALSE,
                     col.names = featNames, 
                     stringsAsFactors = FALSE)
ytest <- read.table("./UCI HAR Dataset/test/y_test.txt", 
                     header = FALSE, 
                     col.names = "activity",
                     stringsAsFactors = FALSE)
indvtest <- read.table("./UCI HAR Dataset/test/subject_test.txt",
                        header = FALSE,
                        col.names = "subject",
                        stringsAsFactors = FALSE)


# 1. Merges the training and the test sets to create one data set.
library(dplyr)

train <- cbind(indvtrain, ytrain, xtrain)
test <- cbind(indvtest, ytest, xtest)
df <- rbind(train, test)
dt <- tibble::as_tibble(df)
rm(list = c("df", "train", "test", "xtrain", "ytrain", "indvtrain", "xtest", "ytest", "indvtest"))

# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
# The mean and standard deviation signals end with "mean" and "std", respectively
usedNames <- grep(pattern = "(mean)|(std)|(subject)|(activity)", names(dt), value = TRUE)
dt <- dt %>%
        select(usedNames)


# 3. Uses descriptive activity names to name the activities in the data set
activities <- c("WALKING", 
                "WALKING_UPSTAIRS", 
                "WALKING_DOWNSTAIRS",
                "SITTING",
                "STANDING",
                "LAYING")

dt$activity <- sapply(dt$activity, function(x) activities[x])
dt$activity <- as.factor(dt$activity)


# 4. Appropriately labels the data set with descriptive variable names. 
# The labels are good enough, from Loading step

#5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
library(data.table)
fdt <- dt %>%
        group_by(subject, activity) %>%
        summarize_all(mean) %>%
        copy
fdt

new_names <- sapply(names(fdt)[3:length(names(fdt))], function(x) paste(x, "_smr", sep=""))
new_names <- c(names(fdt)[1], names(fdt)[2], new_names)
names(fdt) <- new_names
names(fdt)
