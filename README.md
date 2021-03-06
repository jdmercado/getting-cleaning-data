Getting and Cleaning Data Course Project
=====================

This is a guide to understand the project solution for the project in Getting and Cleaning Data course. The raw data for this project must be downloaded from "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip", which contains data collected from the accelerometers from the Samsung Galaxy S smartphone. The credits for the data collectors are in this website:

http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones 

Steps to solve the project assignment:
=====================

* Go to working directory and create a directory called "datProj".
* Download zipped file from "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip".
* Unzip this file directly into directory datProj (deleting from proposed path default subdirectory with the same name of the zipped file).
* Run the script given, run_analysis.R, which will take the unzipped raw data focusing on means and standard deviations, process data, and produce a summarized tidy data set according to the specifications given (mean of each measurement by activity and subject).
* Write text file with the tidy data set just described.

Code provided
=====================

```
# run_analysis -- Based on data downloaded from a Human Activity Recognition 
#   data set using smartphones (see readme for credit information), the 
#   script merges some tables, extracts variables related to mean and
#   standard deviation of different measurements, reshapes them, and 
#   generates a tidy data set with the averages of the variables extracted. 
#   This data set is written to disk.
#
run_analysis <- {
    setwd("~")
# Data should have already been downloaded to a directory datProj
    dirf <- "datProj"
    if (!file.exists(dirf)) {
        stop("First download and unzip data to \"~/datProj\"")
    }
    setwd("datProj")
    options(stringsAsFactors = FALSE)  # disable stringsAsFactors as TRUE
# Load general data
    ftr <- read.table("./UCI HAR Dataset/features.txt")
    activ <- read.table("./UCI HAR Dataset/activity_labels.txt")
# Load test data
    tsSub <- read.table("./UCI HAR Dataset/test/subject_test.txt")
    tsX <- read.table("./UCI HAR Dataset/test/X_test.txt",colClasses="numeric")
    tsY <- read.table("./UCI HAR Dataset/test/Y_test.txt")
# Load train data
    trSub <- read.table("./UCI HAR Dataset/train/subject_train.txt")
    trX <- read.table("./UCI HAR Dataset/train/X_train.txt",colClasses="numeric")
    trY <- read.table("./UCI HAR Dataset/train/Y_train.txt")
# Put together training data set
    tr <- data.frame(trSub[], trY[], trX[,])
    names(tr) <- c("Subject","idActiv",ftr[,2]) # assigns column names
# Put together test data set
    ts <- data.frame(tsSub[], tsY[], tsX[,])
    names(ts) <- c("Subject","idActiv",ftr[,2]) # assigns column names
# Free space from data not needed anymore
    rm(trSub)
    rm(trY)
    rm(trX)
    rm(tsSub)
    rm(tsY)
    rm(tsX)
# Create one data set from training and test data sets
    dat <- rbind(tr, ts)
# Select features for mean() and standard deviation or std()
    var <- ftr$V2[grep("mean\\(\\)|std\\(\\)",ftr$V2)]
# Extract new data frame only with those measurements
    df <- dat[,c("Subject","idActiv",var[])]  # include also subject and activity
# Free space not needed
    rm(dat)
    rm(tr)
    rm(ts)
# Merge data frame with activity labels to get descriptive activity names
    dfm <- merge(activ, df[order(df$idActiv),], by.x="V1", by.y="idActiv")
    dfm <- dfm[,2:69]
    rm(df)   #  Free space of data not needed
# Improve variable names to make them more descriptive
    var <- gsub('^t','Time',var)
    var <- gsub('^f','Freq',var)
    var <- gsub('([[:upper:]])', ' \\1', var)  # split on uppercase letters
    names(dfm) <- c("Activity","Subject",var[]) # replace column names
# Reshape data to create a second data set with averages of variables
# by activity and subject
    library(reshape2)
    dfMelt <- melt(dfm, id=c("Activity","Subject"), measure.vars=var[])
    dfMean <- dcast(dfMelt, Activity + Subject ~ variable, mean)
# Write text file with tidy data set of summary found
    write.table(dfMean, file="tidyDat2.txt", row.names=FALSE)
}
```

Contributor
=====================
Jose D. Mercado
