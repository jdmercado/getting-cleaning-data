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