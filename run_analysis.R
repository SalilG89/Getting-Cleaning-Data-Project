
library(plyr)

download.data = function() {
  
  
  ##Function Checks to see if directory or file exists, and if not creates directory, downloads data
  
  if (!file.exists("UCIHARdata")) {
    dir.create("UCIHARdata")
                                  }
  if (!file.exists("UCIHARdata/UCI HAR Dataset")) {
    
    ZipURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
    FileLoc ="UCIHARdata/UCI_HAR_data.zip"
    
    download.file(ZipURL, destfile = FileLoc)
    unzip(FileLoc, exdir="UCIHARdata")
                                            }
                           }
## Run Function
download.data()

##Format flat files to R Data frames



featuresDF <- read.csv('./UCIHARdata/UCI HAR Dataset/features.txt', sep = ' ', header = FALSE)
featuresDF <- as.character(featuresDF[,2])

trainDF.x <- read.table('./UCIHARdata/UCI HAR Dataset/train/X_train.txt')
trainDF.activity <- read.csv('./UCIHARdata/UCI HAR Dataset/train/y_train.txt', sep = ' ', header = FALSE)
trainDF.subject <- read.csv('./UCIHARdata/UCI HAR Dataset/train/subject_train.txt', sep = ' ', header = FALSE)

trainDF <-  data.frame(trainDF.subject, trainDF.activity , trainDF.x)
names(trainDF) <- c(c('subject', 'activity'), featuresDF)

testDF.x <- read.table('./UCIHARdata/UCI HAR Dataset/test/X_test.txt')
testDF.activity <- read.csv('./UCIHARdata/UCI HAR Dataset/test/y_test.txt', sep = ' ', header = FALSE)
testDF.subject <- read.csv('./UCIHARdata/UCI HAR Dataset/test/subject_test.txt', sep = ' ', header = FALSE)

testDF <-  data.frame(testDF.subject, testDF.activity, testDF.x)
names(testDF) <- c(c('subject', 'activity'), featuresDF)

##Merge the dataset
traintestDF  <- rbind(trainDF, testDF)

##Extract only the measurements we want


col.select <- grep('mean|std', featuresDF)
selectData <- traintestDF[,c(1,2,col.select + 2)]





activity_labels <- read.table('./UCIHARdata/UCI HAR Dataset/activity_labels.txt', header = FALSE)
activity_labels <- as.character(activity_labels[,2])G
selectData$activity <- activity_labels[selectData$activity]


##Appropriately label the dataset with variable names


DescriptiveNames <- names(selectData)
DescriptiveNames <- gsub("[(][)]", "", DescriptiveNames)
DescriptiveNames <- gsub("^t", "Time_", DescriptiveNames)
DescriptiveNames <- gsub("^f", "Frequency_", DescriptiveNames)
DescriptiveNames <- gsub("Acc", "Accelerometer", DescriptiveNames)
DescriptiveNames <- gsub("Gyro", "Gyroscope_", DescriptiveNames)
DescriptiveNames <- gsub("Mag", "Magnitude_", DescriptiveNames)
DescriptiveNames <- gsub("-mean-", "Mean_", DescriptiveNames)
DescriptiveNames <- gsub("-std-", "StandardDeviation_", DescriptiveNames)
DescriptiveNames <- gsub("-", "_", DescriptiveNames)
names(selectData) <- DescriptiveNames

##Creates table with means by SUbject

tidydf <- aggregate(selectData[,3:81], by = list(activity = selectData$activity, subject = selectData$subject),FUN = mean)
write.table(x = tidydf, file = "UCIHAR_data_tidy.txt", row.names = FALSE)



