#Cheks whether the package 'data.table' is already installed
packages <- installed.packages()

package_names <- packages[,1]
if(sum(package_names == c("data.table")) == 0) {
      install.packages("data.table")
}

suppressMessages(library(data.table))

url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
file <- "downloaded_data.zip"
download.file(url, file)
unzip(file, exdir = getwd())

#Gets working directory and set appropriate file paths
filePath <- getwd()
filePath <- paste(filePath, "/UCI HAR Dataset", sep = "")
testPath <- paste(filePath, "/test", sep = "")
trainPath <- paste(filePath, "/train", sep = "")

##DATA PREPARATION

#Extracts features names and creates "X_test" and "X_train" datasets
features <- read.table(paste(filePath, "/features.txt", sep = ""))
X_test <- read.table(paste(testPath, "/X_test.txt", sep = ""), col.names = features[,2])
X_train <- read.table(paste(trainPath, "/X_train.txt", sep = ""), col.names = features[,2])


#1. Merges the training and the test sets to create one data set
X <- rbind(X_test, X_train)

#2. Extracts only the measurements on the mean and standard deviation (string "-mean()" or "-std()") for each measurement
mean_std_rows <- features[grep("(-mean\\()|(-std\\()", features[,2]),]
X_mean_std <- X[,mean_std_rows[,1]]


#Merges "y_test" and "y_train"
y_test <- read.table(paste(testPath, "/y_test.txt", sep = ""))
y_train <- read.table(paste(trainPath, "/y_train.txt", sep = ""))
y <- rbind(y_test, y_train)
subject_test <- read.table(paste(testPath, "/subject_test.txt", sep = ""))
subject_train <- read.table(paste(trainPath, "/subject_train.txt", sep = ""))
subject_bind <- rbind(subject_test, subject_train)

#3. Uses descriptive activity names to name the activities in the data set
y <- as.factor(y$V1)
y <- factor(y, levels = c(1,2,3,4,5,6), labels = c("WALKING", "WALKING_UPSTAIRS", "WALKING_DOWNSTAIRS", "SITTING", "STANDING", "LAYING"))

#4. Appropriately labels the data set with descriptive variable names
X_mean_std_labeled <- cbind(X_mean_std, y)
X_mean_std_labeled <- cbind(X_mean_std_labeled, subject_bind)
colnames(X_mean_std_labeled)[68] <- c("ssubject_id")
colnames(X_mean_std_labeled)[67] <- c("aactivity")
colnames(X_mean_std_labeled) <- substr(colnames(X_mean_std_labeled),2,50)
colnames(X_mean_std_labeled) <- gsub("Acc", "Acceleration", colnames(X_mean_std_labeled))
colnames(X_mean_std_labeled) <- gsub("Gyro", "Gyroscope", colnames(X_mean_std_labeled))
colnames(X_mean_std_labeled) <- gsub("Mag", "Magnitude", colnames(X_mean_std_labeled))
colnames(X_mean_std_labeled) <- gsub("BodyBody", "Body", colnames(X_mean_std_labeled))

#5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject
tidy_data <- data.table(X_mean_std_labeled)
tidy_data$subject_id <- as.factor(tidy_data$subject_id)

#Computes mean by subject id and activity
tidy_aggregated <- suppressWarnings(aggregate(tidy_data, by = list(tidy_data$subject_id, tidy_data$activity), mean))
colnames(tidy_aggregated)[1:2] <- c("subject_id", "activity")

#Removes unecessary columns and makes column names more readable
tidy_aggregated <- tidy_aggregated[,-c(69:70)]
colnames(tidy_aggregated) <- gsub("1", "", colnames(tidy_aggregated))
colnames(tidy_aggregated) <- gsub("\\.","",colnames(tidy_aggregated))
colnames(tidy_aggregated) <- gsub("1", "", colnames(tidy_aggregated))
colnames(tidy_aggregated) <- gsub("\\.", "", colnames(tidy_aggregated))
colnames(tidy_aggregated) <- gsub("mean", "Mean", colnames(tidy_aggregated))
colnames(tidy_aggregated) <- gsub("std", "Std", colnames(tidy_aggregated))


#Writes the result to a .txt file in the working directory
write.table(tidy_aggregated, file = "tidy.txt", row.names = FALSE)