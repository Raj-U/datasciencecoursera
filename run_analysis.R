
# getting the current working directory and then setting the working directory
# to the appropriate working directory
getwd()
setwd("C:/Raj - Personal/CourseEra/Course3week4/")
getwd()


# In the code below we are reading the variable names for the features.txt file, 
# which will be assigned later for the training and text data set below
features <- read.csv('./Assignment/UCI HAR Dataset/features.txt', header = FALSE, sep = ' ')
features <- as.character(features[,2])

# In the following set of code below we will read the training set data from the 
# respective directory

# read the actual values for the 561 variables in the training set data i.e, x_train.txt
x_train_data <- read.table('./Assignment/UCI HAR Dataset/train/X_train.txt')

# read the activity train data from y_train.txt, which contains the activity numbers
activity_train_data <- read.csv('./Assignment/UCI HAR Dataset/train/y_train.txt', header = FALSE, sep = ' ')

# read the subject id of the thirty participants who have volunteered for this event
subject_train_data <- read.csv('./Assignment/UCI HAR Dataset/train/subject_train.txt',header = FALSE, sep = ' ')

# Merge all the above three data sets to have corresponding subject id along with
# activities and their values from the 561 variables and combine the result set
train_data <-  data.frame(subject_train_data, activity_train_data, x_train_data)

# Here we give the appropriate labels for all the 563 variables in the result set
names(train_data) <- c(c('subject', 'activity'), features)

# In the following set of code below we will read the test data set data from the 
# respective directory

# read the actual values for the 561 variables in the test set data i.e, x_train.txt
x_test_data <- read.table('./Assignment/UCI HAR Dataset/test/X_test.txt')

# read the activity train data from y_test.txt, which contains the activity numbers
activity_test_data <- read.csv('./Assignment/UCI HAR Dataset/test/y_test.txt', header = FALSE, sep = ' ')

# read the subject id of the thirty participants who have volunteered for this event
subject_test_data <- read.csv('./Assignment/UCI HAR Dataset/test/subject_test.txt', header = FALSE, sep = ' ')

# Merge all the above three data sets to have corresponding subject id along with
# activities and their values from the 561 variables and combine the result set
test_data <-  data.frame(subject_test_data, activity_test_data, x_test_data)

# Here we give the appropriate labels for all the 563 variables in the result set
names(test_data) <- c(c('subject', 'activity'), features)

# Merges the training and the test sets to create one data set.
all_data <- rbind(train_data, test_data)

#Extracts only the measurements on the mean and standard deviation for each measurement.
meanstd_data <- grep('mean|std', features)
data_subset <- all_data[,c(1,2,meanstd_data + 2)]

#Uses descriptive activity names to name the activities in the data set
activity_labels <- read.table('./Assignment/UCI HAR Dataset/activity_labels.txt', header = FALSE)
activity_labels <- as.character(activity_labels[,2])
data_subset$activity <- activity_labels[data_subset$activity]


#Appropriately labels the data set with descriptive variable names.

new_name<-names(data_subset)
new_name <- gsub("[(][)]", "", new_name)
new_name <- gsub("^t", "TimeDomain_", new_name)
new_name <- gsub("^f", "FrequencyDomain_", new_name)
new_name <- gsub("Acc", "Accelerometer", new_name)
new_name <- gsub("Gyro", "Gyroscope", new_name)
new_name <- gsub("Mag", "Magnitude", new_name)
new_name <- gsub("-std-", "_StandardDeviation_", new_name)
new_name <- gsub("-", "_", new_name)
names(data_subset) <- new_name

#From the data set in step 4, creates a second, independent tidy data set with 
#the average of each variable for each activity and each subject.

tidy_data <- aggregate(data_subset[,3:81], by = list(activity = data_subset$activity, subject = data_subset$subject),FUN = mean)
write.table(x = tidy_data, file = "tidy_data.txt", row.names = FALSE, sep="\t")
str(tidy_data)
write.table(tidy_data, file="./Assignment/UCI HAR Dataset/tidy_data.txt")

