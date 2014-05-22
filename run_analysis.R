# 1. Merge the training and the test sets to create one data set.

tmp1 <- read.table("train/X_train.txt")
tmp2 <- read.table("test/X_test.txt")
Xdata <- rbind(tmp1, tmp2)

tmp1 <- read.table("train/subject_train.txt")
tmp2 <- read.table("test/subject_test.txt")
Sdata <- rbind(tmp1, tmp2)

tmp1 <- read.table("train/y_train.txt")
tmp2 <- read.table("test/y_test.txt")
Ydata <- rbind(tmp1, tmp2)

# 2. Extract only the measurements on the mean and standard deviation for each measurement.

features <- read.table("features.txt")
indices_of_good_features <- grep("-mean\\(\\)|-std\\(\\)", features[, 2])
Xdata <- Xdata[, indices_of_good_features]
names(Xdata) <- features[indices_of_good_features, 2]
names(Xdata) <- gsub("\\(|\\)", "", names(Xdata))
names(Xdata) <- tolower(names(Xdata))

# 3. Use descriptive activity names to name the activities in the data set

activities <- read.table("activity_labels.txt")
activities[, 2] = gsub("_", "", tolower(as.character(activities[, 2])))
Ydata[,1] = activities[Ydata[,1], 2]
names(Ydata) <- "activity"

# 4. Appropriately labels the data set with descriptive activity names.

names(Sdata) <- "subject"
cleaned <- cbind(Sdata, Ydata, Xdata)
write.table(cleaned, "merged_clean_data.txt")

# 5. Create a 2nd, independent tidy data set with the average of each variable for each activity and each subject.

uniqueSubjects = unique(Sdata)[,1]
numSubjects = length(unique(Sdata)[,1])
numActivities = length(activities[,1])
numCols = dim(cleaned)[2]
result = cleaned[1:(numSubjects*numActivities), ]

row = 1
for (n in 1:numSubjects) {
	for (a in 1:numActivities) {
		result[row, 1] = uniqueSubjects[n]
		result[row, 2] = activities[a, 2]
		tmp <- cleaned[cleaned$subject==n & cleaned$activity==activities[a, 2], ]
		result[row, 3:numCols] <- colMeans(tmp[, 3:numCols])
		row = row+1
	}
}
write.table(result, "data_set_with_the_averages.txt")