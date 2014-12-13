# 1. Import and merge the training and test sets --------------------------

X_train <- read.table("./train/X_train.txt")
y_train <- read.table("./train/y_train.txt")
subject_train <- read.table("./train/subject_train.txt")
X_test <- read.table("./test/X_test.txt")
y_test <- read.table("./test/y_test.txt")
subject_test <- read.table("./test/subject_test.txt")

X <- rbind(X_train, X_test)
y <- rbind(y_train, y_test)
subject <- rbind(subject_train, subject_test)
rm(X_train, X_test, y_train, y_test, subject_train, subject_test)

# Label matrix X
features <- read.table("features.txt")
names(X) <- features[,2]

# 2. Extracts measurements on the mean and standard deviation -------------

relevant_columns <- grep(pattern = "-(mean|meanFreq|std)\\(\\)", x = names(X))
X <- X[, relevant_columns]

# 3. Uses descriptive activity names to name the activities ---------------

y[,] <- factor(y[,])

activities <- read.table("activity_labels.txt")
activities[,2] <- tolower(activities[,2])
activities[,2] <- sub(pattern = "_", replacement = " ", activities[,2])

library(plyr)
y <- mapvalues(y[,], from = (as.character(1:6)), to = as.character(activities[,2]))

# 4. Label the data set with descriptive variable names -------------------

names(X) <- gsub(pattern = "\\(\\)", replacement = "", x = names(X))
names(X) <- gsub("BodyBody", "Body", names(X))
names(X) <- gsub("^t", "time", names(X))
names(X) <- gsub("^f", "freq", names(X))

# 5. Create a tidy data set summarizing the averages ----------------------

X <- cbind(X, y, subject)
rm(features, relevant_columns, activities, y, subject)

X <- rename(x = X, replace = c("y" = "activity", "V1" = "subject"))
X$subject <- as.factor(X$subject)

library(reshape2)
Z <- melt(data = X, id = c("subject", "activity"), measure.vars = 1:79)
Z <- dcast(data = Z, subject + activity ~ variable, mean)

write.table(Z, "data_summary.txt", row.name=FALSE)
