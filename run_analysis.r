# Program

 # It is considered the required files are downloaded and extracted under the folder 
 # named "UCI HAR Dataset" in the working directory

##################################################################### 
# setting project path
uciDataSetPath <- file.path(getwd(), "UCI HAR Dataset")

##################################################################### 
#Reading - files.
subjectTraining <- fread(file.path(uciDataSetPath, "train", "subject_train.txt"))
subjectTesting <- fread(file.path(uciDataSetPath, "test", "subject_test.txt"))

dataTraining <- data.table(read.table(file.path(uciDataSetPath, "train", "X_train.txt")))
dataTesting <- data.table(read.table(file.path(uciDataSetPath, "test", "X_test.txt")))

dataActivityTraining <- fread(file.path(uciDataSetPath, "train", "Y_train.txt"))
dataActivityTesting <- fread(file.path(uciDataSetPath, "test", "Y_test.txt"))

##################################################################### 
# Merges the training and the test sets to create one data set.
# Concatenate the data tables. 
# Merge columns. 
# Set key.

dataSubject <- rbind(subjectTraining, subjectTesting)
setnames(dataSubject, "V1", "subject")
dataActivity <- rbind(dataActivityTraining, dataActivityTesting)
setnames(dataActivity, "V1", "activityNum")
dt <- rbind(dataTraining, dataTesting)
dataSubject <- cbind(dataSubject, dataActivity)
dt <- cbind(dataSubject, dt)

setkey(dt, subject, activityNum)

#####################################################################
# Extracts only the measurements on the mean and standard deviation for each measurement.
# Seperating variables in dt are measurements for the mean and standard deviation
# Subset only measurements for the mean and standard deviation.
# Convert the column numbers to a vector of variable names matching columns in dt

dtFeatures <- fread(file.path(uciDataSetPath, "features.txt"))
setnames(dtFeatures, names(dtFeatures), c("featureNum", "featureName"))
dtFeatures <- dtFeatures[grepl("mean\\(\\)|std\\(\\)", featureName)]
dtFeatures$featureCode <- dtFeatures[, paste0("V", featureNum)]
head(dtFeatures)
dtFeatures$featureCode

# Subset these variables using variable names
select <- c(key(dt), dtFeatures$featureCode)
dt <- dt[, select, with = FALSE]

#####################################################################
#Uses descriptive activity names to name the activities in the data set
#Reading activity_labels.txt file - later - used to add descriptive names to the activities.
dtActivityNames <- fread(file.path(uciDataSetPath, "activity_labels.txt"))
setnames(dtActivityNames, names(dtActivityNames), c("activityNum", "activityName"))

#####################################################################
#Appropriately labels the data set with descriptive variable names.

# Merge activity labels.
# Add activityName as a key.
# Melt the data table to reshape it from a short and wide format to a tall and narrow format.
# Merge activity name.

dt <- merge(dt, dtActivityNames, by = "activityNum", all.x = TRUE)
setkey(dt, subject, activityNum, activityName)
dt <- data.table(melt(dt, key(dt), variable.name = "featureCode"))
dt <- merge(dt, dtFeatures[, list(featureNum, featureCode, featureName)], by = "featureCode", all.x = TRUE)

# Creating factor class
dt$activity <- factor(dt$activityName)
dt$feature <- factor(dt$featureName)

#Seperate features from featureName
grepthis <- function(regex) {
    grepl(regex, dt$feature)
}
## Features with 2 categories
n <- 2
y <- matrix(seq(1, n), nrow = n)
x <- matrix(c(grepthis("^t"), grepthis("^f")), ncol = nrow(y))
dt$featDomain <- factor(x %*% y, labels = c("Time", "Freq"))
x <- matrix(c(grepthis("Acc"), grepthis("Gyro")), ncol = nrow(y))
dt$featInstrument <- factor(x %*% y, labels = c("Accelerometer", "Gyroscope"))
x <- matrix(c(grepthis("BodyAcc"), grepthis("GravityAcc")), ncol = nrow(y))
dt$featAcceleration <- factor(x %*% y, labels = c(NA, "Body", "Gravity"))
x <- matrix(c(grepthis("mean()"), grepthis("std()")), ncol = nrow(y))
dt$featVariable <- factor(x %*% y, labels = c("Mean", "SD"))
## Features with 1 category
dt$featJerk <- factor(grepthis("Jerk"), labels = c(NA, "Jerk"))
dt$featMagnitude <- factor(grepthis("Mag"), labels = c(NA, "Magnitude"))
## Features with 3 categories
n <- 3
y <- matrix(seq(1, n), nrow = n)
x <- matrix(c(grepthis("-X"), grepthis("-Y"), grepthis("-Z")), ncol = nrow(y))
dt$featAxis <- factor(x %*% y, labels = c(NA, "X", "Y", "Z"))

# Possible combinations feature Vs factor class variables
r1 <- nrow(dt[, .N, by = c("feature")])
r2 <- nrow(dt[, .N, by = c("featDomain", "featAcceleration", "featInstrument", 
    "featJerk", "featMagnitude", "featVariable", "featAxis")])
r1 == r2

#####################################################################
# From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
setkey(dt, subject, activity, featDomain, featAcceleration, featInstrument, featJerk, featMagnitude, featVariable, featAxis)
dtTidy <- dt[, list(count = .N, average = mean(value)), by = key(dt)]
write.table(dtTidy, "tidy.txt")

# Creating codebook
knit("run_analysis.r", output = "run_analysis.md", encoding = "ISO8859-1", quiet = TRUE)
markdownToHTML("run_analysis.md", "run_analysis.html")
