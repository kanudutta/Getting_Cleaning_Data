# Read the CodeBook.md and README.md for details

library(reshape2)

# read train and test data
X_train1 <- read.csv("./UCI HAR Dataset/train/X_train.txt",header=FALSE,sep="")
y_train1 <- read.csv("./UCI HAR Dataset/train/y_train.txt",header=FALSE,sep="")
X_test1 <- read.csv("./UCI HAR Dataset/test/X_test.txt",header=FALSE,sep="")
y_test1 <- read.csv("./UCI HAR Dataset/test/y_test.txt",header=FALSE,sep="")

# reading features and activity labels

activity_labels <- read.csv("./UCI HAR Dataset/activity_labels.txt",sep="",header=FALSE)
features <- read.csv("./UCI HAR Dataset/features.txt", sep="", header=FALSE)

# reading subject data
subject_train1 <- read.csv("./UCI HAR Dataset/train/subject_train.txt",header=FALSE,sep="")
subject_test1 <- read.csv("./UCI HAR Dataset/test/subject_test.txt",header=FALSE,sep="")

# mapping the coloumn names of training and test sets with features
names(X_train1) <- features[,2]
names(X_test1) = features[,2]

#Labelling the columns
names(y_train1) <- c("Activity_ID")
names(y_test1) <- c("Activity_ID")
names(subject_train1) <- c("subject")
names(subject_test1) <- c("subject")
names(activity_labels) <- c("Activity_ID", "Activity_Label")

# search mean and standard deviation for each measurement and return the index value.
final_features <- which(grepl("mean|std", features[,2]))

#subsetting the measurements on the mean and standard deviation

X_train1 <- X_train1[,final_features]
X_test1 <- X_test1[,final_features]


# mapping activity labels

y_train2 <- merge(y_train1,activity_labels,by="Activity_ID",sort=FALSE) 
y_test2 <-  merge(y_test1,activity_labels,by="Activity_ID",sort=FALSE) 


# train and test data merging
train <- cbind(subject_train1, y_train2, X_train1)
test <- cbind(subject_test1, y_test2, X_test1)
cdata <- rbind(train, test)

#Reshaping dataset into long format
md2 <- md2 <- data.frame(matrix(ncol=5,nrow=0))
colnames(md2) <- c("subject", "Activity_ID", "Activity_Label","variable","value") 
for (i in 4:length(cdata)) {
mvar <- colnames(cdata[i])
md1 <- melt(cdata, id = c("subject", "Activity_ID", "Activity_Label"), measure.vars = mvar)
md2 <- rbind(md2,md1)
}

# applying mean function to all measured variables, with subject and activity label as id variable  
finalData <- dcast(md2, subject + Activity_Label ~ variable, mean)

# creating tidy data set
write.table(finalData, file = "Tidy.Data.txt")