datacleanproject
================
#part1 Merges the training and the test sets to create one data set. 
#Read y_train.txt , x_train.txt , y_test.txt , x_test.txt ,subject_train.txt,subject_test.txt table from download file
ty <- read.table("C:/backup 201311/cece/DATA/GETING AND  CLEANING DATA/project/getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/train/y_train.txt")
tx <- read.table("C:/backup 201311/cece/DATA/GETING AND  CLEANING DATA/project/getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/train/x_train.txt")
tey <-read.table("C:/backup 201311/cece/DATA/GETING AND  CLEANING DATA/project/getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/test/y_test.txt")
tex <-read.table("C:/backup 201311/cece/DATA/GETING AND  CLEANING DATA/project/getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/test/x_test.txt")
ts <- read.table("C:/backup 201311/cece/DATA/GETING AND  CLEANING DATA/project/getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/train/subject_train.txt")
tes <- read.table("C:/backup 201311/cece/DATA/GETING AND  CLEANING DATA/project/getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/test/subject_test.txt")
#combind the testing set
texy <- cbind(tex,tey,tes)
#combind the trainning set
txy <- cbind(tx,ty,ts)
#combind testing and trainning set
comb <- rbind(texy,txy)
#read features.txt
fe <-read.table("C:/backup 201311/cece/DATA/GETING AND  CLEANING DATA/project/getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/features.txt")
#extract colnames from features.txt
fev <- as.vector(fe$V2)
#add the y value and subject value to colnames
fev[562] <- "y"
fev[563] <- "No."
#add names for combind file
names(comb)<-fev
#part2 Extracts only the measurements on the mean and standard deviation for each measurement.
#use grepl command to extract mean and std measurment 
ex <- comb[,grepl("mean",names(comb),ignore.case=TRUE)|grepl("std",names(comb),ignore.case=TRUE)]
#add y and subject value to the fist new dataset
ex$y <- comb$y
ex$No. <- comb$No.
#part3 Uses descriptive activity names to name the activities in the data set
#add descriptive activity names to name the activities in the data set
comb$y<-sapply(comb$y,function(elt){
  state<-c('WALKING','WALKING_UPSTAIRS','WALKING_DOWNSTAIRS','SITTING','STANDING','LAYIN')
  return(state[elt])})
ex$y <- comb$y
#part4 Appropriately labels the data set with descriptive variable names. 
#labels the data set with descriptive variable names
names(ex)<- gsub("[aA]cc","Acceleration",names(ex))
#part 5 Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 
#calculate the average of each variable for each activity and each subject by split and sapply function
exn <- split(ex,list(ex$No.,ex$y))
X <- sapply( exn, function(x) mean(x[,c(-87,-88)]), simplify = TRUE )
OK <- t(X)
#write file to local file
write.csv(ex, file = "ex.txt", row.names = TRUE)
write.csv(OK, file = "OK.txt", row.names = TRUE)
