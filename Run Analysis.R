
# set directory 
dir <- "YOUR DIRECTORY NAME"
setwd(dir)
#######################################################################
#create the inverse map of a list - Thanks StackOverFlow!!
invMap <- function(map) {
  items <- as.character( unlist(map) )
  nams <- unlist(Map(rep, names(map), sapply(map, length)))
  names(nams) <- items
  nams
}
#######################################################################
# readin test
train <- read.table ("train\\X_train.txt")

y.train <- read.table ("train\\y_train.txt")

sub.train <- read.table ("train\\subject_train.txt")

# readin Training
test <- read.table ("test\\X_test.txt")

y.test <- read.table ("test\\y_test.txt")

sub.test <- read.table ("test\\subject_test.txt")

##################################################################
#1) Merges the training and the test sets to create one data set.#
##################################################################

#bind train and test
HAR <- rbind(train, test)
rm(train, test)

# bind train and test activity 
act <- rbind(y.train, y.test)
rm(y.train, y.test)

# bind train and test for subject
sub <- rbind(sub.train, sub.test)
rm(sub.train, sub.test)

############################################################################################
#2) Extracts only the measurements on the mean and standard deviation for each measurement.#
############################################################################################
# readin features
features <- read.table("features.txt")
#isolate mean and std
fts <- grep("-mean\\(\\)|-std\\(\\)", features[, 2])
HAR <- HAR[, fts]
fts2<- features[fts, 2]
#get rid strings that will messup analysis  
fts2 <- gsub("-", "_", fts2); fts2 <- gsub("\\(|\\)|,", "",  fts2)
fts2 <- tolower(fts2) ; rm (features, fts)

# put features in as colnames
mylist <- paste(fts2, sep = " " )
names(HAR) <- c(mylist) 

# output as first data set
write.table(HAR, "HAR_mean_std.txt") ; rm(mylist , fts2)

##############################################################################
# Appropriately labels the data set with descriptive activity names. 
##############################################################################
Mapit <- list(Walking = 1, Walking_Upstairs =2 , Walking_Downstairs=3,
              Sitting=4, Standing=5, Laying=6)

invMap(Mapit) ; act <- transform(act, Activity = invMap(Mapit)[V1])

label <- act[2]  ; rm(act)

#bind back the subject and then the activity to HAR
HAR2 <- cbind(sub, label, HAR) ; rm(sub,label, HAR)

# rename the subjcet column
#install.packages("reshape")
library(reshape)
HAR2 <- rename(HAR2, c(V1="Subject_Name"))

############################################################################
#Creates a second, independent tidy data set with the average of
# each variable for each activity and each subject
############################################################################
#create id for for both of these to average by
HAR2$catid <- as.factor(paste(HAR2$Subject_Name, HAR2$Activity, sep="-"))

#aggregate mean
ag.HAR2<- aggregate(HAR2[,3:68],by=list(HAR2$catid), mean)

#split back into Sub ID and Acitivty
split <- data.frame(Subject.Name=substr(ag.HAR2$Group.1, 1,1), 
           Activity=(substr(ag.HAR2$Group.1,3,20))) ; 

# put back in and drop combo id
out.HAR2 <- cbind(split, ag.HAR2)  ; out.HAR2 <- out.HAR2[, -3]
rm(HAR2, ag.HAR2, split)

#output as text file - the end
write.table(HAR, "Tidy_dataset.txt") 

