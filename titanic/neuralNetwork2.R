library(tidyverse) # metapackage with lots of helpful functions
library(Metrics)
library(dplyr)

train<-read.csv('train.csv',stringsAsFactors=FALSE)
test<-read.csv('test.csv',stringsAsFactors=FALSE)

test$Survived <- NA
all <- rbind(train, test)

colSums(is.na(all))

extractFeatures <- function(data, istrain=TRUE) {
  features <- c("Pclass",
                "Sex",
                "Age",
                "SibSp",
                "Parch",
                "Fare",
                "Embarked")
  fea <- data[,features]
  fea$Embarked[fea$Embarked==""] = "S"
  str(fea)
  #fea$Sex <- fea$Sex[fea$Sex == "^male$"] <- "1"
  fea$Sex[grep("\\bmale\\b", fea$Sex)] <- "1"
  fea$Sex[fea$Sex == "female"] <- "0"
  fea$Sex<-as.numeric(as.character(fea$Sex))
  fea$Embarked[fea$Embarked=="S"] <- "0"
  fea$Embarked[fea$Embarked=="Q"] <- "1"
  fea$Embarked[fea$Embarked=="C"] <- "2"
  
  fea$Age[is.na(fea$Age)] <- median(fea$Age, na.rm=TRUE)
  fea[is.na(fea)] <- 0
  fea[] <- lapply(fea, function(x) as.numeric(as.character(x)))
  
  return(fea)
}

xtrain <- extractFeatures(train,TRUE)
colSums(is.na(xtrain))

xtest <-extractFeatures(test,FALSE)
colSums(is.na(xtest))

attach(train)
xtrain <- cbind(Survived, xtrain)
detach(train)

library(neuralnet)
library(nnet)
library(caret)
library(e1071)

#data scaling using preprocess() method = "range"
xtrain_nn <- preProcess(xtrain, method = "range")
xtrain.df <- predict(xtrain_nn, xtrain )

xtrain.df$Live <- xtrain.df$Survived == 1
xtrain.df$Die <- xtrain.df$Survived == 0

set.seed(3)
options(digits = 3)
nn <- neuralnet( Die + Live ~ ., data = xtrain.df[,-1], hidden = 5, linear.output = F)

# plot network
plot(nn, rep="best")

############# train data confusionMatrix ##################
library(caret)
predict <- compute(nn, xtrain.df[,-c(1,9,10)])
predicted.class=apply(predict$net.result,1,which.max)-1
confusionMatrix( as.factor(predicted.class), as.factor(xtrain$Survived))


######################
###### test data #####
######################

#data scaling using preprocess() method = "range"
xtest_nn <- preProcess(xtest, method = "range")
xtest.df <- predict(xtest_nn, xtest )

predict <- compute(nn, xtest.df)
predicted.class=apply(predict$net.result,1,which.max)-1

submission <- data.frame(PassengerId = test$PassengerId)
submission$Survived <- predicted.class
write.csv(submission, file = "NN_Solution.csv", row.names=FALSE)
