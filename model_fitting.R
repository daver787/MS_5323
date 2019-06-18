library(caret);library(kernlab);data(spam)
set.seed(1234)
inTrain<-createDataPartition(y=spam$type,p=0.75,list=FALSE)
training<-spam[inTrain,]
#all the obs not in inTrain
testing<-spam[-inTrain,]
modelFit<-train(type~.,data=training,method="glm")
modelFit1<-train(type~.,data=training,method="rf")
predictions<-(modelFit,newdata=testing)
predictions
confusionMatrix(predictions,testing$type)
folds<-createResample(y=spam$type,times=10,list=TRUE)
sapply(folds,length)
args(train.default)