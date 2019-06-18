#makes random sampling repeatable and loads packages
set.seed(100)
library(caret)
library(ggplot2)
library(rattle)
library(ROCR)
library(rpart.plot)
#read in the data set
final_test<-read.csv("~/Desktop/machine-testing.csv",header=TRUE)
final<-read.csv("~/Desktop/machine-training_1.csv",header=TRUE)

#cleans up the classe variable to simply tell if a part is made correctly or incorrectly and 
#displays right for good parts and wrong for bad parts
final$classe<-factor(ifelse(final$classe=="A",1,0))
levels(final$classe)<-c("wrong","right")

#take a peek at the data
summary(final)

#split data into training and testing set
inTrain <- createDataPartition(y=final$classe,
                               p=0.7, list=FALSE)
training <- final[inTrain,] 
testing <- final[-inTrain,1:57]
testing_1<-final[-inTrain,58]

#fit a basic logisitc regression model and check accuracy with confusionMatrix
modFit <- train(classe~ .,data=training,method="glm",preProcess=c("zv","pca"))
pred<-predict(modFit,newdata=testing)
confusionMatrix(pred,testing_1)

#make a performance object to check visually how our model did(ROC curve)
pred<-predict(modFit,newdata=testing,"prob")[,1]
roc1<-prediction(pred,testing_1)
perf<-performance(roc1,"tpr","fpr")


#fit a single decision tree and graph it and check accuracy with confusionMatrix
modFit2<-train(classe~.,method="rpart",data=training,preProcess=c("zv","pca"))
pred2<-predict(modFit2,newdata=testing)
confusionMatrix(pred2,testing_1)


# Make another performance object for next model
pred2<-predict(modFit2,newdata=testing,"prob")[,1]
roc2<-prediction(pred2,testing_1)
perf_1<-performance(roc2,"tpr","fpr")


#fit a random forest and check accuracy with confusionMatrix
modFit3<-train(classe~.,data=training,method="rf",preProcess=c("zv","pca"))
pred3<-predict(modFit3 ,newdata=testing)
confusionMatrix(pred3,testing_1)


#Make a performance object for last model
pred3<-predict(modFit3 ,newdata=testing,"prob")[,1]
roc3<-prediction(pred3,testing_1)
perf_2<-performance(roc3,"tpr","fpr")

#print out the predictions for blackboard quiz for each model
predict(modFit,final_test)
predict(modFit2,final_test)
predict(modFit3,final_test)

#plot the ROC curve from each model on one graph and create title and legend
plot(perf,col="blue",main="ROC curve")
plot(perf_1,add=TRUE,col="red")
plot(perf_2,add=TRUE,col="green")
legend(.8,.6, c("LR","DT","RF"),lty=c(1,1,1),lwd=c(2.5,2.5,2.5),col=c("blue","red","green")) 

#plot decision tree to help in presentation of interpretability
fancyRpartPlot(modFit2$finalModel,main="Classification Tree")

#perform five fold cross validation on each model to be sure of results using same splits of data so results are fair to compare

#shuffles the rows of data to randomize observations
set.seed(1234)
n <- nrow(final)
shuffled <- final[sample(n),]

#create vectors to hold accuracy for each iteration
accs <- rep(0,5)
accs_1<-rep(0,5)
accs_2<-rep(0,5)


#start of loop to create splits and measure accuaracy
for (i in 1:5) {
  # These indices indicate the interval of the test set
  indices <- (((i-1) * round((1/5)*nrow(shuffled))) + 1):((i*round((1/5) * nrow(shuffled))))
  
  # Exclude them from the train set
  train <- shuffled[-indices,]
  
  # Include them in the test set
  test <- shuffled[indices,]
  
  # A model is learned using each training set
  modFit4 <- train(classe~ .,data=train,method="glm",preProcess=c("zv","pca"))
  modFit5<-train(classe~.,method="rpart",data=train,preProcess=c("zv","pca"))
  modFit6<-train(classe~.,method="rf",data=train,preProcess=c("zv","pca"))
  
  
  # Make a prediction on the test set using modFit
  pred<-predict(modFit4,newdata=test)
  pred1<-predict(modFit5,newdata=test)
  pred2<-predict(modFit6,newdata=test)
  
  # Assign the accuracy of this model from confusion matrix to the ith index in accs
  accs[i] <- confusionMatrix(pred,test$classe)$overall[1]
  accs_1[i] <- confusionMatrix(pred1,test$classe)$overall[1]
  accs_2[i] <- confusionMatrix(pred2,test$classe)$overall[1]
  
  
}
#end of loop

# Print out the mean of accs(accuracy of each model)
mean(accs)
mean(accs_1)
mean(accs_2)



