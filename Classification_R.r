#This is an attempt in implementing classification algorithms using R

#step 1: Reading the data
#we need to provide the absolute path of the file in R.
data=read.csv2('~/Desktop/Bank/bank-full.csv')
head(data)

#This is a bank-loan dataset , with multiple independent variable and final Y value as a variable to predict whether
#the loan holder will payback the loan in 10-years time or not.

#Step 2: Data Cleaning phase
#step 2.1:removing the NA values and replacing all the categorical data by continuous data
clean<-function(data){
  cols=colnames(data)
  for(col in cols){
    if(any(is.na(data[,col]))){
      if(class(data[,col])%in% c('numerical','integer')){
        data[,col]=ifelse(is.na(data[,col]),mean(data[,col],na.rm=TRUE),data[,col]) #replaces the NA value with mean value 
      }else{
        data[,col]=ifelse(is.na(data[,col]),'none',data[,col]) #replaces the NA value with mean value 
      }
    }else if(class(data[,col])=='factor'){
      data[,col]=factor(data[,col],levels=unique(data[,col]),labels=1:nlevels(data[,col]))
      data[,col]=as.integer(data[,col])
    }
    
  }
  return(data)
}
data=clean(data)

#step 2.2:splitting the data into train and test set
library(caTools)
set.seed(1234)
to_split=sample.split(data,SplitRatio = 0.8)
train_set=data[to_split==TRUE,]
test_set=data[to_split==FALSE,]


#step3: Checking for the significance of the various independent variables on the final answer
#This can be done by taking the summary of the linear regression of the train_data
library(e1071)
significance_check=lm(formula=y~.,data=train_set)
summary(significance_check)

#going by the summary , it seems like only the job and default columns in the data have no significance
#dropping them from the train as well as test data for a better accuracy

train_set=train_set[,!colnames(train_set) %in% c('job','default')]
test_set=test_set[,!colnames(test_set) %in% c('job','default')]


#Step 4: Classification
#since now that we have removed the non-significant variables, lets dive right into the classification part

#Classification Algorithms :
#1.Logistic Regression 
#2.K-Nearest-Neighbours
#3.Support Vector Machine
#4.Support Vector Machine (Kernels)
#5.Naive Bayes
#6.Decision Tree Classifier
#7.Random Forest Classifier

#Here , I will implement each algorithm mentioned above , and in the end we will compute the accuracy score of each 
#algo to finalise the best Classifier, lets go !

#Logistic Regression 
library(e1071)
#here we need the final answer to be of categorical type(duh ! obviously , its classification dude )
train_set$y=factor(train_set$y,levels=c(1,2))
test_set$y=factor(test_set$y,levels=c(1,2))

log_classifier=glm(formula=y~.,data=train_set,family=binomial)
log_predict=predict(log_classifier,newdata = test_set,type='response')
log_predict=ifelse(log_predict>=0.5,2,1)



#K-Nearest Neighbors
library(class)
knn_predict=knn(train=train_set[-15],test=test_set[-15],cl=train_set$y,k=5)
#Thing to notice here , that knn returns the predicted value directly, good for us, saved us typing time :P



#Support Vector Machine
library(e1071)
svm_classifier=svm(formula=y~.,data=train_set,type='C-classification')
svm_predict=predict(svm_classifier,newdata=test_set)


#Support Vector Machine (Kernel)
#same library as of svm, even same function, all we need is an extra parameter kernel
svm_kernel_classifier=svm(formula=y~.,data=train_set,type='C-classification',kernel='radial')
svm_kernel_predict=predict(svm_classifier,newdata=test_set)


#Naive Bayes
library(e1071)
nB_classifier=naiveBayes(formula=y~.,data=train_set)
nB_pred=predict(nB_classifier,newdata = test_set)

#Decision Tree Classifier
library(rpart)
dt_classifier=rpart(formula=y~.,data=train_set,control=rpart.control(minsplit = 10))
dt_pred=predict(dt_classifier,newdata = test_set)


#Random Forest Classifier
library(randomForest)
rf_classifier=randomForest(formula=y~.,data=train_set,ntree=50)
rf_pred=predict(rf_classifier,newdata = test_set)


#Accuracy score calculation


#Logistic Regression
cm=table(test_set$y,log_predict)
acc=sum(diag(cm))/sum(cm)
print(acc)

#KNN
cm=table(test_set$y,knn_predict)
acc=sum(diag(cm))/sum(cm)
print(acc)

#svm
cm=table(test_set$y,svm_predict)
acc=sum(diag(cm))/sum(cm)
print(acc)

#svm-kernel
cm=table(test_set$y,svm_kernel_predict)
acc=sum(diag(cm))/sum(cm)
print(acc)

#Naive Bayes
cm=table(test_set$y,nB_pred)
acc=sum(diag(cm))/sum(cm)
print(acc)

#Decision Tree Classifier
cm=table(test_set$y,dt_pred)
acc=sum(diag(cm))/sum(cm)
print(acc)

#Random Forest Classifier
cm=table(test_set$y,rf_pred)
acc=sum(diag(cm))/sum(cm)
print(acc)



