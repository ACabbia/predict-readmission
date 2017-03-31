#load the libraries
library(DMwR)
library(plyr)
library(dplyr)
library(caret)
library(ranger)
library(doParallel)	

#########################################################
#### select here dataset to analyze: A1C (only subjects with HbA1c measure) or diabetic_data (full dataset)

d_data<-diabetic_data
# fix class imbalance resampling from less represented class

barplot(table(d_data$readmitted))
d_data<-DMwR::SMOTE(readmitted~.,data = d_data)
barplot(table(d_data$readmitted))

#determine predictors (x) and outcome variable (y)
#create train and test set (70/30)

x=select(d_data, -readmitted)
y=factor(d_data$readmitted)

idx=sort(sample(nrow(d_data),nrow(d_data)*.7)) 
train.X=x[idx,]
train.Y=y[idx]
test.X=x[-idx,] 
test.Y=y[-idx]

# ensure the results are repeatable
set.seed(666)

# define the control options
ctrl <- trainControl(method = "cv",   
                     number = 10,   # 10fold cross validation
                     allowParallel = TRUE)

# Set up parallel processing   
registerDoParallel()		# Registrer a parallel backend for train
getDoParWorkers()

# run the ranger algorithm (RF with multicore support) 
# to find variable importance, peform recursive feature elimination for all the variables (x) on the feature 'readmitted'(y) USING ALL THE DATASET
resultsRF <- train(x,y, method = "ranger", trControl=ctrl ,importance="permutation",metric="Kappa")
# summarize the results
resultsRF
# list the importance of features
varImp(resultsRF)

##### rf and logit classification models training #############################
#model.RF <- train(train.X,train.Y, method = "ranger", trControl=ctrl,metric="Kappa")
model.LR <- train(train.X,train.Y, method = 'glm', family = binomial, metric="Kappa")

mx.set.seed(666)
model.MX <- mx.mlp(train.X, train.Y, hidden_node=10, out_node=2, out_activation="softmax",
                num.round=20, array.batch.size=15, learning.rate=0.07, momentum=0.9,
                eval.metric=mx.metric.accuracy)

##### Perform prediction for test data ##################

fitted.results.RF = predict(model.RF,newdata=test.X)
fitted.results.LR = predict(model.LR,newdata=test.X)

#### Evaluate results RF classification
##Total Accuracy

cm.rf=as.matrix(table(test.Y, fitted.results.RF))

AccuracyRF=sum(diag(cm.rf)/length(test.Y))
print(paste('Overall accuracy Random Forest classification: ',AccuracyRF))
##Precision: fraction of correct predictions for a class
PrecisionRF=diag(cm.rf)/colSums(cm.rf)
print(paste('Precision class 1: Correct YES predictions',PrecisionRF[1]))
print(paste('Precision class 2: Correct NO predictions',PrecisionRF[2]))
##Recall: fraction of instances of a class that were correctly predicted
RecallRF=diag(cm.rf)/rowSums(cm.rf)
print(paste('Recall class 1: fraction of readmitted patients correctly predicted',RecallRF[1]))
print(paste('Recall class 2: fraction of NON readmitted patients correctly predicted',RecallRF[2]))

cm.lr=as.matrix(table(test.Y, fitted.results.LR))

AccuracyLR=sum(diag(cm.lr)/length(test.Y))
print(paste('Overall accuracy Logistic Regression classification: ',AccuracyLR))
##Precision: fraction of correct predictions for a class
PrecisionLR=diag(cm.lr)/colSums(cm.lr)
print(paste('Precision class 1: Correct YES prediction', PrecisionLR[1]))
print(paste('Precision class 2: Correct NO prediction', PrecisionLR[2]))
##Recall: fraction of instances of a class that were correctly predicted
RecallLR=diag(cm.lr)/rowSums(cm.lr)
print(paste('Recall class 1: fraction of readmitted patients correctly predicted',RecallLR[1]))
print(paste('Recall class 2: fraction of NON readmitted patients correctly predicted',RecallLR[2]))


#######################################################################################################################
#####Redo prediction with only significant top 5 features from random forest feature importance #######################
SignifVars<-as.data.frame(sort(resultsRF$finalModel$variable.importance, decreasing = TRUE))
diabetic_data_signif=diabetic_data[,c(as.factor(SignifVars[1:6,]))]  # <--- change the "5" in this line to change the number of significant vars considered 

#################################################################################################
x=diabetic_data_signif
y=diabetic_data$readmitted

idx=sort(sample(nrow(x),nrow(x)*.7)) 

train.X.sgf=x[idx,]
train.Y.sgf=y[idx]
test.X.sgf=x[-idx,] 
test.Y.sgf=y[-idx]

modelLR.sgf <- train(train.X.sgf,train.Y.sgf, method = 'glm', family = binomial, metric="Kappa")
summary(modelLR.sgf)

##### Perform prediction for test top5 variables from RF model ##################

fitted.results.LR.sgf = predict(modelLR.sgf,newdata=test.X.sgf)

#### Evaluate results RF classification
##Total Accuracy

cm.lr.sgf=as.matrix(table(test.Y.sgf, fitted.results.LR.sgf))

AccuracyLR.sgf=sum(diag(cm.lr.sgf)/length(test.Y.sgf))
print(paste('Overall accuracy Logistic Regression with top 5 features classification: ',AccuracyLR.sgf))
##Precision: fraction of correct predictions for a class
PrecisionLR.sgf=diag(cm.lr.sgf)/colSums(cm.lr.sgf)
print(paste('Precision class 1: Correct YES predictions',PrecisionLR.sgf[1]))
print(paste('Precision class 2: Correct NO predictions',PrecisionLR.sgf[2]))
##Recall: fraction of instances of a class that were correctly predicted
RecallLR.sgf=diag(cm.lr.sgf)/rowSums(cm.lr.sgf)
print(paste('Recall class 1: fraction of readmitted patients correctly predicted',RecallLR.sgf[1]))
print(paste('Recall class 2: fraction of NON readmitted patients correctly predicted',RecallLR.sgf[2]))

