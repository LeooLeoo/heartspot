library(tree)
library(ISLR)
library(randomForest)
library(e1071)
library(adabag)
library(Matrix)
library(xgboost)
library("pROC") 
library("ROCR") 
train_data_01 <- read.csv(file="datasets/new_data_01.csv")
swiss_01 <- read.csv(file="datasets/swiss_01.csv")
#train_data_012 <- read.csv(file="datasets/new_data_012.csv")
#test_data <- read.csv(file="/Users/abhinavsharma/Desktop/datasets/testing_data.csv")
#coverting class columns as factors
train_data_01$class <- as.factor(train_data_01$class)
#train_data_012$class <- as.factor(train_012$class)
#test_data$class <- as.factor(test_data$class)
colnames(train_data_01)



#Creating training validation split from training data 
species = train_data_01$class
label = as.integer(train_data_01$class)#convert class to label
#train_01$class <- NULL

set.seed(1)
n = nrow(train_data_01)
train_index = sample(n,floor(0.8*n))
train_01 = train_data_01[train_index,]
train_label_01 = label[train_index]
val_data_01 = train_data_01[-train_index,]
val_label_01 = label[-train_index]
species_val = species[-train_index]



#decision trees
set.seed(3)
tree.train_01 = tree(as.factor(train_01$class)~., train_01)

summary(tree.train_01)
plot(tree.train_01)
text(tree.train_01 ,pretty =0)

prediction_dt <- predict(tree.train_01, val_data_01, type="class")
res_dt <- table(prediction_dt, val_data_01$class)
# prediction_dt <- predict(tree.train_01, swiss_01, type="class")
# res_dt <- table(prediction_dt, swiss_01$class)

res_dt

dt.accuracy <- (res_dt[1,1] + res_dt[2,2])/sum(res_dt)
dt.precision_class0 <- res_dt[1,1]/(res_dt[1,1] + res_dt[2,1])
dt.recall_class0 <- res_dt[1,1]/(res_dt[1,1]+res_dt[1,2])

dt.accuracy
dt.precision_class0
dt.recall_class0

#random forests
set.seed(3)
rf.data <- randomForest(as.factor(train_01$class)~., train_01, ntree=20)

prediction_rf <- predict(rf.data, val_data_01, type="class")
res_rf <- table(prediction_rf, val_data_01$class)
res_rf

rf.accuracy <- (res_rf[1,1] + res_rf[2,2])/sum(res_rf)
rf.precision_class0 <- res_rf[1,1]/(res_rf[1,1] + res_rf[2,1])
rf.recall_class0 <- res_rf[1,1]/(res_rf[1,1]+res_rf[1,2])
rf.accuracy
rf.precision_class0
rf.recall_class0


#support vector machines
set.seed(3)
svm.data <- svm(as.factor(train_01$class)~., train_01, kernel = "polynomial")

prediction_svm <- predict(svm.data, val_data_01, type="class")
res_svm <- table(prediction_svm, val_data_01$class)
res_svm

accuracy <- (res_svm[1,1] + res_svm[2,2])/sum(res_svm)
precision <- res_svm[1,1]/(res_svm[1,1]+res_svm[2,1])
recall <- res_svm[1,1]/(res_svm[1,1]+res_svm[1,2])

accuracy
precision
recall


# multiclass adaboost classifier
set.seed(5)
ada <- adabag::boosting(class~., data = train_01, mfinal = 100)
prediction_ada <- adabag::predict.boosting(ada, val_data_01)
res_ada<-prediction_ada$confusion
res_ada
accuracy <- (res_ada[1,1] + res_ada[2,2])/sum(res_ada)
precision <- res_ada[1,1]/(res_ada[1,1]+res_ada[2,1])
recall <- res_ada[1,1]/(res_ada[1,1]+res_ada[1,2])

accuracy
precision
recall



#XGBoost

#species = train_01$class
#train_label_01 = as.integer(train_label_01)#convert class to label
train_01$class <- NULL
#label = as.integer(val_data_01$class)#convert class to label
val_data_01$class <- NULL
#data$class = NULL
#Split the data for training and testing (75/25 split)
set.seed(3)
#n = nrow(train_01)
#train.index = sample(n,floor(0.75*n))
train_01 = as.matrix(train_01)
#train.label = label[train.index]
#test.data = as.matrix(train_data[-train.index,])
val_data_01 <- as.matrix(val_data_01)
#test.label = label[-train.index]
#Create the xgb.DMatrix objects
# Transform the two data sets into xgb.Matrix
xgb.train = xgb.DMatrix(data=train_01,label=train_label_01 -1)
xgb.test = xgb.DMatrix(data=val_data_01,label=val_label_01 -1)
#Define the main parameters
#train_data = as.matrix(train_data)
#xgb.train = xgb.DMatrix(data=as.matrix(train_data),label=)
# Define the parameters for multinomial classification
#num_class = length(as.numeric(levels(species)))
params = list(
  booster="gbtree",
  eta=0.001,
  max_depth=3,
  gamma=3,
  subsample=0.75,
  colsample_bytree=1,
  objective="multi:softprob",
  eval_metric="mlogloss",
  num_class=2
)
#Train the model
# Train the XGBoost classifer
xgb.fit=xgb.train(
  params=params,
  data=xgb.train,
  nrounds=10000,
  nthreads=1,
  early_stopping_rounds=10,
  watchlist=list(val1=xgb.train,val2=xgb.test),
  verbose=0
)

# Review the final model and results
xgb.fit


# Predict outcomes with the test data
xgb.pred = predict(xgb.fit,val_data_01,reshape=T) 
xgb.pred = as.data.frame(xgb.pred) 
colnames(xgb.pred) = c(0,1)

#Identify the class with the highest probability for each prediction
# Use the predicted label with the highest probability
xgb.pred$prediction = apply(xgb.pred,1,function(x) colnames(xgb.pred)[which.max(x)])
xgb.pred$label = species_val

table <- table(xgb.pred$prediction, xgb.pred$label) 
table
res_xgboost <- table
xgb.accuracy <- (res_xgboost[1,1] + res_xgboost[2,2])/sum(res_xgboost)
xgb.precision_class0 <- res_xgboost[1,1]/(res_xgboost[1,1] + res_xgboost[2,1])
xgb.recall_class0 <- res_xgboost[1,1]/(res_xgboost[1,1]+res_xgboost[1,2])
xgb.accuracy
xgb.precision_class0
xgb.recall_class0

#Testing on external dataset
xgb.test.ext = xgb.DMatrix(data=as.matrix(test_data))
test_label <- test_data[,11]
test_data$class <- NULL
test.data <- test_data[,-11]
xgb.pred.ext = predict(xgb.fit,xgb.test.ext,reshape=T) 
xgb.pred.ext = as.data.frame(xgb.pred.ext) 
colnames(xgb.pred.ext) = c(0,1,2,3,4)

#Identify the class with the highest probability for each prediction
# Use the predicted label with the highest probability
xgb.pred.ext$prediction = apply(xgb.pred.ext,1,function(x) colnames(xgb.pred.ext)[which.max(x)])
xgb.pred.ext$label = test_label

table2 <- table(xgb.pred.ext$prediction, xgb.pred.ext$label) 
table2





##AUC
## ROC for DT
auc <- auc(val_data_01$class, as.numeric(prediction_dt))
auc 
plot(roc(val_data_01$class, as.numeric(prediction_dt)))


## ROC for random forest
roc(train_01$class, rf.data$votes[,1], plot=TRUE, legacy.axes=TRUE, percent=TRUE, xlab="False Positive Percentage", ylab="True Postive Percentage", col="#4daf4a", lwd=4, print.auc=TRUE)


## ROC for SVM
svm.probs<-predict(svm.data, val_data_01, type="response")
svm.class<-predict(svm.data, val_data_01)
svm.labels<- val_data_01$class
svm.confusion<-confusionMatrix(svm.class, svm.labels) #predicted and actual
svm.confusion

svm.prediction<-prediction(as.numeric(svm.probs),as.numeric(svm.labels))
svm.performance<-performance(svm.prediction,"tpr","fpr")
svm.auc<-performance(svm.prediction,"auc")@y.values[[1]]
print(svm.auc)
plot(svm.performance,col="red",lwd=2)
par(pty = "m")
