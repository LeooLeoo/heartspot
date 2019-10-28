library(tree)
library(ISLR)
library(randomForest)
library(e1071)
library(adabag)
library(Matrix)
library(xgboost)
#train_data_01 <- read.csv(file="/Users/abhinavsharma/Desktop/datasets/new_data_01.csv")
train_data_012 <- read.csv(file="./datasets/new_data_012.csv")
#test_data <- read.csv(file="/Users/abhinavsharma/Desktop/datasets/testing_data.csv")
#coverting class columns as factors
#train_data_01$class <- as.factor(train_012$class)
train_data_012$class <- as.factor(train_data_012$class)
#test_data$class <- as.factor(test_data$class)

colnames(train_data_012)



#Creating training validation split from training data 
species = train_data_012$class
label = as.integer(train_data_012$class)#convert class to label
#train_012$class <- NULL

set.seed(1)
n = nrow(train_data_012)
train_index = sample(n,floor(0.8*n))
train_012 = train_data_012[train_index,]
train_label_012 = label[train_index]
val_data_012 = train_data_012[-train_index,]
val_label_012 = label[-train_index]


#decision trees
set.seed(3)
tree.train_012 = tree(as.factor(train_012$class)~., train_012)

summary(tree.train_012)
plot(tree.train_012)
text(tree.train_012 ,pretty =0)

prediction_dt <- predict(tree.train_012, val_data_012, type="class")
res_dt <- table(prediction_dt, val_data_012$class)
res_dt

dt.accuracy <- (res_dt[1,1] + res_dt[2,2] + res_dt[3,3])/sum(res_dt)
dt.precision_class0 <- res_dt[1,1]/(res_dt[1,1] + res_dt[1,2] + res_dt[1,3])
dt.recall_class0 <- res_dt[1,1]/(res_dt[1,1]+res_dt[2,1] + res_dt[3, 1])
dt.precision_class1 <- res_dt[2,2]/(res_dt[2,1] + res_dt[2,2] + res_dt[2,3])
dt.recall_class1 <- res_dt[2,2]/(res_dt[1,2]+res_dt[2,2] + res_dt[3, 2])
dt.precision_class2 <- res_dt[3,3]/(res_dt[3,1] + res_dt[3,2] + res_dt[3,3])
dt.recall_class2 <- res_dt[3,3]/(res_dt[1,3]+res_dt[2,3] + res_dt[3, 3])
print(c(dt.accuracy, dt.precision_class0, dt.recall_class0))
print(c(dt.accuracy, dt.precision_class1, dt.recall_class1))
print(c(dt.accuracy, dt.precision_class2, dt.recall_class2))


#random forests
set.seed(3)
rf.data <- randomForest(as.factor(train_012$class)~., train_012, ntree=20)

prediction_rf <- predict(rf.data, val_data_012, type="class")
res_rf <- table(prediction_rf, val_data_012$class)
res_rf

rf.accuracy <- (res_rf[1,1] + res_rf[2,2] + res_rf[3,3])/sum(res_rf)
rf.precision_class0 <- res_rf[1,1]/(res_rf[1,1] + res_rf[1,2] + res_rf[1,3])
rf.recall_class0 <- res_rf[1,1]/(res_rf[1,1]+res_rf[2,1] + res_rf[3, 1])
rf.precision_class1 <- res_rf[2,2]/(res_rf[2,1] + res_rf[2,2] + res_rf[2,3])
rf.recall_class1 <- res_rf[2,2]/(res_rf[1,2]+res_rf[2,2] + res_rf[3, 2])
rf.precision_class2 <- res_rf[3,3]/(res_rf[3,1] + res_rf[3,2] + res_rf[3,3])
rf.recall_class2 <- res_rf[3,3]/(res_rf[1,3]+res_rf[2,3] + res_rf[3, 3])
print(c(rf.accuracy, rf.precision_class0, rf.recall_class0))
print(c(rf.accuracy, rf.precision_class1, rf.recall_class1))
print(c(rf.accuracy, rf.precision_class2, rf.recall_class2))


#support vector machines
set.seed(3)
svm.data <- svm(as.factor(train_012$class)~., train_012, kernel = "polynomial")

prediction_svm <- predict(svm.data, val_data_012, type="class")
res_svm <- table(prediction_svm, val_data_012$class)
res_svm

svm.accuracy <- (res_svm[1,1] + res_svm[2,2] + res_svm[3,3])/sum(res_svm)
svm.precision_class0 <- res_svm[1,1]/(res_svm[1,1] + res_svm[1,2] + res_svm[1,3])
svm.recall_class0 <- res_svm[1,1]/(res_svm[1,1]+res_svm[2,1] + res_svm[3, 1])
svm.precision_class1 <- res_svm[2,2]/(res_svm[2,1] + res_svm[2,2] + res_svm[2,3])
svm.recall_class1 <- res_svm[2,2]/(res_svm[1,2]+res_svm[2,2] + res_svm[3, 2])
svm.precision_class2 <- res_svm[3,3]/(res_svm[3,1] + res_svm[3,2] + res_svm[3,3])
svm.recall_class2 <- res_svm[3,3]/(res_svm[1,3]+res_svm[2,3] + res_svm[3, 3])
print(c(svm.accuracy, svm.precision_class0, svm.recall_class0))
print(c(svm.accuracy, svm.precision_class1, svm.recall_class1))
print(c(svm.accuracy, svm.precision_class2, svm.recall_class2))

# multiclass adaboost classifier
set.seed(5)
ada <- adabag::boosting(class~., data = train_012, mfinal = 100)
prediction_ada <- adabag::predict.boosting(ada, val_data_012)
res_ada <- prediction_ada$confusion
res_ada 

ada.accuracy <- (res_ada[1,1] + res_ada[2,2] + res_ada[3,3])/sum(res_ada)
ada.precision_class0 <- res_ada[1,1]/(res_ada[1,1] + res_ada[1,2] + res_ada[1,3])
ada.recall_class0 <- res_ada[1,1]/(res_ada[1,1]+res_ada[2,1] + res_ada[3, 1])
ada.precision_class1 <- res_ada[2,2]/(res_ada[2,1] + res_ada[2,2] + res_ada[2,3])
ada.recall_class1 <- res_ada[2,2]/(res_ada[1,2]+res_ada[2,2] + res_ada[3, 2])
ada.precision_class2 <- res_ada[3,3]/(res_ada[3,1] + res_ada[3,2] + res_ada[3,3])
ada.recall_class2 <- res_ada[3,3]/(res_ada[1,3]+res_ada[2,3] + res_ada[3, 3])
print(c(ada.accuracy, ada.precision_class0, ada.recall_class0))
print(c(ada.accuracy, ada.precision_class1, ada.recall_class1))
print(c(ada.accuracy, ada.precision_class2, ada.recall_class2))

#XGBoost

#species = train_012$class
#train_label_012 = as.integer(train_label_012)#convert class to label
train_012$class <- NULL
#label = as.integer(val_data_012$class)#convert class to label
val_data_012$class <- NULL
#data$class = NULL
#Split the data for training and testing (75/25 split)
set.seed(3)
#n = nrow(train_012)
#train.index = sample(n,floor(0.75*n))
train_012 = as.matrix(train_012)
#train.label = label[train.index]
#test.data = as.matrix(train_data[-train.index,])
val_data_012 <- as.matrix(val_data_012)
#test.label = label[-train.index]
#Create the xgb.DMatrix objects
# Transform the two data sets into xgb.Matrix
xgb.train = xgb.DMatrix(data=train_012,label=(train_label_012 -1))
xgb.test = xgb.DMatrix(data=val_data_012,label=(val_label_012 -1))
#Define the main parameters
#train_data = as.matrix(train_data)
#xgb.train = xgb.DMatrix(data=as.matrix(train_data),label=)
# Define the parameters for multinomial classification
#num_class = length(as.numeric(levels(species)))
params = list(
  booster="gbtree",
  eta=0.001,
  max_depth=10,
  gamma=3,
  subsample=0.75,
  colsample_bytree=1,
  objective="multi:softprob",
  eval_metric="mlogloss",
  num_class=3
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
xgb.pred = predict(xgb.fit,val_data_012,reshape=T) 
xgb.pred = as.data.frame(xgb.pred) 
colnames(xgb.pred) = c(0,1,2)

#Identify the class with the highest probability for each prediction
# Use the predicted label with the highest probability
xgb.pred$prediction = apply(xgb.pred,1,function(x) colnames(xgb.pred)[which.max(x)])
xgb.pred$label = val_label_012

table <- table(xgb.pred$prediction, xgb.pred$label) 
table
res_xgboost <- table


xgboost.accuracy <- (res_xgboost[1,1] + res_xgboost[2,2] + res_xgboost[3,3])/sum(res_xgboost)
xgboost.precision_class0 <- res_xgboost[1,1]/(res_xgboost[1,1] + res_xgboost[1,2] + res_xgboost[1,3])
xgboost.recall_class0 <- res_xgboost[1,1]/(res_xgboost[1,1]+res_xgboost[2,1] + res_xgboost[3, 1])
xgboost.precision_class1 <- res_xgboost[2,2]/(res_xgboost[2,1] + res_xgboost[2,2] + res_xgboost[2,3])
xgboost.recall_class1 <- res_xgboost[2,2]/(res_xgboost[1,2]+res_xgboost[2,2] + res_xgboost[3, 2])
xgboost.precision_class2 <- res_xgboost[3,3]/(res_xgboost[3,1] + res_xgboost[3,2] + res_xgboost[3,3])
xgboost.recall_class2 <- res_xgboost[3,3]/(res_xgboost[1,3]+res_xgboost[2,3] + res_xgboost[3, 3])
print(c(xgboost.accuracy, xgboost.precision_class0, xgboost.recall_class0))
print(c(xgboost.accuracy, xgboost.precision_class1, xgboost.recall_class1))
print(c(xgboost.accuracy, xgboost.precision_class2, xgboost.recall_class2))

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
