library(tree)
library(ISLR)
library(randomForest)
library(e1071)
library(adabag)
library(Matrix)
library(xgboost)
library("pROC") 
library("ROCR") 
train_data_01 <- read.csv(file="./datasets/new_data_01.csv")
swiss_01 <- read.csv(file="./datasets/swiss_01.csv")
#coverting class columns as factors
train_data_01$class <- as.factor(train_data_01$class)
#colnames(train_data_01)

#Creating training validation split from training data 
species = train_data_01$class
label = as.integer(train_data_01$class)#convert class to label

set.seed(1)
n = nrow(train_data_01)
train_index = sample(n,floor(0.8*n))
train_01 = train_data_01[train_index,]
train_label_01 = label[train_index]
val_data_01 = train_data_01[-train_index,]
val_label_01 = label[-train_index]
species_val = species[-train_index]


#performance function encapsulation
calculate_performance_table <- function(conmatrix, classnum) { #confusion matrix and class number
  # transpose matrix
  conmatrix <- t(conmatrix)
  
  # performance table
  performance <- matrix( rep( 0, len=(7*(classnum+1))), nrow = (classnum+1)) #new container
  colnames(performance) <- c("Precision", "Recall", "F1-Score", "TP", "FP", "FN","Accuracy")
  rownames(performance) <- c("Class0", "Class1", "Average")
  
  
  # calculate all metrics of all classes and fill the table
  for (i in 1:classnum) {
    # precison
    performance[i,1] <- conmatrix[i,i]/sum(conmatrix[,i])
    # recall
    performance[i,2] <- conmatrix[i,i]/sum(conmatrix[i,])
    # f1-score
    performance[i,3] <- 2 * ((performance[i,1] * performance[i,2])/(performance[i,1] + performance[i,2]))
    # true positives
    performance[i,4] <- conmatrix[i,i]
    # false positives
    performance[i,5] <- sum(conmatrix[,i]) - conmatrix[i,i]
    # false negatives
    performance[i,6] <- sum(conmatrix[i,]) - conmatrix[i,i]
    #accuracy
    performance[,7] <- sum(diag(conmatrix))/sum(conmatrix)
  }
  
  # compute average of all metrics
  for (i in 1:7) {
    performance[(classnum+1), i] <- sum(performance[1:classnum,i]) / classnum
  }
  
  performance <- as.table(performance)
  print(conmatrix)
  return(performance)
}



######decision trees######
set.seed(3)
tree.train_01 = tree(train_01$class~., train_01)

summary(tree.train_01)
plot(tree.train_01)
text(tree.train_01 ,pretty =0)

prediction_dt <- predict(tree.train_01, val_data_01, type="class")
res_dt <- table(prediction_dt, val_data_01$class)
#performance
tree.performance <- calculate_performance_table(res_dt, 2)
tree.performance



######random forests########
set.seed(3)
rf.data <- randomForest(as.factor(train_01$class)~., train_01, ntree=20) #5,10,20

prediction_rf <- predict(rf.data, val_data_01, type="class")
res_rf <- table(prediction_rf, val_data_01$class)
#performance
rf.performance <- calculate_performance_table(res_rf, 2)
rf.performance



####### support vector machines######
set.seed(3)
svm.data <- svm(as.factor(train_01$class)~., train_01, kernel = "polynomial")

prediction_svm <- predict(svm.data, val_data_01, type="class")
res_svm <- table(prediction_svm, val_data_01$class)
#performance
svm.performance <- calculate_performance_table(res_svm, 2)
svm.performance



###### multiclass adaboost classifier######
set.seed(5)
ada <- adabag::boosting(class~., data = train_01, mfinal = 20)#20,50,100
prediction_ada <- adabag::predict.boosting(ada, val_data_01)
res_ada<-prediction_ada$confusion
#performance
ada.performance <- calculate_performance_table(res_ada, 2)
ada.performance



#######XGBoost######
#remove the class labels from features
train_01$class <- NULL
val_data_01$class <- NULL

set.seed(3)
#tranfer the datatype to fit XGBoost model
train_01 = as.matrix(train_01)
val_data_01 <- as.matrix(val_data_01)
# Transform the two data sets into xgb.Matrix
xgb.train = xgb.DMatrix(data=train_01,label=train_label_01 -1)
xgb.test = xgb.DMatrix(data=val_data_01,label=val_label_01 -1)

#Define the main parameters
params = list(
  booster="gbtree",
  eta=0.001,
  max_depth=3, #3,5,10
  gamma=3,
  subsample=0.75,
  colsample_bytree=1,
  objective="multi:softprob",
  eval_metric="mlogloss",
  num_class=2
)

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

res_xgboost <- as.matrix(table(xgb.pred$prediction, xgb.pred$label))
#performance
xgboost.performance <- calculate_performance_table(res_xgboost, 2)
xgboost.performance






#Testing on external dataset
test_data <- swiss_01
test_label <- test_data[,11]
test_data$class <- NULL
test.data <- test_data[,-11]
xgb.test.ext = xgb.DMatrix(data=as.matrix(test_data))
xgb.pred.ext = predict(xgb.fit,xgb.test.ext,reshape=T) 
xgb.pred.ext = as.data.frame(xgb.pred.ext) 
colnames(xgb.pred.ext) = c(0,1)

#Identify the class with the highest probability for each prediction
# Use the predicted label with the highest probability
xgb.pred.ext$prediction = apply(xgb.pred.ext,1,function(x) colnames(xgb.pred.ext)[which.max(x)])
xgb.pred.ext$label = test_label

table2 <- table(xgb.pred.ext$prediction, xgb.pred.ext$label) 
table2
#performance
xgboost.performance <- calculate_performance_table(table2, 2)
xgboost.performance




##AUC
## ROC for DT
auc <- auc(val_data_01$class, as.numeric(prediction_dt)) #test and predicted
auc 
plot(roc(val_data_01$class, as.numeric(prediction_dt)))
#another way to plot auc for dt
prediction_dt <- predict(tree.train_01, val_data_01, type="class")
dt.prediction<-prediction(as.numeric(prediction_dt),as.numeric(val_data_01$class))
dt.performance<-performance(dt.prediction,"tpr","fpr")
dt.auc<-performance(dt.prediction,"auc")@y.values[[1]]
dt.auc
plot(dt.performance,col="red",lwd=2)
par(pty = "m")
##one more way to plot auc
library(plotROC)
df <-val_data_01 #test data
rocplot <- ggplot(df, aes(m = as.numeric(prediction_dt), d = as.numeric(val_data_01$class))) + geom_roc(n.cuts=20,labels=F)
rocplot + style_roc(theme = theme_grey) + geom_rocci(fill="pink") 

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
#another way
df <-val_data_01 #test data
rocplot <- ggplot(df, aes(m = as.numeric(svm.class), d = as.numeric(svm.labels))) + geom_roc(n.cuts=20,labels=F)
rocplot + style_roc(theme = theme_grey) + geom_rocci(fill="pink") 
