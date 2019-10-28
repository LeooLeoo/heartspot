library(tree)
library(ISLR)
library(randomForest)
library(e1071)

training_data <- read.csv(file="datasets/training_data.csv")
test_data <- read.csv(file="datasets/testing_data.csv")

#performance function encapsulation
calculate_performance_table <- function(conmatrix, classnum) { #confusion matrix and class number
  # transpose matrix
  conmatrix <- t(conmatrix)

  # performance table
  performance <- matrix( rep( 0, len=(7*(classnum+1))), nrow = (classnum+1)) #new container
  colnames(performance) <- c("Precision", "Recall", "F1-Score", "TP", "FP", "FN","Accuracy")
  rownames(performance) <- c("Class0", "Class1", "Class2", "Class3", "Class4", "Average")
  
  
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
  for (i in 1:(classnum)+1) {
    performance[(classnum+1), i] <- (sum(performance[1:classnum,i]) / classnum)
  }
  
  performance <- as.table(performance)
  print(conmatrix)
  return(performance)
}

########## decision tree ##########
set.seed(3)
tree.data_train = tree(as.factor(training_data$class)~., training_data)

prediction_dt <- predict(tree.data_train, test_data, type="class")
res_dt <- table(prediction_dt, test_data$class)

tree.performance <- calculate_performance_table(res_dt, 5)
tree.performance

########## random forests ##########
# number of trees: ntree = 3/5/10/20
set.seed(3)
rf.data <- randomForest(as.factor(training_data$class)~., training_data, ntree=5)

prediction_rf <- predict(rf.data, test_data, type="class")
res_rf <- table(prediction_rf, test_data$class)

rf.performance <- calculate_performance_table(res_rf, 5)
rf.performance


########## support vector machines ##########
# kernel =linear /radial /polynomial
set.seed(3)
svm.data <- svm(as.factor(training_data$class)~., training_data, kernel = "linear")

prediction_svm <- predict(svm.data, test_data, type="class")
res_svm <- table(prediction_svm, test_data$class)

svm.performance <- calculate_performance_table(res_svm, 5)
svm.performance
