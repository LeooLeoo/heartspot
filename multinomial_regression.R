#multinomial regression
library(rattle.data)
train_data_012 <- read.csv(file="./datasets/training_data.csv")
train_data_012$class[train_data_012$class == 2] <- 1
train_data_012$class[train_data_012$class > 2] <- 2
train_data_012$class <- as.factor(train_data_012$class)
colnames(train_data_012)

#Creating training validation split from training data 
species = train_data_012$class
label = as.integer(train_data_012$class)#convert class to label
#train_012$class <- NULL

set.seed(1)
n = nrow(train_data_012)
train_index = sample(n,floor(0.7*n))
train_012 = train_data_012[train_index,]
train_label_012 = label[train_index]
val_data_012 = train_data_012[-train_index,]
val_label_012 = label[-train_index]

train_012$class <- relevel(train_012$class, ref = "0")
require(nnet)
# Training the multinomial model
multinom.fit <- multinom(class ~., data = train_012)

# Checking the model
summary(multinom.fit)

## extracting coefficients from the model and exponentiate
exp(coef(multinom.fit))

head(probability.table <- fitted(multinom.fit))

# Predicting the values for train dataset
val_data_012$precticed <- predict(multinom.fit, newdata = val_data_012, "class")

# Building classification table
ctable <- table(val_data_012$class, val_data_012$precticed)

# Calculating accuracy - sum of diagonal elements divided by total obs
round((sum(diag(ctable))/sum(ctable))*100,2)
ctable

calculate_performance_table <- function(ctable, class) {
  # transpose matrix
  ctable <- t(ctable)
  
  # performance table
  performance <- matrix( rep( 0, len=(6*(class+1))), nrow = (class+1))
  colnames(performance) <- c("Precision", "Recall", "F1-Score", "TP", "FP", "FN")
  rownames(performance) <- c("Class0", "Class1", "Class2", "Average")
  
  #accuracy <- (ctable[1,1] + ctable[2,2] + ctable[3,3] + ctable[4,4] + ctable[5,5])/sum(ctable)
  # calculate all metrics of all classes and fill the table
  for (i in 1:class) {
    # precison
    performance[i,1] <- ctable[i,i]/sum(ctable[,i])
    # recall
    performance[i,2] <- ctable[i,i]/sum(ctable[i,])
    # f1-score
    performance[i,3] <- 2 * ((performance[i,1] * performance[i,2])/(performance[i,1] + performance[i,2]))
    # true positives
    performance[i,4] <- ctable[i,i]
    # false positives
    performance[i,5] <- sum(ctable[,i]) - ctable[i,i]
    # false negatives
    performance[i,6] <- sum(ctable[i,]) - ctable[i,i]
  }

  # compute average of all metrics
  for (i in 1:(class)+1) {
    performance[(class+1), i] <- ((performance[1,i] + performance[2,i] + performance[3,i]) / class)
  }
  
  performance <- as.table(performance)
  return(performance)
}

multinomial.performance <- calculate_performance_table(ctable, 3)
multinomial.performance


#trying binomial
#=========================================================================================
library(rattle.data)
train_data_012 <- read.csv(file="./datasets/training_data.csv")
#train_data_012$class[train_data_012$class == 2] <- 1
train_data_012$class[train_data_012$class > 0] <- 1
train_data_012$class <- as.factor(train_data_012$class)
colnames(train_data_012)

#Creating training validation split from training data 
species = train_data_012$class
label = as.integer(train_data_012$class)#convert class to label
#train_012$class <- NULL

set.seed(1)
n = nrow(train_data_012)
train_index = sample(n,floor(0.75*n))
train_012 = train_data_012[train_index,]
train_label_012 = label[train_index]
val_data_012 = train_data_012[-train_index,]
val_label_012 = label[-train_index]

train_012$class <- relevel(train_012$class, ref = "0")
require(nnet)
# Training the multinomial model
multinom.fit <- multinom(class ~., data = train_012)

# Checking the model
summary(multinom.fit)

## extracting coefficients from the model and exponentiate
exp(coef(multinom.fit))

head(probability.table <- fitted(multinom.fit))

# Predicting the values for train dataset
val_data_012$precticed <- predict(multinom.fit, newdata = val_data_012, "class")

# Building classification table
ctable <- table(val_data_012$class, val_data_012$precticed)

# Calculating accuracy - sum of diagonal elements divided by total obs
round((sum(diag(ctable))/sum(ctable))*100,2)
ctable

calculate_performance_table <- function(ctable, class) {
  # transpose matrix
  ctable <- t(ctable)
  
  # performance table
  performance <- matrix( rep( 0, len=(6*(class+1))), nrow = (class+1))
  colnames(performance) <- c("Precision", "Recall", "F1-Score", "TP", "FP", "FN")
  rownames(performance) <- c("Class0", "Class1", "Average")
  
  #accuracy <- (ctable[1,1] + ctable[2,2] + ctable[3,3] + ctable[4,4] + ctable[5,5])/sum(ctable)
  # calculate all metrics of all classes and fill the table
  for (i in 1:class) {
    # precison
    performance[i,1] <- ctable[i,i]/sum(ctable[,i])
    # recall
    performance[i,2] <- ctable[i,i]/sum(ctable[i,])
    # f1-score
    performance[i,3] <- 2 * ((performance[i,1] * performance[i,2])/(performance[i,1] + performance[i,2]))
    # true positives
    performance[i,4] <- ctable[i,i]
    # false positives
    performance[i,5] <- sum(ctable[,i]) - ctable[i,i]
    # false negatives
    performance[i,6] <- sum(ctable[i,]) - ctable[i,i]
  }
  
  # compute average of all metrics
  for (i in 1:(class)+1) {
    performance[(class+1), i] <- ((performance[1,i] + performance[2,i] + performance[3,i]) / class)
  }
  
  performance <- as.table(performance)
  return(performance)
}

multinomial.performance <- calculate_performance_table(ctable, 2)
multinomial.performance