library(dplyr)
#load the data
clev <- read.csv2("./datasets/processed.cleveland.csv", sep = "")
swiss <- read.csv2("./datasets/processed.switzerland.csv", sep = ",")
va <- read.csv2("./datasets/processed.va.csv", sep = ",")



#removing the last three features
clev <- clev[,-c(11,12,13)]
va <- va[,-c(11,12,13)]
va[va == "?"] <- NA
#replacing missing values from data va to median
for (i in 1:ncol(va)){ 
  va[,i] <- as.numeric(va[,i])
  va[is.na(va[,i]),i]<- median(va[,i], na.rm =TRUE)
  } 
#combining va and clev
for (i in 1:ncol(clev)){ 
  clev[,i] <- as.numeric(clev[,i])
  }
clev <- clev %>% rename(class = num)
comb_data <- bind_rows(clev, va)
swiss <- swiss[,-c(11,12,13)]
for (i in 1:ncol(swiss)){ 
  swiss[,i] <- as.numeric(swiss[,i])
  swiss[is.na(swiss[,i]),i]<- median(swiss[,i], na.rm =TRUE)
} 
#exporting combined va and clev dataset
#write.csv(comb_data,"/Users/abhinavsharma/Desktop/datasets/training_data.csv", row.names = FALSE)
#write.csv(swiss,"/Users/abhinavsharma/Desktop/datasets/testing_data.csv", row.names = FALSE)

new_data_0123 <- comb_data
new_data_01 <- comb_data
new_data_012 <- comb_data

#relabelling: 0,1,2,3
new_data_0123$class[comb_data$class == 4] <- 3
table(new_data_0123$class)
table(comb_data$class)

#relabelling: 0,1,
new_data_01$class[new_data_01$class != 0] <- 1
table(new_data_01$class)
table(comb_data$class)

#relabelling: 0,1,2
new_data_012$class[new_data_012$class == 2] <- 1
new_data_012$class[new_data_012$class == 3] <- 2
new_data_012$class[new_data_012$class == 4] <- 2
table(new_data_012$class)
table(comb_data$class)



write.csv(new_data_0123,"/Users/abhinavsharma/Desktop/datasets/new_data_0123.csv", row.names = FALSE)
write.csv(new_data_01,"/Users/abhinavsharma/Desktop/datasets/new_data_01.csv", row.names = FALSE)
write.csv(new_data_012,"/Users/abhinavsharma/Desktop/datasets/new_data_012.csv", row.names = FALSE)



