library(dplyr)
#load the data
clev <- read.csv("./datasets/processed.cleveland.csv", sep = "")
swiss <- read.csv("./datasets/processed.switzerland.csv", sep = ",")
va <- read.csv("./datasets/processed.va.csv", sep = ",")
hug <- read.csv("./datasets/processed.hungarian.csv", sep = "")



#####data exploration
#对于R语言，默认的missing符号是NA
clev[clev == "?"] <- NA
swiss[swiss == "?"] <- NA
va[va == "?"] <- NA
hug[hug == "?"] <- NA

#Initial Visualization for 缺失值、名义变量、连续变量
library(DataExplorer)
plot_missing(clev) ## Are there missing values, and what is the missing data profile?
plot_bar(clev) ## How does the categorical frequency for each discrete variable look like?
plot_histogram(clev) ## What is the distribution of each continuous variable?
#generate report for data exploration 神器!!!一键生成所有.html
create_report(clev)
# create_report(va, config = config) ##principal_component_analysis can only be applied on numerical data
# config <- list(
#   "introduce" = list(),
#   "plot_str" = list(
#     "type" = "diagonal",
#     "fontSize" = 35,
#     "width" = 1000,
#     "margin" = list("left" = 350, "right" = 250)
#   ),
#   "plot_missing" = list(),
#   "plot_histogram" = list(),
#   "plot_qq" = list(sampled_rows = 1000L),
#   "plot_bar" = list(),
#   "plot_correlation" = list("cor_args" = list("use" = "pairwise.complete.obs")),
#   #  "plot_prcomp" = list(),
#   "plot_boxplot" = list(),
#   "plot_scatterplot" = list(sampled_rows = 1000L)
# )




#####combine the training & testing data
#removing the last three features
clev <- clev[,-c(11,12,13)]
va <- va[,-c(11,12,13)]
swiss <- swiss[,-c(11,12,13)]

#rename the colum names
clev <- clev %>% rename(class = num)

#replacing missing values from to median
for (i in 1:ncol(va)){ 
  va[,i] <- as.numeric(va[,i])
  va[is.na(va[,i]),i]<- median(va[,i], na.rm =TRUE)
  } 
for (i in 1:ncol(clev)){ 
  clev[,i] <- as.numeric(clev[,i])
  }
for (i in 1:ncol(swiss)){ 
  swiss[,i] <- as.numeric(swiss[,i])
  swiss[is.na(swiss[,i]),i]<- median(swiss[,i], na.rm =TRUE)
} 

#combining va and clev
comb_data <- bind_rows(clev, va)

#exporting training and testing dataset
#write.csv(comb_data,"./datasets/training_data.csv", row.names = FALSE)
#write.csv(swiss,"./datasets/testing_data.csv", row.names = FALSE)




######relabel data for experiments
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

#export the data
write.csv(new_data_0123,"./datasets/new_data_0123.csv", row.names = FALSE)
write.csv(new_data_01,"./datasets/new_data_01.csv", row.names = FALSE)
write.csv(new_data_012,"./datasets/new_data_012.csv", row.names = FALSE)

