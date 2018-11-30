library(dplyr)

train <- read.csv("D://Consulting Project/tutu/train.csv", header = T)
trainNoid <- train[,-1]
str(train)
numofCol <- length(train[1, ])

##explore missing value
numofNA <- sapply(1:numofCol, function(i) sum(is.na(train[, i])))
ind <- 1:numofCol
names(train)[ind[numofNA > 100]]

##feature engineering
numofRow <- length(train[,1])
trainNoid$Alley <- as.factor(sapply(1:numofRow, function(i) ifelse(is.na(trainNoid$Alley[i]),"missing", trainNoid$Alley[i])))
trainNoid$FireplaceQu <- as.factor(sapply(1:numofRow, function(i) ifelse(is.na(trainNoid$FireplaceQu[i]),"missing", trainNoid$FireplaceQu[i])))
trainNoid$PoolQC <- as.factor(sapply(1:numofRow, function(i) ifelse(is.na(trainNoid$PoolQC[i]),"missing", trainNoid$PoolQC[i])))
trainNoid$MiscFeature <- as.factor(sapply(1:numofRow, function(i) ifelse(is.na(trainNoid$MiscFeature[i]),"missing", trainNoid$MiscFeature[i])))
trainNoid$Fence <- as.factor(sapply(1:numofRow, function(i) ifelse(is.na(trainNoid$Fence[i]),"missing", trainNoid$Fence[i])))


numofNA1 <- sapply(1:(numofCol-1), function(i) sum(is.na(trainNoid[, i])))
ind1 <- 1:(numofCol-1)
names(trainNoid)[ind[numofNA1 > 100]]

trainNoid$LFisNA <- as.factor(sapply(1:numofRow, function(i) ifelse(is.na(train$LotFrontage[i]), 1, 0)))
trainNoid$LotFrontage <- sapply(1:numofRow, function(i) ifelse(is.na(trainNoid$LotFrontage[i]), 0, trainNoid$LotFrontage[i]))