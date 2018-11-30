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

model0 <- lm(SalePrice ~ ., data = trainNoid, na.action = na.exclude)


## neural network modeling
library(nnet)
## normalizing
isFactor <- sapply(trainNoid[1,], is.factor)
## normalize 
locPar <- sapply(na.omit(trainNoid[,!isFactor]), mean)
scalPar <- sapply(na.omit(trainNoid[,!isFactor]), sd)
trainNumeric <- trainNoid[,!isFactor]

len <- sum(!isFactor)
trainNumeric1 <- as.data.frame(sapply(1:len, function(i) scale(trainNumeric[, i], center = locPar[i], scale = scalPar[i])))
names(trainNumeric1) <- names(trainNumeric)

trainNnet <- na.omit(cbind(trainNumeric1, trainNoid[,isFactor]))


model1 <- nnet(SalePrice ~ ., data = trainNnet, na.action = na.exclude, size = 5, decay = 1, linout = T, MaxNWts = 100000, maxit = 50000)


## cross validation for regularization parameter tuning, and hidden layer construction.
set.seed(123)
ctrl <- trainControl(method = "cv", number = 10, verboseIter = T, allowParallel = T)

pars <- expand.grid(size = c(1,3,5,7,9), decay = seq(0,1,0.2))

neuralnetwork <- train(SalePrice~., trainNnet, method = "nnet", trControl = ctrl, trace=TRUE, maxit=50000, MaxNWts = 50000, linout = 1, tuneGrid = pars)

neuralnetworkTest <- train(SalePrice~., trainNnet, method = "nnet", trControl = ctrl, trace=TRUE, maxit=50000, MaxNWts = 50000, linout = 1)


neuralnetwork$results

neuralnetwork$results[neuralnetwork$results$MAE == min(neuralnetwork$results$MAE), ]

model2 <- nnet(SalePrice ~ ., data = trainNnet, na.action = na.exclude, size = 3, decay = 1, linout = T, MaxNWts = 100000, maxit = 50000)


pars1 <- expand.grid(lambda = seq(0.0001, 1, 0.1), alpha = seq(0.0001, 1, 0.1))

elasticNet <- train(SalePrice~., trainNnet, method = "glmnet", trControl = ctrl, tuneGrid = pars1)

SLR <- train(SalePrice~., trainNnet, method = "lm")
SLR$results

#######
summary(trainNumeric)

trainNumeric$SalePrice <- log(trainNumeric$SalePrice)

names(trainNumeric)[names(trainNumeric) == "SalePrice"] <- "logPrice"

locPar1 <- sapply(1:length(trainNumeric[1,]), function(i) mean(na.omit(trainNumeric[,i])))
scalPar1 <- sapply(1:length(trainNumeric[1,]), function(i) sd(na.omit(trainNumeric[,i])))

trainNumeric1 <- as.data.frame(sapply(1:len, function(i) scale(trainNumeric[, i], center = locPar[i], scale = scalPar[i])))
names(trainNumeric1) <- names(trainNumeric)
trainNumeric1$logPrice <- trainNumeric$logPrice

trainNnet <- na.omit(cbind(trainNumeric1, trainNoid[,isFactor]))

pars1 <- expand.grid(lambda = seq(0, 1, 0.025), alpha = seq(0, 1, 0.025))

elasticNet_logPrice <- train(logPrice~., na.omit(trainNnet), method = "glmnet", trControl = ctrl, tuneGrid = pars1)

library(glmnetUtils)
glmnetModel <- glmnet(logPrice ~., na.omit(trainNnet), family = "gaussian", alpha = 0.025, lambda = 0.125)

write.csv(neuralnetwork$results, file = "D://Consulting Project/tutu/nnet_cv_result.csv")
