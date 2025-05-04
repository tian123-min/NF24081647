# ======================================================================================================
# Title: Machine learning model for wheat yield and N surplus and the relative importance of their explanatory variables

# Code authors: Gang He (hegang029@nwafu.edu.cn)

# Date: 04/26/2025

# Publication: Data-driven nitrogen management benchmarks support China's wheat self-sufficiency by 2030

# Description: Establishing RF yield model and N surplus model

# ====================================================================================================== 
# Set up path
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Load data
fname = 'data.csv'
data = fread(fname)

#Random Forest model on wheat yield
data_yield <- data[,-17]
set.seed(123)
train_data <- sample(nrow(data_yield),7/10*nrow(data_yield))
train <- data_yield[train_data,]
test <- data_yield[-train_data,]
dim(train)
dim(test)
grid <- expand.grid(
  mtry = c(2, 3, 5, 7), 
  splitrule = c("variance", "extratrees"), 
  min.node.size = c(1, 5, 10) )
control <- trainControl(method = "cv", number = 10)
model <- train(
  Yield ~ .,  
  data = data_yield,
  method = "ranger",
  trControl = control,
  tuneGrid = grid,
  importance = 'permutation',
  respect.unordered.factors = TRUE,
  num.trees = 500,
  keep.inbag = TRUE)
print(model$bestTune)

set.seed(123)
rf_yield = ranger(dependent.variable.name = "Yield",
                  data = data_yield,
                  importance = 'permutation',
                  mtry = 5,
                  min.node.size = 1,
                  respect.unordered.factors = T, 
                  num.trees = 500, 
                  keep.inbag = T, 
                  oob.error = T)
# Determination of coefficient (R squared) by out of bag and importance of variables
print(rf_yield)
importance <- rf_yield$variable.importance
total <- sum(importance)
importance_normalized <- (importance/ total) * 100
importance_sorted <- sort(importance_normalized, decreasing =T)
barplot(importance_sorted)

#Gradient boosting machine on wheat yield
h2o.init()
independent<-colnames(train)[1:15]
dependent<-colnames(train)[16]
amvtrain_h2o<-as.h2o(train)
set.seed(123)
gbm_h2o<-h2o.gbm(x=independent,y=dependent,
                 training_frame = amvtrain_h2o,
                 distribution = "AUTO"
)

data_test<-test[,c(1:15)]
data_pred<-data_yield[,c(1:15)]
amvtest_h2o<-as.h2o(data_test)
amvvali_h2o<-as.h2o(data_pred)

gbm_lm<-lm(as.vector(h2o.predict(gbm_h2o,amvvali_h2o))~data_yield$Yield)
gbm_lm$coefficients
summary(gbm_lm)$r.squared
sqrt(mean(as.vector(h2o.predict(gbm_h2o,amvvali_h2o))-data_yield$Yield)^2)

pred_e <- h2o.predict(gbm_h2o,amvvali_h2o)
pred_n<-as.data.frame(pred_e)  
pred_t<-data.frame(data_yield, pred_n)
plot(pred_t$Yield,pred_t$predict)

# Support vector machine on wheat yield
svm_yield <- svm(Yield ~.,data=train,kernel="radial",
                 epsilon=0.1)
print(svm_yield)
data_pred <- predict(svm_yield, data_yield)
data <-data.frame(data_yield, data_pred)
plot(data$Yield,data$data_pred)

# multi-layer perceptron on wheat yield
set.seed(123)
AMV_MLP<-normalizeData(data_yield,type = "0_1")
Index <- data_yield$Yield;
Index_norm<-normalizeData(data_yield$Yield,type = "0_1")
normpara<-getNormParameters(Index_norm)
data_MLP <- data.frame(AMV_MLP,Index_norm,data_yield$Yield)

set.seed(123)
train_data <- sample(nrow(data_MLP),7/10*nrow(data_MLP))
train <- data_MLP[train_data,] 
test <- data_MLP[-train_data,]
dim(train)
dim(test)

set.seed(123)
mlp<-mlp(train[,1:15],train[,16])
print(mlp)
summary(mlp)

amvtest_mlp <- data_MLP[,c(1:15)]
AMV_MLP_p<-AMV_MLP[,c(1:15)]
mlp_lm<-lm(denormalizeData(predict(mlp,AMV_MLP_p),normpara)
           ~denormalizeData(Index_norm,normpara))

mlp_lm$coefficients
summary(mlp_lm)$r.squared

sqrt(mean(denormalizeData(predict(mlp,amvtest_mlp),normpara)
          -denormalizeData(data_MLP$Index_norm,normpara))^2)

AMV_pred <-denormalizeData(predict(mlp,amvtest_mlp), normpara)

data <-data.frame(data_yield, AMV_pred)
plot(data$Yield,data$AMV_pred)


#Random Forest model on N surplus
fname = 'data.csv'
data = fread(fname)
data_N <- data[,-16]
set.seed(123)
train_data <- sample(nrow(data_N),7/10*nrow(data_N))
train <- data_N[train_data,]
test <- data_N[-train_data,]
dim(train)
dim(test)

grid <- expand.grid(
  mtry = c(2, 3, 5, 7), 
  splitrule = c("variance", "extratrees"), 
  min.node.size = c(1, 5, 10) )
control <- trainControl(method = "cv", number = 10)
model <- train(
  N_surplus ~ .,  
  data = data_N,
  method = "ranger",
  trControl = control,
  tuneGrid = grid,
  importance = 'permutation',
  respect.unordered.factors = TRUE,
  num.trees = 500,
  keep.inbag = TRUE)
print(model$bestTune)
set.seed(123)
rf_N = ranger(dependent.variable.name = "N_surplus",
              data = data_N,
              importance = 'permutation',
              mtry = 5,
              min.node.size = 1,
              respect.unordered.factors = T, 
              num.trees = 1000, 
              keep.inbag = T, 
              oob.error = T)
# Determination of coefficient (R squared) by out of bag and importance of variables
print(rf_N)
importance <- rf_N$variable.importance
total <- sum(importance)
importance_normalized <- (importance/ total) * 100
importance_sorted <- sort(importance_normalized, decreasing =T)
barplot(importance_sorted)

#Gradient boosting machine on N surplus
independent<-colnames(train)[1:15]
dependent<-colnames(train)[16]
amvtrain_h2o<-as.h2o(train)
set.seed(123)
gbm_h2o<-h2o.gbm(x=independent,y=dependent,
                 training_frame = amvtrain_h2o,
                 distribution = "AUTO"
)

data_test<-test[,c(1:15)]
data_pred<-data_N[,c(1:15)]
amvtest_h2o<-as.h2o(data_test)
amvvali_h2o<-as.h2o(data_pred)

gbm_lm<-lm(as.vector(h2o.predict(gbm_h2o,amvvali_h2o))~data_N$N_surplus)
gbm_lm$coefficients
summary(gbm_lm)$r.squared
sqrt(mean(as.vector(h2o.predict(gbm_h2o,amvvali_h2o))-data_N$N_surplus)^2)

pred_e <- h2o.predict(gbm_h2o,amvvali_h2o)
pred_n<-as.data.frame(pred_e)  
pred_t<-data.frame(data_N, pred_n)
plot(pred_t$N_surplus,pred_t$predict)

# Support vector machine on wheat yield
svm_N <- svm(N_surplus ~.,data=train,kernel="radial",
                 epsilon=0.1)
print(svm_N)
data_pred <- predict(svm_N, data_N)
data <-data.frame(data_N, data_pred)
plot(data$N_surplus,data$data_pred)

# multi-layer perceptron on wheat yield
set.seed(123)
AMV_MLP<-normalizeData(data_N,type = "0_1")
Index <- data_N$N_surplus;
Index_norm<-normalizeData(data_N$N_surplus,type = "0_1")
normpara<-getNormParameters(Index_norm)
data_MLP <- data.frame(AMV_MLP,Index_norm,data_N$N_surplus)

set.seed(123)
train_data <- sample(nrow(data_MLP),7/10*nrow(data_MLP))
train <- data_MLP[train_data,] 
test <- data_MLP[-train_data,]
dim(train)
dim(test)

set.seed(123)
mlp<-mlp(train[,1:15],train[,16])
print(mlp)
summary(mlp)

amvtest_mlp <- data_MLP[,c(1:15)]
AMV_MLP_p <- AMV_MLP[,c(1:15)]
mlp_lm<-lm(denormalizeData(predict(mlp,AMV_MLP_p),normpara)
           ~denormalizeData(Index_norm,normpara))

mlp_lm$coefficients
summary(mlp_lm)$r.squared

sqrt(mean(denormalizeData(predict(mlp,amvtest_mlp),normpara)
          -denormalizeData(data_MLP$Index_norm,normpara))^2)

AMV_pred <-denormalizeData(predict(mlp,amvtest_mlp), normpara)

data <-data.frame(data_N, AMV_pred)
plot(data$N_surplus,data$AMV_pred)





