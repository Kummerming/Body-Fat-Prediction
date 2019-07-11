setwd("C://Doc/19spring/STAT628/Module1")
rm(list=ls())


#data cleaning
BodyFat = read.table("BodyFat.csv", sep = ",", header = T)
BodyFat = BodyFat[,-1]
BodyFat = BodyFat[,-2]
BodyFat$HEIGHT[42] <- 69.5
BodyFat$WEIGHT[221] <- 173.25
BodyFat$WEIGHT[163] <- 164.25
BodyFat <- BodyFat[-c(182,172),]




require(plotmo)
library(glmnet)
#LASSO normalization
normalization <- function(datafr){
  if(ncol(datafr) > 1){
    new_dt <- as.data.frame(sapply(1:ncol(datafr), FUN = function(x)
      (datafr[,x]-min(datafr[,x]))/(max(datafr[,x]-min(datafr[,x])))))
    names(new_dt) <- names(datafr)
    return(new_dt)
  }else{
    return((datafr-min(datafr))/(max(datafr)-min(datafr)))
  }
}
unnormalization <- function(data_norm, data_raw){
  ###data_norm is the data before normalization, data_raw is the data after
  ###normalization
  if(ncol(data_raw) == 1){
    data_trans <- data_raw*(max(data_norm)-min(data_norm))+min(data_norm)
    return(data_trans)
  }
  if(!(ncol(data_norm) == ncol(data_raw))){
    stop("Size does not match")
  }
  if(ncol(data_norm)>1){
    data_trans = as.data.frame(sapply(1:ncol(data_raw), FUN = function(x) 
      data_raw[,x]*(max(data_norm)-min(data_norm))+min(data_norm)))
    names(data_trans) <- names(data_raw)
    return(data_trans)
  }
}

BodyFat = BodyFat[,c("BODYFAT","AGE","WEIGHT","ABDOMEN","HEIGHT","KNEE","THIGH","ANKLE","BICEPS","FOREARM","WRIST")]
corrplot::corrplot(cor(BodyFat))
#data separation
all_row = 1:nrow(BodyFat)
CV = sample(nrow(BodyFat),nrow(BodyFat)*0.2)
test = sample(all_row[!(all_row %in% CV)],nrow(BodyFat)*.2)
train = all_row[!((all_row %in% CV) | (all_row %in% test))]
CV = BodyFat[CV,]
test = BodyFat[test,]
train = BodyFat[train,]
CV_norm <- normalization(CV)
test_norm <- normalization(test)
train_norm <- normalization(train)
#Training lasso reg
log_lambda_range <- seq(-10,-1,0.01)
MSE_train=c()
MSE_CV = c()
for(log_lambda in log_lambda_range){
  fit.lasso <- glmnet(x=model.matrix(~.,train_norm[,-1]), y=train_norm[,1],
                      family="gaussian", alpha=1, lambda = exp(log_lambda))
  y_pred <- predict(fit.lasso, newx = model.matrix(~.,train_norm[,-1]))
  train_error = y_pred - train_norm[,1]
  MSE_train <- c(MSE_train, mean(train_error^2))
  y_pred <- predict(fit.lasso, newx = model.matrix(~.,CV_norm[,-1]))
  CV_error = y_pred - CV_norm[,1]
  MSE_CV = c(MSE_CV, mean(CV_error^2))
}
plot(log_lambda_range, MSE_train, col = 'red', type="line", xlab = "log_lambda",
     ylim=c(min(c(MSE_train,MSE_CV)),max(c(MSE_train,MSE_CV))))
lines(log_lambda_range, MSE_CV, col = "blue", type="line")
legend("top", legend = c("Train", "CV"), col = c("red","blue"), lty=1)
#plot model
model <- glmnet(x=model.matrix(~.,train_norm[,-1]), y=train_norm[,1],
                family="gaussian", alpha=1, lambda = exp(log_lambda_range))
plotmo::plot_glmnet(model)
#lasso_cv
cv_model = cv.glmnet(x=model.matrix(~.,train_norm[,-1]), y=train_norm[,1],
                     family="gaussian", alpha=1, lambda = exp(log_lambda_range),
                     nfolds=10)
plot(cv_model)
#Final Model
rmse=c()
for(iter in 1:200){
  all_row = 1:nrow(BodyFat)
  test = sample(all_row,nrow(BodyFat)*.2)
  train = all_row[!((all_row %in% test))]
  test = BodyFat[test,]
  train = BodyFat[train,]
  test_norm <- normalization(test)
  train_norm <- normalization(train)
  
  best_model <- glmnet(x=model.matrix(~.,train_norm[,-1]), y=train_norm[,1],
                       family="gaussian", alpha=1, lambda = exp(-3.9))
  best_model$beta
  test_y <- predict(best_model, newx =  model.matrix(~.,test_norm[,-1])) 
  test_y <- unnormalization(test$BODYFAT,test_y)
  test_error <- test[,1]-test_y
  rmse = c(rmse,sqrt(mean(test_error^2)))
}
BodyFat_norm <- normalization(BodyFat)
mo <- glmnet(x=model.matrix(~.,BodyFat_norm[,-1]), y=BodyFat_norm[,1],
             family="gaussian", alpha=1, lambda = exp(-4.5))
BodyFat_y <- predict(mo, newx =  model.matrix(~.,BodyFat_norm[,-1])) 
BodyFat_y <- unnormalization(BodyFat$BODYFAT,BodyFat_y)
BodyFat_error <- BodyFat[,1]-t(BodyFat_y)
plot(BodyFat_error,BodyFat$BODYFAT)


#residual Analysis
train_y <- predict(best_model, newx =  model.matrix(~.,train_norm[,-1])) 
train_y <- unnormalization(train$BODYFAT, train_y)
res <- train$BODYFAT-train_y
plot(x=train_y,y=res)
qqnorm(res/sd(res))
plot(x=train_y,y=res/sd(res))
