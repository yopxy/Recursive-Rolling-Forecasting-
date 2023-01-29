#Import Library
library("readxl")
library(glmnet)
library(Matrix)
library(tidyverse)
library(dplyr)
library(Metrics)
library(caret)

#load data
data <- read_excel("swemacroupdated.xlsx")
mydata =data [,-1] # remove first coumn with date

x <- as.matrix(mydata)
y <- as.matrix(mydata[,2]) #inflation variable

# #splitting data 50/50 
 x_train <- as.matrix(x[1:199,])
 y_train <- as.matrix(y[1:199,])
 train.data = cbind(x_train, y_train)
 #View(train.data)
# 
 x_test <- as.matrix(x[200:474,])
 y_test <- as.matrix(y[200:474, ])
 test.data = cbind(x_test, y_test)
# #View(test.data)

# Ridge , alpha = 0
 Inflation.ridgeCV = cv.glmnet(x=x_train,
                                   y=y_train,alpha=0, nfolds=10)
 
#choose the lambda from averaging of the RSS(lambda_10-fold) functions. 
 lambdaCV.ridge = Inflation.ridgeCV$lambda.min ## 0.135
 Inflation.ridge = glmnet(x=x_train, y=y_train,
                                   alpha=0, lambda=lambdaCV.ridge)
                          
as.numeric(coefficients(Inflation.ridge))
               
y_test.prediction = predict(Inflation.ridge, newx = x_test)
                    

ridge_loss_MSE <- matrix(NA, nrow = length(200:474), ncol = 12)
ridge_loss_MAE <- matrix(NA, nrow = length(200:474), ncol = 12)

#for loop for Rolling forecast at each step(months) that forecast 
#12 months ahead. e.g at January, it will do rolling forecast at 
#200-212 forecast 12 month ahead, at 201 it forecat 12 months by
#rolling 201-213
#and at february it will aslo forrecast 12 month ahead...and it
#continue like dat till 12month which is december.

for (h in c(1:12)){
  for (t in c(200:474)){
    as.numeric(cat(h, " step at ", t," forcast from", t+1, " to ",  t+12, "\n" )) 
    xtrain_t <- x[1:(t-1),]
    ytrain_t <- y[(1+h):(t+h-1),]
    xtest_t <-  x[t,]
    y_test_t <- y[t+h,]
    xtestlamda = cv.glmnet(xtrain_t ,
                           ytrain_t,alpha=0, nfolds=10)
    lambdatestCV.ridge = xtestlamda$lambda.min
    xtestlamda.ridge = glmnet(xtrain_t, ytrain_t,
                              alpha=0, lambda=lambdatestCV.ridge)
    as.numeric(coefficients(xtestlamda.ridge))
    y_test.prediction = predict(xtestlamda.ridge, newx = xtest_t)
    
    ridge_loss_MSE[t-199,h] <-  as.numeric((y_test_t - y_test.prediction)^2)
    ridge_loss_MAE[t-199,h] <-  as.numeric(abs(y_test_t - y_test.prediction))
    #print(ridge_loss[t-199,])
  } 
  
}

colMeans(ridge_loss_MSE) #MSE
colMeans(ridge_loss_MAE) #MAE

#Things to do here, I will like to have graph that can show
#the predicton and the actual  graph or any grapgh that can tell 
#the data story

#LASSO
#Lasso ; alpha = 1
Inflation.lassoCV = cv.glmnet(x=x_train,
                              y=y_train,alpha=1, nfolds=10)

##choose the lambda from averaging of the RSS(lambda_10-fold) functions. 
lambdaCV.lasso = Inflation.lassoCV$lambda.min #0.039

Inflation.lasso = glmnet(x=x_train, y=y_train,
                         alpha=1, lambda=lambdaCV.lasso)
as.numeric(coefficients(Inflation.lasso))
y_test.prediction = predict(Inflation.lasso, newx = x_test)


lasso_loss_MSE <- matrix(NA, nrow = length(200:474), ncol = 12)
lasso_loss_MAE <- matrix(NA, nrow = length(200:474), ncol = 12)

for (h in c(1:12)){
  for (t in c(200:474)){
    as.numeric(cat(h, " step at ", t," forcast from", t+1, " to ",  t+12, "\n" )) 
    xtrain_t <- x[1:(t-1),]
    ytrain_t <- y[(1+h):(t+h-1),]
    xtest_t <-  x[t,]
    y_test_t <- y[t+h,]
    xtestlamda = cv.glmnet(xtrain_t ,
                           ytrain_t,alpha=1, nfolds=10)
    lambdatestCV.lasso = xtestlamda$lambda.min
    xtestlamda.lasso = glmnet(xtrain_t, ytrain_t,
                              alpha=1, lambda=lambdatestCV.lasso)
    as.numeric(coefficients(xtestlamda.lasso))
    y_test.prediction = predict(xtestlamda.lasso, newx = xtest_t)
    
    lasso_loss_MSE[t-199,h] <-  as.numeric((y_test_t - y_test.prediction)^2)
    lasso_loss_MAE[t-199,h] <-  as.numeric(abs(y_test_t - y_test.prediction))
    #print(lasso_loss[t-199,])
  } 
  
}

colMeans(lasso_loss_MSE) #MSE
colMeans(lasso_loss_MAE) #MAE

colMeans(ridge_loss_MSE)
colMeans(ridge_loss_MAE)

cbind(colMeans(ridge_loss_MSE), colMeans(lasso_loss_MSE), #Ridge and lasso MSE
      colMeans(ridge_loss_MSE)- colMeans(lasso_loss_MSE) )#Ridge - lasso MSE

cbind(colMeans(ridge_loss_MAE), colMeans(lasso_loss_MAE), #Ridge and lasso MAE
      colMeans(ridge_loss_MAE)- colMeans(lasso_loss_MAE))
#Also here, I will like to have a graph of prediction and actual 
# or other graph that can tell data story


#Now this elastic net, my supervisor asked me to use a package
#in R that can select lambda and alpha automatically, 
#i dont know how to go about it, i just set alpha to 0.5 but my 
#result is close to lasso. I also need you to help with this
#Elastic net
#alpha = 0.5 
#########################################

Inflation.elasticnetCV = cv.glmnet(x=x_train,
                              y=y_train,alpha=0.5, nfolds=10)

##choose the lambda from averaging of the RSS(lambda_10-fold) functions. 
lambdaCV.elasticnet = Inflation.elasticnetCV$lambda.min #0.034

Inflation.elasticnet = glmnet(x=x_train, y=y_train,
                         alpha=0.5, lambda=lambdaCV.elasticnet)
as.numeric(coefficients(Inflation.elasticnet))
y_test.prediction = predict(Inflation.elasticnet, newx = x_test)

elastic_loss_MSE <- matrix(NA, nrow = length(200:474), ncol = 12)
elastic_loss_MAE <- matrix(NA, nrow = length(200:474), ncol = 12)

for (h in c(1:12)){
  for (t in c(200:474)){
    as.numeric(cat(h, " step at ", t," forcast from", t+1, " to ",  t+12, "\n" )) 
    xtrain_t <- x[1:(t-1),]
    ytrain_t <- y[(1+h):(t+h-1),]
    xtest_t <-  x[t,]
    y_test_t <- y[t+h,]
    xtestlamda = cv.glmnet(xtrain_t ,
                           ytrain_t,alpha=0.5, nfolds=10)
    lambdatestCV.elasticnet = xtestlamda$lambda.min
    xtestlamda.elasticnet = glmnet(xtrain_t, ytrain_t,
                              alpha=0.5, lambda=lambdatestCV.elasticnet)
    as.numeric(coefficients(xtestlamda.elasticnet))
    y_test.prediction = predict(xtestlamda.elasticnet, newx = xtest_t)
    
    elastic_loss_MSE[t-199,h] <-  as.numeric((y_test_t - y_test.prediction)^2)
    elastic_loss_MAE[t-199,h] <-  as.numeric(abs(y_test_t - y_test.prediction))
    #print(elastic_loss_MSE[t-199,])
  } 
  
}
colMeans(elastic_loss_MSE) #MSE
colMeans(elastic_loss_MAE) #MAE

#Also, I will like to have a graph that can tell the story.
# I also need P values for statistical significance






