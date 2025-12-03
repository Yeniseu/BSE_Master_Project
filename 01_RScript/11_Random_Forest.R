# Author: ...
# Date: 26/11/2025
# Scope: Apply Random Forest

library(data.table)
library(randomForest)

rm(list = ls())
options(print.max = 300, scipen = 30, digits = 5)

#### Load & prepare ####

fred <- readRDS("02_Input/data_cleaned.rds")
setDT(fred)
dim(fred)

setnames(fred, "CPIAUCSL", "inf")
setcolorder(fred, c("date", "inf"))

#### FUNCTIONS ####

# rf function
runrf=function(Y,indice,lag){
  comp=princomp(scale(Y,scale=FALSE))
  Y2=cbind(Y,comp$scores[,1:4])
  aux=embed(Y2,4+lag)
  y=aux[,indice]
  X=aux[,-c(1:(ncol(Y2)*lag))]  
  
  if(lag==1){
    X.out=tail(aux,1)[1:ncol(X)]  
  }else{
    X.out=aux[,-c(1:(ncol(Y2)*(lag-1)))]
    X.out=tail(X.out,1)[1:ncol(X)]
  }
  
  #browser()
  model=randomForest(X,y,importance=TRUE)
  pred=predict(model,X.out)
  
  return(list("model"=model,"pred"=pred))
}

# rolling window setting
rf.rolling.window=function(Y,nprev,indice=1,lag=1){
  
  save.importance=list()
  save.pred=matrix(NA,nprev,1)
  for(i in nprev:1){
    Y.window=Y[(1+nprev-i):(nrow(Y)-i),]
    lasso=runrf(Y.window,indice,lag)
    save.pred[(1+nprev-i),]=lasso$pred
    save.importance[[i]]=importance(lasso$model)
    cat("iteration",(1+nprev-i),"\n")
  }
  
  real=Y[,indice]
  plot(real,type="l")
  lines(c(rep(NA,length(real)-nprev),save.pred),col="red")
  
  rmse=sqrt(mean((tail(real,nprev)-save.pred)^2))
  mae=mean(abs(tail(real,nprev)-save.pred))
  errors=c("rmse"=rmse,"mae"=mae)
  
  return(list("pred"=save.pred,"errors"=errors,"save.importance"=save.importance))
}


#### PREDICTIONS ####

# First Out of Sample Predictions: 
# Out of Sample Period : 2001-2015

fred <- fred[date < "2001-01-01"]
dim(fred)

set.seed(123)
rf1c=rf.rolling.window(Y,nprev,1,1)






dt<-data.table(x=c(1,2,3),y=c(2,3,4),z=c(3,4,5))
dt
dt[, 1:2]
aux=embed(Y2,4+lag)


fre <- fred
Y <- fre[, 1:5]
Y
Y <- Y[, -1]
Y <- as.matrix(Y)
Y2 <- Y

comp=princomp(scale(Y,scale=FALSE))

Y2=cbind(Y,comp$scores[,1:4])
aux=embed(Y2,2)
tail(Y2, 5)
tail(aux, 5)
y=aux[,indice]
X=aux[,-c(1:(ncol(Y2)*lag))]  

if(lag==1){
  X.out=tail(aux,1)[1:ncol(X)]  
}else{
  X.out=aux[,-c(1:(ncol(Y2)*(lag-1)))]
  X.out=tail(X.out,1)[1:ncol(X)]
}