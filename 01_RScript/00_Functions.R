
runlasso <- function(Y, indice, lag, alpha=1, type="lasso", lambda, learn_lambda_grid=F, nlambda=25){
  dum  <- Y[,ncol(Y)]
  Y    <- Y[,-ncol(Y)]
  comp <- princomp(scale(Y,scale=FALSE))
  Y2   <- cbind(Y,comp$scores[,1:4])
  aux  <- embed(Y2,4+lag)
  y    <- aux[,indice]
  X    <- aux[,-c(1:(ncol(Y2)*lag))]  
  
  if(learn_lambda_grid == T){
    fit_lambdas <- glmnet(X, y, alpha=alpha, standardize=T, nlambda = nlambda)
    lambda_grid <- fit_lambdas$lambda
    if(alpha == 1) {  # Remove lambdas that does not change variable count
      lambda_grid <- lambda_grid[fit_lambdas$df != shift(fit_lambdas$df, fill=-1)]  
    }
    return(lambda_grid)
  }
  
  if(lag==1){
    X.out <- tail(aux,1)[1:ncol(X)]  
  } else {
    X.out <- aux[,-c(1:(ncol(Y2)*(lag-1)))]
    X.out <- tail(X.out,1)[1:ncol(X)]
  }
  dum=tail(dum,length(y))
  if(type=="quant" || type=="quantadalasso"){
    model <- model=bicqlasso(cbind(X,dum),y,alpha = alpha)
    coef  <- model$coef
    pred  <- c(1,X.out,0)%*%coef
    if(type=="quantadalasso"){
      penalty <- (abs(coef[-1])+1/sqrt(length(y)))^(-1)
      model   <- bicqlasso(cbind(X,dum),y,penalty.factor = penalty,alpha=alpha)
      coef    <- model$coef
      pred    <- c(1,X.out,0)%*%coef
    }
    return(list("pred"=pred,"model"=list("coef"=coef)))
  }
  
  #browser()
  #model <- ic.glmnet(cbind(scale(X),dum),y,alpha = alpha)
  model <- glmnet(cbind(X,dum), y, alpha=alpha, lambda=lambda, standardize=T)
  coef  <- model$beta
  if(type=="adalasso"){
    penalty <- (abs(coef[-1])+1/sqrt(length(y)))^(-1)
    model   <- ic.glmnet(cbind(scale(X),dum),y,penalty.factor = penalty,alpha=alpha)
  }
  #browser()
  pred <- predict(model, c(X.out,0))
  return(list("model"=model,"pred"=pred))
}


get_best_lambda <- function(Y,nprev,indice=1,lag=1,alpha=1, type="lasso", nlambda=25) {
  # Create lambda grid automatically
  lambda_grid <- runlasso(Y[1:(nrow(Y)-nprev),],indice,lag,alpha,type,0,learn_lambda_grid=T, nlambda=nlambda)
  # Check the grid one by one
  save_res <- list(NA)
  for (i in 1:length(lambda_grid)) {
    save_res[[i]] <- lasso_roll_win(Y,nprev,indice,lag,alpha,type,lambda_grid[i])
  }
  # Select best lambda with lowest rmse
  rmse <- sapply(save_res, function(x) x$errors[1])
  best_lam <- lambda_grid[which.min(rmse)]
  best_lam_all <- list(best_lam = best_lam, all_res = save_res)
  return(best_lam_all)
}


lasso_roll_win <- function(Y,nprev,indice=1,lag=1,alpha=1, type="lasso", lambda) {
  save.coef <- matrix(NA,nprev,21-3+ncol(Y[,-1])*4-1 )
  save.pred <- matrix(NA,nprev,1)
  for(i in nprev:1){
    Y.window <- Y[(1+nprev-i):(nrow(Y)-i),]
    #browser()
    lasso    <- runlasso(Y.window,indice,lag,alpha,type,lambda)
    #browser()
    save.coef[(1+nprev-i),] <- as.vector(lasso$model$beta)
    save.pred[(1+nprev-i),] <- lasso$pred
    cat("iteration",(1+nprev-i),"\n")
  }
  real <- Y[,indice]
  plot(real,type="l")
  lines(c(rep(NA,length(real)-nprev),save.pred),col="red")
  
  rmse   <- sqrt(mean((tail(real,nprev)-save.pred)^2))
  mae    <- mean(abs(tail(real,nprev)-save.pred))
  errors <- c("rmse"=rmse,"mae"=mae)
  
  return(list("pred"=save.pred,"coef"=save.coef,"errors"=errors))
}
