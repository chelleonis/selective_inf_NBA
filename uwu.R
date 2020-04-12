#analysis step:
#load in all the data from initial analysis

library(selectiveInference)
library(future)
library(glmnet)
library(mice)
#C_data_full, PF_data, SF_data, SG_data, PG_data
#start with centers


#FIXED LASSO
set.seed(13)
sigma = 1 #noise sd
n = 100 # No. of observations
p = 500 # No. of covariates
s = 5 #sparsity index
X = matrix(rnorm(n * p), n, p)  # Design Matrix
X = scale(X, TRUE, TRUE) # Standardize X

beta = c(5:1, rep(0, p - s))/10 # True coefficient
Y = X %*% beta + rnorm(n) #Response


lamb = sigma * sqrt(2.05 * log(p)/n) # Lasso Tuning parameter
mod = glmnet(x = X, y = Y,intercept = FALSE) # Fit the lasso
beta_hat = coef(mod, x = X, y = Y, s = lamb, exact = TRUE)[-1] # Coef. Estimates

CI = fixedLassoInf(y = Y, x = X, sigma = 1, beta = beta_hat, lambda = n * lamb) # Selective Inference - Note the Tuning Paramter!
print(CI)
print(beta_hat[1:5])

X = as.matrix(sapply(C_data_full[,-1],as.numeric))
Y = as.matrix(C_data_full[,1])

sigma = 1 #noise sd
n = length(Y)
p = dim(X)[2]
lamb = sigma*2.05*p/n
#tuning paramter choice

#fit the lasso
mod = glmnet(x = X, y = Y,intercept = FALSE)
beta_hat =  coef(mod, x = X, y = Y, s = 50, exact = TRUE)[-1]
CI = fixedLassoInf(y = Y, x = X, lambda = 20, beta = beta_hat)


sigma = 1 #noise
n = 48 # No. of observations
p = 27 # No. of covariates
s = 5 #sparsity index
betas = c(5:1, rep(0, p - s))/10

#fit the lasso
lamb = sigma * sqrt(2.05 * log(p)/n) # Lasso Tuning parameter
mod = glmnet(x = Xtest, y = Ytest,intercept = FALSE) # Fit the lasso
beta_hat = coef(mod, x = Xtest, y = Ytest, s = 500, exact = TRUE)[-1] # Coef. Estimates

#selective inference step
CI = fixedLassoInf(y = Ytest, x = Xtest, lambda = 50, beta = beta_hat)

#Forward Selection



#


#Least Angle Regression



#P-value correction:


#






X = as.matrix(sapply(overall_data[,-1],as.numeric))
Y = as.matrix(overall_data[,1])

Xtest = as.matrix(sapply(overall_data_test[,-1],as.numeric))
Ytest = as.matrix(overall_data_test[,1])

sigma = 1 #noise
n = 48 # No. of observations
p = 27 # No. of covariates
s = 5 #sparsity index
betas = c(5:1, rep(0, p - s))/10

#fit the lasso
lamb = sigma * sqrt(2.05 * log(p)/n) # Lasso Tuning parameter
mod = glmnet(x = Xtest, y = Ytest,intercept = FALSE) # Fit the lasso
beta_hat = coef(mod, x = Xtest, y = Ytest, s = 500, exact = TRUE)[-1] # Coef. Estimates

#selective inference step
CI = fixedLassoInf(y = Ytest, x = Xtest, lambda = 50, beta = beta_hat)

larfit = lar(Xtest,Ytest)
CI_2 = larInf(larfit)


CI = fixedLassoInf(y = Y, x = X, sigma = 1, beta = beta_hat, lambda = n * lamb)

#forwardstepinf(x,y) forward  stepwise regression (AIC stoppign rule)
#fixedLassoinf(x,y) fixed lambda lasso 
#larInf(x,y)

#fsfit (foward sleection)


OLS_step <- lm(twoK_score ~. , data = C_data_full)
p_values <- coef(summary(OLS_step))[,"Pr(>|t|)"]

p.adjust(p_values, method = p.adjust.methods, n = length(p))

fsfit <- fs(X,Y)
out   <- fsInf(fsfit)

plot(fsfit)

larfit = lar(X,Y)
CI_3 = larInf(larfit)

