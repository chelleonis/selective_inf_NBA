#analysis step:
#load in all the data from initial analysis

library(selectiveInference)
library(glmnet)

#C_data_full, PF_data, SF_data, SG_data, PG_data
#start with centers
mod = glmnet(x = X_2, y = Y,intercept = FALSE, standardize = FALSE)
beta_hat =  coef(mod, x = X_2, y = Y, s = lamb/n, exact = TRUE)[-1]
beta_hat

CI = fixedLassoInf(y = Y, x = X_2, lambda = lamb , beta = beta_hat)

howdy_partner <- function(pos_data) {
  sigma = 1 # noise sd
  X = as.matrix(sapply(pos_data[,-1],as.numeric))
  X <- scale(X,TRUE,TRUE)
  Y = as.matrix(pos_data[,1], drop = FALSE)
  Y <- scale(Y, FALSE,TRUE)
  n = length(Y) #number of observations
  p = dim(X)[2] #number of covariates
  lamb = sigma * sqrt(2.05 * log(p)/n) #tuning param
  mod = glmnet(x = X, y = Y, intercept= FALSE)
  beta_hat = coef(mod, x = X, y = Y, s = lamb/n, exact = TRUE)[-1]
  print(beta_hat)
  CI = fixedLassoInf(y = Y, x = X, lambda = lamb, beta = beta_hat)
  return(CI)
}

C_las <- howdy_partner(C_data)

PF_las <- howdy_partner(PF_data)

SF_las <- howdy_partner(SF_data)

SG_las <- howdy_partner(SG_data)

PG_las <- howdy_partner(PG_data)


#Forward Selection

susume <- function (pos_data) {
  X = as.matrix(sapply(pos_data[,-1],as.numeric))
  Y = as.matrix(pos_data[,1], drop = FALSE)
  X <- scale(X,TRUE,TRUE)
  Y <- scale(Y,FALSE,TRUE)
  fsfit <- fs(X,Y)
  out   <- fsInf(fsfit)
  plot(fsfit)
  return(out)
}

C_fwd <- susume(C_data)
PF_fwd <- susume(PF_data)
SF_fwd <- susume(SF_data)
SG_fwd <- susume(SG_data)
PG_fwd <- susume(PG_data)

X = as.matrix(sapply(SG_data[,-1],as.numeric))
Y = as.matrix(SG_data[,1], drop = FALSE)
X <- scale(X,TRUE,TRUE)
Y <- scale(Y,FALSE,TRUE)
fsfitty <- fs(X,Y)
out   <- fsInf(fsfitty)

n = length(Y) #number of observations
p = dim(X)[2] #number of covariates
lamb = sigma * sqrt(2.05 * log(p)/n) #tuning param

betas = coef(out, s = lamb/n, mode = "lambda")

out2 = fixedLassoInf(X, Y, betas, lamb/n, sigma=sigma)
out2


#lawr
#Least Angle Regression with p-value correction(?)

larry <- function (pos_data) {
  X = as.matrix(sapply(pos_data[,-1],as.numeric))
  Y = as.matrix(pos_data[,1], drop = FALSE)
  X <- scale(X,TRUE,TRUE)
  Y <- scale(Y,FALSE,TRUE)
  larfit <- lar(X,Y)
  out   <- larInf(larfit)
  plot(larfit)
  return(out)
}

C_lar <- larry(C_data)
PF_lar <- larry(PF_data)
SF_lar <- larry(SF_data)
SG_lar <- larry(SG_data)
PG_lar <- larry(PG_data)




## as above, but use lar function instead to get initial 
## lasso fit (should get same results)
lfit = lar(X,Y)

beta = coef(lfit, s=lamb/n, mode="lambda")

out2 = fixedLassoInf(X, Y, beta, lamb, sigma=sigma)

out2


#P-value correction:


OLS_step <- lm(twoK_score ~. -1, data = C_data)
summary(OLS_step)
p_values <- coef(summary(OLS_step))[,"Pr(>|t|)"]

p.adjust(p_values, method = p.adjust.methods)

OLS_step <- lm(twoK_score ~. -1, data = C_data)
summary(OLS_step)
p_values <- coef(summary(OLS_step))[,"Pr(>|t|)"]

