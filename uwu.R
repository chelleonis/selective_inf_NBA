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
no_height_test <- PF_data_full %>% dplyr::select(-c("m_height")) %>%
  mutate(start_pct = gs/gp) %>%
  mutate(start_pct = gs_regcar/gp_regcar) %>%
  dplyr::select(-c("gp","gs","gs_regcar","gp_regcar"))


susume <- function (pos_data) {
  X = as.matrix(sapply(pos_data[,-1],as.numeric))
  Y = as.matrix(pos_data[,1], drop = FALSE)
  fsfit <- fs(X,Y)
  out   <- fsInf(fsfit)
  plot(fsfit)
  return(out)
}




#


#Least Angle Regression
larfit = lar(X,Y)
CI_3 = larInf(larfit)

fixedLassoInf(X,Y,grid_range)



#P-value correction:


#


#forwardstepinf(x,y) forward  stepwise regression (AIC stoppign rule)
#fixedLassoinf(x,y) fixed lambda lasso 
#larInf(x,y)

#fsfit (foward sleection)


OLS_step <- lm(twoK_score ~. , data = C_data_full)
p_values <- coef(summary(OLS_step))[,"Pr(>|t|)"]

p.adjust(p_values, method = p.adjust.methods, n = length(p))

fsfit <- fs(X_2,Y)
out   <- fsInf(fsfit)

plot(fsfit)


