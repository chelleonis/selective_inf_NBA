#lol

library(selectiveInference)
library(nbastatR)
library(future)
library(glmnet)

library(dplyr)

plan(multiprocess)

test <- game_logs(seasons = 2018, league = "NBA")

# nba 2k player rating
twoK <- read.csv("C:/Users/typer321/Desktop/nba2k ratings.csv") %>% filter(Rank < 50) %>% 
  rename(namePlayer = Player)

players_careers(players = twoK$Player,
                           modes = c("Totals", "PerGame")) #career data and season data

#need to correct some names

# variable sets:
# draft combine (TBD), career numbers(?), season numbers (slugSeason: 2018-2019)
#DoF respectively: 


#need to change these titels lol
set1 <- dataPlayerSeasonTotalsRegularSeason %>% filter(slugSeason == "2018-19") %>%
  dplyr::select(-one_of("slugSeason","slugTeam","idTeam","urlNBAAPI","idPlayer","slugSeasonType", "isRookie"))

set2 <- dataPlayerSeasonTotalsPostSeason %>% filter(slugSeason == "2018-19") %>%
  dplyr::select(-one_of("slugSeason","slugTeam","idTeam","urlNBAAPI","idPlayer","slugSeasonType", "isRookie"))

set3 <- dataPlayerCareerTotalsRegularSeason %>%
  dplyr::select(-one_of("urlNBAAPI","idPlayer","slugSeasonType"))

set4 <- dataPlayerCareerTotalsPostSeason %>%
  dplyr::select(-one_of("urlNBAAPI","idPlayer","slugSeasonType"))
 
overall_data <- inner_join(twoK, set1, by = "namePlayer") %>% 
  inner_join(.,set2, by = "namePlayer") %>% 
  inner_join(.,set3, by = "namePlayer") %>% 
  inner_join(.,set4, by = "namePlayer") %>% 
  rename(twoK_score = OVR) %>%
  dplyr::select(-one_of("Pos.","Rank","Team","Build","namePlayer"))

overall_data[is.na(overall_data)] <- 0
  
overall_data_test <- inner_join(twoK, set1, by = "namePlayer") %>% 
  rename(twoK_score = OVR) %>%
  dplyr::select(-one_of("Pos.","Rank","Team","Build","namePlayer","Height"))

#need to do stuff with lambda

#X = matrix(rnorm(n * p), n, p)  # Design Matrix

overall_data_test[is.na(overall_data_test)] <- 0

X = as.matrix(sapply(overall_data[,-1],as.numeric))
Y = as.matrix(overall_data[,1])

#can probably multiple impute the data using MICE)

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

OLS_step <- lm(twoK_score ~ . , data = overall_data_test)

