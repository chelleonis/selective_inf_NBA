#selective inferece toy example rofl

library(selectiveInference)

set.seed(15)
n     <- 500
p     <- 10
x     <- matrix(rnorm(n*p),n,p)
x[,2] <- x[,1] 
x[,4] <- x[,3]
b     <- rep(0,p)
b[1]  <- 1
b[4]  <- 1
y     <- drop(x %*% b + rnorm(n))

#Unfortunately, the selective inference methods won’t allow duplicate columns.
try(fsfit <- fs(x,y))
try(larfit <- lar(x,y))

#modificaitons to the dataset
x[,2] <- x[,1] + rnorm(n,0,0.1) 
x[,4] <- x[,3] + rnorm(n,0,0.1)
cor(x[,1],x[,2])
cor(x[,3],x[,4])
# [1] 0.9955977
# [1] 0.9952243

fsfit <- fs(x,y)
out   <- fsInf(fsfit)

plot(fsfit)

