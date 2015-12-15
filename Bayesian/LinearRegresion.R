
data(iris)

X <- data.frame(iris$Sepal.Width, iris$Petal.Length, iris$Petal.Width) 
Y <- data.frame(iris$Sepal.Length)

library(ggplot2)
prior.mean <- function(x) dnorm(x, 3, .2)
prior.sd <- function(x) dgamma(x, 5, 40)
plot(prior.mean, col="darkblue", xlim=c(-5,18), lwd="2", main="Prior for mean", ylab="density")
plot(prior.sd , col="darkred", xlim=c(0,1), lwd="2", main="Prior for standard deviation", ylab="density")

#lm

objdens <- function(data, theta){
  m=nrow(data)
  print(theta)
  # Compute loglikelihood
  lkh=0
  for(i in 1:m) {
    lkh = lkh + -.5*((X[i,]-theta[1])/theta[2])^2-log(theta[2])
    print(lkh)
  }
  # Compute logprior
  
  logprior = dnorm(theta[1], 3.0,.5) + dgamma(theta[2], 5.0, 40.0)
  # Log of target density
  lkh + logprior
}

val <- objdens(X, c(2,3))
