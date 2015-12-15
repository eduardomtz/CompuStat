# clase 28-10-2015

# TODO ESTO ESTA MAL, EL ARCHIVO DEBE SER Bayesianinference.R

data(iris)
View(iris)

#hacer inferencia: pivotales, maxima inverosimilitud, etc.
#frecuentista, parametrico
#cantidaes pivotales

samp <- iris$Sepal.Width
hist(samp)
N <- length(samp)


# aproach 1: pointwise estimators and pivotal quantities
est.mean <- sum(samp)/N # mean(samp) estimador insesgado
est.sd <- sqrt(sum((samp-est.mean)^2)/(N-1)) # sd(samp) _ puede ser un estimador sesgado
# var estimador insesgado

# we can prove that (sqrt(N)/est.sd)(bar(X)-mu) ~ t-STUDENT_(N-1) degrees
# So we can make CONFIDENCE INTERVALS using the quantiles of a t-student
# NOTE: THESE ESTIMATES ONLY WORK IF WE ASSUME WE HAVE A NORMAL DISTRIBUTION
alpha <- 0.05
intervals <- c((est.mean - est.sd*qt(df=N-1, alpha/2, lower.tail = FALSE)/sqrt(N)),
               (est.mean + est.sd*qt(df=N-1, alpha/2, lower.tail = FALSE)/sqrt(N)))


# extra
# la funcion de densidad de una muestra de tamaño n
# asumir iid
# 
# aproach 2 maximum likelihood
# a) the same will work for any model
# b) model=choice of likelihood function
# c) it is better to work with LOG-likelihood
# d) for the likelihood we will only need PROPORTIONAL to, not the exact one
# so may forget any constant

cppFunction('
            double lkh(NumericVector X, NumericVector theta)
            {
  int m = X.size();
  double lkh = 0;
  for(int i=0; i<m; i++){
    lkh += -.5*pow((X[i]-theta[0])/theta[1],2)-log(theta[1]);
  }
  return lkh;
}
            ')

guess <- c(3,1) # puede ser casi lo que sea
params <- optim(guess, function(t) -lkh(samp,t))$par
params

# vamos a ver que hicimos graficamente
adjusted.dens <- function(x) dnorm(x, mean=params[1], sd=params[2])
hist(samp, prob=TRUE, ylim = c(0,1.5), main="Frequentist...")
#falta...


#BAYESIAN APPROACH

# Suppose some researchers believes from previous studies
# mu ~ N(mean=3, .2)
# sigma ~ GAMMA(rate=5, shape=40)
prior.mean <- function(x) dnorm(x,3,.2)
prior.sd <- function(x) dgamma(x,5,40)
plot(prior.mean, col="darkblue", xlim=c(0,7), lwd="2", main="Prior for mean")
plot(prior.sd, col="darkred", xlim=c(0,1), lwd="2", main="Prior for standar")

library(Rcpp)

#function objdens
# function log likelihood
# compute logprior
# logprior = R::dnorm(theta[0], 3.0, .5, true) + R::dgamma(theta[1], 5.0, 1.0/40.0, true)
# log of target density
# return lkh - logprior

objdens(data, c(2,3))

# caminata aleatoria
# 2) proposal: random walk in the same dimension as the number of parameters

cppFunction('NumericVector proposal(NumericVector theta)
{
  int nparam = theta.size();
  double jump = .1;
  NumericVector newtheta(nparam);
  for(int i =1; i<nparam; i++)
  {
    newtheta[i] = R::rnorm(theta[i],jump);
  }
return newtheta;
}')

proposal(c(1,2))