setwd('/Users/eduardomartinez/Documents/ITAM/semestre 1/Estadistica computacional/CompuStat/CompuStat/RegresionLineal')

library(Rcpp)

data(iris)

View(iris)
# BAYESIAN APPROACH
vec <- rep(1,150)
data <- matrix(cbind(vec,iris$Sepal.Width,iris$Petal.Length,iris$Petal.Width,iris$Sepal.Length),ncol=5)

summary(data)

# WE WILL USE AND MCMC METHOD.
# NEED 
# 1: A objective density: 2) a proposal density
# Recall obj ~~ L(theta|X)prior(X)
# But as we want logarithms, we have log(obj) = log(L) + log(prior)

# objdens <- function(data, theta){
#   m <- nrow(data)
#   print(theta) 
#   Y <- data[,5]  # In this example is redundant but helps to generalise
#   X <- data
#   # Compute loglikelihood
#   lkh <- 0.0
#   for (i in 1:m)
#   {
#     mu <- 0.0
#     for (j in 1:4)
#     {
#       mu <- mu + theta[j]*X[i,j]
#     }
#     lkh <- lkh + -0.5*((Y[i]-mu)/theta[5])^2-log(theta[5])
#     #printf("L:%f,m:%f","mu",lkh);
#   }
#   # Compute logprior
#   logprior <- dnorm(theta[1], 1,1) + dnorm(theta[2], 3,1) + dnorm(theta[3], 4,2) + dnorm(theta[4], 1,3) + dgamma(theta[5], 5, 1/40)
#   # Log of target density
#   return lkh + logprior
# }

cppFunction('
            double objdens(NumericMatrix data, NumericVector theta){
            double lkh, logprior, mu;
            int m=data.nrow();
            NumericVector Y(m);
            NumericMatrix X(m,theta.size());
            Y = data(_,4); // In this example is redundant but helps to generalise
            X = data;
            
            // Compute loglikelihood
            
            lkh=0.0;
            for (int i=0; i<150; i++){
            mu=0.0;
            for (int j=0; j<4; j++){
            mu += theta[j]*X(i,j);
            }
            lkh += -.5*pow((Y[i]-mu)/theta[4],2)-log(theta[4]);
            //printf("L:%f,m:%f","mu",lkh);            
            }
            
            // Compute logprior
            logprior = R::dnorm(theta[0], 2.0,3.0, true) + R::dnorm(theta[1], 1.0,3.0, true) + R::dnorm(theta[2], 1.0,3.0, true) 
            + R::dnorm(theta[3], 0.0,3.0, true)+ R::dgamma(theta[4], 5.0, 1.0/40.0, true);
            // Log of target density
            return lkh + logprior;
            }')

# intercepto, atributos e y
objdens(data,c(1,2,2,2,3))

# 2) Proposal: random walk in the same dimension as the number of parameters
cppFunction('
            NumericVector proposal(NumericVector theta){
              int nparam = theta.size();
              double jump = .2; 
              NumericVector newtheta(nparam);
              for (int i=0; i<nparam; i++){
                newtheta[i] = R::rnorm(theta[i], jump);
              }
              return newtheta;
}')

proposal(c(1,2,2,2))

# 3) METROPOLIS
sourceCpp("BayesianMH.cpp")

nsim <- 1000
init <- c(1,3,3,3,1)
MHBayes(20, init, objdens, proposal, data)
mh.samp <- MHBayes(nsim, init, objdens, proposal, data)
estims <- mh.samp$theta

#  SOME DIAGNOSTIC IMPORTANT STUFF
#  Exploration graph:
library(calibrate)
pts <- seq(1,100,by=5)
plot(estims[pts, ], type="l", xlab="mean", ylab="sd")
textxy(estims[pts,1], estims[pts,2], pts)
cor(estims)
### 1) REJECTION RATES
rejections <- mh.samp$rejections[-1]
trials <- rejections + 1
rej.rate <- cumsum(rejections)/cumsum(trials)
plot(rej.rate, type="l", ylim=c(0,1), main="Rejection rate")
plot(trials[-1], type="l", main="Number of trials")
### 2) AUTOCORRELATION
acf(estims[ , 1])
acf(estims[ , 2]) # WARNING HERE!
acf(estims[ , 3])
acf(estims[ , 4])
# burnin and subsampling
burnin <- 100
estim <- estims[-(1:burnin), ]
thinning <- .9 # meaning we'll keep 75% of observations to reduce autocorrelation
# OBS: thinning is rarely usefull!!!! check that nothing changes
sub <- sample.int(nsim-burnin, size=round(thinning*nsim))
estims <- estims[sub, ]
acf(estims[ , 1])
acf(estims[ , 2]) 
acf(estims[ , 3])
acf(estims[ , 4])
acf(estims[ , 5])


# LET'S COMPARE PRIORS AND POSTERIORS AND DO INFERENCE

hist(estims[ ,1], prob=TRUE, xlim=c(2.5,3.5), breaks=20, col="lightgreen",
     main="Histogram and Posterior(blue) vs Prior(red) of the Mean") # posterior distribution of mean
plot(prior.mean, xlim=c(2.5,3.5), col="darkred", lwd="2", ylim=c(0,10), add=TRUE)
lines(density(estims[ ,1]), col="darkblue", lwd="2")

hist(estims[ ,2], prob=TRUE, xlim=c(0,1), breaks=40, col="yellow",
     main="Histogram and Posterior(blue) vs Prior(red) of the s.d.") # posterior distribution of mean
plot(prior.sd, xlim=c(0,1), col="darkred", lwd="2", ylim=c(0,10), add=TRUE)
lines(density(estims[ ,2]), col="darkblue", lwd="2")

hist(estims[ ,3], prob=TRUE, xlim=c(0,1), breaks=40, col="yellow",
     main="Histogram and Posterior(blue) vs Prior(red) of the s.d.") # posterior distribution of mean
plot(prior.sd, xlim=c(0,1), col="darkred", lwd="2", ylim=c(0,10), add=TRUE)
lines(density(estims[ ,3]), col="darkblue", lwd="2")

hist(estims[ ,4], prob=TRUE, xlim=c(0,1), breaks=40, col="yellow",
     main="Histogram and Posterior(blue) vs Prior(red) of the s.d.") # posterior distribution of mean
plot(prior.sd, xlim=c(0,1), col="darkred", lwd="2", ylim=c(0,10), add=TRUE)
lines(density(estims[ ,4]), col="darkblue", lwd="2")

hist(estims[ ,5], prob=TRUE, xlim=c(0,1), breaks=40, col="yellow",
     main="Histogram and Posterior(blue) vs Prior(red) of the s.d.") # posterior distribution of mean
plot(prior.sd, xlim=c(0,1), col="darkred", lwd="2", ylim=c(0,10), add=TRUE)
lines(density(estims[ ,5]), col="darkblue", lwd="2")


mean(estims[ ,1]) # approx. mean-value of the posterior of mean
mean(estims[ ,2]) # approx. mean-value of the posterior of standard deviation
mean(estims[ ,3]) # approx. mean-value of the posterior of standard deviation
mean(estims[ ,4]) # approx. mean-value of the posterior of standard deviation

# CERTAINTY INTERVALS
intervals3 <- quantile(estims[ ,1], c(alpha/2, 1-alpha/2))
intervals3
quantile(estims[ ,2], c(alpha/2, 1-alpha/2)) # ALSO FOR SSSDDDD

# COMPARISON OF ALL RESULTS
meanestims <- c(est.mean, params[1], mean(estims[ ,1]))
sdestims <- c(est.sd, params[2], mean(estims[ ,2]))
intmeanlow <- c(intervals[1], intervals2[1], intervals3[1])
intmeanhigh <- c(intervals[2], intervals2[2], intervals3[2])
Comparison <- data.frame(meanestims, sdestims, intmeanlow, intmeanhigh)
row.names(Comparison) <- c("Pivot", "Likelihood", "Bayesian")
Comparison
