

pi.obj <- function(x) {
  return (dgamma(x, shape=3, rate=5))
}

UpdateR <- function(theta) {
  sd <- 2
  
  maxiter <- 1e9
  iter <- 1
  accepted <- FALSE
  while(!accepted & iter < maxiter)
  {
    X <- rnorm(1, mean = theta, sd = sd)
    U <- runif(1)
    if(U<= pi.obj(X)/pi.obj(theta))
    {
      accepted <- TRUE
      theta <- X
    }
    iter <- iter + 1
  }
  return (theta)
}

#probamos que funciona
theta = .5
UpdateR(theta)

#1) pure R
MHGammaR <- function(nsim, theta0, update)
{
  theta <- numeric(nsim)
  theta[1] <- theta0
  for(i in 2:nsim)
  {
    theta[i] <- update(theta[i-1])
  }
  return (theta)
}

sample <- MHGammaR(1000, .5, UpdateR)
hist(sample)
acf(sample)
sample2 <- rgama(1000, 3, 5)
acf(sample2)


#2) hybrid

library('Rcpp')
cppFunction('
            NumericVector MHGammaH(int nsim, double theta0, Function update)
            {
              NumericVector theta(nsim);
              theta[0] = theta0;
              for(int i=1;i<nsim;i++){
                theta[i] = as<double>(update(theta[i-1]));
              }
              return theta;
            }')

MHGammaH(1000, .4, UpdateR)

#3) pure R

