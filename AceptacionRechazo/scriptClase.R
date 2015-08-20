f <- function(x){
  (2/sqrt(2*pi))*exp(-x^2/2)
}

g<-function(x){
  exp(-x)
}
plot(f,ylim = c(0,2), xlim = c(0,5))
plot(g, add=TRUE, col="red",ylim=c(0,2),xlim=c(0,5))

h<-function(x){
  f(x)/g(x)
}

plot(h,xlim=c(0.1,4), ylim=c(0,2))

M<- 2


#aceptacion rechazo





#...........#
#2) acepto o rechazo

genera.una <- function(...)
{
  MAXITER = 10000
continuar = TRUE
iter <- 1
while(continuar | iter <= MAXITER)
{
  #haz  exponencial # # o usen rexp
  Y <- log(1/(1-runif(1))) # exponencial
  U <- runif(1) #unif
  if(U<=f(Y)/(M*g(Y))){
    X <- Y
    continuar = FALSE
  }
  iter <- iter + 1
}
return(X)
}


genera.muchas <- function(n){
  sapply(1:n, FUN=genera.una)
}

vec <- genera.muchas(500)

hist(vec,breaks=10)

genera.normales <- function(n){
  sample(size =n,c(-1,1), replace= TRUE) * genera.muchas(n)
}

vec1 <- genera.normales(500)
hist(vec1,breaks=20)