
############# clase 23-09-2015

#INTERVALO 0 A 2 1/sqrt(2pi)*exp(-x^2/2) DF
pnorm(2)-1/2

#dividir entre la densidad de una uniforme
#opcion crudo
nsim<-1000
U <- runif(nsim,0,2)
phi <- function(x) 2*dnorm(x)
estim <- mean(phi(U))

#pone lambda = 1 por default, para el ejericio no debe ser 1
#opcion prioritaria
nsim<-100
U<-runif(nsim,0,1)
#exponencial(lambda=1) truncada a [0,2]
X<- -log(1-(1-exp(-10))*U) # por propiedad de logaritmo se agrega un menos al principio
#densidad de exponencial truncada
fun <- function(x) dexp(x)/(1-exp(-10))
# monte carlo
phi <- function(x) dnorm(x)/fun(x)
estim <- mean(phi(U))

#ejemplo practico

a <- 0
b <- 1000
nsim <- 100
#crudo
U <- runif(nsim,a,b)
mean((b-a)*dnorm(U))
#prioritario
U<-rexp(nsim,rate=4)
mean(dnorm(U)/dexp(U))

#EJERCICIO NO RATED - variables de control

#########################
