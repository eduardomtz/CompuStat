#poisson monte carlo

nsim <- 500
t <- 1
lambda <- 150
Nt <- rpois(nsim, lambda*t)
plot(dpois(1:500, lambda), main = "Funcion de masa de una poisson")

#Supongamos que los costos fijos que nos generan en reclamos, 
# se pueden modelar con una funcion
#costo(n) = 1000*log(n+1)
costo <- function(n) 1000*log(n+1)
plot(1000*log(1:500+1), main = "funcion de costos")


#pregunta ¿cual es mi costo fijo esperado en un año?
#con montecarlo crudo
t<-12

nsim<-100
Nt<-rpois(nsim, lambda*t)
mean(costo(Nt))

nsim<-1000
Nt<-rpois(nsim, lambda*t)
mean(costo(Nt))

nsim<-2000
Nt<-rpois(nsim, lambda*t)
mean(costo(Nt))


#verdadero valor,
sum(costo(1:100000)*dpois(1:100000, lambda = 150*12))

#¿como sería el importance sampling?
plot(costo(1:4000)*dpois(1:4000, lambda = 150*12))


#por ejemplo una binomial con N grande
hist(rbinom(nsim, 3000, prob = 1.8/3), xlim=c(0,3000))

nsim <- 10000
size <- 2000
prob <- 1.8/3
B <- rbinom(nsim, size, prob)
mean(costo(B)*dpois(B, lambda = 150*12)/dbinom(B, size, prob))
