#Bootstrap
library(boot)

#parametros
sample.size <- 100 #N
X <- runif(sample.size)
fun <- function(x) var(x)
nboot <- 1000 #numero de remuestreos bootstrap

# bootstrap-mc a la antigua
vec <- numeric(nboot)
for(i in 1:nboot){
  ind <- sample(1:sample.size, sample.size, replace=TRUE) #sampleando los indices
  X.b <- X[ind] #obtiene ciertos indices de X, ind es un vector de indices
  vec[i]<-fun(X.b)
}
hist(vec) # podemos ver la distribucion de la muestra fun
summary(vec) 
var(vec) #varianza del estimador de la varianza
        #es pequeña, lo que quiere decir que 
        #es un buen estimador de la varianza
#1/12 es la media de una normal


#lo mismo usando boot

#siempre hay que definir una funcion que reciva 2 argumentos
#los datos que puede ser un vector un data frame, y un vector de indices
#en el que se evalua
my.fun<-function(x,i) fun(x[i])
boot.res <- boot(X, statistic=my.fun, R=nboot)
boot.res$t #nos devuelve lo que nosotros llamamos vec
plot(boot.res)
boot.ci(boot.res)


#bootstrap intervalos de confianza
alpha <- .1
# 1) Usando metodo de percentiles
int.quant <- quantile(vec, c(alpha/2, 1-alpha/2))
boot.res$t0 #estimador de la muestra original
int.quant - boot.res$t0 #no es centrado

# 2) Metodo centrado




### OTRO EJEMPLO DE CLASE 07-10-2015

