setwd("/Users/eduardomartinez/Documents/ITAM/semestre 1/Estadistica computacional/CompuStat/CompuStat/tarea final")
library(foreign)
library(mvtnorm)

table <- read.dta("datos_politicos.dta") # LEEMOS LOS DATOS DE FORMATO STATA
ano <- 1986 # POR EL MOMENTO ESCOJAMOS UN SOLO A?O PUES NO SABEMOS NADA DE DATOS PANEL
data <- table[table$year==ano, ]
labels <- paste(names(data), attributes(data)$var.labels, sep=": ") # NOMBRES DE LAS VARIABLES

Y <- data$reg # INDICADORA DE SI SE ES O NO UNA DEMOCRACIA
list.depend <- c("level", "open", "g", "strikes", "govpwt", "ls", "invpwt",
                 "fertil", "lfagric", "popg", "femsec", "EnergProd") # VARIABLES ECON?MICAS EXPLICATIVAS

X <- subset(data, select=list.depend)
for(j in 1:ncol(X)) X[ ,j] <- as.numeric(X[,j]) # para cada columna hazlos numericos
row.names(X) <- data$name # para saber quien es cada pais

X.comp <- X[complete.cases(X), ] # individuos con todos los datos
nrow(X.comp) # con cuantos hariasmoa el estudio si no usuaramos imputacion?

#estas 3 lineas no funcionan
#cor(X.comp) # matriz de corelacion
#res <- glm(Y~ ., data=data.full, family="binomial") # regresión logit # overfit 
#guess_res <- predict()


nrows <- nrow(X)
ncols <- ncol(X)
m <- 5 #numero de imputaciones bootstrap
tol <- 1e-2
res <- list()
imputed.sets <- list()
pred.success <- numeric(m)

for(rep in 1:m){
  #bootstrap
  print(paste("bootstrap", rep))
  samp <- sample(1:nrows, nrows, replace=TRUE)
  xb <- X[samp,]
  #inicia
  M <- is.na(xb)
  sigma <- cov(xb[complete.cases(xb),]) #matriz de covarianzas
  sd.vec <- sqrt(diag(sigma))  # vector de desviaciones standar
  mu <- apply(xb[complete.cases(xb), ],2,mean)
  for(i in 1:nrows)
  {
    for(j in 1:ncols)
    {
      if(M[i,j]){
        xb[i,j] <- rnorm(1,mu[j], sd.vec[j])
      }
    }
  }
  logv <- sum(apply(xb,1,function(row) log(dmvnorm(row,mu,sigma)))) # log verosimilitud completa, 
  # de adentro hacia afuera, multiplicacion de la funcion de una normal a cada individuo, 
  # paquete hace la parte dificil, apply 1 cada fila, 2 cada columna, 
  # dmvnorm funcion de densidad de una normal multivariada con esa mu y esa sigma
  iter <- 1
  repeat{
    #valor actual de la verosimilitud
    # iteraciones por variable
    for(j in 1:ncols) {
      ind <- as.matrix(xb[,j], ncol=1) # se guarda como matriz para poder meterlo en lm
      dep <- as.matrix(xb[,-j])
      mod <- lm(ind ~ dep)
      pred <- predict(mod)
      for (k in 1:nrows) if(M[k,j]) xb[k,j] <- pred[k]
      #xb[M[,j],j] <- pred[M[,j]]
    }
    # nueva matriz de covarianzas
    sigma <- cov(xb)
    mu <- apply(xb,2,mean)
    logv[iter+1] <- sum(apply(xb,1,function(row) log(dmvnorm(row,mu,sigma))))
    if (abs(logv[iter+1]-logv[iter]) < tol) break
    iter <- iter + 1
  }
  
  print(paste("     - iteraciones totales:", iter))
  imputed.sets[[rep]] <- xb
  
  #grafica , par ver que pasa con la log verosimilitud
  plot(logv[-(1:3)], type="l", col="blue", main=paste("bootstrap", rep))
  # quito los primeros 3 para no graficar - inf
  # esta es la q del algotimo EM, log veroslimitud completa no la puedes saber, porque no teienes datos reales.
  # casi sacando la q del E-step
  # algoritmo EM promete siempre mejorar la log verosimilitud
  
  #modelo
  data.full <- data.frame(Y[samp],xb)
  names(data.full)[1] <- "Y"
  res[[rep]] <- glm(Y~., data=data.full, family="binomial") # regresion lo
  guess <- round(predict(res[[rep]], type="response")) # para los que usan logic
  # aplica logit inversa y da la probabilidad real
  # te da el resultado de la combinacion lineal
  
  #sumary del modelo sumary(res[[rep]])
  pred.success[rep] <- sum(guess==data.full$Y)/nrows
  
  #confusion matrix
  table(guess, data.full$Y)
}

# betas promediadas, 
# 1 solo predict de las betas de pooling

# despues dijo
# pooling de los predicts

# varianza de betas, por eso imputacion multiple es el mejor metodo
# porque da un estimador in sesgado de la varianza de las betas

# pooling
beta.all <- matrix(0, nrow=ncols, ncol=m)
for(j in 1:m){
  beta.all[,j] <- coef(res[[rep]])[-1]
}

# promedio de las betas
beta.estims <- apply(beta.all, 1, mean)

# estimacion de las betas
beta.var.within <- numeric(ncols)
for(rep in 1:m){
  beta.var.within <- beta.var.within + (summary(res[[rep]])$coefficients[,2][-1])^2/m
}

beta.var.between <- apply(beta.all,1,var)
beta.var <- beta.var.within + (1+1/m)*beta.var.between

#z-values finales
table <- data.frame(beta=beta.estims, sd=sqrt(beta.var))

round(table,3)
