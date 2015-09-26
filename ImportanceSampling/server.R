
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(plyr)

shinyServer(function(input, output) {
  #para tarea falta g(x) para el calculo
  #Monte Carlo Crudo
  
  
  
  mc.intervals <- function(N, X.dens=runif,X.densimp=runif, alpha=0.05, interval=c()
                           , control=list())
  {
    # N: is a vector which contains different sample sizes fo our estimate
    # alpha: determines the confidence intervals of level 1-alpha
    # sample: must be a function from which to draw N trials of X
    # Phi: is used sum_{i=1}^N(phi(X_i)=E[phi(X_i)]-int phi(X_i/f_X(x)dx))
    # interval : if the user wants to give integratin limits c=(a,b) instead
    # of sample, then sample is taken unif(a,b) and Phi is replaced by (b-)
    
    #Doy una C(x) funcion
    #estimacion de c es el promedio de las c
    
    # control: 3 parametros, funcion, media(verdadero valor), estimacion de la correlacion
    
    # recibe data frame y regresa lista
    results.list <- lapply(N, function(nsim){
      # MonteCarlo step
      lam <- input$lam
      X <- sapply(nsim, FUN=X.dens) #N samples of the density of X
      Ximp = sapply(nsim, FUN=X.densimp)
      exponencial <- function(x) lam*exp(-lam*x) #por intervalo
      PhiXCrudo <- sapply(X,exponencial) #evaluate phi at each X_i
      estim <- mean(2*PhiXCrudo) #Estimate of int a^b
      
      #K <- 1-exp(-2*lam)
      #inversa <- 1/lam*log((1-K*Ximp)^-1)
      fun <- function(x) dexp(x)/(1-exp(-2*lam))
      #fun <- function(x) dexp(x)/inversa
      phi <- function(x) exponencial(x)/fun(x)
      PhiXImportance <- sapply(X,phi)
      importance = mean(PhiXImportance)
      
      S2 <- var(PhiXCrudo) # Estimate of the variance of phi(X_i)
      if(length(control)>0)
      {
        C <- sapply(X,control[[1]])
        estim <- estim + control[[3]]*mean(C)*control[[2]]
      }
      # revisar funcion replicate
      quant <- qnorm(alpha/2, lower.tail = FALSE) #Right quantile for alpha/2
      int.upper <- estim+sqrt(S2/nsim)*quant 
      int.lower <- estim-sqrt(S2/nsim)*quant
      return (data.frame(N=nsim, EstimateCrudo=estim, LI=int.lower, UI= int.upper, EstimateImportance=importance))
    })
    
    # tecnicamente exponencial
    # para subir puntos con la beta
    # phi * 2 y dividir entre 2x
    
    # recibe lista y regresa data frame
    results.table <- ldply(results.list)
    return (results.table)
  }
  
  output$distPlot <- renderPlot({
  })
  
  output$cdk1 <- renderTable({
    #table1 <- matrix(log((1-runif(input$bins))^-1), input$bins)
    #table1 <- table1 * 1/input$bins
    
    set.seed(110104)
    
    X.dens <- function(nsim) runif(nsim,0,2)
    X.densimp <- function(nsim) runif(nsim,0,1)
    #N <- seq(from=1000, to=10000, by=1000)
    N <- c()
    for (i in 1:100)
    {
      N <- c(N, input$uni)
    }
    
    data <- mc.intervals(N=N, X.dens=X.dens, X.densimp=X.densimp)
    data
  })
})
