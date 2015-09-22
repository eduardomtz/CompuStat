
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
  mc.intervals <- function(Phi, N, X.dens=runif, alpha=0.05, interval=c()
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
      X <- sapply(nsim, FUN=X.dens) #N samples of the density of X
      PhiX <- sapply(X,Phi) #evaluate phi at each X_i
      estim <- mean(PhiX) #Estimate of int a^b
      S2 <- var(PhiX) # Estimate of the variance of phi(X_i)
      
      if(length(control)>0)
      {
        C <- sapply(X,control[[1]])
        estim <- estim + control[[3]]*mean(C)*control[[2]]
      }
      # revisar funcion replicate
      quant <- qnorm(alpha/2, lower.tail = FALSE) #Right quantile for alpha/2
      int.upper <- estim+sqrt(S2/nsim)*quant 
      int.lower <- estim -sqrt(S2/nsim)*quant
      return (data.frame(N=nsim, Estimate=estim, LI=int.lower, UI= int.upper))
    })
    # recibe lista y regresa data frame
    results.table <- ldply(results.list)
    return (results.table)
  }
  
  output$distPlot <- renderPlot({

   set.seed(110104)
    Phi <- function(x) 2*sqrt(4-x^2)
    X.dens <- function(nsim) runif(nsim,0,2)
    N <- seq(from=1000, to=10000, by=1000)
    data <- mc.intervals(Phi=Phi,N=N, X.dens = X)
    data
    
    #tarea
    x.dens <- function(nsim){
      x <- rexp(nsim,lambda)
      return (x[x<=1])
    }
  })
  
  
  
  output$cdk1 <- renderTable({
    table1 <- matrix(log((1-runif(input$bins))^-1), input$bins)
    table1 <- table1 * 1/input$bins
    
  })
  

})
