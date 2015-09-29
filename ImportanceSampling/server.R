
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(plyr)
library("ggplot2", lib.loc="/Library/Frameworks/R.framework/Versions/3.2/Resources/library")

shinyServer(function(input, output) {
  #para tarea falta g(x) para el calculo
  #Monte Carlo Crudo
  mc.intervals <- function(N, lambda_exp,lambda_dens, alpha, interval=c()
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
      X <- runif(nsim,0,2) #N samples of the density of X
      phi <- function(x) lambda_exp*exp(-lambda_exp*x) #por intervalo
      g_x <- function(x) 1/2
      PhiXCrudo <- phi(X)/g_x() #densidad = 1
      estim <- mean(PhiXCrudo)
      S2 <- var(PhiXCrudo) # Estimate of the variance of phi(X_i)
      
      #lambda <- 1
      # ImportanceSampling step
      U <- runif(nsim,0,1) 
      # inversa de exponencial truncada
      Y <- (1/lambda_dens)*log(1/(1-(1-exp(-2*lambda_dens))*U))
      hist(Y)
      #densidad de exponencial truncada
      fun <- function(x) {
        dexp(x,lambda_dens)/(1-exp(-2*lambda_dens))
      }
      theta <- function(x) phi(x)/fun(x)
      PhiXImportance <- theta(Y)
      estimImp = mean(PhiXImportance)
      S2imp <- var(PhiXImportance)
      
      
      #dbeta(x,1,3,ncp = 0,log = FALSE)
      
      # revisar funcion replicate
      quant <- qnorm(alpha/2, lower.tail = FALSE) #Right quantile for alpha/2
      int.upper <- estim+sqrt(S2/nsim)*quant 
      int.lower <- estim-sqrt(S2/nsim)*quant
      
      quant_imp <- qexp(alpha/2, lower.tail = FALSE)
      LIimp = estimImp-sqrt(S2imp/nsim)*quant_imp
      UIimp = estimImp+sqrt(S2imp/nsim)*quant_imp
      
      return (data.frame(N=nsim, EstimateCrudo=estim, LI=int.lower, UI= int.upper, EstimateImportance=estimImp, LIimp = LIimp, UIimp = UIimp))
    })
    
    # tecnicamente exponencial
    # para subir puntos con la beta
    # phi * 2 y dividir entre 2x
    
    # recibe lista y regresa data frame
    results.table <- ldply(results.list)
    return (results.table)
  }
  
  output$mc <- renderPlot({
    set.seed(110104)
    lam_exp <- input$lam_exp
    lam_dens <- input$lam_dens
    alpha <- input$alpha
    inicial <- 100
    final <- inicial*input$uni
    N <- seq(from=inicial, to=final, by=inicial)
    data <- mc.intervals(N=N, lambda_exp = lam_exp, 
                         lambda_dens = lam_dens, alpha = alpha)
    
    h <- ggplot(data, aes(x=N))
    h <- h + geom_ribbon(aes(ymin=LI, ymax=UI), fill="blue", alpha=0.5)
    h <- h + geom_ribbon(aes(ymin=LIimp, ymax=UIimp), fill="red", alpha=0.5)
    h <- h + geom_line(aes(y=EstimateCrudo), color="blue")
    h <- h + geom_line(aes(y=EstimateImportance), color="red")
    print(h)
  })
  
  output$is <- renderPlot({
    set.seed(110104)
    lam_exp <- input$lam_exp
    lam_dens <- input$lam_dens
    alpha <- input$alpha
    inicial <- 100
    final <- inicial*input$uni
    N <- seq(from=inicial, to=final, by=inicial)
    data <- mc.intervals(N=N, lambda_exp = lam_exp, 
                         lambda_dens = lam_dens, alpha = alpha)
    
    media <- mean(as.matrix(data["EstimateImportance"]))
    
    h <- ggplot(data, aes(x=EstimateImportance)) +
      geom_density(aes(x=EstimateCrudo), color="blue", fill="blue", alpha=.5) +
      geom_density(aes(x=EstimateImportance),color="red", fill="red", alpha=.5) +
      geom_vline(xintercept = media,
               linetype="dashed", size=1) 
    
    print(h)
  })
  
  output$cdk1 <- renderTable({
    #set.seed(110104)
    #lam_exp <- input$lam_exp
    #lam_dens <- input$lam_dens
    #alpha <- input$alpha
    #inicial <- 100
    #final <- inicial*input$uni
    #N <- seq(from=inicial, to=final, by=inicial)
    #data <- mc.intervals(N=N, lambda_exp = lam_exp, 
    #                     lambda_dens = lam_dens, alpha = alpha)
    #data
  })
})
