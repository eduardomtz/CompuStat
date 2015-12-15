
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyServer(function(input, output) {
  
  # procedimiento spcoef
  # para los siguientes valores de x e y
  # X <- c(1.0,3.0,6.0,8.0)
  # Y <- c(2.0,5.0,3.0,4.5)
  # debe regresar
  # s <- c(0.0,-1.71,1.36,0.0)
  spcoef <- function(x,y) {
    n <- length(x)
    s <- array(0,n)
    sigma <- array(0,n)
    tau <- array(0,n)
    sigma[2] <- 0
    tau[2] <- 0
    for (i in 2:(n-1)){
      h1 <- x[i] - x[i-1]
      h2 <- x[i+1] - x[i]
      temp <- (h1/h2) * (sigma[i]+2)+2
      sigma[i+1] <- -1 / temp
      d = 6*(((y[i+1]-y[i])/h2) - ((y[i]-y[i-1])/h1)) / h2
      tau[i+1] <- (d-h1*tau[i]/h2)/temp
    }
    s[1] <- 0
    s[n] <- 0
    for (i in 1:(n-2)){
      ib <- n-i
      s[ib]<-sigma[ib+1]*s[ib+1]+tau[ib+1]
    }
    s
  }
  
  #procedimiento spline
  spl <- function(x,y,s,alpha){
    j <- 0
    n <- length(x)
    for (i in 2:n)
    {
      j <- i
      if(alpha <= x[i]){
        break
      }
    }
    i <- j -1
    a <- x[i+1]-alpha
    b <- alpha-x[i]
    hi <- x[i+1] - x[i]
    beta <- a*s[i]*(a^2/hi-hi)/6+b*s[i+1]*(b^2/hi-hi)/6+(a*y[i]+b*y[i+1])/hi
    beta
  }
  
  #coeficientes
  coeficientes <- function(x,y,s,alpha){
    X <- seq(1,input$bins,1) 
    Y <- dataY()
    XX <- c()
    YY <- c()
    XHueco <- c()
    valor <- valores()
    for(i in valor)
    {
      Y[i] <- NA
    }
    for(i in 1:length(Y))
    {
      if(!is.na(Y[i]))
      {
        XX <- c(XX, X[i])
        YY <- c(YY, Y[i])
      }
    }
    
    s <- spcoef(XX,YY)
    
    n <- length(XX)
    RES <- matrix(nrow=n,ncol=6)
    for (i in 1:(n-1))
    {
      hi <- XX[i+1] - XX[i]
      
      D <- s[i+1]/(6*hi) - s[i]/(6*hi)
      C <- 3*s[i]/(6*hi)*XX[i+1]-3*s[i+1]/(6*hi)*XX[i]
      B <- 3*s[i+1]/(6*hi)*XX[i]-3*s[i]/(6*hi)*(XX[i+1]^2)+YY[i+1]/hi-s[i]*hi/6-YY[i]/hi+s[i]*hi/6
      A <- s[i]/(6*hi)*(XX[i+1]^3)-s[i+1]/(6*hi)*(XX[i]^3)-(YY[i+1]/hi-s[i+1]*hi/6)*XX[i]+(YY[i]/hi-s[i]*hi/6)*XX[i+1]
      
      RES[i,1] <- XX[i]
      RES[i,2] <- XX[i+1]
      RES[i,3] <- A
      RES[i,4] <- B
      RES[i,5] <- C
      RES[i,6] <- D
    }
    RES
  }
  
  
  valoresXY <- function()
  {
    X <- seq(1,input$bins,1) 
    Y <- dataY()
    XX <- c()
    YY <- c()
    XHueco <- c()
    valor <- valores()
    for(i in valor)
    {
      Y[i] <- NA
    }
    for(i in 1:length(Y))
    {
      if(!is.na(Y[i]))
      {
        XX <- c(XX, X[i])
        YY <- c(YY, Y[i])
      }
      else
      {
        XHueco <- c(XHueco, X[i])
      }
    }
    s <- spcoef(XX,YY)
    min <- min(XX)
    top <- max(XX)
    inc <- 0.1
    if(top>10)
    {
      inc = .25
    }
    Xprima <- seq(min,top,inc)
    n <- length(Xprima)
    Yprima <- array(0,n)
    YprimaVent <- array(0,n)
    for(i in 1:n)
    {
      Yprima[i] <- spl(XX,YY,s,Xprima[i])
    }
    Yspline = c()
    Yventana = c()
    j <- 1
    YventanaPrima = c()
    for(i in 1:length(Y))
    {
      YventanaPrima = c(YventanaPrima,Y[i])
    }
    for(i in XHueco)
    {
      Yspline[j] <- spl(XX,YY,s,i)
      YventanaPrima[i] <- (YventanaPrima[i-3] + YventanaPrima[i-2] + YventanaPrima[i-1])/3
      Yventana[j] <- YventanaPrima[i]
      j <- j + 1
    }
    
    out <- list(X=X,Y=Y,Xprima=Xprima,Yprima=Yprima, XHueco=XHueco, Yspline=Yspline,Yventana=Yventana)#, coef = s)
    out
  }
  
  dataY <- reactive(runif(input$bins, 1, 100))
  valores <- reactive(sample(4:(input$bins-1),input$bins*.25))
  
  output$tablaCoeficientes <- renderTable({
    M <- coeficientes()
    colnames(M) <- c('X', 'X+1', 'A', 'B', 'C', 'D')
    M
  })
  
  output$tablaDatos <- renderTable({
    datos = valoresXY()
    n = length(datos$X)
    M = matrix(0,nrow=n,ncol=3)
    colnames(M) <- c('Original', 'Splines', 'Ventanas')
    for(i in 1:n)
    {
      M[i,1] = datos$Y[i]
      M[i,2] = datos$Y[i]
      M[i,3] = datos$Y[i]
    }
    j <- 1
    for(i in datos$XHueco)
    {
      M[i,2] <- datos$Yspline[j]
      M[i,3] <- datos$Yventana[j]
      j <- j+1
    }
    
    M
  })
  
  output$distPlot <- renderPlot({
    
    datos = valoresXY()
    
    plot(datos$Xprima,datos$Yprima,typ='l',col="darkgreen")
    points(datos$X,datos$Y,col="black")
    lines(datos$X,datos$Y,col="gray")
    points(datos$XHueco,datos$Yspline,col="blue",pch='+', cex = 2)
    points(datos$XHueco,datos$Yventana,col="red",pch=4, cex = 2)
    
    legend("topright", c("spline", "ventana"), lty = 1, 
           col = c("blue", "red"), pch = c('+', 'X'), bty='n', cex=1)
  })
  
  output$Camino <- renderPlot({
    
    file <- read.csv(file="Camino.csv",head=TRUE,sep=",")
    
    Xreal <- as.vector(as.matrix(file['Distancia']))
    Yreal <- as.vector(as.matrix(file['Altura']))
    
    ele <- c(1,8,20,27,36,43,50,55,60,71)
    
    X <- as.vector(as.matrix(subset(file, ID %in% ele, select = Distancia)))
    Y <- as.vector(as.matrix(subset(file, ID %in% ele, select = Altura)))
    
    s <- spcoef(X,Y)
    
    min <- min(X)
    top <- max(X)
    inc <- 0.1
    if(top>10)
    {
      inc = 1
    }
    Xprima <- seq(min,top,inc)
    n <- length(Xprima)
    Yprima <- array(0,n)
    for(i in 1:n)
    {
      Yprima[i] <- spl(X,Y,s,Xprima[i])
    }
    plot(Xreal,Yreal, typ='l',col="brown",
         xlab="Distancia", ylab="Altura")
    lines(Xprima,Yprima,col="darkgreen")
    points(X,Y)
    legend("bottomright", c("relieve", "spline"), lty = 1, 
           col = c("brown", "darkgreen"), bty='n', cex=1)
  })
})
