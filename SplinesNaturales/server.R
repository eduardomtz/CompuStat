
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
  
  
  valoresXY <- function()
  {
    X <- seq(1,input$bins,1)
    Y <- dataY()
    XX <- c()
    YY <- c()
    XHueco = c()
    for(i in valores())
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
    for(i in XHueco)
    {
      Yspline[j] <- spl(XX,YY,s,i)
      Yventana[j] <- (YY[i-3] + YY[i-2] + YY[i-1])/3
      j <- j + 1
    }
    #return (X,Y,Xprima,Yprima, XHueco, YSpline,Yventana)
    out <- list(X=X,Y=Y,Xprima=Xprima,Yprima=Yprima, XHueco=XHueco, Yspline=Yspline,Yventana=Yventana)
    out
  }
  
  dataY <- reactive(runif(input$bins, 1, 100))
  valores <- reactive(sample(2:(input$bins-1),input$bins*.2))
  
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
    points(datos$XHueco,datos$Yspline,col="brown",pch='+', cex = 2)
    points(datos$XHueco,datos$Yventana,col="red",pch=4, cex = 2)
    
    #legend(10, 6, c("spline", "datos", "ref", "llenado spline", "llenado ventana"), 
    #       col = c("darkgreen", "black", "gray", "brown","red"),
    #       text.col = "green4", lty = c(2, -1, 1,1,1), pch = c(NA,NA,NA, '+', 4),
    #       merge = TRUE, bg = "gray90")
  })
  
  output$Camino <- renderPlot({
    
    file <- read.csv(file="Camino.csv",head=TRUE,sep=",")
    
    Xreal <- as.vector(as.matrix(file['Distancia']))
    Yreal <- as.vector(as.matrix(file['Altura']))
    
    #subset(DF, ID %in% c(2,5))
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
    
    plot(Xreal,Yreal,typ='l',col="brown")
    lines(Xprima,Yprima,col="darkgreen")
    points(X,Y)
  })
})
