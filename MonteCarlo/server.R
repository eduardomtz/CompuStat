
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyServer(function(input, output) {
  
  q <- function(x){
    2*(sqrt(4-x^2))
  }
  
  q2 <- function(x)
  {
    4/(1+x^2)
  }
  
  q3 <- function(x)
  {
    6/(sqrt(4-x^2))
  }
  
  output$distPlot <- renderPlot({
    # generate bins based on input$bins from ui.
    #0,2
    #x = seq(-2,2,0.1)
    #t = q(x)
    curve(q,xlim=c(0,2))
    
    
    #cord.x <- c(seq(0,2,0.01)) 
    #cord.y <- c(q(seq(0,2,0.01))) 
    #polygon(cord.x,cord.y,col='skyblue')
    
    # 
    #
  })
  
  output$text1 <- renderText({ 
     t = runif(input$bins,0,2)
     r = q(t)
     p = mean(r)
     paste("Solucion para 2*raiz(4-x^2): ",  p)
  })
  
  output$distPlot2 <- renderPlot({
    # generate bins based on input$bins from ui.
    #0,2
    x = seq(0,1,0.1)
    curve(q2)
  })
  
  output$text2 <- renderText({ 
    t = runif(input$bins,0,1)
    r = q2(t)
    p = mean(r)
    paste("Solucion para 4/(1+x^2): ",  p)
  })
  
  output$distPlot3 <- renderPlot({
    # generate bins based on input$bins from ui.
    #0,2
    x = seq(0,1,0.1)
    curve(q3)
  })
  
  output$normal <- renderPlot({
    t <- runif(input$bins,0,1)
    y <- normalStandard(t)
    curve(normalStandard, from=-2, to=2)
  })
  
  normalStandard <- function(x)
  {
    m <- length(x)
    ((1/(2*pi)^1)*exp(x^2)/2)
  }
  
  output$text3 <- renderText({ 
    t = runif(input$bins,0,1)
    r = q3(t)
    p = mean(r)
    paste("Solucion para 6/(sqrt(4-x^2)): ",  p)
  })
  
  montecarlo <- function(nsim,a=0.05)
  {
    x<-runif(nsim,0,1)
    u<-6/(sqrt(4-x^2))
    thetaF <- mean(u)
    Za <- qnorm(a/2, lower.tail = FALSE)
    S2 <- var(u)
    limup <- thetaF + (Za*sqrt(S2/nsim))
    liminf <- thetaF - (Za*sqrt(S2/nsim))
    return (data.frame(est = thetaF, limup=limup, liminf=liminf))
  }
  
  output$MonteCarlo <- renderPlot({
    N = seq(10,input$bins,10)
    res = t(sapply(N,montecarlo))
    plot(N,res[,1],type='l',col='red')
    lines(N,res[,2],type='l')
    lines(N,res[,3],type='l')
    abline(h=pi)
  })
  
  trapecio <- function(N,fun,a,b)
  {
    pnts<-seq(a,b,(b-a)/N)
    integral<-0
    for(x in 1:N)
    {
      integral <- integral + (fun(pnts[x])+fun(pnts[x+1]))*((b-a)/(2*N))
    }
    return (integral)
  }
  
  #evaluar y debe dar cercano a pi
  #trapecio(1000,function(x) 4*sqrt(1-x^2),0,1)
})
