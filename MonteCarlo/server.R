
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
    x = seq(0,2,0.1)
    curve(q)
    
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
  
  output$text3 <- renderText({ 
    t = runif(input$bins,0,1)
    r = q3(t)
    p = mean(r)
    paste("Solucion para 6/(sqrt(4-x^2)): ",  p)
  })
})
