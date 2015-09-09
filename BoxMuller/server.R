
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyServer(function(input, output) {
  
  output$distPlot <- renderPlot({

    # generate bins based on input$bins from ui.R
    U1 <- runif(input$bins)
    U2 <- runif(input$bins)
    X <- sqrt(-2*log(U2))*cos(2*360*U1)
    bins <- 25
    
    # draw the histogram with the specified number of bins
    hist(X, breaks=bins, col = 'darkgray', border = 'white')
    
  })
  
  output$distPlotY <- renderPlot({
    
    # generate bins based on input$bins from ui.R
    U1 <- runif(input$bins)
    U2 <- runif(input$bins)
    Y <- sqrt(-2*log(U2))*sin(2*360*U1)
    bins <- 25
    
    # draw the histogram with the specified number of bins
    hist(Y, breaks=bins, col = 'darkgray', border = 'white')
    
  })
  
  #output$cdk1 <- renderTable({
  #  table1 <- table1 * 1/input$bins
  #})
  

})
