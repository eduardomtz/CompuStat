
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyServer(function(input, output) {

  table1 <- matrix(log((1-runif(20))^-1), 20)
  
  output$distPlot <- renderPlot({

    # generate bins based on input$bins from ui.R
    x    <- 1/input$bins*log((1-runif(1000))^-1)
    #bins <- seq(min(x), max(x), length.out = input$bins + 1)

    # draw the histogram with the specified number of bins
    hist(x, col = 'darkgray', border = 'white')
    
  })
  
  
  
  output$cdk1 <- renderTable({
    table1 <- table1 * 1/input$bins
    
  })
  

})
