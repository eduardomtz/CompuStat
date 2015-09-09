
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(

  # Application title
  titlePanel("Monte Carlo"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      sliderInput("bins",
                  "n:",
                  min = 1,
                  max = 5000,
                  value = 300)
    ),

    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("normal"),
      
      plotOutput("MonteCarlo"),
      
      textOutput("text1"),
      plotOutput("distPlot"),
      
      textOutput("text2"),
      plotOutput("distPlot2"),
      
      textOutput("text3"),
      plotOutput("distPlot3")
    )
  )
))
