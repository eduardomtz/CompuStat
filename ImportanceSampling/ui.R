
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(

  # Application title
  titlePanel("Funcion Inversa"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      sliderInput("uni",
                  "Uniformes:",
                  min = 100,
                  max = 500,
                  value = 100),
      sliderInput("lam",
                  "lambda:",
                  min = 0.1,
                  max = 2,
                  value = 1)
    ),

    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  ),
  
  mainPanel(
    tableOutput("cdk1")
    )
))
