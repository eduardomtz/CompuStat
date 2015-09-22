
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#
library(shiny)


shinyUI(fluidPage(

  # Application title
  titlePanel("Splines cubicos naturales"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      sliderInput("bins",
                  "Numero de atributos:",
                  min = 3,
                  max = 30,
                  value = 10)
    ),
    
    mainPanel(
      h2("Tabla de atributos"),
      tableOutput('tablaDatos'),
      h2("Representacion Grafica"),
      plotOutput("distPlot"),
      h2("Uso para calculo de subrasante de caminos"),
      plotOutput("Camino")
      
    )
  )
))
