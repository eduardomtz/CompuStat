
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyUI(fluidPage(

  # Application title
  titlePanel("Monte Carlo vs Importance Sampling"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      sliderInput("uni",
                  "Estimaciones:",
                  min = 0,
                  max = 100,
                  value = 20),
      sliderInput("alpha",
                  "alpha intervalos:",
                  min = 0.01,
                  max = 1,
                  value = 0.05),
      sliderInput("lam_exp",
                  "lambda phi:",
                  min = 0.01,
                  max = 1,
                  value = 0.85),
      sliderInput("lam_dens",
                  "lambda g(x):",
                  min = 0.01,
                  max = 1,
                  value = 1)
    ),

    # Show a plot of the generated distribution
    mainPanel(
      #tableOutput("cdk1"),
      h1("Intervalos de confianza"),
      plotOutput("mc"),
      h1("Densidad de estimaciones"),
      plotOutput("is"),
      plotOutput("distPlot")
    )
  )
))
