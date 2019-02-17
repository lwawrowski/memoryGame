library(shiny)
library(tidyverse)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Memory Game"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      actionButton("newGame", "Nowa gra")

          ),
    
    # Show a plot of the generated distribution
    mainPanel(
       plotOutput("distPlot", click = "plot_click"),
       verbatimTextOutput("info")
    )
  )
))
