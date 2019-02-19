library(shiny)
library(tidyverse)
library(shinyjs)
library(ggfittext)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  useShinyjs(),
  
  # Application title
  titlePanel("Memory Game"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      radioButtons("level", "Zakres materiału", c("Miary klasyczne - symbole" = "1",
                                                  "Miary klasyczne - interpretacje" = "2")),
      actionButton("newGame", "Nowa gra")
    ),
    # Show a plot of the generated distribution
    mainPanel(
       plotOutput("distPlot", click = "plot_click"),
       verbatimTextOutput("info")
    )
  )
))
