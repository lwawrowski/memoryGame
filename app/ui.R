library(shiny)
library(tidyverse)
library(shinyjs)
library(ggfittext)

# zbieranie danych na temat liczby ruchów
# dashboard

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  useShinyjs(),
  
  titlePanel("Statystyczna pamięć"),
  
  sidebarLayout(
    sidebarPanel(
      radioButtons("level", "Zakres materiału", c("Miary klasyczne - symbole" = "1",
                                                  "Miary klasyczne - interpretacje" = "2",
                                                  "Wszystko - losowo" = "0")),
      actionButton("newGame", "Nowa gra")
    ),

    mainPanel(
       plotOutput("distPlot", click = "plot_click", width = "800px", height = "500px")
       # verbatimTextOutput("info")
    )
  )
))
