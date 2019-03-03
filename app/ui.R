library(shiny)
library(shinydashboard)
library(tidyverse)
library(shinyjs)
library(ggfittext)

# zbieranie danych na temat liczby ruchów

dashboardPage(
  dashboardHeader(title = "Statystyczna pamięć"),
  dashboardSidebar(radioButtons("level", "Zakres materiału", c("Miary klasyczne - interpretacje" = "2",
                                                               "Miary klasyczne - symbole" = "1",
                                                               "Miary pozycyjne - interpretacje" = "4",
                                                               "Miary pozycyjne - symbole" = "3")),
                   actionButton("newGame", "Nowa gra")),
  dashboardBody(
    useShinyjs(),
    fluidRow(
      box(plotOutput("distPlot", click = "plot_click", width = "800px", height = "500px"), width = 9),
      infoBox(title = "Rekord", icon = icon("cog"), color = "blue", width = 3, value = 0),
      # infoBox(title = "Liczba ruchów", icon = icon("cog"), color = "blue", width = 3, value = game$data$points)
      # infoBoxOutput("top", width = 2),
      infoBoxOutput("moves", width = 3)
  ))
)