library(shiny)
library(shinydashboard)
library(tidyverse)
library(shinyjs)
library(ggfittext)

# zbieranie danych na temat liczby ruchów

dashboardPage(
  dashboardHeader(title = "Statystyczna pamięć"),
  dashboardSidebar(radioButtons("level", "Zakres materiału", c("Miary klasyczne - symbole" = "1",
                                                               "Miary klasyczne - interpretacje" = "2",
                                                               "Wszystko - losowo" = "0")),
                   actionButton("newGame", "Nowa gra")),
  dashboardBody(
    useShinyjs(),
    fluidRow(
      box(plotOutput("distPlot", click = "plot_click", width = "800px", height = "500px"), width = 9),
      infoBox(title = actionLink("top", "Rekord"), icon = icon("cog"), color = "blue", width = 2, value = 0)
      # infoBoxOutput("top", width = 2),
  ))
)