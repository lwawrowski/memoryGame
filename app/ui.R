# zbieranie danych na temat liczby ruchów

dashboardPage(
  dashboardHeader(title = "Statystyczna pamięć"),
  dashboardSidebar(radioButtons("level", "Zakres materiału", c("Miary klasyczne - interpretacje" = "2",
                                                               "Miary klasyczne - symbole" = "1",
                                                               "Miary pozycyjne - interpretacje" = "4",
                                                               "Miary pozycyjne - symbole" = "3",
                                                               "Funkcje excela" = "5",
                                                               "Korelacje cech" = "6",
                                                               "Regresja - interpretacje" = "8",
                                                               "Regresja - symbole" = "7")),
                   actionButton("newGame", "Nowa gra")),
  dashboardBody(
    useShinyjs(),
    fluidRow(
      box(plotOutput("distPlot", click = "plot_click", width = "800px", height = "500px"), width = 9),
      # infoBox(title = "Rekord", icon = icon("cog"), color = "blue", width = 3, value = 0),
      infoBoxOutput("top", width = 3),
      infoBoxOutput("moves", width = 3)
  ))
)