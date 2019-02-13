#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

# https://github.com/wilkox/ggfittext

library(shiny)
library(tidyverse)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  game <- reactiveValues(data = NULL)
  
  new_game <- reactive({
    
    baza <- readxl::read_xlsx("baza.xlsx")
    
    labels <- baza %>%
      gather(cz, wart, -poziom) %>%
      select(-poziom, -cz) %>%
      as.data.frame %>%
      mutate(wart=as.character(wart))
    
    n <- sqrt(nrow(labels))
    
    d <- expand.grid(x = 1:n, y = 1:n)
    d$label <- sample(labels$wart)
    
    d <- d %>%
      mutate(x_lower = x - 0.5,
             x_upper = x + 0.5,
             y_lower = y - 0.5,
             y_upper = y + 0.5)
    
    game$data <- list(baza=baza,
                      map=d)
    
  })
  
  output$distPlot <- renderPlot({
    
    new_game()
    
    # baza <- readxl::read_xlsx("app/baza.xlsx")
    
    ggplot(game$data$map, aes(x=x, y=y)) + 
      geom_tile(fill="#e5f5f9", colour = "gray80") +
      geom_text(aes(label = label)) +
      theme_minimal() +
      theme(axis.title = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            panel.grid = element_blank())
    
  })
  
  output$info <- renderText({
    paste0("x=", input$plot_click$x, "\ny=", input$plot_click$y, "\nval: ", clicked_value())
  })
  
  clicked_value <- reactive({
  
    x <- input$plot_click$x
    y <- input$plot_click$y
    
    if(is.numeric(x) & is.numeric(y)){
      
      n <- sqrt(nrow(game$data$map))
      
      if(x > 0.5 & y > 0.5 & x < n + 0.5 & y < n + 0.5){
        
        # x <- 1.75
        # y <- 1.2
        
        d <- game$data$map
        
        val <- d$label[x > d$x_lower & x < d$x_upper & y > d$y_lower & y < d$y_upper]
        
        return(val)
      } 
    }
    
  })
  
})
