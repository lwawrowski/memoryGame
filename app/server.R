library(shiny)
library(tidyverse)
library(shinyjs)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  game <- reactiveValues(data = NULL)
  
  new_game <- reactive({
    
    game$data <- NULL
    
    baza <- readxl::read_xlsx("baza.xlsx") %>%
      mutate_if(is.numeric, as.character)
    
    # baza <- readxl::read_xlsx("app/baza.xlsx") %>%
    #   mutate_if(is.numeric, as.character)
    # 
    # baza[baza$cz1 %in% c("1", "b") & baza$cz2 %in% c("b", "1"), ]
    
    labels <- baza %>%
      gather(cz, wart, -poziom) %>%
      select(-poziom, -cz) %>%
      as.data.frame %>%
      mutate(wart=as.character(wart))
    
    n <- sqrt(nrow(labels))
    
    d <- expand.grid(x = 1:n, y = 1:n)
    #d$label <- sample(labels$wart)
    d$label <- labels$wart
    
    d <- d %>%
      mutate(label_show="",
             x_lower = x - 0.5,
             x_upper = x + 0.5,
             y_lower = y - 0.5,
             y_upper = y + 0.5)
    
    game$data <- list(baza=baza,
                      map=d,
                      clicks=0,
                      click1="",
                      click2="",
                      points=0)
    
  })
  
  observeEvent(input$newGame, {
    
    new_game()
    
  })
  
  render_map <- reactive({
    
    ggplot(game$data$map, aes(x=x, y=y)) + 
      geom_tile(fill="#e5f5f9", colour = "gray80") +
      geom_text(aes(label = label_show)) +
      theme_minimal() +
      theme(axis.title = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            panel.grid = element_blank())
    
  })
  
  output$distPlot <- renderPlot({
    
    if(!is.null(game$data)){
        render_map()
    }
    
  })
  
  output$info <- renderText({
    paste0("x=", input$plot_click$x, "\ny=", input$plot_click$y, "\nval: ", clicked_value(), 
           "\nclicks: ", game$data$clicks, "\nc1: ", game$data$click1, "\nc2: ", game$data$click2)
    # paste0("Points: ", game$data$points)
  })
  
  clicked_value <- reactive({
    
    if(!is.null(game$data)){
      x <- input$plot_click$x
      y <- input$plot_click$y
      
      if(is.numeric(x) & is.numeric(y)){
        
        n <- sqrt(nrow(game$data$map))
        
        if(x > 0.5 & y > 0.5 & x < n + 0.5 & y < n + 0.5){
          
          d <- game$data$map
          
          val <- d$label[x > d$x_lower & x < d$x_upper & y > d$y_lower & y < d$y_upper]
          
          return(val)
        } 
      }
    }
    
  })
  
  is_already_clicked <- function(tile_val){
    
    result <- FALSE
    
    if(is.character(tile_val)){
      
      label_show <- game$data$map$label_show[game$data$map$label == tile_val]
      
      result <- ifelse(label_show != "", TRUE, FALSE)
      
    }
    
    return(result)
    
  }
  
  timer <- reactiveTimer(2000)
  
  observeEvent(input$plot_click, {
    
    tile_val <- clicked_value()
    
    if(is.character(tile_val)){
      # pokaż kliknięte pole - zmień obiekt
      # jeśli dwa kliknięta pola nie pasują - ukryj
      # jeśli pasują to odnotuj w głównym obiekcie i nie ukrywaj
      
      if(game$data$clicks == 0){
        game$data$click1 <- tile_val
        
        # nie licz kliknięcia na już odsłonięte
        if(!is_already_clicked(tile_val)){
          click <- game$data$clicks
          game$data$clicks <- click + 1
          game$data$map$label_show[game$data$map$label == tile_val] <- tile_val
        }
        
      } else if (game$data$clicks == 1){
        game$data$click2 <- tile_val
        
        # nie licz kliknięcia na już odsłonięte
        if(!is_already_clicked(tile_val)){
          click <- game$data$clicks
          game$data$clicks <- click + 1
          game$data$map$label_show[game$data$map$label == tile_val] <- tile_val
        }
        
        if(game$data$clicks == 2){
          
          df <- game$data$baza[game$data$baza$cz1 %in% c(game$data$click1, game$data$click2) & 
                                 game$data$baza$cz2 %in% c(game$data$click1, game$data$click2),]
          
          if(nrow(df) != 1){
            
            delay(1000, clear_map())
            
          } else {
            
            points <- game$data$points
            game$data$points <- points + 1
            
            df_labels <- game$data$map[game$data$map$label_show == "",]
            
            if(nrow(df_labels) == 0){
              showModal(modalDialog(title = "Koniec gry", "Gratulacje!", footer = modalButton("Zamknij")))
            }
            
          }
          
          game$data$clicks <- 0
          
        }
        
      } 
      
    }
    
  })
  
  clear_map <- reactive({
    
    game$data$map$label_show[game$data$map$label == game$data$click1] <- ""
    game$data$map$label_show[game$data$map$label == game$data$click2] <- ""
    
  })
  
})
