shinyServer(function(input, output) {
  
  game <- reactiveValues(data = NULL)
  
  new_game <- reactive({
    
    game$data <- NULL
    
    colors <- grDevices::colors()[grep("gr(a|e)y", grDevices::colors(), invert = T)]
    
    import <- readxl::read_xlsx("baza.xlsx", sheet = 2) %>%
      mutate_if(is.numeric, as.character)
    
    if(input$level == "0"){
      database <- import %>%
        sample_n(8)
    } else {
      database <- import[import$poziom == input$level,]
    }
  
    colors_database <- sample(colors, nrow(database))
    
    database$colour <- colors_database
    
    labels <- database %>%
      gather(cz, label, -poziom, -colour) %>%
      select(-poziom, -cz) %>%
      as.data.frame %>%
      mutate(label=as.character(label))
    
    n <- sqrt(nrow(labels))
    
    d <- expand.grid(x = 1:n, y = 1:n)
    d$label <- sample(labels$label)
    # d$label <- labels$wart
    
    d <- inner_join(d, labels, by = "label")
    
    d <- d %>%
      mutate(label_show="",
             x_lower = x - 0.5,
             x_upper = x + 0.5,
             y_lower = y - 0.5,
             y_upper = y + 0.5)
    
    # load log
    
    gs <- gs_key("1niK-cua22XNK20adjzuOFqIWLGHEheJJCdvor2wU_Qk")
    
    log <- gs_read(gs) %>%
      filter(poziom==input$level)
    
    if(nrow(log)>0){
      top <- max(log$ruchy)
    } else{
      top <- "-"
    }
    
    # game object
    
    game$data <- list(database=database,
                      map=d,
                      clicks=0,
                      click1="",
                      click2="",
                      points=0,
                      gs=gs,
                      top=top,
                      end_game=0)
    
  })
  
  observeEvent(input$newGame, {
    
    new_game()
    
  })
  
  render_map <- reactive({
    
    ggplot(game$data$map, aes(x=x, y=y)) + 
      geom_tile(fill="#e5f5f9", colour = "gray80") +
      geom_fit_text(aes(label = label_show, colour = colour), reflow = T, min.size = 6) +
      theme_minimal() +
      theme(axis.title = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            panel.grid = element_blank(),
            legend.position = "none")
    
  })
  
  output$distPlot <- renderPlot({
    
    if(!is.null(game$data)){
        render_map()
    }
    
  })
  
  # output$info <- renderText({
  #   paste0("x=", input$plot_click$x, "\ny=", input$plot_click$y, "\nval: ", clicked_value(), 
  #          "\nclicks: ", game$data$clicks, "\nc1: ", game$data$click1, "\nc2: ", game$data$click2)
  # })
  
  clicked_value <- reactive({
    
    if(!is.null(game$data)){
      if(game$data$clicks != 2){
        
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
      
      if(game$data$clicks == 0){
        game$data$click1 <- tile_val
        
        if(!is_already_clicked(tile_val)){
          click <- game$data$clicks
          game$data$clicks <- click + 1
          game$data$map$label_show[game$data$map$label == tile_val] <- tile_val
        }
        
      } else if (game$data$clicks == 1){
        game$data$click2 <- tile_val
        
        if(!is_already_clicked(tile_val)){
          click <- game$data$clicks
          game$data$clicks <- click + 1
          game$data$map$label_show[game$data$map$label == tile_val] <- tile_val
        }
        
        if(game$data$clicks == 2){
          
          df <- game$data$database[game$data$database$cz1 %in% c(game$data$click1, game$data$click2) & 
                                 game$data$database$cz2 %in% c(game$data$click1, game$data$click2),]
          
          points <- game$data$points
          game$data$points <- points + 1
          
          if(nrow(df) != 1){
            
            delay(2500, clear_map())
            
          } else {
            
            df_labels <- game$data$map[game$data$map$label_show == "",]
            
            game$data$clicks <- 0
            
            if(nrow(df_labels) == 0){
              
              # log
              
              log <- data.frame(data=Sys.Date(),
                                poziom=input$level,
                                ruchy=game$data$points)
              
              gs <- game$data$gs
              
              # gs_edit_cells(gs, ws = "log", input = log, trim = TRUE)
              gs_add_row(gs, ws = "log", input = log)
              
              game$data$end <- 1
              
              delay(1500, showModal(modalDialog(title = "Koniec gry", 
                                    paste0("Gratulacje! Twój wynik to: ", game$data$points, " ruchów."), 
                                    footer = modalButton("Zamknij"))))
              
            }
            
          }
          
        }
        
      } 
      
    }
    
  })
  
  clear_map <- reactive({
    
    game$data$map$label_show[game$data$map$label == game$data$click1] <- ""
    game$data$map$label_show[game$data$map$label == game$data$click2] <- ""
    
    game$data$clicks <- 0
    
  })
  
  output$moves <- renderInfoBox({

    moves <- ""

    if(!is.null(game$data)){
      moves <- game$data$points
    }

    infoBox("Ruchy", moves, icon = icon("bullseye"), color = "blue")
  })
  
  output$top <- renderInfoBox({

    top <- "-"

    if(!is.null(game$data)){
      
      if(game$data$end==0){
        top <- game$data$top
      } else{
        
        log <- gs_read(gs) %>%
          filter(poziom==input$level)
        
        if(nrow(log)>0){
          top <- max(log$ruchy)
        }  
      }
      
      
    }

    infoBox("Rekord", top, icon = icon("bullseye"), color = "blue")
  })
  
  
})
