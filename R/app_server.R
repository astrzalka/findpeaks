#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  # List the first level callModules here
  
  library(Peaks)
  library.dynam('Peaks', 'Peaks', lib.loc=NULL) 
  library(magrittr)
  
  dane <- reactive({
    # validate(
    #   need(input$dane != '', 'Proszę wybierz dane')
    # )
    
    if(input$example == TRUE){
      d <- dane_1
      return(d)
    }
    
    inFile <- input$dane
    if (is.null(inFile))
      return(NULL)
    
    inFile <- input$dane
    d <- read.table(inFile$datapath, header=input$header)
    colnames(d) <- c('V1', 'V2')
    return(d)
    
    
  }) 
  
  #   punkt <- reactiveValues(p = NULL)
  #   
  #   ile <- reactiveValues(i = 0)
  #   
  #   observeEvent(input$dodaj, {
  #     
  #     punkt$p <- input$plot_click
  #     
  #   })
  #   
  #   observeEvent(input$dodaj, {
  #     
  #     ile$i <- ile$i + 1
  #     
  #   })
  #   
  #   output$test <- renderText({
  #     
  #     if(!is.null(punkt$p)){
  #     
  #     print(c('test ', ile$i))
  #     } else {
  #       print('brak')
  #     }
  #     
  #   })
  #   
  
  output$dane_raw <- renderTable(dane())
  
  
  wynik <- reactive({
    
    x <- find_peaks(dane(), s = input$sigma, procent = input$procent, m = as.logical(input$markov), 
                    threshold = input$threshold, lapse = input$lapse)
    #     print('nie')
    #     print(nrow(x[[1]]))
    #    
    #      
    #      if(!is.null(punkt$p)){
    #         
    #         nr <- (nrow(x[[1]]) + ile$i)
    #         
    #         x[[1]][nr,2] <- punkt$p$x
    #         x[[1]][nr,9] <- punkt$p$panelvar1
    #         
    #         print('tak')
    #         print(nr)
    #       }
    
    
    x[[1]]$komorka <- input$id
    x[[1]]$szczep <- input$szczep
    
    usun_kompleksy <- sub(' ', '', unlist(stringr::str_split(input$usun, ',')))
    
    x[[1]] <- subset(x[[1]], !(id %in% usun_kompleksy))
    x[[1]]$parametry <- paste(input$sigma, input$procent, input$threshold, input$markov,
                              input$usun, sep = '_')
    return(x)
    
  })
  
  
  
  
  output$wykres <- renderPlot ({
    
    wyn <- wynik()
    
    p <- plot_find_peaks(wyn[[2]], wyn[[1]])
    
    #p_ly <- plotly::ggplotly(p)
    print(p)
    
  })
  
  output$tabela <- renderTable({
    wyn <- wynik()
    
    wyn[[1]]
    
  })
  
  output$strzepka <- renderPlot({
    
    wyn <- wynik()
    
    p <- plot_scheme_find_peaks(wyn[[1]], odwroc = input$odwroc, color_point = input$punkt)
    
    print(p)
    
  })
  
  
  output$kymograf <- renderPlot({
    
    wyn <- wynik()
    
    dane1 <- dane()
    
    p <- plot_kymograph_find_peaks(dane_raw = dane1, dane_find = wyn[[1]], odwroc = input$odwroc, 
                                   pokaz = input$pokaz, color_point = input$punkt, 
                                   color_gradient = input$gradient, lapse = input$lapse)
    print(p)
    
  })
  
  plot_ridges <- reactive({
    
    dane1 <- dane()
    
    p <- plot_peaks_ridges(data = dane1, 
                           scale = input$norm_ridges,
                           gradient = input$gradient_ridges,
                           skala = input$ridges_scale
                           )
    
    return(p)
  })
  
  output$ridges <- renderPlot({plot_ridges()})
  
  # download hyphae plot
  output$download_data <- downloadHandler(
    
    filename = function() {
      paste('wynik', input$id, '.txt', sep = '')
    },
    content = function(file) {
      write.table(wynik(), file)
    }
    
  )
  
  
}
