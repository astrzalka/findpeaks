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
    
    return(x)
    
  })
  
  
  
  
  output$wykres <- renderPlot ({
    
    wyn <- wynik()
    
    p <- ggplot2::ggplot(wyn[[2]])
    p <- p+ggplot2::geom_line(ggplot2::aes(x = x, y = y), color = "red")+
      ggplot2::facet_wrap(~czas, scales = "free")+
      ggplot2::geom_line(ggplot2::aes(x = x, y = int))+
      ggplot2::theme_bw()+
      ggplot2::geom_point(data = wyn[[1]], ggplot2::aes(x = dist_tip, y = 1), color = "forestgreen", size = 3, shape = 3)
    p
  })
  
  output$info <- renderText({
    paste0("x=", input$plot_click$x, "\ny=", input$plot_click$y)
  })
  
  output$tabela <- renderTable({
    wyn <- wynik()
    
    wyn[[1]]
    
  })
  
  output$strzepka <- renderPlot({
    
    wyn <- wynik()
    if(as.logical(input$odwroc == TRUE)){
      p <- ggplot2::ggplot(wyn[[1]],  ggplot2::aes(y=dist_tip, x=czas))
      p <- p + ggplot2::geom_bar(ggplot2::aes(x=czas, y=dlug), stat="identity", fill="snow3", color="black", 
                    position="dodge", width=5)+
        ggplot2::geom_point(color=input$punkt, ggplot2::aes(size=int_raw))+
        ggplot2::scale_size_continuous(range=c(1,6), "Fluorescence\nintensity")+
        ggplot2::theme_bw()+
        ggplot2::ylab(expression(paste("Length [", mu, "m]")))+
        ggplot2::xlab("Time [min]")
      p
    } else {
      p <- ggplot2::ggplot(wyn[[1]],  ggplot2::aes(y=dist_base, x=czas))
      p<-p+ggplot2::geom_bar(ggplot2::aes(x=czas, y=dlug), stat="identity", fill="snow3", color="black", 
                    position="dodge", width=5)+
        ggplot2::geom_point(color=input$punkt, ggplot2::aes(size=int_raw))+
        ggplot2::scale_size_continuous(range=c(1,6), "Fluorescence\nintensity")+
        ggplot2::theme_bw()+
        ggplot2::ylab(expression(paste("Length [", mu, "m]")))+
        ggplot2::xlab("Time [min]")
      p
      
    }
  })
  
  
  output$kymograf <- renderPlot({
    
    wyn <- wynik()
    
    dane1 <- dane()
    
    dane1 <- dodaj_ind(dane1)
    
    dane1 <- dane1 %>% dplyr::group_by(ind) %>% dplyr::mutate(V3 = rev(V1))
    
    env <- environment()
    
    p <- ggplot2::ggplot(dane1, environment = env)
    
    if(as.logical(input$odwroc == TRUE)){
      if(as.logical(input$pokaz) == TRUE){
        p <- p + ggplot2::geom_tile(ggplot2::aes(x = (input$lapse + ind*input$lapse), y = V1, fill = V2))+
          ggplot2::geom_point(data = wyn[[1]], ggplot2::aes(x = (input$lapse + indeks*input$lapse), y = dist_tip), 
                     color = input$punkt, size = 3)+
          ggplot2::scale_fill_gradient("Fluorescence\nintensity", low = "black", high = input$gradient)+
          ggplot2::theme_bw()+
          ggplot2::ylab(expression(paste("Length [", mu, "m]")))+
          ggplot2::xlab("Time [min]")
      } else {
        p <- p + ggplot2::geom_tile(ggplot2::aes(x = (input$lapse + ind*input$lapse), y = V1, fill = V2))+
          ggplot2::scale_fill_gradient("Fluorescence\nintensity", low = "black", high = input$gradient)+
          ggplot2::theme_bw()+
          ggplot2::ylab(expression(paste("Length [", mu, "m]")))+
          ggplot2::xlab("Time [min]")
      }
    } else {
      
      if(as.logical(input$pokaz) == TRUE){
        p <- p + ggplot2::geom_tile(ggplot2::aes(x = (input$lapse + ind*input$lapse), y = V3, fill = V2))+
          ggplot2::geom_point(data = wyn[[1]], ggplot2::aes(x = (input$lapse + indeks*input$lapse), y = dist_base), 
                     color = input$punkt, size = 3)+
          ggplot2::scale_fill_gradient("Fluorescence\nintensity", low = "black", high = input$gradient)+
          ggplot2::theme_bw()+
          ggplot2::ylab(expression(paste("Length [", mu, "m]")))+
          ggplot2::xlab("Time [min]")
      } else {
        p <- p + ggplot2::geom_tile(ggplot2::aes(x = (input$lapse + ind*input$lapse), y = V3, fill = V2))+
          ggplot2::scale_fill_gradient("Fluorescence\nintensity", low = "black", high = input$gradient)+
          ggplot2::theme_bw()+
          ggplot2::ylab(expression(paste("Length [", mu, "m]")))+
          ggplot2::xlab("Time [min]")
      }
    }
    p
  })
  
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
