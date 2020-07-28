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
    
    dane_2 <- wyn[[2]]
    dane_1 <- wyn[[1]]
    
    dane_2 %>% dplyr::filter(czas >= input$filtr_czas[1], czas <= input$filtr_czas[2]) -> dane_2
    dane_1 %>% dplyr::filter(czas >= input$filtr_czas[1], czas <= input$filtr_czas[2]) -> dane_1
    
    p <- plot_find_peaks(dane_2, dane_1)
    
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
      write.table(wynik()[[1]], file)
    }
    
  )
  
  # slider for filtering the plots in all plots
  # takes time from raw data, otherwise ot will update every time the wynik is changed
  output$filtr_czas <- renderUI({
    
    dane <- dane()
    dane <- dodaj_ind(dane)
    dane$czas <- dane$ind * input$lapse
    
    min_czas <- min(dane$czas)
    max_czas <- max(dane$czas)
    
    sliderInput('filtr_czas', 'Podaj zakres czasu - filtrowanie wykresów', min_czas, max_czas, value = c(min_czas, max_czas))
    
  })
  
  #### Code for analysis of multiple hyphae/strains
  
  
  # load multiple files into shiny using data.table and lapply
  dane_porownanie <-reactive({
    data.table::rbindlist(lapply(input$wyniki$datapath, read.table),
                          use.names = TRUE, fill = TRUE)
  })
  
  output$tabela_wyniki <- renderTable(dane_porownanie())
  
  
  # create summary table for all data
  podsumowanie <- reactive({
    
    dane <- dane_porownanie()
    
    dane %>% dplyr::filter(numer_chrom <= input$n_kompl) -> dane
    
    if(input$summ_type == 'szczep'){
      
      dane %>%
        dplyr::group_by(szczep) -> dane
      
      dane %>% dplyr::group_by(szczep, komorka) %>%
        dplyr::summarise(dlugosc = max(dlug),
                         czas = max(czas)) %>%
        dplyr::group_by(szczep) %>%
        dplyr::summarise(max_dlugosc = mean(dlugosc),
                         max_czas = mean(czas)) -> dane_podsum_3
      
    } else if(input$summ_type == 'hyphae'){
      
      dane %>%
        dplyr::group_by(szczep, komorka) -> dane
      
      dane %>% dplyr::group_by(szczep, komorka) %>%
        dplyr::summarise(max_dlugosc = max(dlug),
                         max_czas = max(czas)) -> dane_podsum_3 
    } else if(input$summ_type == 'komp'){
      
      dane %>% dplyr::group_by(szczep, numer_chrom) -> dane
      
      dane %>% dplyr::group_by(szczep, komorka) %>%
        dplyr::summarise(dlugosc = max(dlug),
                         czas = max(czas)) %>%
        dplyr::group_by(szczep) %>%
        dplyr::summarise(max_dlugosc = mean(dlugosc),
                         max_czas = mean(czas)) -> dane_podsum_3
      
    }
    
    dane %>% dplyr::summarise(mean_dist_tip = mean(dist_tip),
                              sd_dist_tip = sd(dist_tip),
                              # mean_dist_base = mean(dist_base),
                              # sd_dist_base = sd(dist_base),
                              mean_dist_pom = mean(dist_pom, na.rm = TRUE),
                              sd_dist_pom = sd(dist_pom, na.rm = TRUE),
                              n = dplyr::n()) -> dane_podsum
    
    dane %>% dplyr::distinct(szczep, komorka, indeks, .keep_all = TRUE) %>%
      dplyr::summarise(mean_n_chrom = mean(n_chrom)) -> dane_podsum_2
    
    dane_podsum %>% dplyr::left_join(dane_podsum_2) %>% dplyr::left_join(dane_podsum_3) -> 
      dane_podsum
    

    return(dane_podsum)
    
  })
  
  # show table with all data
  output$tabela_podsumowanie <- renderTable(podsumowanie())
  
}
