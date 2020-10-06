#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @import ggpubr
#' @import rtiff
#' @noRd

options(shiny.maxRequestSize=30*1024^2) 

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
      d <- dane_2
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
                    threshold = input$threshold, lapse = input$lapse, back = input$background)
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
    
    
    x[[1]]$cell <- input$id
    x[[1]]$strain <- input$szczep
    
    usun_kompleksy <- sub(' ', '', unlist(stringr::str_split(input$usun, ',')))
    
    x[[1]] <- subset(x[[1]], !(id %in% usun_kompleksy))
    x[[1]]$parameters <- paste(input$sigma, input$procent, input$threshold, input$markov,
                              input$usun, sep = '_')
    
    x[[1]] %>% dplyr::arrange(time, dist_tip) %>%
      dplyr::group_by(time) %>%
      dplyr::mutate(dist_between = dist_tip - dplyr::lag(dist_tip),
                    n_comp = dplyr::n(),
                    number_comp = 1:dplyr::n()) -> x[[1]]
    
    return(x)
    
  })
  
  
  
  
  output$wykres <- renderPlot ({
    
    wyn <- wynik()
    
    dane_2 <- wyn[[2]]
    dane_1 <- wyn[[1]]
    
    dane_2 %>% dplyr::filter(time >= input$filtr_czas[1], time <= input$filtr_czas[2]) -> dane_2
    dane_1 %>% dplyr::filter(time >= input$filtr_czas[1], time <= input$filtr_czas[2]) -> dane_1
    
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
                           skala = input$ridges_scale,
                           reverse = as.logical(input$odwroc)
    )
    
    return(p)
  })
  
  output$ridges <- renderPlot({plot_ridges()})
  
  # download hyphae plot
  output$download_data <- downloadHandler(
    
    filename = function() {
      paste('wynik', input$id, '_', input$szczep, '.txt', sep = '')
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
    
    min_czas <- min(dane$czas)-input$lapse
    max_czas <- max(dane$czas)-input$lapse
    
    sliderInput('filtr_czas', 'Choose time range for plots filtering', min_czas, max_czas, value = c(min_czas, max_czas))
    
  })
  
  #load tiff file
  
  image_tiff <- reactive({
    
    inFile <- input$image_file
    
    tiff <- bioimagetools::readTIF(inFile$datapath)
    
    return(tiff)
  })
  
  #plot tiff
  
  output$plot_tiff <- renderPlot({
    
    tiff <- image_tiff()
    
    if(input$display_all == FALSE){
    
    pixmap::plot(tiff[,,input$channel,input$frame]*(1/mean(tiff[,,input$channel,])))
    } else {
      
      norm <- (1/mean(tiff[,,input$channel,])/2)
      
      EBImage::display(EBImage::rotate(tiff[,,input$channel,]*norm, angle = 90), method = 'raster', all = TRUE)
      
    }
  })
  
  #### Code for analysis of multiple hyphae/strains
  
  
  # load multiple files into shiny using data.table and lapply
  dane_porownanie <-reactive({
    data.table::rbindlist(lapply(input$wyniki$datapath, read.table),
                          use.names = TRUE, fill = TRUE)
  })
  
  output$tabela_wyniki <- DT::renderDataTable(dane_porownanie()[,-c(4,7:8,16)], options = list(pageLength = 50))
  
  
  # create summary table for all data
  podsumowanie <- reactive({
    
    dane <- dane_porownanie()
    
    dane %>% dplyr::filter(number_comp <= input$n_kompl) -> dane
    
    if(input$summ_type == 'strain'){
      
      dane %>%
        dplyr::group_by(strain) -> dane
      
      dane %>% dplyr::group_by(strain, cell) %>%
        dplyr::summarise(length = max(length),
                         time = max(time)) %>%
        dplyr::group_by(strain) %>%
        dplyr::summarise(max_length = mean(length),
                         max_time = mean(time)) -> dane_podsum_3
      
    } else if(input$summ_type == 'hyphae'){
      
      dane %>%
        dplyr::group_by(strain, cell) -> dane
      
      dane %>% dplyr::group_by(strain, cell) %>%
        dplyr::summarise(max_length = max(length),
                         max_time = max(time)) -> dane_podsum_3 
    } else if(input$summ_type == 'komp'){
      
      dane %>% dplyr::group_by(strain, number_comp) -> dane
      
      dane %>% dplyr::group_by(strain, cell) %>%
        dplyr::summarise(length = max(length),
                         time = max(time)) %>%
        dplyr::group_by(strain) %>%
        dplyr::summarise(max_length = mean(length),
                         max_time = mean(time)) -> dane_podsum_3
      
    }
    
    dane %>% dplyr::summarise(mean_dist_tip = mean(dist_tip),
                              sd_dist_tip = sd(dist_tip),
                              # mean_dist_base = mean(dist_base),
                              # sd_dist_base = sd(dist_base),
                              mean_dist_between = mean(dist_between, na.rm = TRUE),
                              sd_dist_between = sd(dist_between, na.rm = TRUE),
                              n = dplyr::n()) -> dane_podsum
    
    dane %>% dplyr::distinct(strain, cell, index, .keep_all = TRUE) %>%
      dplyr::summarise(mean_n_comp = mean(n_comp)) -> dane_podsum_2
    
    dane_podsum %>% dplyr::left_join(dane_podsum_2) %>% dplyr::left_join(dane_podsum_3) -> 
      dane_podsum
    
    
    return(dane_podsum)
    
  })
  
  # show table with all data
  output$tabela_podsumowanie <- renderTable(podsumowanie())
  
  
  histogramInput <- reactive({
    
    wb <- dane_porownanie()
    
    wb %>% dplyr::filter(number_comp <= input$n_kompl) -> wb
    
    if(input$bin == 0){
      
      bin <- mean(wb[,input$os_x_hist], na.rm = TRUE) / 10
    } else {
      
      bin <- input$bin
    }
    
    p <- EDA::draw_histogram(wb = wb,
                             variable = input$os_x_hist,
                             facet_draw = input$facet,
                             facet_var = input$color_hist,
                             bin = bin,
                             y_density = input$os_y,
                             x_name = input$os_x,
                             y_name = input$os_y_nazwa,
                             kolory = input$kolory_hist,
                             viridis = input$viridis_hist,
                             brewer = input$colorbrewer_hist,
                             wlasne = input$wlasne_kolory_hist)
    
    print(p)
  })
  
  densityInput <- reactive({
    wb <- dane_porownanie()
    
    wb %>% dplyr::filter(number_comp <= input$n_kompl) -> wb
    
    p <- EDA::draw_density(wb = wb,
                           variable = input$os_x_hist,
                           color_var = input$color_hist,
                           fill = input$fill_dens,
                           x_name = input$os_x_dens,
                           y_name = input$os_y_dens,
                           kolory = input$kolory_dens,
                           viridis = input$viridis_dens,
                           brewer = input$colorbrewer_dens,
                           wlasne = input$wlasne_kolory_dens)
    
    print(p)
    
  })
  
  boxplotInput <- reactive({
    
    wb <- dane_porownanie()
    
    
    wb %>% dplyr::filter(number_comp <= input$n_kompl) -> wb
    wb <- as.data.frame(wb)
    
    p <- EDA::draw_boxplot(wb = wb,
                           x_var = input$os_x_box,
                           y_var = input$os_y_box,
                           type = input$boxviolin,
                           p_format = input$p_format,
                           porownanie = input$porownanie,
                           punkty = input$punkty,
                           anova = input$anova,
                           test_type = input$rodzaj_test,
                           kontrola = input$kontrola,
                           grupy_porownania = input$porownania,
                           x_name = input$os_x_box,
                           y_name = input$os_y_box,
                           kolory = input$kolory,
                           viridis = input$viridis,
                           brewer = input$colorbrewer,
                           wlasne = input$wlasne)
    
    return(p)
    
  })
  
  scatterInput <- reactive({
    
    wb <- dane_porownanie()
    wb %>% dplyr::filter(number_comp <= input$n_kompl) %>%
      dplyr::mutate(number_comp = factor(number_comp))-> wb
    
    p <- EDA::draw_scatter(wb = wb,
                           x_var = input$os_x_scatter,
                           y_var = input$os_y_scatter,
                           color_var = input$os_color_scatter,
                           facet_var = input$os_facet_scatter,
                           trend = input$trend,
                           size_trend = input$size_trend,
                           model = input$rodzaj_trend,
                           span = input$span,
                           se = input$se,
                           alpha = input$alpha_point,
                           size_point = input$size_point,
                           kolory = input$kolory_scatter,
                           viridis = input$viridis_scatter,
                           brewer = input$colorbrewer_scatter,
                           wlasne = input$wlasne_kolory_scatter)
    
    return(p)
    
  })
  
  
  output$wykres_podsumowanie <- renderPlot({
    if (is.null(input$wyniki))
      return(NULL)
    if(input$rodzaj_wykres_summ == 'hist'){
      print(histogramInput())
    } else if(input$rodzaj_wykres_summ == 'density'){
      print(densityInput())
    } else if(input$rodzaj_wykres_summ == 'box'){
      print(boxplotInput())
    } else if(input$rodzaj_wykres_summ == 'scatter'){
      print(scatterInput())
    }
  })
  
  final_scatter <- reactive ({
    
    dane <- dane_porownanie()
    
    dane %>% dplyr::filter(number_comp <= input$n_kompl) %>%
      dplyr::mutate(number_comp = factor(number_comp))-> dane
    
    grupy <- colnames(dane)
    
    numer_1 <- which(grupy == input$os_x_scatter)
    numer_2 <- which(grupy == input$os_y_scatter)
    
    numery <- c(numer_1, numer_2)
    
    if(input$os_color_scatter != 'brak'){
      numer_3 <- which(grupy == input$os_color_scatter)
      numery <- c(numery, numer_3)
    }
    
    if(input$os_facet_scatter != 'brak'){
      numer_4 <- which(grupy == input$os_facet_scatter)
      numery <- c(numery, numer_4)
    }
    
    dane <- dane[,numery]
    
    return(dane)
  })
  
  #output$scatter <- renderTable(final_scatter())
  
  tabela_korelacja <- reactive({
    
    dane <- final_scatter()
    
    # nazwy <- c('x', 'y')
    
    if(input$os_color_scatter == 'brak' & input$os_facet_scatter == 'brak'){
      colnames(dane) <- c('x', 'y')
      
      dane %>% tidyr::nest(data = tidyr::everything()) -> nested
    }
    
    if(input$os_color_scatter != 'brak' & input$os_facet_scatter == 'brak'){
      colnames(dane) <- c('x', 'y', 'grupa1')
      
      dane %>% tidyr::nest(data = -grupa1) -> nested
    }
    
    if(input$os_color_scatter == 'brak' & input$os_facet_scatter != 'brak'){
      colnames(dane) <- c('x', 'y', 'grupa2')
      
      dane %>% tidyr::nest(data = -grupa2) -> nested
    }
    
    if(input$os_color_scatter != 'brak' & input$os_facet_scatter != 'brak'){
      colnames(dane) <- c('x', 'y', 'grupa1', 'grupa2')
      
      dane %>% tidyr::nest(data = -c(grupa1, grupa2)) -> nested
    }
    
    nested %>% 
      dplyr::mutate(test = purrr::map(data, ~ cor.test(.x$x, .x$y, method = input$corr)), # S3 list-col
                    tidied = purrr::map(test, broom::tidy)
      ) %>% 
      tidyr::unnest(tidied) %>% dplyr::select(-data, -test) -> wynik
    
    return(wynik)
    
  }) 
  
  output$tabela_korelacja <- renderTable({
    if(input$corr == 'nie'){
      return(NULL)
    }
    
    tabela_korelacja()
    
  })
  
  
  tabela_lm <- reactive({
    
    dane <- final_scatter()
    
    # nazwy <- c('x', 'y')
    
    if(input$os_color_scatter == 'brak' & input$os_facet_scatter == 'brak'){
      colnames(dane) <- c('x', 'y')
      
      dane %>% tidyr::nest(data = tidyr::everything()) -> nested
    }
    
    if(input$os_color_scatter != 'brak' & input$os_facet_scatter == 'brak'){
      colnames(dane) <- c('x', 'y', 'grupa1')
      
      dane %>% tidyr::nest(data = -grupa1) -> nested
    }
    
    if(input$os_color_scatter == 'brak' & input$os_facet_scatter != 'brak'){
      colnames(dane) <- c('x', 'y', 'grupa2')
      
      dane %>% tidyr::nest(data = -grupa2) -> nested
    }
    
    if(input$os_color_scatter != 'brak' & input$os_facet_scatter != 'brak'){
      colnames(dane) <- c('x', 'y', 'grupa1', 'grupa2')
      
      dane %>% tidyr::nest(data = -c(grupa1, grupa2)) -> nested
    }
    
    nested %>% 
      dplyr::mutate(fit = purrr::map(data, ~ lm(y~ x, data = .x)), # S3 list-col
                    tidied = purrr::map(fit, broom::tidy)
      ) %>% 
      tidyr::unnest(tidied) %>% dplyr::select(-data, -fit) -> wynik
    
    return(wynik)
    
  }) 
  
  output$tabela_lm <- renderTable({
    if(input$rodzaj_trend == 'lm'){
      tabela_lm()
    } else {
      return(NULL)
    }
    
    
    
  },
  digits = -3)
  
  
  
  dane_download <- reactive({
    wb <- dane_porownanie()
    wb %>% dplyr::filter(number_comp <= input$n_kompl) -> wb
    return(wb)
  })
  
  
  # download data from second tab - bound together from many files
  output$download_data_all <- downloadHandler(
    
    filename = function() {
      paste('wyniki_all', '.txt', sep = '')
    },
    content = function(file) {
      write.table(dane_download(), file)
    }
    
  )
  
  getactiveplot <- reactive({
    
    if(input$rodzaj_wykres_summ == 'hist'){
      return(histogramInput())
    } else if(input$rodzaj_wykres_summ == 'density'){
      return(densityInput())
    } else if(input$rodzaj_wykres_summ == 'box'){
      return(boxplotInput())
    } else if(input$rodzaj_wykres_summ == 'scatter'){
      return(scatterInput())
    }
    
    
  })
  
  output$download_plot <- downloadHandler(
    filename = function() { paste(input$dataset, '.png', sep='') },
    content = function(file) {
      png(file, res = input$res_plot, width = input$width_plot, input$height_plot, unit = 'cm')
      print(getactiveplot())
      dev.off()
    })
  
}
