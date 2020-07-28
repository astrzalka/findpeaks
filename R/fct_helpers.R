#' Funkcja do szukania pików fluorescencji w danych z time lapse
#'
#' @param ramka tabela z danymi (kolumna 1 - odległości, 2 - intensywność fluorescencji)
#' @param s sigma - dokładność dopasowania pików, im wyższa tym mniej znajdzie
#' @param m jak ma wygładzać dane, przy FALSE znajduje więcej pików
#' @param procent  ile tła ma odjąć, rozsądny zakres to od 1(całość) do 0.01 (1 procent)
#' @param threshold powyżej jakiego poziomu ma zaznaczać piki, procent wysokości najwyższego piku
#' @param back lepiej zostawić FALSE 
#' @param lapse czas pomiędzy klatkami
#' @param ... 
#'
#' @return ramkę danych zawierającą pozycje wszystkich pików razem z czasem i intensywnośćią fluorescencji
#' @export
#'
#' @examples
find_peaks <- function (ramka, s = 2, m = FALSE, procent = 1, threshold=10, 
                        back=FALSE, lapse = 10, ...) { 
  
  ramka <- dodaj_ind(ramka)
  
  # sprawdzamy ile jest klatek
  ramka[,3]<-as.factor(ramka[,3])
  n <- nlevels(ramka[,3])
  # jakie są poziomy
  poziomy <- levels(ramka[,3])
  # robimy pętlę osobno dla każdej klatki
  for (i in 1:n) {
    # wybieramy klatkę
    x <- subset(ramka, ramka[,3] == poziomy[i])
    x1 <- x
    # normalizacja przez baseline (moda)
    baza <- modeest::mlv(x[,2], method="shorth")
    # jaki procent tła ma odjąć
    baza2 <- baza[[1]] * procent
    # odejmujemy wartość baseline od intensywności i dzielimy przez baseline
    x[,2]<-(x[,2]-baza2[1])/baza2[1]
    #zamieniamy wartości ujemne na zera
    x[,2]<-replace(x[,2], x[,2]<0, 0)
    
    # szukanie pików
    piki<-Peaks::SpectrumSearch(x[,2], sigma=s, markov=m, threshold=threshold, background=back)
    # rysuje wykres z zaznaczonymi pikami, jeżeli plot == TRUE
    if (length(piki[[1]]) > 0){
      # przygotowuje tabelę z wynikami
      wynik<-miejsca<-data.frame(dist_base=max(x[,1])-x[piki[[1]],1], # odległość od podstawy strzępki
                                 dist_tip =x[piki[[1]],1], # odległość od tipa
                                 int_raw = x1[piki[[1]],2],
                                 int_nor=round((x1[piki[[1]],2]-baza[[1]])/baza[[1]]), # intensywność fluorescencji zaokrąglona do liczby całkowitej
                                 dlug = max(x[,1]), # długość strzępki
                                 indeks = i,
                                 tlo = baza[[1]],
                                 tlo_nor = baza2[1],
                                 czas = (i * lapse)-lapse) # kolejna klatka
    } else { wynik <- NULL}
    # jeżeli obrót pętli inny niż jeden to dopisujemy wyniki do poprzednich
    if (i == 1){ wynik_kon = wynik
    wynik2 = data.frame(y = piki[[2]], czas = (i * lapse)-lapse, x = x$V1, int = x$V2)
    } else {
      wynik_kon <-rbind(wynik_kon, wynik)
      dane <- data.frame(y = piki[[2]], czas = (i * lapse)-lapse, x = x$V1, int = x$V2)
      wynik2 <- rbind(wynik2, dane)
    }
    
  }
  
  # sortuje wyniki najpierw według klatek, potem według odległości od tipa
  wynik_kon <- dplyr::arrange(wynik_kon, indeks, dist_tip) %>%
    dplyr::mutate(id = 1:dplyr::n()) 
  # zwraca wynik
  return(list(wynik = wynik_kon, wynik2 = wynik2))
}

#' Rysuje wykres ggplot, komórki jako słupki, kompleksy jako kropki
#'
#' @param wynik wynik funkcji find_peaks
#' @param int czy intensywność ma być znormalizowana
#' @param zaznacz czy ma zaznaczać najsilniejszy kompleks
#' @param size tylko dla int_nor, czy wielkość kompleksu ma odpowiadać intensywności, FALSE - gradient kolorów
#'
#' @return obiekt ggplot
#' @export
#'
#' @examples
wykres_str <- function (wynik, int = c("int_nor","int_raw"), zaznacz=FALSE, size = TRUE) {
  p<-ggplot(data=wynik, aes(y=dist_base, x=indeks))
  if ( zaznacz == TRUE){    
    if (int == "int_raw"){
      test <- arrange(wynik, indeks, desc(int_raw))
      test <- test %>% group_by(indeks) %>% mutate(order=order(int_raw, decreasing=T))
      test<-subset(test, order == 1)
      
      p<-p+geom_bar(aes(x=indeks, y=dlug), stat="identity", fill="snow3", color="black", 
                    position="dodge", width=0.4)+
        geom_point(color="green3", aes(size=int_raw))+
        geom_point(data=test, aes(x=indeks, y=dist_base),color="red3", shape=4, size=5)+
        scale_size_continuous(range=c(1,6))
      print(p)
    }
    if (int == "int_nor"){
      test <- arrange(wynik, indeks, desc(int_nor))
      test <- test %>% group_by(indeks) %>% mutate(order=order(int_nor, decreasing=T))
      test<-subset(test, order == 1)
      
      p<-p+geom_bar(aes(x=indeks, y=dlug), stat="identity", fill="snow3", color="black", 
                    position="dodge", width=0.4)+
        geom_point(color="green3", aes(size=factor(int_nor)))+
        geom_point(data=test, aes(x=indeks, y=dist_base),color="red3", shape=4, size=5)+
        scale_size_discrete(range=c(1,6))
      print(p)
    }
  }else{
    
    if (int == "int_raw"){
      p<-p+geom_bar(aes(x=indeks, y=dlug), stat="identity", fill="snow3", color="black", 
                    position="dodge", width=0.4)+
        geom_point(color="green3", aes(size=int_raw))+
        scale_size_continuous(range=c(1,6))
      
    }
    if (int == "int_nor"){
      
      if ( size == TRUE){
        wynik$int_nor <- factor(wynik$int_nor, levels=c(0:36))
        
        p<-ggplot(data=wynik, aes(y=dist_base, x=indeks))
        
        p<-p+geom_bar(aes(x=indeks, y=dlug), stat="identity", fill="snow3", color="black", 
                      position="dodge", width=0.4)+
          geom_point(color="green3", aes(size=int_nor))+
          scale_size_manual(values=seq(3, 12, by=(7/37)))
      } else {
        
        p<-ggplot(data=wynik, aes(y=dist_base, x=indeks))
        
        p<-p+geom_bar(aes(x=indeks, y=dlug), stat="identity", fill="snow3", color="black", 
                      position="dodge", width=0.4)+
          geom_point( aes(color=int_nor), size=4)+scale_colour_gradient(low="darkgreen", high="yellow")
      }
    }
    return(p)
  }
}

#' Uzupełnia indeksy w danych, które potem można użyć w funkcji find_peaks
#'
#' @param dane ramka danych z dwiema kolumnami: długość, intensywność fluorescencji
#'
#' @return ramkę danych z dodaną kolumną ind
#' @export
#'
#' @examples
dodaj_ind<-function(dane){
  # ilość wierszy w danych
  n <- nrow(dane)
  # j - będą kolejne numery klatek
  j <- 0
  # dodajemy nową kolumnę
  dane<-data.frame(dane, ind=0)
  for (i in 1:n){
    # zwiększamy j jak pojawi się zero w pierwszej kolumnie
    if (dane[i,1] == 0) {j <- j+1}
    # zamieniamy wartość w kolumnie nr 3 na j
    dane[i,3] = j}
  return(dane)
}


#' Plots results form find_peaks() 
#' Visualizes indetified maxima
#'
#' @param dane_raw fluorescence profiles data
#' @param dane_find data generated by find_peaks
#'
#' @return ggplot of fluorescence maxima
#' @export
#'
#' @examples
plot_find_peaks <- function(dane_raw, dane_find){
  
  dane_raw %>% dplyr::group_by(czas) %>%
    dplyr::summarise(maksimum = max(y)) %>% dplyr::right_join(dane_find, by = 'czas') -> dane_find2
  
  p <- ggplot2::ggplot(dane_raw)
  p <- p+ggplot2::geom_line(ggplot2::aes(x = x, y = y), color = "red")+
    ggplot2::facet_wrap(~czas, scales = "free", ncol = 4)+
    ggplot2::geom_line(ggplot2::aes(x = x, y = int))+
    ggplot2::theme_bw()+
    ggplot2::geom_point(data = dane_find, ggplot2::aes(x = dist_tip, y = 1), 
                        color = "forestgreen", size = 3, shape = 3)+
    ggplot2::geom_text(data = dane_find2, ggplot2::aes(x = dist_tip, y = 1.1*maksimum, label = id),
                       color = 'darkblue')
  
  return(p)
}


#' Rysuje schemat komórki na podstawie wyniku find_peaks
#'
#' @param dane_find wynik funkcji find_peaks
#' @param odwroc gdzie jest "góra" komórki
#'
#' @return schematyczny wykres komórki
#' @export
#'
#' @examples
plot_scheme_find_peaks <- function(dane_find, odwroc = TRUE, color_point = 'red'){
  
  if(as.logical(odwroc == TRUE)){
    p <- ggplot2::ggplot(dane_find,  ggplot2::aes(y=dist_tip, x=czas))
  } else {
    p <- ggplot2::ggplot(dane_find,  ggplot2::aes(y=dist_base, x=czas))
  }
  
  p <- p + ggplot2::geom_bar(ggplot2::aes(x=czas, y=dlug), stat="identity", fill="snow3", color="black", 
                             position="dodge", width=5)+
    ggplot2::geom_point(color=color_point, ggplot2::aes(size=int_raw))+
    ggplot2::scale_size_continuous(range=c(1,6), "Fluorescence\nintensity")+
    ggplot2::theme_bw()+
    ggplot2::ylab(expression(paste("Length [", mu, "m]")))+
    ggplot2::xlab("Time [min]")
  
  return(p)
  
}


#' Rysuje kymograf fluorescencji na podstawie profilu, zaznacza znalezione maksima
#'
#' @param dane_raw profile fluorescencji
#' @param dane_find wynik find_peaks
#' @param odwroc gdzie jest "góra" komórki
#' @param pokaz czy pokazać maksima
#' @param color_point kolor punktów
#' @param color_gradient kolor gradientu, zawsze drugi kolor jest czarny
#' @param lapse czas pomiędzy klatkami
#'
#' @return
#' @export
#'
#' @examples
plot_kymograph_find_peaks <- function(dane_raw, dane_find, odwroc = TRUE, pokaz = TRUE,
                                      color_point, color_gradient, lapse){
  
  dane_raw <- dodaj_ind(dane_raw)
  
  dane_raw <- dane_raw %>% dplyr::group_by(ind) %>% dplyr::mutate(V3 = rev(V1))
  
  p <- ggplot2::ggplot(dane_raw)
  
  if(as.logical(odwroc == TRUE)){
    p <- p + ggplot2::geom_tile(ggplot2::aes(x = (ind*lapse), y = V1, fill = V2))
  } else {
    p <- p + ggplot2::geom_tile(ggplot2::aes(x = (ind*lapse), y = V3, fill = V2)) 
  }
  
  if(as.logical(pokaz) == TRUE){
    if(as.logical(odwroc == TRUE)){
      p <- p + ggplot2::geom_point(data = dane_find, ggplot2::aes(x = (indeks*lapse), y = dist_tip), 
                                   color = color_point, size = 3)
    } else {
      p <- p + ggplot2::geom_point(data = dane_find, ggplot2::aes(x = (indeks*lapse), y = dist_base), 
                                   color = color_point, size = 3)
    }
  }
  
  p <- p + ggplot2::scale_fill_gradient("Fluorescence\nintensity", low = "black", high = color_gradient)+
    ggplot2::theme_bw()+
    ggplot2::ylab(expression(paste("Length [", mu, "m]")))+
    ggplot2::xlab("Time [min]")
  
  return(p)
}


plot_peaks_ridges <- function(data, scale = 'osobno', gradient = TRUE, skala = 2){
  
  dane_raw <- dodaj_ind(data)
  
  dane_raw <- dane_raw %>% dplyr::group_by(ind) %>% dplyr::mutate(V3 = rev(V1))
  
  if(scale == 'osobno'){
    
    dane_raw %>% dplyr::group_by(ind) %>% 
      dplyr::mutate(V2 = V2- min(V2),
                    V2 = V2/max(V2)) -> 
      dane_raw
  }
  
  if(scale == 'razem'){
    
    dane_raw %>% dplyr::ungroup() %>%
      dplyr::mutate(V2 = V2- min(V2),
                    V2 = V2/max(V2))-> 
      dane_raw
  }
  
  p <- ggplot2::ggplot(dane_raw, ggplot2::aes(x = V1, y = factor(ind), height = V2, fill = V2))
  
  if(gradient == TRUE){
    p <- p + ggridges::geom_ridgeline_gradient(scale = skala)+
      ggplot2::scale_fill_viridis_c(direction = -1, guide = 'none')
  } else {
    p <- ggplot2::ggplot(dane_raw, ggplot2::aes(x = V1, y = factor(ind), height = V2))
    p <- p + ggridges::geom_ridgeline(scale = skala, alpha = 0.75)
    
  }
  
  p <- p + ggridges::theme_ridges()+
    ggplot2::xlab('Długość komórki')+
    ggplot2::ylab("Czas")
  
  return(p)
  
  
}
