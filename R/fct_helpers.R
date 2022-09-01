#' Funkcja do szukania pików fluorescencji w danych z time lapse
#'
#' @param ramka tabela z danymi (kolumna 1 - odległości, 2 - intensywność fluorescencji)
#' @param s sigma - dokładność dopasowania pików, im wyższa tym mniej znajdzie
#' @param m jak ma wygładzać dane, przy FALSE znajduje więcej pików
#' @param procent  ile tła ma odjąć, rozsądny zakres to od 1(całość) do 0.01 (1 procent)
#' @param threshold powyżej jakiego poziomu ma zaznaczać piki, procent wysokości najwyższego piku
#' @param back "No" odejmie tylko procent tła, "minimum" zostanie odjęta wartość minimalna, "Peaks" zostanie użyta funkcja obecna w pakiecie Peaks
#' @param lapse czas pomiędzy klatkami
#' @param ... 
#'
#' @return ramkę danych zawierającą pozycje wszystkich pików razem z czasem i intensywnośćią fluorescencji
#' @export
#'
#' @examples
find_peaks <- function (ramka, 
                        s = 2, 
                        m = FALSE, 
                        procent = 1, 
                        threshold=10, 
                        back='No', 
                        lapse = 10, 
                        filter_local = FALSE, 
                        filter_local_int = 1.1, 
                        filter_local_width = 2, ...) { 
  
  library(Peaks)
  library.dynam('Peaks', 'Peaks', lib.loc=NULL) 
  
  #ramka <- dodaj_ind(ramka)
  
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
    
    if(back == 'No' & procent != 0){
      # odejmujemy wartość baseline od intensywności i dzielimy przez baseline
      x[,2]<-(x[,2]-baza2[1])/baza2[1]
      #zamieniamy wartości ujemne na zera
      x[,2]<-replace(x[,2], x[,2]<0, 0)
    }
    
    if(back == 'minimum'){
      x[,2] <- x[,2] - min(x[,2])
    }
    
    # szukanie pików
    if(back == 'Peaks'){
      piki<-Peaks::SpectrumSearch(x[,2], sigma=s, markov=m, threshold=threshold, background=TRUE)
      x[,2] <- x[,2] - min(x[,2])
    } else {
      piki<-Peaks::SpectrumSearch(x[,2], sigma=s, markov=m, threshold=threshold, background=FALSE)
      
    }
    
    # rysuje wykres z zaznaczonymi pikami, jeżeli plot == TRUE
    if (length(piki[[1]]) > 0){
      # przygotowuje tabelę z wynikami
      wynik<-data.frame(dist_base=max(x[,1])-x[piki[[1]],1], # odległość od podstawy strzępki
                        dist_tip =x[piki[[1]],1], # odległość od tipa
                        int_raw = x1[piki[[1]],2],
                        #int_nor=round((x1[piki[[1]],2]-baza[[1]])/baza[[1]]), # intensywność fluorescencji zaokrąglona do liczby całkowitej
                        length = max(x[,1]), # długość strzępki
                        index = i,
                        #tlo = baza[[1]],
                        #tlo_nor = baza2[1],
                        time = (i * lapse)-lapse) # kolejna klatka
      if(filter_local){
        usun <- numeric(0)
        for(k in 1:nrow(wynik)){
          peak_maximum <- wynik$int_raw[k]
          min_width <- wynik$dist_tip[k] - filter_local_width/2
          max_width <- wynik$dist_tip[k] + filter_local_width/2
          
          locality <- subset(x1, V1 >= min_width & V1 < max_width)
          locality <- mean(locality$V2)
          
          if(peak_maximum/locality > filter_local_int){
            
          } else {
            usun <- c(usun, k)
          }
          
        }
        
        if(length(usun) > 0){
          wynik <- wynik[-usun,]
        }
      }
      
    } else { wynik <- NULL}
    # jeżeli obrót pętli inny niż jeden to dopisujemy wyniki do poprzednich
    if (i == 1){ wynik_kon = wynik
    wynik2 = data.frame(y = piki[[2]], time = (i * lapse)-lapse, x = x$V1, int = x$V2)
    } else {
      wynik_kon <-rbind(wynik_kon, wynik)
      dane <- data.frame(y = piki[[2]], time = (i * lapse)-lapse, x = x$V1, int = x$V2)
      wynik2 <- rbind(wynik2, dane)
    }
    
  }
  
  # sortuje wyniki najpierw według klatek, potem według odległości od tipa
  wynik_kon <- dplyr::arrange(wynik_kon, index, dist_tip) %>%
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
  p<-ggplot(data=wynik, aes(y=dist_base, x=index))
  if ( zaznacz == TRUE){    
    if (int == "int_raw"){
      test <- arrange(wynik, index, desc(int_raw))
      test <- test %>% group_by(index) %>% mutate(order=order(int_raw, decreasing=T))
      test<-subset(test, order == 1)
      
      p<-p+geom_bar(aes(x=index, y=length), stat="identity", fill="snow3", color="black", 
                    position="dodge", width=0.4)+
        geom_point(color="green3", aes(size=int_raw))+
        geom_point(data=test, aes(x=index, y=dist_base),color="red3", shape=4, size=5)+
        scale_size_continuous(range=c(1,6))
      print(p)
    }
    if (int == "int_nor"){
      test <- arrange(wynik, index, desc(int_nor))
      test <- test %>% group_by(index) %>% mutate(order=order(int_nor, decreasing=T))
      test<-subset(test, order == 1)
      
      p<-p+geom_bar(aes(x=index, y=length), stat="identity", fill="snow3", color="black", 
                    position="dodge", width=0.4)+
        geom_point(color="green3", aes(size=factor(int_nor)))+
        geom_point(data=test, aes(x=index, y=dist_base),color="red3", shape=4, size=5)+
        scale_size_discrete(range=c(1,6))
      print(p)
    }
  }else{
    
    if (int == "int_raw"){
      p<-p+geom_bar(aes(x=index, y=length), stat="identity", fill="snow3", color="black", 
                    position="dodge", width=0.4)+
        geom_point(color="green3", aes(size=int_raw))+
        scale_size_continuous(range=c(1,6))
      
    }
    if (int == "int_nor"){
      
      if ( size == TRUE){
        wynik$int_nor <- factor(wynik$int_nor, levels=c(0:36))
        
        p<-ggplot(data=wynik, aes(y=dist_base, x=index))
        
        p<-p+geom_bar(aes(x=index, y=length), stat="identity", fill="snow3", color="black", 
                      position="dodge", width=0.4)+
          geom_point(color="green3", aes(size=int_nor))+
          scale_size_manual(values=seq(3, 12, by=(7/37)))
      } else {
        
        p<-ggplot(data=wynik, aes(y=dist_base, x=index))
        
        p<-p+geom_bar(aes(x=index, y=length), stat="identity", fill="snow3", color="black", 
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
  if(dane[1,1] != 0){
    j <- 1
  }
  
  # dodajemy nową kolumnę
  dane <- data.frame(dane, ind=0)
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
  
  dane_raw %>% dplyr::group_by(time) %>%
    dplyr::summarise(maksimum = max(y)) %>% dplyr::right_join(dane_find, by = 'time') -> dane_find2
  
  p <- ggplot2::ggplot(dane_raw)
  p <- p+ggplot2::geom_line(ggplot2::aes(x = x, y = y), color = "red")+
    ggplot2::facet_wrap(~time, scales = "free", ncol = 3)+
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
    p <- ggplot2::ggplot(dane_find,  ggplot2::aes(y=dist_tip, x=time))
  } else {
    p <- ggplot2::ggplot(dane_find,  ggplot2::aes(y=dist_base, x=time))
  }
  
  p <- p + ggplot2::geom_bar(ggplot2::aes(x=time, y=length), stat="identity", fill="snow3", color="black", 
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
  
  #colnames(dane_raw) <- c('V1', 'V2')
  
  #dane_raw <- dodaj_ind(dane_raw)
  
  dane_raw <- dane_raw %>% dplyr::group_by(ind) %>% dplyr::mutate(V3 = rev(V1))
  
  p <- ggplot2::ggplot(dane_raw)
  
  x_shift <- lapse/2  + 0.1
  y_shift <- (dane_raw$V1[2] - dane_raw$V1[1])/2 + 0.01
  
  if(as.logical(odwroc == TRUE)){
    #p <- p + ggplot2::geom_tile(ggplot2::aes(x = (ind*lapse), y = V1, fill = V2))
    p <- p + ggplot2::geom_rect(ggplot2::aes(xmin = (ind*lapse) - x_shift,
                                             xmax = (ind*lapse) + x_shift,
                                             ymin = V1 - y_shift,
                                             ymax = V1 + y_shift,
                                             fill = V2))
  } else {
    #p <- p + ggplot2::geom_tile(ggplot2::aes(x = (ind*lapse), y = V3, fill = V2)) 
    p <- p + ggplot2::geom_rect(ggplot2::aes(xmin = (ind*lapse) - x_shift,
                                             xmax = (ind*lapse) + x_shift,
                                             ymin = V3 - y_shift,
                                             ymax = V3 + y_shift,
                                             fill = V2))
  }
  
  if(as.logical(pokaz) == TRUE){
    if(as.logical(odwroc == TRUE)){
      p <- p + ggplot2::geom_point(data = dane_find, ggplot2::aes(x = (index*lapse), y = dist_tip), 
                                   color = color_point, size = 3)
    } else {
      p <- p + ggplot2::geom_point(data = dane_find, ggplot2::aes(x = (index*lapse), y = dist_base), 
                                   color = color_point, size = 3)
    }
  }
  
  p <- p + ggplot2::scale_fill_gradient("Fluorescence\nintensity", low = "black", high = color_gradient)+
    ggplot2::theme_bw()+
    ggplot2::ylab(expression(paste("Length [", mu, "m]")))+
    ggplot2::xlab("Time [min]")
  
  return(p)
}


#' Title
#'
#'Uses geom_ridgeline to plot all fluorescence profiles on one plot
#'
#' @param data raw fluorescence plots data
#' @param scale should data be normalized for each plot individually
#' @param gradient plot color gradient? (scale viridis)
#' @param skala passed to geom_ridgeline - how scale plots (1 do not overplot)
#' @param reverse should plot be reversed (is 0 the tip or the bottom of the hyphae?)
#'
#' @return ggplot
#' @export
#' @examples
plot_peaks_ridges <- function(data, scale = 'osobno', gradient = TRUE, skala = 2, reverse = FALSE){
  
  dane_raw <- dodaj_ind(data)
  
  if(reverse == FALSE){
    dane_raw <- dane_raw %>% dplyr::group_by(ind) %>% dplyr::mutate(V1 = rev(V1))
  }
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
    ggplot2::xlab('Length')+
    ggplot2::ylab("Time")
  
  return(p)
  
  
}


ponumeruj <- function (wynik, a=0.23, b=0.36, rekord_staly = 10, zakres = 1.5, gap = 0, split = TRUE){
  # zmieniamy indeks na faktor, żeby można dzielić na części
  wynik$index <- as.factor(wynik$index)
  # dodajemy nową kolumnę na numery kompleksów
  wynik<-data.frame(wynik, kompleks = NA)
  #pętla dla każdej klatki
  for (i in 1:nlevels(wynik$index)){
    # wydzielamy kompleksy z jednej klatki
    x<-subset(wynik, index == i)
    # jeżeli są kompleksy
    if(nrow(x) != 0){
      # jeżeli klatka nr 1 to numerujemy kompleksy po kolei i
      # wprowadzamy zmienną zuzyte - ile było różnych numerów
      if (i == 1){ 
        x$kompleks <- 1:nrow(x)
        zuzyte <- length(1:nrow(x))
      } else { 
        for( j in 1:nrow(x)){
          
          # jeżeli klatka inna niż 1 
          # wybieramy dane z poprzedniej klatki
          poprzedni <- subset(wynik_kon, index == (i-1)&(( 
            dist_tip >= x$dist_tip[j] - zakres &
              dist_tip <= x$dist_tip[j] + zakres)|
              ( 
                dist_base >= x$dist_base[j] - zakres &
                  dist_base <= x$dist_base[j] + zakres)
          ))
          # jeżeli nie ma poprzedniej (brak kompleksów), wybieramy jeszcze wcześniejszą
          if(nrow(poprzedni) == 0 & gap == 1) {
            poprzedni <- subset(wynik_kon, index == (i-2)&(( 
              dist_tip >= x$dist_tip[j] - zakres &
                dist_tip <= x$dist_tip[j] + zakres)|
                ( 
                  dist_base >= x$dist_base[j] - zakres &
                    dist_base <= x$dist_base[j] + zakres)
            )
            )
          }
          # pętla dla każdego wiersza z danej klatki
          if(nrow(poprzedni) == 1){
            x$kompleks[j] <- poprzedni$kompleks[1]
          }
          
          if(nrow(poprzedni) > 1){
            poprzedni$roznica <- abs(poprzedni$dist_tip - x$dist_tip[j])
            poprzedni <- dplyr::arrange(poprzedni, roznica)
            x$kompleks[j] <- poprzedni$kompleks[1]
          }
          # # ustawiamy rekord do porównywania kompleksów
          # rekord <- rekord_staly
          # # sprawdzamy czy któryś kompleks z poprzedniej klatki miał podobną odległość od tip/base do naszego
          # for (k in 1:nrow(poprzedni)){
          #   procent <- a*log(x[1,4])+b
          #   if (abs(x[j,1] - poprzedni[k,1]) < procent | abs(x[j,2] - poprzedni[k,2]) < procent) {
          #     roznica <- abs(x[j,1] - poprzedni[k,1])+abs(x[j,2] - poprzedni[k,2])
          #     # czy różnica jest mniejsza od rekordu
          #     if (roznica < rekord){
          #       # jeżeli tak, dodajemy numer kompleksu i ustawiamy nowy rekord
          #       x[j,8] <- poprzedni[k,8]
          #       rekord <- roznica
          #     }
          #   } 
          # }
          # jeżeli żaden nie pasował podstawiamy nową liczbę
          if (is.na(x$kompleks[j]) == TRUE) {
            x$kompleks[j] <- (zuzyte+1)
            # nadpisujemy zuzyte z uwzględnieniem nowego kompleksu
            zuzyte<-zuzyte+1
          }
          
          
          
        }
      }
      
      if(split == FALSE){
        
        # jeżeli liczba unikalnych numerów kompleksów jest mniejsza od ilości kompleksów
        if (nrow(x) != length(unique(x$kompleks))){
          # ilość powtarających się kompleksów
          x2 <- as.factor(x$kompleks[duplicated(x$kompleks)])
          
          # jakie numery się powtarzają
          poziomy <- as.numeric(levels(x2))
          # analizujemy każdy powtarzający się numer osobno
          for(l in 1:nlevels(x2)){
            
            # wartości wcześniejszej klatki tylko dla powtarzającego się kompleksu
            
            poprzedni2 <- subset(wynik_kon, index == i-1 & kompleks == poziomy[l])
            
            if(nrow(poprzedni2) == 0 & gap == 1){
              poprzedni2 <- subset(wynik_kon, index == i-2 & kompleks == poziomy[l])  
            }
            
            # wartości analizowanej klatki tylko dla powtarzającego się kompleksu
            x3 <- subset(x,  kompleks == poziomy[l])
            # sprawdzamy każdy rząd
            for(m in 1:nrow(x3)){
              # jeżeli pierwszy rząd to liczymy sumę różnic odległosci między klatkami
              if(m == 1){
                rekord <- abs(x3$dist_base[m] - poprzedni2$dist_base[1])+
                  abs(x3$dist_tip[m] - poprzedni2$dist_tip[1])
                # którego rzędu dotyczy rekord
                ktory <- 1
              } else {
                # jeżeli rząd inny niż 1 to liczymy nową sumę różnic
                nowy <- abs(x3$dist_base[m] - poprzedni2$dist_base[1])+
                  abs(x3$dist_tip[m] - poprzedni2$dist_tip[1])
                # porównujemy rekord i nową
                if (rekord <= nowy){
                  # jeżeli rekord mniejszy to zmieniamy numer kompleksu w nowym rzędzie
                  x3$kompleks[m] <- (zuzyte + 1)
                  zuzyte <- zuzyte+1
                } else {
                  # jeżeli rekord większy to zmieniamy numer kompleksu w rzędzie rekordu
                  x3$kompleks[ktory]<- (zuzyte + 1)
                  zuzyte <- zuzyte+1
                  # ustawiamy nowy rekord i nowy ktory
                  rekord <- nowy
                  ktory <- m
                }
              }
            }
            # podstawiamy zmienione wiersze pod x
            x[x$kompleks==poziomy[l],] <- x3
          }
        }
        
      }
      
      # przygotowujemy wynik końcowy
      if (i == 1) {
        wynik_kon <- x
      } else {
        wynik_kon<-rbind(wynik_kon,x)
      }
    }
  }
  return(wynik_kon)
}


ponumeruj_stara <- function (wynik, a=0.23, b=0.36, rekord_staly = 10){
  # zmieniamy indeks na faktor, żeby można dzielić na części
  wynik$index <- as.factor(wynik$index)
  # dodajemy nową kolumnę na numery kompleksów
  wynik<-data.frame(wynik, kompleks = NA)
  #pętla dla każdej klatki
  for (i in 1:nlevels(wynik$index)){
    # wydzielamy kompleksy z jednej klatki
    x<-subset(wynik, index == i)
    # jeżeli są kompleksy
    if(nrow(x) != 0){
      # jeżeli klatka nr 1 to numerujemy kompleksy po kolei i
      # wprowadzamy zmienną zuzyte - ile było różnych numerów
      if ( i == 1){ 
        x[,8] <- 1:nrow(x)
        zuzyte <- length(1:nrow(x))
      } else { 
        # jeżeli klatka inna niż 1 
        # wybieramy dane z poprzedniej klatki
        poprzedni <- subset(wynik_kon, index == (i-1))
        # jeżeli nie ma poprzedniej (brak kompleksów), wybieramy jeszcze wcześniejszą
        if(nrow(poprzedni) == 0) {
          poprzedni <- subset(wynik_kon, index == (i-2))
        }
        # pętla dla każdego wiersza z danej klatki
        for( j in 1:nrow(x)){
          # ustawiamy rekord do porównywania kompleksów
          rekord <- rekord_staly
          # sprawdzamy czy któryś kompleks z poprzedniej klatki miał podobną odległość od tip/base do naszego
          for (k in 1:nrow(poprzedni)){
            procent <- a*log(x[1,4])+b
            if (abs(x[j,1] - poprzedni[k,1]) < procent | abs(x[j,2] - poprzedni[k,2]) < procent) {
              roznica <- abs(x[j,1] - poprzedni[k,1])+abs(x[j,2] - poprzedni[k,2])
              # czy różnica jest mniejsza od rekordu
              if (roznica < rekord){
                # jeżeli tak, dodajemy numer kompleksu i ustawiamy nowy rekord
                x[j,8] <- poprzedni[k,8]
                rekord <- roznica
              }
            } 
          }
          # jeżeli żaden nie pasował podstawiamy nową liczbę
          if (is.na(x[j,8]) == TRUE) {
            x[j,8] <- zuzyte+1
            # nadpisujemy zuzyte z uwzględnieniem nowego kompleksu
            zuzyte <- zuzyte+1
          }
        }
      }
      # przygotowujemy wynik końcowy
      if (i == 1) {
        wynik_kon <- x
      } else {
        wynik_kon<-rbind(wynik_kon,x)
      }
    }
  }
  return(wynik_kon)
}

plot_tracking_hyphae <- function(wynik, filter_tracks = NA, filter_length = 3){
  
  wynik %>% dplyr::group_by(kompleks, time) %>%
    dplyr::summarise(index = unique(index)) %>%
    dplyr::mutate(track_length = dplyr::n()) %>%
    dplyr::right_join(wynik) %>%
    dplyr::filter(track_length >= filter_length) -> wynik
  
  if(!is.na(filter_tracks)){
    
    wynik %>% dplyr::filter(kompleks %in% filter_tracks) -> wynik
    
  }
  
  p<-ggplot2::ggplot(data=wynik, ggplot2::aes(y=dist_base, x=time, label = kompleks))
  
  p<-p+ggplot2::geom_bar(ggplot2::aes(x=time, y=length), stat="identity", fill="snow1", color="black", 
                         position="dodge", width=5)+
    ggplot2::geom_text(ggplot2::aes(color = factor(kompleks)))+
    ggplot2::theme_bw()+
    ggplot2::ylab(expression(paste("Length [", mu, "m]")))+
    ggplot2::xlab("Time [min]")+
    ggplot2::scale_color_discrete(name = 'Track id')+
    ggplot2::theme(legend.position = 'bottom')
  
  #print(p)
  
  return(list(p, wynik))
  
}


summarize_tracks <- function(wynik, filter_length = 3){
  
  wynik %>% dplyr::group_by(kompleks, time) %>%
    dplyr::summarise(dist_tip = mean(dist_tip)) %>%
    dplyr::mutate(track_length = dplyr::n()) %>%
    dplyr::filter(track_length >= filter_length) %>%
    dplyr::mutate(diff = abs(dist_tip - dplyr::lag(dist_tip))) %>%
    dplyr::summarize(track_length = unique(track_length),
                     mean_diff = mean(diff, na.rm = TRUE)) ->
    wynik_tracks
  
  return(wynik_tracks)
  
}

plot_hyphae_heatmap <- function(data, num_bins = 50, max_time = 150){
  
  data %>% 
    dplyr::filter(time <= max_time) %>%
    dplyr::group_by(strain) %>%
    dplyr::mutate(length_perc = length/max(length),
                  dist_tip_perc = dist_tip/max(length),
                  dist_base_perc = dist_base/max(length)) -> data
  
  data %>%  dplyr::group_by(strain, time,  
                            dist_binned = cut(dist_base_perc, 
                                              breaks = seq(from = 0, to = 1, length.out = num_bins)), 
                            .drop = FALSE) %>%
    dplyr::count() %>%
    dplyr::group_by(time) %>%
    dplyr::mutate(percent = n/sum(n),
                  start = seq(from = 0, to = 1, length.out = num_bins-1)) -> data_timepoint
  
  data %>%  dplyr::group_by(strain, time) %>%
    dplyr::summarise(max_length = max(length_perc)) -> length_timepoint
  
  data_timepoint %>%  dplyr::left_join(length_timepoint) %>%
    dplyr::filter(start <= max_length) -> data_timepoint
  
  data %>%
    dplyr::group_by(strain, time, number_comp) %>%
    dplyr::summarise(srednia = mean(dist_base_perc)) ->
    #tidyr::pivot_wider(names_from = number_comp, values_from = srednia) -> 
    timepoint2
  
  data_timepoint %>%  dplyr::left_join(timepoint2) -> data_timepoint
  
  
  p <- ggplot2::ggplot(data = data_timepoint, ggplot2::aes(x = time, y = start, fill = percent))
  p <- p + ggplot2::geom_tile()+
    ggplot2::facet_wrap(~strain)+
    ggplot2::xlim(NA,max_time)+
    ggplot2::scale_fill_viridis_c(option = 'D', 
                                  values = c(0,0.01,0.05,0.1,0.15, 0.2,0.25,0.3,0.4,1), 
                                  labels = c('0%', '25%', '50%', '75%', '100%'), name = '')+
    ggplot2::geom_line(ggplot2::aes(x = time, y = srednia, group = factor(number_comp)), 
                       color = 'white', linetype = 2)+
    ggplot2::xlab('Time [min]')+
    ggplot2::ylab(expression('Cell length %'))+
    ggplot2::theme_minimal()+
    ggplot2::theme(legend.position = 'bottom',legend.key.height = grid::unit(0.2, 'cm'), 
                   legend.box.spacing = grid::unit(0, 'cm'), legend.key.width = grid::unit(1, 'cm'), 
                   panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank())
  
  
  return(p)
  
}

plot_multiple_kymograph <- function(data, num_bins = 100, fun = 'median', color_option = 'D', lapse){
  
  data %>% 
    dplyr::mutate(length_perc = distance/max(distance)) %>%
    dplyr::group_by(i,
                    ind,  
                    dist_binned = cut(length_perc, 
                                      breaks = seq(from = -0.0000001, to = 1, length.out = num_bins)), 
                    .drop = TRUE) %>%
    dplyr::summarise(int = mean(int, na.rm = TRUE)) %>%
    dplyr::group_by(ind, dist_binned) %>%
    dplyr::summarise(mean = mean(int, na.rm = TRUE),
                     sum = sum(int, na.rm = TRUE),
                     median = median(int, na.rm = TRUE),
                     sd = sd(int, na.rm = TRUE),
                     max = max(int, na.rm = TRUE)) %>%
    dplyr::mutate(dist = sub(x = dist_binned, pattern = '\\(', replacement = ''),
                  dist = sub(x = dist, pattern = '\\]', replacement = ''),
                  dist = sub(x = dist, pattern = ',[0-9.]{1,}', replacement = ''),
                  dist = as.numeric(dist)) -> timepoint_data
  
  if(fun == 'median'){
    timepoint_data$fun <- timepoint_data$median
  } else if(fun == 'mean'){
    timepoint_data$fun <- timepoint_data$mean
  } else if(fun == 'sum'){
    timepoint_data$fun <- timepoint_data$sum
  } else if(fun == 'sd'){
    timepoint_data$fun <- timepoint_data$sd
  } else if(fun == 'max'){
    timepoint_data$fun <- timepoint_data$max
  }
  
  p <- timepoint_data %>% ggplot2::ggplot(ggplot2::aes(x = (ind*lapse)-lapse, y = dist, fill = fun))+
    ggplot2::geom_tile(height = (1/num_bins) + 0.001)+
    #ggplot2::xlim(0, 20)+
    ggplot2::scale_fill_viridis_c(option = color_option, name = '')+
    ggplot2::theme_minimal()+
    ggplot2::xlab('Time [min]')+
    ggplot2::ylab('% length')
  
  return(p)
}


plot_demograph <- function(data, color = 'D', normalize_fluo = FALSE){
  
  data %>% dplyr::group_by(ind) %>%
    dplyr::mutate(length = distance - (max(distance)/2),
                  max = max(distance)) %>%
    dplyr::arrange(-max, ind, distance) -> data # arrange data so that smallest cell is on top
  
  levels <- unique(data$ind)
  
  # change ind to factor with new levels determined by previous arranging, then change to numeric so that levels become new values
  data %>% dplyr::mutate(ind = factor(ind, levels = levels),
                         ind = as.numeric(ind),
                         int_nor = int/max(int)) -> data
  
  dist = data$distance[2] - data$distance[1]
  
  # has to plot as geom_rect - geom_tiles fails on length (don't know why)
  # add 0.01 to avoid white lines on kymograph
  if(normalize_fluo){
    data %>% ggplot2::ggplot(ggplot2::aes(xmin = length - (dist/2) - 0.01,
                                          xmax = length + (dist/2) + 0.01,
                                          ymin = ind-0.5,
                                          ymax = ind+0.5,
                                          fill = int_nor)) -> p
  } else {
    
    data %>% ggplot2::ggplot(ggplot2::aes(xmin = length - (dist/2) - 0.01,
                                          xmax = length + (dist/2) + 0.01,
                                          ymin = ind-0.5,
                                          ymax = ind+0.5,
                                          fill = int)) -> p
  }
  
  p + ggplot2::geom_rect()+
    ggplot2::scale_fill_viridis_c(option = color, name = 'Fluorescence intensity')+
    ggplot2::theme_minimal()+
    ggplot2::theme(legend.position = 'bottom')+
    ggplot2::xlab('Cell length')+
    ggplot2::theme(text = ggplot2::element_text(size = 14),
                   legend.key.width = grid::unit(2, 'cm'))-> p
  
  return(p)
}
