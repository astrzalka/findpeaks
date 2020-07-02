# Funkcja do szukania pików fluorescencji w danych z time lapse
# ramka - tabela z danymi (kolumna 1 - odległości, 2 - intensywność fluorescencji)
# s - sigma - dokładność dopasowania pików, im wyższa tym mniej znajdzie
# m - jak ma wygładzać dane, przy FALSE znajduje więcej pików
# procent - ile tła ma odjąć, rozsądny zakres to od 1(całość) do 0.01 (1 procent)
# threshold - powyżej jakiego poziomu ma zaznaczać piki, procent wysokości najwyższego piku
# back - na razie niech zostanie FALSE 
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
  wynik_kon<-dplyr::arrange(wynik_kon, indeks, dist_tip)
  # zwraca wynik
  return(list(wynik = wynik_kon, wynik2 = wynik2))
}

# rysuje wykres ggplot, komórki jako słupki, kompleksy jako kropki
# podajemy dane z funkcji find_peaks, int - czy ma być znormalizowana czy nie
# zaznacz - czy ma zaznaczać najsilniejszy kompleks
# size - tylko dla int_nor, czy wielkość kompleksu ma odpowiadać intensywności, FALSE - gradient kolorów
#' Title
#'
#' @param wynik 
#' @param int 
#' @param zaznacz 
#' @param size 
#'
#' @return
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

# Uzupełnia indeksy w danych, które potem można użyć w funkcji find_peaks. 
# Zakłada, że pojawienie się zera w pierwszej kolumnie (odległości) oznacza kolejną analizowaną klatkę
#' Title
#'
#' @param dane 
#'
#' @return
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