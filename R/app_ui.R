#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # List the first level UI elements here 
    navbarPage("Find Peaks",
               theme = shinythemes::shinytheme("united"),
               tabPanel("Wczytanie danych",
                        sidebarLayout(
                          
                          # boczny panel zawierający wszystkie suwaczki, przcyski itp.
                          sidebarPanel(
                            checkboxInput('example', 'Czy chcesz załadować przykładowe dane?'),
                            fileInput("dane", 'Wybierz plik .txt',
                                      accept=c('.txt')),
                            checkboxInput('header', 'Czy dane mają nagłówki?', value = TRUE)
                          ),
                          mainPanel(tableOutput('dane_raw'))
                        )
               ),
               tabPanel("Analiza",
                        sidebarLayout(
                          sidebarPanel(
                            radioButtons('rodzaj_wykres', 'Jaki wykres pokazać?', 
                                         choices = list('Wszystkie klatki' = 'wszystko',
                                                        'Schemat komórki' = 'schemat',
                                                        'Kymograf' = 'kymograf',
                                                        'Ridges (wszystkie profile razem)' = 'ridges'),
                                         inline = TRUE),
                            sliderInput("sigma", "Wybierz wartość sigma", 
                                        min = 1, max = 5, value = 2, step = 0.1),
                            sliderInput("procent", "Wybierz procent odejmowanego tła", 
                                        min = 0.05, max = 1, value = 0.2, step = 0.05),
                            sliderInput("threshold", "Wybierz wartość threshold", 
                                        min = 0, max = 100, value = 25, step = 1),
                            radioButtons("markov", "Czy użyć dopasowania markova?", 
                                         choices = list("TRUE" = "TRUE", "FALSE" = "FALSE"),
                                         selected = "TRUE", inline = TRUE),
                            numericInput("lapse", "Podaj wartość time-lapse (min)", 
                                         value = 10),
                            conditionalPanel('input.rodzaj_wykres == "wszystko"',
                                             uiOutput('filtr_czas'),
                                             textInput('usun', 'Czy chesz usunąć kompleksy? Podaj ich numery id po przecinku', 
                                                       placeholder = 'np. 1, 4')
                            ),
                            # radioButtons("akcja", "Wybierz akcję?", choices = list("Nic" = "nic", "Dodaj punkt" = "dodaj", "Usuń punkt" = 'usun'), selected = "nic"),
                            # actionButton("dodaj", label = "Dodaj punkt"),
                            conditionalPanel('input.rodzaj_wykres == "schemat" |
                                                        input.rodzaj_wykres == "kymograf" |
                                             input.rodzaj_wykres == "ridges"',
                                             radioButtons("odwroc", "Czy początek pomiaru komórki ma być na dole wykresu?", 
                                                          choices = list("TRUE" = "TRUE", "FALSE" = "FALSE"), selected = "FALSE",
                                                          inline = TRUE)
                            ),
                            conditionalPanel('input.rodzaj_wykres == "schemat" |
                                                        input.rodzaj_wykres == "kymograf"',
                                             selectInput("punkt", "Wybierz kolor punktów",
                                                         choices = list(czerwony = "red", 
                                                                        zielony = "green3", 
                                                                        niebieski = "blue", 
                                                                        żółty = "yellow"), 
                                                         selected = "red")
                            ),
                            conditionalPanel(condition = 'input.rodzaj_wykres == "kymograf"',
                                             selectInput("gradient", "Wybierz kolor gradientu", 
                                                         choices = list(czerwony = "red", 
                                                                        zielony = "green", 
                                                                        niebieski = "skyblue", 
                                                                        żółty = "yellow"), 
                                                         selected = "green"),
                                             radioButtons("pokaz", "Czy pokazać maksima na kymografie?", 
                                                          choices = list("TRUE" = "TRUE", "FALSE" = "FALSE"), selected = "TRUE",
                                                          inline = TRUE)
                            ),
                            conditionalPanel(condition = 'input.rodzaj_wykres == "ridges"',
                                             radioButtons('norm_ridges', 'Czy do plot_ridges znormalizować każdą komórkę osobno czy wszystkie razem?', 
                                                          choices = list("osobno", "razem"), inline = TRUE),
                                             numericInput("ridges_scale", "Podaj skalę do wykresu plot_ridges (Im wyższa tym bardziej linie na siebie zachodzą)",
                                                          value = 2, min = 1, max = 10, step = 0.5),
                                             radioButtons("gradient_ridges", "Czy dodać gradient kolorów do plot_ridges?", 
                                                          choices = list("Tak" = TRUE, "Nie" = FALSE), inline = TRUE)
                            ),
                            textInput('id', 'Podaj identyfikator komórki', value = 'x_1'),
                            textInput('szczep', 'Podaj nazwę szczepu', value = 'szczep'),
                            downloadButton('download_data', 'Pobierz wynik w formacie txt'),
                            width = 3
                          ),
                          mainPanel(
                            tabsetPanel(type = "tabs", 
                                        tabPanel("Wykresy", 
                                                 conditionalPanel('input.rodzaj_wykres == "wszystko"',
                                                                  plotOutput("wykres", height = 800)
                                                 ),
                                                 conditionalPanel('input.rodzaj_wykres == "schemat"',
                                                                  plotOutput("strzepka", height = 800)
                                                 ),
                                                 conditionalPanel('input.rodzaj_wykres == "kymograf"',
                                                                  plotOutput("kymograf", height = 800)
                                                 ),
                                                 conditionalPanel('input.rodzaj_wykres == "ridges"',
                                                                  plotOutput("ridges", height = 800)
                                                 )
                                        ),
                                        #tabPanel("Schemat komórki", plotOutput("strzepka", height = 700)),
                                        #tabPanel("Kymograf", plotOutput("kymograf", height = 700)),
                                        #tabPanel("Ridges_plot", plotOutput("ridges", height = 700)),
                                        tabPanel("Wynik", tableOutput("tabela"))),
                            width = 9
                          )
                        )
                        
                        
                        
               ),
               tabPanel("Porównanie szczepów",
                        sidebarLayout(sidebarPanel(
                          fileInput('wyniki', 'Wczytaj pliki txt z wynikami', multiple = TRUE),
                          radioButtons('summ_type', 'Jakie podsumowanie pokazać?', 
                                       choices = list('Szczepy' = 'szczep',
                                                      "Szczepy i komórki" = 'hyphae',
                                                      "szczepy i kompleksy" = 'komp')),
                          numericInput('n_kompl', "Ile maksymalnie kompleksów uzwględnić w analizach?",
                                       value = 1),
                          downloadButton('download_data_all', 'Pobierz zebrane dane w formacie txt'),
                          selectInput('rodzaj_wykres_summ', 'Wybierz rodzaj wykresu',
                                      choices = list('Histogram' = 'hist',
                                                     "Wykres gęstości" = 'density',
                                                     'Boxplot' = 'box',
                                                     'Scatterplot' = 'scatter'
                                      )),
                          conditionalPanel(condition = 'input.rodzaj_wykres_summ == "hist" | input.rodzaj_wykres_summ == "density"',
                                           selectInput('os_x_hist', 'Wybierz zmienną do analizy',
                                                       choices = list('Odległość od tipa' = 'dist_tip',
                                                                      'Odległość pomiędzy kompleksami' = 'dist_pom',
                                                                      'Intensywnosć fluorescencji' = 'int_raw')),
                                           selectInput('color_hist', "Kolor",
                                                       choices = list('Szczep' = 'szczep',
                                                                      'Kompleks' = 'numer_chrom',
                                                                      'Komórka' = 'komorka'))
                          ),
                          conditionalPanel(condition = 'input.rodzaj_wykres_summ == "hist"',
                                           numericInput("bin", "Szerokość słupków", value=0, step = 0.1),
                                           radioButtons("facet", "Czy podzielić na panele?", choices = list("Tak" = TRUE, "Nie" = FALSE), selected = TRUE, inline = TRUE),
                                           radioButtons("os_y", "Oś Y?", choices = list("count" = 1, "density" = 2), selected = 1, inline = TRUE),
                                           radioButtons('kolory_hist', 'Jaką skalę kolorów zastosować?', c('domyślna', 'colorbrewer', 'viridis', 'odcienie szarości', 'własna :)'),
                                                        selected = 'domyślna', inline = TRUE),
                                           conditionalPanel(
                                             condition = "input.kolory_hist == 'colorbrewer'",
                                             selectInput('colorbrewer_hist', label = 'Którą skalę Colorbrewer zastosować?',
                                                         choices = c('Set1', 'Set2', 'Set3', 'Pastel1', 'Pastel2', 'Paired', 'Dark2', 'Accent',
                                                                     'Spectral' ,'RdYlGn','RdYlBu','RdBu', 'PuOr','PRGn','PiYG', 'YlOrRd','YlGnBu',
                                                                     'PuBuGn','Blues', 'YlGn', 'Reds', 'RdPu', 'Purples', 'OrRd', 'GnBu' ),
                                                         selected = 'Set1', multiple = FALSE)
                                           ),
                                           conditionalPanel(
                                             condition = "input.kolory_hist == 'viridis'",
                                             selectInput('viridis_hist', label = 'Którą skalę viridis zastosować?',
                                                         choices = c('viridis', 'magma', 'plasma', 'inferno', 'cividis'),
                                                         selected = 'viridis', multiple = FALSE)
                                           ),
                                           conditionalPanel(
                                             condition = "input.kolory_hist == 'własna :)'",
                                             textInput('wlasne_kolory_hist', 'Tutaj wpisz wybrane nazwy kolorów oddzielając je przecinkiem. Powinny być to kolory 
                                          predefiniowane w R (można sprawdzić jakie np. na stronie 
                                          http://sape.inf.usi.ch/quick-reference/ggplot2/colour) albo skorzystać 
                                          z notacji #FF0000')
                                           ),
                                           textInput('os_x', 'Nazwa osi X', 'Wartość'),
                                           textInput('os_y_nazwa', 'Nazwa osi Y', 'Liczba')
                          ),
                          conditionalPanel(condition = 'input.rodzaj_wykres_summ == "density"',
                                           radioButtons('fill_dens', 'Czy dodać wypełnienie?', 
                                                        choices = list("Tak" = "TRUE", "Nie" = "FALSE"), 
                                                        selected = "FALSE", inline = TRUE),
                                           radioButtons('kolory_dens', 'Jaką skalę kolorów zastosować?', c('domyślna', 'colorbrewer', 'viridis', 'odcienie szarości', 'własna :)'),
                                                        selected = 'domyślna', inline = TRUE),
                                           conditionalPanel(
                                             condition = "input.kolory_dens == 'colorbrewer'",
                                             selectInput('colorbrewer_dens', label = 'Którą skalę Colorbrewer zastosować?',
                                                         choices = c('Set1', 'Set2', 'Set3', 'Pastel1', 'Pastel2', 'Paired', 'Dark2', 'Accent',
                                                                     'Spectral' ,'RdYlGn','RdYlBu','RdBu', 'PuOr','PRGn','PiYG', 'YlOrRd','YlGnBu',
                                                                     'PuBuGn','Blues', 'YlGn', 'Reds', 'RdPu', 'Purples', 'OrRd', 'GnBu' ),
                                                         selected = 'Set1', multiple = FALSE)
                                           ),
                                           conditionalPanel(
                                             condition = "input.kolory_dens == 'viridis'",
                                             selectInput('viridis_dens', label = 'Którą skalę viridis zastosować?',
                                                         choices = c('viridis', 'magma', 'plasma', 'inferno', 'cividis'),
                                                         selected = 'viridis', multiple = FALSE)
                                           ),
                                           conditionalPanel(
                                             condition = "input.kolory_dens == 'własna :)'",
                                             textInput('wlasne_kolory_dens', 'Tutaj wpisz wybrane nazwy kolorów oddzielając je przecinkiem. Powinny być to kolory 
                                          predefiniowane w R (można sprawdzić jakie np. na stronie 
                                          http://sape.inf.usi.ch/quick-reference/ggplot2/colour) albo skorzystać 
                                          z notacji #FF0000')
                                           ),
                                           textInput('os_x_dens', 'Nazwa osi X', 'Wartość'),
                                           textInput('os_y_dens', 'Nazwa osi Y', 'Liczba')
                          ),
                          conditionalPanel(condition = 'input.rodzaj_wykres_summ == "box"',
                                           selectInput('os_x_box', 'Wybierz oś X',
                                                       choices = list('Szczep' = 'szczep',
                                                                      'Kompleks' = 'numer_chrom',
                                                                      'Czas' = 'czas')),
                                           selectInput('os_y_box', 'Wybierz oś Y',
                                                       choices = list('Odległość od tipa' = 'dist_tip',
                                                                      'Odległość pomiędzy kompleksami' = 'dist_pom',
                                                                      'Intensywnosć fluorescencji' = 'int_raw')),
                                           radioButtons('boxviolin', 'Jaki wykres narysować?', 
                                                        c('Boxplot' = 'Boxplot', 'Violin' = 'Violin', 'Średnia z przedziałem ufności' = 'mean_ci'), 
                                                        inline = TRUE),
                                           radioButtons('porownanie', 'Jakie chcesz wykonać porównanie', 
                                                        list('brak' = 'brak', 'Tylko wobec kontroli' = 'kontrola', 
                                                             'Pomiędzy niektórymi grupami (podaj niżej)' = 'grupy')),
                                           conditionalPanel(condition = 'input.porownanie == "kontrola"',
                                                            numericInput('kontrola', 'Która grupa to kontrola?', 1, min = 1)
                                           ),
                                           conditionalPanel(condition = 'input.porownanie == "grupy"',
                                                            textInput('porownania', 'Tutaj wpisz grupy do porównania w formacie:
                                          Typ_A Typ_B;Typ_A Typ_C')
                                           ),
                                           radioButtons('punkty', 'Czy dodać wszystkie obserwacje?', 
                                                        c('Nie' = 'none', 'Tak (beeswarm)' = 'beeswarm', 
                                                          'Tak (quasirandom)' = 'quasirandom'), 
                                                        inline = TRUE),
                                           conditionalPanel(condition ='input.porownanie != "brak"' ,
                                                            radioButtons('rodzaj_test', 'Jaki test zastosować?', 
                                                                         c('t.test' = 't.test', 'wilcoxon' = 'wilcox.test'), inline = TRUE),
                                                            radioButtons('p_format', 'Jak pokazać wartość p?', c('Liczbowo' = 'p.adj', 'Gwiazdki' = 'p.signif'), inline = TRUE)
                                           ),
                                           radioButtons('anova', 'Czy dodać wynik Anova lub Kruskal Wallis test?', 
                                                        c('Nie' = 'nie','Anova' = 'anova', 'Kruskal Wallis' = 'kruskal.test'), 
                                                        selected = 'nie', inline = TRUE),
                                           radioButtons('kolory', 'Jaką skalę kolorów zastosować?', c('domyślna', 'colorbrewer', 'viridis', 'odcienie szarości', 'własna :)'),
                                                        selected = 'domyślna', inline = TRUE),
                                           conditionalPanel(
                                             condition = "input.kolory == 'colorbrewer'",
                                             selectInput('colorbrewer', label = 'Którą skalę Colorbrewer zastosować?',
                                                         choices = c('Set1', 'Set2', 'Set3', 'Pastel1', 'Pastel2', 'Paired', 'Dark2', 'Accent',
                                                                     'Spectral' ,'RdYlGn','RdYlBu','RdBu', 'PuOr','PRGn','PiYG', 'YlOrRd','YlGnBu',
                                                                     'PuBuGn','Blues', 'YlGn', 'Reds', 'RdPu', 'Purples', 'OrRd', 'GnBu' ),
                                                         selected = 'Set1', multiple = FALSE)
                                           ),
                                           conditionalPanel(
                                             condition = "input.kolory == 'viridis'",
                                             selectInput('viridis', label = 'Którą skalę viridis zastosować?',
                                                         choices = c('viridis', 'magma', 'plasma', 'inferno', 'cividis'),
                                                         selected = 'viridis', multiple = FALSE)
                                           ),
                                           conditionalPanel(
                                             condition = "input.kolory == 'własna :)'",
                                             textInput('wlasne_kolory', 'Tutaj wpisz wybrane nazwy kolorów oddzielając je przecinkiem. Powinny być to kolory 
                                          predefiniowane w R (można sprawdzić jakie np. na stronie 
                                          http://sape.inf.usi.ch/quick-reference/ggplot2/colour) albo skorzystać 
                                          z notacji #FF0000')
                                           ),
                                           textInput('os_x_box', 'Nazwa osi X', 'Wartość'),
                                           textInput('os_y_box', 'Nazwa osi Y', 'Liczba')
                          ),
                          conditionalPanel(condition = 'input.rodzaj_wykres_summ == "scatter"',
                                           selectInput('os_x_scatter', 'Wybierz oś X',
                                                       choices = list('Czas' = 'czas',
                                                                      "Długość" = 'dlug',
                                                                      "Odległość od tipa" = 'dist_tip',
                                                                      "Odległość od podstawy" = 'dist_base',
                                                                      "Odległość pomiędzy" = 'dist_pom',
                                                                      "Intensywność fluorescencji" = 'int_raw',
                                                                      "Liczba kompleksów" = 'n_chrom',
                                                                      'Kompleks' = 'numer_chrom')),
                                           selectInput('os_y_scatter', 'Wybierz oś Y',
                                                       choices = list('Czas' = 'czas',
                                                                      "Długość" = 'dlug',
                                                                      "Odległość od tipa" = 'dist_tip',
                                                                      "Odległość od podstawy" = 'dist_base',
                                                                      "Odległość pomiędzy" = 'dist_pom',
                                                                      "Intensywność fluorescencji" = 'int_raw',
                                                                      "Liczba kompleksów" = 'n_chrom',
                                                                      'Kompleks' = 'numer_chrom'),
                                                       selected = 'dist_tip'),
                                           selectInput('os_color_scatter', 'Pokoloruj według',
                                                       choices = list("brak" = 'brak',
                                                                      "Szczep" = 'szczep',
                                                                      "Komórka" = 'komorka',
                                                                      'Czas' = 'czas',
                                                                      "Długość" = 'dlug',
                                                                      "Odległość od tipa" = 'dist_tip',
                                                                      "Odległość od podstawy" = 'dist_base',
                                                                      "Odległość pomiędzy" = 'dist_pom',
                                                                      "Intensywność fluorescencji" = 'int_raw',
                                                                      "Liczba kompleksów" = 'n_chrom',
                                                                      'Kompleks' = 'numer_chrom')),
                                           selectInput('os_facet_scatter', 'Podziel na panele według',
                                                       choices = list("brak" = 'brak',
                                                                      "Szczep" = 'szczep',
                                                                      "Komórka" = 'komorka',
                                                                      "Liczba kompleksów" = 'n_chrom',
                                                                      'Kompleks' = 'numer_chrom')),
                                           sliderInput("alpha_point", "Podaj wartość alpha", min = 0, max = 1, value = 1, step = 0.1),
                                           sliderInput("size_point", "Podaj wielkość punktów", min = 1, max = 10, value = 2, step = 0.5),
                                           radioButtons('kolory_scatter', 'Jaką skalę kolorów zastosować?', c('domyślna', 'colorbrewer', 'viridis', 'odcienie szarości', 'własna :)'),
                                                        selected = 'domyślna', inline = TRUE),
                                           conditionalPanel(
                                             condition = "input.kolory_scatter == 'colorbrewer'",
                                             selectInput('colorbrewer_scatter', label = 'Którą skalę Colorbrewer zastosować?',
                                                         choices = c('Set1', 'Set2', 'Set3', 'Pastel1', 'Pastel2', 'Paired', 'Dark2', 'Accent',
                                                                     'Spectral' ,'RdYlGn','RdYlBu','RdBu', 'PuOr','PRGn','PiYG', 'YlOrRd','YlGnBu',
                                                                     'PuBuGn','Blues', 'YlGn', 'Reds', 'RdPu', 'Purples', 'OrRd', 'GnBu' ),
                                                         selected = 'Spectral', multiple = FALSE)
                                           ),
                                           conditionalPanel(
                                             condition = "input.kolory_scatter == 'viridis'",
                                             selectInput('viridis_scatter', label = 'Którą skalę viridis zastosować?',
                                                         choices = c('viridis', 'magma', 'plasma', 'inferno', 'cividis'),
                                                         selected = 'viridis', multiple = FALSE)
                                           ),
                                           conditionalPanel(
                                             condition = "input.kolory_scatter == 'własna :)'",
                                             textInput('wlasne_kolory_scatter', 'Tutaj wpisz wybrane nazwy kolorów oddzielając je przecinkiem. Powinny być to kolory 
                                          predefiniowane w R (można sprawdzić jakie np. na stronie 
                                          http://sape.inf.usi.ch/quick-reference/ggplot2/colour) albo skorzystać 
                                          z notacji #FF0000')),
                                           checkboxInput('trend', 'Czy dodać linię trendu?',
                                                         value =  FALSE),
                                           conditionalPanel(
                                             condition = "input.trend",
                                             radioButtons('rodzaj_trend', 'Wybierz rodzaj linii trendu',
                                                          choices = list('Loess' = 'loess',
                                                                         'Liniowa (lm)' = 'lm')),
                                             conditionalPanel(
                                               condition = "input.rodzaj_trend == 'loess'",
                                               numericInput('span', 'Wybiersz stopień dopasowania',
                                                            value = 0.75, min = 0, step = 0.05)
                                             ),
                                             checkboxInput('se', 'Czy pokazać przedział ufności?', value = TRUE),
                                             numericInput('size_trend', "Podaj grubość lilnii trendu",
                                                          value = 1, step = 0.5, min = 0)
                                           )
                                           #,
                                           # radioButtons('corr', 'Czy policzyć korelację?',
                                           #              choices = list('Nie' = 'nie', 
                                           #                             'Tak (pearson)' = 'pearson',
                                           #                             'Tak (spearman)' = 'spearman'
                                           #              ))
                                           
                                           
                                           
                          )
                        ),
                        mainPanel(
                          tabsetPanel(
                            tabPanel("Dane",
                                     tableOutput("tabela_wyniki")
                            ),
                            tabPanel("Podsumowanie",
                                     tableOutput("tabela_podsumowanie")
                            ),
                            tabPanel("Wykresy",
                                     plotOutput('wykres_podsumowanie', height = "600px")
                            )
                          )
                        )
                        )
               )
               
    )
  )
  
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
  
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'findpeaks'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

