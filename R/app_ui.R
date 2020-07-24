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
                                             textInput('usun', 'Czy chesz usunąć kompleksy? Podaj ich numery id po przecinku', 
                                                       placeholder = 'np. 1, 4')
                            ),
                            # radioButtons("akcja", "Wybierz akcję?", choices = list("Nic" = "nic", "Dodaj punkt" = "dodaj", "Usuń punkt" = 'usun'), selected = "nic"),
                            # actionButton("dodaj", label = "Dodaj punkt"),
                            conditionalPanel('input.rodzaj_wykres == "schemat" |
                                                        input.rodzaj_wykres == "kymograf"',
                                             radioButtons("odwroc", "Czy początek pomiaru komórki ma być na dole wykresu?", 
                                                          choices = list("TRUE" = "TRUE", "FALSE" = "FALSE"), selected = "TRUE",
                                                          inline = TRUE),
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

