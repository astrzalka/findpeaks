#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @import colourpicker
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # List the first level UI elements here 
    navbarPage("Find Peaks",
               theme = shinythemes::shinytheme("united"),
               #tabPanel("Wczytanie danych",
               
               ############################################# Load data ############################################
               
               tabPanel("Load data file",
                        sidebarLayout(
                          
                          # boczny panel zawierający wszystkie suwaczki, przcyski itp.
                          sidebarPanel(
                            checkboxInput('example', 'Do you want to use example dataset?'),
                            fileInput("dane", 'Choose txt file',
                                      accept=c('.txt')),
                            checkboxInput('header', 'Does your dataset contain headers', value = TRUE),
                            fileInput("image_file", "Load tiff file (optional)")
                          ),
                          mainPanel(tableOutput('dane_raw'))
                        )
               ),
               
               
               ################################################# Analysis ###########################################
               
               tabPanel("Analysis",
                        sidebarLayout(
                          sidebarPanel(
                            radioButtons('rodzaj_wykres', 'Plot type', 
                                         choices = list('All timepoints' = 'wszystko',
                                                        'Hyphae scheme' = 'schemat',
                                                        'Kymograph' = 'kymograf',
                                                        'Ridges (all profiles together)' = 'ridges'),
                                         inline = TRUE),
                            #checkboxInput("background", "Remove background? (recommended if background was not removed during image preprocessing)", value = FALSE),
                            radioButtons('background_type', 'Remove background (recommended if background was not removed during image preprocessing)',
                                         choices = list('No' = 'No',
                                                        'Yes (based on SNIP algorithm from Peaks)' = 'Peaks',
                                                        'Yes (substract minimal value)' = 'minimum')),
                            checkboxInput("markov", "Use Markov chain method for smoothing?", 
                                          value = TRUE),
                            sliderInput("sigma", "Choose sigma value", 
                                        min = 1, max = 5, value = 2, step = 0.1),
                            conditionalPanel('input.background_type == "No"',
                                             sliderInput("procent", "Choose percentage of removed background", 
                                                         min = 0, max = 1, value = 0.2, step = 0.05)
                            ),
                            sliderInput("threshold", "Choose threshold value (%)", 
                                        min = 0, max = 100, value = 25, step = 1),
                            numericInput("lapse", "Time lapse (min)", 
                                         value = 10),
                            checkboxInput('local', 'Use local filter?', value = FALSE),
                            conditionalPanel('input.local == true',
                                             sliderInput('local_width', 'Choose width of local filter',
                                                         min = 0.1, max = 10, value = 2),
                                             numericInput('local_int', 'Choose ratio for local filter',
                                                          value = 1.5)
                            ),
                            conditionalPanel('input.rodzaj_wykres == "wszystko"',
                                             uiOutput('filtr_czas'),
                                             textInput('usun', 'Do you wish to remove specific complexes? Specify their id numbers (separated by commas)', 
                                                       placeholder = '1, 4')
                            ),
                            # radioButtons("akcja", "Wybierz akcję?", choices = list("Nic" = "nic", "Dodaj punkt" = "dodaj", "Usuń punkt" = 'usun'), selected = "nic"),
                            # actionButton("dodaj", label = "Dodaj punkt"),
                            conditionalPanel('input.rodzaj_wykres == "schemat" |
                                                        input.rodzaj_wykres == "kymograf" |
                                             input.rodzaj_wykres == "ridges"',
                                             radioButtons("odwroc", "Show start of the cell at the bottom of the plot?", 
                                                          choices = list("TRUE" = "TRUE", "FALSE" = "FALSE"), selected = "FALSE",
                                                          inline = TRUE)
                            ),
                            conditionalPanel('input.rodzaj_wykres == "schemat" |
                                                        input.rodzaj_wykres == "kymograf"',
                                             # selectInput("punkt", "Choose point color",
                                             #             choices = list(czerwony = "red", 
                                             #                            zielony = "green3", 
                                             #                            niebieski = "blue", 
                                             #                            żółty = "yellow"), 
                                             #             selected = "red")
                                             colourInput("punkt", "Choose point colour",
                                                         "red")
                            ),
                            conditionalPanel(condition = 'input.rodzaj_wykres == "kymograf"',
                                             # selectInput("gradient", "Choose gradient color", 
                                             #             choices = list(czerwony = "red", 
                                             #                            zielony = "green", 
                                             #                            niebieski = "skyblue", 
                                             #                            żółty = "yellow"), 
                                             #             selected = "green"),
                                             colourInput("gradient", "Choose gradient colour",
                                                         "green3"),
                                             radioButtons("pokaz", "Show found maximas on the kymograph?", 
                                                          choices = list("TRUE" = "TRUE", "FALSE" = "FALSE"), selected = "TRUE",
                                                          inline = TRUE)
                            ),
                            conditionalPanel(condition = 'input.rodzaj_wykres == "ridges"',
                                             radioButtons('norm_ridges', 'Perform data normalization for each timepoint individually?', 
                                                          choices = list("individually" = "osobno","together" = "razem"), inline = TRUE),
                                             numericInput("ridges_scale", "Choose scale for ridges plot (determines profiles overlap)",
                                                          value = 2, min = 1, max = 10, step = 0.5),
                                             radioButtons("gradient_ridges", "Add color gradient to ridges plot?", 
                                                          choices = list("Tak" = TRUE, "Nie" = FALSE), inline = TRUE)
                            ),
                            textInput('id', 'Choose cell id', value = 'x_1'),
                            textInput('szczep', 'Choose strain name', value = 'strain'),
                            downloadButton('download_data', 'Download results in txt file'),
                            width = 3
                          ),
                          mainPanel(
                            tabsetPanel(type = "tabs", 
                                        tabPanel("Plots", 
                                                 conditionalPanel('input.rodzaj_wykres == "wszystko"',
                                                                  plotOutput("wykres", height = 650)
                                                 ),
                                                 conditionalPanel('input.rodzaj_wykres == "schemat"',
                                                                  plotOutput("strzepka", height = 650)
                                                 ),
                                                 conditionalPanel('input.rodzaj_wykres == "kymograf"',
                                                                  plotOutput("kymograf", height = 650)
                                                 ),
                                                 conditionalPanel('input.rodzaj_wykres == "ridges"',
                                                                  plotOutput("ridges", height = 650)
                                                 )
                                        ),
                                        #tabPanel("Schemat komórki", plotOutput("strzepka", height = 700)),
                                        #tabPanel("Kymograf", plotOutput("kymograf", height = 700)),
                                        #tabPanel("Ridges_plot", plotOutput("ridges", height = 700)),
                                        tabPanel("Result", tableOutput("tabela")),
                                        tabPanel("Image",
                                                 numericInput("frame", "Choose frame", value = 1, step = 1),
                                                 numericInput("channel", "Choose channel", value = 1, step = 1),
                                                 checkboxInput('display_all', "Display all images?"),
                                                 plotOutput("plot_tiff", height = 600, width = 900)
                                        )
                            ),
                            width = 9
                          )
                        )
                        
                        
                        
               ),
               
               ############################################### Tracking #############################################
               
               tabPanel("Tracking",
                        sidebarLayout(
                          sidebarPanel(
                            fileInput("dane_tracks", 'Choose txt file',
                                      accept=c('.txt')),
                            numericInput('diff_width', 
                                         'Choose maximum position difference (calculated from the tip of the hypha)',
                                         value = 0.6, min = 0, max = 10, step = 0.1),
                            checkboxInput('gap', "Allow for gaps (one frame)?"),
                            checkboxInput('split', 'Allow for tracks splitting?'),
                            numericInput('filter_length', "Choose minimum track length", value = 3, min = 0, step = 1),
                            textInput('tracks_id', 'Show only specific tracks', placeholder = '1, 4'),
                            downloadButton('download_data_tracks', 'Donwload tracks txt data file')
                          ),
                          mainPanel(
                            plotOutput("plot_tracks", height = 600, width = 900),
                            tableOutput('tracks_summary'),
                            tableOutput('tracks_table')
                          )
                        )
               ),
               
               
               ########################################## Comparison ################################################
               
               tabPanel("Strains comparison",
                        #sidebarLayout(sidebarPanel(
                        fluidRow(
                          column(2, style = "background-color: #f8f9fa;",
                                 fileInput('wyniki', 'Load data files (multiple datafiles can be chosen)', multiple = TRUE),
                                 radioButtons('summ_type', 'Choose summary type?', 
                                              choices = list('Strains' = 'strain',
                                                             "Strains and hyphae" = 'hyphae',
                                                             "Strains and complexes" = 'komp')),
                                 numericInput('n_kompl', "How many complexes should be included in analysis?",
                                              value = 1),
                                 downloadButton('download_data_all', 'Donwload txt data files'),
                                 selectInput('rodzaj_wykres_summ', 'Choose plot type',
                                             choices = list('Histogram' = 'hist',
                                                            "Density plot" = 'density',
                                                            'Boxplot' = 'box',
                                                            'Scatterplot' = 'scatter',
                                                            'Heatmap' = 'heatmap'
                                             )),
                                 conditionalPanel(condition = 'input.rodzaj_wykres_summ == "hist" | input.rodzaj_wykres_summ == "density"',
                                                  selectInput('os_x_hist', 'Choose variable for analysis',
                                                              choices = list('Distance to the tip' = 'dist_tip',
                                                                             'Distance between complexes' = 'dist_between',
                                                                             'Fluorescence intensity' = 'int_raw')),
                                                  selectInput('color_hist', "Color",
                                                              choices = list('Strains' = 'strain',
                                                                             'Complex' = 'number_comp',
                                                                             'Hyphae' = 'cell'))
                                 ),
                                 
                                 
                                 
                                 conditionalPanel(condition = 'input.rodzaj_wykres_summ == "box"',
                                                  selectInput('os_x_box', 'Choose variable for X axis',
                                                              choices = list('Strain' = 'strain',
                                                                             'Complex' = 'number_comp',
                                                                             'Time' = 'time')),
                                                  selectInput('os_y_box', 'Choose variable for Y axis',
                                                              choices = list('Distance to the tip' = 'dist_tip',
                                                                             'Distance between complexes' = 'dist_between',
                                                                             'Fluorescence intensity' = 'int_raw'))
                                                  
                                 ),
                                 conditionalPanel(condition = 'input.rodzaj_wykres_summ == "scatter"',
                                                  selectInput('os_x_scatter', 'Choose variable for X axis',
                                                              choices = list('Time' = 'time',
                                                                             "Hyphae length" = 'length',
                                                                             "Distance to the tip" = 'dist_tip',
                                                                             "Distance to the base" = 'dist_base',
                                                                             "Distance between complexes" = 'dist_between',
                                                                             "Fluorescence intensity" = 'int_raw',
                                                                             "Number of complexes" = 'n_comp',
                                                                             'Complex' = 'number_comp')),
                                                  selectInput('os_y_scatter', 'Choose variable for Y axis',
                                                              choices = list('Time' = 'time',
                                                                             "Hyphae length" = 'length',
                                                                             "Distance to the tip" = 'dist_tip',
                                                                             "Distance to the base" = 'dist_base',
                                                                             "Distance between complexes" = 'dist_between',
                                                                             "Fluorescence intensity" = 'int_raw',
                                                                             "Number of complexes" = 'n_comp',
                                                                             'Complex' = 'number_comp'),
                                                              selected = 'dist_tip'),
                                                  selectInput('os_color_scatter', 'Variable for color scale',
                                                              choices = list("none" = 'none',
                                                                             "Strain" = 'strain',
                                                                             "Hyphae" = 'cell',
                                                                             'Time' = 'time',
                                                                             "Hyphae length" = 'length',
                                                                             "Distance to the tip" = 'dist_tip',
                                                                             "Distance to the base" = 'dist_base',
                                                                             "Distance between complexes" = 'dist_between',
                                                                             "Fluorescence intensity" = 'int_raw',
                                                                             "Number of complexes" = 'n_comp',
                                                                             'Complex' = 'number_comp')),
                                                  selectInput('os_facet_scatter', 'Divide into facets',
                                                              choices = list("none" = 'none',
                                                                             "Strain" = 'strain',
                                                                             "Hyphae" = 'cell',
                                                                             "Number of complexes" = 'n_comp',
                                                                             'Complex' = 'number_comp'))
                                                  
                                 ),
                                 conditionalPanel(condition = 'input.rodzaj_wykres_summ == "heatmap"',
                                                  numericInput('bins_heatmap', 'Number of bins for the heatmap?', 
                                                               value = 50, min = 5, max = 200, step = 5),
                                                  numericInput('max_time_heatmap', 'Maximum time for heatmap',
                                                               value = 200, min = 10)
                                                  
                                                  
                                 )
                                 
                          ),
                          # downloadButton('download_plot', 'Pobierz wykres (dodaj .png do nazwy pliku)'),
                          # numericInput('width_plot', 'Szerokość obrazka [cm]', 20, min = 5, max = 25),
                          # numericInput('height_plot', 'Wysokość obrazka [cm]', 14, min = 5, max = 25),
                          # numericInput('res_plot', 'Rozdzielczość', 200, min = 100, max = 500)
                          #,
                          
                          
                          
                          
                          
                          
                          #mainPanel(
                          column(8,
                                 tabsetPanel(
                                   tabPanel("Data",
                                            DT::dataTableOutput("tabela_wyniki", width = 600)
                                            #tableOutput('scatter')
                                   ),
                                   tabPanel("Summary",
                                            tableOutput("tabela_podsumowanie")
                                   ),
                                   tabPanel("Plots",
                                            plotOutput('wykres_podsumowanie', height = "600px"),
                                            conditionalPanel('input.rodzaj_wykres_summ == "scatter"',
                                                             tableOutput('tabela_korelacja'),
                                                             tableOutput('tabela_lm')
                                            )
                                   )
                                 )
                          ),
                          column(2, style = "background-color: #f8f9fa;",
                                 conditionalPanel(condition = 'input.rodzaj_wykres_summ == "hist"',
                                                  numericInput("bin", "Bin size?", value=0, step = 0.1),
                                                  radioButtons("facet", "Divide plot into facets?", choices = list("Yes" = TRUE, "No" = FALSE), selected = TRUE, inline = TRUE),
                                                  radioButtons("os_y", "Y axis type?", choices = list("count" = 1, "density" = 2), selected = 1, inline = TRUE),
                                                  radioButtons('kolory_hist', 'Which color scale should be used?', c('default', 'colorbrewer', 'viridis', 'greyscale', 'custom'),
                                                               selected = 'default', inline = TRUE),
                                                  conditionalPanel(
                                                    condition = "input.kolory_hist == 'colorbrewer'",
                                                    selectInput('colorbrewer_hist', label = 'Choose colorbrewer scale',
                                                                choices = c('Set1', 'Set2', 'Set3', 'Pastel1', 'Pastel2', 'Paired', 'Dark2', 'Accent',
                                                                            'Spectral' ,'RdYlGn','RdYlBu','RdBu', 'PuOr','PRGn','PiYG', 'YlOrRd','YlGnBu',
                                                                            'PuBuGn','Blues', 'YlGn', 'Reds', 'RdPu', 'Purples', 'OrRd', 'GnBu' ),
                                                                selected = 'Set1', multiple = FALSE)
                                                  ),
                                                  conditionalPanel(
                                                    condition = "input.kolory_hist == 'viridis'",
                                                    selectInput('viridis_hist', label = 'Choose viridis scale?',
                                                                choices = c('viridis', 'magma', 'plasma', 'inferno', 'cividis'),
                                                                selected = 'viridis', multiple = FALSE)
                                                  ),
                                                  conditionalPanel(
                                                    condition = "input.kolory_hist == 'custom'",
                                                    textInput('wlasne_kolory_hist', 'Please provide color names for custom scale separated by commas. 
                                                    Color names should be predefined in R (check:  http://sape.inf.usi.ch/quick-reference/ggplot2/colour) or use
                                                    #FF0000 format'
                                                    )
                                                  ),
                                                  textInput('os_x', 'X axis name', 'Variable'),
                                                  textInput('os_y_nazwa', 'Y axis name', 'Count')
                                 ),
                                 conditionalPanel(condition = 'input.rodzaj_wykres_summ == "density"',
                                                  radioButtons('fill_dens', 'Add filling?', 
                                                               choices = list("Yes" = "TRUE", "No" = "FALSE"), 
                                                               selected = "FALSE", inline = TRUE),
                                                  radioButtons('kolory_dens', 'Which color scale should be used?', c('default', 'colorbrewer', 'viridis', 'greyscale', 'custom'),
                                                               selected = 'default', inline = TRUE),
                                                  conditionalPanel(
                                                    condition = "input.kolory_dens == 'colorbrewer'",
                                                    selectInput('colorbrewer_dens', label = 'Choose colorbrewer scale',
                                                                choices = c('Set1', 'Set2', 'Set3', 'Pastel1', 'Pastel2', 'Paired', 'Dark2', 'Accent',
                                                                            'Spectral' ,'RdYlGn','RdYlBu','RdBu', 'PuOr','PRGn','PiYG', 'YlOrRd','YlGnBu',
                                                                            'PuBuGn','Blues', 'YlGn', 'Reds', 'RdPu', 'Purples', 'OrRd', 'GnBu' ),
                                                                selected = 'Set1', multiple = FALSE)
                                                  ),
                                                  conditionalPanel(
                                                    condition = "input.kolory_dens == 'viridis'",
                                                    selectInput('viridis_dens', label = 'Choose viridis scale?',
                                                                choices = c('viridis', 'magma', 'plasma', 'inferno', 'cividis'),
                                                                selected = 'viridis', multiple = FALSE)
                                                  ),
                                                  conditionalPanel(
                                                    condition = "input.kolory_dens == 'custom'",
                                                    textInput('wlasne_kolory_dens', 'Please provide color names for custom scale separated by commas. 
                                                    Color names should be predefined in R (check:  http://sape.inf.usi.ch/quick-reference/ggplot2/colour) or use
                                                    #FF0000 format'
                                                    )
                                                  ),
                                                  textInput('os_x_dens', 'X axis name', 'Variable'),
                                                  textInput('os_y_dens', 'Y axis name', 'Density')
                                 ),
                                 conditionalPanel(condition = 'input.rodzaj_wykres_summ == "box"',
                                                  radioButtons('boxviolin', 'Choose plot type?', 
                                                               c('Boxplot' = 'Boxplot', 'Violin' = 'Violin'
                                                                 #, 'Mean with confidence interval' = 'mean_ci'
                                                               ), 
                                                               inline = TRUE),
                                                  radioButtons('porownanie', 'Choose comparison type', 
                                                               list('none' = 'brak', 'Only against control' = 'kontrola', 
                                                                    'Between groups (provide below)' = 'grupy')),
                                                  conditionalPanel(condition = 'input.porownanie == "kontrola"',
                                                                   numericInput('kontrola', 'Which group is the control group?', 1, min = 1)
                                                  ),
                                                  conditionalPanel(condition = 'input.porownanie == "grupy"',
                                                                   textInput('porownania', 'Provide groups for comaprison, please use format:
                                          Typ_A Typ_B;Typ_A Typ_C')
                                                  ),
                                          radioButtons('punkty', 'Include all observations?', 
                                                       c('No' = 'none', 'Yes (beeswarm)' = 'beeswarm', 
                                                         'Yes (quasirandom)' = 'quasirandom'), 
                                                       inline = TRUE),
                                          conditionalPanel(condition ='input.porownanie != "brak"' ,
                                                           radioButtons('rodzaj_test', 'Choose test type', 
                                                                        c('t.test' = 't.test', 'wilcoxon' = 'wilcox.test'), inline = TRUE),
                                                           radioButtons('p_format', 'Choose p-value format', c('Numeric' = 'p.adj', 'Stars' = 'p.signif'), inline = TRUE)
                                          ),
                                          radioButtons('anova', 'Add Anova or Kruskal-Wallis test result to the plot?', 
                                                       c('Np' = 'nie','Anova' = 'anova', 'Kruskal Wallis' = 'kruskal.test'), 
                                                       selected = 'nie', inline = TRUE),
                                          radioButtons('kolory', 'Which color scale should be used?', c('default', 'colorbrewer', 'viridis', 'greyscale', 'custom'),
                                                       selected = 'default', inline = TRUE),
                                          conditionalPanel(
                                            condition = "input.kolory == 'colorbrewer'",
                                            selectInput('colorbrewer', label = 'Choose colorbrewer scale',
                                                        choices = c('Set1', 'Set2', 'Set3', 'Pastel1', 'Pastel2', 'Paired', 'Dark2', 'Accent',
                                                                    'Spectral' ,'RdYlGn','RdYlBu','RdBu', 'PuOr','PRGn','PiYG', 'YlOrRd','YlGnBu',
                                                                    'PuBuGn','Blues', 'YlGn', 'Reds', 'RdPu', 'Purples', 'OrRd', 'GnBu' ),
                                                        selected = 'Set1', multiple = FALSE)
                                          ),
                                          conditionalPanel(
                                            condition = "input.kolory == 'viridis'",
                                            selectInput('viridis', label = 'Choose viridis scale?',
                                                        choices = c('viridis', 'magma', 'plasma', 'inferno', 'cividis'),
                                                        selected = 'viridis', multiple = FALSE)
                                          ),
                                          conditionalPanel(
                                            condition = "input.kolory == 'custom'",
                                            textInput('wlasne_kolory', 'Please provide color names for custom scale separated by commas. 
                                                    Color names should be predefined in R (check:  http://sape.inf.usi.ch/quick-reference/ggplot2/colour) or use
                                                    #FF0000 format'
                                            )
                                          ),
                                          textInput('os_x_box', 'X axis name', 'Variable'),
                                          textInput('os_y_box', 'Y axis name', 'Count')
                                 ),
                                 conditionalPanel(condition = 'input.rodzaj_wykres_summ == "scatter"',
                                                  sliderInput("alpha_point", "Alpha value for points (transparency)", min = 0, max = 1, value = 1, step = 0.1),
                                                  sliderInput("size_point", "Point size", min = 1, max = 10, value = 2, step = 0.5),
                                                  radioButtons('corr', 'Calculate correlation?',
                                                               choices = list('No' = 'nie',
                                                                              'Yes (pearson)' = 'pearson',
                                                                              'Yes (spearman)' = 'spearman'
                                                               )),
                                                  radioButtons('kolory_scatter', 'Which color scale should be used?', c('default', 'colorbrewer', 'viridis', 'greyscale', 'custom'),
                                                               selected = 'default', inline = TRUE),
                                                  conditionalPanel(
                                                    condition = "input.kolory_scatter == 'colorbrewer'",
                                                    selectInput('colorbrewer_scatter', label = 'Choose colorbrewer scale',
                                                                choices = c('Set1', 'Set2', 'Set3', 'Pastel1', 'Pastel2', 'Paired', 'Dark2', 'Accent',
                                                                            'Spectral' ,'RdYlGn','RdYlBu','RdBu', 'PuOr','PRGn','PiYG', 'YlOrRd','YlGnBu',
                                                                            'PuBuGn','Blues', 'YlGn', 'Reds', 'RdPu', 'Purples', 'OrRd', 'GnBu' ),
                                                                selected = 'Set1', multiple = FALSE)
                                                  ),
                                                  conditionalPanel(
                                                    condition = "input.kolory_scatter == 'viridis'",
                                                    selectInput('viridis_scatter', label = 'Choose viridis scale?',
                                                                choices = c('viridis', 'magma', 'plasma', 'inferno', 'cividis'),
                                                                selected = 'viridis', multiple = FALSE)
                                                  ),
                                                  conditionalPanel(
                                                    condition = "input.kolory_scatter == 'custom'",
                                                    textInput('wlasne_kolory_scatter', 'Please provide color names for custom scale separated by commas. 
                                                    Color names should be predefined in R (check:  http://sape.inf.usi.ch/quick-reference/ggplot2/colour) or use
                                                    #FF0000 format'
                                                    )
                                                  ),
                                                  checkboxInput('trend', 'Add trend line?',
                                                                value =  FALSE),
                                                  conditionalPanel(
                                                    condition = "input.trend",
                                                    radioButtons('rodzaj_trend', 'Choose type of trend line',
                                                                 choices = list('Loess' = 'loess',
                                                                                'Linear (lm)' = 'lm')),
                                                    conditionalPanel(
                                                      condition = "input.rodzaj_trend == 'loess'",
                                                      numericInput('span', 'Choose smoothing degree',
                                                                   value = 0.75, min = 0, step = 0.05)
                                                    ),
                                                    checkboxInput('se', 'Show confidence intervals?', value = TRUE),
                                                    numericInput('size_trend', "Trendline size",
                                                                 value = 1, step = 0.5, min = 0)
                                                  )
                                 ),
                                 downloadButton('download_plot', 'Download plot'),
                                 numericInput('width_plot', 'Width [cm]', 20, min = 5, max = 25),
                                 numericInput('height_plot', 'Height [cm]', 14, min = 5, max = 25),
                                 numericInput('res_plot', 'Resolution', 200, min = 100, max = 500)
                                 
                          )
                        )
               ),
               
               ########################################### Multiple Kymograph #######################################
               
               tabPanel('Multiple Kymograph (beta)',
                        sidebarLayout(
                          sidebarPanel(
                            fileInput('data_kymograph', 
                                      'Load files with plot profiles (multiple files can be chosen)', 
                                      multiple = TRUE),
                            numericInput('bins_kymograph', 'Number of bins for the kymograph?', 
                                         value = 100, min = 5, max = 200, step = 5),
                            selectInput('fun_kymograph', 'Which function should be used to summarise kymograph?',
                                        multiple = FALSE, selected = 'median', 
                                        choices = c('median', 'mean', 'sd', 'max', 'sum')),
                            selectInput('fill_kymograph', 'Choose color option for the kymograph',
                                        choices = c('A', 'B', 'C', 'D', 'E'), selected = 'D', multiple = FALSE),
                            numericInput('lapse_kymograph', 'Choose lapse value', value = 10),
                            numericInput('max_kymograph', 'Choose maximal value for x scale (frame not time)', 
                                         value = 100)
                            
                          ),
                          mainPanel(
                            #tableOutput('test_kymo')
                            plotOutput('multiple_kymograph', height = "800px"),
                          )
                        )
                        
                        
               ),
               
               
               ############################################### Demograph #############################################
               
               tabPanel('Demograph (beta)',
                        sidebarLayout(
                          sidebarPanel(
                            fileInput('data_demograph', 
                                      'Load files with plot profiles (multiple files can be chosen)', 
                                      multiple = TRUE),
                            selectInput('fill_demograph', 'Choose color option for the demograph',
                                        choices = c('A', 'B', 'C', 'D', 'E'), selected = 'D', multiple = FALSE),
                            checkboxInput('nor_fluo', 'Normalize fluorescence intensity?', value = FALSE)
                            
                            
                          ),
                          mainPanel(
                            #tableOutput('test_demo')
                            #plotOutput('demograph', height = "800px"),
                          )
                          
                        )
               ),
               
               
               
               ############################################# Help ######################################################
               
               tabPanel('Help',
                        p("findpeaks looks for maxima in fluorescence profiles generated from time-lapse microscopic movie."),
                        p('Data file should be in txt format and contain two columns: Length of measured hypha and fluorescence intensity. 
                          It can also contain optional first column with numerical index, which will be automatically removed during analysis.
                          Data file can be generated using an ImageJ script available at the Github page :'),
                        tags$a(
                          id = 'mydiv',
                          href = "https://github.com/astrzalka/findpeaks", "findpeaks"
                        ),
                        p(' '),
                        p("Background signal can be removed completely or partially depending on the image preprocessing. Complete background removal can be done by minimum gluorescence substraction 
                        or using a SNIP algorithm from Peaks package.
                        
                        Removal of background signal will not affect the fluorescence intesity of peaks in the results file."),
                        p('Peaks are identified after signal deconvolution using Gaussian with sigma value provided by the user. 
                          Higher sigma value result in broader peaks'),
                        p('Additional smoothing before denconvolution can be achieved using the Markov chain method.'), 
                        p('Peaks below a given threshold (%) are removed from the analysis. The threshold is calculated for each timepoint separately. Local filtering of peaks is also
                          available. For each peak ratio of fluorescne vs surrounding background (width can be adjusted by the user) is calculated and peaks below a certain threshold will be removed.
                          This approach can be helpful in removing false peaks, which can result from noisy fluorescent signal.'),
                        p('Erroneously detected peaks can also be removed manually using their id number.'),
                        p('All chosen values of parameters are saved in the results. Hypha id and strain name can also be set.'),
                        p("Results can be saved in txt format for further analysis in the application or in another program."),
                        p("Tracking of detected peaks can be done using nearest neighboor algorithm. Track splitting, one frame gaps and maximal distance for peaks tracking can be adjusted by the user.s"),
                        p(' '),
                        p('For results analysis multiple txt files (also from different strains) can be uploaded and common plots such as: histogram, density, boxplot, scatterplot can be created.'),
                        p(strong("Application is based upon Peaks package:")),
                        p("Miroslav Morhac (2012). Peaks: Peaks. R package version 0.2.
  https://CRAN.R-project.org/package=Peaks"))
  
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

