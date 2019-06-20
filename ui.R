ui <- navbarPage('CrimeWatch CDMX', id = 'nav',
                 tabPanel('Incidentes Diarias',
                          titlePanel("Carpetas de Investigación por Día"),
                          sidebarLayout(
                            sidebarPanel(
                            selectInput('alc', 
                                          'Alcadia', selected = 'SIN FILTRO',
                                          c('SIN FILTRO', unique(as.character(cdmx$alcaldia_hechos))),
                                          multiple = TRUE
                              ),
                              
                              selectInput('col',
                                          'Colonia', selected = 'SIN FILTRO',
                                          c('SIN FILTRO', unique(as.character(cdmx$colonia_hechos))),
                                          multiple = TRUE
                              ),
                              
                              selectInput('del',
                                          'Categoría de Delito', selected = 'SIN FILTRO',
                                          c('SIN FILTRO', unique(as.character(cdmx$categoria_delito))),
                                          multiple = TRUE
                              ),
                              
                              selectInput('del_',
                                          'Delito', selected = 'SIN FILTRO',
                                          c('SIN FILTRO', unique(as.character(cdmx$delito))),
                                          multiple = TRUE
                              ),
                              
                              sliderInput("Dat",
                                          "Fecha",
                                          min = as.Date("2016-01-01"),
                                          max = as.Date("2019-12-01"),
                                          value = as.Date(c("2016-01-01", "2019-12-01")),
                                          timeFormat="%Y-%m-%d",
                                          dragRange = TRUE
                              ),
                              
                              checkboxInput("jit", 'Jitter'
                              ), print('Se de debe especificar SIN FILTRO o una opción de la lista para consultar la base de datos.'),
                            width = 3
                              
                            ),
                            mainPanel(
                              ggiraphOutput("plot1", height = 600, width = 1200
                              )
                            )
                          )
                 ),
                 tabPanel('Los Ganadores',
                          titlePanel('Alcaldías, colonias y vialidades \ncon mayor incidencia\n'),
                          sidebarLayout(
                            sidebarPanel( 
                              radioButtons('but', 'Filtrar por',
                                           c('Alcaldía' = 'alcaldia_hechos',
                                             'Colonia' = 'colonia_hechos',
                                             'Vialidad' = 'calle_hechos')
                              ),
                              
                              conditionalPanel("input.but == 'colonia_hechos'",
                                               
                                               checkboxInput('col_1', 'Especificar Colonias', FALSE
                                               ),
                                               
                                               conditionalPanel("input.col_1 == true",
                                               
                                               
                                               selectInput('colon',
                                                           'Colonia', selected = 'SIN FILTRO',
                                                           c('SIN FILTRO', unique(as.character(cdmx$colonia_hechos))),
                                                           multiple = TRUE
                                               )
                                               )
                              ),
                              numericInput('num', 'Número de observaciones', 10
                              ),
                              
                              selectInput('del1',
                                          'Categoría de Delito', selected = 'SIN FILTRO',
                                          choices = c("SIN FILTRO",
                                                      unique(as.character(cdmx$categoria_delito)))
                              ),
                              
                              
                              sliderInput("Dat1",
                                          "Fecha",
                                          min = as.Date("2016-01-01"),
                                          max = as.Date("2019-12-01"),
                                          value = as.Date(c("2016-01-01", "2019-12-01")),
                                          timeFormat="%Y-%m-%d",
                                          dragRange = TRUE
                              ), width = 3
                            ),
                            mainPanel(
                              ggiraphOutput("plot2", height = 600, width = 1200
                              
                              )
                            )
                          )
                 ),
                 tabPanel('Por Hora',
                          
                          titlePanel('Delitos por hora'),
                          
                          sidebarLayout(
                            
                            sidebarPanel(
                              
                              radioButtons('hora', 'Tipo de gráfico',
                                           c('Histograma' = 'histo',
                                             'Line Chart' = 'lineas')
                              ),
                              
                              
                              conditionalPanel("input.hora == 'histo'",
                                               
                                               sliderInput('bins', 'Tamaño histograma',
                                                           min = 1, max = 24, value = 20
                                               )
                              ),
                              
                              selectInput('del2', 
                                          'Categoría de delito', selected = 'SIN FILTRO',
                                          c("SIN FILTRO",
                                            unique(as.character(cdmx$categoria_delito))),
                                          multiple = TRUE
                              ),
                              
                              selectInput('del_2', 
                                          'Delito', selected = 'SIN FILTRO',
                                          c("SIN FILTRO",
                                            unique(as.character(cdmx$delito))),
                                          multiple = TRUE
                              ), 
                              
                              checkboxInput('colonias1', 'Especificar Colonias', FALSE),
                              
                              conditionalPanel("input.colonias1 == true",
                                
                                               selectInput('col3',
                                                           'Colonia', selected = 'SIN FILTRO',
                                                           c('SIN FILTRO', unique(as.character(cdmx$colonia_hechos))),
                                                           multiple = TRUE
                                               )                                               
                                
                              ), width = 3
                              
                            ),
                            
                            mainPanel(
                              ggiraphOutput("plot3", height = 600, width = 1200
                              )
                              )
                            
                          )
                 ),
                 navbarMenu('Mapas',
                            
                            tabPanel('Por Zona', titlePanel(''),
                                     
                                     sidebarLayout(
                                       
                                       sidebarPanel(
                                       
                                       selectInput('zona1', 'Filtrar por', selected = '',
                                                    c('','Alcaldia','Sector', 'Cuadrante'), 
                                                   multiple = FALSE
                                                    
                                                      
                                       ),
                                       selectInput('cri', 
                                                   'Categoría de Delito', selected = 'HOMICIDIO DOLOSO',
                                                   c(unique(as.character(mx$cat))),
                                                   multiple = FALSE
                                       ),
                                       checkboxInput("legend1", "Mostrar Leyenda", TRUE)
                                       , width = 3
                                       ),
                                       mainPanel(
                                         leafletOutput('mapa1', height = 700)
                                       )
                                       
                                     )
                                     
                            ),
                            tabPanel('Por Georeferencia', titlePanel(''),
                                     
                                     sidebarLayout(
                                       
                                       sidebarPanel(
                                         selectInput('del3', 
                                                     'Categoría de Delito', selected = '',
                                                     c('',"ROBO A CUENTAHABIENTE SALIENDO DEL CAJERO CON VIOLENCIA",
                                                       "LESIONES DOLOSAS POR DISPARO DE ARMA DE FUEGO"          ,
                                                       "ROBO A TRANSEUNTE EN VÍA PÚBLICA CON Y SIN VIOLENCIA"   ,
                                                       "ROBO DE VEHÍCULO CON Y SIN VIOLENCIA"                   ,
                                                       "HOMICIDIO DOLOSO"                                       ,
                                                       "ROBO A PASAJERO A BORDO DEL METRO CON Y SIN VIOLENCIA"  ,
                                                       "VIOLACIÓN"                                              ,
                                                       "ROBO A PASAJERO A BORDO DE TAXI CON VIOLENCIA"          ,
                                                       "ROBO A REPARTIDOR CON Y SIN VIOLENCIA"                  ,
                                                       "ROBO A NEGOCIO CON VIOLENCIA"                           ,
                                                       "ROBO A TRANSPORTISTA CON Y SIN VIOLENCIA"               ,
                                                       "ROBO A PASAJERO A BORDO DE MICROBUS CON Y SIN VIOLENCIA",
                                                       "ROBO A CASA HABITACIÓN CON VIOLENCIA"                   ,
                                                       "SECUESTRO" ),
                                                     multiple = TRUE
                                         ),
                                         sliderInput("Dat2",
                                                     "Fecha",
                                                     min = as.Date("2016-01-01"),
                                                     max = as.Date("2019-12-01"),
                                                     value = as.Date(c("2019-01-01", "2019-12-01")),
                                                     timeFormat="%Y-%m-%d",
                                                     dragRange = TRUE
                                         ),
                                         sliderInput("hr_",
                                                     "Hora",
                                                     min = 0,
                                                     max = 23,
                                                     value = c(0, 23),
                                                     dragRange = TRUE
                                         ),
                                         checkboxInput("legend", "Show legend", TRUE
                                         ),
                                         width = 3
                                         
                                       ),
                                       mainPanel(
                                         leafletOutput('mapa2', height = 700)
                                       )
                                         
                                       )
                                       
                            )
                   
                 ),
                 tabPanel('Acerca'
                  

                           
                 )
                 
)  

shinyApp(ui, server)
