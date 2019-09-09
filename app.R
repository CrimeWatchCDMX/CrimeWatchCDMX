library(shiny)
library(shinyWidgets)
library(tidyverse)
library(lubridate)
library(ggthemes)
library(ggiraph)
library(RColorBrewer)
library(sf)
library(leaflet)
library(leaflet.extras)

mx <- read_sf('elshape.shp')

cdmx <- read_csv('cdmx-pgj.csv') %>% 
  filter(categoria_delito != 'HECHO NO DELICTIVO',
         year(fecha_hechos) != 2018 & year(fecha_hechos) != 2019)


angelinput <- "C:/Users/Internacionale/Downloads"

angelbae <- read_csv(paste0(angelinput, '/cdmx_adip_julio_reclasificada.csv')) 

angelbase <- angelbae %>% 
  mutate(fecha_inicio = fecha,
         categoria_delito = categoria) %>% 
  select(fecha_inicio, fecha_hechos, delito, categoria_delito, 
         colonia_hechos, alcaldia_hechos, calle_hechos,
         longitud, latitud
         )

cdmx <- rbind(cdmx, angelbase)
  


cdmxjim <- function (mapping = NULL, data = NULL, stat = "identity", 
                 position = "jitter", na.rm = FALSE, show.legend = NA, 
                 inherit.aes = TRUE, ..., width = NULL, height = NULL) 
  
{{ position <- position_jitter(width = width, height = height)}
  
  
  layer(data = data, mapping = mapping, stat = stat, geom = GeomInteractivePoint, 
        position = position, show.legend = show.legend, inherit.aes = inherit.aes, 
        params = list(na.rm = na.rm, ...))
}

ui <- navbarPage('CrimeWatch CDMX', id = 'nav',
                 tabPanel('Incidentes Diarias',
                          titlePanel("Carpetas de Investigación Iniciadas por Día"),
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
                              ),
                              
                              checkboxInput('tend', 'Linea de tendencia'
                              ),
                              
                              print('Se de debe especificar SIN FILTRO o una opción de la lista para consultar la base de datos.'),
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
                 tabPanel('Acerca',
                          
                          tags$h2('Fuentes'),        
                          
                          tags$br(),
                          
                          tags$p('Esta página analiza la base de datos de la Procuraduría general de Justicia de la Ciudad de México que se encuentra en el portal de datos abiertos del gobierno de la Ciudad de México. Esta plataforma, sus gráficos, y la maniuplación de datos se desarrollaron usando el lenguaje de R.'),
                          
                          
                          tags$br(),
                          
                          tags$a(href = 'https://datos.cdmx.gob.mx/explore/dataset/carpetas-de-investigacion-pgj-cdmx/custom/', 'PGJ-CDMX base de datos'),
                          
                          tags$br(),
                          tags$br(),
                          
                          tags$p('Los datos geoespaciales de alcaldías, sectores y cuadrantes se bajaron por medio del API del portal Hoyo de Crimen'),
                          
                          
                          tags$br(),
                          
                          tags$a(href = 'https://hoyodecrimen.com/', 'Página de Hoyo de Crimen'),
                          
                          tags$br(),
                          
                          tags$h2('Aprende'),
                          
                          tags$p('Si a ti o a tu organización les interesa aprender a usar R, cuento con experiencia capacitando grupos de las Naciones Unidas, me pueden escribir a mxdatascience@gmail.com además ofrezco cursos de R online'),
                          
                          tags$br(),
                          
                          tags$a(href = 'https://www.udemy.com/aprende-r/learn/?couponCode=DATAMX4', 'Curso de R'),
                          
                          tags$h2('Contacto'),
                          
                          tags$p('No dudes en contactarme si tienes preguntas, comentarios o sugerencias. Además el código se puede encontrar en mi Github'),
                          
                          tags$br(),
                          fluidRow(
                            column(8, align="center",
                                   
                                   tags$a(href = 'https://www.linkedin.com/in/erik-mcdonald-588061155/', icon('linkedin', "fa-3x")),
                                   tags$a(href = 'https://github.com/Masi-osare', icon('github',"fa-3x")),
                                   tags$a(href = 'https://twitter.com/optimis78485169', icon('twitter', "fa-3x"))
                            )
                            
                          ),
                          
                          tags$br(),
                          tags$h2('Donar'),
                          
                          tags$p('Por favor considera ayudar con el mantenimiento de esta página e información con un pequeño donativo'),
                          
                          
                          tags$a(href = 'https://www.paypal.com/donate/?token=oBDpSBCcUtfpfQCjsfyITGOMZqs1TAaRwvO50quqrJSnDnnkdoWwkp2aVwJQejavdVO3Am&country.x=US&locale.x=', 'Donar')
                 )
)

server <- function(input, output){
  
  output$plot1 <- renderggiraph({
    
    if(input$jit == TRUE & input$tend == TRUE) {
      g1 <-cdmx %>% 
        mutate(dia = date(fecha_inicio)) %>%
        filter(if (input$alc == 'SIN FILTRO') TRUE else alcaldia_hechos %in% c(input$alc),
               if (input$col == 'SIN FILTRO') TRUE else colonia_hechos %in% c(input$col),
               if (input$del == 'SIN FILTRO') TRUE else categoria_delito %in% c(input$del),
               if (input$del_ == 'SIN FILTRO') TRUE else delito %in% c(input$del_),
               between(dia, input$Dat[1], input$Dat[2])) %>%
        group_by(dia) %>%
        summarise(n = n()) %>%
        drop_na()%>%
        ggplot(aes(x = dia, y = n)) +
        jim(aes(tooltip = paste(dia, n, sep = '\n')))+
        geom_smooth(se = FALSE) +
        theme_minimal()+
        labs(x = 'Fecha', y = NULL) +
        theme(text = element_text(size=25)) 
      
      
      ggiraph(code = print(g1), height_svg = 9, width_svg = 17)
      
    } else if(input$jit == TRUE & input$tend == FALSE){
      
      g2 <-cdmx %>% 
        mutate(dia = date(fecha_inicio)) %>%
        filter(if (input$alc == 'SIN FILTRO') TRUE else alcaldia_hechos %in% c(input$alc),
               if (input$col == 'SIN FILTRO') TRUE else colonia_hechos %in% c(input$col),
               if (input$del == 'SIN FILTRO') TRUE else categoria_delito %in% c(input$del),
               if (input$del_ == 'SIN FILTRO') TRUE else delito %in% c(input$del_),
               between(dia, input$Dat[1], input$Dat[2])) %>%
        group_by(dia) %>%
        summarise(n = n()) %>%
        drop_na()%>%
        ggplot(aes(x = dia, y = n)) +
        jim(aes(tooltip = paste(dia, n, sep = '\n')))+
        theme_minimal()+
        labs(x = 'Fecha', y = NULL) +
        theme(text = element_text(size=25)) 
      
      
      ggiraph(code = print(g2), height_svg = 9, width_svg = 17)
      

      
    }      else if(input$jit == FALSE & input$tend == TRUE){
      
      g3 <- cdmx %>% 
        mutate(dia = date(fecha_inicio)) %>%
        filter(if (input$alc == 'SIN FILTRO') TRUE else alcaldia_hechos %in% c(input$alc),
               if (input$col == 'SIN FILTRO') TRUE else colonia_hechos %in% c(input$col),
               if (input$del == 'SIN FILTRO') TRUE else categoria_delito %in% c(input$del),
               if (input$del_ == 'SIN FILTRO') TRUE else delito %in% c(input$del_),
               between(dia, input$Dat[1], input$Dat[2])) %>%
        group_by(dia) %>%
        summarise(n = n()) %>%
        drop_na()%>%
        ggplot(aes(x = dia, y = n)) +
        geom_point()+
        geom_smooth(se = FALSE) +
        theme_minimal()+
        labs(x = 'Fecha', y = NULL) +
        theme(text = element_text(size=25)) +
        geom_point_interactive(aes(tooltip = paste(dia, n, sep = '\n')))
      
      ggiraph(code = print(g3), height_svg = 9, width_svg = 17)
      
    } else {
      
      g2 <- cdmx %>% 
        mutate(dia = date(fecha_inicio)) %>%
        filter(if (input$alc == 'SIN FILTRO') TRUE else alcaldia_hechos %in% c(input$alc),
               if (input$col == 'SIN FILTRO') TRUE else colonia_hechos %in% c(input$col),
               if (input$del == 'SIN FILTRO') TRUE else categoria_delito %in% c(input$del),
               if (input$del_ == 'SIN FILTRO') TRUE else delito %in% c(input$del_),
               between(dia, input$Dat[1], input$Dat[2])) %>%
        group_by(dia) %>%
        summarise(n = n()) %>%
        drop_na()%>%
        ggplot(aes(x = dia, y = n)) +
        geom_point()+
        theme_minimal()+
        labs(x = 'Fecha', y = NULL) +
        theme(text = element_text(size=25)) +
        geom_point_interactive(aes(tooltip = paste(dia, n, sep = '\n')))
      
      ggiraph(code = print(g2), height_svg = 9, width_svg = 17)
      
    }    
    
    
    
  })
  
  b1 <- reactive({
    
    bb <- input$del1
    
  })
  
  
  output$plot2 <- renderggiraph({
    
    if(input$but == 'alcaldia_hechos'){
      
      alcbar   <- cdmx %>%
        mutate(dia = date(fecha_inicio)) %>%
        filter(if (input$del1 == 'SIN FILTRO') TRUE else categoria_delito == b1(),
               between(dia, input$Dat1[1], input$Dat1[2])) %>%
        group_by(alcaldia_hechos) %>%
        summarise(count = n()) %>%
        drop_na() %>%
        top_n(input$num, count) %>%
        ggplot(aes(x = reorder(alcaldia_hechos, count), y = count, tooltip = count)) +
        geom_bar_interactive(stat = 'identity', fill = 'Olivedrab', alpha = .6) +
        coord_flip()+
        theme_tufte(base_family = 'arial') + labs(x = NULL, y = NULL)+
        theme(text = element_text(size=30)) 
      
      ggiraph(code = print(alcbar), height_svg = 14, width_svg = 26)
      
    } else if (input$but == 'colonia_hechos'){
      
      colbar <- cdmx %>%
        mutate(dia = date(fecha_inicio)) %>%
        filter(if (input$del1 == 'SIN FILTRO') TRUE else categoria_delito == b1(),
               if (input$colon == 'SIN FILTRO') TRUE else colonia_hechos %in% c(input$colon),
               between(dia, input$Dat1[1], input$Dat1[2])) %>%
        group_by(colonia_hechos) %>%
        summarise(count = n()) %>%
        drop_na() %>%
        top_n(input$num, count) %>%
        ggplot(aes(x = reorder(colonia_hechos, count), y = count, tooltip = count)) +
        geom_bar_interactive(stat = 'identity', fill = 'Olivedrab', alpha = .6)  +
        coord_flip()+
        theme_tufte(base_family = 'arial')+ labs(x = NULL, y = NULL)+
        theme(text = element_text(size=30)) 
      
      ggiraph(code = print(colbar), height_svg = 14, width_svg = 26)
      
      
    } else {
      
      callebar <-  cdmx %>%
        mutate(dia = date(fecha_inicio)) %>%
        filter(if (input$del1 == 'SIN FILTRO') TRUE else categoria_delito == b1(),
               between(dia, input$Dat1[1], input$Dat1[2])) %>%
        group_by(calle_hechos) %>%
        summarise(count = n()) %>%
        filter(count > 3) %>%
        drop_na() %>%
        top_n(input$num, count) %>%
        ggplot(aes(x = reorder(calle_hechos, count), y = count, tooltip = count)) +
        geom_bar_interactive(stat = 'identity', fill = 'Olivedrab', alpha = .6)  +
        coord_flip() +
        theme_tufte(base_family = 'arial') + labs(x = NULL, y = NULL)+
        theme(text = element_text(size=30)) 
      
      ggiraph(code = print(callebar), height_svg = 14, width_svg = 26)
      
      
    }
    
  })
  
  output$plot3 <- renderggiraph({
    
    if (input$hora == 'histo') {
      
      inthist <- cdmx %>%
        filter(if (input$del2 == 'SIN FILTRO') TRUE else categoria_delito %in% c(input$del2),
               if (input$del_2 == 'SIN FILTRO') TRUE else delito %in% c(input$del_2),
               if (input$col3 == 'SIN FILTRO') TRUE else colonia_hechos %in% c(input$col3)) %>%
        mutate(hr = hour(fecha_hechos)) %>%
        ggplot(aes(x = hr, tooltip = ..count..)) +
        geom_histogram_interactive(bins = input$bins, fill = "Olivedrab", alpha = .6, col = 'white') +
        theme_tufte(base_family = 'arial')+
        labs(x = 'Hora del Día', y = NULL) +
        theme(text = element_text(size=30)) 
      
      ggiraph(code = print(inthist), height_svg = 14, width_svg = 26)
      
    } else { 
      
      intlin <- cdmx %>%
        filter(if (input$del2 == 'SIN FILTRO') TRUE else categoria_delito %in% c(input$del2),
               if (input$del_2 == 'SIN FILTRO') TRUE else delito %in% c(input$del_2),
               if (input$col3 == 'SIN FILTRO') TRUE else colonia_hechos %in% c(input$col3)) %>%
        mutate(hr = hour(fecha_hechos)) %>%
        group_by(hr) %>%
        summarise(n = n()) %>%
        ggplot(aes(x = hr, y = n)) +
        geom_line_interactive()+
        theme_minimal(base_family = 'arial')+
        labs(x = 'Hora del Día', y = NULL) +
        theme(text = element_text(size=30)) 
      
      ggiraph(code = print(intlin), height_svg = 14, width_svg = 26)
      
    }
  })
  
  mapin <- reactive({
    
    data_subset1 <- subset(mx, type == input$zona1 & cat == input$cri)
    
  })
  
  
  output$mapa1 <- renderLeaflet({
    
    pal <- colorNumeric("YlOrRd", NULL)
    
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addSearchOSM()  %>%
      setView(lng = -99.13, lat = 19.41, zoom = 11)
    
  })  
  
  observe({
    
    label1 <- sprintf(
      "<strong>%s</strong><br/>%g carpetas de investigación",
      mapin()$name, mapin()$Count_all
    ) %>% lapply(htmltools::HTML)    
    
    pal <- colorNumeric("YlOrRd", NULL)
    
    leafletProxy('mapa1', data = mapin()) %>%
      clearShapes() %>%
      addPolygons(label = ~label1[1:nrow(mapin())],
                  fillColor = ~pal(Count_all),
                  weight = .4,
                  opacity = 1,
                  color = 'Gray',
                  fillOpacity = 0.4,
                  highlight = highlightOptions(weight = 2,
                                               color = "Black"))
    
  })
  
  observe({
    proxy <- leafletProxy("mapa1", data = mapin())
    
    # Remove any existing legend, and only if the legend is
    # enabled, create a new one.
    proxy %>% clearControls()
    if (input$legend1) {
      pal <- colorNumeric("YlOrRd", NULL)
      proxy %>% addLegend(position = "bottomright",
                          pal = pal, values = ~Count_all, title = 'Carpetas'
      )
    }
  })
  
  newdata <- reactive({
    
    
    filter(cdmx, categoria_delito %in% c(input$del3)) %>%
      mutate(dia = date(fecha_hechos),
             hr = hour(fecha_hechos),
             fecha2 = fecha_hechos) %>%
      filter(between(dia, input$Dat2[1], input$Dat2[2]),
             between(hr, input$hr_[1], input$hr_[2])) %>%
      separate(fecha2, into = c('dia1', 'hora'), sep = ' ')
    
  })
  
  output$mapa2 <- renderLeaflet({
    
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = -99.13, lat = 19.41, zoom = 11)%>%
      addSearchOSM()
    
  })  
  
  observe({
    
    label2 <- sprintf(
      "<strong>%s</strong><br/> Hora:%s <br/> Fecha:%s <br/> Colonia:%s",
      newdata()$categoria_delito, newdata()$hora, newdata()$dia, newdata()$colonia_hechos
    ) %>% lapply(htmltools::HTML)    
    
    pal <- colorFactor(brewer.pal(9, 'Set1'), domain = newdata()$categoria_delito)
    
    leafletProxy('mapa2', data = newdata()) %>%
      clearMarkers() %>%
      addCircleMarkers(lng = newdata()$longitud, lat = newdata()$latitud,
                       stroke = FALSE, radius = 4, color =  ~pal(newdata()$categoria_delito), 
                       fillOpacity = 0.6, label = ~label2[1:nrow(newdata())]) 
    
  })
  
  observe({
    proxy <- leafletProxy("mapa2", data = newdata())
    
    proxy %>% clearControls()
    if (input$legend1) {
      pal <- colorFactor(brewer.pal(9, 'Set1'), domain = newdata()$categoria_delito)
      proxy %>% addLegend(position = "bottomright",
                          pal = pal, values = ~categoria_delito, title = 'Categoría de Delito'
      )
    }
  })
  
}


shinyApp(ui, server)
