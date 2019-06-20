server <- function(input, output){
  
  output$plot1 <- renderggiraph({
    
    if(input$jit == TRUE) {
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
      theme_minimal()+
        labs(x = 'Fecha', y = NULL) +
        theme(text = element_text(size=25)) 
      
      
      ggiraph(code = print(g1), height_svg = 9, width_svg = 17)
      
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
        geom_point_interactive(aes(tooltip = dia))
    
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

