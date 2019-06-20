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
library(geojsonsf)

mx <- read_sf('elshape.shp')

cdmx <- read_delim('carpetas-de-investigacion-pgj-cdmx (1).csv', delim = ';')

jim <- function (mapping = NULL, data = NULL, stat = "identity", 
                 position = "jitter", na.rm = FALSE, show.legend = NA, 
                 inherit.aes = TRUE, ..., width = NULL, height = NULL) 
  
{{ position <- position_jitter(width = width, height = height)}
  
  
  layer(data = data, mapping = mapping, stat = stat, geom = GeomInteractivePoint, 
        position = position, show.legend = show.legend, inherit.aes = inherit.aes, 
        params = list(na.rm = na.rm, ...))
}


shinyApp(ui, server)

