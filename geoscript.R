library(sf)
library(leaflet)
library(leaflet.extras)
library(geojsonsf)
library(tidyverse)


###bajar datos geoespaciales
mxmap <- geojson_sf('https://hoyodecrimen.com/api/v1/municipios/geojson')

mxsec <- geojson_sf('https://hoyodecrimen.com/api/v1/sectores/geojson')

mxcuad <- geojson_sf('https://hoyodecrimen.com/api/v1/cuadrantes/geojson')

###preparar la rutina
lista1  <-c(unique(cdmx$categoria_delito))


datalist <- list()

for (i in 1:length(lista1)) {

spa <- cdmx %>%
  filter(categoria_delito == lista1[[i]]) %>%
  drop_na(longitud, latitud)

points <- st_as_sf(spa, coords = c('longitud', 'latitud'), crs = 4326)

res <- st_within(points, mxcuad, sparse = FALSE)

joined <- mxcuad %>%
  mutate(Count_all = apply(res, 2, sum),
         cat = lista1[[i]],
         type = 'Cuadrante')

datalist[[i]] <- joined 

}

finlist <- datalist[2:15]

p <- do.call(rbind, datalist)

cuads <- p

###preparar y unir las bases

goodcuads <- st_zm(cuads, drop = T, what = "ZM") ##para remover el objeto ZM de los cuadrantes


alc1 <- alcaldias %>%
  mutate(name = municipio,
         sector = 'NA',
         zona = 'NA') %>%
  select(name, sector, municipio, Count_all, cat, type, zona)


sec1 <- sectores %>%
  mutate(name = sector,
         zona = 'NA') %>%
  select(name, sector, municipio, Count_all, cat, type, zona)


cua1 <- goodcuads %>%
  mutate(name = cuadrante) %>%
  select(name, sector, municipio, Count_all, cat, type, zona)


elshape <- rbind(alc1, sec1, cua1) ##create final shapefile

write_sf(elshape, 'elshape.shp') ##write shapefile


object.size(elshape)




mx <- read_sf('elshape.shp')

sep <- filter(mx, cat == 'HOMICIDIO DOLOSO',
              type == 'Sector')

pal <- colorBin("YlOrRd", domain = sep$Count_all)

pal1 <- colorNumeric("YlOrRd", NULL)


label1 <- sprintf(
  "<strong>%s</strong><br/>%g carpetas de investigación",
  sep$name, sep$Count_all
) %>% lapply(htmltools::HTML)

leaflet(sep) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(
    fillColor = ~pal1(Count_all),
    weight = .4,
    opacity = 1,
    color = 'Gray',
    fillOpacity = 0.4,
    highlight = highlightOptions(weight = 2,
                                 color = "Black"),
    label = sep$name) %>%
    addSearchOSM()  %>%
setView(lng = -99.13, lat = 19.41, zoom = 11)
  
  
data1 <- cdmx %>%
  filter(categoria_delito %in% 'HOMICIDIO DOLOSO') %>%
  select(longitud, latitud)

leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addCircleMarkers(lng = data1$longitud, lat = data1$latitud,
                   stroke = FALSE, radius = 3, color = 'Firebrick',fillOpacity = 0.4) %>%
  setView(lng = -99.13, lat = 19.41, zoom = 11)%>%
    addSearchOSM()


unique(cdmx$categoria_delito)

                                     
c("ROBO A CUENTAHABIENTE SALIENDO DEL CAJERO CON VIOLENCIA",
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
"SECUESTRO" )





filter(cdmx, categoria_delito == 'HOMICIDIO DOLOSO') %>%
  select(mes_hechos)


p1 <- cdmx %>%
  filter(categoria_delito == 'HOMICIDIO DOLOSO') %>%
  mutate(hr11 = hour(fecha_hechos))

