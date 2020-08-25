library(tidyverse)
library(sf)
#library(mapview)
library(leaflet)
library(htmltools)

zonas_orig <- st_read('results/zonas_orig.shp') %>%
  st_transform(4326)

zonas <- st_read('results/zonas_simplificadas.shp') %>%
  st_transform(4326)

#### Publicando mapa ####

zonas_limpo <- zonas %>%
  mutate(classe = as.integer(as.factor(classe))) %>%
  select(nome, geocodg, uf, classe) 

zonas_limpo_orig <- zonas_orig %>%
  mutate(classe = as.factor(classe)) %>%
  select(nome, geocodg, uf, classe) 

# uf_gs <- uf %>%
#   filter(sigla %in% c('MA', 'PI', 'CE', 'RN', 'PB',
#                       'PE', 'AL', 'SE', 'BA', 'MG'))

# não estava conseguindo publicar com o mapview
# estourava memória quando tentava converter
# mapview(zonas_limpo, zcol = 'classe',
#         col.regions = rainbow(18))

pal <- colorFactor(rainbow(18), domain = zonas_limpo$classe)

m <- leaflet(zonas_limpo) %>%
  addTiles() %>%
  addPolygons(
    fillOpacity = 0.5,
    fillColor = ~pal(classe),
    weight = 0,
    popup = ~paste(nome, '-', uf, '<br>Zona', classe),
    group = 'zonas'
  ) %>%
  addLegend("bottomright", pal = pal, values = ~classe,
            title = "Zona",
            opacity = 1
  ) %>%
  addLayersControl(
    overlayGroups = c('zonas')
  )

m

pal2 <- colorFactor(rainbow(24), domain = as.numeric(zonas_limpo_orig$classe))

m2 <- leaflet() %>%
  addTiles() %>%
  addPolygons(data = zonas_limpo,
    fillOpacity = 0.9,
    fillColor = pal(zonas_limpo$classe),
    weight = 0,
    popup = paste(zonas_limpo$nome, '-', zonas_limpo$uf,
                  '<br>Zona', zonas_limpo$classe),
    group = 'zonas'
  ) %>%
  addPolygons(data = zonas_limpo_orig,
    fillOpacity = 0.9,
    fillColor = pal2(as.numeric(zonas_limpo_orig$classe)),
    weight = 0,
    popup = paste(zonas_limpo_orig$nome, '-', zonas_limpo_orig$uf,
                 '<br>Zona', zonas_limpo_orig$classe),
    group = 'zonas orig'
  ) %>%
  addLegend("bottomright", pal = pal, values = zonas_limpo$classe,
            title = "Zona",
            opacity = 1
  ) %>%
  addLayersControl(
    overlayGroups = c('zonas', 'zonas orig')
  )

m2
