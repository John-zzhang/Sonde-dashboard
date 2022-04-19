#read shapefile
library(leaflet)
library("rgdal")
library(magrittr)
riverShape <- readOGR("./river","Rivers")
riverShape <- spTransform(riverShape, CRS("+proj=longlat +ellps=GRS80"))
wales_boundaries <- rgdal::readOGR(dsn = "./country", layer = "localauthorities_lwm")
wales_boundaries <- spTransform(wales_boundaries, CRS("+proj=longlat +ellps=GRS80"))
# boundaries <- rgdal::readOGR(dsn = "./country",
#                              layer = "Counties_and_Unitary_Authorities_(December_2016)_Boundaries")
#                              # layer ="localauthorities_lwm")

# simplifiedBoundaries <-rmapshaper::ms_simplify(boundaries)
  simplifiedWalesBoundaries <-rmapshaper::ms_simplify(wales_boundaries)

simplifiedRiver<-rmapshaper::ms_simplify(riverShape)
# 
# str(boundaries@data)

leaflet() %>%
  setView(lng = -3.11, lat = 52.01, zoom = 9) %>% 
  addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
  # addPolylines(data = riverShape,
  #              color = "blue",
  #              weight  = 1)
  addPolygons(data =  simplifiedWalesBoundaries,
              color = "blue",
              fillOpacity = 0,
              weight  = 1)
              # popup = ~ctyua16nm)

# leaflet(riverShape) %>%
#   addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
#               opacity = 1.0, fillOpacity = 0.5,
#               # fillColor = ~colorQuantile("YlOrRd", ALAND)(ALAND),
#               highlightOptions = highlightOptions(color = "white", weight = 2,
#                                                   bringToFront = TRUE))