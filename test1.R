states <- geojsonio::geojson_read("geo_json/ref/CNTR_BN_10M_2016_3857.geojson", what = "sp")
topoData <- readLines("geo_json/provinces.json")
class(states)
names(states)
states@data

m <- leaflet(states) %>%
  setView(-96, 37.8, 4) %>%
  addTiles()
m %>% addPolygons()


bins <- c(0, 10, 20, 50, 100, 200, 500, 1000, Inf)
pal <- colorBin("YlOrRd", domain = states$density, bins = bins)


m %>% addPolygons(
  fillColor = ~pal(density),
  weight = 2,
  opacity = 1,
  color = "white",
  dashArray = "3",
  fillOpacity = 0.7)
provinces.json

library(sf)
countries <- rgdal::readOGR("geo_json/lau/LAU_2018.shp")
countries@bbox
class(countries)


nc <- st_read("geo_json/states_provinces/ne_10m_admin_1_states_provinces.shp", quiet = TRUE)
# limit to first 2 counties
#nc <- nc[1:2,]
# convert to SpatialPolygonsDataFrame
nc_sp <- as(nc, "Spatial")
class(nc)
glimpse(nc)
view(as_tibble(nc) %>% filter(admin == "Australia"))

class(nc_sp)
str(nc_sp)

(nc_geom <- st_geometry(nc))
st_geometry(nc) %>% class()
attributes(nc_geom)

nc_geom[[1]] %>% class

