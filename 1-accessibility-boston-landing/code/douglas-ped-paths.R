options(java.parameters = '-Xmx2G')

library(r5r)
library(sf)
library(leaflet)
library(htmltools)

core <- here("Networks",
             "Douglas-mod") |>
  setup_r5()



street_net <- street_network_to_sf(core)
edges <- street_net$edges
verts <- street_net$vertices

leaflet(street_net$edges) |>
  addTiles() |>
  addPolylines(data = edges,
               popup = ~htmlEscape(paste(from_vertex, to_vertex)),
               highlightOptions = highlightOptions(color = "red")) |>
  addCircleMarkers(data = verts,
                   popup = ~htmlEscape(geometry))


# South side of high school is from "7271187234" to  "7913208915"
#       way id is "848033001"

# Connect "66317316" to "7913208914"

stop_r5()