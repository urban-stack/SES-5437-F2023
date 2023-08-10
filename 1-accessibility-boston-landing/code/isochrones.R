options(java.parameters = '-Xmx3G')

library(r5r)
library(sf)
library(ggthemes)
library(units)
library(ggspatial)
library(terra)
library(here)

make_iso <- function(lat, lon, net_dir,
                     crs, time, mode,
                     buffer,
                     lts = 2,
                     leave_time = Sys.time()) {
  core <- setup_r5(net_dir)
  
  point <- tibble(id = "origin",
                  lat = lat, 
                  lon = lon) |>
    st_as_sf(coords = c("lon", "lat"), crs = "WGS84") 
  
  area <- point |>
    st_transform(crs) |>
    st_buffer(dist = buffer) |>
    st_make_grid(cellsize = buffer/40,
                 what = "polygons",
                 square = FALSE) |>
    as_tibble() |>
    st_set_geometry("geometry")
  
  n_points <- nrow(area)
  
  area <- area |>
    mutate(id = as.character(seq(1, n_points, by=1))) 
  
  area_points <- st_centroid(area) |>
    st_transform("WGS84")
  
  area <- area |>
    st_transform("WGS84")
  
  tt <- travel_time_matrix(core, 
                           origins = point, 
                           destinations = area_points,
                           mode = mode,
                           max_lts = lts,
                           departure_datetime = leave_time,
                           breakdown = TRUE) |>
    rename(id = toId)

  stop_r5()
  
  area |>
    left_join(tt) |>
    filter(travel_time <= time) |>
    st_union() 
  

  
}

iso_plot <- function(lat, 
                     lon, 
                     net_dir, 
                     crs, 
                     time, 
                     mode,
                     buffer, # in same units as crs
                     lts = 2,
                     leave_time = Sys.time()) {
  
  core <- setup_r5(net_dir)
  
  point <- tibble(id = "origin",
                  lat = lat, 
                  lon = lon) |>
    st_as_sf(coords = c("lon", "lat"), crs = "WGS84") 
  
  area <- point |>
    st_transform(crs) |>
    st_buffer(dist = buffer) |>
    st_make_grid(cellsize = buffer/40,
                 what = "polygons",
                 square = FALSE) |>
    as_tibble() |>
    st_set_geometry("geometry")
  
  n_points <- nrow(area)
  
  area <- area |>
    mutate(id = as.character(seq(1, n_points, by=1))) 
  
  area_points <- st_centroid(area) |>
    st_transform("WGS84")
  
  area <- area |>
    st_transform("WGS84")
  
  tt <- travel_time_matrix(core, 
                           origins = point, 
                           destinations = area_points,
                           mode = mode,
                           max_lts = lts,
                           departure_datetime = leave_time,
                           breakdown = TRUE) |>
    rename(id = toId)
  
  iso <- area |>
    left_join(tt) |>
    filter(travel_time <= time) |>
    st_union() 
  
  area_km2 = units::set_units(st_area(iso), km^2)
  bbox <- st_bbox(area)
  
  if (mode != "TRANSIT" & mode != "CAR") {
    streets <- street_network_to_sf(core)$edges[area,]
    
    if(lts < 2 & mode == "BICYCLE") {
      streets <- streets |>
        filter(car == FALSE)
    }
  } else {
    if(mode == "CAR") {
      streets <- tigris::primary_secondary_roads(state = "MA", 
                                                 filter_by = bbox)
    } else {

      streets <- transit_network_to_sf(core)$routes[area,]

    }
  }
  
  

  
  ggplot(streets) +
    geom_sf(linewidth = 0.5,
            color = "gray") +
    geom_sf(data = iso,
            alpha = 0.5,
            fill = "pink",
            color = NA) +
    geom_sf_text(data = point,
                 aes(label = paste0(formatC(area_km2,
                                            format = "f",
                                            digits = 1), " sq km"))) +
    annotation_scale(style = "ticks", location = "br",
                     text_cex = 1) +
    coord_sf(xlim = c(bbox["xmin"],
                      bbox["xmax"]),
             ylim = c(bbox["ymin"],
                      bbox["ymax"])) +
    theme_map() 
}

iso_plot_compare_nets <- function(lat, 
                             lon, 
                             net_dir,
                             net_dir_mod,
                             new_links,
                             crs, 
                             time, 
                             mode,
                             buffer, # in same units as crs
                             lts = 2,
                             leave_time = Sys.time()) {
  
  core <- setup_r5(net_dir)
  core_mod <- setup_r5(net_dir_mod)
  
  point <- tibble(id = "origin",
                  lat = lat, 
                  lon = lon) |>
    st_as_sf(coords = c("lon", "lat"), crs = "WGS84") 
  
  area <- point |>
    st_transform(crs) |>
    st_buffer(dist = buffer) |>
    st_make_grid(cellsize = buffer/40,
                 what = "polygons",
                 square = FALSE) |>
    as_tibble() |>
    st_set_geometry("geometry")
  
  n_points <- nrow(area)
  
  area <- area |>
    mutate(id = as.character(seq(1, n_points, by=1))) 
  
  area_points <- st_centroid(area) |>
    st_transform("WGS84")
  
  area <- area |>
    st_transform("WGS84")
  
  tt <- travel_time_matrix(core, 
                           origins = point, 
                           destinations = area_points,
                           mode = mode,
                           max_lts = lts,
                           departure_datetime = leave_time) |>
    rename(id = toId,
           orig_tt = travel_time)
  
  tt_mod <- travel_time_matrix(core_mod, 
                           origins = point, 
                           destinations = area_points,
                           mode = mode,
                           max_lts = lts,
                           departure_datetime = leave_time) |>
    rename(id = toId,
           new_tt = travel_time)
  
  iso <- area |>
    left_join(tt) |>
    left_join(tt_mod) |>
    filter(new_tt <= time) |>
    mutate(new_iso = orig_tt > time) |>
    group_by(new_iso) |> 
    summarize(geometry = st_union(geometry))
  
  streets_new <- street_network_to_sf(core_mod)$edges[area,] |>
    mutate(new = FALSE)
  
  streets_new$new[new_links] <- TRUE
    
  if(lts < 2) {
    streets <- streets |>
      filter(car == FALSE)
  }
  
  stop_r5()
  
  ggplot(streets_new) +
    geom_sf(linewidth = 0.5,
            aes(color = new)) +
    geom_sf(data = iso,
            alpha = 0.5,
            color = NA,
            aes(fill = new_iso)) +
    annotation_scale(style = "ticks", location = "br",
                     text_cex = 1) +
    scale_fill_manual(values = c("pink",
                                 "lightblue"),
                      labels = c("Original walkshed area",
                                 "Additional walkshed area"),
                      name = "") +
    scale_color_manual(values = c("gray",
                                 "blue"),
                      labels = c("Original street network",
                                 "New pedestrian path"),
                      name = "") +
    theme_map() +
    theme(legend.title = element_blank(),
          legend.background = element_blank(),
          legend.text = element_text(size = 10),
          legend.position = c(0,0.55))
}

iso_plot_compare_times <- function(lat, 
                                  lon, 
                                  net_dir,
                                  crs, 
                                  time, 
                                  buffer, # in same units as crs
                                  leave_time = Sys.time()) {
  
  core <- setup_r5(net_dir)
  
  point <- tibble(id = "origin",
                  lat = lat, 
                  lon = lon) |>
    st_as_sf(coords = c("lon", "lat"), crs = "WGS84") 
  
  area <- point |>
    st_transform(crs) |>
    st_buffer(dist = buffer) |>
    st_make_grid(cellsize = buffer/40,
                 what = "polygons",
                 square = FALSE) |>
    as_tibble() |>
    st_set_geometry("geometry")
  
  n_points <- nrow(area)
  
  area <- area |>
    mutate(id = as.character(seq(1, n_points, by=1))) 
  
  area_points <- st_centroid(area) |>
    st_transform("WGS84")
  
  area <- area |>
    st_transform("WGS84")
  
  tt_1 <- travel_time_matrix(core, 
                           origins = point, 
                           destinations = area_points,
                           mode = "TRANSIT",
                           departure_datetime = leave_time[1]) |>
    rename(id = toId)
  
  tt_2 <- travel_time_matrix(core, 
                               origins = point, 
                               destinations = area_points,
                               mode = "TRANSIT",
                               departure_datetime = leave_time[2]) |>
    rename(id = toId)
  
  iso_1 <- area |>
    left_join(tt_1) |>
    filter(travel_time <= time) |>
    st_union()
  
  iso_2 <- area |>
    left_join(tt_2) |>
    filter(travel_time <= time) |>
    st_union()
  
  bbox <- st_bbox(area)
  streets <- transit_network_to_sf(core)$routes[area,]

  stop_r5()
  
  ggplot(streets) +
    geom_sf(linewidth = 0.5,
            color = "gray") +
    geom_sf(data = iso_1,
            alpha = 0.5,
            color = NA,
            aes(fill = "1")) +
    geom_sf(data = iso_2,
            alpha = 0.5,
            color = NA,
            aes(fill = "2")) +
    annotation_scale(style = "ticks", location = "br",
                     text_cex = 1) +
    scale_fill_manual(values = c("pink",
                                 "lightblue"),
                      labels = format(leave_time, "%A %I:%M%p"),
                      name = "Departure time") +
    coord_sf(xlim = c(bbox["xmin"],
                      bbox["xmax"]),
             ylim = c(bbox["ymin"],
                      bbox["ymax"])) +
    theme_map() +
    theme(legend.background = element_blank(),
          legend.text = element_text(size = 10),
          legend.position = c(0,0.7))
}

# MA_st_plane <- "+proj=lcc +lat_1=42.68333333333333 +lat_2=41.71666666666667 +lat_0=41 +lon_0=-71.5 +x_0=200000.0001016002 +y_0=750000 +ellps=GRS80 +datum=NAD83 +to_meter=0.3048006096012192 +no_defs"
# 
# iso_before <- make_iso(lat = 42.35736986009441, 
#                        lon = -71.14304674648061,
#                        net_dir = here("1-accessibility-boston-landing",
#                                       "Networks", 
#                                       "before-BL"),
#                        crs = MA_st_plane,
#                        time = 30,
#                        mode = "TRANSIT",
#                        buffer = 5280*7,
#                        leave_time = as.POSIXct("2016-06-29 18:00:00 EDT")) 
# 
# iso_after <- make_iso(lat = 42.35736986009441, 
#                       lon = -71.14304674648061,
#                       net_dir = here("1-accessibility-boston-landing",
#                                      "Networks", "after-BL"),
#                       crs = MA_st_plane,
#                       time = 30,
#                       mode = "TRANSIT",
#                       buffer = 5280*7,
#                       leave_time = as.POSIXct("2017-06-28 18:00:00 EDT")) 
# 
# st_write(iso_before, here("1-accessibility-boston-landing",
#                           "data",
#                           "bl-iso-before.geojson"))
# 
# st_write(iso_after, here("1-accessibility-boston-landing",
#                           "data",
#                           "bl-iso-after.geojson"))







