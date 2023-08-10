MA_st_plane <- "+proj=lcc +lat_1=42.68333333333333 +lat_2=41.71666666666667 +lat_0=41 +lon_0=-71.5 +x_0=200000.0001016002 +y_0=750000 +ellps=GRS80 +datum=NAD83 +to_meter=0.3048006096012192 +no_defs"

before_date_time <- as.POSIXct("2016-06-29 18:00:00 EDT")
after_date_time <- as.POSIXct("2017-06-28 18:00:00 EDT")

#before_core <- setup_r5(here("Networks", "before-BL"))
#after_core <- setup_r5(here("Networks", "after-BL"))

#before_routes <- transit_network_to_sf(before_core)$routes
#after_routes <- transit_network_to_sf(after_core)$routes
#stop_r5()

after_transit <- here("Networks", "after-BL", "gtfs.zip") |>
  read_gtfs() 

BL_area <- tibble(id = "BL",
                  lat = 42.35736986009441, 
                  lon = -71.14304674648061) |>
  st_as_sf(coords = c("lon", "lat"), crs = "WGS84")|>
  st_transform(MA_st_plane) |>
  st_buffer(dist = 5280/2) |>
  as_tibble() |>
  st_set_geometry("geometry") 

before_close_stops <- before_transit$stops |>
  st_transform(MA_st_plane) |>
  st_filter(BL_area)

before_removed_service <- before_transit$calendar_dates |>
  filter(exception_type == 2 &
           date < before_date_time &
           date + 1 > before_date_time)

before_removed_service <- before_transit$calendar_dates |>
  filter(exception_type == 2 &
           date < before_date_time &
           date + 1 > before_date_time)

before_service_avail <- before_transit$calendar |>
  filter(get(tolower(weekdays(before_date_time))) == 1,
         start_date < before_date_time,
         end_date > before_date_time) 