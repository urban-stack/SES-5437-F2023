library(tidyverse)
library(here)
library(sf)
library(leaflet)

fy2023 <-  "https://data.boston.gov/dataset/e02c44d2-3c64-459c-8fe2-e1ce5f38a035/resource/1000d81c-5bb5-49e8-a9ab-44cd042f1db2/download/fy2023-property-assessment-data.csv"
fy2022 <- "https://data.boston.gov/dataset/e02c44d2-3c64-459c-8fe2-e1ce5f38a035/resource/4b99718b-d064-471b-9b24-517ae5effecc/download/fy2022pa-4.csv"
fy2021 <- "https://data.boston.gov/dataset/e02c44d2-3c64-459c-8fe2-e1ce5f38a035/resource/c4b7331e-e213-45a5-adda-052e4dd31d41/download/data2021-full.csv"
fy2020 <- "https://data.boston.gov/dataset/e02c44d2-3c64-459c-8fe2-e1ce5f38a035/resource/8de4e3a0-c1d2-47cb-8202-98b9cbe3bd04/download/data2020-full.csv"
fy2019 <- "https://data.boston.gov/dataset/e02c44d2-3c64-459c-8fe2-e1ce5f38a035/resource/695a8596-5458-442b-a017-7cd72471aade/download/fy19fullpropassess.csv"
fy2018 <- "https://data.boston.gov/dataset/e02c44d2-3c64-459c-8fe2-e1ce5f38a035/resource/fd351943-c2c6-4630-992d-3f895360febd/download/ast2018full.csv"
fy2017 <- "https://data.boston.gov/dataset/e02c44d2-3c64-459c-8fe2-e1ce5f38a035/resource/062fc6fa-b5ff-4270-86cf-202225e40858/download/property-assessment-fy2017.csv"
fy2016 <- "https://data.boston.gov/dataset/e02c44d2-3c64-459c-8fe2-e1ce5f38a035/resource/cecdf003-9348-4ddb-94e1-673b63940bb8/download/property-assessment-fy2016.csv"
fy2015 <- "https://data.boston.gov/dataset/e02c44d2-3c64-459c-8fe2-e1ce5f38a035/resource/bdb17c2b-e9ab-44e4-a070-bf804a0e1a7f/download/property-assessment-fy2015.csv"
fy2014 <- "https://data.boston.gov/dataset/e02c44d2-3c64-459c-8fe2-e1ce5f38a035/resource/7190b0a4-30c4-44c5-911d-c34f60b22181/download/property-assessment-fy2014.csv"
fy2013 <- "https://data.boston.gov/dataset/e02c44d2-3c64-459c-8fe2-e1ce5f38a035/resource/425fd527-e26b-49c9-853c-1c4d3d2bdd97/download/property-assessment-fy13.csv"
fy2012 <- "https://data.boston.gov/dataset/e02c44d2-3c64-459c-8fe2-e1ce5f38a035/resource/4326ca95-09ec-42e0-8cee-f048e00e6728/download/property-assessment-fy12.csv"
fy2011 <- "https://data.boston.gov/dataset/e02c44d2-3c64-459c-8fe2-e1ce5f38a035/resource/110e8ded-d7cd-40d2-a72c-e4f3c7e9c541/download/property-assessment-fy11.csv"

cols_prop <- c("PID",
               "Parcel_ID",
          "ST_NUM",
          "ST_NAME",
          "LU",
          "OWNER",
          "LAND_SF",
          "GROSS_AREA",
          "LAND_VALUE",
          "AV_LAND",
          "BLDG_VALUE",
          "AV_BLDG",
          "TOTAL_VALUE",
          "AV_TOTAL",
          "YR_BUILT") 

data2023 <- read_csv(fy2023, guess_max = 30000,
           show_col_types = FALSE) |>
  filter(ST_NAME == "Guest ST" &
           ST_NUM %in% c(40, 60, 80, 100, 125, 178)) |>
  select(any_of(cols_prop)) |>
  mutate(year = 2023) 

data2020 <- read_csv(fy2020, guess_max = 30000,
                     show_col_types = FALSE) |>
  filter(PID %in% data2023$PID) |>
  select(any_of(cols_prop)) |>
  mutate(year = 2020) 

geo2023 <- "https://bostonopendata-boston.opendata.arcgis.com/datasets/boston::parcels-2023.geojson"
geo2022 <- "https://bostonopendata-boston.opendata.arcgis.com/datasets/boston::parcels-2022.geojson"
geo2021 <- "https://bostonopendata-boston.opendata.arcgis.com/datasets/boston::parcels-2021.geojson"
geo2020 <- "https://bostonopendata-boston.opendata.arcgis.com/datasets/boston::parcels-2020.geojson"
geo2019 <- "https://bostonopendata-boston.opendata.arcgis.com/datasets/boston::boston-parcels-2019.geojson"
geo2018 <- "https://bostonopendata-boston.opendata.arcgis.com/datasets/boston::boston-parcels-2018.geojson"
geo2017 <- "https://bostonopendata-boston.opendata.arcgis.com/datasets/boston::parcels-2017.geojson"
geo2016 <- "https://bostonopendata-boston.opendata.arcgis.com/datasets/boston::boston-parcels-2016.geojson"
geo2015 <- "https://bostonopendata-boston.opendata.arcgis.com/datasets/boston::boston-parcels-2015.geojson"
geo2014 <- "https://bostonopendata-boston.opendata.arcgis.com/datasets/boston::parcels-2014-1.geojson"
geo2013 <- "https://bostonopendata-boston.opendata.arcgis.com/datasets/boston::boston-parcels-2013.geojson"
geo2012 <- "https://bostonopendata-boston.opendata.arcgis.com/datasets/boston::boston-parcels-2012.geojson"
geo2011 <- "https://bostonopendata-boston.opendata.arcgis.com/datasets/boston::boston-parcels-2011.geojson"

old_pids <- c("2201905001",
              "2201905000",
              "2201904010",
              "2201904004",
              "2201904005")

boundaries2023 <- st_read(geo2023) |>
  filter(MAP_PAR_ID %in% data2023$GIS_ID) |>
  select(MAP_PAR_ID) |>
  rename(PID = MAP_PAR_ID)

boundaries2022 <- st_read(geo2022) |>
  filter(MAP_PAR_ID %in% data2023$GIS_ID |
           MAP_PAR_ID %in% old_pids) |>
  select(MAP_PAR_ID) |>
  rename(PID = MAP_PAR_ID)

boundaries2021 <- st_read(geo2021) |>
  filter(MAP_PAR_ID %in% data2023$GIS_ID |
           MAP_PAR_ID %in% old_pids) |>
  select(MAP_PAR_ID) |>
  rename(PID = MAP_PAR_ID)

boundaries2020 <- st_read(geo2020)  |>
  filter(PID_LONG %in% old_pids |
           PID_LONG %in% data2023$GIS_ID) |>
  rename(PID = PID_LONG) |>
  select(PID)

boundaries2019 <- st_read(geo2019)  |>
  filter(PID_LONG %in% old_pids |
           PID_LONG %in% data2023$GIS_ID) |>
  rename(PID = PID_LONG) |>
  select(PID)

boundaries2018 <- st_read(geo2018)  |>
  filter(PID_LONG %in% old_pids |
           PID_LONG %in% data2023$GIS_ID) |>
  rename(PID = PID_LONG) |>
  select(PID)

boundaries2017 <- st_read(geo2017)  |>
  filter(PID_LONG %in% old_pids |
           PID_LONG %in% data2023$GIS_ID) |>
  rename(PID = PID_LONG) |>
  select(PID)

boundaries2016 <- st_read(geo2016)  |>
  filter(PID_LONG %in% old_pids |
           PID_LONG %in% data2023$GIS_ID) |>
  rename(PID = PID_LONG) |>
  select(PID)

boundaries2015 <- st_read(geo2015)  |>
  filter(PID_LONG %in% old_pids |
           PID_LONG %in% data2023$GIS_ID) |>
  rename(PID = PID_LONG) |>
  select(PID)

boundaries2014 <- st_read(geo2014)  |>
  filter(PID_LONG %in% old_pids) |>
  rename(PID = PID_LONG) |>
  select(PID)

boundaries2013 <- st_read(geo2013)  |>
  filter(PID_LONG %in% old_pids) |>
  rename(PID = PID_LONG) |>
  select(PID)

boundaries2012 <- st_read(geo2012)  |>
  filter(PID_LONG %in% old_pids) |>
  rename(PID = PID_LONG) |>
  select(PID)

boundaries2011 <- st_read(geo2011) |>
  filter(PID_LONG %in% old_pids) |>
  rename(PID = PID_LONG) |>
  select(PID)

map <- leaflet::leaflet(data = boundaries2023) |>
  addPolygons(popup = ~PID,
              group = "fy2023") |>
  addPolygons(data = boundaries2022,
              popup = ~PID,
              group = "fy2022") |>
  addPolygons(data = boundaries2021,
              popup = ~PID,
              group = "fy2021") |>
  addPolygons(data = boundaries2020,
              popup = ~PID,
              group = "fy2020") |>
  addPolygons(data = boundaries2019,
              popup = ~PID,
              group = "fy2019") |>
  addPolygons(data = boundaries2018,
              popup = ~PID,
              group = "fy2018") |>
  addPolygons(data = boundaries2017,
              popup = ~PID,
              group = "fy2017") |>
  addPolygons(data = boundaries2016,
              popup = ~PID,
              group = "fy2016") |>
  addPolygons(data = boundaries2015,
              popup = ~PID,
              group = "fy2015") |>
  addPolygons(data = boundaries2014,
              popup = ~PID,
              group = "fy2014") |>
  addPolygons(data = boundaries2013,
              popup = ~PID,
              group = "fy2013") |>
  addPolygons(data = boundaries2012,
              popup = ~PID,
              group = "fy2012") |>
  addPolygons(data = boundaries2011,
              popup = ~PID,
              group = "fy2011") |>
  addLayersControl(overlayGroups = c("fy2023",
                                     "fy2022",
                                     "fy2021",
                                     "fy2020",
                                     "fy2019",
                                     "fy2018",
                                     "fy2017",
                                     "fy2016",
                                     "fy2015", 
                                     "fy2014",
                                     "fy2013", 
                                     "fy2012", 
                                     "fy2011"),
                   options = layersControlOptions(collapsed = FALSE))

htmlwidgets::saveWidget(map, here("compare2011_2023.html"))

data2022 <- read_csv(fy2022, guess_max = 30000,
                     show_col_types = FALSE) |>
  filter(PID %in% boundaries2022$PID) |>
  select(any_of(cols_prop)) |>
  mutate(year = 2022) 

data2021 <- read_csv(fy2021, guess_max = 30000,
                     show_col_types = FALSE) |>
  filter(PID %in% boundaries2021$PID) |>
  select(any_of(cols_prop)) |>
  mutate(year = 2021) 

data2020 <- read_csv(fy2020, guess_max = 30000,
                     show_col_types = FALSE) |>
  filter(PID %in% boundaries2020$PID) |>
  select(any_of(cols_prop)) |>
  mutate(year = 2020) 

data2019 <- read_csv(fy2019, guess_max = 30000,
                     show_col_types = FALSE) |>
  filter(PID %in% boundaries2019$PID) |>
  select(any_of(cols_prop)) |>
  mutate(year = 2019) 

data2018 <- read_csv(fy2018, guess_max = 30000,
                     show_col_types = FALSE) |>
  filter(PID %in% boundaries2018$PID) |>
  select(any_of(cols_prop)) |>
  mutate(year = 2018) 

data2017 <- read_csv(fy2017, guess_max = 30000,
                     show_col_types = FALSE) |>
  filter(PID %in% paste0(boundaries2017$PID, "_")) |>
  select(any_of(cols_prop)) |>
  mutate(year = 2017) 

data2016 <- read_csv(fy2016, guess_max = 30000,
                     show_col_types = FALSE) |>
  filter(PID %in% paste0(boundaries2016$PID, "_")) |>
  select(any_of(cols_prop)) |>
  mutate(year = 2016) 

data2015 <- read_csv(fy2015, guess_max = 30000,
                     show_col_types = FALSE) |>
  filter(PID %in% boundaries2015$PID |
           PID %in% paste0(boundaries2015$PID, "_")) |>
  select(any_of(cols_prop)) |>
  mutate(year = 2015) 

data2014 <- read_csv(fy2014, guess_max = 30000,
                     show_col_types = FALSE) |>
  filter(Parcel_ID %in% boundaries2014$PID |
           Parcel_ID %in% paste0(boundaries2014$PID, "_")) |>
  select(any_of(cols_prop)) |>
  mutate(year = 2014) 

data2013 <- read_csv(fy2013, guess_max = 30000,
                     show_col_types = FALSE) |>
  filter(PID %in% boundaries2013$PID |
           PID %in% paste0(boundaries2013$PID, "_")) |>
  select(any_of(cols_prop)) |>
  mutate(year = 2013) 

data2012 <- read_csv(fy2012, guess_max = 30000,
                     show_col_types = FALSE) |>
  filter(PID %in% boundaries2012$PID |
           PID %in% paste0(boundaries2012$PID, "_")) |>
  select(any_of(cols_prop)) |>
  mutate(year = 2012) 

data2011 <- read_csv(fy2011, guess_max = 30000,
                     show_col_types = FALSE) |>
  filter(PID %in% boundaries2011$PID) |>
  select(any_of(cols_prop)) |>
  mutate(year = 2011) 
  
