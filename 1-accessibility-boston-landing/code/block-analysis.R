library(tidyverse)
library(tigris)
library(sf)
library(here)

res2020 <- "https://lehd.ces.census.gov/data/lodes/LODES8/ma/rac/ma_rac_S000_JT00_2020.csv.gz" |>
  read_csv() |>
  mutate(GEOID20 = as.character(h_geocode))|>
  rename(w2020 = C000) |>
  select(GEOID20, w2020)

res2012 <- "https://lehd.ces.census.gov/data/lodes/LODES8/ma/rac/ma_rac_S000_JT00_2012.csv.gz" |>
  read_csv() |>
  mutate(GEOID20 = as.character(h_geocode)) |>
  rename(w2012 = C000) |>
  select(GEOID20, w2012)

wrk2020 <- "https://lehd.ces.census.gov/data/lodes/LODES8/ma/wac/ma_wac_S000_JT00_2020.csv.gz" |>
  read_csv() |>
  mutate(GEOID20 = as.character(w_geocode)) |>
  rename(e2020 = C000) |>
  select(GEOID20, e2020)

wrk2012 <- "https://lehd.ces.census.gov/data/lodes/LODES8/ma/wac/ma_wac_S000_JT00_2012.csv.gz" |>
  read_csv() |>
  mutate(GEOID20 = as.character(w_geocode)) |>
  rename(e2012 = C000) |>
  select(GEOID20, e2012)

iso_any <- st_union(iso_before, iso_after)

blocks_any <- blocks(state = "MA") |>
  st_transform("WGS84") |>
  select(GEOID20) |>
  st_filter(iso_any)

blocks_after <- blocks_any |>
  st_filter(iso_after)

blocks_before <- blocks_any |>
  st_filter(iso_before)

blocks_any <- blocks_any |>
  mutate(before = GEOID20 %in% blocks_before$GEOID20,
         after = GEOID20 %in% blocks_after$GEOID20,
         site = GEOID20 %in% c("250250001022029",
                               "250250001022031")) |>
  left_join(res2012) |>
  left_join(res2020) |>
  left_join(wrk2012) |>
  left_join(wrk2020) |>
  replace_na(list(w2012 = 0,
                  w2020 = 0,
                  e2012 = 0,
                  e2020 = 0))


st_write(blocks_any, here("Data", "blocks.geojson"))
