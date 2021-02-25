# set up
# load packages 
library(here)
library(tidyverse)
library("tigris", lib.loc="/usr/local/lib/R/site-library")
library(sf)
library(raster)
library(tidycensus)
library(ggthemes)
options(tigris_use_cache = TRUE)

# load tidycensus variables for reference 
v19 <- load_variables(2019, "acs5", cache = TRUE)

# explore variables
v19_concepts <- distinct(v19, concept)

# read tigris shapefiles for southern AZ
# five uazcc catchment counties
southern_az <- tracts(state = 04, county = c("Cochise", "Pima", "Pinal", "Santa Cruz", "Yuma"))

# write to disk for import into grass
st_write(obj = southern_az, 
         dsn = "data/tidy/shapefiles/catchment/catchment.shp")

# pima county only 
tracts_pima <- tracts(state = 04, county = "Pima")

# write to disk for import into grass
st_write(obj = tracts_pima, 
         dsn = "data/tidy/shapefiles/pima/census_tract.shp")

# counties in AZ
az_counties <- counties(state = 04)

# write to disk for import into grass
st_write(az_counties,
         dsn = "data/tidy/shapefiles/counties/az_counties.shp")

# read census data 
# raw estimate of hispanic latino
pima_hisp_latino <- get_acs(geography = "tract",
                    variables = c("Hispanic_Latino" = "B03002_012"),
                    state = "AZ", 
                    county = "Pima",
                    geometry = TRUE)

# plot to preview 
pima_hisp_latino %>%
  ggplot(aes(fill = estimate)) +
  geom_sf() +
  scale_fill_viridis_b() +
  theme_map()

# save to disk as shapefile for import to grass
st_write(pima_hisp_latino,
         dsn = "data/tidy/shapefiles/pima/hispanic_latino.shp",
         layer = "estimate")

# read census data 
# GINI INDEX OF INCOME INEQUALITY
pima_gini_index <- get_acs(geography = "tract",
                            variables = c("gini_index" = "B19083_001"),
                            state = "AZ", 
                            county = "Pima",
                            geometry = TRUE)

# preview
pima_gini_index %>%
  ggplot(aes(fill = estimate)) +
  geom_sf() +
  scale_fill_viridis_b() +
  theme_map()

# save to disk as shapefile for import to grass
st_write(pima_gini_index,
         dsn = "data/tidy/shapefiles/pima/gini_index.shp",
         layer = "estimate")

# read data 
ejscreen <- read_csv("data/raw/EJSCREEN_2019_USPR.csv.zip")

# inspect
glimpse(ejscreen)

# find the correct geographies for pima county
tracts_pima %>%
  summarise(min(GEOID), max(GEOID))

# filter to pima county 
ejscreen_pima <- ejscreen %>%
  as_tibble() %>%
  filter(str_detect(ID, "04019")) %>%
  mutate(GEOID = ID) %>%
  dplyr::select("GEOID", "CANCER") %>%
  drop_na("CANCER")

# inspect
glimpse(ejscreen_pima)
class(ejscreen_pima$GEOID)
class(tracts_pima$GEOID)

# attempt to join but it doesn't work 
inner_join(ejscreen_pima, tracts_pima, by = "GEOID")

