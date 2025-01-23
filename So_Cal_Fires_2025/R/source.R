# This code uses open data to estimate tne number of homes'
# that are within the boundaries of the So Cal Fires of Jan 2025
# to estimate the total cost to rebuild those homes

library(tidyverse)
library(sf)
library(arcpullr)

options(scipen = 999)

ProjectFolder <-
  paste0(getwd(), "/So_Cal_Fires_2025")

ifelse(
  !dir.exists(file.path(ProjectFolder))
  , dir.create(file.path(ProjectFolder))
  , FALSE)

c("/R","/data","/plots") %>% 
  lapply(., function(x) ifelse(
    !dir.exists(paste0(ProjectFolder, x))
    , dir.create(paste0(ProjectFolder,x))
    , FALSE))

rm(list = ls()) #start with empty workspace

# download the file boundaries shapefile

socalFires <-
  get_spatial_layer("https://services.arcgis.com/BLN4oKB0N1YSgvY8/arcgis/rest/services/Southern_CA_Fire_Perimeters_Jan_2025_view/FeatureServer/0") %>%
  mutate(name = toupper(name)) %>% # change coordinate system to LA County official system
  st_transform(., 2229) %>%
  st_make_valid() %>%
  mutate(
    created = as.POSIXct(created/1000, origin = "1970-01-01", tz = "America/Los_Angeles"),
    area_feet = st_area(.),
    area_acres = area_feet/43560,
    area_acres = as.numeric(area_acres))

# get fire points

library(jsonlite)

firePoints <- fromJSON("https://rdipowerplatformfd-e5hhgqaahef7fbdr.a02.azurefd.net/incidents/incidents-gj.json")

df_firePoints <-
  firePoints$features$properties %>%
  select(name, updated, started, latitude, longitude) %>%
  st_as_sf(coords = c( "longitude", "latitude"),
           crs = 4269) %>% # change coordinate system to LA County official system
  st_transform(., 2229)

rm(firePoints)

# create buffers for each file

fireBuffers <-
  socalFires %>%
  st_union() %>%
  st_buffer(., dist = 1000) %>%
  st_sf(.) %>%
  rmapshaper::ms_explode() %>%
  mutate(id = row_number())

st_Boxes <-
  fireBuffers$geometry %>%
  lapply(., function(x) {
    st_bbox(x) %>%
      st_as_sfc()
  }) %>%
  do.call(rbind, .) %>%
  as.data.frame() %>%
  st_sf %>%
  mutate(id = row_number(),
         EventName = case_match( id,
                                 1 ~ "EATON",
                                 2 ~ "LIDIA",
                                 3 ~ "SUNSET",
                                 4 ~ "tmp3",
                                 5 ~ "HURST",
                                 6 ~ "PALISADES",
                                 7 ~ "ARCHER",
                                 8 ~ "KENNETH",
                                 9 ~ "AUTO"),
         id = as.character(id)) %>%
  sf::st_set_crs(., 2229) %>%
  rename(geometry = V1)

Buffer_Fires <-
  st_join(socalFires %>%
            rmapshaper::ms_explode(), st_Boxes)

# validate the output map
st_Boxes %>%
  ggplot() +
  geom_sf(aes(fill = id))

st_centroid_within_poly <- function (poly) {
  # check if centroid is in polygon
  centroid <- poly %>% st_centroid()
  in_poly <- st_within(centroid, poly, sparse = F)[[1]]
  
  # if it is, return that centroid
  if (in_poly) return(centroid)
  
  # if not, calculate a point on the surface and return that
  centroid_in_poly <- st_point_on_surface(poly)
  return(centroid_in_poly)
}

combo_Points <-
  st_Boxes %>%
  mutate(lon = map_dbl(geometry, ~st_centroid_within_poly(.x)[[1]]),
         lat = map_dbl(geometry, ~st_centroid_within_poly(.x)[[2]]))

Buffer_Fires %>%
  ggplot() +
  geom_sf(aes(fill = EventName), show.legend = FALSE) +
  ggrepel::geom_label_repel(data = combo_Points,
                            aes(x = lon, y = lat, label = EventName),
                            alpha = .5)

finalFires <-
  Buffer_Fires %>%
  group_by(id, EventName) %>%
  summarize %>%
  st_cast()

# download the file of Los Angeles County Parcels

if (!any(file.exists(list.files("So_Cal_Fires_2025/data/tmpzip/",
                            full.names = TRUE, pattern = "gdb"))) ) {
  
  td = tempdir()
  # create the placeholder file
  tf = tempfile(tmpdir = td, fileext = ".zip")
  library(httr)
  
  response <- GET("https://apps.gis.lacounty.gov/hubfiles/LACounty_Parcels.zip",
                  write_disk(tf, overwrite = TRUE))
  
  unzip(tf, exdir = "So_Cal_Fires_2025/data/tmpzip/")
  
  #check the layers from the unzip file
  
  st_layers(list.files("So_Cal_Fires_2025/data/tmpzip/",
                       full.names = TRUE, pattern = "gdb"))
  
}

# read the parcel data this is a huge file so it may take awhile to download

if (!file.exists("So_Cal_Fires_2025/data/LACO_PARCELS_202501.RData")) {
  
  Parcels <- sf::st_read(dsn = list.files("So_Cal_Fires_2025/data/tmpzip/",
                                          full.names = TRUE, pattern = "gdb")
                         ,layer = "LACounty_Parcels")
  
  dim(Parcels) # 2428282      91
  
  Parcels <- 
    Parcels %>%
    st_drop_geometry() %>% 
    select(CENTER_X,
           CENTER_Y,
           UseDescription,
           UseType)
  
  save(Parcels,
       file = "So_Cal_Fires_2025/data/LACO_PARCELS_202501.RData")
  
} else {
  load("So_Cal_Fires_2025/data/LACO_PARCELS_202501.RData")
}

parcel_Centers <-
  Parcels %>%
  st_drop_geometry() %>%
  st_as_sf(coords = c("CENTER_X", "CENTER_Y"),
           crs = 2229)

# find parcels that are inside the fire boundaries

startTime <- Sys.time()

addrs_in_fire_areas <-
  st_intersects(
    parcel_Centers,
    finalFires,
    sparse = T)

Sys.time() - startTime # Time difference of 14.41596 secs

df_addrs_in_fire_areas <-
  addrs_in_fire_areas %>%
  as.data.frame() %>%
  rename(rowid = row.id,
         id = col.id)

addrs_with_fire <-
  parcel_Centers %>%
  rowid_to_column() %>%
  left_join(df_addrs_in_fire_areas) %>%
  filter(!is.na(id)) %>%
  mutate(id = as.character(id)) %>%
  left_join(
    finalFires %>%
      st_drop_geometry() %>%
      distinct(id, EventName))

rm(addrs_in_fire_areas)

finalFires %>%
  ggplot() +
  geom_sf(aes(fill = EventName),
          show.legend = TRUE) +
  geom_sf(data = addrs_with_fire %>%
            select(id) %>%
            sample_n(500),
          aes(), color = "gray", alpha = .7) +
  scale_fill_manual(name = "",values = RColorBrewer::brewer.pal(9, "Spectral")) +
  theme(legend.position = "bottom")

ggsave("So_Cal_Fires_2025/plots/So_Cal_Fires_2025.png")

addrs_with_fire %>%
  st_drop_geometry() %>%
  count(EventName, UseType) %>%
  pivot_wider(names_from = UseType, values_from = n)

library(scales)

# calculate monetary amount using median home price

est_homes_burned <- .8

tmp <-
  addrs_with_fire %>%
  st_drop_geometry() %>%
  filter(UseType == "Residential") %>%
  count(EventName) %>% 
  mutate(PcntHomesBurned = n * est_homes_burned,
         HomePrice = case_when(EventName == "EATON" ~ 
                                 1248945 - 400000,  # https://www.zillow.com/home-values/30187/altadena-ca/
                               EventName == "PALISADES" ~ 
                                 3462178 - 400000, # https://www.zillow.com/home-values/19810/pacific-palisades-los-angeles-ca/
                               TRUE ~ 948383 - 400000), # https://www.zillow.com/home-values/12447/los-angeles-ca/
         TotalResidentialLosses = PcntHomesBurned * HomePrice)

label_currency(accuracy = 1, prefix = "USD ")(sum(tmp$TotalResidentialLosses)) %>%
  trimws()

label_currency(accuracy = .01, scale = 1e-09, prefix = "USD ",  suffix = " Billion")(sum(tmp$TotalResidentialLosses)) %>%
  trimws()


