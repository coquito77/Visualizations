# this code uses US. Census data to 
# create population pyramids by Sup District


library(tidyverse)   # seudo-package used for data manipulation
library(tidycensus)  # package used to get data from the U.S. Census
library(sf)          # package used for working with shapefiles
library(arcpullr)    # A package for pulling spatial data from an ArcGIS REST API

options(scipen = 999)

ProjectFolder <-
  paste0(getwd(), "/Riv_CO_Pop_Pyramids")

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

if (!file.exists("./Riv_CO_Pop_Pyramids/data/CA_RI_RACE_2019-2023 5-year ACS.parquet")){
  
  # get shape file of Riverside County Supervisory Districts
  
  supDist2021 <- 
    get_spatial_layer("https://gis.countyofriverside.us/arcgis_mapping/rest/services/OpenData/SupervisorialDistricts/MapServer/0/") %>%
    #   change coordinate system 
    st_transform(., 26946) #   browseURL("https://www.conservation.ca.gov/cgs/rgm/state-plane-coordinate-system")
  
  census_api_key( Sys.getenv("CENSUS_API_KEY") ) # get your key from here:
  # https://api.census.gov/data/key_signup.html
  
  # get data of population by Sex by Age for the ACS 5year 2023 year
  
  variables2023 <- 
    load_variables('2023', "acs5", cache = TRUE)
  
  # get data by population and gender
  
  censusVars <-
    variables2023 %>% 
    filter(grepl("^B01001_", name )) %>%
    filter(!grepl("Total:$|Male:$|Female:$", label)) 
  
  CO_Population <- get_acs(
    geography = "tract",
    state = "CA",
    cache_table = TRUE,
    county = "Riverside",
    variables = censusVars$name %>% sort,
    year = 2023,
    survey = "acs5",
    geometry = TRUE) %>% 
    sf::st_transform(., 26946) # see https://epsg.io/26946
  
  # find where the census tracts fall in each sup district 
  # using the intersection function and calculate the area covered
  
  startTime <- Sys.time()
  
  tracts_SupDist <- 
    supDist2021 %>%
    select(DISTRICT ) %>% 
    mutate(SupDistrictArea = st_area(.),  # calc area by Sup District
           SupDistrictArea = as.numeric(SupDistrictArea)) %>% 
    st_intersection(CO_Population %>%
                      select(GEOID) %>% 
                      distinct() %>% 
                      mutate(tractArea = st_area(.), # calc area by tract
                             tractArea = as.numeric(tractArea))) %>% 
    mutate(tractSupDistArea = st_area(.),  # this is the area of the tract by SupDist
           tractSupDistArea = as.numeric(tractSupDistArea),
           pcnt_Area_Tract_Sup_ovr_Tract = tractSupDistArea/tractArea )
  
  Sys.time() - startTime # Time difference of 9.094202 mins
  
  if (FALSE) { 
    # create a data frame of Census Tracts that are split by more than one Sup District
    
    Split_Tracts <- 
      tracts_SupDist %>%
      st_drop_geometry() %>%
      count( GEOID, sort = TRUE) %>%
      filter(n > 1) %>% 
      inner_join(  
        tracts_SupDist %>%
          st_drop_geometry() )
    
    View(Split_Tracts)
    
    # group percentages into quarters
    
    Split_Tracts$pcnt_cuts <- cut(Split_Tracts$pcnt_Area_Tract_Sup_ovr_Tract,
                                  seq(0,1, by = .25))
    
    View(Split_Tracts)
    
    # get counts of the split tracts
    
    Split_Tracts %>% 
      count(pcnt_cuts) %>% 
      mutate(pcnt = n/sum(n))
    
    Split_Tracts %>% 
      count(n, pcnt_cuts) %>% 
      mutate(pcnt = n/sum(n))
    
    # view the records in wide form
    
    Split_Tracts %>% #head(50) %>% 
      select(GEOID, pcnt_Area_Tract_Sup_ovr_Tract) %>% 
      group_by(GEOID) %>%
      mutate(id = row_number()) %>%
      pivot_wider(names_from = id, values_from = pcnt_Area_Tract_Sup_ovr_Tract)  %>% 
      setNames( make.names(names(.), unique = TRUE)) %>% 
      mutate(sumVar = sd(c_across(X1:X3), na.rm = TRUE)) -> Split_tracts_wide
    
    Split_tracts_wide$pcnt_cuts <- cut(Split_tracts_wide$sumVar,
                                       seq(0,1,by = .25))
    
    View(Split_tracts_wide)
    
    Split_tracts_wide %>% 
      ungroup() %>% 
      count( pcnt_cuts) %>% 
      mutate(pcnt = n/sum(n)) # the majority of tracts (0.5,0.75] n =63 0.685 are almost all in one SD
    
    # some tracts may be split in more than one Sup District
    # create a chart to explain this
  }
  
  if (FALSE) {
    
    split_tract <- CO_Population %>%
      filter(GEOID == "06065042717") %>%
      slice(1) %>% 
      select(GEOID)
    
    # create square buffers by 25, 5 and 1 miles. One mile is 5280 feet
    Buffer_25_Miles <- split_tract %>% 
      st_buffer(., 5280 * 25) %>%
      st_bbox 
    
    Buffer_05_Miles <- split_tract %>% 
      st_buffer(., 5280 * 5) %>%
      st_bbox
    
    Buffer_01_Miles <- split_tract %>% 
      st_buffer(., 5280 * 1) %>%
      st_bbox
    
    split_tract_p <- 
      ggplot() +
      geom_sf(data = supDist2021, aes(fill = DISTRICT),
              linewidth = .1, alpha = .7) +
      coord_sf(#default_crs = sf::st_crs(2229),
        xlim = c(Buffer_25_Miles[3]
                 ,Buffer_25_Miles[1]), 
        ylim = c(Buffer_25_Miles[2]
                 ,Buffer_25_Miles[4]),
        expand = FALSE) +
      scale_fill_distiller(palette = "Set1") +
      theme(legend.position = "none")
    
    split_tract_p <- 
      split_tract_p +
      geom_sf(data = Buffer_05_Miles %>% 
                st_as_sfc, aes(), 
              color = "black",
              fill = "NA",
              linewidth = 1, alpha = .9) +
      geom_sf(data = split_tract, fill = NA, color = "red", 
              linewidth = .1, alpha = .9) +
      coord_sf(#default_crs = sf::st_crs(2229),
        xlim = c(Buffer_25_Miles[3]
                 ,Buffer_25_Miles[1]), 
        ylim = c(Buffer_25_Miles[2]
                 ,Buffer_25_Miles[4]),
        expand = FALSE)
    
    library(cowplot)
    library(scales)
    
    tmpLbs <- 
      tracts_SupDist %>%
      st_drop_geometry() %>% 
      select(GEOID, DISTRICT, pcnt_Area_Tract_Sup_ovr_Tract) %>% 
      filter(split_tract$GEOID == GEOID) %>% 
      arrange(desc(pcnt_Area_Tract_Sup_ovr_Tract))
    
    title <- 
      paste0("Census Tract ", split_tract$GEOID,
             "\nis split between Supervisorial Districs ",
             tmpLbs[1,2],  ", ", tmpLbs[2,2], ", and ", tmpLbs[3,2], ", ",
             percent(tmpLbs[1,3]),  ", ", percent(tmpLbs[2,3]), ", and ", percent(tmpLbs[3,3]),
             " respectively")
    
    subtitle <-
      paste0( "For this analysis all the population of this tract was assigned to Sup. District ", 
              tmpLbs[1,2] )
    
    areaPlot <- 
      ggplot() +
      geom_sf(data = supDist2021, aes(fill = DISTRICT), alpha = .3) +
      geom_sf(data = split_tract, fill = NA, color = "red", linewidth = 1, alpha = .9) + 
      geom_sf_text(data = split_tract, 
                   aes(label =  GEOID), 
                   color = "red",
                   fun.geometry = st_centroid, alpha = .9) +
      coord_sf(#default_crs = sf::st_crs(2229),
        xlim = c(Buffer_01_Miles[3]
                 ,Buffer_01_Miles[1]),
        ylim = c(Buffer_01_Miles[2]
                 ,Buffer_01_Miles[4]),
        expand = FALSE) +
      scale_fill_distiller(palette = "Set1") +
      theme(legend.position = "none",
            panel.border = element_rect(colour = "black", fill = NA, linewidth = 3)) +
      labs(title = title,
           subtitle = subtitle,
           x = "",
           y = "")
    
    final_inset_plot_split <- 
      areaPlot %>% 
      ggdraw() +
      draw_plot(
        {
          split_tract_p
        },
        x = 0.58, 
        y = 0,
        width = 0.36, 
        height = 0.36)
    
    Cairo::CairoPDF(file = "./Riv_CO_Pop_Pyramids/plots/final_inset_plot_split.pdf",
                    #units = "in", dpi = 150,
                    width = 11, 
                    height = 8, 
                    pointsize = 20)
    
    final_inset_plot_split
    
    dev.off()
    
    system(paste0('open "', './Riv_CO_Pop_Pyramids/plots/final_inset_plot_split.pdf', '"'))
    
  }
  
  # create a data frame of deduplicated Census Tracts
  
  Dedup_tracts_SupDist <- 
    tracts_SupDist %>%
    st_drop_geometry() %>% 
    select(GEOID, DISTRICT, pcnt_Area_Tract_Sup_ovr_Tract) %>%
    arrange(desc(pcnt_Area_Tract_Sup_ovr_Tract)) %>% # sort by area spliced 
    distinct(GEOID, .keep_all = TRUE)
  
  # join the data of population estimates with the data of duplicated
  # Census Tracts
  
  comboDat <- 
    CO_Population %>% 
    st_drop_geometry() %>% 
    select(GEOID, variable, estimate ) %>% 
    full_join( Dedup_tracts_SupDist ) %>%          # dedup by where tract with largest area wins
    left_join(censusVars %>% select(variable = name, label))
  
  # clean up the data
  comboDat <-
    comboDat %>% left_join(
      comboDat %>% distinct(label) %>% 
        mutate(AgeGroup = sub("Estimate!!Total:!!", "", label)) %>% 
        separate(AgeGroup, c("SEX", "AGEGROUP"), sep = ":!!") %>% 
        mutate(AGEGROUP = sub(" years","", AGEGROUP),
               AGEGROUP = sub(" and over"," and older", AGEGROUP),
               AGEGROUP = sub("15 to 17|18 and 19", "15 to 19", AGEGROUP),
               AGEGROUP = sub("20|21|22 to 24", "20 to 24", AGEGROUP),
               AGEGROUP = sub("60 and 61|62 to 64", "60 to 64", AGEGROUP),
               AGEGROUP = sub("65 and 66|67 to 69", "65 to 69", AGEGROUP)))
  
  censusVarRace <-
    variables2023 %>% 
    filter(grepl("^B01001\\D", name )) %>%
    filter(!grepl("Total:$|Male:$|Female:$", label)) %>% 
    filter(grepl("\\(", concept))
  
  # get data by Sex by Age and Race for Riverside County 2023 year 5-year ACS
  
  CoRace <- 
    get_acs(
      geography = "tract",
      state = "CA",
      cache_table = TRUE,
      county = "Riverside",
      variables = censusVarRace$name %>% sort,
      year = 2023,
      survey = "acs5",
      geometry = TRUE) %>% 
    sf::st_transform(., 2229)
  
  comboDatRace <- 
    CoRace %>% 
    st_drop_geometry() %>% 
    select(GEOID, variable, estimate ) %>% 
    full_join( Dedup_tracts_SupDist) %>% 
    left_join(censusVarRace %>% select(variable = name, label, concept)) 
  
  comboDatRace <-
    comboDatRace %>%
    left_join(
      comboDatRace %>%
        distinct(label) %>% 
        mutate(AgeGroup = sub("Estimate!!Total:!!", "", label)) %>% 
        separate(AgeGroup, c("SEX", "AGEGROUP"), sep = ":!!") %>% 
        mutate(AGEGROUP = sub(" years","", AGEGROUP),
               AGEGROUP = sub(" and over"," and older", AGEGROUP),
               AGEGROUP = sub("15 to 17|18 and 19", "15 to 19", AGEGROUP))) 
  
  rm(list = setdiff(ls(), c("comboDat",
                            "comboDatRace",
                            "supDist2021")))
  
  ifelse(  # create folder to save shapefile
    !dir.exists(file.path("./Riv_CO_Pop_Pyramids/data/RiCoMap"))
    , dir.create(file.path("./Riv_CO_Pop_Pyramids/data/RiCoMap"))
    , FALSE)
  
  st_write(supDist2021,
           "./Riv_CO_Pop_Pyramids/data/RiCoMap/RivCO_SupDist_2021.shp", delete_layer = TRUE)
  
  arrow::write_parquet(comboDat,
                       "./Riv_CO_Pop_Pyramids/data/CA_RI_POP_2019-2023 5-year ACS.parquet")
  
  arrow::write_parquet(comboDatRace,
                       "./Riv_CO_Pop_Pyramids/data/CA_RI_RACE_2019-2023 5-year ACS.parquet")
  
}

rm(list = ls()) #start with empty workspace
gc()

# download supervisors head shots
# the head shots are located here: https://rivco.org/board-supervisors

library(httr)

Links_Supervisor_Headshots <-
  GET("https://rivco.org/board-supervisors",
      user_agent("Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/51.0.2704.103 Safari/537.36")) %>% 
  read_html() %>%
  html_nodes(css = "img") %>%
  html_attr("src") %>% 
  .[grepl("public",.)] %>% 
  paste0("https://rivco.org/", .)

# download each image and save it with a clean name

Links_Supervisor_Headshots %>% 
  lapply(., function(x)  download.file(x, 
                                       paste0("./Riv_CO_Pop_Pyramids/data/",
                                              basename(x) %>% 
                                                sub("(.*.jpg).*$","\\1", .) %>%
                                                gsub("\\%20", "-",.) %>% 
                                                gsub("-+", "_",.))
                                       , mode = "wb"))

rm(list = ls()) #start with empty workspace

comboDat <- arrow::read_parquet("./Riv_CO_Pop_Pyramids/data/CA_RI_POP_2019-2023 5-year ACS.parquet")

comboDatRace <- arrow::read_parquet( "./Riv_CO_Pop_Pyramids/data/CA_RI_RACE_2019-2023 5-year ACS.parquet")

# clean up the data by age and sex

Sup_Age_Sex <- 
  comboDat %>% 
  group_by(DISTRICT, SEX, AGEGROUP) %>%
  summarise(value = sum(estimate, na.rm = TRUE)) %>% 
  filter(!is.na(DISTRICT)) %>% 
  mutate(value = ifelse(SEX == "Male", -value, value),
         AGEGROUP = sub(" years","", AGEGROUP),
         AGEGROUP = sub(" and over"," and older", AGEGROUP),
         AGEGROUP = as.factor(AGEGROUP),
         AGEGROUP = fct_relevel(AGEGROUP, "Under 5",
                                "5 to 9","10 to 14",
                                "15 to 19", "20 to 24",
                                "25 to 29","30 to 34",
                                "35 to 39","40 to 44",
                                "45 to 49","50 to 54",
                                "55 to 59","60 to 64",
                                "65 to 69","70 to 74",
                                "75 to 79","80 to 84",
                                "85 and older"),
         DISTRICT = paste("Sup. District", DISTRICT)) %>% 
  ungroup 

# validate total pop estimated counts
sum(abs(Sup_Age_Sex$value))

# validate total counts by sup district
Sup_Age_Sex %>% 
  group_by(DISTRICT) %>% summarise(totalPop = sum(abs(value)))

# create a function that will be used for the plots

plot_function <- function() {
  list(
    theme_minimal(base_family = "Arial", 
                  base_size = 12),
    scale_x_continuous(
      labels = ~ number_format(scale = .001, suffix = "k")(abs(.x)),
      limits = 30000 * c(-1,1)  ),
    facet_wrap(~DISTRICT, ncol =2),
    scale_y_discrete(labels = ~ str_remove_all(.x, "Age\\s|\\syears")),
    labs(x = "", 
         y = "Age groups", 
         title = "Population structure Riverside County by
       Supervisorial District", 
         fill = "", 
         caption = "Data source: US Census Bureau American Community Survey 2019-2023 5-year - population estimates & tidycensus R package")
  )
  
}

# create plots, Age and Sex, Age and Sex with excess counts and Age and Race

# plot by Sex and Gender

p_Sex_Gender <- 
  ggplot() + 
  geom_col(data = Sup_Age_Sex, 
           aes(x = value, 
               y = AGEGROUP, 
               fill = SEX), width = 0.95, 
           alpha = 0.75) + 
  geom_text(data = 
              Sup_Age_Sex %>% 
              filter(SEX == "Male",
                     grepl("1", DISTRICT), 
                     grepl("74", AGEGROUP)) %>%
              distinct(DISTRICT, y = AGEGROUP) %>%
              ungroup %>%
              mutate(x = -25000), aes(x = x, y = y, 
                                      label = "Male"),
            color = "#2c7fb8") +
  geom_text(data = Sup_Age_Sex %>% 
              filter(SEX == "Female",
                     grepl("1", DISTRICT), 
                     grepl("74", AGEGROUP)) %>%
              distinct(DISTRICT, y = AGEGROUP) %>%
              ungroup %>%
              mutate(x = 25000), aes(x = x, y = y, 
                                     label = "Female"),
            color = "#de2d26") +
  scale_fill_manual(name = "Sex", values = c("#de2d26", "#2c7fb8")) +
  plot_function() + 
  theme(strip.text = element_text(
    size = 10, face = "bold",colour = "grey25"),
    legend.position = "none",
    text = element_text(colour = "grey25"),
    legend.text = element_text(size = 7)) 

# calculate excess counts by gender from data

tmp <- 
  Sup_Age_Sex %>% 
  group_by(DISTRICT, AGEGROUP) %>% 
  mutate(value = abs(value)) %>% 
  pivot_wider(names_from = SEX, values_from = value) %>% 
  mutate(excessMale = case_when(Female < Male ~ Male - Female),
         excessFemale = case_when(Female > Male ~ Female - Male),
         across(where(is.numeric), ~ replace_na(.x, 0)),
         BaseMale = Male - excessMale,
         BaseFemale = Female - excessFemale) %>% 
  select(-c(Female, Male )) %>%
  pivot_longer(-c(DISTRICT, AGEGROUP)) %>% 
  mutate(SEX = case_when(grepl("Male",name) ~ "Male",
                         TRUE ~ "Female"),
         name  = as.factor(name ),
         name  = fct_relevel(name , 
                             "excessFemale","BaseFemale","excessMale",
                             "BaseMale"),
         value = ifelse(SEX == "Male", -value, value))

# create plot by Age and Sex with Excess counts

p_Sex_Gender_Excess <- 
  ggplot() + 
  geom_col(data = tmp, 
           aes(x = value, 
               y = AGEGROUP, 
               fill = name), width = 0.95, alpha = 0.75) + 
  geom_text(data =
              tmp %>% 
              filter(SEX == "Male",
                     grepl("1", DISTRICT), 
                     grepl("79", AGEGROUP)) %>%
              distinct(DISTRICT, y = AGEGROUP) %>%
              ungroup %>%
              mutate(x = -25000), aes(x = x, y = y, 
                                      label = "Male"),
            color = "#006d2c") +
  geom_text(data = tmp %>% 
              filter(SEX == "Female",
                     grepl("1", DISTRICT), 
                     grepl("79", AGEGROUP)) %>%
              distinct(DISTRICT, y = AGEGROUP) %>%
              ungroup %>%
              mutate(x = 25000), aes(x = x, y = y, 
                                     label = "Female"),
            color = "#993404") +
  plot_function() +
  theme(legend.position = "inside", 
        legend.position.inside = c(0.75, 0.15),
        text = element_text(colour = "grey25"),
        strip.text = element_text(
          size = 10, face = "bold",colour = "grey25"),
        legend.key.height = unit(.5, 'cm')) +
  scale_fill_manual(name = "Sex",
                    values = c("#d95f0e", "#993404",
                               "#2ca25f", "#006d2c"),
                    labels = c("Excess Female", "Base Female", 
                               "Excess Male", "Base Male"))

# create data for Age and Race counts by Gender

Sup_Age_RaceGender <-
  comboDatRace %>% 
  filter(grepl("ALONE\\)|RACES\\)", concept, ignore.case = TRUE)) %>% 
  left_join(comboDatRace %>% 
              distinct(concept) %>% 
              mutate(clnConcept = gsub("SEX BY AGE \\(|\\)","", concept))) %>% 
  group_by(DISTRICT, SEX, AGEGROUP, clnConcept) %>%
  summarise(value = sum(estimate, na.rm = TRUE)) %>% 
  filter(!is.na(DISTRICT)) %>% 
  mutate(value = ifelse(SEX == "Male", -value, value),
         AGEGROUP = sub(" years","", AGEGROUP),
         AGEGROUP = sub(" and over"," and older", AGEGROUP),
         AGEGROUP = as.factor(AGEGROUP),
         AGEGROUP = fct_relevel(AGEGROUP, "Under 5",
                                "5 to 9","10 to 14",
                                "15 to 19", "20 to 24",
                                "25 to 29","30 to 34",
                                "35 to 44",
                                "45 to 54",
                                "55 to 64",
                                "65 to 74",
                                "75 to 84",
                                "85 and older"),
         clnConcept = sub("Sex by Age \\(" ,"", clnConcept),
         DISTRICT = paste0("Sup. District ", DISTRICT)) %>% 
  ungroup

sum(abs(Sup_Age_RaceGender$value))

# plot data by Age, Race and Gender

p_Sex_Race <- 
  ggplot() + 
  geom_col(data = Sup_Age_RaceGender ,
           aes(x = value, 
               y = AGEGROUP, 
               group = SEX,
               fill = clnConcept ), width = 0.95, alpha = 0.75) + 
  geom_text(data = Sup_Age_RaceGender %>% 
              filter(SEX == "Male",
                     grepl("1", DISTRICT), 
                     grepl("84", AGEGROUP)) %>%
              distinct(DISTRICT, y = AGEGROUP) %>%
              ungroup %>%
              mutate(x = -25000), aes(x = x, y = y, 
                                      label = "Male")) +
  geom_text(data = Sup_Age_RaceGender %>% 
              filter(SEX == "Female",
                     grepl("1", DISTRICT), 
                     grepl("84", AGEGROUP)) %>%
              distinct(DISTRICT, y = AGEGROUP) %>%
              ungroup %>%
              mutate(x = 25000), aes(x = x, y = y, 
                                     label = "Female")) +
  scale_fill_brewer(name = "Race",
                    palette = "Dark2") +
  plot_function() +
  scale_x_continuous(
    labels = ~ number_format(scale = .001, suffix = "k")(abs(.x)),
    limits = 40000 * c(-1,1)  ) +  # need to readjust the limits because of bigger groups
  theme(legend.position = "inside", legend.position.inside = c(0.80, 0.15),
        text = element_text(colour = "grey25"),
        strip.text = element_text(
          size = 10, face = "bold",colour = "grey25"),
        legend.text = element_text(size = 8),
        legend.key.height = unit(.5, 'cm'))

# validate total counts

if (FALSE ) Sup_Age_Sex %>%
  group_by(DISTRICT, SEX ,AGEGROUP ) %>%
  summarise(sex = sum(abs(value))) %>% 
  full_join( Sup_Age_RaceGender %>%
               group_by(DISTRICT, SEX ,AGEGROUP ) %>%
               summarise(Race_Gender = sum(abs(value))) ) %>% 
  full_join( tmp %>% 
               group_by(DISTRICT, SEX ,AGEGROUP ) %>%
               summarise(Race_Gender_Excess = sum(abs(value)))  ) %>% 
  View("validation_Counts")

# create map of LACO Sup Districts

supDist2021 <- 
  sf::st_read("./Riv_CO_Pop_Pyramids/data/RiCoMap/RivCO_SupDist_2021.shp")

st_centroid_within_poly <- function(poly) {
  
  # check if centroid is in polygon
  centroid <- poly %>% st_centroid() 
  in_poly <- st_within(centroid, poly, sparse = F)[[1]] 
  
  # if it is, return that centroid
  if (in_poly) return(centroid) 
  
  # if not, calculate a point on the surface and return that
  centroid_in_poly <- st_point_on_surface(poly) 
  return(centroid_in_poly)
}

# determine where to place Sup District labels

lbsDPSS_supDist2021 <- 
  supDist2021 %>% 
  mutate(lon = map_dbl(geometry, ~st_centroid_within_poly(.x)[[1]]),
         lat = map_dbl(geometry, ~st_centroid_within_poly(.x)[[2]])) %>% 
  st_set_geometry(., NULL) 

# create map of the County by Sup District

p_Sup_Dist_Map <- 
  ggplot() +
  geom_sf(data = supDist2021,
          aes(fill = DISTRIC),
          color = "purple4",
          size = .01, show.legend = FALSE, alpha = .5) +
  scale_fill_distiller(palette = "Set1") +
  geom_label(data = lbsDPSS_supDist2021
             ,aes(label = DISTRIC, x = lon, y = lat)
             ,alpha = 0.75
             ,size = 3) +
  theme_minimal(base_family = "Arial", 
                base_size = 12)

# process the Supervisors headshots 

require(grid)
require(png)
library(magick)

annotation_custom2 <- 
  function(grob, xmin = -Inf, xmax = Inf, 
           ymin = -Inf, ymax = Inf, data){ layer(data = data, 
                                                 stat = StatIdentity,
                                                 position = PositionIdentity, 
                                                 geom = ggplot2:::GeomCustomAnn,
                                                 inherit.aes = TRUE,
                                                 params = list(grob = grob,
                                                               xmin = xmin, xmax = xmax, 
                                                               ymin = ymin, ymax = ymax))}

# create an image of a circle that will be used to modify the Supervisors headshots

png(tf <- tempfile(fileext = ".png"), 1000, 1000)
par(mar = rep(0, 4), yaxs = "i", xaxs = "i")
plot(  0,
       type = "n",
       ylim = c(0, 1),
       xlim = c(0, 1),
       axes = F,
       xlab = NA,
       ylab = NA)
plotrix::draw.circle(.5, 0.5, .5, col = "black")
dev.off()

# get list of Sup District Supervisors

png_files <- 
  list.files("./Riv_CO_Pop_Pyramids/data",
             pattern = "jpg$",
             full.names = TRUE) %>% 
  .[!grepl("Group_Phot",.)]

# Process each headshot to create images to plot on first population pyramid chart

for (i in seq_along(png_files)) {
  
  x <- 
    image_read(png_files[i]) %>% 
    image_resize(geometry_size_pixels(1000, 1000, preserve_aspect = TRUE))
  
  mask <- 
    image_read(tf) %>% 
    image_scale(., as.character(image_info(x)$width - 200))
  
  # compose_types()
  
  x <- 
    image_composite(mask, x, 
                    operator = "Plus")
  
  x <-
    image_fill(x, "transparent", point = "+790+790", fuzz = 10) %>% # bottom right
    image_fill(., "transparent", point = "+1+1", fuzz = 10) %>% # top left
    image_fill(., "transparent", point = "+1+790", fuzz = 10)  %>% # bottom left
    image_fill(., "transparent", point = "+790+1", fuzz = 10) # top left
  
  # create objects to plot on the map
  
  plot_pic <- 
    annotation_custom2(
      rasterGrob(x, interpolate = TRUE),
      xmin = 11500, xmax = 22000, 
      ymin = 16.75, ymax = 18.5,
      data = data.frame(DISTRICT = case_when(grepl("Medina_", png_files[i]) ~ "Sup. District 1",
                                             grepl("_Spiegel", png_files[i]) ~ "Sup. District 2",
                                             grepl("Washington_", png_files[i]) ~ "Sup. District 3",
                                             grepl("Perez_", png_files[i]) ~ "Sup. District 4",
                                             grepl("_Gutierre", png_files[i]) ~ "Sup. District 5")))
  
  assign(  case_when(grepl("Medina_", png_files[i]) ~ "bos_pic1",
                     grepl("_Spiegel", png_files[i]) ~ "bos_pic2",
                     grepl("Washington_", png_files[i]) ~ "bos_pic3",
                     grepl("Perez_", png_files[i]) ~ "bos_pic4",
                     grepl("_Gutierre", png_files[i]) ~ "bos_pic5") , plot_pic , pos = 1 )
  
  districts <- 
    case_when(grepl("Medina_", png_files[i]) ~ "1",
              grepl("_Spiegel", png_files[i]) ~ "2",
              grepl("Washington_", png_files[i]) ~ "3",
              grepl("Perez_", png_files[i]) ~ "4",
              grepl("_Gutierre", png_files[i]) ~ "5")
  
  supCoords <- 
    lbsDPSS_supDist2021[which(lbsDPSS_supDist2021$DISTRIC == districts), c(6:7)]
  
  plot_map <- 
    annotation_custom2(
      rasterGrob(x, interpolate = TRUE),
      xmin = supCoords$lon - 12000, xmax = supCoords$lon + 12000, 
      ymin = supCoords$lat + 2500, ymax = supCoords$lat + 7500,
      data = data.frame(DIST = case_when(grepl("Medina_", png_files[i]) ~ "Sup. District 1",
                                         grepl("_Spiegel", png_files[i]) ~ "Sup. District 2",
                                         grepl("Washington_", png_files[i]) ~ "Sup. District 3",
                                         grepl("Perez_", png_files[i]) ~ "Sup. District 4",
                                         grepl("_Gutierre", png_files[i]) ~ "Sup. District 5")))
  
  assign(  case_when(grepl("Medina_", png_files[i]) ~ "bos_map_pic1",
                     grepl("_Spiegel", png_files[i]) ~ "bos_map_pic2",
                     grepl("Washington_", png_files[i]) ~ "bos_map_pic3",
                     grepl("Perez_", png_files[i]) ~ "bos_map_pic4",
                     grepl("_Gutierre", png_files[i]) ~ "bos_map_pic5") , plot_map , pos = 1 )
  
  rm(x)
  
}

# create pdf file with charts

Cairo::CairoPDF(file = "./Riv_CO_Pop_Pyramids/plots/plots_pyramids.pdf",
                #units = "in", dpi = 150,
                width = 8, 
                height = 11, 
                pointsize = 20)

# plot Sex vs Gender and add the plots of the Sup District Supervisors

p_Sex_Gender + 
  bos_pic1 +
  bos_pic2 + 
  bos_pic3 + 
  bos_pic4 +
  bos_pic5

ggsave("./Riv_CO_Pop_Pyramids/plots/RIV_CO_POP_PYRM.png",
       width = 7.94,
       height = 8.12,
       units = "in",
       dpi = 300)

p_Sex_Gender_Excess

p_Sex_Race

p_Sup_Dist_Map +
  bos_map_pic1 +
  bos_map_pic2 +
  bos_map_pic3 +
  bos_map_pic4 +
  bos_map_pic5

dev.off()

system(paste0('open "', './Riv_CO_Pop_Pyramids/plots/plots_pyramids.pdf', '"'))



