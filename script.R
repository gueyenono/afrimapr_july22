# Load packages ----

pacman::p_load(
  afrihealthsites, # African health sites
  dplyr, # Data manipulation
  ggplot2, # Data visualization
  here, # Relative paths
  janitor, # Data cleaning
  nngeo, # k-nearest neighbors
  purrr, # Functional programming
  readxl, # For importing Excel files
  sf, # Spatial data manipulation
  tmap # Mapping
)

# Set mapping mode to interactive
tmap_mode(mode = "view")

sf_use_s2(FALSE) # To avoid an error regarding "duplicate vertex with edge" (source: https://github.com/r-spatial/sf/issues/1762)

# ----------------------------------------------------------------------------------
# IMPORT THE DATA ----
# ----------------------------------------------------------------------------------

# > Country polygon
geo_country_raw <- st_read(dsn = here("data/hdx/civ_admbnda_adm0_cntig_20180706/civ_admbnda_adm0_cntig_20180706.shp")) %>%
  st_transform(4326)


# > Population for each health district
pop_hd_raw <- read_xls(here("data/BD ETS SANITAIRES_avec coord gps07062022.xls"), sheet = 2, skip = 4)

pop_hd_data <- pop_hd_raw %>%
  setNames(c("region", "district", "pop")) %>%
  filter(!is.na(district), !is.na(pop)) %>%
  select(-region) 

# > Health districts
geo_hd_raw <- st_read(dsn = here("data/admin_divisions_2020/DISTRICT SANITAIRE 2020.shp")) %>% 
  st_transform(4326) %>%
  left_join(y = pop_hd_data, by = c("NOM" = "district")) %>%
  mutate(
    pop = case_when(
      NOM %in% c("KORHOGO 1", "KORHOGO 2") ~ 634501,
      NOM == "KOUASSI-KOUSSIKRO" ~ 34402,
      NOM == "BOUAKE NOURD-OUEST" ~ 435618,
      TRUE ~ pop
    )
  )

# > Health sites (1) (from the afrihealthsites R package)
geo_hs_raw_1 <- afrihealthsites(country = "CIV", plot = FALSE)

# > Health sites (2) (from the country's administration)
hs_raw <- read_xls(here("data/BD ETS SANITAIRES_avec coord gps07062022.xls"))

geo_hs_raw_2 <- hs_raw %>%
  clean_names() %>%
  select(nom_etablissement, longitude, latitude, niveau_etablissement, statut_etablissement) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)


# ----------------------------------------------------------------------------------
# ---- OBJECTIVE 1: STATE OF THINGS ----
# ----------------------------------------------------------------------------------

# 1.a - All health sites ----

# > Map all health sites in the country (points)

tm_shape(shp = geo_country_raw) +
  tm_polygons() +
  tm_shape(shp = geo_hs_raw_2) +
  tm_dots(col = "red")

# > Map of all health sites with health districts (points)

tm_shape(shp = geo_hd_raw) +
  tm_polygons() +
  tm_shape(shp = geo_hs_raw_2) +
  tm_dots(col = "red", alpha = 0.4)


# 1.b - Determine appropriate health sites for storage and handling of cure ----

# > Distribution of health sites by type

# >> Bar chart
hs_count_type <- geo_hs_raw_1 %>%
  st_drop_geometry() %>%
  count(amenity) %>%
  mutate(amenity = reorder(amenity, n))

ggplot(data = hs_count_type) +
  geom_col(mapping = aes(x = amenity, y = n)) +
  labs(x = "", y = "") +
  coord_flip() +
  theme_bw()

# >> Map

geo_app_hs <- geo_hs_raw_1 %>%
  select(osm_id, amenity, completeness) %>%
  filter(
    amenity %in% c("clinic", "doctors", "hospital"), 
    completeness >= 20
  )

tm_shape(shp = geo_hd_raw) +
  tm_polygons() +
  tm_shape(shp = geo_app_hs) +
  tm_dots(col = "red")


# 1.c Determine health districts with no appropriate health sites ----

# > Find intersections between health districts and health sites
no_app_hs_intersections <- st_intersects(x = geo_hd_raw, y = geo_app_hs)
n_app_hs <- map_dbl(no_app_hs_intersections, length)

geo_hd <- geo_hd_raw %>%
  mutate(n_app_hs = n_app_hs) %>%
  rename(name = NOM)

# > Distribution of health districts with at least one appropriate health facility for the cure

tm_shape(shp = geo_hd) +
  tm_borders() +
  tm_fill(col = "n_app_hs", breaks = c(0, 1, 5, seq(10, 60, 10))) +
  tm_layout(legend.outside = TRUE)


# --------------------------------------------------------------------------------
# OBJECTIVE 2: CAPACITY BUILDING ----
# --------------------------------------------------------------------------------

# 2.a Get the centroids of each health district ----

geo_hd_centroids <- st_centroid(x = geo_hd)

tm_shape(shp = geo_hd_raw) +
  tm_polygons() +
  tm_shape(shp = geo_app_hs) +
  tm_dots(col = "red") +
  tm_shape(shp = geo_hd_centroids) +
  tm_dots(col = "darkgreen", size = 0.01)


# 2.b For each health district, find out the health facility which is closest to the centroid ----

geo_nearest_hs_indices <- st_nn(x = geo_hd_centroids, y = geo_hs_raw_2, k = 1)
geo_nearest_hs <- geo_hs_raw_2 %>% slice(unlist(geo_nearest_hs_indices))

tm_shape(shp = geo_hd_raw) +
  tm_polygons() +
  tm_shape(shp = geo_hd_centroids) +
  tm_dots(col = "darkgreen", size = 0.01) +
  tm_shape(shp = geo_nearest_hs) +
  tm_dots(col = "red")


# 2.c In the health districts with appropriate health sites, do we still need to upgrade some more optimally located health sites? (appropriate health sites over 50 km from centroid) ----

# > Visualize
geo_buffers_50km <- geo_hd_centroids %>%
  # filter(n_app_hs > 0) %>%
  st_transform(crs = 6829) %>% # Just to be able to use meters to create the buffer
  st_buffer(dist = 50000) %>% # 50km buffer
  st_transform(crs = 4326)

tm_shape(shp = geo_hd) +
  tm_polygons() +
  tm_shape(shp = geo_app_hs) +
  tm_dots(col = "red") +
  tm_shape(shp = geo_hd_centroids %>% filter(n_app_hs > 0)) +
  tm_dots() +
  tm_shape(shp = geo_buffers_50km %>% filter(n_app_hs > 0)) +
  tm_fill(col = "darkgreen", alpha = 0.5)

# > Filter the corresponding health districts
buffers_50km_app_hs_intersections <- st_intersects(x = geo_buffers_50km %>% filter(n_app_hs > 0), y = geo_app_hs)

buffers_no_intersection <- geo_buffers_50km %>%
  filter(n_app_hs > 0) %>%
  mutate(n_app_hs_within_50km = map_dbl(buffers_50km_app_hs_intersections, length)) %>%
  filter(n_app_hs_within_50km == 0) %>%
  st_transform(crs = 4326)

geo_hd_need_add_upgr <- geo_hd %>%
  filter(name %in% buffers_no_intersection$name)

tm_shape(shp = geo_hd) +
  tm_polygons() +
  tm_shape(shp = geo_hd_need_add_upgr) +
  tm_polygons(col = "red")


# 2.d What are the perfectly fine health districts?

buffers_with_intersection <- geo_buffers_50km %>%
  filter(n_app_hs > 0) %>%
  mutate(n_app_hs_within_50km = map_dbl(buffers_50km_app_hs_intersections, length)) %>%
  filter(n_app_hs_within_50km > 0) %>%
  st_transform(crs = 4326)

geo_hd_fine <- geo_hd %>%
  filter(name %in% buffers_with_intersection$name)

tm_shape(shp = geo_hd) +
  tm_polygons() +
  tm_shape(shp = geo_hd_fine) +
  tm_polygons(col = "green")


# 2.e Create an object with all the shortlisted health sites based on the strategy ----

geo1 <- geo_app_hs %>%
  mutate(id = row_number(), status = "appropriate") %>%
  select(id, status)

hd_fine_nearest_hs_intersections <- st_intersects(x = geo_hd_fine, y = geo_nearest_hs)

geo2 <- geo_nearest_hs %>%
  slice(-unlist(hd_fine_nearest_hs_intersections)) %>%
  mutate(id = row_number() + nrow(geo1), status = "needs upgrade") %>%
  select(id, status)

geo_target_hs <- bind_rows(x = geo1, y = geo2)


tm_shape(shp = geo_hd) +
  tm_polygons() +
  tm_shape(shp = geo1) +
  tm_dots(col = "blue") +
  tm_shape(shp = geo2) +
  tm_dots(col = "orange") 


# ------------------------------------------------------------------------------
# ---- OBJECTIVE 3: WHAT ARE THE HEALTH DISTRICTS WITH THE HIGHEST RISKS? 
# ------------------------------------------------------------------------------

# 3.a Calculate the population density in each health district ----

geo_hd <- geo_hd_raw %>%
  mutate(
    area = as.numeric(st_area(geometry) / 1e6),
    density = pop / area
  )


tm_shape(shp = geo_hd) +
  tm_polygons(col = "density") 

tm_shape(shp = geo_hd) +
  tm_borders() +
  tm_fill(col = "density", breaks = c(0, 50, 100, 500, 2000, 10000, 30000))


# 3.b Compute the "influence index" of each designated health sites ----

geo_target_hs_buffer_50km <- geo_target_hs %>%
  st_transform(crs = 6829) %>% # Just to be able to use meters to create the buffer
  st_buffer(dist = 50000) %>% # 50km buffer
  st_transform(crs = 4326) %>%
  mutate(buffer_area = as.numeric(st_area(geometry) / 1e6))

influence_index <- 
  st_intersection(x = geo_hd, y = geo_target_hs_buffer_50km) %>%
  mutate(
    influence_index = density * area
  ) %>%
  group_by(id) %>%
  summarize(influence_index = sum(influence_index)) %>%
  arrange(-influence_index) %>%
  mutate(rank = row_number())

geo_target_hs2 <- geo_target_hs %>%
  left_join(influence_index %>% st_drop_geometry(), by = "id") %>%
  arrange(rank)


# 3.c Top 20 target health sites ----

tm_shape(shp = geo_hd) +
  tm_borders() +
  tm_fill(col = "density", breaks = c(0, 50, 100, 500, 2000, 10000, 30000)) +
  tm_shape(shp = influence_index %>% slice(1:20)) +
  tm_fill(col = "darkgreen", alpha = 0.5)



# 3.d Top 100 target health sites ----

tm_shape(shp = geo_hd) +
  tm_borders() +
  tm_fill(col = "density", breaks = c(0, 50, 100, 500, 2000, 10000, 30000)) +
  tm_shape(shp = influence_index %>% slice(1:100)) +
  tm_fill(col = "darkgreen", alpha = 0.5)


# 3.e Top 200 target health sites ----

tm_shape(shp = geo_hd) +
  tm_borders() +
  tm_fill(col = "density", breaks = c(0, 50, 100, 500, 2000, 10000, 30000)) +
  tm_shape(shp = influence_index %>% slice(1:200)) +
  tm_fill(col = "darkgreen", alpha = 0.5)
