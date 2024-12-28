#######################
#Tom Leven
#Thesis location map
# 20.Nov.2024
#######################
#libraries
library(ggplot2) #visualization
library(dplyr) #data manipulation
library(terra) #raster operations
library(tidyterra) #plot terra objects
library(sf) #spatial data operations
library(tidygeocoder) #geocode addresses
library(rnaturalearth) #for world shapefile
library(tibble) #create dfs
library(tidyr) #reshaping data
library(marmap) #bathymetry
library(showtext) #for fonts
library(ggnewscale) 

#load font for later
font_add_google("Lato", family = "lato")
showtext_auto()
showtext_opts(dpi = 600)

#LOCATIONS----
#create a dataframe with thesis locations
some_addresses <- tibble::tribble(
  ~name,                  ~addr,
  "Ghent",          "Ghent, Belgium",
  "Ostende",        "Ostende, Belgium",
  "Faro",           "Faro, Portugal",
  "Brest",          "Brest, France",
  "Roscoff",        "Roscoff, France",
  "Bergen",         "Bergen, Norway",
  "Lerici",         "Lerici, Italy",
  "Punta Arenas",   "Punta Arenas, Chile",
  "Napoli",         "Napoli, Italy",
  "Roskilde",       "Roskilde, Denmark",
  "La Paz",         "La Paz, Mexico",
  "Cape Verde",     "Cape Verde",
  "Southampton",    "Southampton, England",
  "London",         "London, England",
  "Sicily",         "Sicily, Italy",
  "Madrid",         "Madrid, Spain",
  "Palma de Mallorca", "Palma de Mallorca, Spain",
  "Sidney",         "Sidney, Australia",
  "Valencia",       "Valencia, Spain",
  "Bari",           "Bari, Italy",
  "Boston",         "Boston, USA",
  "Den Helder",     "Den Helder, Netherlands",
  "Florianopolis",  "Florianopolis, Brazil",
  "Ulvik",          "Ulvik, Norway",
  "Faial",          "Faial, Azores",
  "Funchal",        "Funchal, Madeira",
  "Arendal",        "Arendal, Norway",
  "Paris",          "Paris, France",
  "Tenerife",       "Tenerife, Spain",
  "St. Andrews",    "St. Andrews, Scotland",
  "Los Angeles",    "Los Angeles, USA",
  "Galway",         "Galway, Ireland",
  "Vigo",           "Vigo, Spain",
  "Bremen",         "Bremen, Germany",
  "Tromso",         "Tromso, Norway",
  "Ancona",         "Ancona, Italy",
  "Oviedo",         "Oviedo, Spain",
  "Moorea",         "Moorea, French-Polynesia",
  "Hobart",         "Hobart, Australia",
  "Tübingen",       "Tübingen, Germany",
  "Ushuaia",        "Ushuaia, Argentina",
  "Crete",          "Crete, Greece",
  "Lisbon",         "Lisbon, Portugal",
  "Gothenburg",     "Gothenburg, Sweden",
  "Gladstone",      "Gladstone, Australia"
)

#geocode the addresses
lat_longs <- some_addresses %>%
  geocode(addr, method = 'osm', lat = latitude , long = longitude)

#convert to sf
lat_longs_sf <- lat_longs %>% 
  st_as_sf(coords = c("longitude", "latitude"),crs = 4326) %>% 
  st_transform(crs = "+proj=moll")

#get world outline
world <- ne_countries(scale = "medium")

#ROUTES FROM POINTS----
#create routes df, everything starts from Ghent
routes <- data.frame("source" = rep("Ghent", nrow(lat_longs)),
                     "destination" = lat_longs$name)[-1,]

#create id column to routes
routes_id <- rowid_to_column(routes, var = "id")

#prepare for join
routes_long <- routes_id %>% 
  gather(key = "type", value = "name", source, destination)

#add coordinate values
routes_long_geo <- left_join(routes_long, lat_longs, by = "name")

#convert coordinate data to sf object
routes_long_sf <- st_as_sf(routes_long_geo,
                           coords = c("longitude", "latitude"),
                           crs = 4326)

#convert point geometry to multipoint, then linestring
routes_lines <- routes_long_sf %>% 
  group_by(id) %>% 
  summarise(do_union = FALSE) %>% 
  st_cast("LINESTRING")

#join sf object with attributes data
routes_lines <- left_join(routes_lines, routes_id, by = "id")

#convert rhumb lines to great circles
routes_sf_tidy <- routes_lines %>% 
  st_segmentize(units::set_units(20, km))

#compare number of points in routes_lines and routes_sf_tidy
nrow(st_coordinates(routes_lines))
nrow(st_coordinates(routes_sf_tidy))

#GLOBAL PLOT----
#bounding box
bbox <- st_bbox(routes_sf_tidy)

#plot in WGS84
ggplot()+
  geom_sf(data = world) + 
  geom_sf(data = routes_sf_tidy, alpha = 0.5, color = "red") +
  coord_sf(xlim = c(bbox[1], bbox[3]), ylim = c(bbox[2], bbox[4]))

#try different projection (Mollweide)
routes_projected <- st_transform(routes_sf_tidy, crs = "+proj=moll")
world_proj <- st_transform(world, crs = "+proj=moll")

#new bounding box
bbox_proj <- st_bbox(routes_projected)

#plot lines and points in Mollweide
ggplot() +  
  geom_sf(data = world_proj, fill = "wheat") + 
  geom_sf(data = routes_projected, alpha = 0.5,color = "red") + 
  geom_sf(data = lat_longs_sf, color = "red", size = 0.7) +
  coord_sf(xlim = c(bbox_proj[1], bbox_proj[3]), ylim = c(bbox_proj[2], bbox_proj[4])) +  # Use a spherical projection like Mollweide
  theme_minimal() +
  theme_minimal() +
  theme(text = element_text(family = "lato", color = "#22211d"),
        axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "none",
        panel.grid.major = element_line(color = "white", linewidth = 0.2),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size=16, color="grey20", hjust=1, vjust=-5),
        plot.caption = element_text(size=10, color="grey70", hjust=0, vjust=4),
        plot.margin = unit(c(t=0, r=.2, b=0, l=0),"lines"), #added these narrower margins to enlarge maps
        plot.background = element_rect(fill = "white", color = NA), 
        panel.background = element_rect(fill = "lightblue"),
        panel.border = element_blank()) +
  labs(x = "", 
       y = NULL, 
       title = "IMBRSea Cohort 2023 Thesis Locations", 
       subtitle = "", 
       caption = "©2024 Tom Leven (https://github.com/tom-lvn)")

#save plot
ggsave(filename = "location_map_plain.png", width = 11, height = 6, dpi = 600,
       device = "png")

#WITH TOPOGRAPHY----
#get topography
bathy <- getNOAA.bathy(
  lon1 = -180,
  lon2 = 180,
  lat1 = -80,
  lat2 = 80,
  resolution = 6)

#create xyz object
#bathymetry
bat_xyz <- as.xyz(bathy) %>% 
  rename(Longitude=V1,Latitude=V2,Depth=V3) %>% 
  filter(Depth <= 0)
#topography
topo_xyz <- as.xyz(bathy) %>% 
  rename(Longitude=V1,Latitude=V2,Depth=V3) %>% 
  filter(Depth > 0)

#GLOBAL TOPOGRAPHY PLOT----
#convert to terra spatraster
bathy_terra <- as_spatraster(bat_xyz, xycols = c(1:2), crs=4326) %>%  
  project("+proj=moll")
topo_terra <- as_spatraster(topo_xyz, xycols = c(1:2), crs=4326) %>%  
  project("+proj=moll")

#create basemap with topography
basemap <- ggplot() + 
  geom_spatraster(data= bathy_terra,aes(fill=Depth), maxcell = 22000000) +
  scale_fill_hypso_c(palette = "colombia_bathy", na.value = NA) +
  new_scale_fill() +
  geom_spatraster(data = topo_terra, aes(fill = Depth), maxcell = 22000000) +
  scale_fill_hypso_c(palette = "colombia_hypso", na.value = NA) +
  theme_minimal() + 
  theme(text = element_text(family = "lato", color = "#22211d"),
        axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "none",
        panel.grid.major = element_line(color = "white", linewidth = 0.2),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size=16, color="grey20", hjust=1, vjust=-5),
        plot.caption = element_text(size=10, color="grey70", hjust=.2, vjust=4),
        plot.margin = unit(c(t=0, r=.2, b=0, l=0),"lines"), #added these narrower margins to enlarge maps
        plot.background = element_rect(fill = "white", color = NA), 
        panel.background = element_rect(fill = "white", color = NA),
        panel.border = element_blank()) +
  labs(x = "", 
       y = NULL, 
       title = "IMBRSea Cohort 2023 Thesis Locations", 
       subtitle = "", 
       caption = "©2024 Tom Leven (https://github.com/tom-lvn)")


#add lines and points to basemap
basemap + 
  geom_sf(data = lat_longs_sf, color = "red", size = 0.7) +
  geom_sf(data = routes_projected, alpha = 0.5,color = "red") + 
  coord_sf(xlim = c(bbox_proj[1], bbox_proj[3]), ylim = c(bbox_proj[2], bbox_proj[4]))

#save active plot
ggsave(filename = "location_map.png", width = 11, height = 6, dpi = 600,
       device = "png")

#ONLY EUROPE----
#zoom in on Europe
#bathy_europe <- getNOAA.bathy(
#  lon1 = -30,
#  lon2 = 32,
#  lat1 = 32,
#  lat2 = 75,
#  resolution = 1)
#download didn't work here

#downloaded straight from GEBCO, boundary box used is in the file name
#link: https://download.gebco.net
bathy_europe <- readGEBCO.bathy("../GEBCO_18_Dec_2024_6e7b1e66b34b/gebco_2024_n75.5634_s22.4155_w-39.151_e47.0623.nc",
                                resolution = 3)

#create xyz object
#bathymetry
bat_europe_xyz <- as.xyz(bathy_europe) %>% 
  rename(Longitude=V1,Latitude=V2,Depth=V3) %>% 
  filter(Depth <= 0)
#topography
topo_europe_xyz <- as.xyz(bathy_europe) %>% 
  rename(Longitude=V1,Latitude=V2,Depth=V3) %>% 
  filter(Depth > 0)

#convert to terra spatraster
bathy_europe_terra <- as_spatraster(bat_europe_xyz, xycols = c(1:2), crs=4326)
topo_europe_terra <- as_spatraster(topo_europe_xyz, xycols = c(1:2), crs=4326)

#CROP POINTS IN EUROPE AND CREATE ROUTES----
#define bounding box for Europe
bbox_eu <- st_bbox(c(xmin = -27, xmax = 35, ymin = 33, ymax = 72), crs = st_crs(4326)) %>%
  st_as_sfc()

#transform bounding box to the same CRS as `lat_longs_sf`
bbox_eu_mod <- st_transform(bbox_eu, crs = st_crs(lat_longs_sf))

#keep only points within the bounding box
lat_longs_crop <- st_intersection(lat_longs_sf, bbox_eu_mod)

#convert cropped points back to a usable dataframe with lon/lat
lat_longs_df <- lat_longs_crop %>%
  mutate(lon = st_coordinates(.)[, 1],
         lat = st_coordinates(.)[, 2]) %>%
  st_drop_geometry()

#create routes for points in Europe starting from Ghent
routes_eu <- data.frame("source" = rep("Ghent", nrow(lat_longs_crop)),
                        "destination" = lat_longs_crop$name)[-1, ]

#add ID column to routes
routes_eu_id <- rowid_to_column(routes_eu, var = "id")

#prepare routes for conversion to sf objects
routes_long_eu <- routes_eu_id %>%
  gather(key = "type", value = "name", source, destination)

#join with lat/lon data for the corresponding names
routes_long_geo_eu <- left_join(routes_long_eu, lat_longs_df, by = "name")

#convert to sf object
routes_long_sf_eu <- st_as_sf(routes_long_geo_eu,
                              coords = c("lon", "lat"),
                              crs = st_crs(lat_longs_sf))

#convert to linestring geometry
routes_lines_eu <- routes_long_sf_eu %>%
  group_by(id) %>%
  summarise(do_union = FALSE) %>%
  st_cast("LINESTRING")

#join routes with ID data for attributes
routes_lines_eu <- left_join(routes_lines_eu, routes_eu_id, by = "id")

# Convert rhumb lines to great circles for smoother routes
routes_sf_tidy_eu <- routes_lines_eu %>%
  st_segmentize(units::set_units(20, km))

#PLOT EUROPE ONLY----
#create basemap with topography
basemap_eu <- ggplot() + 
  geom_spatraster(data= bathy_europe_terra,aes(fill=Depth), maxcell = 15000000) +
  scale_fill_hypso_c(palette = "colombia_bathy", na.value = NA) +
  new_scale_fill() +
  geom_spatraster(data = topo_europe_terra, aes(fill = Depth), maxcell = 15000000) +
  scale_fill_hypso_c(palette = "colombia_hypso", na.value = NA) +
  theme_minimal() + 
  theme(text = element_text(family = "lato", color = "#22211d"),
        axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        legend.position = "none",
        panel.grid.major = element_line(color = "white", linewidth = 0.2),
        panel.grid.minor = element_blank(),
        plot.title = element_text(size=16, color="grey20", hjust=1, vjust=-4),
        plot.caption = element_text(size=10, color="grey70", hjust=.05, vjust=4),
        plot.margin = unit(c(t=0, r=.2, b=0, l=0),"lines"), #added these narrower margins to enlarge maps
        plot.background = element_rect(fill = "white", color = NA), 
        panel.background = element_rect(fill = "white", color = NA),
        panel.border = element_blank()) +
  labs(x = "", 
       y = NULL, 
       title = "IMBRSea Cohort 2023 \nThesis Locations in Continental Europe", 
       subtitle = "", 
       caption = "©2024 Tom Leven (https://github.com/tom-lvn)")

#add lines and points to basemap
basemap_eu + 
  geom_sf(data = lat_longs_crop[-1,], color = "red") +
  geom_sf(data = routes_sf_tidy_eu, alpha = 0.5,color = "red") +
  coord_sf(crs = st_crs(3035),
           ylim = c(1450000, 5200000),
           xlim = c(2600000, 5656000))

#save graphic window
ggsave(filename = "location_map_europe.png", width = 6, height = 8, dpi = 600,
       device = "png")
