# Load required libraries
library(cowplot)
library(ggplot2)
library(here)
library(sf)

# set loc
here::i_am("code/ak_map.R")
options(max.print=2000)

# Load Alaska shapefile
alaska <- st_read(here("data", "Alaska_1%3A250%2C000.shp"))

# Define the bounding box for Southeast Alaska
bbox_seak <- st_bbox(c(xmin = 750000, ymin = 700000, xmax = 1500000, ymax = 1300000), crs = st_crs(alaska))
bbox_ak <- st_bbox(c(xmin = -800000, ymin = 700000, xmax = 1500000, ymax = 3000000), crs = st_crs(alaska))
seak_box <- st_as_sfc(st_bbox(bbox_seak))
sitka <- data.frame(long = 1110000, lat = 940000,  city = "Sitka")
juneau <- data.frame(long = -134.433304, lat = 58.305801,  city = "Juneau")
ketchikan <- data.frame(long = -131.647507, lat = 55.341808,  city = "Ketchikan")

# Clip Alaska to Southeast Alaska
seak <- st_crop(alaska, bbox_seak)
alaska <- st_crop(alaska, bbox_ak)

# Create the main map of Southeast Alaska
main_map <- ggplot() +
  geom_sf(data = seak, color = "black", fill = "green4") +
  geom_point(data = sitka,  aes(x = long, y = lat), color = "red", size = 2) +
  geom_text(data = sitka, 
            aes(x = long, y = lat, label = city),
            color = "black", size = 5, nudge_x = -30000, nudge_y = -40000) +
  theme_void() +
  theme(panel.background = element_rect(fill = "lightcyan"))
main_map

# Create the inset map of the entire Alaska
inset_map <- ggplot() +
  geom_sf(data = alaska, fill = "darkgrey", color = "transparent") +
  geom_sf(data = seak_box, color = "black", fill = "transparent", size = 0.25) +
  theme_void() +
  theme(panel.background = element_rect(fill = "white"))
inset_map

# Combine both maps
combined_map <- ggdraw() +
  draw_plot(inset_map) +
  draw_plot(main_map, x = 0.275, y = 0.35, width = 0.65, height = 0.6)
combined_map

# Save the map
ggsave(here("output", "figures", "southeast_alaska_with_inset.png"), combined_map)
