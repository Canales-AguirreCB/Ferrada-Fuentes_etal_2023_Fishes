#remotes::install_github("BigelowLab/rasf", upgrade = FALSE)

library(readr)
library(dplyr)
library(ggplot2)
library(sf)
library(ggspatial)
library(rasf)
data <- read_csv("data/data_mapa.txt")
data <- data %>% mutate(long2 = to360(Longitude))
shift_value_1 <- 0
shift_value_2 <- 360
#paises que se quieran...
sa_countries <- c("Chile", "Argentina", "New Zealand", "Peru", "Bolivia","Uruguay", "Paraguay","Brazil", "Venezuela", "Ecuador", "Colombia" )
#trick
map_world_df <- map_data('world', wrap = c(shift_value_1, shift_value_2)) %>%
  dplyr::filter(region %in% sa_countries)
# def propiedades de los poligonos
country_shapes <-  geom_polygon(data = map_world_df, 
                                aes(x = long, y = lat, group = group),
                                fill = "gray80",
                                color = "white",
                                size = 0.15)
data_sf <- st_as_sf(x = data, coords = c("Longitude", "Latitude"), crs = 4326)
#esconder capas de info comentando...por ejemplo, acá estan comentados los puntos muestreo
plot <- ggplot() +
  country_shapes +
  #nombre ejes
  labs(x = "Longitude", y = "Latitude") +
  geom_sf() +
  geom_point(data = data, alpha = 0.7, size = 2, aes(x = long2, y = Latitude)) +
  geom_text(data,mapping = aes(x = long2, y = Latitude, label = Location), check_overlap = TRUE,hjust = 0, nudge_x = 1, size = 4, vjust = 0) +
  #propiedades de la escala y rosa de los vientos
  annotation_scale(location = "tl") +
  annotation_north_arrow(location = "tl", which_north = "true", pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"), style = north_arrow_fancy_orienteering) +
  #limites de xlim e ylim
  coord_sf(crs = 4326, xlim = c(170, 290), ylim = c(0, -60)) +
  theme_bw()
plot

#Map plot save in ps
postscript("data/Fig1_map.eps",height=4,width=7)
plot
dev.off()
