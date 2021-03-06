library(rgdal)
library(ggplot2)
library(proj4)
library(RColorBrewer)
library(ggsn)
source("path/to/fun/plot.save.R")

setwd("path/to/Data")

#---------------------------------- Data prep ---------------------------------------

df <- read.csv(file = "temp_coords.csv", h = T)
df <- subset(df, select = c("X_COORD", "Y_COORD"))

#Transform coordinate data to the same CRS as shp layer
proj4string <- '+init=epsg:27700 +proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +units=m +no_defs'
pj          <- proj4::project(df, proj4string, inverse = TRUE)
df_latlon   <- data.frame(lat=pj$y, long=pj$x)

shp <- readOGR(
  dsn = "path/to/Data",
  layer = "site_count_districts"
)

england_districts <- read.csv("city-district-names.csv",header=F)$V1
e_districts       <- as.character(england_districts)
shp               <- shp[shp@data$lad19nm %in% e_districts, ]
map.df            <- fortify(shp)

#------------------------------ Theme for static plots --------------------------------

theme_z_maps <- function(...) {
  theme_void() +
    theme(
      legend.background = element_blank(),
      
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 9, color = "grey10"),
      legend.position = c(0.175, 0.5),
      
      plot.title = element_text(size = 13, hjust = 0.05, vjust = 0)
    )
}

#--------------------------------- Static plots ---------------------------------------

# Hexbin plot -------------------------------------------------------------------------
p <- ggplot() +  
  
  geom_polygon(data = map.df, aes(x = long, y = lat, group = group, linetype = "City districts"),
               fill = "grey60", colour = "grey45") +  
  geom_hex(data = df_latlon, aes(x = long, y = lat), binwidth = c(0.09, 0.09)) +
  
  scale_fill_gradientn(
    colors = rev(brewer.pal(9, "Spectral")),
    values = c(0, exp(seq(-5, 0, length.out = 100))),
    name = "No. of ancient woodland sites") +
  coord_fixed() +
  
  north(df_latlon, location = "topright", symbol = 10, scale = 0.1) +
  scalebar(df_latlon, transform = T, model = "Airy", dist = 100, dist_unit = "km",
           box.fill = c("black", "white"), location = "bottomright", st.size = 3) +
  
  theme_z_maps() +
  labs(caption = "Data sourced from Natural England UK.") +
  guides(linetype = guide_legend(
    title.theme = element_text(
      size = 12),
    title = "Legend",
    order = 1)
    )

plot.save(plot = p, width = 740, height = 720, 
          filename = "hexbin.png",
          path = "path/to/visualisations")

# Kernel density and isocontours ----------------------------------------------------------
p1 <- ggplot(df_latlon) +
  
  geom_polygon(data = map.df, aes(x = long, y = lat, group = group, linetype = "City districts"),
               fill = "grey60", colour = "grey70") + 
  geom_density2d(aes(x = long, y = lat), h = 0.5, bins = 100) +
  stat_density2d(aes(x = long, y = lat, fill=..level..), alpha = 0.75, geom="polygon") +
  
  scale_fill_gradientn(
    colors = rev(brewer.pal(9, "YlGnBu")),
    values = c(0, exp(seq(-5, 0, length.out = 100))),
    name = "Density estimation") +
  
  north(df_latlon, location = "topright", symbol = 10, scale = 0.1) +
  scalebar(df_latlon, transform = T, model = "Airy", dist = 100, dist_unit = "km",
           box.fill = c("black", "white"), location = "bottomright", st.size = 3) +
  
  coord_fixed() +
  theme_z_maps() +
  xlim(-6, 2) + ylim(49.5, 56) +
  labs(caption = "Data sourced from Natural England UK.") +
  guides(linetype = guide_legend(
    title.theme = element_text(
      size = 12),
    title = "Legend",
    order = 1)
  )

plot.save(plot = p1, width = 740, height = 720, 
          filename = "kdensity.png",
          path = "path/to/visualisations")
