library(raster)
library(proj4)
library(rgdal)
library(parallel)
library(doParallel)
library(foreach)
source("~/Desktop/github /minor_projects/ancient_woodland/R/fun/rescale.R")
source("~/Desktop/github /minor_projects/ancient_woodland/R/fun/validate.R")
source("~/Desktop/github /minor_projects/ancient_woodland/R/fun/plot.dist.R")
source("~/Desktop/github /minor_projects/ancient_woodland/R/fun/plot.save.R")
source("~/Desktop/github /minor_projects/ancient_woodland/R/fun/plot.maps.R")

# ------------------------------------- Prep point data -------------------------------------
values       <- read.csv(file = "~/Desktop/R-projects/GIS-work/Project_2/Data/temp_coords.csv", h = T)
sp           <- subset(values, select = c(X_COORD, Y_COORD))
colnames(sp) <- c("x", "y")
sp$x         <- as.numeric(sp$x)
sp$y         <- as.numeric(sp$y)
proj4string  <- '+init=epsg:27700 +proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +units=m +no_defs'
pj           <- proj4::project(sp, proj4string, inverse = TRUE)
sp           <- data.frame(x=pj$x, y=pj$y)

# ------------------------------------- Rescale layers ---------------------------------------

layers <- list.files(path = "~/Desktop/worldclim", pattern = ".tif" )

for (i in layers) {
  
  data <- raster(i)
  
  rescale.mean(points = sp, 
               layer = data,
               export = TRUE, 
               filename = paste0("~/Desktop/worldclim/mean_scaled/", gsub(".tif", ".rds", i))
  )
}

# -------------------------------------- Plot maps -------------------------------------------

files <- list.files(path= "~/Desktop/worldclim/mean_scaled/", pattern = ".rds")
vars <- "BIO1 = Annual Mean Temperature
BIO2 = Mean Diurnal Range (Mean of monthly (max temp - min temp))
BIO3 = Isothermality (BIO2/BIO7) (* 100)
BIO4 = Temperature Seasonality (standard deviation *100)
BIO5 = Max Temperature of Warmest Month
BIO6 = Min Temperature of Coldest Month
BIO7 = Temperature Annual Range (BIO5-BIO6)
BIO10 = Mean Temperature of Warmest Quarter
BIO11 = Mean Temperature of Coldest Quarter
BIO12 = Annual Precipitation
BIO13 = Precipitation of Wettest Month
BIO14 = Precipitation of Driest Month
BIO15 = Precipitation Seasonality (Coefficient of Variation)
BIO16 = Precipitation of Wettest Quarter
BIO17 = Precipitation of Driest Quarter
BIO18 = Precipitation of Warmest Quarter
BIO19 = Precipitation of Coldest Quarter"
names  <- strsplit(vars, split = "\n")
names  <- lapply(names, function(x) substring(x, 8))
rds_with_labels <- data.frame(files, names)

ncores <- detectCores()
cl     <- makeCluster(ncores - 1)
registerDoParallel(cl)

# For all layers 
foreach (i = rds_with_labels[,1]) %dopar% {
  
  data <- readRDS(paste0("~/Desktop/worldclim/mean_scaled/", i))
  
  p <- plot.maps(data = data, fill = data$scaled, title = rds_with_labels[,2][i], subtitle = "", 
                 caption = "Bioclimatic data sourced from WorldClim (Fick and Hijmans, 2017).")
  
  plot.save(plot = p, width = 740, height = 720, 
            filename = gsub("-eng.rds", ".png", i), 
            path = "~/Desktop/worldclim/maps/")
}

stopCluster(cl) 

# For overall average 
scales <- lapply(files, function(x) {
  data <- readRDS(paste0("~/Desktop/worldclim/mean_scaled/", x))
  scale <- data$scaled
})

data <- rbind.data.frame(scales)

# Coordinates are the same across layers so only need one layer here
xy   <- readRDS(paste0("~/Desktop/worldclim/mean_scaled/","01-eng.rds"))
xy$z <- rowMeans(data)

xy$z <- scale(xy$z, 
              center = min(xy$z),
              scale = max(xy$z) - min(xy$z))

avg <- plot.maps(data = xy, fill = xy$z,
                 title = "Suitable climate for ancient woodland in England",
                 subtitle = "",
                 caption = "Derived from the average of 17 rescaled bioclimatic layers. 
                 Bioclimatic layers sourced from WorldClim (Fick and Hijmans, 2017).")

plot.save(plot = avg, width = 740, height = 720, 
          filename = "average.png", 
          path = "~/Desktop/worldclim/maps/")

# -------------------------------- Plot distribution plots ----------------------------------

tif_with_labels <- data.frame(layers, names)

ncores <- detectCores()
cl     <- makeCluster(ncores - 1)
registerDoParallel(cl)

foreach (i = tif_with_labels[,1]) %dopar% {
  
  layer <- raster::raster(paste0("~/Desktop/worldclim/", i))
  
  pd <- plot.dist(layer = layer, points = sp, xlab = tif_with_labels[,2][i])
  
  plot.save(plot = pd, width = 500, height = 500,
            filename = gsub("-eng.tif", ".png", i), 
            path = "~/Desktop/worldclim/dist_plots/")
}

stopCluster(cl)

# ---------------------------------- Validate predictions ----------------------------------

avg_score <- validate(points = sp, data = xy, column = "z", threshold = 0.1, output = "score")
avg_area  <- validate(points = sp, data = xy, column = "z", threshold = 0.1, output = "polygons")

england <- readOGR(
  dsn = "~/Desktop/worldclim/", 
  layer = "england"
)

backlayer <- fortify(england)

# Thresholded map
p_threshold <- ggplot() +
  geom_polygon(data = backlayer, aes(long, lat, group = group, fill = "Surrounding area")) +
  geom_polygon(data = avg_area, aes(long, lat, group = group, fill = "Suitable areas")) +
  labs(title = "Suitable climatic areas for ancient woodland:", 
       subtitle = "Thresholded at a mean distance of 0.1",
       caption = "Derived from the average of 17 rescaled bioclimatic layers. 
       Bioclimatic data sourced from WorldClim (Fick and Hijmans, 2017).") +
  scale_fill_manual(values = c("Surrounding area" = "grey60", "Suitable areas" = "darkred")) +
  guides(fill = guide_legend(title = "Legend")) +
  north(x.min = min(backlayer$long), x.max = max(backlayer$long), 
        y.min = min(backlayer$lat), y.max = max(backlayer$lat),
        location = "topright", symbol = 10, scale = 0.1) +
  scalebar(x.min = min(backlayer$long), x.max = max(backlayer$long), 
           y.min = min(backlayer$lat), y.max = max(backlayer$lat), 
           transform = T, model = "Airy", dist = 100, dist_unit = "km",
           box.fill = c("black", "white"), location = "bottomright", st.size = 3) +
  theme_maps()

plot.save(plot = p_threshold, width = 740, height = 720, 
          filename = "threshold.png", 
          path = "~/Desktop/worldclim/maps/")
