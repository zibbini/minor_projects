library(raster)
library(proj4)
library(rgdal)
library(parallel)

# Prep point data
values       <- read.csv(file = "~/Desktop/R-projects/GIS-work/Project_2/Data/temp_coords.csv", h = T)
sp           <- subset(values, select = c(X_COORD, Y_COORD))
colnames(sp) <- c("x", "y")
sp$x         <- as.numeric(sp$x)
sp$y         <- as.numeric(sp$y)
proj4string  <- '+init=epsg:27700 +proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +units=m +no_defs'
pj           <- proj4::project(sp, proj4string, inverse = TRUE)
sp           <- data.frame(x=pj$x, y=pj$y)

scale.mean <- function(file, path) {
  
  layer <- raster::raster(paste0(path, file))
  data <- data.frame(rasterToPoints(layer))
  
  at_sites <- cbind(extract(layer, sp, df = TRUE), sp)
  
  MEAN <- mean(at_sites[,2], na.rm = TRUE)
  
  # Mean transform before scale transform to yield correct values
  data$scaled <- ifelse(data[,3] > MEAN,
                        (MEAN - (data[,3] - MEAN))/MEAN,
                        data[,3]/MEAN)
  
  data$scaled <- scale(data$scaled, 
                  center = min(data$scaled),
                  scale = max(data$scaled) - min(data$scaled))
  
  saveRDS(data, file = paste0(path, "mean_scaled/", gsub(".tif", ".rds", file)))
}

path = "/home/z/Desktop/worldclim/"

mclapply(list.files(path, ".tif"), 
         scale.mean,
         path = path, 
         mc.cores = detectCores() - 1)
