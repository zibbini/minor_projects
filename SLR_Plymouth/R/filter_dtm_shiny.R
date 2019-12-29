library(raster)
library(dplyr)
library(rgdal)
library(parallel)
library(sp)

filter.raster <- function(file, inpath, outpath) {
  
  data <- readRDS(file = paste0(inpath, file))
  slr  <- data %>% filter(col == "Land lost to SLR")

  slr$col  <- 1
  raslayer <- rasterFromXYZ(slr)
  raslayer <- raster::aggregate(raslayer, fact = 1.5)
  writeRaster(raslayer, filename = paste0(outpath, gsub(".rds", "", file), ".tif"))
}

ncores <- detectCores() 

mclapply(list.files(path = "/path/to/rcp2.6/data/", pattern = ".rds"), 
         filter.raster, 
         inpath = "/path/to/rcp2.6/data/",
         outpath = "/path/to/rcp2.6/",
         mc.cores = ncores - 1)

mclapply(list.files(path = "/path/to/rcp4.5/data/", pattern = ".rds"), 
         filter.raster, 
         inpath = "/path/to/rcp4.5/data/",
         outpath = "/path/to/rcp4.5/",
         mc.cores = ncores - 1)

mclapply(list.files(path = "/path/torcp8.5/data/", pattern = ".rds"), 
         filter.raster, 
         inpath = "/path/to/data/",
         outpath = "/path/to/rcp8.5/",
         mc.cores = ncores - 1)
