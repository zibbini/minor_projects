library(parallel)
library(ggplot2)
library(rgdal)
source("/path/to/github /minor_projects/SLR_Plymouth/R/plot.save.R")

#------------------------------------- Data prep ---------------------------------------

roads <- readOGR(
  dsn = "/path/to/DTM_plym/",
  layer = "roads"
)
roads <- fortify(roads)

#--------------------------------- Plotting functions -----------------------------------

make.plot <- function(inpath, file, outpath) {
  
  data     <- readRDS(file = paste0(inpath, file))

  p <- ggplot() +
    
    geom_raster(data = data, aes(x = data[,1], y = data[,2], fill = data[,3])) +
    geom_path(data = roads, aes(x = long, y = lat, group = group, linetype = "Roads"),
              colour = "grey45") +
    
    labs(title = gsub(pattern = ".rds", replacement = "", file),
         caption = "Projection data sourced from Met Office UK. DEM and road data sourced from Environment Agency UK.") +
    scale_fill_manual(name = NULL, 
                      values = c("Current sea extent"="#299CCB",
                                 "Land lost to SLR"="#E72A21",
                                 "Unaffected land"="#DFD89C")) +
    xlim(240413.1, 253206.3) +
    ylim(50815.85, 58330.64) +
    theme_void() +
    theme(plot.title = element_text(size = 15, hjust = 0.05, vjust = 0),
          legend.position = "bottom") +
    guides(fill = guide_legend(title.theme = element_text(size = 12), order = 1),
           linetype = guide_legend(title = NULL))
  
  plot.save(plot = p, 
            width = 875, 
            height = 656, 
            path = outpath,
            filename = paste0(gsub(pattern = ".rds", replacement = "", file), ".png"))
  
}

#------------------------------- Render plots (parallel) --------------------------------

ncores <- detectCores()

mclapply(list.files(path = "/path/to/rcp2.6/data/", pattern = ".rds"),
         make.plot, 
         inpath = "/path/to/rcp2.6/data/",
         outpath = "/path/to/rcp2.6/visualisations/", 
         mc.cores = ncores - 1)

mclapply(list.files(path = "/path/to/rcp4.5/data/", pattern = ".rds"),
         make.plot, 
         inpath = "/path/to/rcp4.5/data/",
         outpath = "/path/to/rcp4.5/visualisations/", 
         mc.cores = ncores - 1)

mclapply(list.files(path = "/path/to/rcp8.5/data/", pattern = ".rds"),
         make.plot, 
         inpath = "/path/to/rcp8.5/data/",
         outpath = "/path/to/rcp8.5/visualisations/", 
         mc.cores = ncores - 1)



