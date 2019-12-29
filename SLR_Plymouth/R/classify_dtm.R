library(readr)
library(parallel)
library(foreach)
library(doParallel)

#------------------------------------- Data prep ------------------------------------------

dem           <- read_csv(file = "path/to/filtered2.csv")
colnames(dem) <- c("x","y","z")

projdir       <- "path/to/projections/"
projpaths     <- c(paste0(projdir, "rcp2.6.csv"), paste0(projdir, "rcp4.5.csv"), paste0(projdir, "rcp8.5.csv"))
files         <- lapply(projpaths, read_csv)

files <- lapply(files, function(x){ 
  x$year <- 2007:2299 
  return(x) 
  }
)

# Required as lidar data has been collected in 2019
files <- lapply(files, function(x){
  x <- subset(x, year >= 2019)
  return(x)
})

rcp2.6 <- files[[1]]
rcp4.5 <- files[[2]]
rcp8.5 <- files[[3]]

#----------------------------------- Backend setup -----------------------------------------

ncores <- detectCores()
cl     <- makeCluster(ncores - 1)
registerDoParallel(cl)

#-------------------------------- Parallel functions ---------------------------------------

make.vals <- function(vals, data, path) {
  
  foreach (i = 1:length(vals[,1])) %dopar% {
      col <- ifelse(data$z > 2,
                    ifelse(data$z > vals[,1][i] + 2, 
                           "Unaffected land", 
                           "Land lost to SLR"), 
                    "Current sea extent")
      saveRDS(data.frame(data$x, data$y, col),
              file = paste0(path, as.character(vals[,2][i]), ".rds"), 
              compress = TRUE)
    }
}

make.vals(vals = data.frame(rcp2.6$`50`, rcp2.6$year), 
          data = dem, 
          path = "path/to/rcp2.6/data/")
make.vals(vals = data.frame(rcp4.5$`50`, rcp4.5$year), 
          data = dem, 
          path = "path/to/rcp4.5/data/")
make.vals(vals = data.frame(rcp8.5$`50`, rcp8.5$year), 
          data = dem, 
          path = "path/to/rcp8.5/data/")

stopCluster(cl) 




