library(dplyr)
library(parallel)
library(ggplot2)
source("/home/z/Desktop/github /minor_projects/SLR_Plymouth/R/plot.save.R")

#-------------------------------------- Data prep ------------------------------------------

quantify.landlost <- function(file, path) {
  
  data <- readRDS(file = paste0(path, file))
  slr  <- data %>% filter(col == "Land lost to SLR")
  num  <- (length(slr$col)*25)/1000000
  
  return(num)
}

ncores <- detectCores()

rcp2.6 <- mclapply(list.files(path = "~/Desktop/slr-files/rcp2.6/data/", pattern = ".rds"),
                   quantify.landlost, 
                   path = "~/Desktop/slr-files/rcp2.6/data/", 
                   mc.cores = ncores - 1
                   )
rcp4.5 <- mclapply(list.files(path = "~/Desktop/slr-files/rcp4.5/data/", pattern = ".rds"),
                   quantify.landlost, 
                   path = "~/Desktop/slr-files/rcp4.5/data/", 
                   mc.cores = ncores - 1
                   )

rcp8.5 <- mclapply(list.files(path = "~/Desktop/slr-files/rcp8.5/data/", pattern = ".rds"),
                   quantify.landlost, 
                   path = "~/Desktop/slr-files/rcp8.5/data/", 
                   mc.cores = ncores - 1
                   )

year <- 2019:2299

landlost <- data.frame(year, rcp2.6 = unlist(rcp2.6), rcp4.5 = unlist(rcp4.5), rcp8.5 = unlist(rcp8.5))

#---------------------------------------- Plot ---------------------------------------------

p <- ggplot(landlost) +
  geom_line(aes(x = year, y = rcp2.6, color = "2.6")) +
  geom_line(aes(x = year, y = rcp4.5, color = "4.5")) +
  geom_line(aes(x = year, y = rcp8.5, color = "8.5")) +
  labs(title = "Land flooded due to projected sea level rise in Plymouth (2019-2300)",
       x = "Date (Yr)",
       y = bquote("Mean area of land flooded"~(km^2))) +
  scale_color_manual(name = "RCP model:", 
                     values = c("2.6"="blue", "4.5"="purple", "8.5"="red")) +
  theme_bw() +
  theme(legend.position = "bottom")

plot.save(plot = p, width = 800, height = 600, filename = "landlost.png", 
          path = "/home/z/Desktop/github /minor_projects/SLR_Plymouth/visualisations/")
  