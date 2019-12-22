# Description:
# Extracts values from a raster object at locations of point data, and plots a frequency 
# distribution from the result. 

plot.dist <- function(layer, points, xlab) {
  
  require(ggplot2)
  
  at_points <- cbind(raster::extract(layer, points, df = TRUE), points)
  
  ggplot(data = at_points, aes(x = at_points[,2])) +
    geom_density(aes(y = ..count.., fill = "red")) +
    theme_bw() + 
    theme(legend.position = "none") +
    labs(x = xlab, y = "Frequency") 
}
