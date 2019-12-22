rescale.mean <- function(points, layer, export = FALSE, filename) {
  
  #' Rescale raster to mean of presence points
  #' 
  #' @description This function extracts values from a raster object at the location of chosen point data. 
  #' A proxy index is then produced based on distance to the mean of the extracted values. 
  #' 
  #' @usage rescale.mean(points, layer, export = FALSE)
  #' 
  #' @param points Point data to extract raster values with. Point data must be supplied as a two column dataframe.
  #' @param layer Raster object to rescale based on point data.
  #' @param export logical. If TRUE a rescaled layer will be written to disk in .rds format. 
  #' Can be useful if you are applying this function to multiple layers and don't want to surpass your 
  #' memory limit. Defualt is FALSE. 
  #' @param filename Name of file to write to disk if export = TRUE.
  #' 
  #' @return If export is FALSE, a rescaled layer in dataframe format is returned. The x, y and z coordinates of
  #' the original raster will be returned as well as the proxy index. 
  #' 
  #' @details The proxy index has a range of 0 - 1, where 0 is furthest from the mean and 1 is a value 
  #' identical to the mean. Note that because the index is derived from the mean, the distribution of your point 
  #' data across the z component of the raster layer should ideally be normal. A non-normal distribution 
  #' may not produce a useful result. Nonetheless, you can test the accuracy of a proxy index using validate().
  
  data  <- data.frame(raster::rasterToPoints(layer))
  
  at_points <- cbind(raster::extract(layer, points, df = TRUE), points)
  
  MEAN <- mean(at_points[,2], na.rm = TRUE)
  
  # Mean transform before scale transform to yield correct values
  data$scaled <- ifelse(data[,3] > MEAN,
                        (MEAN - (data[,3] - MEAN))/MEAN,
                        data[,3]/MEAN)
  
  data$scaled <- scale(data$scaled, 
                       center = min(data$scaled),
                       scale = max(data$scaled) - min(data$scaled))
  
  if (export == TRUE) {
    saveRDS(data, file = filename)
  } else {
    return(data)
  }
}

docstring::docstring(rescale.mean)
