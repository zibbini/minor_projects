validate <- function(points, data, column, threshold, output) {
  
  #' Validate the accuracy of rescale.mean()
  #' 
  #' @description For a given threshold, test how well the predictions from rescale.mean() match the locations of 
  #' the original point data. Threshold can be set from 0 - 1 depending on the desired distance from the mean, 
  #' with 1 selecting all the data and 0 selecting none of the data. 
  #' 
  #' @param points Point data used in rescale.mean(). Must be supplied as a two column dataframe. 
  #' @param data Rescaled layer produced from rescale.mean(). 
  #' @param column character. Column in your rescaled layer containing the proxy index. 
  #' @param threshold Desired distance from the mean.  
  #' @param output character. Output to return. Options are "polygons" and "score". 
  #' 
  #' @usage validate(points, data, column, threshold, output)
  #' 
  #' @return If output = "polygons", a SpatialPolygonsDataFrame object is returned. It will cover the extent of the rescaled
  #' layer that falls between the threshold value and 1, i.e. the mean. If output = "score", the percentage of points that 
  #' fall within the threshold chosen is returned. 
  #' 
  #' @details Note that because this method of validation does not rely on an additional set of point data, there is 
  #' a degree of bias introduced in the output. 

  ifelse(threshold == 0, 
         val <- 0, 
         val <- 1 - threshold)
  
  threshold_raster <- raster::rasterFromXYZ(
    subset(data, data[[column]] >= val, select = c("x", "y", "z"))
  )
  
  at_threshold <- na.omit(
    cbind(
      raster::extract(threshold_raster, points, df = TRUE), 
      points)
  )
  
  if (output == "polygons") {
    return(
      ggplot2::fortify(
        raster::rasterToPolygons(threshold_raster)
        )
      )
  } else if (output == "score") {
    return(
      (length(at_threshold[,1])/length(points[,1]))*100
    )
  }
}

docstring::docstring(validate)
