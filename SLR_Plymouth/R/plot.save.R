plot.save <- function(plot, width, height, filename, path) {
  
  #' Save a ggplot object by resolution
  #' 
  #' @description This wrapper function allows you to specify the resolution of a plot to be saved, rather than
  #' dimensions in standard units (e.g. cm). 
  #' 
  #' @param plot The plot object to be saved.
  #' @param width Width in pixels. 
  #' @param height Height in pixels.
  #' @param filename Name of the file to create on disk.
  #' @param path Path to save file to. 
  #' 
  #' @usage plot.save(plot, width, height, filename, path)
  #' 
  #' @details Since this is a wrapper function, you can pass any grob object you normally would if you were using ggsave(). 
  #' 
  #' @examples
  #' p <- ggplot(mtcars, aes(mpg, wt)) + geom_point()
  #' 
  #' plot.save(plot = p, width = 800, height = 600, 
  #'           filename = "mtcars.png", path = ".")

  dpi   <- 100
  resw <- width/dpi
  resh <- height/dpi
  
  ggplot2::ggsave(filename = filename, dpi = dpi, width = resw, height = resh, units = 'in', plot = plot, path = path)
}

docstring::docstring(plot.save)
