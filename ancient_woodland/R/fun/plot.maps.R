# Description:
# Theme and helper functions for plotting rescaled layers as maps. 

theme_maps <- function(...) {
  
  require(ggplot2)
  
  theme_void() +
    theme(
      legend.title = element_text(size = 12),
      legend.text = element_text(size = 9),
      legend.position = c(0.15, 0.5),
      plot.title = element_text(size = 14, hjust = 0.05, vjust = 0),
      plot.subtitle = element_text(size = 13, hjust = 0.05, vjust = 0)
    )
}

plot.maps <- function(data, fill, title, subtitle, caption) {
  
  require(ggplot2)
  require(ggsn)
  
  ggplot(data) +
    geom_raster(aes(x, y, fill = fill)) +
    scale_fill_viridis_c(begin = 0, end = 1.00, option = "B") +
    labs(title = title, 
         subtitle = subtitle,
         caption = caption) +
    guides(fill = guide_colorbar(title = "Deviation index")) +
    north(x.min = min(data$x), x.max = max(data$x), 
          y.min = min(data$y), y.max = max(data$y),
          location = "topright", symbol = 10, scale = 0.1) +
    scalebar(x.min = min(data$x), x.max = max(data$x), 
             y.min = min(data$y), y.max = max(data$y), 
             transform = T, model = "Airy", dist = 100, dist_unit = "km",
             box.fill = c("black", "white"), location = "bottomright", st.size = 3) +
    theme_maps()
}