## GUI for exploring sea level rise in Plymouth (2019-2300)

Interactive dashboard built with `leaflet` and `shiny`. Flooded areas are visible on an open street map.

Please note that the data used in the [live version](https://z-lab.shinyapps.io/slrvis/) of this dashboard has been resampled by a factor of 1.5 to meet the memory limits set by [shinyapps.io](https://www.shinyapps.io/). This introduces a degree of error in the projections and so the map renders should not be taken as reliable representations of potential sea level rise in Plymouth. 

If you would like to use the dashboard to explore non-resampled data, you can do so by using the dashboard locally in your own R session. After cloning this repository and unpacking the archives contained in `./Data/`, you can launch the dashboard using the following:

``` r
shiny::runApp("path/to/minor_projects/SLR_Plymouth/shiny/app.R")
```

Please note this dashboard is dependent on several packages: `shiny`, `leaflet`, `raster` and `rgdal`. If you do not have these packages installed, you can install them using the following:

``` r
install.packages(c("shiny","leaflet","raster",rgdal"))
```


