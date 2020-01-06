library(dplyr)
library(rgdal)
library(leaflet)
library(htmltools)

setwd("/path/to/Data/")

#---------------------------------- Data prep ---------------------------------------

shp <- readOGR(
  dsn = "/path/to/Data/",
  layer = "site_count_districts"
)

counts <- read.csv(file = "sites_per_district.csv", h = T)

#Subset and filter shapefile to England 
counts         <- counts[-c(317:382), ]
counts$lad19nm <- as.character(counts$lad19nm)
districts      <- counts$lad19nm
shp            <- shp[shp$lad19nm %in% districts, ]

#------------------------------ Formatting for map ----------------------------------

#Specifying legend scale and colour
bins <- c(0, 20, 50, 100, 200, 500, 1000, 2000, 3000)
pal  <- colorBin("viridis", domain = shp$NUMPOINTS, bins = bins)

#Label formatting
labs <- sprintf(
  "<strong>%s</strong><br/>%g Ancient Woodland sites",
  shp$lad19nm, shp$NUMPOINTS
) %>% lapply(htmltools::HTML)

#--------------------------------- Final plot ---------------------------------------

leaflet(shp) %>%

  addPolygons(
    fillColor = ~pal(NUMPOINTS),
    weight = 2,
    opacity = 1,
    color = " ",
    dashArray = "3",
    fillOpacity = 0.7,
    highlight = highlightOptions(
      weight = 2,
      color = "black",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE),
    label = labs,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto")) %>%
  
  addLegend(pal = pal, values = ~NUMPOINTS, opacity = 0.7, title = "No. of sites:",
            position = "bottomright")
