library(crosstalk)
library(leaflet)
library(htmltools)
library(proj4)
library(sp)
library(plyr)
library(lubridate)

setwd("/path/to/F. exsecta analysis/Data")
#---------------------------------- Data prep -----------------------------------------

df <- read.csv("data-all.csv", header = TRUE)

df           <- subset(df, select = c(No., X.1, E, N, X, Y, diameter, activity, cmpt, Brood.recorded.2018))
colnames(df) <- c("no", "satellite", "easting", "northing", "x", "y", "diameter", 
                  "activity", "compartment", "brood emergence")
df           <- na.omit(df)
df$diameter  <- as.numeric(df$diameter)
df           <- df[!duplicated(df[c("x","y")]),]
df_latlon    <- subset(df, select = c(x, y))

#Specifying projection parameters
wgs84 <- "+init=epsg:4326" #desired crs
proj  <- "+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.999601 +x_0=400000 +y_0=-100000 +ellps=airy +towgs84=446.448, 
        -125.157,542.060,0.1502,0.2470,0.8421,-20.4894 +units=m +no_defs" #crs of UTM coords

#Transform UTM coords to lat/lon
points <- data.frame(
  spTransform(
    SpatialPoints(df_latlon, proj4string = CRS(proj)),
    CRS(wgs84)
  )
)

df$longitude <- points$x
df$latitude  <- points$y

#Format brood emergence to single date
colnames(df)[10] <- "brood_emerge"
df$brood_emerge  <- gsub("\\,.*", "", df$brood_emerge)
df$year          <- "2018"
df$brood_emerge  <- paste(df$brood_emerge, df$year, sep = "/")
df$brood_emerge  <- ifelse(df$brood_emerge == "/2018", "", df$brood_emerge)
df$brood_emerge  <- dmy(df$brood_emerge)

#Remove unwanted cols
df$year <- NULL
df$x    <- NULL
df$y    <- NULL

#------------------------------- Map formatting ----------------------------------------
bins <- c(0,5,10,15,20,25,30,35)
pal <- colorBin("plasma", domain = df$diameter, bins = bins)

labs <-  paste(sep = "<br/>",
               paste("Number:", df$no),
               paste("Activity:", df$activity),
               paste("Diameter:", df$diameter, "cm"),
               paste("Brood emergence:", df$brood_emerge),
               paste("Coordinates (E:N):", df$easting, df$northing)
)

#----------------------------- Summary info-board ---------------------------------------

dfmap <- SharedData$new(df)

bscols(widths = c(3,9),
       
       #Inputs =================
       list(
         filter_select("cmpt", "Reserve compartment:", dfmap, ~compartment),
         filter_checkbox("activity", "Nest activity:", dfmap, ~ifelse(activity == "ACTIVE", "Active", "Inactive"),
                         inline = TRUE),
         filter_slider("cm", "Nest diameter (cm):", dfmap, ~diameter, width = "100%")
       ),
       
       #Outputs ================
       list(
         leaflet(dfmap) %>% 
           
           addTiles(group = "Default") %>%
           addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
           
           addCircleMarkers(~longitude, ~latitude, popup = ~labs, color = ~pal(df$diameter), 
                            fillColor = pal(df$diameter)) %>%
           
           addLayersControl(
             baseGroups = c("Defualt", "Satellite"),
             options = layersControlOptions(collapsed = FALSE)) %>%
           
           addLegend(pal = pal, values = ~diameter, opacity = 1, title = "Nest diameter (cm):",
                     position = "bottomright")
       )
)
