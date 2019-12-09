library(ggplot2)
library(plotly)
library(lubridate)
library(data.table)
library(plyr)
library(gridExtra)
library(RColorBrewer)

setwd("/run/media/z/Z/R-projects/GIS-work/minor-projects/F. exsecta analysis/Data")

#------------------------------------------ Data prep -------------------------------------------

temp  <- read.csv("first_temp.csv", header = TRUE) #First monitoring period (Active nests ~ Soil controls) 
temp2 <- read.csv("second_temp.csv", header = TRUE) # Second monitoring period (Active nests ~ Abandoned nests) 

d_t        <- ldply(strsplit(as.character(temp2$Date...Time), split = " "))
temp2$Date <- d_t$V1
temp2$Time <- d_t$V2

date.format <- function(df){
  
  df$Date       <- as.Date(dmy(df$Date))
  
  ifelse(is.character(df$Time) == TRUE, 
         df$Time <- (as.numeric(factor(df$Time)))/2,
         df$Time <- (as.numeric(df$Time))/2
  )
  
  return(df)
}

temp  <- date.format(df = temp)
temp2 <- date.format(df = temp2)

temp2$Mean_active     <- rowMeans(temp2[c("T17","T12","T16","T19")])
temp2$Mean_inactive   <- rowMeans(temp2[c("T11","T14","T15","T13")])

#----------------------------------------- Heatmap plots -----------------------------------------

heatmap <- function(data, col, limits, subtitle, xlab, legpos) {
  
  col1 <- data[, col]
  col2 <- data[, limits]
  scale_min <- ifelse((min(col1) < min(col2)) == TRUE,
                      min(col1),
                      min(col2))
  scale_max <- ifelse((max(col1) > max(col2)) == TRUE,
                      max(col1),
                      max(col2))
  
  ggplot(data, aes(x = Date, y = Time, fill = col1)) +
    geom_tile(color = "grey80", size = 0.1) +
    scale_fill_gradientn(name = "Temperature (°C)   ", 
                         colors = rev(brewer.pal(n = 9, name = "Spectral")),
                         limits = c(scale_min, scale_max)) + 
    scale_y_continuous(breaks = c(1,5,10,15,20,24)) +
    theme_minimal() +
    labs(" ", subtitle = subtitle, y = "Time (hr)", x = xlab) +
    theme(legend.position = legpos,
          axis.text = element_text(size = 10),
          legend.title = element_text(size = 12),
          panel.grid = element_blank())
  
}

#Individual nest/soil control comparison ---------------------------------------------------------
heatmap1 <- heatmap(temp, "N7", "N7", "(a)", NULL, "none")
heatmap2 <- heatmap(temp, "O7", "N7", "(b)", "Date", "bottom")
grid.arrange(heatmap1, heatmap2, heights = c(1, 1.35))

#Mean nest/soil control comparison ---------------------------------------------------------------
heatmap3 <- heatmap(temp, "Meannest", "Meannest", "(a)", NULL, "none")
heatmap4 <- heatmap(temp, "Meansoil", "Meannest", "(b)", "Date", "bottom")
grid.arrange(heatmap3, heatmap4, heights = c(1, 1.35))

#Mean active/inactive nest comparison ------------------------------------------------------------
heatmap5 <- heatmap(temp2, "Mean_active", "Mean_active", "(a)", NULL, "none")
heatmap6 <- heatmap(temp2, "Mean_inactive", "Mean_active", "(b)", "Date", "bottom")
grid.arrange(heatmap5, heatmap6, heights = c(1, 1.35))
