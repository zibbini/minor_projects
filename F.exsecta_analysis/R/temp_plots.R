library(lubridate)
library(ggplot2)
library(dplyr)
library(gganimate)
library(plotly)

setwd("/run/media/z/Z/R-projects/GIS-work/minor-projects/F. exsecta analysis/Data")

#----------------------------------------- Data prep ----------------------------------------------

temp  <- read.csv("first_temp.csv", header = TRUE) # First monitoring period (Active nests ~ Soil controls) 
temp2 <- read.csv("second_temp.csv", header = TRUE) # Second monitoring period (Active nests ~ Abandoned nests) 

temp$Time  <- (as.numeric(temp$Time))/2
d_t        <- ldply(strsplit(as.character(temp2$Date...Time), split = " "))
temp2$Time <- d_t$V2
temp2$Time <- (as.numeric(factor(temp2$Time)))/2

temp$meannest <- rowMeans(temp[c("N7","N8","N11","N14","N15","N18","N19","N20","N21")])
temp$meansoil <- rowMeans(temp[c("O7","O8","O11","O14","O15","O18","O19","O20","O21")])
temp$Meannest <- NULL #these two lines only included as I had previous data under the same feature name
temp$Meansoil <- NULL
temp2$mean_active     <- rowMeans(temp2[c("T17","T12","T16","T19")])
temp2$mean_inactive   <- rowMeans(temp2[c("T11","T14","T15","T13")])

#Create mean daily temperature regimes
means <- aggregate(.~Time, data = subset(temp, select = -c(Date)), mean)
means2 <- aggregate(.~Time, data = subset(temp2, select = -c(Date...Time)), mean)

#--------------------------------------- Theme for plots ------------------------------------------

theme_anim <- function(...){
  theme(
    
    text = element_text(color = "white"),
    plot.background = element_rect(fill = "#313234",
                                   color = "#313234"),
    panel.background = element_rect(fill = "#313234",
                                    color = "#313234"),
    panel.grid = element_blank(),
    
    legend.title = element_blank(),
    legend.text = element_text(size = 10),
    legend.background = element_blank(),
    legend.key = element_rect(fill = "transparent", 
                              color = "transparent"),
    
    axis.line = element_line(color = "white"),
    axis.ticks = element_blank(),
    axis.title = element_text(size = 12),
    axis.text = element_text(color = "white")
  )
}

#------------------------------------- Temperature plots ------------------------------------------
#For first observation period ---------------------------------------------------------------------
basicplot <- ggplot(data = temp) +
  geom_line(aes(x = Time, y = N7, group = Date, col = "Nest #7")) +
  geom_line(aes(x = Time, y = O7, group = Date, col = "Soil control #7")) + 
  labs(x = "Time (hr)", y = "Temperature (°C)") +
  scale_x_continuous(breaks = c(0,5,10,15,20,24)) +
  theme_anim() + 
  scale_color_manual(values = c("#F7551F", "#F8BE2F")) +
  guides(col = guide_legend(title = NULL)) +
  theme(legend.position = "top") 

ggsave(filename = "basic.png", plot = basicplot,
       path = "/run/media/z/Z/Linux files/Desktop/R projects/GIS-work/Project 1 - Nest proximity/Visualisations/")

plot <- ggplot(data = means) +
  geom_line(aes(x = Time, y = meannest, col = "Nests")) +
  geom_line(aes(x = Time, y = meansoil, col = "Soil controls")) +
  labs(x = "Time (hr)", y = "Mean temperature (°C)") +
  scale_x_continuous(breaks = c(0,5,10,15,20,24)) +
  theme_anim() +
  scale_color_manual(values = c("#F7551F", "#F8BE2F")) +
  guides(col = guide_legend(title = NULL)) +
  theme(legend.position = "top") 

plot +
  geom_point(aes(x = Time, y = meannest, col = "Nests")) +
  geom_point(aes(x = Time, y = meansoil, col = "Soil controls")) +
  transition_reveal(Time)

anim_save(filename = "mean1.gif", animation = last_animation(), 
          path = "/run/media/z/Z/Linux files/Desktop/R projects/GIS-work/Project 1 - Nest proximity/Visualisations/")

#For second observation period --------------------------------------------------------------------
plot1 <- ggplot(data = means2) +
  geom_line(aes(x = Time, y = mean_active, col = "Active nests")) +
  geom_line(aes(x = Time, y = mean_inactive, col = "Inactive nests")) +
  labs(x = "Time (hr)", y = "Temperature (°C)") +
  scale_x_continuous(breaks = c(1,5,10,15,20,24)) +
  theme_anim() +
  scale_color_manual(values = c("#F7551F", "#F8BE2F")) +
  guides(col = guide_legend(title = NULL)) +
  theme(legend.position = "top") 

plot1 +
  geom_point(aes(x = Time, y = mean_active, col = "Active nests")) +
  geom_point(aes(x = Time, y = mean_inactive, col = "Inactive nests")) +
  transition_reveal(Time)

anim_save(filename = "mean2.gif", animation = last_animation(), 
          path = "/run/media/z/Z/Linux files/Desktop/R projects/GIS-work/Project 1 - Nest proximity/Visualisations/")
