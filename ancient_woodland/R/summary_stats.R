library(ggplot2)
library(gridExtra)

setwd("/run/media/z/Z/Linux files/Desktop/GIS-work/Project_2/Data/")

#----------------------------------- Data prep ----------------------------------------

data               <- read.csv(file = "Ancient_Woodland_England.csv", h = T)
data$AREA_m        <- data$AREA * 1000000
data$AREA_log      <- log(data$AREA_m)
data$PERIMETER_log <- log(data$PERIMETER)

#------------------------------- Summary statistics -----------------------------------

se <- function(x)(sd(x)/sqrt(length(x)))

mean(data$AREA_m)
se(data$AREA_m)
mean(data$PERIMETER)
se(data$PERIMETER)

range(data$AREA)
range(data$PERIMETER)

#-------------------------------- Summary plots ---------------------------------------

#Linear plots
area  <- ggplot(data, aes(x = AREA)) + 
  xlab("Area (km^2)") +
  ylab("Frequency")
perim <- ggplot(data, aes(x = PERIMETER)) + 
  xlab("Perimeter (m)") +
  ylab("Frequency")

plot_a_i <- area + geom_density(aes(y = ..count..)) + theme_bw()

plot_b_i <- perim + geom_density(aes(y = ..count..)) + theme_bw()

#Log plots
area  <- ggplot(data, aes(x = AREA_log)) + 
  xlab("log(Area (m^2))") +
  ylab("Frequency")
perim <- ggplot(data, aes(x = PERIMETER_log)) + 
  xlab("log(Perimeter (m))") +
  ylab("Frequency")

plot_a_ii <- area + geom_histogram(bins = 30,color = "black", fill = "white") +
  geom_density(aes(y = ..count..)) + 
  theme_bw() + 
  labs(y = NULL)

plot_b_ii <- perim + geom_histogram(bins = 30,color = "black", fill = "white") +
  geom_density(aes(y = ..count..)) + 
  theme_bw() +
  labs(y = NULL)

#Compiling graphs
plot_a_i  <- plot_a_i + ggtitle(" ", subtitle = "a(i)")
plot_a_ii <- plot_a_ii + ggtitle(" ", subtitle = "a(ii)")
plot_b_i  <- plot_b_i + ggtitle(" ", subtitle = "b(i)")
plot_b_ii <- plot_b_ii + ggtitle(" ", subtitle = "b(ii)")

grid.arrange(plot_a_i, plot_a_ii, plot_b_i, plot_b_ii, ncol = 2, nrow = 2)
