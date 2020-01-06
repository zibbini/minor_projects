library(dplyr)
library(ggplot2)
library(ggpmisc)
library(lubridate)
library(plyr)
library(gridExtra)
source("path/to/plot.save.R")

setwd("/path/to/Data/")

#--------------------------------- Data prep ------------------------------------

NestD <- read.csv("Nests-NestDistance.csv", header = TRUE)
dfall <- read.csv("data-all.csv", header = TRUE)

#Filter and format 
NestD <- subset(NestD, Distance <= 5, ) #5 is the max range of F. exsecta territories according to the literature
NestD <- na.omit(NestD)
NestD <- NestD %>% mutate_at(c("InputID", "TargetID"), as.numeric) 

dfall           <- subset(dfall, select = c(No., X.1, E, N, X, Y, diameter, activity, cmpt, Brood.recorded.2018))
colnames(dfall) <- c("no", "satellite", "easting", "northing", "x", "y", "diameter", 
                     "activity", "compartment", "brood_emerge")
dfall           <- na.omit(dfall)
dfall$diameter  <- as.numeric(dfall$diameter)
dfall           <- subset(dfall, select = c(no, x, y, diameter, compartment, brood_emerge))

#Format brood emergence dates
brood_dates <- ldply(strsplit(as.character(dfall$brood_emerge), split = ","), rbind)
brood_dates <- as_tibble(brood_dates)
format.date <- function(x){paste(x, "2018", sep = "/")}
brood_dates <- brood_dates %>% mutate_at(c("1", "2", "3", "4", "5", "6", "7", "8"), format.date)
brood_dates <- brood_dates %>% mutate_at(c("1", "2", "3", "4", "5", "6", "7", "8"), dmy)

#Calculate difftime and merge 
dfall$maxtime  <- ymd(apply(brood_dates[1:8], 1, max, na.rm = TRUE))
dfall$mintime  <- ymd(apply(brood_dates[1:8], 1, min, na.rm = TRUE))
dfall$difftime <- as.numeric(difftime(dfall$maxtime, dfall$mintime, units = "days"))
dfall$difftime <- ifelse(dfall$difftime == 0, 1, dfall$difftime)
dfall$difftime <- ifelse(is.na(dfall$difftime), 0, dfall$difftime)

dfall$compartment <- as.character(dfall$compartment)
dfall$compartment <- ifelse(dfall$compartment == "", 0, dfall$compartment)

#Add column identifier
dfall_input               <- dfall
colnames(dfall_input)[1]  <- "InputID"

#Merging with Nest distances 
nestd_input  <- dfall_input %>% filter(InputID %in% unique(NestD$InputID))
NestD        <- merge(NestD, nestd_input, by = "InputID")

NestD$Distance <- ifelse(NestD$Distance == 0, NA, NestD$Distance)
NestD          <- NestD[complete.cases(NestD[,3]), ]
NestD          <- NestD[!duplicated(NestD), ]

#--------------------------------- Analysis ---------------------------------------

#Summary graphs
p1 <- ggplot(dfall, aes(x = diameter)) +
  geom_density(aes(y = ..count..), fill = "grey50") +
  theme_bw() +
  xlab("Nest diameter (cm)") + 
  ylab("Frequency") +
  labs(title = "", subtitle = "(a)")

p2 <- ggplot(dfall, aes(x = mintime)) +
  geom_density(aes(y = ..count..), fill = "grey50") +
  theme_bw() +
  xlab("First recorded brood") + 
  ylab("Frequency") +
  labs(title = "", subtitle = "(b)")

p3 <- ggplot(dfall, aes(x = difftime)) +
  geom_density(aes(y = ..count..), fill = "grey50") +
  theme_bw() +
  xlab("No. of days nest(s) were producing brood") + 
  ylab("Frequency") +
  labs(title = "", subtitle = "(c)")

p4 <- ggplot(dfall, aes(x = maxtime)) +
  geom_density(aes(y = ..count..), fill = "grey50") +
  theme_bw() +
  xlab("Last recorded brood") + 
  ylab("Frequency") +
  labs(title = "", subtitle = "(d)")

pall <- grid.arrange(p1, p2, p3, p4, ncol = 2, nrow = 2)

plot.save(pall, width = 713, height = 630, 
          filename = "dist_plots.png", path = "/path/to/F.exsecta_analysis/Visualisations/")

#Normality tests for cor tests
shapiro.test(dfall$diameter)
shapiro.test(dfall$difftime)
shapiro.test(NestD$Distance)
shapiro.test(NestD$difftime)
shapiro.test(NestD$diameter)

cor.test(dfall$diameter, dfall$difftime, method = "pearson") #significant: weak +
cor.test(NestD$Distance, NestD$difftime, method = "pearson") #non-significant
cor.test(NestD$Distance, NestD$diameter, method = "pearson") #significant: weak -

#Plots for above correlation tests
p5 <- ggplot(dfall, aes(x = diameter, y = difftime)) + 
  geom_point() +
  geom_smooth(method = "lm", color = "red") +
  theme_bw() +
  xlab("Nest diameter (cm)") +
  ylab("Recorded time nest(s) produced brood (days)")

p6 <- ggplot(NestD, aes(x = Distance, y = difftime)) +
  geom_point() +
  geom_smooth(method = "lm", color = "red") +
  theme_bw() +
  xlab("Distance between nests (m)") +
  ylab("Recorded time nest(s) produced brood (days)") +
  labs(title = "", subtitle = "(a)")

p7 <- ggplot(NestD, aes(x = Distance, y = diameter)) +
  geom_point() +
  geom_smooth(method = "lm", color = "red") +
  theme_bw() +
  xlab("Distance between nests (m)") +
  ylab("Nest diameter (cm)") +
  labs(title = "", subtitle = "(b)")

pall2 <- grid.arrange(p6, p7, ncol = 2, nrow = 1)

plot.save(pall2, width = 850, height = 500, filename = "nestdist_plots.png", 
          path = "/path/to/F.exsecta_analysis/Visualisations/")

plot.save(p5, width = 624, height = 487, filename = "nestdiam_plot.png", 
          path = "/path/to/F.exsecta_analysis/Visualisations/")
