library(dplyr)
library(ggplot2)
library(ggpmisc)
library(lubridate)
library(plyr)

setwd("/run/media/z/Z/R-projects/GIS-work/minor-projects/F. exsecta analysis/Data")

#--------------------------------- Data prep ------------------------------------

NestD <- read.csv("nest_distance.csv", header = TRUE)
dfall <- read.csv("data-all.csv", header = TRUE)

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
ggplot(dfall, aes(x = diameter)) +
  geom_density(aes(y = ..count..), fill = "grey50") +
  theme_bw() +
  xlab("Nest diameter (cm)") + 
  ylab("Frequency")

ggplot(dfall, aes(x = mintime)) +
  geom_density(aes(y = ..count..), fill = "grey50") +
  theme_bw() +
  xlab("First recorded brood") + 
  ylab("Frequency")

ggplot(dfall, aes(x = maxtime)) +
  geom_density(aes(y = ..count..), fill = "grey50") +
  theme_bw() +
  xlab("Last recorded brood") + 
  ylab("Frequency")

ggplot(dfall, aes(x = difftime)) +
  geom_density(aes(y = ..count..), fill = "grey50") +
  theme_bw() +
  xlab("No. of days nest(s) were producing brood") + 
  ylab("Frequency")

# Normality tests for cor tests
shapiro.test(dfall$diameter)
shapiro.test(dfall$difftime)
shapiro.test(NestD$Distance)
shapiro.test(NestD$difftime)
shapiro.test(NestD$diameter)

cor.test(dfall$diameter, dfall$difftime, method = "pearson") # significant: weak +
cor.test(NestD$Distance, NestD$difftime, method = "pearson") # non-significant
cor.test(NestD$Distance, NestD$diameter, method = "pearson") # significant: weak -

# Plots for above correlation tests
ggplot(dfall, aes(x = diameter, y = difftime)) + 
  geom_point() +
  geom_smooth(method = lm,formula = y~x, colour = "red") +
  theme_bw() +
  xlab("Nest diameter (cm)") +
  ylab("Time nest(s) produced brood (days)") 

ggplot(NestD, aes(x = Distance, y = difftime)) +
  geom_point() +
  geom_smooth(method = lm,formula = y~x, colour = "red") +
  theme_bw() +
  xlab("Distance between nests (m)") +
  ylab("Recorded time nest(s) produced brood (days)") 

ggplot(NestD, aes(x = Distance, y = diameter)) +
  geom_point() +
  geom_smooth(method = lm,formula = y~x, colour = "red") +
  theme_bw() +
  xlab("Distance between nests (m)") +
  ylab("Nest diameter (cm)") 