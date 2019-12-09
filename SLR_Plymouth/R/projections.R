library(ggplot2)
library(lubridate)
library(gridExtra)

setwd("/home/z/Desktop/R-projects/GIS-work/minor-projects/ME_SLR/Data/projections")

#----------------------------------- Data prep -------------------------------------

files <- list.files(path = ".", pattern = "*.csv")
dfs   <- lapply(files, read.csv, skip = 12)

dfs <- lapply(dfs, function(x) {
  colnames(x) <- c("Date","5","10","30","33","50","67","70","90","95")
  return(x)
  }
)

dfs <- lapply(dfs, function(x) {
  x[,1] <- ymd(x[,1])
  return(x)
  }
)

rcp2.6 <- merge(dfs[[1]], dfs[[2]], by = "Date")
rcp4.5 <- merge(dfs[[3]], dfs[[4]], by = "Date")
rcp8.5 <- merge(dfs[[5]], dfs[[6]], by = "Date")

findmean <- function(x){
  x <- data.frame(
    x$Date,
    rowMeans(x[c("5.x", "5.y")]),
    rowMeans(x[c("10.x", "10.y")]),
    rowMeans(x[c("30.x", "30.y")]),
    rowMeans(x[c("33.x", "33.y")]),
    rowMeans(x[c("50.x", "50.y")]),
    rowMeans(x[c("67.x", "67.y")]),
    rowMeans(x[c("70.x", "70.y")]),
    rowMeans(x[c("90.x", "90.y")]),
    rowMeans(x[c("95.x", "95.y")])
  )
  colnames(x) <- c("Date","5","10","30","33","50","67","70","90","95")
  return(x)
}

rcp2.6 <- findmean(rcp2.6)
rcp4.5 <- findmean(rcp4.5)
rcp8.5 <- findmean(rcp8.5)

write.csv(rcp2.6, file = "rcp2.6.csv")
write.csv(rcp4.5, file = "rcp4.5.csv")
write.csv(rcp8.5, file = "rcp8.5.csv")

#------------------------------------- Plots --------------------------------------

sum.plot <- function(x, col, title, xlab) {
  ggplot(x, aes(x = Date)) +
    geom_ribbon(aes(ymin = x$`5`, ymax = x$`95`), fill = col, alpha = 0.1) +
    geom_ribbon(aes(ymin = x$`10`, ymax = x$`90`), fill = col, alpha = 0.15) +
    geom_ribbon(aes(ymin = x$`30`, ymax = x$`70`), fill = col, alpha = 0.2) +
    geom_ribbon(aes(ymin = x$`33`, ymax = x$`67`), fill = col, alpha = 0.25) +
    geom_line(aes(y = `50`), colour = col) +
    labs(title = title,
         x = xlab, 
         y = "Projected SLR (m)") +
    theme_bw()
}

low    <- sum.plot(x = rcp2.6, col = "blue", title = "RCP 2.6", xlab = "Date (Yr)")
medium <- sum.plot(x = rcp4.5, col = "purple", title = "RCP 4.5", xlab = NULL)
high   <- sum.plot(x = rcp8.5, col = "red", title = "RCP 8.5", xlab = NULL)

p <- ggplot() +
  geom_line(data = rcp2.6, aes(Date, `50`, color = "2.6")) +
  geom_line(data = rcp4.5, aes(Date, `50`, color = "4.5")) +
  geom_line(data = rcp8.5, aes(Date, `50`, color = "8.5")) +
  labs(title = "Projected sea level rise (SLR) for Plymouth (2007-2300)",
       x = "Date (Yr)",
       y = "Mean projected SLR (m)") +
  scale_color_manual(name = "RCP model:", 
                     values = c("2.6"="blue", "4.5"="purple", "8.5"="red")) +
  theme_bw() +
  theme(legend.position = "bottom")

grid.arrange(
  arrangeGrob(p,
              arrangeGrob(high, medium, low, nrow = 3),
              ncol = 2, widths = c(2,1.5)
  )
)

ggsave(filename = "summaryplot.png", plot = last_plot(), 
       path = "/home/z/Desktop/R-projects/GIS-work/minor-projects/ME_SLR/Visualisations")
