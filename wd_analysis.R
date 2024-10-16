## Load required packages
library(dplyr)
library(ggplot2)
library(lubridate)
library(magrittr)
library(rgdal)
library(raster)
library(rasterVis)
library(RColorBrewer)
library(rts)
## 2) Read in the data
setwd('D:/Paul/California/Original_data')
data <- read.csv("data/SacT41c1_data.csv", header = TRUE, stringsAsFactors = FALSE)
head(data)
str(data)
summary(data)
data$date <- strptime(data$date, "%d/%m/%Y", tz="America/Los_Angeles")
# Aggregate the data to get the average water level by day
#library(lubridate)
#library(dplyr)
data$date <- as.POSIXct(data$date)
data2 <- data %>%
  group_by(date) %>%
  summarize(mean.level.mm = mean(level.mm)) # make sure to use dplyr summarize, not plyr's call!
data2$mean.level.cm <- data2$mean.level.mm*0.1 # convert depth data to cm for use in mapping shorebird suitability
## Add sensor location and base elevation information (collected during RTK GPS survey)
data2$logger.base.cm <- 2496
data2$logger.long.wgs84 <- -122.17349
data2$logger.lat.wgs84 <- 39.371062
## Add wetland identifier
data2$survey <- "SAC:T41:1"
## Limit to 2016 water depth data only
data2 <- data2 %>% filter(date >= as.Date("2016-01-31") & date <= as.Date("2016-06-01"))
## Add month and year day information
data2$month <- as.numeric(format(data2$date, "%m"))
data2$yday <- as.numeric(format(data2$date, "%j"))
## Plot the data
plot(data2$yday, data2$mean.level.cm, xlab = "Year day", ylab = "depth (cm)", main = "SAC:T41:1 water depth by year day")
## A nice plot
p <- ggplot(data2) +
  geom_line(aes(x = yday, y = mean.level.cm)) +
  xlab("Year day") + ylab("Depth measured at sensor (cm)") +
  theme(text = element_text(size = 20),
        axis.text = element_text(size = 20))+
  theme_classic()
#theme_bw()
#ggsave('output/SacT41c1_water_depth_spring2016.png', plot = p,
#       width = 190, height = 190, units = c("mm"),
#      dpi = 300)
## Write out the prepped water depth data
write.csv(data2, "data/SacT41c1_data_prepped.csv")
## Read in the wetland elevation raster (m)
elev <- raster("data/sact41c1_elev_m.tif")
## convert elevation in m to cm elevation
elev <- elev*100
## relativize the elevation layer
elev.min <- cellStats(elev, min)
elev <- elev - elev.min
## plot the elevation (cm)
plot(elev)
## fancier plot
#levelplot(elev, margin = FALSE, par.settings = GrTheme, main = "SAC:T41:1 Elevation (cm)")
p <- levelplot(elev, margin = FALSE, par.settings = GrTheme,
               scales=list(x=list(rot=90, cex = 1.5), y =list(cex = 1.5)),
               #colorkey=list(labels=list(cex=1, font=1),title=expression(Elevation (cm))))
               colorkey=list(labels=list(cex=1.5, font=1))) #+
#p + layer(panel.points(data2$logger.long.wgs84, data2$logger.lat.wgs84, pch=1, cex = 3))
#p + geom_point(aes(x = logger.long.wgs84, y = logger.lat.wgs84), data = data2[,1],
#               alpha = .5, color="black", size = 3)
# png(file = "output/SacT41c1_elev_cm.png",
#     width = 190, height = 190, units = "mm", res = 300)
print(p)
dev.off()
## Contour plot of the elevations for more clarity
#library(RColorBrewer)
elev # 0 - 323.9237cm relative elevation
min.elev <- 0
max.elev <- 325
brk = seq(min.elev, max.elev, 25) # 25 cm breaks
p <- levelplot(elev, contour = T, par.settings = GrTheme, at = brk, colorkey=list(labels=list(cex=2, rot=90)))
# png(file = "output/SacT41c1_contour_cm.png",
#     width = 190, height = 190, units = "mm", res = 600)
# print(p)
#dev.off()
## Generate average water surface elevation raster for one date: April 1, 2016
#data2[data2$date=="2016-04-01",]
b <- 2496.242 - elev.min # logger base elevation in cm pulled from water_depth_stations
h <- 68.36817
# water surface elevation measured at data logger in cm
v <- b + h
surface <- !is.na(elev)
surface[surface==1] <- v
plot(surface)
## Compute the depth (water surface height - dem elevation) for each cell
depth <- surface - elev
min.depth <- depth@data@min
max.depth <- depth@data@max
min.depth #-128.3148
max.depth #195.609
m <- c(-130, 0, NA, 15, 200, 16) # Assume that anything <0 is not inundated
depth2 <- reclassify(depth, m)
## Another way to plot
min.depth2 <- depth2@data@min
max.depth2 <- depth2@data@max
brk = c(min.depth2, 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15, max.depth2)
p <- levelplot(depth2, margin = FALSE, col.regions = colorRampPalette(brewer.pal(9, "YlGnBu"))(18), at = brk,
               scales=list(x=list(cex = 1.5, rot=90), y=list(cex = 1.5)),
               #colorkey=list(labels=list(cex=1, font=1),title=expression(Depth (cm))))
               colorkey=list(labels=list(cex=2, font=1))) #+
#layer(sp.polygons(t41c1_bound), packets = 1)
png(file = "output/SacT41c1_depth_cm_April1_2016.png",
    width = 190, height = 190, units = "mm", res = 600)
print(p)
dev.off()
## Plot just the region of optimal habitat (water depth <10cm)
depth
m <- c(-130, 0, NA, 0, 10, 10, 10, 250, NA)
hab.10 <- reclassify(depth, m)
p <- levelplot(hab.10, margin = FALSE, col.regions = colorRampPalette(brewer.pal(9, "Blues"))(18), at = brk,
               scales=list(x=list(cex = 1.5, rot=90), y=list(cex = 1.5)),
               #colorkey=list(labels=list(cex=1, font=1),title=expression(Depth (cm))))
               colorkey=list(labels=list(cex=1.5, font=1))) #+
#layer(sp.polygons(t41c1_bound), packets = 1)
png(file = "output/SacT41c1_lt10cm_April1_2016.png",
    width = 190, height = 190, units = "mm", res = 600)
print(p)
dev.off()
## Read in the wetland elevation raster (m)
elev <- raster("data/sact41c1_elev_m.tif")
## convert m to cm elevation
elev <- elev*100
## Initialize fields to store the total extent, and proportion of cells that are below depth thresholds of interest
data2$ncell <- 0
data2$prop.5cm <- 0
data2$prop.10cm <- 0
data2$prop.15cm <- 0
## Initialize raster stacks to store the extent of water depths below thresholds of interest
s.5cm <- stack() # Stack for water depths of <=5cm
s.10cm <- stack()
s.15cm <- stack()
dates <- data2$date
for(i in 1:nrow(data2)){
  ## Generate average water surface elevation
  b <- data2$logger.base.cm # logger base elevation in cm pulled from datalogger_positionsV2.csv
  h <- data2$mean.level.cm[i]
  # water level measured at data logger in cm
  v <- b + h
  surface <- !is.na(elev)
  surface[surface==1] <- v
  #plot(surface)
  ## Compute the depth at every cell Water surface height - dem elevation for each cell
  depth <- surface - elev
  ## Reclassify the depth map to show the areas less than 5, 10, and 15cm deep
  #min.depth <- depth@data@min
  #max.depth <- depth@data@max
  #m <- c(min.depth, 5, 5, 5, 10, 10, 15, max.depth, NA ) # reclassification matrix
  m <- c(-Inf, 0, NA, 15, Inf, 16) # reclassification matrix
  suitable <- reclassify(depth, m, include.lowest = TRUE)
  ## calculate total area and proportion of suitable habitat
  ncell <- length(suitable) # 15345 floodable pixels
  # total # valid, non-nodata pixels in the elev map
  # total # pixels below 5cm
  #calc(suitable, fun = function(x, na.rm) sum(x[x<5]))
  suitable.5 <- reclassify(suitable,c(0,5,1))
  suitable.5 <- reclassify(suitable.5, c(5.001, +Inf, 0))
  #calc(suitable.5, fun = function(x, na.rm) sum(x))
  #freq(suitable.5)
  ncell.5 <- sum(suitable.5[suitable.5==1]) #556 pixels
  prop.5 <- ncell.5/ncell
  prop.5 # 0.0362333, <4% of the feature was suitable for peeps!
  # total # pixels below 10cm
  suitable.10 <- reclassify(suitable,c(0,10,1))
  suitable.10 <- reclassify(suitable.10, c(10.001, +Inf, 0))
  #calc(suitable.5, fun = function(x, na.rm) sum(x))
  #freq(suitable.5)
  ncell.10 <- sum(suitable.10[suitable.10==1]) #556 pixels
  prop.10 <- ncell.10/ncell
  prop.10 #
  # total # pixels below 15cm
  suitable.15 <- reclassify(suitable,c(0,15,1))
  suitable.15 <- reclassify(suitable.15, c(15.001, +Inf, 0))
  #calc(suitable.5, fun = function(x, na.rm) sum(x))
  #freq(suitable.5)
  ncell.15 <- sum(suitable.15[suitable.15==1]) #556 pixels
  prop.15 <- ncell.15/ncell
  prop.15 # 0.1163897, ~12% of the feature was suitable for longer-leggeds!
  ## Estimate the volume of water in the wetland
  res.suitable <- res(suitable)
  vol <- suitable*(res.suitable[1]^2)
  vol_cm3 <- cellStats(vol, sum) # 15089.03 cm^3
  vol_af <- (cellStats(vol, sum))/1233481855.32
  data2$ncell[i] <- ncell
  data2$prop.5cm[i] <- prop.5
  data2$prop.10cm[i] <- prop.10
  data2$prop.15cm[i] <- prop.15
  data2$vol_cm3[i] <- vol_cm3
  data2$vol_af[i] <- vol_af
  s.5cm <- stack(s.5cm, suitable.5)
  s.10cm <- stack(s.10cm, suitable.10)
  s.15cm <- stack(s.15cm, suitable.15)
}
head(data2)
## Write the updated data table
write.csv(data2, "data/SacT41c1_spring2016_data_prepped.csv")
## Write out the habitat extent rasters based on water depth thresholds of interest.
names(s.5cm) <- dates
writeRaster(s.5cm, "data/SacT41c1_spring5cm.tif", overwrite = T)
s.5cm #123 layers
names(s.10cm) <- dates
writeRaster(s.10cm, "data/SacT41c1_spring10cm.tif", overwrite = T)
names(s.15cm) <- dates
writeRaster(s.15cm, "data/SacT41c1_spring15cm.tif", overwrite = T)
# This function identifies the sceneID
splitFun <- function(x){
  split1 <- strsplit(x, '_')
  sceneID <- unlist(split1)[1]
  return(sceneID)
}
## This is a function to get the year date information from the same filename
dateFun <- function(x){
  split <- strsplit(x, 'X')
  last <- unlist(split)[2]
  return(last)
}
date.list <- lapply(names(s.10cm), dateFun)
d <- strptime(date.list, "%Y.%m.%d", tz="America/Los_Angeles")
Date <- as.Date(d)
## Extract the Year
Year <- as.numeric(format(Date, "%Y"))
## Extract the Month
month <- as.numeric(format(Date, "%m"))
## Extract the year day
yday <- as.numeric(format(Date, "%j"))
## creating a RasterStackTS object for the extent of habitat (water depth <10cm):
rt <- rts(s.10cm, Date)
# Create a subset for "April" in the year 2016:
index.April.2016 <- which(month == 4 & Year == 2016)
rt.April.2016 <- subset(s.10cm, index.April.2016)
# Generate a "habitat" frequency raster for April 2016
rt.April.2016.sum <- calc(rt.April.2016, sum, na.rm=TRUE)
rt.April.2016.freq<- (rt.April.2016.sum/30)
#plot(rt.April.2016.freq, main = "SAC:T4:3 April 2016 <5cm")
plot(rt.April.2016.freq)
## Reclassify to set 0 to NA for plotting
m <- c(-1, 0, NA)
rt.April.2016.freq <- reclassify(rt.April.2016.freq, m)
# Sets the minimum (0), the maximum (15), and the increasing steps (+1) for the color scale for frequency of suitability.
breaks = seq(0, 1, by = 0.05)
levelplot(rt.April.2016.freq, margin = FALSE, col.regions = colorRampPalette(brewer.pal(7, "Spectral"))(21), at = breaks,
          scales=list(x=list(rot=90)),
          #colorkey=list(labels=list(cex=1, font=1),title=expression(Depth (cm))))
          colorkey=list(labels=list(cex=1, font=1)))
## Nicer ggplot
breaks <- seq(0,1, 0.1)
f <- hist(rt.April.2016.freq, breaks=breaks)
dat <- data.frame(counts= f$counts, breaks = f$mids)
dat # view the counts and breaks
ggplot(dat, aes(x = breaks, y = counts)) +
  geom_bar(stat = "identity",fill='blue',alpha = 0.8)+
  xlab("Frequency <10cm")+ ylab("Frequency")+
  ggtitle("SAC:T41:1 April 2016")+
  scale_x_continuous(breaks = seq(0,1,0.1),  ## without this you will get the same scale
                     labels = seq(0,1,0.1))
sum.10cm <- sum(s.10cm, na.rm = T) # tabulate total number of times each cell was "suitable" <=5cm
prop.10cm <- sum.10cm/123 # compute proportion of observations with suitable habitat
plot(prop.10cm )
## The color ramp will match that for April 2016 frequency of <10cm depth
## reclassify to make regions never habitat NA
m <- c(-1, 0, NA)
prop.10cm <- reclassify(prop.10cm, m)
levelplot(prop.10cm, margin = FALSE, col.regions = colorRampPalette(brewer.pal(7, "Spectral"))(21), at = breaks,
          scales=list(x=list(rot=90)),
          #colorkey=list(labels=list(cex=1, font=1),title=expression(Depth (cm))))
          colorkey=list(labels=list(cex=1, font=1)))
## Plot the proportion of suitable habitat (<= 10 cm)
p <- ggplot(data2, aes(date, prop.10cm)) + geom_line() +
  xlab("date") +
  ylab("Proportion <10cm depth")+
  #theme_bw()+
  theme_classic()+
  theme(text = element_text(size = 35),
        axis.title.x = element_blank()) #+
ggsave('output/SacT41c1_proportion_lt10cm_spring2016.png', plot = p,
       width = 200, height = 170, units = c("mm"),
       dpi = 600)
## Plot the raw water depth data recorded at the data logger spring 2016
p <- ggplot(data2, aes(date, mean.level.cm)) + geom_line() +
  xlab("date") + ylab("Water depth at sensor (cm)")+
  #theme_bw()+
  theme_classic() +
  theme(text = element_text(size = 35),
        axis.title.x = element_blank())
ggsave('output/SacT41c1_logger_depth_spring2016.png', plot = p,
       width = 195, height = 175, units = c("mm"),
       dpi = 600)
## plot the estimated volume of water in the wetland (acre feet)
p <- ggplot(data2, aes(date, vol_af)) + geom_line() +
  xlab("date") + ylab("Volume (acre ft)")+
  theme_classic() +
  theme(text = element_text(size = 35),
        axis.title.x = element_blank())
ggsave('output/SacT41c1_vol_af_spring2016.png', plot = p,
       width = 200, height = 170, units = c("mm"),
       dpi = 600)
