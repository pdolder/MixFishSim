###################################################
## Testing methods for identifying closure areas ##
## Based on irregular data
###################################################

library(MixFishSim)
library(sp)
library(gstat)
library(dplyr)
library(ggplot2)

load('TestResults.RData')

## Test results
logs <- combine_logs(res[["fleets_catches"]])

## Take one month of a year
## Summarise with catch/no_tows
logs2 <- filter(as.data.frame(logs), month == 6, year == 10) %>% group_by(x, y) %>% summarise(spp1 = sum(spp1)/n())

## These are the data
logs2 %>% as.data.frame %>% 
	  ggplot(aes(x, y)) + geom_point(aes(size=spp1), color="blue", alpha=3/4) + 
	    ggtitle("catches of spp1") + coord_equal() + theme_bw()

##################
## For kriging 
##################
coordinates(logs2) <- ~ x+ y
class(logs2)
bbox(logs2)

logs2.vgm <- variogram(spp1 ~ 1, logs2)
logs2.fit <- fit.variogram(logs2.vgm, model = vgm(0.1, "Mat", 10, 1))

summary(logs2.fit)

plot(logs2.vgm, logs2.fit)
grid <- data.frame(x = rep(seq_len(100), times = 100), y = rep(seq_len(100), each = 100))

## Points and grid
par(mfrow = c(1,2))
plot(logs2$x, logs2$y)
plot(grid)

## the actual krigging
coordinates(grid) <- ~ x + y
logs2.kriged <- krige(spp1 ~ 1, logs2, grid, model = logs2.fit, debug.level = -1, block = 1)

########################################################################
### This takes faaar too long, let's try some simple linear interpolation
#########################################################################
library(akima)

grid.interp <- data.frame(x = rep(seq_len(100), times = 100), y = rep(seq_len(100), each = 100))

## linear interpolation
akima.li <- interp(logs2$x, logs2$y, logs2$spp1, nx = 100, ny = 100)
li.zmin <- min(akima.li$z,na.rm=TRUE)
li.zmax <- max(akima.li$z,na.rm=TRUE)
breaks <- pretty(c(li.zmin,li.zmax),100)
colors <- heat.colors(length(breaks)-1)
with(akima.li, image  (x,y,z, breaks=breaks, col=colors))

## spline interpolation
akima.spl <- with(logs2, interp(x, y, spp1, nx=100, ny=100, linear=FALSE))


## Compare vs data
par(mfrow = c(1,2))
with(akima.li, image  (x,y,z, breaks=breaks, col=colors))
points(logs2$x, logs2$y, pch = 16, cex = log(logs2$spp1), col = "blue")
image(akima.spl, main = "smooth  interp(*, linear = FALSE)", col = colors)
points(logs2$x, logs2$y, pch = 16, cex = log(logs2$spp1), col = "blue")

## What if we were to close top 10% catches

##################
# linear interp ##
##################
akimaDF <- data.frame(x = rep(seq_len(100), times = 100), 
		      y = rep(seq_len(100), each = 100),
		      spp1 = as.numeric(akima.li$z))

akimaDF$spp1[is.na(akimaDF$spp1)] <- 0

## highest 10 % of each
q90 <- quantile(akimaDF$spp1, prob = 0.95)

akimaDF$closure <- ifelse(akimaDF$spp1 >= q90, "Closed" , "Open")

p1 <- ggplot(akimaDF, aes(x=x, y=y)) + geom_tile(aes(fill=factor(closure)) )+
coord_equal() + scale_fill_manual(values = c("red","green")) + theme_bw() + geom_point(data = logs2, aes(x = x, y = y, size=spp1), color="blue", alpha=0.1)  

##################
# spline interp ##
##################
akimaDFsp <- data.frame(x = rep(seq_len(100), times = 100), 
		      y = rep(seq_len(100), each = 100),
		      spp1 = as.numeric(akima.spl$z))

akimaDFsp$spp1[is.na(akimaDFsp$spp1)] <- 0

## highest 10 % of each
q90 <- quantile(akimaDFsp$spp1, prob = 0.95)

akimaDFsp$closure <- ifelse(akimaDFsp$spp1 >= q90, "Closed" , "Open")

p2 <- ggplot(akimaDFsp, aes(x=x, y=y)) + geom_tile(aes(fill=factor(closure)) )+
coord_equal() + scale_fill_manual(values = c("red","green")) + theme_bw() + geom_point(data = logs2, aes(x = x, y = y, size=spp1), color="blue", alpha=0.1)  

library(cowplot)

plot_grid(p1, p2)

## Seems much better!  Can use linear interpolation for simplicity and time


## What's actually needed::
load('TestResults.RData')

## Test results
logs <- combine_logs(res[["fleets_catches"]])

## Filter as appropriate, summarise with catch/no_tows
logs2 <- filter(as.data.frame(logs), month == 6, year == 10) %>% group_by(x, y) %>% summarise(spp1 = sum(spp1)/n())

# Do interpolation
akima.li <- interp(logs2$x, logs2$y, logs2$spp1, nx = 100, ny = 100)

# Make as DF
akimaDF <- data.frame(x = rep(seq_len(100), times = 100), 
		      y = rep(seq_len(100), each = 100),
		      spp1 = as.numeric(akima.li$z))

akimaDF$spp1[is.na(akimaDF$spp1)] <- 0
## highest 10 % of each
q90 <- quantile(akimaDF$spp1, prob = 0.95)
akimaDF$closure <- ifelse(akimaDF$spp1 >= q90, "Closed" , "Open")

closed_areas <- akimaDF[akimaDF$closure == 'Closed',]
