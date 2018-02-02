
library(CircStats)

## radians to degrees
rad2deg <- function (r) {
	r * 180/pi
}

# degrees to radians
deg2rad <- function (d) {
	d * pi/180
}

## The von mises distribution

# 1 value, 0 current bearing, 10 concentration param

rad2deg(rvm(1, 0, 10))



## If its a good catch, want to fish back over the same patch..i.e. new bearing
## show be ~ 180 degrees

newbear <- rad2deg(rvm(100,0, 100))

hist(newbear)

newbear2  <- ifelse(newbear > 180, newbear - 180, newbear + 180)
hist(newbear2)

## Then we take the current bearing, and add to the new bearing
# i.e. if current bearing is 270

mean(ifelse(270 + newbear2 > 360, 270 - newbear2, newbear2 - 270))

