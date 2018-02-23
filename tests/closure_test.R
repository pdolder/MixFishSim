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

## Commercial data ##
logs <- combine_logs(res[["fleets_catches"]])
ratios <- T

## Take one month of a year
## Summarise with catch/no_tows

if(ratios != T) {
logs2 <- filter(as.data.frame(logs), month == 6, year == 10) %>% group_by(x, y) %>% summarise(spp1 = sum(spp1)/n())
}

##############
## Ratios!! ##
##############
if(ratios == T) {
logs2 <- filter(as.data.frame(logs), month == 6, year == 10) %>% group_by(x, y) %>% summarise(spp1 = sum(spp1) / n(), spp2 =sum(spp2)/n())

## Calculate the ratios
logs2$dat <- logs2$spp2 / logs2$spp1

## Deal with any NaN and Infs - calculate the Infs as large
## numbers and Nans as 0s
logs2$dat[is.nan(logs2$dat)]      <- 0 
logs2$dat[is.infinite(logs2$dat)] <- 10 * logs2$spp2[is.infinite(logs2$dat)]
## scale the values
logs2$dat <- 1 - (logs2$dat / max(logs2$dat)) ## want to exclude low spp2 areas

logs2$spp1 <- logs2$dat
}
###########

###########
## Survey data
###########

logs <- res[["survey"]][["log.mat"]]

if(ratios != T) {
logs2 <- filter(as.data.frame(logs), year == 10) %>% group_by(x, y) %>% summarise(spp1 = sum(spp1)/n())
}

if(ratios == T) {
logs2 <- filter(as.data.frame(logs), year == 10) %>% group_by(x, y) %>% summarise(spp1 = sum(spp1)/n(), spp2 = sum(spp2)/n())
## Calculate the ratios
logs2$dat <- logs2$spp2 / logs2$spp1

## Deal with any NaN and Infs - calculate the Infs as large
## numbers and Nans as 0s
logs2$dat[is.nan(logs2$dat)]      <- 0 
logs2$dat[is.infinite(logs2$dat)] <- 10 * logs2$spp2[is.infinite(logs2$dat)]
## scale the values
logs2$dat <- 1 - (logs2$dat / max(logs2$dat)) ## want to exclude low spp2 areas

logs2$spp1 <- logs2$dat


}

####################
## Real pop basis ##
####################

logs <- res[["pop_bios"]]

# [[yr,wk]][[spp]]

yrs <- 8:10
wks   <- 1:52
n_spp <- 4


logs_yr_list <- lapply(yrs, function(y) {
	logs_wk_list <- lapply(wks, function(w) {

## Number species
res2 <- lapply(seq_len(n_spp), function(x) {
res <- data.frame(spp = as.numeric(logs[[y,w]][[x]]))
colnames(res) <- paste("spp",x,sep="")
 return(res) 
	   })

res3 <- cbind(data.frame(x = rep(seq_len(100), times = 100), 
			   y = rep(seq_len(100), each = 100),
			   year = y, week = w,  do.call(cbind,res2)))

return(res3)
})

logs_yr_list <- do.call(rbind, logs_wk_list)
return(logs_yr_list)
})

logs <- do.call(rbind, logs_yr_list)

## The calcs

if(ratios != T) {
logs2 <- filter(as.data.frame(logs), year == 10) %>% group_by(x, y) %>% summarise(spp1 = sum(spp1)/n())
}

if(ratios == T) {
logs2 <- filter(as.data.frame(logs), year == 10) %>% group_by(x, y) %>% summarise(spp1 = sum(spp1)/n(), spp2 = sum(spp2)/n())
## Calculate the ratios
logs2$dat <- logs2$spp2 / logs2$spp1

## Deal with any NaN and Infs - calculate the Infs as large
## numbers and Nans as 0s
logs2$dat[is.nan(logs2$dat)]      <- 0 
logs2$dat[is.infinite(logs2$dat)] <- 10 * logs2$spp2[is.infinite(logs2$dat)]
## scale the values
logs2$dat <- 1 - (logs2$dat / max(logs2$dat)) ## want to exclude low spp2 areas

logs2$spp1 <- logs2$dat


}






## These are the data
logs2 %>% as.data.frame %>% 
	  ggplot(aes(x, y)) + geom_point(aes(size=spp1), color="blue", alpha=3/4) + 
	    ggtitle("catches of spp1") + coord_equal() + theme_bw()

##################
## For kriging 
##################
#coordinates(logs2) <- ~ x+ y
#class(logs2)
#bbox(logs2)

#logs2.vgm <- variogram(spp1 ~ 1, logs2)
#logs2.fit <- fit.variogram(logs2.vgm, model = vgm(0.1, "Mat", 10, 1))

#summary(logs2.fit)

#plot(logs2.vgm, logs2.fit)
#grid <- data.frame(x = rep(seq_len(100), times = 100), y = rep(seq_len(100), each = 100))

## Points and grid
#par(mfrow = c(1,2))
#plot(logs2$x, logs2$y)
#plot(grid)

## the actual krigging
#coordinates(grid) <- ~ x + y
#logs2.kriged <- krige(spp1 ~ 1, logs2, grid, model = logs2.fit, debug.level = -1, block = 1)

########################################################################
### This takes faaar too long, let's try some simple linear interpolation
#########################################################################
library(akima)

## ADD A SCALE PARAMETER
sc <- 1 
px <- 100

nx <- px / sc
ny <- px / sc

## linear interpolation
akima.li <- interp(logs2$x, logs2$y, logs2$spp1, nx = nx, ny = ny, duplicate = "mean")
li.zmin <- min(akima.li$z,na.rm=TRUE)
li.zmax <- max(akima.li$z,na.rm=TRUE)
breaks <- pretty(c(li.zmin,li.zmax),100)
colors <- heat.colors(length(breaks)-1)
#with(akima.li, image  (x,y,z, breaks=breaks, col=colors))

## spline interpolation
#akima.spl <- with(logs2, interp(x, y, spp1, nx=nx, ny=ny, linear=FALSE))

## Compare vs data
#par(mfrow = c(1,2))
#with(akima.li, image  (x,y,z, breaks=breaks, col=colors))
#points(logs2$x, logs2$y, pch = 16, cex = log(logs2$spp1), col = "blue")
#image(akima.spl, col = colors)
#points(logs2$x, logs2$y, pch = 16, cex = log(logs2$spp1), col = "blue")

## What if we were to close top 5 % catches

##################
# linear interp ##
##################
#akimaDF <- data.frame(x = rep(seq_len(100), times = 100), 
#		      y = rep(seq_len(100), each = 100),
#		      spp1 = as.numeric(akima.li$z))
#akimaDF$spp1[is.na(akimaDF$spp1)] <- 0

## Trying to get the right values in the right places...
## Fill the data frame
akimaDF <- data.frame(x = rep(seq_len(px), times = px), 
		      y = rep(seq_len(px), each = px),
		      spp1 = as.numeric(apply(t(apply(akima.li$z, 1, rep, each = sc)), 2, rep, each = sc)))

akimaDF$spp1[is.na(akimaDF$spp1)] <- 0

## Check we've allocated grid correctly
x <- 10/sc 
y <- 16/sc
akimaDF[akimaDF$x == 10 & akimaDF$y == 16,]
akima.li$z[x,y]

## highest 10 % of each
q95 <- quantile(akimaDF$spp1[akimaDF$spp1 > 0], prob = 0.95)
akima.li$z[akima.li$z > q95]

akimaDF$closure <- ifelse(akimaDF$spp1 >= q95, "Closed" , "Open")

p1 <- ggplot(akimaDF, aes(x=x, y=y)) + geom_tile(aes(fill=factor(closure)) )+
coord_equal() + scale_fill_manual(values = c("red","green")) + theme_bw() + 
geom_point(data = logs2, aes(x = x, y = y, size=spp1), color="blue", alpha=0.1)  

p1
with(akima.li, image  (x,y,z, breaks=breaks, col=colors))

##################
# spline interp ##
##################
akimaDFsp <- data.frame(x = rep(seq_len(100), times = 100), 
		      y = rep(seq_len(100), each = 100),
		      spp1 = as.numeric(akima.spl$z))

akimaDFsp$spp1[is.na(akimaDFsp$spp1)] <- 0

## highest 10 % of each
q95 <- quantile(akimaDFsp$spp1[akimaDF$spp1 > 0], prob = 0.95)

akimaDFsp$closure <- ifelse(akimaDFsp$spp1 >= q95, "Closed" , "Open")

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
## highest 5 % of each
q95 <- quantile(akimaDF$spp1[akimaDF$spp1 >0], prob = 0.95)
akimaDF$closure <- ifelse(akimaDF$spp1 >= q95, "Closed" , "Open")

closed_areas <- akimaDF[akimaDF$closure == 'Closed',]

print(paste(nrow(closed_areas) / nrow(akimaDF) * 100, "%"))
