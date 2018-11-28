
###########################################################
## Plot of fishing locations before and after closures...
###########################################################

library(MixFishSim)
library(dplyr)
library(ggplot2)

yrs <- c(29, 46) ## years before and after the closures

#timescale <- "yearly"
#timescale <- "monthly"
timescale <- "weekly"

load('Common_Params.RData')

if(timescale == "yearly")  { Run  <- 3 }
if(timescale == "monthly") { Run  <- 2 }
if(timescale == "weekly")  { Run  <- 1 }

load(file.path('Scenario_runs_Nov18', paste0("Scenario_", Run, ".RData")))

logs <- combine_logs(res[["fleets_catches"]])

logs$closure <- ifelse(logs$year == yrs[1], "before", ifelse(logs$year == yrs[2], "after", "NONE"))
logs$closure <- factor(logs$closure)
logs <- filter(logs, year %in% yrs)

##########################
## Extract the closures ##
##########################
## Closures run from year 31:50
closure_yrs <- 31:50

if(timescale == "yearly") {
closed_areas     <- res$closures[which(closure_yrs == yrs[2])]
closed_areas     <- as.data.frame(closed_areas)
closed_areas$x   <- as.numeric(closed_areas$x)
closed_areas$y   <- as.numeric(closed_areas$y)
closed_areas$dat <- as.numeric(closed_areas$dat)
closed_areas$year <- yrs[2]
}

if(timescale == "monthly") {
closed_areas     <- res$closures[c(which(closure_yrs == yrs[2]) * 12):
				  c(which(closure_yrs == yrs[2]) * 12+11)]
closed_areas <- lapply(1:12, function(x) { cbind(as.data.frame(closed_areas[x]), data.frame(month =x))})
closed_areas       <- do.call(rbind, closed_areas)
closed_areas$x     <- as.numeric(closed_areas$x)
closed_areas$y     <- as.numeric(closed_areas$y)
closed_areas$dat   <- as.numeric(closed_areas$dat)
}

if(timescale == "weekly") {
closed_areas     <- res$closures[c(which(closure_yrs == yrs[2]) * 52):
				  c(which(closure_yrs == yrs[2]) * 52+51)]
closed_areas <- lapply(1:52, function(x) { cbind(as.data.frame(closed_areas[x]), data.frame(week =x))})
closed_areas       <- do.call(rbind, closed_areas)
closed_areas$x     <- as.numeric(closed_areas$x)
closed_areas$y     <- as.numeric(closed_areas$y)
closed_areas$dat   <- as.numeric(closed_areas$dat)
}


## Find bounds of area closures
## But we need to be able to keep areas as contiguous

library(raster)
library(sf)
library(units)
library(smoother)
library(cowplot)

if(timescale == "yearly") {
make_ggpoly <- function(y) {
test <- closed_areas
dfr <- rasterFromXYZ(test[,c(1,2,5)])
polyr <- rasterToPolygons(dfr, dissolve = T)
polyr <- fortify(polyr)
return(cbind(polyr, data.frame(year = y)))
}
cl <- lapply(yrs[2], function(x) make_ggpoly(y = x))
cl <- do.call(rbind, cl)

p1 <- ggplot(filter(logs, closure == "before"), aes(x = x , y = y)) +
	geom_point(colour = "blue", alpha = 0.2, shape = "x") +
	expand_limits(x = c(0,100), y = c(0,100)) + facet_wrap(~year) +
	theme_bw() + theme(plot.margin = margin(1, 0.5, 0.5, 0.5, "cm"))

p2 <- ggplot(cl) + geom_polygon(aes(long, lat, group = group), colour = "red",fill = NA) +
	expand_limits(x = c(0,100), y = c(0,100)) + facet_wrap(~year) +
	geom_point(aes(x = x, y = y), colour = "blue", 
		   data = filter(logs, closure == "after"), 
		   alpha = 0.2, shape = "x") +
	theme_bw() + theme(plot.margin = margin(1, 0.5, 0.5, 0.5, "cm"))

plot_grid(p1,p2, labels = c("(a) before closures", "(b) after closures"), vjust = 2)
ggsave(file = "Closure_fishing_locations_yearly.pdf", width = 16, height = 8)

}


if(timescale == "monthly") {
make_ggpoly <- function(m) {
test <- filter(closed_areas, month == m)
dfr <- rasterFromXYZ(test[,c(1,2,5)])
polyr <- rasterToPolygons(dfr, dissolve = T)
polyr <- fortify(polyr)
return(cbind(polyr, data.frame(month = m)))
}
cl <- lapply(1:12, function(x) make_ggpoly(m = x))
cl <- do.call(rbind, cl)

p1 <- ggplot(filter(logs, closure == "before"), aes(x = x , y = y)) +
	geom_point(colour = "blue", alpha = 0.2, shape = "x") +
	expand_limits(x = c(0,100), y = c(0,100)) + facet_wrap(~month) +
	theme_bw() + theme(plot.margin = margin(1, 0.5, 0.5, 0.5, "cm"))

p2 <- ggplot(cl) + geom_polygon(aes(long, lat, group = group), colour = "red",fill = NA) +
	expand_limits(x = c(0,100), y = c(0,100)) + facet_wrap(~month) +
	geom_point(aes(x = x, y = y), colour = "blue", 
		   data = filter(logs, closure == "after"), 
		   alpha = 0.2, shape = "x") +
	theme_bw() + theme(plot.margin = margin(1, 0.5, 0.5, 0.5, "cm"))

plot_grid(p1,p2, labels = c("(a) before closures", "(b) after closures"), vjust = 2)
ggsave(file = "Closure_fishing_locations_monthly.pdf", width = 16, height = 8)

}

if(timescale == "weekly") {
make_ggpoly <- function(w) {
test <- filter(closed_areas, week == w)
dfr <- rasterFromXYZ(test[,c(1,2,5)])
polyr <- rasterToPolygons(dfr, dissolve = T)
polyr <- fortify(polyr)
return(cbind(polyr, data.frame(week = w)))
}
cl <- lapply(1:52, function(x) make_ggpoly(w = x))
cl <- do.call(rbind, cl)

p1 <- ggplot(filter(logs, closure == "before"), aes(x = x , y = y)) +
	geom_point(colour = "blue", alpha = 0.2, shape = "x") +
	expand_limits(x = c(0,100), y = c(0,100)) + facet_wrap(~week) +
	theme_bw() + theme(plot.margin = margin(1, 0.5, 0.5, 0.5, "cm"))

p2 <- ggplot(cl) + geom_polygon(aes(long, lat, group = group), colour = "red",fill = NA) +
	expand_limits(x = c(0,100), y = c(0,100)) + facet_wrap(~week) +
	geom_point(aes(x = x, y = y), colour = "blue", 
		   data = filter(logs, closure == "after"), 
		   alpha = 0.2, shape = "x") +
	theme_bw() + theme(plot.margin = margin(1, 0.5, 0.5, 0.5, "cm"))

plot_grid(p1,p2, labels = c("(a) before closures", "(b) after closures"), vjust = 2)
ggsave(file = "Closure_fishing_locations_weekly.pdf", width = 16, height = 8)

}




