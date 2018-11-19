
###########################################################
## Plot of fishing locations before and after closures...
###########################################################
library(MixFishSim)
library(dplyr)
library(ggplot2)

load('Common_Params.RData')

Run <- 2
load(file.path('Scenario_runs_Nov18', paste0("Scenario_", Run, ".RData")))

logs <- combine_logs(res[["fleets_catches"]])

logs$closure <- ifelse(logs$year %in% 26:30, "before", ifelse(logs$year %in% 46:50, "after", "NONE"))
logs$closure <- factor(logs$closure)


##########################
## Extract the closures ##
##########################
# The common settings
load(file.path('..', 'analysis', 'Common_Params.RData'))
## Scenarios here
load(file.path('..', 'analysis','scenarios.RData'))

# Re-calc the closures as implemented
r  <- 2 ## second closure settings
closure <- init_closure(input_coords = NULL, basis = sc$data_type[r], rationale = sc$basis[r], spp1 = 'spp3', spp2 = 'spp2', year_start = 31, year_basis = c(21:30), closure_thresh = 0.95, sc = sc$resolution[r], temp_dyn = sc$timescale[r])

#mn <- 1:12
wk <- 1:52
# t for month 1:12 of year 41
closure_areas <- lapply(1:12, function(m) {
t <-which(sim$brk.idx$year.breaks == 46 & sim$brk.idx$month.breaks == m)[1]
year <- 46
month <- m
#week <- sim$brk.idx$week.breaks[t] 
AreaClosures <- close_areas(sim_init = sim, closure_init = closure, commercial_logs = res$fleets_catches, survey_logs = NULL, real_pop = NULL, t = t)
AreaClosures <- as.data.frame(AreaClosures)
AreaClosures$closure <- "after" # rename for consistency in the plot
AreaClosures$month <- m
return(AreaClosures)
})


# y for years 31-40
#closure_areas <- lapply(46:50, function(y) {
#t <-which(sim$brk.idx$year.breaks == y)[1]
#year <- y 
#mn <- 1:12
#wk <- 1:52
#AreaClosures <- close_areas(sim_init = sim, closure_init = closure, commercial_logs = res$fleets_catches, survey_logs = NULL, real_pop = NULL, t = t)
#AreaClosures <- as.data.frame(AreaClosures)
#AreaClosures$closure <- "after" # rename for consistency in the plot
#AreaClosures$year <- y
#return(AreaClosures)
#})


##############
## Plots    ##
##############

## Let's plot the first fleet only
logs <- filter(logs, closure %in% c("before", "after"))
closure_areas <- do.call(rbind, closure_areas) # make closures list a combined df

closure_areas$x   <- as.numeric(closure_areas$x)
closure_areas$y   <- as.numeric(closure_areas$y)
closure_areas$dat <- as.numeric(closure_areas$dat)

library(cowplot)

logs <- filter(logs, year %in% c(29,46))


## Find bounds of area closures
## But we need to be able to keep areas as contiguous

library(raster)
library(sf)
library(units)
library(smoother)

make_ggpoly <- function(m) {
test <- filter(closure_areas, month == m)
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
ggsave(file = "Closure_fishing_locations.pdf", width = 16, height = 8)

#ggplot(filter(closure_areas, year == 46), aes(x = x , y = y)) + geom_point(colour = "red", shape = 15) +
#	expand_limits(x = c(0,100), y = c(0,100)) + facet_wrap(~year) +
#	geom_point(aes(x = x, y = y, colour = factor(trip)), data = filter(logs, closure == "after", year == 46), 
#		   alpha = 0.2, shape = "x") +
#	theme_bw() + theme(plot.margin = margin(1, 0.5, 0.5, 0.5, "cm"))


