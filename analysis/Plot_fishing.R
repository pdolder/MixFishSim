
###########################################################
## Plot of fishing locations before and after closures...
###########################################################
library(MixFishSim)
library(dplyr)
library(ggplot2)

load('Common_Params.RData')

Run <- 3
load(file.path('Scenario_runs2', paste0("Scenario_", Run, "_.RData")))

logs <- as.data.frame(combine_logs(res[["fleets_catches"]]))

logs$closure <- ifelse(logs$year %in% 26:30, "before", ifelse(logs$year %in% 36:40, "after", "NONE"))
logs$closure <- factor(logs$closure)


##########################
## Extract the closures ##
##########################
# The common settings
load(file.path('..', 'analysis', 'Common_Params.RData'))
## Scenarios here
load(file.path('..', 'analysis','scenarios.RData'))

# Re-calc the closures as implemented
r  <- 3 ## second closure settings
closure <- init_closure(input_coords = NULL, basis = sc$data_type[r], rationale = sc$basis[r], spp1 = 'spp3', spp2 = 'spp2', year_start = 31, year_basis = c(21:30), closure_thresh = 0.95, sc = sc$resolution[r], temp_dyn = sc$timescale[r])

mn <- 1:12
wk <- 1:52
# t for month 1:12 of year 31
closure_areas <- lapply(1:12, function(m) {
t <-which(sim$brk.idx$year.breaks == 21 & sim$brk.idx$month.breaks == m)[1]
year <- 31
month <- m
#week <- sim$brk.idx$week.breaks[t] 
AreaClosures <- close_areas(sim_init = sim, closure_init = closure, commercial_logs = res$fleets_catches, survey_logs = NULL, real_pop = NULL, t = t)
AreaClosures$closure <- "after" # rename for consistency in the plot
AreaClosures$month <- m
return(AreaClosures)
})


# y for years 31-40
closure_areas <- lapply(31:40, function(y) {
t <-which(sim$brk.idx$year.breaks == y)[1]
year <- y 
mn <- 1:12
wk <- 1:52
AreaClosures <- close_areas(sim_init = sim, closure_init = closure, commercial_logs = res$fleets_catches, survey_logs = NULL, real_pop = NULL, t = t)
AreaClosures$closure <- "after" # rename for consistency in the plot
AreaClosures$year <- y
return(AreaClosures)
})


##############
## Plots    ##
##############

## Let's plot the first fleet only
logs <- filter(logs, fleet == 1, closure %in% c("before", "after"))
closure_areas <- do.call(rbind, closure_areas) # make closures list a combined df

library(cowplot)

p1 <- ggplot(filter(logs, closure == "before"), aes(x = x , y = y)) +
	geom_point(colour = "blue", alpha = 0.2, shape = "x") +
	expand_limits(x = c(0,100), y = c(0,100)) + facet_wrap(~year) +
	theme_bw() + theme(plot.margin = margin(1, 0.5, 0.5, 0.5, "cm"))

p2 <- ggplot(closure_areas, aes(x = x , y = y)) + geom_point(colour = "red", shape = 15) +
	expand_limits(x = c(0,100), y = c(0,100)) + facet_wrap(~year) +
	geom_point(aes(x = x, y = y, colour = factor(trip)), data = filter(logs, closure == "after"), 
		   alpha = 0.2, shape = "x") +
	theme_bw() + theme(plot.margin = margin(1, 0.5, 0.5, 0.5, "cm"))

plot_grid(p1,p2, labels = c("(a) before closures", "(b) after closures"), vjust = 2)
ggsave(file = "Closure_fishing_locations.pdf", width = 12, height = 8)

ggplot(filter(closure_areas, year == 40), aes(x = x , y = y)) + geom_point(colour = "red", shape = 15) +
	expand_limits(x = c(0,100), y = c(0,100)) + facet_wrap(~year) +
	geom_point(aes(x = x, y = y, colour = factor(trip)), data = filter(logs, closure == "after", year == 40), 
		   alpha = 0.2, shape = "x") +
	theme_bw() + theme(plot.margin = margin(1, 0.5, 0.5, 0.5, "cm"))

######## Testing slow down
debug(go_fish)
res <- go_fish(sim_init = sim, fleet_params = fleets$fleet_params[[1]],
fleet_catches = res$fleets_catches[[1]][[1]][[1]],
sp_fleet_catches = res$fleets_catches[[1]][[2]], closed_areas = closure_areas[[1]],
t = 1356, pops = res$pop_bios[[4,34]])

