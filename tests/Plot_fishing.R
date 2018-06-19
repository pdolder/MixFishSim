###########################################################
## Plot of fishing locations before and after closures...
###########################################################
library(MixFishSim)
library(dplyr)
library(ggplot2)

load("TestResults_Close.RData")

logs <- as.data.frame(combine_logs(res[["fleets_catches"]]))

logs$closure <- ifelse(logs$year %in% 1, "before", ifelse(logs$year %in% 2, "after", "NONE"))
logs$closure <- factor(logs$closure)


##########################
## Extract the closures ##
##########################
sim <- init_sim(nrows = 100, ncols = 100, n_years = 2, n_tows_day = 4, n_days_wk_fished = 5,
     n_fleets = 5, n_vessels = 10, n_species = 4, move_freq = 2)


# Re-calc the closures as implemented
closure <- init_closure(input_coords = NULL, basis = 'commercial', rationale = 'high_pop', spp1 = 'spp1', spp2 = 'spp2', year_start = 2, year_basis = 1, closure_thresh = 0.9, sc = 5, temp_dyn = 'annual')


# y for years 31-40
closure_areas <- lapply(2, function(y) {
t <-which(sim$brk.idx$year.breaks == y)[1]
year <- y 
mn <- 1:12
wk <- 1:52
AreaClosures <- close_areas(sim_init = sim, closure_init = closure, commercial_logs = res$fleets_catches, survey_logs = NULL, real_pop = NULL, t = t)
#AreaClosures$closure <- "after" # rename for consistency in the plot
#AreaClosures$year <- y
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
	geom_point(aes(x = x, y = y), data = filter(logs, closure == "after"), 
		   alpha = 0.2, shape = "x") +
	theme_bw() + theme(plot.margin = margin(1, 0.5, 0.5, 0.5, "cm"))

plot_grid(p1,p2, labels = c("(a) before closures", "(b) after closures"), vjust = 2)
ggsave(file = "Closure_fishing_locations.pdf", width = 12, height = 8)

ggplot(filter(closure_areas, year == 40), aes(x = x , y = y)) + geom_point(colour = "red", shape = 15) +
	expand_limits(x = c(0,100), y = c(0,100)) + facet_wrap(~year) +
	geom_point(aes(x = x, y = y, colour = factor(trip)), data = filter(logs, closure == "after", year == 40), 
		   alpha = 0.2, shape = "x") +
	theme_bw() + theme(plot.margin = margin(1, 0.5, 0.5, 0.5, "cm"))

