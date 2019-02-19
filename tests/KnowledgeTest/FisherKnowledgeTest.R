############################################################################
## Code to test efficiency of past knowledge over CRW 
#############################################################################
#######################
#
## Conditioning
#
######################

library(MixFishSim)
library(tidyverse)

set.seed(123, kind = "L'Ecuyer-CMRG")

## initialise the simulation

sim <- init_sim(nrows = 20, ncols = 20, n_years = 10, n_tows_day = 4, n_days_wk_fished = 5,
     n_fleets = 1, n_vessels = 1, n_species = 4, move_freq = 2)

## create the suitable habitat for each species

hab <- create_hab(sim_init = sim,
		  spp.ctrl = list(
           'spp.1' = list('nu' = 1/0.015, var = 1, scale = 10, Aniso =
           matrix(nc=2, c(1.5, 3, -3, 4))),
           'spp.2' = list('nu' = 1/0.05, var = 2, scale = 12, Aniso =
           matrix(nc=2, c(1, 2, -1, 2))),
	   'spp.3' = list('nu' = 1/0.01, var = 1, scale = 12, Aniso =
           matrix(nc=2, c(2.5, 1, -1, 2))),
           'spp.4' = list('nu' = 1/0.005, var = 1, scale = 8, Aniso =
           matrix(nc=2, c(0.1, 2, -1, 0.02)))
				  ),
		  spawn_areas = list("spp1" = list(area1 = c(4, 5, 4, 5), area2 =
				   c(8, 9, 6, 7)), 
		 "spp2" = list(area1 = c(2, 4,  0, 2), area2 = c(8, 10, 9, 10)),
		 "spp3" = list(area1 = c(2, 3, 1, 2), area2 = c(6, 7, 9, 10)),
		 "spp4" = list(area1 = c(5, 6, 8, 9), area2 = c(3, 4, 3, 4))
		 ),
		spwn_mult = 10, plot.dist = TRUE, plot.file = getwd())

## Look at overlaps of species habitats

over<-hab[["hab"]]

for(i in 1:4) {
x<- over[[i]]
x[which(x>0)] <-1
over[[i]] <- x
}

plot <- F
if(plot) {
par(mfrow = c(2,3))
image(over[[1]] + over[[2]], main = "spp1 and spp 2")
image(over[[1]] + over[[3]], main = "spp1 and spp 3")
image(over[[1]] + over[[4]], main = "spp1 and spp 4")
image(over[[2]] + over[[3]], main = "spp2 and spp 3")
image(over[[2]] + over[[4]], main = "spp2 and spp 4")
image(over[[3]] + over[[4]], main = "spp3 and spp 4")
}


## Initialise the populations

## Notes: A small lambda will speed up the spread of the population
##        This will reduce the need for a larger number of init_move_steps
##        which is the slower part of the function

# Define the stock recruitment parameters - these will apply to the whole pop,
# so I'm unsure of scale as yet

#Recr(model = "BH", params = list("a" = 1000, "b" = 450), 
#     B = max(Pop[["Start_pop"]][["spp1"]]), cv = 0)
#Recr(model = "BH", params = list("a" = 2000, "b" = 900), 
#     B = max(Pop[["Start_pop"]][["spp2"]]), cv = 0)

Pop <- init_pop(sim_init = sim, Bio = c(spp1 = 1e5, spp2 = 2e5, spp3 = 1e5, spp4 = 1e4), 
		hab = hab[["hab"]], start_cell = c(2,2), 
		lambda = c("spp1" = 0.1, "spp2" = 0.1, "spp3" = 0.1, "spp4" = 0.1), 
		init_move_steps = 20, 
		rec_params =  list("spp1" = c("model" = "BH", "a" = 6000, "b" = 4, "cv" = 0.7), 
				   "spp2" = c("model" = "BH", "a" = 24000, "b" = 4, "cv" = 0.6),
				   "spp3" = c("model" = "BH", "a" = 9000, "b" = 11, "cv" = 0.7), 
				   "spp4" = c("model" = "BH", "a" =  3000, "b" = 0.5, "cv" = 0.6)
				   ),
				   rec_wk = list("spp1" = 13:16, "spp2" = 12:16, "spp3" = 14:16, "spp4" = 16:20),
				   spwn_wk = list("spp1" = 16:18, "spp2" = 16:19, "spp3" = 16:18, "spp4" = 18:20),
				   M  = c("spp1" = 0.2, "spp2" = 0.1, "spp3" = 0.2, "spp4" = 0.1),
				   K  = c("spp1" = 0.3, "spp2" = 0.3, "spp3" = 0.3, "spp4" = 0.3))


#### Spatiotemporal movement covariates

moveCov <- init_moveCov(sim_init = sim, steps = 52, 
			spp_tol = list("spp1" = list("mu" = 12, va = 8),
				       "spp2" = list("mu" = 15, va = 8),
				       "spp3" = list("mu" = 17, va = 7), 
				       "spp4" = list("mu" = 14, va = 10)))


#plot_spatiotemp_hab(hab = hab, moveCov =, spwn_wk = list("spp1" = 16:18, "spp2" = 16:19, "spp3" = 16:18, "spp4" = 18:20))


## Initialise the fleets
Q_mult  <- 0.1

## maximum possible revenue

VPT <- c("spp1" = 100, 
	 "spp2" = 200, 
	 "spp3" = 350, 
	 "spp4" = 600)

# fleet 1
B3_1   <- quantile(sapply(1:400, function(x) { 1 * Q_mult * Pop[["Start_pop"]][[1]][[x]] * VPT[["spp1"]] +
				 2 * Q_mult * Pop[["Start_pop"]][[2]][[x]] * VPT[["spp2"]] +
				 1 * Q_mult * Pop[["Start_pop"]][[3]][[x]] * VPT[["spp3"]] +
				 2 * Q_mult * Pop[["Start_pop"]][[4]][[x]] * VPT[["spp4"]] 
				   }), prob = 0.9)

#test_step(step_params = list("rate"  = 20, "B1" = 1, "B2" = 5, "B3" = B3_1), rev.max = 3200)

fleets_knowledge <- init_fleet(sim_init = sim, VPT = VPT,
	   Qs = list("fleet 1" = c("spp1" = Q_mult * 1, "spp2" = Q_mult * 1, "spp3" = Q_mult * 0.2, "spp4" = Q_mult * 1) 
		     ),
		     fuelC = list("fleet 1" = 3),
	   step_params = list("fleet 1" = c("rate" = 20, "B1" = 1, "B2" = 7, "B3" = B3_1)
			      ),
	   past_knowledge = TRUE,
	   past_year_month = TRUE,
	   past_trip = TRUE,
	   threshold = 0.7)

fleets_CRW <- init_fleet(sim_init = sim, VPT = VPT,
	   Qs = list("fleet 1" = c("spp1" = Q_mult * 1, "spp2" = Q_mult * 1, "spp3" = Q_mult * 0.2, "spp4" = Q_mult * 1) 
		     ),
		     fuelC = list("fleet 1" = 3),
	   step_params = list("fleet 1" = c("rate" = 20, "B1" = 1, "B2" = 7, "B3" = B3_1)
			      ),
	   past_knowledge = FALSE,
	   past_year_month = FALSE,
	   past_trip = FALSE,
	   threshold = 0)


## Setup survey
survey <- NULL

cl <- expand.grid(x = seq(8,12,1), y = c(seq(4,7,1)))

closure <- init_closure(input_coords = cl[,c("x","y")], year_start = 3, temp_dyn = "yearly")

library(doParallel)

registerDoParallel(cores = 2)

set.seed(123, kind = "L'Ecuyer-CMRG")

scenarios <- c("fleets_knowledge", "fleets_CRW")


runs <- foreach(r = scenarios) %dopar% { 
run_sim(sim_init = sim, pop_init = Pop, move_cov = moveCov, 
	fleets_init = get(r), hab_init = hab, 
	InParallel = TRUE, cores = 1, 
	save_pop_bio = TRUE, survey = survey, closure = closure)
}

names(runs) <- scenarios

plot_pop_summary(runs[[scenarios[1]]], timestep = "annual", save = FALSE)
plot_pop_summary(runs[[scenarios[2]]], timestep = "annual", save = FALSE)

## Fishing mortality by scenario

Fs.sc1 <- lapply(runs[[scenarios[[1]]]]$pop_summary, function(x) {
       return(apply(x$F.mat, 1, sum, na.rm = T)) 
})
Fs.sc1 <- reshape2::melt(as.data.frame(do.call(rbind, Fs.sc1)))
Fs.sc1$pop <- c("spp1", "spp2", "spp3", "spp4")
Fs.sc1$scenario <- scenarios[[1]]
colnames(Fs.sc1) <- c("year", "data", "pop", "scenario")

Fs.sc2 <- lapply(runs[[scenarios[[2]]]]$pop_summary, function(x) {
       return(apply(x$F.mat, 1, sum, na.rm = T)) 
})
Fs.sc2 <- reshape2::melt(as.data.frame(do.call(rbind, Fs.sc2)))
Fs.sc2$pop <- c("spp1", "spp2", "spp3", "spp4")
Fs.sc2$scenario <- scenarios[[2]]
colnames(Fs.sc2) <- c("year", "data", "pop", "scenario")

Fs <- rbind(Fs.sc1, Fs.sc2)

ggplot(Fs, aes(x = year, y = data)) + geom_point(aes(colour = scenario)) + 
	geom_line(aes(colour = scenario, group = scenario)) + facet_wrap(~pop)


###################################
## How to compare performance....
##################################

## plot of value over time ??
logs <- lapply(runs, function(x) {
	return(combine_logs(x[["fleets_catches"]]))
	})
names(logs) <- scenarios

logs[[1]]$scenario <- scenarios[[1]]
logs[[2]]$scenario <- scenarios[[2]]

logs <- do.call(rbind, logs)

sum_logs <- logs %>% group_by(year, scenario) %>% summarise(val = sum(val))

ggplot(logs, aes(x = factor(year), y = val, fill = scenario)) + geom_boxplot()

ggplot(sum_logs, aes(x = factor(year), y = val, colour = scenario, group = scenario)) +
	geom_point() + geom_line()

