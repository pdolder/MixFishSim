
library(MixFishSim)

set.seed(123, kind = "L'Ecuyer-CMRG")

## initialise the simulation

sim <- init_sim(nrows = 100, ncols = 100, n_years = 20, n_tows_day = 4, n_days_wk_fished = 5,
     n_fleets = 5, n_vessels = 20, n_species = 4, move_freq = 2)


# Here's what is produced...
#names(sim)

## create the suitable habitat for each species

hab <- create_hab(sim_init = sim,
		  spp.ctrl = list(
           'spp.1' = list('nu' = 1/0.15, var = 1, scale = 10, Aniso =
           matrix(nc=2, c(1.5, 3, -3, 4))),
           'spp.2' = list('nu' = 1/0.05, var = 2, scale = 20, Aniso =
           matrix(nc=2, c(1, 2, -1, 2))),
	   'spp.3' = list('nu' = 1/0.55, var = 1, scale = 5, Aniso =
           matrix(nc=2, c(2.5, 1, -1, 2))),
           'spp.4' = list('nu' = 1/0.05, var = 1, scale = 30, Aniso =
           matrix(nc=2, c(0.1, 2, -1, 0.2)))
				  ),
		  spawn_areas = list("spp1" = list(area1 = c(40, 50, 40, 50), area2 =
				   c(80, 90, 60, 70)), 
		 "spp2" = list(area1 = c(50, 60, 30, 40), area2 = c(80, 90, 90, 90)),
		 "spp3" = list(area1 = c(30, 34, 10, 20), area2 = c(60, 70, 20, 30)),
		 "spp4" = list(area1 = c(50, 55, 80, 85), area2 = c(30, 40, 30, 40))
		 ),
		spwn_mult = 10, plot.dist = TRUE, plot.file = getwd())

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
		hab = hab[["hab"]], start_cell = c(25,25), 
		lambda = c("spp1" = 0.1, "spp2" = 0.1, "spp3" = 0.1, "spp4" = 0.1), 
		init_move_steps = 20, 
		rec_params =  list("spp1" = c("model" = "BH", "a" = 60, "b" = 250, "cv" = 0.4), 
				   "spp2" = c("model" = "BH", "a" = 100, "b" = 250, "cv" = 0.3),
				   "spp3" = c("model" = "BH", "a" = 80, "b" = 200, "cv" = 0.4), 
				   "spp4" = c("model" = "BH", "a" =  2, "b" = 50, "cv" = 0.3)
				   ),
				   rec_wk = list("spp1" = 13:16, "spp2" = 12:16, "spp3" = 14:16, "spp4" = 16:20),
				   spwn_wk = list("spp1" = 16:18, "spp2" = 16:19, "spp3" = 16:18, "spp4" = 18:20),
				   M  = c("spp1" = 0.2, "spp2" = 0.1, "spp3" = 0.2, "spp4" = 0.1),
				   K  = c("spp1" = 0.3, "spp2" = 0.3, "spp3" = 0.3, "spp4" = 0.3))


#### Spatiotemporal movement covariates

moveCov <- init_moveCov(sim_init = sim, steps = 52, spp_assoc = list("spp1" = 1, "spp2" = -1, "spp3" = -1, "spp4" = 1))

sapply(moveCov[["cov.matrix"]], range)

par(mfrow=c(7, 8), mar = c(0,0,0,0))
for(i in 1:52) {
image(moveCov[["cov.matrix"]][[i]])
}


temp_fun <- function(x, mu, va) {
	(1/(sqrt(2*pi*va)))*(exp(-(((x - mu)^2) / (2 * va))))
}

## For the whole time series

par(mfrow=c(7,8), mar = c(0,0,0,0))
for(i in 1:52) {
x <- moveCov[["cov.matrix"]][[i]]

y <- sapply(x, temp_fun, mu = mean(moveCov[["cov.matrix"]][[i]]),
	    va = 10)
#plot(x,y, type = "p")
image(matrix(nc = 100, y))

}
