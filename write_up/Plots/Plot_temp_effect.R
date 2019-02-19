
library(MixFishSim)

set.seed(123, kind = "L'Ecuyer-CMRG")

## initialise the simulation

sim <- init_sim(nrows = 100, ncols = 100, n_years = 50, n_tows_day = 4, n_days_wk_fished = 5,
     n_fleets = 5, n_vessels = 20, n_species = 4, move_freq = 2)

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
		  spawn_areas = list("spp1" = list(area1 = c(40, 50, 40, 50), area2 =
				   c(80, 90, 60, 70)), 
		 "spp2" = list(area1 = c(20, 40,  0, 20), area2 = c(80, 100, 90, 100)),
		 "spp3" = list(area1 = c(20, 35, 10, 20), area2 = c(60, 70, 90, 100)),
		 "spp4" = list(area1 = c(50, 60, 80, 90), area2 = c(30, 40, 30, 40))
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
		rec_params =  list("spp1" = c("model" = "BH", "a" = 6, "b" = 4, "cv" = 0.7), 
				   "spp2" = c("model" = "BH", "a" = 27, "b" = 4, "cv" = 0.6),
				   "spp3" = c("model" = "BH", "a" = 18, "b" = 11, "cv" = 0.7), 
				   "spp4" = c("model" = "BH", "a" =  0.3, "b" = 0.5, "cv" = 0.6)
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

png("Temperature_gradient.png", height = 800, width = 800)
par(mfrow =c(7,8), mar = c(0,0,0,0))
for(i in 1:52) {
image(1:100, 1:100, moveCov[["cov.matrix"]][[i]], zlim = range(lapply(moveCov[["cov.matrix"]], range)), xaxt = "n", yaxt = "n", col = heat.colors(20))
text(92, 92, labels = i, cex = 2, font = 2)
}
plot(0:1,0:1, type = "n", xaxt = "n", yaxt = "n", bty = "n")
legend(0,1, legend = 1:5, fill = heat.colors(20)[1:5], ncol = 1, border = 'white', y.intersp = 0.6, x.intersp = 0.2, cex = 3)
plot(0:1,0:1, type = "n", xaxt = "n", yaxt = "n", bty = "n")
legend(0,1, legend = 6:10, fill = heat.colors(20)[6:10], ncol = 1, border = 'white', y.intersp = 0.6, x.intersp = 0.2, cex = 3)
plot(0:1,0:1, type = "n", xaxt = "n", yaxt = "n", bty = "n")
legend(0,1, legend = 11:15, fill = heat.colors(20)[11:15], ncol = 1, border = 'white', y.intersp = 0.6, x.intersp = 0.2, cex = 3)
plot(0:1,0:1, type = "n", xaxt = "n", yaxt = "n", bty = "n")
legend(0,1, legend = 16:20, fill = heat.colors(20)[16:20], ncol = 1, border = 'white', y.intersp = 0.6, x.intersp = 0.2, cex = 3)
dev.off()

## Now the thermal tolerances
norm_fun <- function (x, mu, va) {
		(1/(sqrt(2*pi*va)))*(exp(-(((x - mu)^2) / (2 * va))))
}

png("Species_tolerances.png", width = 800, height = 800)
par(mfrow = c(2,2))
for(i in 1:4) {
plot(sapply(seq(1,20,1), norm_fun, mu = moveCov[["spp_tol"]][[i]][["mu"]], 
	    va = moveCov[["spp_tol"]][[i]][["va"]]), type = "l", ylab = "species tolerance", xlab = "Temp")
}
dev.off()

## Spatiotemporal habitat distributions
plot_spatiotemp_hab(hab = hab, moveCov = moveCov, spwn_wk = list("spp1" = 16:18, "spp2" = 16:19, "spp3" = 16:18, "spp4" = 18:20))


