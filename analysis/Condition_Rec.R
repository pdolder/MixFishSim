###################################################
## Conditioning of recruitment 
###################################################

library(MixFishSim)

set.seed(123, kind = "L'Ecuyer-CMRG")

## initialise the simulation

sim <- init_sim(nrows = 100, ncols = 100, n_years = 20, n_tows_day = 4, n_days_wk_fished = 5,
     n_fleets = 5, n_vessels = 20, n_species = 4, move_freq = 2)

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


moveCov <- init_moveCov(sim_init = sim, steps = 52, 
			spp_tol = list("spp1" = list("mu" = 10, va = 6),
				       "spp2" = list("mu" = 15, va = 4),
				       "spp3" = list("mu" = 17, va = 7), 
				       "spp4" = list("mu" = 12, va = 10)))



plot_spatiotemp_hab(hab = hab, moveCov = moveCov, spwn_wk = list("spp1" = 16:18, "spp2" = 16:19, "spp3" = 16:18, "spp4" = 18:20))


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
				   "spp2" = c("model" = "BH", "a" = 17, "b" = 4, "cv" = 0.6),
				   "spp3" = c("model" = "BH", "a" = 4, "b" = 11, "cv" = 0.7), 
				   "spp4" = c("model" = "BH", "a" = 1, "b" = 2, "cv" = 0.6)
				   ),
				   rec_wk = list("spp1" = 13:16, "spp2" = 12:16, "spp3" = 14:16, "spp4" = 16:20),
				   spwn_wk = list("spp1" = 16:18, "spp2" = 16:19, "spp3" = 16:18, "spp4" = 18:20),
				   M  = c("spp1" = 0.2, "spp2" = 0.1, "spp3" = 0.2, "spp4" = 0.1),
				   K  = c("spp1" = 0.3, "spp2" = 0.3, "spp3" = 0.3, "spp4" = 0.3))


par(mfrow = c(2,2))

for(spp in 1:4) {

if(spp %in% c(1,3)) { 
	wk <- 16
	bv <- 1e5
	b <- seq(0,bv * 5,length.out = 100)
}

if(spp == 2) {
	wk <- 16
	bv <- 1e6
	b <- seq(0,bv * 5,length.out = 100)
}

if(spp == 4) { 
	wk <- 18 
	bv <- 1e4
	b <- seq(0,bv * 5, length.out = 100)
}

hab_sim <- hab[["spwn_hab"]][[spp]] * moveCov[["cov.matrix"]][[wk]]
hab_sim <- hab_sim / sum(hab_sim)

test <- sapply(b, function(x) {

B <- x * hab_sim 
mat_rec <- Recr_mat(model = Pop[["dem_params"]][[spp]][["rec_params"]][["model"]],
     params = c("a" = as.numeric(Pop[["dem_params"]][[spp]][["rec_params"]][["a"]]),
		"b" = as.numeric(Pop[["dem_params"]][[spp]][["rec_params"]][["b"]])),
     B = B, 
     cv = as.numeric(Pop[["dem_params"]][[spp]][["rec_params"]][["cv"]]))
rec <- sum(mat_rec)
return(rec)
				   })

maxR <- round(max(test) / bv , 2)
plot(b, test, main = paste0("max recruitment = ", maxR * 100))
abline(v = bv, col = "red")
	}


