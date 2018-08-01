library(MixFishSim)
library(dplyr)
library(ggplot2)

load("TestResults_Close.RData")

logs <- as.data.frame(combine_logs(res[["fleets_catches"]]))

sim <- init_sim(nrows = 100, ncols = 100, n_years = 2, n_tows_day = 4, n_days_wk_fished = 5,
     n_fleets = 5, n_vessels = 10, n_species = 4, move_freq = 2)

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

moveCov <- init_moveCov(sim_init = sim, steps = 52, 
			spp_tol = list("spp1" = list("mu" = 10, va = 6),
				       "spp2" = list("mu" = 15, va = 4),
				       "spp3" = list("mu" = 17, va = 7), 
				       "spp4" = list("mu" = 12, va = 10)))

## Initialise the fleets
Q_mult  <- 0.01

## maximum possible revenue

# fleet 1
B3_1   <- quantile(sapply(1:1000, function(x) { 1 * Q_mult * Pop[["Start_pop"]][[1]][[x]] * 100 +
				 2 * Q_mult * Pop[["Start_pop"]][[2]][[x]] * 200 +
				 1 * Q_mult * Pop[["Start_pop"]][[3]][[x]] * 300 +
				 2 * Q_mult * Pop[["Start_pop"]][[4]][[x]] * 600 
				   }), prob = 0.9)
# fleet 2
B3_2   <- quantile(sapply(1:1000, function(x) { 2 * Q_mult * Pop[["Start_pop"]][[1]][[x]] * 100 +
				 1 * Q_mult * Pop[["Start_pop"]][[2]][[x]] * 200 +
				 2 * Q_mult * Pop[["Start_pop"]][[3]][[x]] * 300 +
				 1 * Q_mult * Pop[["Start_pop"]][[4]][[x]] * 600 
				   }), prob = 0.9)


# fleet 3
B3_3 <- quantile(sapply(1:1000, function(x) { 2 * Q_mult * Pop[["Start_pop"]][[1]][[x]] * 100 +
				 2 * Q_mult * Pop[["Start_pop"]][[2]][[x]] * 200 +
				 2 * Q_mult * Pop[["Start_pop"]][[3]][[x]] * 300 +
				 2 * Q_mult * Pop[["Start_pop"]][[4]][[x]] * 600 
				   }), prob = 0.85)

# fleet 4
B3_4 <- quantile(sapply(1:1000, function(x) { 1 * Q_mult * Pop[["Start_pop"]][[1]][[x]] * 100 +
				 1 * Q_mult * Pop[["Start_pop"]][[2]][[x]] * 200 +
				 1 * Q_mult * Pop[["Start_pop"]][[3]][[x]] * 300 +
				 5 * Q_mult * Pop[["Start_pop"]][[4]][[x]] * 600 
				   }), prob = 0.9)

# fleet 5
B3_5 <- quantile(sapply(1:1000, function(x) { 1 * Q_mult * Pop[["Start_pop"]][[1]][[x]] * 100 +
				 3 * Q_mult * Pop[["Start_pop"]][[2]][[x]] * 200 +
				 2 * Q_mult * Pop[["Start_pop"]][[3]][[x]] * 300 +
				 1 * Q_mult * Pop[["Start_pop"]][[4]][[x]] * 600 
				   }), prob = 0.8)

fleets <- init_fleet(sim_init = sim, VPT = c("spp1" = 100, "spp2" = 200, "spp3" = 600, "spp4" = 1600),
	   Qs = list("fleet 1" = c("spp1" = Q_mult * 1, "spp2" = Q_mult * 2, "spp3" = Q_mult * 1, "spp4" = Q_mult * 2), 
		     "fleet 2" = c("spp1" = Q_mult * 2, "spp2" = Q_mult * 1, "spp3" = Q_mult * 2, "spp4" = Q_mult * 1),
		     "fleet 3" = c("spp1" = Q_mult * 2, "spp2" = Q_mult * 2, "spp3" = Q_mult * 2, "spp4" = Q_mult * 2),
		     "fleet 4" = c("spp1" = Q_mult * 1, "spp2" = Q_mult * 1, "spp3" = Q_mult * 1, "spp4" = Q_mult * 5),
		     "fleet 5" = c("spp1" = Q_mult * 1, "spp2" = Q_mult * 3, "spp3" = Q_mult * 2, "spp4" = Q_mult * 1)
		     ),
	   fuelC = list("fleet 1" = 3, "fleet 2" = 2, "fleet 3" = 5, "fleet 4" = 2, "fleet 5" = 1),
	   step_params = list("fleet 1" = c("rate" = 10, "B1" = 1, "B2" = 10, "B3" = B3_1),
			      "fleet 2" = c("rate" = 20, "B1" = 2 , "B2" = 15, "B3" = B3_2),
			      "fleet 3" = c("rate" = 15, "B1" = 1, "B2" =  8, "B3" = B3_3),
			      "fleet 4" = c("rate" = 25, "B1" = 2, "B2" = 12, "B3" = B3_4),
			      "fleet 5" = c("rate" = 10, "B1" = 3, "B2" =  7, "B3" = B3_5)
			      ),
	   past_knowledge = TRUE,
	   past_year_month = TRUE,
	   past_trip = TRUE,
	   threshold = 0.75)



# Re-calc the closures as implemented
closure <- init_closure(input_coords = NULL, basis = 'commercial', rationale = 'high_pop', spp1 = 'spp1', spp2 = 'spp2', year_start = 2, year_basis = 1, closure_thresh = 0.9, sc = 5, temp_dyn = 'annual')

closure_areas <- lapply(2, function(y) {
t <-which(sim$brk.idx$year.breaks == y)[1]
year <- y 
mn <- 1:12
wk <- 1:52
AreaClosures <- close_areas(sim_init = sim, closure_init = closure, commercial_logs = res$fleets_catches, survey_logs = NULL, real_pop = NULL, t = t)
return(AreaClosures)
})


####################################
######## Testing slow down #########
#########  Go fish func ############
####################################

t = 1356
fl <- 1

library(microbenchmark)

Rprof()
catches <- foreach(fl=seq_len(5)) %do% 
go_fish_fleet(FUN = go_fish,	sim_init = sim, 
			fleets_params = fleets[["fleet_params"]][[fl]],
		   fleets_catches =     res$fleets_catches[[fl]][["fleets_catches"]], 
		   sp_fleets_catches =  res$fleets_catches[[fl]][["sp_fleets_catches"]],
		   pops = res$pop_bios[[2,34]], t = t, closed_areas = closure_areas)
Rprof(NULL)
summaryRprof()

source(file.path("go_fishOLD.R"))
source(file.path("go_fishTEST.R"))

microbenchmark(
"new" = out <- go_fish(sim_init = sim, fleet_params = fleets[["fleet_params"]][[fl]], 
	fleet_catches = res[["fleets_catches"]][[fl]][["fleets_catches"]][[1]], 
	sp_fleet_catches = res$fleets_catches[[fl]][["sp_fleets_catches"]][[1]], 
	pops = res$pop_bios[[2,34]], t = t, closed_areas = closure_areas[[1]]),
"old" = out <- go_fishOLD(sim_init = sim, fleet_params = fleets[["fleet_params"]][[fl]], 
	fleet_catches = res[["fleets_catches"]][[fl]][["fleets_catches"]][[1]], 
	sp_fleet_catches = res$fleets_catches[[fl]][["sp_fleets_catches"]][[1]], 
	pops = res$pop_bios[[2,34]], t = t, closed_areas = closure_areas[[1]]),
"test" = out <- go_fishTEST(sim_init = sim, fleet_params = fleets[["fleet_params"]][[fl]], 
	fleet_catches = res[["fleets_catches"]][[fl]][["fleets_catches"]][[1]], 
	sp_fleet_catches = res$fleets_catches[[fl]][["sp_fleets_catches"]][[1]], 
	pops = res$pop_bios[[2,34]], t = t, closed_areas = as.matrix(closure_areas[[1]])),
	       times = 100)


## For some reason, the R implementation is quicker...??
microbenchmark("Cpp" = distance_calc(x1 = 23, x2 = 45, y1 = 4, y2 = 56),
	       "R" = distance_calcR(x1 = 23, x2 = 45, y1 = 4, y2 = 56),
	       times = 100000)

new.point <- c("12", "14")

class(closure_areas[[1]][,"y"])



#### This is quicker, so replaced
microbenchmark(
"old" = apply(closure_areas[[1]], 1, function(x) {
			    x["x"] == as.integer(new.point[1]) & 
			    x["y"] == as.integer(new.point[2])}),
"test" = mapply(x1 = closure_areas[[1]][,"x"],
       y1 = closure_areas[[1]][,"y"],
       FUN = function(x1,y1) {
	x1 == as.integer(new.point[1]) & 
	y1 == as.integer(new.point[2])}),
	       times = 10000)


###############################
### Test final spatial Fs
###############################
## With mapply
find_spat_f_m  <- function(sim_init = NULL, C = C, B = B, M = M, FUN = baranov_f) {

	ncols <- sim_init[["idx"]][["ncols"]] 
	nrows <- sim_init[["idx"]][["nrows"]]

res <- mapply(x = seq(ncols * nrows), FUN = function(x) {
	       
	       uniroot(f = FUN, interval = c(0,2), tol = 1e-7, C = C[[x]], B =
		       B[[x]], M = M, extendInt = "yes", maxiter = 1000)$root })

res <- matrix(res, ncol = ncols, nrow = nrows)
return(res)

}

fl <- 1

microbenchmark::microbenchmark(
find_spat_f(sim_init = sim, C = res$fleets_catches[[fl]][["sp_fleets_catches"]][[1]][[3]], 
	    B = res$pop_bios[[2,34]][[3]], M = 0.2/52, FUN = baranov_f), 
find_spat_f_m(sim_init = sim, C = res$fleets_catches[[fl]][["sp_fleets_catches"]][[1]][[3]], 
	    B = res$pop_bios[[2,34]][[3]], M = 0.2/52, FUN = baranov_f),
	       times = 10)

## No benefit


## Speed up in general

## A naive calc of F (i.e. C/B)
microbenchmark::microbenchmark(
"exact_f" = find_spat_f(sim_init = sim, C = res$fleets_catches[[fl]][["sp_fleets_catches"]][[1]][[3]], 
	    B = res$pop_bios[[2,34]][[3]], M = 0.2/52, FUN = baranov_f),
"naive_f" = find_spat_f_naive(C = res$fleets_catches[[fl]][["sp_fleets_catches"]][[1]][[3]],
		    B =  res$pop_bios[[2,34]][[3]]),
			       times = 100)

##############################
##### do par v do v for
#############################
#source(file.path("go_fishTEST.R"))
#source(file.path("..","R","go_fish.R"))

for(t in 1000:2000)  {
	print(t)
out <- go_fish(sim_init = sim, fleet_params = fleets[["fleet_params"]][[fl]], 
	fleet_catches = res[["fleets_catches"]][[fl]][["fleets_catches"]][[1]], 
	sp_fleet_catches = res$fleets_catches[[fl]][["sp_fleets_catches"]][[1]], 
	pops = res$pop_bios[[2,34]], t = t, closed_areas = as.matrix(closure_areas[[1]]))
}

library(doParallel)
registerDoParallel() 

t <- 1041

microbenchmark(
"do" = catches <- foreach(fl=seq_len(5)) %do% 
go_fish_fleet(FUN = go_fish,	sim_init = sim, 
			fleets_params = fleets[["fleet_params"]][[fl]],
		   fleets_catches =     res$fleets_catches[[fl]][["fleets_catches"]], 
		   sp_fleets_catches =  res$fleets_catches[[fl]][["sp_fleets_catches"]],
		   pops = res$pop_bios[[2,34]], t = t, closed_areas = closure_areas[[1]]),
"dopar" = catches <- foreach(fl=seq_len(5)) %dopar% 
go_fish_fleet(FUN = go_fish,	sim_init = sim, 
			fleets_params = fleets[["fleet_params"]][[fl]],
		   fleets_catches =     res$fleets_catches[[fl]][["fleets_catches"]], 
		   sp_fleets_catches =  res$fleets_catches[[fl]][["sp_fleets_catches"]],
		   pops = res$pop_bios[[2,34]], t = t, closed_areas = closure_areas[[1]]),
	       times = 10 
)


#################################
### Testing early v late calls to go_fish
#######################################

microbenchmark(
"early" = catches <- foreach(fl=seq_len(5)) %do% 
go_fish_fleet(FUN = go_fish,	sim_init = sim, 
			fleets_params = fleets[["fleet_params"]][[fl]],
		   fleets_catches =     res$fleets_catches[[fl]][["fleets_catches"]], 
		   sp_fleets_catches =  res$fleets_catches[[fl]][["sp_fleets_catches"]],
		   pops = res$pop_bios[[2,34]], t = 2, closed_areas = NULL),
"late" = catches <- foreach(fl=seq_len(5)) %do% 
go_fish_fleet(FUN = go_fish,	sim_init = sim, 
			fleets_params = fleets[["fleet_params"]][[fl]],
		   fleets_catches =     res$fleets_catches[[fl]][["fleets_catches"]], 
		   sp_fleets_catches =  res$fleets_catches[[fl]][["sp_fleets_catches"]],
		   pops = res$pop_bios[[2,34]], t = 2000, closed_areas = closure_areas[[1]]),
	       times = 10 
)



#################################
### Testing go_fish function before it slowed down 
#######################################

microbenchmark(
"old" = catches1 <- foreach(fl=seq_len(5)) %do% 
go_fish_fleet(FUN = go_fish_old, sim_init = sim, 
			fleets_params = fleets[["fleet_params"]][[fl]],
		   fleets_catches =     res$fleets_catches[[fl]][["fleets_catches"]], 
		   sp_fleets_catches =  res$fleets_catches[[fl]][["sp_fleets_catches"]],
		   pops = res$pop_bios[[2,34]], t = 2002, closed_areas = closure_areas[[1]]),
"new" = catches2 <- foreach(fl=seq_len(5)) %do% 
go_fish_fleet(FUN = go_fish,	sim_init = sim, 
			fleets_params = fleets[["fleet_params"]][[fl]],
		   fleets_catches =     res$fleets_catches[[fl]][["fleets_catches"]], 
		   sp_fleets_catches =  res$fleets_catches[[fl]][["sp_fleets_catches"]],
		   pops = res$pop_bios[[2,34]], t = 2002, closed_areas = closure_areas[[1]]),
	       times = 100
)



