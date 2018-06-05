###############################################
# Stability test for fully conditioned model 
###############################################
#source('build.R')

library(MixFishSim)

set.seed(123, kind = "L'Ecuyer-CMRG")

## initialise the simulation

sim <- init_sim(nrows = 100, ncols = 100, n_years = 3, n_tows_day = 4, n_days_wk_fished = 5,
     n_fleets = 5, n_vessels = 10, n_species = 4, move_freq = 2)

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

#test_step(step_params = list("rate"  = 20, "B1" = 1, "B2" = 10, "B3" = B3_1), rev.max = max1)
#test_step(step_params = list("rate"  = 20, "B1" = 0.5, "B2" = 15, "B3" = max2), rev.max = max2)

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


## Setup survey
survey <- init_survey(sim_init = sim, design = "fixed_station", 
		n_stations = 50, start_day = 92, Qs = c("spp1" = 1, "spp2" = 1, "spp3" = 1, "spp4" = 1)) 

## Example 1
#closure <- init_closure(input_coords = NULL, basis = 'commercial', rationale =
#'high_pop', spp1 = 'spp1', spp2 = NULL, year_start = 2, year_basis = NULL, closure_thresh = 0.9, sc = 5, temp_dyn = 'annual')

closure <- init_closure(input_coords = NULL, basis = 'survey', rationale = 'high_ratio', spp1 = 'spp1', spp2 = 'spp2', year_start = 2, year_basis = 1, closure_thresh = 0.9, sc = 5, temp_dyn = 'annual')

## Example 2 - fails correctly
#closure <- init_closure(input_coords = list("area1" = c(2,3), "area2" = c(3,5)),
#			basis = 'survey', rationale = 'high_pop', spp1 = 'spp1', spp2 = 'spp2', year_start = 15, temp_dyn = 'weekly')

## Example 3
#closure <- init_closure(input_coords = NULL, basis = 'survey', rationale = 'high_pop', spp1 = 'spp1', spp2 = 'spp2', year_start = 15, closure_thresh = 0.95, temp_dyn = 'weekly')

## run_sim function for overall control
res <- run_sim(sim_init = sim, pop_init = Pop, move_cov = moveCov, fleets_init = fleets, hab_init = hab, InParallel = TRUE, cores = 1, save_pop_bio = TRUE, survey = survey, closure = closure)

format(object.size(res), units = "auto")

save(res, file = 'TestResults_Close.RData')
############################################

############################################
load('TestResults.RData')
load('TestResults_Close.RData')

plot_pop_summary(res, timestep = "daily", save = FALSE)

plot_pop_summary(res, timestep = "annual", save = FALSE)
ggsave(file.path("plots", "annual_summary.png"))

plot_daily_fdyn(res)
ggsave(file.path("plots", "fDynamics.png"))

## Look at a vessels tracks

logs <- combine_logs(res[["fleets_catches"]])

plot_vessel_move(sim_init = sim, logs = logs, fleet_no = 1, vessel_no = 1,
       year_trip = 20, trip_no = 1)
ggsave(file.path("plots", "vessel_move.png"))

# multiple trips
plot_vessel_move(sim_init = sim, logs = logs, fleet_no = 1, vessel_no = 1:10,
       year_trip = 10, trip_no = 43:52)
ggsave(file.path("plots", "vessel_multi_move.png"))

# with the value field behind
plot_vessel_move(sim_init = sim, logs = logs, fleet_no = 1, vessel_no = 1:10,
       year_trip = 10, trip_no = 43, fleets_init = fleets, pop_bios = res[["pop_bios"]])
ggsave(file.path("plots", "vessel_move_value.png"))


plot_fleet_trip(logs = logs, fleet_no = 1, year_trip = 10, trip_no = 1)
ggsave(file.path("plots", "fleets_move.png"))

# catch compositions
png(file = file.path("plots", "catch_comp.png"), width = 1600, height = 400)
plot_catch_comp(gran = c(20, 10, 5, 2), logs = logs, fleets = 1:5,
       vessels = 1:20, trips = 1:20, years = 1:10, cluster_plot = FALSE)
dev.off()

png(file = file.path("plots", "catch_comp_clusters.png"), width = 1600, height = 800)
plot_catch_comp(gran = c(20, 10, 5, 2), logs = logs, fleets = 1:5,
       vessels = 1:20, trips = 1:60, years = 1:10, cluster_plot = TRUE, cluster_k = 5)
dev.off()

plot_catch_comp(gran = c(20, 10, 5, 2), logs = logs, fleets = 1:5,
       vessels = 1:20, trips = 1:60, years = 1:10, cluster_plot = TRUE, cluster_k = 5, scale_data = TRUE)

# fisheies independent survey
plot_survey(survey = res[["survey"]], type = "spatial")
ggsave(file.path("plots", "spatial_survey.png"), width = 12, height = 16)

plot_survey(survey = res[["survey"]], type = "index")
ggsave(file.path("plots", "survey_index.png"))


# res[["fleets_catches"]] levels
# [[1]] is all fleet
# [[1]][[1]] is the first fleets catch logs
# [[1]][[1]][[1]] is the first vessel of the first fleet
# [[1]][[2]][[1]] is the spatial catches of both species for the first fleet
# [[1]][[2]][[1]][[1]] is the spatial catches for the first fleet, first
# species

## The step functions diagnostics

png(file = file.path("plots", "step_function.png"), width = 800, height = 400)
plot_realised_stepF(logs = logs, fleet_no = 2, vessel_no = 2)
dev.off()


