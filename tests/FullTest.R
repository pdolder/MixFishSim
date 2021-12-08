#source('build.R')

library(MixFishSim)

set.seed(123, kind = "L'Ecuyer-CMRG")

## initialise the simulation

sim <- init_sim(nrows = 100, ncols = 100, n_years = 2, n_tows_day = 4, n_days_wk_fished = 5,
     n_fleets = 2, n_vessels = 10, n_species = 2, move_freq = 2)

# Here's what is produced...
#names(sim)

## create the suitable habitat for each species

hab <- create_hab(sim_init = sim,
		  spp.ctrl = list(
           'spp.1' = list('nu' = 1/0.15, var = 1, scale = 10, Aniso =
           matrix(nc=2, c(1.5, 3, -3, 4))),
           'spp.2' = list('nu' = 1/0.05, var = 2, scale = 20, Aniso =
           matrix(nc=2, c(1, 2, -1, 2)))),
		  spawn_areas = list("spp1" = list(area1 = c(40, 50, 40, 50), area2 =
				   c(80, 90, 60, 70)), 
		 "spp2" = list(area1 = c(50, 60, 30, 40), area2 = c(80, 90, 90, 90))),
		spwn_mult = 10, plot.dist = TRUE, plot.file = ".")

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

Pop <- init_pop(sim_init = sim, Bio = c(spp1 = 1e5, spp2 = 2e5), 
		hab = hab[["hab"]], start_cell = c(25,25), lambda = c("spp1" = 0.1, "spp2" = 0.1), 
		init_move_steps = 20, 
		rec_params =  list("spp1" = c("model" = "BH", "a" = 600, "b" = 1500, "cv" = 0.2), 
				   "spp2" = c("model" = "BH", "a" = 800, "b" = 3000, "cv" = 0.1)),
				   rec_wk = list("spp1" = 13:16, "spp2" = 12:16),
				   spwn_wk = list("spp1" = 16:18, "spp2" = 16:19),
				   M  = c("spp1" = 0.2, "spp2" = 0.1))

## Initialise the fleets

## maximum possible revenue

# fleet 1
max1 <- max(sapply(1:1000, function(x) { 0.01 * Pop[["Start_pop"]][[1]][[x]] * 100 +
				 0.02 * Pop[["Start_pop"]][[2]][[x]] * 200}))
# fleet 2
max2 <- max(sapply(1:1000, function(x) { 0.02 * Pop[["Start_pop"]][[1]][[x]] * 100 +
				 0.01 * Pop[["Start_pop"]][[2]][[x]] * 200}))

#test_step(step_params = list("rate"  = 10, "B1" = 0.1, "B2" = 15, "B3" = max1), rev.max = max1)
#test_step(step_params = list("rate"  = 10, "B1" = 0.5, "B2" = 17, "B3" = max2), rev.max = max2)

fleets <- init_fleet(sim_init = sim, VPT = c("spp1" = 100, "spp2" = 200),
	   Qs = list("fleet 1" = c("spp1" = 0.01, "spp2" = 0.02), 
		     "fleet 2" = c("spp1" = 0.02, "spp2" = 0.01)),
		     fuelC = list("fleet 1" = 800, "fleet 2" = 600),
	   step_params = list("fleet 1" = c("rate" = 10, "B1" = 0.1, "B2" = 15, "B3" = max1),
			      "fleet 2" = c("rate" = 10, "B1" = 0.5, "B2" = 17, "B3" = max2)
			      ),
	   past_knowledge = TRUE,
	   past_year_month = TRUE,
	   past_trip = TRUE,
	   threshold = 0.75)

## Setup survey
survey <- init_survey(sim_init = sim, design = "fixed_station", 
		n_stations = 50, start_day = 92, Qs = c("spp1" = 1, "spp2" = 1, "spp3" = 1, "spp4" = 1)) 

## run_sim function for overall control

#Rprof()

res <- run_sim(sim_init = sim, pop_init = Pop, fleets_init = fleets, hab_init = hab, InParallel = TRUE, cores = 1, 
	       save_pop_bio = TRUE, survey = survey)

#Rprof(NULL)

#summaryRprof()

plot_survey(survey = res[["survey"]], type = "spatial")
plot_survey(survey = res[["survey"]], type = "index")


par(mfrow=c(2,2))
image(res[["pop_bios"]][[1,1]][[1]])
image(res[["pop_bios"]][[1,2]][[1]])
image(res[["pop_bios"]][[2,1]][[1]])
image(res[["pop_bios"]][[1,17]][[1]])

image(res[["pop_bios"]][[4]][[1]]-res[["pop_bios"]][[44]][[1]])


res$pop_summary

## Biomass spp 1
plot(as.vector(t(res[["pop_summary"]][["spp1"]][["Bio.mat"]])), ylim = c(0,max(res[["pop_summary"]][["spp1"]][["Bio.mat"]], na.rm = T)))

## Biomass spp 1
plot(as.vector(t(res[["pop_summary"]][["spp2"]][["Bio.mat"]])), ylim = c(0,max(res[["pop_summary"]][["spp2"]][["Bio.mat"]], na.rm = T)), type = "b")

# Catches, spp 1
plot(as.vector(t(res[["pop_summary"]][["spp1"]][["Catch.mat"]])), col = "red")

# Catches, spp 2
plot(as.vector(t(res[["pop_summary"]][["spp2"]][["Catch.mat"]])), col = "red")

# Fishing mortality, spp 1
plot(as.vector(t(res[["pop_summary"]][["spp1"]][["F.mat"]])), type = "b")

# Fishing mortality, spp 2
plot(as.vector(t(res[["pop_summary"]][["spp2"]][["F.mat"]])), type = "b")

# Recruitment, spp 1
barplot(c(res[["pop_summary"]][["spp1"]][["Rec.mat"]][1,]))

# Recruitment, spp 2
barplot(c(res[["pop_summary"]][["spp2"]][["Rec.mat"]][1,]))

## Look at a vessels tracks

# res[["fleets_catches"]] levels
# [[1]] is all fleet
# [[1]][[1]] is the first fleets catch logs
# [[1]][[1]][[1]] is the first vessel of the first fleet
# [[1]][[2]][[1]] is the spatial catches of both species for the first fleet
# [[1]][[2]][[1]][[1]] is the spatial catches for the first fleet, first
# species

## The value field
image(1:100, 1:100, 0.01 * 100 * Pop[["Start_pop"]][[1]] + 
                0.02 * 200 * Pop[["Start_pop"]][[2]])

# For a vessel, the locations fished
v <- 1

# All points
points(res[["fleets_catches"]][[1]][[1]][[v]][, "x"], 
     res[["fleets_catches"]][[1]][[1]][[v]][, "y"], pch = "x")

# Only last year
image(1:100, 1:100, 0.01 * 100 * Pop[["Start_pop"]][[1]] + 
                0.02 * 200 * Pop[["Start_pop"]][[2]])
points(res[["fleets_catches"]][[1]][[1]][[v]][4159:5200, "x"], 
     res[["fleets_catches"]][[1]][[1]][[v]][4159:5200, "y"], pch = "x")


## The step function
plot(res[["fleets_catches"]][[1]][[1]][[v]][1:5000, "val"], 
     res[["fleets_catches"]][[1]][[1]][[v]][2:5001, "stepD"])

## The change in bearing
plot(res[["fleets_catches"]][[1]][[1]][[v]][1:5000, "val"],
     res[["fleets_catches"]][[1]][[1]][[v]][1:5000, "angles"] - 
     res[["fleets_catches"]][[1]][[1]][[v]][2:5001, "angles"])

## Fleet 2

image(1:100, 1:100, 0.02 * 100 * Pop[["Start_pop"]][[1]] + 
                0.01 * 200 * Pop[["Start_pop"]][[2]])
# For a vessel, the locations fished
v <- 1

points(res[["fleets_catches"]][[2]][[1]][[v]][, "x"], 
     res[["fleets_catches"]][[2]][[1]][[v]][, "y"], pch = "x")

# Only last year
image(1:100, 1:100, 0.02 * 100 * Pop[["Start_pop"]][[1]] + 
                0.01 * 200 * Pop[["Start_pop"]][[2]])

points(res[["fleets_catches"]][[2]][[1]][[v]][4159:5200, "x"], 
     res[["fleets_catches"]][[2]][[1]][[v]][4159:5200, "y"], pch = "x")


## Considerations:
# Add a % tows from historic activity when catches < threshold 
# What about the idea of a home port or a limited range for smaller vessels ??

## Test von mises

get_bearing(b = 90, k = 1)

x <- seq(0.1,100,0.1)
x <- rep(x, 1000)
y <- sapply(x, FUN = get_bearing, b = 0)

library(dplyr)

xy <- data.frame(x = x, y = y)
xy <- xy %>% group_by(x) %>% 
	summarise(five = quantile(y, probs = 0.05), 
		  fifty = quantile(y, probs = 0.5), 
		  ninetyfive = quantile(y, probs = 0.95)) 

plot(xy$x, xy$fifty, type = "l", ylim = c(0,360))
lines(xy$x, xy$five, type = "l", col = "blue")
lines(xy$x, xy$ninetyfive, type = "l", col = "blue")
