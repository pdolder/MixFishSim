############################################################################
## Code to run a number of scenarios testing data type (commercial, survey,
## real population), resolution (spatio-temporal) and basis for closure (high
## pop or high ratio spp1:spp2)
#############################################################################
#############
#
## Runs/ scenarios 
#
#############

library(MixFishSim)
#library(doParallel)
#registerDoParallel(cores = 40)

set.seed(123, kind = "L'Ecuyer-CMRG")

## Common settings
load('Common_Params.RData')

# Closure locations
load('Fixed_Closure_Locations.RData')

## Change closure per scenario
closure <- init_closure(input_coords = cl[,c("x","y")], year_start = 1, temp_dyn = "yearly")

## run_sim function for overall control
res <- run_sim(sim_init = sim, pop_init = Pop, move_cov = moveCov, fleets_init = fleets, hab_init = hab, InParallel = TRUE, cores = 1, save_pop_bio = TRUE, survey = survey, closure = closure)

save(res, file = file.path('Scenario_runs', 'Comparison_Run.RData'))
############################################

rm(res); gc()




