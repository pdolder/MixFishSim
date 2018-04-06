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

set.seed(123, kind = "L'Ecuyer-CMRG")

## Common settings
load('Common_Params.RData')

## Scenarios here
load('scenarios.RData')

closure <- NULL

## run_sim function for overall control
res <- run_sim(sim_init = sim, pop_init = Pop, move_cov = moveCov, fleets_init = fleets, hab_init = hab, InParallel = TRUE, cores = 1, save_pop_bio = TRUE, survey = survey, closure = closure)

save(res, file = paste('Scenario',r , '.RData',sep = "_"))
############################################

rm(res); gc()


