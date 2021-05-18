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
suppressMessages(require(doParallel))
registerDoParallel(cores = 8)

set.seed(123, kind = "L'Ecuyer-CMRG")

## Common settings
load('Common_Params.RData')

## Scenarios here
load('scenarios.RData')

run_no <- 21:24

runs <- foreach(r = run_no) %dopar% {
## Change closure per scenario
closure <- init_closure(input_coords = NULL, basis = sc$data_type[r], rationale = sc$basis[r], spp1 = 'spp3', spp2 = 'spp2', year_start = 31, year_basis = c(21:30), closure_thresh = 0.95, sc = sc$resolution[r], temp_dyn = sc$timescale[r])

## run_sim function for overall control
res <- run_sim(sim_init = sim, pop_init = Pop, move_cov = moveCov, fleets_init = fleets, hab_init = hab, InParallel = TRUE, cores = 1, save_pop_bio = TRUE, survey = survey, closure = closure)

save(res, file = paste('Scenario',r , '.RData',sep = "_"))
############################################

rm(res); gc()

}

save(runs, file = paste('Scenarios', run_no,'.RData', sep = "_"))


