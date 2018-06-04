
library(MixFishSim)

load(file.path('..', 'analysis', 'Common_Params.RData'))
load(file.path('..', 'analysis', 'Scenario_runs2', 'Scenario_2_.RData'))
load(file.path('..', 'analysis','scenarios.RData'))## Scenarios here

run_no <- 2

r  <- 2
## Change closure per scenario
closure <- init_closure(input_coords = NULL, basis = sc$data_type[r], rationale = sc$basis[r], spp1 = 'spp3', spp2 = 'spp2', year_start = 31, year_basis = c(21:30), closure_thresh = 0.95, sc = sc$resolution[r], temp_dyn = sc$timescale[r])

catches <- as.data.frame(combine_logs(res$fleets_catches))

# t for month 1:12 of year 31

closure_areas <- lapply(1:12, function(m) {

t <-which(sim$brk.idx$year.breaks == 21 & sim$brk.idx$month.breaks == m)[1]
year <- 31
month <- m
week <- sim$brk.idx$week.breaks[t] 
AreaClosures <- close_areas(sim_init = sim, closure_init = closure, commercial_logs = res$fleets_catches, survey_logs = NULL, real_pop = NULL, t = t)
return(AreaClosures)
})

par(mfrow=c(3,4))
for(m in 1:12) {
plot(closure_areas[[m]]$x, closure_areas[[m]]$y)
}


