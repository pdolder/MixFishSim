############################################
#
### Quick plots to inspect results
#
############################################
library(MixFishSim)

load('Common_Params.RData')

Run <- 43
load(file.path('Scenario_runs', paste0("Scenario_", Run, "_.RData")))

plot_pop_summary(res, timestep = "annual", save = FALSE)

plot_daily_fdyn(res)


logs <- combine_logs(res[["fleets_catches"]])

plot_vessel_move(sim_init = sim, logs = logs, fleet_no = 1, vessel_no = 1,
       year_trip = 20, trip_no = 1)

plot_vessel_move(sim_init = sim, logs = logs, fleet_no = 1, vessel_no = 1:10,
       year_trip = 10, trip_no = 43:52)

plot_vessel_move(sim_init = sim, logs = logs, fleet_no = 1, vessel_no = 1:10,
       year_trip = 10, trip_no = 43, fleets_init = fleets, pop_bios = res[["pop_bios"]])

plot_fleet_trip(logs = logs, fleet_no = 1, year_trip = 10, trip_no = 1)

plot_catch_comp(gran = c(20, 10, 5, 2), logs = logs, fleets = 1:5,
       vessels = 1:20, trips = 1:60, years = 1:10, cluster_plot = TRUE, cluster_k = 5)

plot_survey(survey = res[["survey"]], type = "index")

plot_realised_stepF(logs = logs, fleet_no = 2, vessel_no = 2)

