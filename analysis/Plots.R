############################################
#
### Quick plots to inspect results
#
############################################
library(MixFishSim)

load('Common_Params.RData')

Run <- 0 
load(file.path('Scenario_runs_Nov18', paste0("Scenario_", Run, ".RData")))

plot_pop_summary(res, timestep = "annual", save = FALSE)

plot_daily_fdyn(res)
ggsave(file = file.path('..', 'write_up', 'Plots', 'f_dynamics.png'), width = 8, height = 8)

combine_logs <- function (fleets_catches) {

	# index
	no_fleets     <- length(fleets_catches)
	ves_per_fleet <- length(fleets_catches[[1]][[1]])
	rec_per_vess  <- nrow(fleets_catches[[1]][[1]][[1]])


	all_logs <- lapply(seq_len(no_fleets), function(x1) {
	
	fleets_logs <- lapply(seq_len(ves_per_fleet), function(x2) {
		
	fleet_level <- cbind("vessel" = x2, fleets_catches[[x1]][[1]][[x2]]) 
	return(fleet_level)
})
	fleets_logs <- do.call(rbind, fleets_logs)
	
	
	all_level <- cbind("fleet" = x1, fleets_logs)
})

	all_logs <- do.call(rbind, all_logs)
          
	return(as.data.frame(all_logs))

}

logs <- combine_logs(res[["fleets_catches"]])

plot_vessel_move(sim_init = sim, logs = logs, fleet_no = 2, vessel_no = 5,
       year_trip = 3, trip_no = 1:8)
ggsave(file = file.path("..", "write_up", "Plots", "vessel_move.png"), width = 8, height = 8)


plot_vessel_move(sim_init = sim, logs = logs, fleet_no = 1, vessel_no = 1:10,
       year_trip = 10, trip_no = 43:52)
ggsave(file = file.path("..", "write_up", "Plots", "vessel_multi_move.png"), width = 8, height = 8)

plot_vessel_move(sim_init = sim, logs = logs, fleet_no = 2, vessel_no = 1:10,
       year_trip = 34, trip_no = 43, fleets_init = fleets, pop_bios = res[["pop_bios"]])
ggsave(file = file.path("..", "write_up", "Plots", "vessel_move_value.png"), width = 8, height = 8)

plot_fleet_trip(logs = logs, fleet_no = 1, year_trip = 10, trip_no = 1)
ggsave(file = file.path("..", "write_up", "Plots", "fleet_moves.png"), width = 8, height = 8)

plot_catch_comp(gran = c(20, 10, 5, 2), logs = logs, fleets = 1:5, scale_data = FALSE,
       vessels = 1:20, trips = 1:60, years = 1:10, cluster_plot = TRUE, cluster_k = 6)

plot_survey(survey = res[["survey"]], type = "index")

png(file = file.path("..", "write_up", "Plots", "step_function.png"), width = 1200, height = 800)
plot_realised_stepF(logs = logs, fleet_no = 2, vessel_no = 2)
dev.off()



