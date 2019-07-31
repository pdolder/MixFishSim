############################################
#
### Quick plots to inspect results
#
############################################
library(MixFishSim)

load('Common_Params.RData')

Run <- 0 
load(file.path('Scenario_runs_Nov18', paste0("Scenario_", Run, ".RData")))
#load(file.path('Scenario_runs_Nov18', paste0("Comparison_Run.RData")))

plot_pop_summary(res, timestep = "annual", save = FALSE)

plot_daily_fdyn(res)


daily_fdyn <- function (results) {
	
	n_spp <- length(res[["pop_summary"]]) 
	res_spp <- lapply(seq_len(n_spp), function(x) {
 			 x1_res <- tidyr::gather(as.data.frame(t(results[["pop_summary"]][[x]][["F.mat"]])), key = "year", factor_key = T)
			 F_df <- data.frame("pop" = paste0("spp_", x), 
					    "day" = rep(1:362,length.out = nrow(x1_res)),
					    "year" = x1_res$year,
					    data = x1_res$value)
			 
				  })
	res_out <- do.call(rbind, res_spp)

	return(res_out)

}

library(tidyverse)

df <- daily_fdyn(res)

mean_f <- df %>% group_by(pop, day) %>%
	summarise(data = mean(data, na.rm = T))

ggplot(filter(df, !is.na(data)), aes(x = day, y = data, group = year)) +
	geom_line(colour = "grey", alpha = 0.4) +
	geom_line(data = filter(mean_f,!is.na(data)), aes(group = pop),
		  colour = "black", size = 1) +  
	facet_wrap(~pop) +
	theme_classic()
	
ggsave(file = file.path('..', 'write_up', 'Plots', 'f_dynamics.png'), width = 8, height = 8)

logs <- combine_logs(res[["fleets_catches"]])

plot_vessel_move(sim_init = sim, logs = logs, fleet_no = 3, vessel_no = 14,
       year_trip = 30, trip_no = 1)
ggsave(file = file.path("..", "write_up", "Plots", "vessel_move.png"), width = 8, height = 8)


plot_vessel_move(sim_init = sim, logs = logs, fleet_no = 3, vessel_no = 12,
       year_trip = 30, trip_no = 43:52)
ggsave(file = file.path("..", "write_up", "Plots", "vessel_multi_move.png"), width = 8, height = 8)

plot_vessel_move(sim_init = sim, logs = logs, fleet_no = 3, vessel_no = 1:20,
       year_trip = 34, trip_no = 43, fleets_init = fleets, pop_bios = res[["pop_bios"]])
ggsave(file = file.path("..", "write_up", "Plots", "vessel_move_value.png"), width = 8, height = 8)

plot_fleet_trip(logs = logs, fleet_no = 3, year_trip = 30, trip_no = 2)
ggsave(file = file.path("..", "write_up", "Plots", "fleet_moves.png"), width = 8, height = 8)

plot_catch_comp(gran = c(20, 10, 5, 2), logs = logs, fleets = 1:5, scale_data = FALSE,
       vessels = 1:20, trips = 1:60, years = 40:50, cluster_plot = TRUE, cluster_k = 6)

plot_survey(survey = res[["survey"]], type = "index")

png(file = file.path("..", "write_up", "Plots", "step_function.png"), width = 1200, height = 800)
plot_realised_stepF(logs = logs, fleet_no = 3, vessel_no = 2)
dev.off()



