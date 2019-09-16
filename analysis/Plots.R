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

plot_vessel_move(sim_init = sim, logs = logs, fleet_no = 3, fleets_init = fleets, 
		 vessel_no = 14,
       year_trip = 30, trip_no = 1, pop_bios = res[["pop_bios"]])
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


### New panel plot combing for manuscript
logs <- combine_logs(res[["fleets_catches"]])


source("../R/plot_vessel_move.R")

p1 <- plot_vessel_move(sim_init = sim, logs = logs, fleet_no = 3, fleets_init = fleets, 
		 vessel_no = 14,  year_trip = 30, trip_no = 1, pop_bios = res[["pop_bios"]])

p2 <- plot_vessel_move(logs = logs, fleet_no = 3, vessel_no = 4, year_trip = 30, trip_no =  1:12)

source("../R/plot_fleet_trip.R")


p3 <- plot_fleet_trip(logs = logs, fleet_no = 3, year_trip = 30, trip_no = 2,
		      fleets_init = fleets, pop_bios = res[["pop_bios"]], 
		      sim_init = sim)

## Recreate step function plots in ggplot

stepDF <- dplyr::filter(as.data.frame(logs), fleet == 3, vessel == 2)

## Lag the data
	stepDF$val_lagged    <- c(stepDF$val[1:nrow(stepDF)-1],NA)
	stepDF$step_lagged   <- c(stepDF$stepD[2:nrow(stepDF)],NA)
	stepDF$angles_lagged <- c(stepDF$angles[2:nrow(stepDF)],NA)

	plot(stepDF$val_lagged, stepDF$step_lagged, main = "Realised step distances", xlab = "value", ylab = "step distance")
	plot(stepDF$val_lagged, stepDF$angles_lagged, main = "Realised turning angles", xlab = "value", ylab = "change in angle")


stepDF2 <- stepDF %>% select("val_lagged", "step_lagged", "angles_lagged") %>% 
	reshape2::melt(id = "val_lagged")


levels(stepDF2$variable)[levels(stepDF2$variable) == "step_lagged"]  <- "Steps"
levels(stepDF2$variable)[levels(stepDF2$variable) == "angles_lagged"]  <- "Angles"

colnames(stepDF2)[1] <- "value"
colnames(stepDF2)[3] <- "data"


p4 <- ggplot(stepDF2, aes(x = value, y = data)) +
	geom_point() + theme_bw() +
	facet_wrap(~variable, ncol = 2, scale = "free") +
	ylab("distance or angle")

library(cowplot)

mult_pl <- plot_grid(p1, p2, p3, p4, 
		     labels= c("A", "B", "C", "D")
#		     labels = c("(A) Single vessel and trip",
#			        "(B) Single vessel multiple trips",
#				"(C) Multiple vessels over a trip",
#				"(D) Realised distribution from step function"),
)


save_plot(mult_pl, file =  "../write_up/Plots/Combined_Movement.png",
	  ncol = 2, nrow = 3)

