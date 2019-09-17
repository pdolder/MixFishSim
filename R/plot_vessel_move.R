#' @title Plot vessel move

#' @description \code{plot_vessel_move} is a plot of a single vessel movement
#' during one trip. It's intended for diagnostics.

#' @param logs is the combined log file, from \link{combine_logs}.

#' @param fleet_no is a Numeric, the fleet from which to plot
#' @param vessel_no is a Numeric, the vessel to plot from the chosen fleet
#' @param year_trip is a Numeric, the year in which the trip took place
#' @param trip_no is a Numeric for the trip you wish to plot
#' @param fleets_init is the output from \link{init_fleet}
#' @param pop_bios is the output from \link{run_sim} when option save_pops_bios
#' = TRUE

#' @examples
#' plot_vessel_move(sim_init = NULL, logs = logs, fleet_no = 1, vessel_no = 1, year_trip = 1,
#' trip_no = 1, fleets_init = NULL, pop_bios = NULL)

#' @export

plot_vessel_move <- function (sim_init = NULL, logs = logs, fleet_no = 1, vessel_no = 1, year_trip = 1, trip_no = 1, fleets_init = NULL, pop_bios = NULL) {

require(ggplot2)
require(dplyr)


log <- filter(as.data.frame(logs), fleet %in% fleet_no, vessel %in% vessel_no, year %in% year_trip, trip %in% trip_no)

log$trip <- as.factor(log$trip)

if(is.null(fleets_init) | is.null(pop_bios)) {

return(print(ggplot(log, aes(x = x, y = y, group = paste(vessel, trip))) +
      geom_point(aes(colour = trip)) +
      geom_path(aes(colour = trip))+ theme_bw() +
      expand_limits(y = c(0,sim_init[["idx"]][["ncols"]]), 
		    x = c(0,sim_init[["idx"]][["nrows"]])) + 
	     theme(legend.position = "none") + ylab("y distance") + 
	     xlab("x distance")
	     ))

}

if(!is.null(fleets_init) & !is.null(pop_bios)) {

## Extract pop biomasses
Bs <- pop_bios[[year_trip, trip_no]]

## Create value field for fleet

Value <- lapply(names(Bs), function(x) {
		     fleets_init[["fleet_params"]][[fleet_no]][["VPT"]][[x]] * 
			     fleets_init[["fleet_params"]][[fleet_no]][["Qs"]][[x]] *
			     Bs[[x]]
		     })

TotVal <- Reduce("+", Value)

TotValDF <- tidyr::gather(as.data.frame(TotVal), factor_key = TRUE)
TotValDF$x <- rep(seq_len(sim_init[["idx"]][["nrows"]]), times = sim_init[["idx"]][["nrows"]])
TotValDF$y <- rep(seq_len(sim_init[["idx"]][["ncols"]]), each = sim_init[["idx"]][["ncols"]])

return(print(ggplot(TotValDF, aes(x = x, y = y)) + geom_tile(aes(fill = value)) +
	scale_fill_gradient2(low = "blue", high = "darkblue") + 
	geom_point(data = log, aes(x = x, y = y, colour = tow)) + 
	geom_path(data = log, aes(x = x, y = y, colour = tow, group = paste(vessel, trip))) +
	theme_bw() + scale_colour_gradient(low = "red", high = "darkred") + 
	expand_limits(y = c(0,sim_init[["idx"]][["ncols"]]), 
		      x = c(0,sim_init[["idx"]][["nrows"]])) + 
       theme(legend.position = "none")+ ylab("y distance") + 
	     xlab("x distance")
))

}

}
