#' @title Plot an entire fleet for a trip 

#' @description \code{plot_fleet_trip} is a plot of a whole fleets vessels movement
#' during one trip. It's intended for diagnostics.

#' @param logs is the combined log file, from \link{combine_logs}.

#' @param fleet_no is a Numeric, the fleet from which to plot
#' @param year_trip is a Numeric, the year in which the trip took place
#' @param trip_no is a Numeric for the trip you wish to plot

#' @examples
#' plot_fleet_trip(logs = logs, fleet_no = 1, year_trip = 1, trip_no = 1,
#' pop_bios = NULL, fleets_init = NULL)

#' @export

plot_fleet_trip <- function (logs = logs, fleet_no = 1, year_trip = 1, trip_no = 1,
			 pop_bios = NULL, fleets_init = NULL, sim_init = NULL) {

require(ggplot2)
require(dplyr)

log <- filter(as.data.frame(logs), fleet %in% fleet_no, year %in% year_trip, trip %in% trip_no)

log$vessel <-as.factor(log$vessel)

## We need to identify trips that are jumps across the taurus
log$move_type[2:nrow(log)] <- ifelse(!is.na(log$stepD[1:(nrow(log)-1)]), "CRW", "Experience")
log$move_type[2:nrow(log)] <- ifelse(log$move_type[2:nrow(log)] == "CRW" & log$stepD[1:(nrow(log)-1)] > 10, "OutsideMove", log$move_type[2:nrow(log)])



if(is.null(pop_bios)) {
print(ggplot(log, aes(x = x, y = y)) + 
      geom_point(aes(colour = vessel)) +
      geom_path(data = filter(log, move_type == "CRW"), aes(colour = vessel)) +
      geom_path(data = filter(log, move_type == "Experience"), aes(colour = vessel)) +
      geom_path(data = filter(log, move_type == "OutsideMove"), aes(colour = vessel)) +
      theme_bw() + xlab("x distance") + ylab("y distance")
      )
}


if(!is.null(pop_bios)) {

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


print(ggplot(TotValDF, aes(x = x, y = y)) + geom_tile(aes(fill = value)) +
	scale_fill_gradient2(low = "blue", high = "darkblue") +
	geom_point(data = log, aes(colour = vessel)) +
	geom_path(data = filter(log, move_type == "CRW"), aes(colour = vessel), linetype = "solid") +
      	geom_path(data = filter(log, move_type == "Experience"), aes(colour = vessel), linetype = "longdash") +
      	geom_path(data = filter(log, move_type == "OutsideMove"), aes(colour = vessel), linetype = "twodash") +
	theme_bw() + #scale_colour_gradient(low = "red", high = "darkred") + 
	expand_limits(y = c(0,sim_init[["idx"]][["ncols"]]), 
		      x = c(0,sim_init[["idx"]][["nrows"]])) +
      theme(legend.position = "none") +
      xlab("x distance") + ylab("y distance")

	
	)

}

}


