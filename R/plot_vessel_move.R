#' @title Plot vessel move

#' @description \code{plot_vessel_move} is a plot of a single vessel movement
#' during one trip. It's intended for diagnostics.

#' @param logs is the combined log file, from \link{combine_logs}.

#' @param fleet_no is a Numeric, the fleet from which to plot
#' @param vessel_no is a Numeric, the vessel to plot from the chosen fleet
#' @param year_trip is a Numeric, the year in which the trip took place
#' @param trip_no is a Numeric for the trip you wish to plot

#' @examples
#' plot_vessel_move(logs = logs, fleet_no = 1, vessel_no = 1, year_trip = 1, trip_no = 1)

#' @export

plot_vessel_move <- function (logs = logs, fleet_no = 1, vessel_no = 1, year_trip = 1, trip_no = 1) {

require(ggplot2)
require(dplyr)

log <- filter(as.data.frame(logs), fleet == fleet_no, vessel == vessel_no, year == year_trip, trip == trip_no)

print(ggplot(log, aes(x = x, y = y)) + geom_point(aes(colour = tow)) + geom_path(aes(colour = tow)))

}
