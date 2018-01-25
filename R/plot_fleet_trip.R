#' @title Plot an entire fleet for a trip 

#' @description \code{plot_fleet_trip} is a plot of a whole fleets vessels movement
#' during one trip. It's intended for diagnostics.

#' @param logs is the combined log file, from \link{combine_logs}.

#' @param fleet_no is a Numeric, the fleet from which to plot
#' @param year_trip is a Numeric, the year in which the trip took place
#' @param trip_no is a Numeric for the trip you wish to plot

#' @examples
#' plot_fleet_trip(logs = logs, fleet_no = 1, year_trip = 1, trip_no = 1)

#' @export

plot_fleet_trip <- function (logs = logs, fleet_no = 1, year_trip = 1, trip_no = 1) {

require(ggplot2)
require(dplyr)

log <- filter(as.data.frame(logs), fleet %in% fleet_no, year %in% year_trip, trip %in% trip_no)

print(ggplot(log, aes(x = x, y = y)) + geom_point(aes(colour = factor(vessel))) + geom_path(aes(colour = factor(vessel))))

}
