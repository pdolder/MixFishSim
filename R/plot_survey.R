#' @title Plot the fisheries independent survey results

#' @description \code{plot_survey} plots the spatial abundances and an index
#' from the fisheries independent survey, for each population.

#' @param survey is the survey results from the \link{run_sim} function.
#' @param type is a character indicating if \emph{spatial} or \emph{index}

#' @return is a plot of the spatial distribution of survey catches and an
#' inter-annual abundance index

#' @examples
#' plot_survey(survey = survey, type = "spatial")

#' @export

plot_survey <- function (survey = NULL, type = "spatial") {
	require(ggplot2)
	require(dplyr)
	require(reshape2)

	# spatial catches
	log.mat  <- as.data.frame(survey[["log.mat"]])


	if(type == "spatial") {
	log.spat <- melt(log.mat, id = c("station_no", "x", "y", "day", "tow", "year"))

	print(ggplot(log.spat, aes(x = x, y = y, size = value)) +
		geom_point(shape = 21, aes(fill = value)) +
		facet_grid(variable ~ year))

	}

	if(type == "index") {
	log.idx <- melt(log.mat, id = c("station_no", "x", "y", "day", "tow", "year"))

log.idx <- 	log.idx %>% group_by(year, variable) %>% summarise(mean= mean(value), se = (sd(value, na.rm = T) / sqrt(n())))

	print(ggplot(log.idx, aes(x = factor(year), y = mean, group = variable)) + geom_point() + geom_line() + geom_linerange(aes(ymin = mean - 1.96 * se, ymax = mean + 1.96 * se)) + facet_wrap(~variable) +
	      xlab("") + ylab("abundance index"))

	}
	
}
