#' @title Plot realised step function
#'
#' @description \code{plot_realised_stepF} diagnostics plot of the step function shape realised in the
#' simulation

#' @param logs is the log file from \link{combine_logs}
#' @param fleet_no is a Numeric of the fleet to plot
#' @param vessel_no is a Numeric of the vessel to plot

#' @examples
#' plot_realised_stepF(logs = logs, fleet_no = 1, vessel_no = 1)

#' @export

plot_realised_stepF <- function(logs = logs, fleet_no = 1, vessel_no = 1) {

	stepDF <- dplyr::filter(as.data.frame(logs), fleet == fleet_no, vessel == vessel_no)

	par(mfrow=c(1,2))

	plot(stepDF[["val"]][1:c(nrow(stepDF)-1)], stepDF[["stepD"]][2:nrow(stepDF)], main = "Realised step distances", xlab = "value", ylab = "step distance")


	plot(stepDF[["val"]][1:c(nrow(stepDF)-1)],
     stepDF[["angles"]][1:c(nrow(stepDF)-1)]- 
     stepDF[["angles"]][2:nrow(stepDF)], main = "Relalised turning angles", xlab = "value", ylab = "change in angle")

}
