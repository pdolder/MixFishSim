#' @title Plot the habitat distribution fields for all populations
#'
#' @description \code{plot_habitat} is a function to plot the habitat spatial
#' fields.
#'
#' @param hab is the habitat to plot, either unadjusted or adjusted for
#' spawning area. 

#' @return Silently plots the habitat fields (can be either the unadjusted
#' habitat, or including the spawning adjustment).

#' @examples
#' plot_habitat(hab = hab)

#' @export

plot_habitat <- function(hab = hab) {

	n.spp <- length(hab)
	nrows <- dim(hab[[1]])[1]
	ncols <- dim(hab[[1]])[2]

	par(mfrow = c(ceiling(sqrt(n.spp)), ceiling(n.spp/ceiling(sqrt(n.spp)))), mar = c(2, 2, 2, 2))
	for(i in seq(n.spp)) {
	image(hab[[paste0("spp", i)]], cex.axis = 1.5, cex.main = 2, col = grey(seq(1,0, l = 51)), axes = F)
	axis(1, at = seq(0, 1, by = 0.2), labels = seq(0, nrows, by = nrows/5))
	axis(2, at = seq(0, 1, by = 0.2), labels = seq(0, ncols, by = ncols/5))
	text(0.5, 0.98, labels = paste("habitat spp = ", i), cex = 1)
	}
	
}


