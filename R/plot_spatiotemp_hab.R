#' @title Plot spatiotemporal habitat suitability

#' @description Function to plot out the habitat suitability, as adjusted by
#' the spatiotemporal move covariates

#' @param hab is the output from \link{create_hab}
#' @param moveCov is the output from \link{init_moveCov}
#' @param plot.file path to save the plots of the spatiotemporal habitats 
#' @param spwn_wk is a named list of the spawning week for each population

#' @examples None

#' @export

plot_spatiotemp_hab <- function(hab = NULL, moveCov = NULL, plot.file = NULL, spwn_wk = NULL) {

	nrows <- nrow(hab[["hab"]][[1]]) 
	ncols <- ncol(hab[["hab"]][[1]])


for(s in seq_len(length(hab[["hab"]]))) {

	nt <- length(moveCov[["cov.matrix"]])
	if(!is.null(plot.file)) {
	png(filename = paste0(plot.file,'/','habitat_spatiotemp_spp_',s,'.png'), width = 800, height = 800)
	}
	par(mfrow = c(ceiling(sqrt(nt)), ceiling(nt/ceiling(sqrt(nt)))), mar = c(1, 1, 1, 1))

	for(i in seq_len(nt)) {

	move_cov_wk <- moveCov[["cov.matrix"]][[i]]

	move_cov_wk_spp <- matrix(nc = ncols,
				 nr = nrows, 
				 sapply(move_cov_wk, norm_fun, 
                          mu = moveCov[["spp_tol"]][[s]][["mu"]], 
		  	  va = moveCov[["spp_tol"]][[s]][["va"]]))

		if(!i %in% spwn_wk[[s]]) {
		image(hab[["hab"]][[paste0('spp',s)]] * move_cov_wk_spp, cex.axis = 1.5, cex.main = 2, col = grey(seq(1,0,l = 51)), axes = F)
		}
	
		if(i %in% spwn_wk[[s]]) {
		image(hab[["spwn_hab"]][[paste0('spp',s)]] * move_cov_wk_spp, cex.axis = 1.5, cex.main = 1, col = grey(seq(1,0,l = 51)), axes = F)
		}
	axis(1, at = seq(0, 1, by = 0.2), labels = seq(0, nrows, by = nrows/5))
	axis(2, at = seq(0, 1, by = 0.2), labels = seq(0, ncols, by = ncols/5))
	text(0.5, 0.98, labels = paste('week', i), cex = 1)

	}
	dev.off()
}
	
	
}
