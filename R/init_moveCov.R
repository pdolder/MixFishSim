#' @title Initialise movement covariates
#'
#' @description This function creates a list of covariates, to be used 

#' @param sim_init is the output from the function \link{init_sim}.
#' @param steps is a Numeric with the number of timesteps over which the
#' covariate changes
#' @param spp_tol is a named list (each species) with a list of mean (mu) and
#' variance (va) for the normal distribution for thermal tolerance.

#' @examples None

#' @export

init_moveCov <- function (sim_init = NULL, steps = 52, spp_tol = NULL) {	

	## Create list of matrices to capture spatio-temporal move covariates

	nx <- sim_init[["idx"]][["ncols"]]
	ny <- sim_init[["idx"]][["nrows"]]

	cov.matrix <- list()

	for(i in 1:steps) {

		if(i == 1) {
	temp <-matrix(NA,nrow=nx, ncol=ny)
	temp <-row(temp) + col(temp)  ## From South-West to North-East
	covariate.trend<- (temp-1)/10 #define cost values
	covariate.trend <- matrix(covariate.trend, nx , ny) #assign costs 
	cov.matrix[[i]] <- covariate.trend
	covariate.trend_base <- covariate.trend
		}

		if(i > 1) {
			covariate.trend <- ifelse(row(temp) + col(temp) < ((nx + ny) / 2), 
						  covariate.trend_base + (10 * (i/steps)), 
						  covariate.trend_base - (10 * (i/steps)))
			cov.matrix[[i]] <- covariate.trend
			}
}


	return(list(cov.matrix = cov.matrix, spp_tol = spp_tol))

}
