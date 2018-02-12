#' @title Initialise movement covariates
#'
#' @description This function creates a list of covariates, to be used 

#' @param sim_init is the output from the function \link{init_sim}.
#' @param steps is a Numeric with the number of timesteps over which the
#' covariate changes
#' @param spp_assoc is a named list of whether species are positively
#' associated (+1) or negatively associated (-1) with the covariate.

#' @examples None

#' @export

init_moveCov <- function (sim_init = NULL, steps = 52, spp_assoc = NULL) {	

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


	return(list(spp_assoc = spp_assoc, cov.matrix = cov.matrix))

}
