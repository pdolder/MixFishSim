#' @title Create species distribution fields
#' 
#' @description \code{create_fields} parametrises and returns the
#' spatio-temporal fields used for the spatial distribution of fish populations
#' and movement in space and time for the simulations. 
#'
#' The spatio-temporal fields are generated using
#' \code{\link[spate]{spate.sim}} function from the \emph{spate} package
#' using an advective-diffusion Stochastic Partial Differential Equation
#' (SPDE). See \emph{Lindgren 2011 and Sigrist 2015} for further detail.
#'
#' @param npt Numeric integer with the dimensions of the field in
#' \emph{npt * npt}
#' @param t Numeric integer with the number of time-steps in the simulation
#' @param seed (Optional) Numeric integer with the seed for the simulation
#' @param n.spp Numeric integer with the number of species to be simulated.
#' Each species must have an individual control list as detailed below.
#' @param spp.ctrl List of controls to generate each species spatio-temporal
#' distribution. Must be of the form spp.ctrl = list(spp.1 = c(rho0 = 0.001, ...),
#' spp.2 = c(rho0 = 0.001, ..),..) and contain the following:
#' \itemize{
#' 	\item \strong{rho0} (\emph{>=0}) Controls the range in a matern covariance
#' 	structure.
#'	\item \strong{sigma2} (\emph{>=0}) Controls the marginal variance (i.e. process
#'	error) in the matern (\emph{>=0}) covariance structure.
#'	\item \strong{zeta} (\emph{>=0}) Damping parameter; regulates the temporal
#'	correlation.
#'	\item \strong{rho1} (\emph{>=0}) Range parameter for the diffusion process
#'	\item \strong{gamma} (\emph{>=0}) Controls the level of anisotropy 
#'	\item \strong{alpha} (\emph{[0, \eqn{\pi/2}]}) Controls the direction
#'	of anisotropy
#'	\item \strong{muX} (\emph{[-0.5, 0.5]}) x component of drift effect
#'	\item \strong{muY} (\emph{[-0.5, 0.5]}) y component of drift effect
#'	\item \strong{tau2} (\emph{>=0}) Nugget effect (measurement error)
#'	\item \strong{nu} Smoothness parameter for the matern covariance
#'	function
#'}
#' @param plot.dist Boolean, whether to plot the distributions to file
#' @param plot.file path to save the plots of the species distributions

#' @return Silently returns a list of spatial distributions with first level of
#' the list being the population (1 -> n.spp) and the second being time (1 ->
#' t). If \code{plot.dist = TRUE} it produces an image of the spatial
#' distributions at each time step for each of the populations saved to the
#' working directory (unless specified otherwise in \code{plot.file})

#' @examples
#' fields <- create_fields(n.spp = 1, t = 2, 
#'	      spp.ctrl = list(
#'	      'spp.1' = c('rho0' = 0.1, 'sigma2' = 1, 'zeta' = 0.1,
#'			  'rho1' = 0.01, 'gamma' = 0.3, 'alpha' = pi/4,
#'			  'muX' = -0.05, 'muY' = -0.05, 'tau2' = 0, 'nu' = 1.5)),
#'			plot.dist = TRUE, plot.file = getwd())

#' @export

create_fields <- function (npt = 1000, t = 1, seed = 123, n.spp = NULL, 
			   spp.ctrl = NULL, plot.dist = FALSE, plot.file = getwd()) {

	# Checks
	if(is.null(n.spp)) stop('must specify the number of species to simulate')
	if(is.null(spp.ctrl)) stop('must specify the control parameters for the species simulations')

	for (i in 1:n.spp) {
	par  <- spp.ctrl[[paste0('spp.',i)]]

	# Check
	for (params in c('rho0','sigma2','zeta','rho1','gamma','alpha','muX','muY','tau2','nu')) {
	if(!(params %in% names(par))) stop(paste0('spp.',i,' does not contain ',params))
	}

	# Create the sim object
	assign(paste0('spate.','spp.',i), spate::spate.sim(par = par[names(par) !='nu'], n =
						    npt, T = t, nu =
						    par[names(par)=='nu'], StartVal =
						    NULL, seed = seed))
	
	# Convert to list of matrices
	assign(paste0('spp',i), lapply(split(get(paste0('spate.','spp.',i))$xi,
					     seq(t)), function(x) matrix(x, ncol = npt, nrow = npt)))
					     

	# Normalise from 0 to 1 per time period
	assign(paste0('spp',i), lapply(get(paste0('spp',i)), function(x) {
					   x[x < 0] <- 0 # Any negative to zeros
					   x / max(x) }))

	# Plot
	if(plot.dist == TRUE) {
	png(filename = paste0(plot.file,'/','spp',i,'.png'), width = 800, height = 1200)
	par(mfrow = c(ceiling(sqrt(t)), ceiling(t/ceiling(sqrt(t)))), mar = c(2, 2, 2, 2))
	for (tm in seq(t)) {
	image(get(paste0('spp',i))[[tm]], cex.axis = 1.5, cex.main = 2, col = grey(seq(1,0,l = 51)), axes = F)
	axis(1, at = seq(0, 1, by = 0.2), labels = seq(0, npt, by = npt/5))
	axis(2, at = seq(0, 1, by = 0.2), labels = seq(0, npt, by = npt/5))
	text(0.5, 0.98, labels = paste('time =', tm), cex = 2)
		}
	dev.off()
	}
	
	}

	# Return the list invisibly
	fields <- mget(paste0('spp',1:n.spp))
	return(invisible(fields))
		
}
