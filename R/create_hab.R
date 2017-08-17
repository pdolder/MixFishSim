#' @title Create habitat distribution fields
#' 
#' @description \code{create_hab} parametrises and returns the
#' spatial fields used for the distribution of suitable habitat for the
#' populations in the simulation. 
#'
#' The spatial fields are generated using
#' \code{\link[RandomFields]{RFsimulate}} function from the \emph{RandomFields}
#' package.
#'
#' @param npt Numeric integer with the dimensions of the field in
#' \emph{npt * npt}
#' @param n.spp Numeric integer with the number of species to be simulated.
#' Each species must have an individual control list as detailed below.
#' @param spp.ctrl List of controls to generate suitable habitat for each
#' species. Must be of the form spp.ctrl = list(spp.1 = c(var = 20, ...),
#' spp.2 = c(var = 10, ..),..) and contain the following:
#' \itemize{
#'	\item \strong{nu} (\emph{>=0})
#' 	\item \strong{var} (\emph{>=0}) Controls the range in a matern covariance
#' 	\item \strong{scale} (\emph{>=0})
#'	\item \strong{Aniso} (\emph{matrix, dim = c(2,2)})
#'}
#' @param plot.dist Boolean, whether to plot the distributions to file
#' @param plot.file path to save the plots of the species distributions

#' @return Silently returns a list of spatial distributions of suitable habitat
#' with first level of the list being the population (1 -> n.spp). If
#' \code{plot.dist = TRUE} it produces an image of the spatial distributions at
#' each time step for each of the populations saved to the working directory
#' (unless specified otherwise in \code{plot.file})

#' @examples
#' fields <- create_hab(n.spp = 1,  
#'	      spp.ctrl = list(
#'	      'spp.1' = list('nu' = 1/0.15, var = 1, scale = 10, Aniso =
#'	      matrix(nc=2, c(1.5, 3, -3, 4)))), plot.dist = TRUE, plot.file =
#'	      getwd())

#' @export

create_hab <- function (npt = 100, seed = 123, n.spp = NULL, 
			   spp.ctrl = NULL, plot.dist = FALSE, plot.file = getwd()) {

	RandomFields::RFoptions(spConform = FALSE) # faster and only returns the matrix of values
	set.seed(seed)

	# Checks
	if(is.null(n.spp)) stop('must specify the number of species to simulate')
	if(is.null(spp.ctrl)) stop('must specify the control parameters for the species simulations')

	for (i in 1:n.spp) {
	par  <- spp.ctrl[[paste0('spp.',i)]]

	# Check
	for (params in c('nu', 'var', 'scale', 'Aniso')) {
	if(!(params %in% names(par))) stop(paste0('spp.',i,' does not contain ',params))
	}

	if(!is.matrix(par$Aniso)) stop(paste0('Aniso must be a matrix of dim c(2,2)'))

	# Create the sim object
	hab_mod <- RandomFields::RMmatern(nu = par$nu, var = par$var, scale =
			    par$scale, Aniso = par$Aniso)
	assign(paste0('hab.','spp.',i), RandomFields::RFsimulate(model = hab_mod, x = 1:npt, y = 1:npt))


	# Normalise from 0 to 1 per time period
	x<- get(paste0('hab.','spp.',i))
	x[x < 0] <- 0 # Any negative to zeros
	assign(paste0('hab_','spp',i), x / sum(x)) # sum to 1

	}

	# Plot
	if(plot.dist == TRUE) {
	png(filename = paste0(plot.file,'/','habitat','.png'), width = 800, height = 800)
	par(mfrow = c(ceiling(sqrt(n.spp)), ceiling(n.spp/ceiling(sqrt(n.spp)))), mar = c(2, 2, 2, 2))
	for (i in seq(n.spp)) {
	image(get(paste0('hab_','spp',i)), cex.axis = 1.5, cex.main = 2, col = grey(seq(1,0,l = 51)), axes = F)
	axis(1, at = seq(0, 1, by = 0.2), labels = seq(0, npt, by = npt/5))
	axis(2, at = seq(0, 1, by = 0.2), labels = seq(0, npt, by = npt/5))
	text(0.5, 0.98, labels = paste('habitat spp =', i), cex = 2)
		}
	dev.off()
	}
	
	# Return the list invisibly
	fields <- mget(paste0('hab_','spp',1:n.spp))
	return(invisible(fields))
		
}
