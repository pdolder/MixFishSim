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
#' @param sim is the parameter settings for the simulation, made by
#' \code{init_sim} function.
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
#' hab <- create_hab(sim.init = sim.init, spp.ctrl = list(
#'	      'spp.1' = list('nu' = 1/0.15, var = 1, scale = 10, Aniso =
#'	      matrix(nc=2, c(1.5, 3, -3, 4)))), spawn_areas = list("spp1" =
#'	      list("area1" = c(2,4,6,8))), list("spp2" = list("area1" =
#'	      c(0,10,23,35))), spwn_mult = 10, plot.dist = TRUE, plot.file =   getwd())

#' @export

create_hab <- function (sim_init = sim, seed = 123, spp.ctrl = NULL, spawn_areas = NULL, spwn_mult = 10, plot.dist = FALSE, plot.file = getwd(), cores = 3) {

	require(doParallel)
	registerDoParallel(cores = cores)


	# Extract indices
	idx <- sim_init[["idx"]]
	n.spp <- idx[["n.spp"]]
	ncols <- idx[["ncols"]]
	nrows <- idx[["nrows"]]

	RandomFields::RFoptions(spConform = FALSE) # faster and only returns the matrix of values
	set.seed(seed)

	# Checks
	if(is.null(n.spp)) stop('must specify the number of species to simulate')
	if(is.null(spp.ctrl)) stop('must specify the control parameters for the species simulations')

hab <- 	foreach(i = seq_len(n.spp)) %dopar% {
	par  <- spp.ctrl[[paste0('spp.',i)]]

	# Check
	for (params in c('nu', 'var', 'scale', 'Aniso')) {
	if(!(params %in% names(par))) stop(paste0('spp.',i,' does not contain ',params))
	}

	if(!is.matrix(par$Aniso)) stop(paste0('Aniso must be a matrix of dim c(2,2)'))

	# Create the sim object
	hab_mod <- RandomFields::RMmatern(nu = par$nu, var = par$var, scale =
			    par$scale, Aniso = par$Aniso)
	assign(paste0('hab.','spp.',i), RandomFields::RFsimulate(model = hab_mod, x = seq(nrows), y = seq(ncols)))


	# Normalise from 0 to 1 per time period
	x<- get(paste0('hab.','spp.',i))
	x[x < 0] <- 0 # Any negative to zeros
	assign(paste0('spp',i), x / sum(x)) # sum to 1
	return(get(paste0('spp',i)))

	}

	names(hab) <- paste0("spp", seq_len(n.spp))

	# Plot
	if(plot.dist == TRUE) {
	png(filename = paste0(plot.file,'/','habitat','.png'), width = 800, height = 800)
	par(mfrow = c(ceiling(sqrt(n.spp)), ceiling(n.spp/ceiling(sqrt(n.spp)))), mar = c(2, 2, 2, 2))
	for (i in seq(n.spp)) {
	image(hab[[paste0('spp',i)]], cex.axis = 1.5, cex.main = 2, col = grey(seq(1,0,l = 51)), axes = F)
	axis(1, at = seq(0, 1, by = 0.2), labels = seq(0, nrows, by = nrows/5))
	axis(2, at = seq(0, 1, by = 0.2), labels = seq(0, ncols, by = ncols/5))
	text(0.5, 0.98, labels = paste('habitat spp =', i), cex = 2)
		}
	dev.off()
	}

#	hab <- mget(paste0('spp',seq(n.spp)))


# Now the spawning habitat
# Define the spawning habitat preferences

	if(!is.null(spawn_areas)) {

	spwn_hab <- foreach(x = paste0("spp",seq_len(n.spp))) %dopar% {
		create_spawn_hab(hab = hab[[x]], spwnareas = spawn_areas[[x]], mult = spwn_mult)
	}
	names(spwn_hab) <- paste0("spp", seq_len(n.spp))

# create a matrix of 0.5s with right dims
spwn <- matrix(rep(0.5, nrows * ncols), nc = ncols)

	spwn_loc <- foreach(x = names(spwn_hab)) %dopar% {
	res <- define_spawn(coord = spawn_areas[[x]], spwn = spwn, mult = 2)
	res[res==0.5] <- 0 # zeros for non-spawning areas
	return(res)
	
	}

	names(spwn_loc) <- paste0("spp", seq_len(n.spp))


	}

	if(is.null(spawn_areas)) {
	spwn_hab <- NULL
	spwn_loc <- NULL
	}


	habitat_lst <- list(hab = hab, spwn_hab = spwn_hab, spwn_loc = spwn_loc, spawn_areas = spawn_areas)
	# Return the list invisibly
	return(habitat_lst)
		
}
