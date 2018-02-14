#' @title Initialise populations
#'
#' @description \code{init_pop} sets up the populations spatial distribution
#' based on the habitat preference, starting cell and 'n' numbers of movements
#' for all populations in the simulation.
#'
#' @param Bio is a named Numeric vector of the starting (total) biomass for each of the
#' populations.
#' @param hab is the list of Matrices with the habitat preferences created by \code{create_hab}
#' @param spawn_areas is a list of lists, with the first level the population
#' ("spp1" etc..) and the second the boundary coordinates (x1, x2, y1, y2) for
#' the \code{create_spawn_hab} function
#' @param start_cell is a list of Numeric vectors with the starting cells for
#' the populations
#' @param lambda is the strength that the movement distance decays at in the
#' \code{move_prob} function
#' @param init_move_steps is a Numeric indicating the number of movements to
#' initialise for the population distributions
#' @param rec_params is a list with an element for each population, containing
#' a vector of the stock recruit parameters which must contain \strong{model},
#' \strong{a}, \strong{b} and \strong{cv}. See \code{Recr} for details.
#' @param rec_wk is a list with an element for each population, containing a
#' vector of the weeks in which recruitment takes place for the population
#' @param spwn_wk is a list with an element for each population, containing a
#' vector of the weeks in which spawning takes place for the population
#' @param M is a named vector, with the annual natural mortality rate for each
#' population
#' @param K is a named vector, with the annual growth rate for each population
#'
#' @return The function returns the recording vectors at the population level,
#' the spatial matrices for the starting population densities and the
#' demographic parameters for each population

#' @examples init_pop(sim_init = sim_init, Bio = c("spp1" = 1e6, "spp2" = 2e5), hab = list(spp1 = matrix(nc = 10,
#' runif(10*10)), spp2 = matrix(nc = 10, runif(10*10)), lambda = c("spp1" =
#' 0.2, "spp2" = 0.3), init_move_steps = 10), rec_params = list("spp1" =
#' c("model" = "BH", "a" = 10, "b" = 50, "cv" = 0.2), "spp2" = c("model" = "BH",
#' "a" = 1, "b" = 8, "cv" = 0.2)), rec_wk = list("spp1" = 13:16, "spp2" =
#' 13:18), spwn_wk = list("spp1" = 15:18, "spp2" = 18:20),M = c("spp1" = 0.2,
#' "spp2" = 0.1), K = c("spp1" = 0.3, "spp2" = 0.2))
#' Note, example will not have the right biomass

#' @export

init_pop <- function(sim_init = sim_init, Bio = NULL, hab = NULL, start_cell = NULL, lambda = NULL, init_move_steps = 10, rec_params = NULL, rec_wk = NULL, spwn_wk = NULL, M = NULL, K = NULL, cores = 3) {

suppressMessages(require(doParallel))
registerDoParallel(cores = cores)

# extract the indices
idx <- sim_init[["idx"]]
brk.idx <- sim_init[["brk.idx"]]
max.day <- max(brk.idx[["day.seq"]])


# set up population matrices
	# Apply over all populations, returning a list
Pop <- foreach(x = names(Bio)) %dopar% {

		      ## Initial distribution
		      PopIn <- matrix(nc = ncol(hab[[x]]), nr = nrow(hab[[x]]), 0)
		      PopIn[start_cell[1], start_cell[2]] <- Bio[[x]] 

		      # Move the population around a bit
		      # 1. Move probabilities

		      MoveProp <- move_prob_Lst(lambda = lambda[[x]], hab = hab[[x]])

		      # 2. Apply move n times

		      for (i in seq(init_move_steps)) {
		      PopIn <- move_population(moveProp = MoveProp, StartPop = PopIn)
		      PopIn <- Reduce("+", PopIn)
		      }

		      # Return the starting population
		      return(PopIn)

}

names(Pop) <- paste("spp",seq(idx[["n.spp"]]), sep ="")

## Set up the population level recording vectors

Pop_vec <- foreach(x = seq_len(idx[["n.spp"]])) %dopar% {

Pop_vec <- list( 
	# Pop level biomass
	Bio.mat = matrix(NA, nrow = idx["ny"], ncol = max.day, dimnames =
			  list(seq(idx["ny"]), seq(1,max(brk.idx[["day.breaks"]])) )),
	# Pop level Fs
	F.mat = matrix(NA, nrow = idx["ny"], ncol = max.day, dimnames =
			list(seq(idx["ny"]), seq(1,max(brk.idx[["day.breaks"]])) )),

	# Pop level catches
	Catch.mat = matrix(NA, nrow = idx["ny"], ncol = max.day, dimnames =
			    list(seq(idx["ny"]), seq(1,max(brk.idx[["day.breaks"]])) )),
	
	# Pop level recruitment
	Rec.mat = matrix(NA,nrow= 1,ncol = idx["ny"]+1,dimnames=list(1, 0:idx["ny"]))

	)

return(Pop_vec)

}

names(Pop_vec) <- paste("spp",seq(idx[["n.spp"]]), sep ="")

## Sets up the stock-recruitment parameters

dem_params <- lapply(names(Bio), function(x) {

dem_params = list(rec_params = rec_params[[x]], rec_wk = rec_wk[[x]], spwn_wk = spwn_wk[[x]], M = M[[x]], K = K[[x]])

return(dem_params)
		  
	})

names(dem_params) <- names(Bio)



# Return the recording vectors for the populations and the matrix of starting
# pop locations
return(list(Pop_record = Pop_vec, Start_pop = Pop, dem_params = dem_params ))


}
