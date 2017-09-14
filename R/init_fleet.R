#' @title Initialise fleet
#'
#' @description \code{init_fleet} sets up the parameters and results data frame
#' to record the catches from the simulation.
#'
#' @param sim_init is the output (a list) from the \code{sim_init} function with the
#' indexing for the simulation.
#' @param n_fleets is an integer of the number of fleets in the model
#' @param n_vessels is an integer of the number of vessels in each fleet
#' @param VPT is a named vector of numerics detailing the value-per-tonne for catches
#' from each of the species (same for all fleets)
#' @param Qs is a list (an element for each fleet) with each element containing
#' a named vector with the catchability parameters for each species the vessels
#' in the fleet
#' @param step_params is a list (an element for each fleet) with each element
#' containing a named vector with the step parameters used in
#' \code{step_length}. This must include the named elements \strong{rate},
#' \strong{B1}, \strong{B2}, \strong{B3}.
#' @param past_knowledge is a Boolean (TRUE / FALSE) whether past knowledge
#' should determine fishing location (only after the first year)
#' @param past_year_month is a Boolean (TRUE / FALSE) that indicates whether
#' the same month in previous years should be included in the past knowledge
#' decision
#' @param past_trip is a Boolean (TRUE / FALSE) that indicates whether the past
#' trip undertaken should be included in the past knowledge decision
#' @param knowledge_threshold is a numeric (0 - 1) detailing the threshold at
#' which a fishing tow should be considered "good" and included in the
#' selection of possible choices of starting fishing locations in future tows.
#'
#' @return is a list with three elements containing i) the fleet parameters,
#' a named list \strong{fleet_params}, ii) the fleet catches,
#' \strong{catches_list}, which is a list of a list. For
#' the\strong{catches_list} the first element denotes the fleet number, the
#' second element is the vessel number with a dataframe for recording the
#' vessels catches. Finally, iii) is the spatial catches for the fleets, which
#' is a list (fleet) containing a list (vessels) containing a list (population)
#' - which is to be passed to the delay difference model.

#' @examples
#' None yet, to add

#' @export

init_fleet <- function(sim_init = NULL, n_fleets = 1, n_vessels = 1, VPT =
		       NULL, Qs = NULL, step_params = NULL, past_knowledge =
		       FALSE, past_year_month = FALSE, past_trip = FALSE,
	       threshold = NULL) {

	# useful indexes
	idx     <- sim_init$idx
	brk.idx <- sim_init$brk.idx

	## Set up parameters list
	params_lst <- lapply(seq(n_fleets), function(x) {

	params <- list(VPT = VPT, Qs = Qs[[x]], step_params = step_params[[x]], 
		       past_knowledge = past_knowledge, past_year_month = past_year_month,
		       past_trip = past_trip, threshold = threshold)
	return(params)
})


	##########################
	##########################
	## set up catch matrix
	##########################
	##########################

	catch.mat <- matrix(NA, nrow = idx[["ntow"]], ncol = 13 + idx[["n.spp"]])
	colnames(catch.mat)  <- c("x","y","stepD","angles","day","tow","trip","month","year",
				  paste0("spp",seq(idx[["n.spp"]])),"allspp","val","meanval","sdval")

	catch.mat[,'day']     <- brk.idx$day.breaks
	catch.mat[,'tow']     <- seq(idx[["ntow"]])
	catch.mat[,'trip']    <- brk.idx[["trip.breaks"]]
	catch.mat[,'month']   <- brk.idx[["month.breaks"]]
	catch.mat[,'year']    <- brk.idx[["year.breaks"]]

	# Return a list of fleets with (as a list): the parameters, a results
	# DF, spatial catches

	# results a list for fleets (separate item) with a DF covering all vessels in a fleet
	catches_lst <- lapply(seq(n_fleets), function(f) {
	catch.fl  <- lapply(seq(n_vessels), function(v) {
				    catch.vess <- catch.mat
				    return(catch.vess)
		       })
	return(catch.fl)
				  })

	## matrix of the catches, to be stored for the delay-difference
	## projections
	
	sp_catches_lst <- lapply(seq(n_fleets), function(f) {
	SpCatch.fl  <- lapply(seq(n_vessels), function(v) {

	catch_matrix <- lapply(seq(idx[["n.spp"]]), function(.) {
			       matrix(0, ncol = idx[["ncols"]], nrow = idx[["nrows"]])
	    })

	names(catch_matrix) <- paste("spp", seq(idx[["n.spp"]]), sep = "")
					    return(catch_matrix)
		       })
	return(SpCatch.fl)
				  })


	out_lst <- list(fleet_params = params_lst, 
			fleet_catches = catches_lst, 
			sp_fleet_catches = sp_catches_lst)
	return(out_lst)

}
