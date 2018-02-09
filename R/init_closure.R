#' @title Initialise spatial closure(s)
#'
#' @description \code{init_closure} sets up the parameters for spatial
#' closure(s) in the simulation.
#'
#' @param input_coords is a list of coordinates defining the closure(s). If the
#' temp_dyn are not static, the list should be multilayered with the
#' [[week/month]][[coords]]
#' @param basis is a character string detailing the data used to define a
#' closure 'on the fly'. Can be \emph{survey} to be based on survey data,
#' \emph{commercial} to be based on commercial data, \emph{real_pop} to be
#' based on the simulated population. Not needed if coordinates defined.
#' @param rationale is the basis for any 'on the fly' closure. Can be
#' \emph{high_pop} for the areas of a highest population or \emph{high_ratio}
#' for the areas of the highest ratio of population 1: population 2. Not needed
#' if coordinates defined.
#' @param spp1 is the first population as basis for the closure. If rationale =
#' high_pop then that should go here If rationale = high_ratio, its the target
#' (high quota) population. Not needed if coordinates are defined.
#' @param spp2 is the second population when rationale = high_ratio, the lowest
#' quota population. Not needed if coordinates provided or rationale =
#' high_pop.
#' @param year_start is a Numeric indicating the first year the spatial
#' closure(s) shoud be implemented.
#' @param closure_thresh is the quantile of catches or high catch ratio which
#' determines closed cells
#' @param temp_dyn is a character string detailing whether closures should
#' be temporally 'annual', or change 'monthly' or 'weekly'. 

#' @return is a list of parameter settings for the spatial closures which
#' serves as an input to \link{run_sim}.

#' @examples Not as yet

#' @export

init_closure <- function (input_coords = NULL, basis = 'commercial', rationale = 'high_pop', spp1 = 'spp1', spp2, year_start = 1, closure_thresh = 0.95, temp_dyn = 'annual') {

	input_coords <- input_coords

	temp_dyn     <- temp_dyn

	if(!temp_dyn == 'annual') {
	if(temp_dyn == "weekly" & length(input_coords) != 52 & !is.null(input_coords)) stop("If the temp_dyn are weekly, input coords must be of length 52")
	
	if(temp_dyn == "monthly" & length(input_coords) != 12 & !is.null(input_coords)) stop("If the temp_dyn are monthly, input coords must be of length 12")
	}

	basis     <- basis
	if(!basis %in% c("survey", "commercial", "real_pop")) stop("The basis must be survey, commercial or real_pop") 

	rationale <- rationale
	if(!rationale %in% c("high_pop", "high_ratio")) stop("The rationale must be either high_pop or high_ratio")

	spp1 <- spp1
	spp2 <- spp2

	if(rationale == "high_pop" & is.null(spp1)) stop("If the rationale is high_ratio, you need to define spp1")

	if(rationale == "high_ratio" & is.null(spp1) | is.null(spp2)) stop("If the rationale is high_ratio, you need to define spp1 and spp2")

	if(basis == "survey" & temp_dyn %in% c("weekly", "monthly")) stop("The survey only takes place once a year, so temp_dyn has to be annual")

	year_start     <- year_start
	closure_thresh <- closure_thresh

	return(list(input_coords = input_coords, basis = basis, rationale = rationale, spp1 = spp1, spp2 = spp2, year_start = year_start, closure_thresh = closure_thresh, temp_dyn = temp_dyn)) 
	
}
