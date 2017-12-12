#' @title find spatial f pops

#' @description \code{find_spat_f_pops} applies the \code{find_spat_f} function
#' to all the populations, returning the spatial fishing mortality rates for
#' each of the populations.

#' @param FUN is the \code{find_spat_f} function
#' @param sim_init is the simulation settings initialised by \code{init_sim}
#' @param C is the spatial catch matrices for all populations
#' @param B is the spatial biomass for all populations
#' @param dem_params are the demographic parameters for all populations (containing
#' the natural mortality rate, M.

#' @examples None as yet

#' @export

find_spat_f_pops <- function (FUN = find_spat_f, sim_init = sim, C = C, B = B, dem_params = NULL, ...) {

	n.spp <- sim_init[["idx"]][["n.spp"]]

#	out <- lapply(seq(n.spp), function(x) { res <- find_spat_f(sim_init =
#								   sim_init, B
#							   = B[[x]], C =
#								   C[[x]], M =
#								   dem_params[[x]][["M"]],
#							   FUN = baranov_f) })
	
	## In parallel

out <- foreach(x = seq_len(n.spp)) %dopar%  {
			       find_spat_f(sim_init = sim_init, B = B[[x]], C =
					   C[[x]], M = dem_params[[x]][["M"]]/365,
				   FUN = baranov_f) }

	names(out)  <- paste("spp", seq_len(n.spp), sep = "")

	return(out)
	
}
