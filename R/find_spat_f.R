#' @title find spatial Fs (fishing mortality rates)
#' 
#' @description \code{find_spat_f} uses \code{\link[base]{uniroot}} to find the
#' fishing mortality rate for a population given the catch, biomass and natural
#' mortality using the \code{\link{baranov_f}} objective function.

#' @param sim_init is the parameterised sim settings, made by \code{init_sim}
#' @param FUN is the objective function, here the Baranov equation
#' \link{baranov_f}
#' @param C is a Numeric vector detailing the catch at \eqn{wk_{t}} 
#' @param B is a Numeric vector of the biomass at \eqn{wk_{t}} 
#' @param M is a Numeic vector of the natural mortality rate at \eqn{wk_{t}}

#' @return Gives a matrix the spatial fishing mortality estimate \emph{F}

#' @examples
#' find_spat_f(sim_init = sim, C = matrix(1000,3000, nc =2), B =
#' matrix(12000,10000, ncol = 2), M = 0.2, FUN = baranov_f)

#' @export

find_spat_f  <- function(sim_init = NULL, C = C, B = B, M = M, FUN = baranov_f) {

	ncols <- sim_init[["idx"]][["ncols"]] 
	nrows <- sim_init[["idx"]][["nrows"]]

res <- sapply(seq(ncols * nrows), function(x) {
	       
	       uniroot(f = FUN, interval = c(0,1), tol = 1e-6, C = C[[x]], B =
		       B[[x]], M = M, extendInt = "yes")$root })

res <- matrix(res, ncol = ncols, nrow = nrows)
return(res)

}

