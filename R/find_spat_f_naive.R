#' @title find spatial Fs (fishing mortality rates), naive methods of C/B for speed
#' 
#' @description \code{find_spat_f} uses \code{\link[base]{uniroot}} to find the
#' fishing mortality rate for a population given the catch, biomass and natural
#' mortality using the \code{\link{baranov_f}} objective function.

#' @param C is a Numeric vector detailing the catch at \eqn{wk_{t}} 
#' @param B is a Numeric vector of the biomass at \eqn{wk_{t}} 

#' @return Gives a matrix the spatial fishing mortality estimate \emph{F}

#' @examples
#' find_spat_f_naive(C = matrix(1000,3000, nc =2), B =
#' matrix(12000,10000, ncol = 2))

#' @export

find_spat_f_naive  <- function(C = C, B = B) {
res <- C / B
res[is.na(res)] <- 0
return(res)

}

