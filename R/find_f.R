#' @title find F (fishing mortality)
#' 
#' @description \code{find_f} uses \code{\link[base]{uniroot}} to find the
#' fishing mortality rate given the catch, biomass and natural mortality using
#' the \code{\link{baranov_f}} objective function.

#' @param FUN is the objective function, here the Baranov equation
#' \link{baranov_f}
#' @param C is a Numeric vector detailing the catch at \eqn{wk_{t}} 
#' @param B is a Numeric vector of the biomass at \eqn{wk_{t}} 
#' @param M is a Numeic vector of the natural mortality rate at \eqn{wk_{t}}

#' @return Gives the fishing mortality estimate \emph{F}

#' @examples
#' find_f(C = 3000, B = 12000, M = 0.2, FUN = baranov_f)

#' @export

find_f <- function(C = C, B = B, M = M,FUN = baronov_f) {
return(uniroot(BaronovF,c(0,2),tol = 1E-16, C = C, B = B, M = M)$root)
}

