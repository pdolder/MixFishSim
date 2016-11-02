#' @title Baranov F

#' @description \code{baranov_f} provides the function to solve in
#' \code{\link{find_f}} for estimating weekly fishing mortality from catch
#' (\emph{C}), biomass (\emph{B}) and natural mortality (\emph{M}). It's based
#' on the standard Baranov catch equation.
#'
#' @param C is a Numeric vector detailing the catch at \eqn{wk_{t}} 
#' @param B is a Numeric vector of the biomass at \eqn{wk_{t}} 
#' @param M is a Numeic vector of the natural mortality rate at \eqn{wk_{t}}
#' @param F is the fishing mortality rate to solve.

#' @return Returns nothing, is objective to be solved by \code{\link{find_f}}

#' @examples
#' ## No examples

#' @export

baranov_f <- function(F, C, B, M) {
Ca = (F/(F + M)) * (1 - exp(-(F + M))) * B
Ca - C
}

