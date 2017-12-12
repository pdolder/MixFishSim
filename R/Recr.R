#' @title Recruitment function
#'
#' @description \code{Recr} returns a biomass of recruited fish to the
#' population based on a stock-recruit relationship and some measure of
#' variation.

#' @param model is a character detailing the recruitment function to use
#' (currently 'BH' for Beverton and Holt or 'Ricker' for a Ricker stock-recruit
#' relationship.
#' @param params is a Numeric vector of length 2, containing labelled \emph{a} and
#' \emph{b} parameters for the stock-recruit function. 

#' For Beverton and Holt \emph{a} refers to the maximum recruitment rate in
#' biomass, \emph{b} refers to the Spawning Stock Biomass (SSB) required to
#' produce half the maximum.

#' For Ricker \emph{a} refers to the maximum productivity per spawner and
#' \emph{b} the density dependent reduction in productivity as the stock
#' increases.
#' 
#' @param B is a Numeric vector containing the SSB of the adult population from
#' which the recruitment derives. 
#' @param cv is a Numeric vector containing the coefficient of variation in the
#' recruitment function.

#' @return returns the recruitment to the population in biomass.

#' @examples
#' Recr(model = 'BH', params = c("a" = 2000, "b" = 200), B = 1000, cv = 0.1)

#' @export
Recr <- function(model, params, B, cv,..) {

if(model=="BH" & any(!names(params) %in% c("a","b"))) stop("Beverton-Holt must have 'a' and 'b' params")
# a = max recruitment rate in B
# b = SSB to produce half the max

if(model=="Ricker" & any(!names(params) %in% c("a","b"))) stop("Ricker must have 'a' and 'b' params")
# a = max productivity per spawner
# b = density dependent reduction in productivity as stock increases

a <- params[["a"]]
b <- params[["b"]]
B <- B

if(model=="BH") {
	Recr1  <- (a*B)/(b+B)
	Recr2  <- rnorm(1, mean = Recr1, sd = cv * Recr1)
}

if(model=="Ricker") {
	Recr1  <- B*exp(a - b * B)
	Recr2  <- rnorm(1, mean = Recr1, sd = cv * Recr1)
}

return(Recr2)

}

