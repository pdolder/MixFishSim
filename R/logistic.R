#' @title Logistic probability

#' @description \code{logistic} is a helper function to generate a logistic
#' curve to transition through the fishery stages (exploratory > transition >
#' established). Where ~0 is exploratory fishing and ~1 is established on past
#' knowledge. 
#' Only Q (can be set at tmax/100) and t (current tow) need to be supplied.

#' @param A is the lower asymptote, set at 0
#' @param K is the upper asymptote, set at 0.95 (to keep some few exploratory
#' tows, even when established)
#' @param B is the growth rate, e.g· 0.001
#' @param v affects bear where asymptote maximum growth occurs, set at 1
#' @param Q defines the lower curve, related to Y at 0, usefully set at
#' tmax/100
#' @param C = 1

#' @examples  NOT RUN
#tmax <- 10000
#ts   <- seq(1,tmax,1) 
#plot(ts, sapply(ts, function(x) {logistic(Q = tmax / 100, t = x)}))

#' @export

logistic <- function(A = 0, K = 0.95, C = 1, Q = NULL, B = 0.001, v = 1, t = NULL) {

	return(A + ((K-A)/(C + Q * exp(-B * t))^1/v))

}
