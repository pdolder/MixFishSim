#' @title Radians to degrees

#' @description \code{rad2deg} is a helper function to covert radians to
#' decimal degrees

#' @param d is the bearing in degrees
#' @param r is the bearing in radians

#' @examples rad2deg(pi)

#' @export 

rad2deg <- function(r) {
	r * 180/pi
}
