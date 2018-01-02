#' @title Degrees to radians

#' @description \code{deg2rad} is a helper function to covert decimal degrees
#' to radians

#' @param r is the bearing in radians

#' @return is the bearing in degrees

#' @examples deg2rad(90)

#' @export

deg2rad <- function (d) {
	d * pi/180
}

