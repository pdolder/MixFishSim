#' @title Distance calc, R implementation (C++ seems slow??) 

#' @description \code{distance_calcR} is a helper function to covert decimal degrees
#' to radians

#' @param x1 

#' @return distance 

#' @examples distance_calcR(x1 = 0, y1 = 0, x2 = 2, y2 = 9)

#' @export

distance_calcR <- function (x1 = 0, y1 = 0, x2 = 1, y2 = 1) {
	sqrt((x1 - x2)^2 + (y1 - y2)^2)
}
