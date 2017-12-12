#' @title make step function

#' @description \code{make_step} determines the new position of the vessel
#' following a move, using the step distance and bearing inputs.

#' @param stepD is a Numeric vector of the distance to move
#' @param Bear is a Numeric vector of the bearing to move (in degrees)
#' @param start.x is the starting point on the x-axis
#' @param start.y is the starting point on the y-axis

#' @return returns a new coordinate position through a vector (x, y)

#' @examples
#' make_step(stepD = 20, Bear = 90, start.x = 20, start.y = 5)

#' @export

make_step <- function (stepD, Bear, start.x,start.y) {
	Br     <-	(pi*Bear)/180 # Bearing in radians
	end.x  <- 	start.x + stepD * cos(Br)  # x * len * cos(Br)
	end.y  <-	start.y + stepD * sin(Br)  # y * len * sin(Br) 
	return(c(end.x,end.y))
}

