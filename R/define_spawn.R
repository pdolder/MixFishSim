#' @title define spawning areas 
#' 
#' @description \code{define_spawn} is an auxiliary function called by
#' \code{create_spawn_hab} to create the spawning habitat preferences.

#' @param coord is a List of Numeric vectors of the boundaries of the spawning
#' areas, i.e. list(spwn1 = c(x1, x2, y1, y2), spwn2 = ...)
#' @param spwn is a Numeric matrix of 1s fed in by \code{create_spawn_hab}
#' @param mult is a Numeric of the attractiveness of the spawning areas

#' @return a matrix of spawning preference 

#' @examples
#' define_spawn(coord = list(spwn1 = c(2,4,2,4)), spwn = matrix(nc = 3, runif(9)), mult = 10)  

#' @export

define_spawn <- function(coord = NULL, spwn = NULL, mult = 10){

	for(i in 1:length(coord)) {
	x1 <- coord[[i]][1]; x2 <- coord[[i]][2]
	y1 <- coord[[i]][3]; y2 <- coord[[i]][4]
	spwn[x1:x2, y1:y2] <- spwn[x1:x2, y1:y2] * mult
	}

	return(spwn)

}

