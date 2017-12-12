#' @title create spawning habitat 
#' 
#' @description \code{create_spawn_hab} modifies the habitat preference maps
#' created by \code{create_hab} to account for spawning habitat preference -
#' can be used as a substitute during spawning periods. 

#' @param hab is the habitat preference for the population 
#' @param spwnareas is a list of Numeric vectors with the West, East, South and
#' North dimensions of the spawning areas, in the form list(spwn1 = c(x1, x2,
#' y1, y2)
#' @param mult is a Numeric with the attractiveness of the spawning area (a
#' multiplier)
#'
#' @return is the new habitat preference, taking account of the spawning area

#' @examples create_spawn_hab(hab = matrix(nc = 100, runif(100 *
#' 100)), spwnareas = list(spwn1 = c(20, 30, 50, 60)), mult = 10)
#' 
#' @export

## The basic premise is to modify the habitat to bias towards the defined
## spawning areas. Probably best to have as a separate input rather than
## recalculate each time its needed...

create_spawn_hab <- function(hab = hab, spwnareas = NULL, mult = 10) {

ncols  <- ncol(hab)
nrows  <- nrow(hab)

# create a matrix of 1s with right dims
spwn <- matrix(rep(1, nrows * ncols), nc = ncols)

spwn <- define_spawn(coord = spwnareas, spwn = spwn, mult = mult)

hab.spwn <- hab * spwn
hab.spwn <- hab.spwn / sum(hab.spwn)
return(hab.spwn)

}
