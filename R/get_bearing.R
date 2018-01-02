#' @title Get bearing function

#' @description \code{get_bearing} is a function to calculate a new bearing for
#' a vessel. The new bearing is determined from the Von Mises circular
#' distribution, with a concentration parameter, \emph{k} which is linked to
#' the value of the recent tow. Thus, if a vessel has a good tow, its more
#' likely to turn round and fish again in the same area.

#' @param b is a Numeric based on decimal degrees (0 - 360) of the current
#' bearing for the vessel 
#' @param k is a Numeric [0-100] for the concentration parameter determining
#' the likely new direction for the vessel.

#' @return bearing - is the new bearing for the vessel

#' @examples get_bearing(b = 270, k = 100)

#' @export

get_bearing <- function (b = NULL, k = NULL) {

# First we calculate the change in bearing from the current, i.e. always use 0
# as the bearing and then calc new bearing at the end
# rvm is the von mises distribution

ChangeBear <- rad2deg(CircStats::rvm(1, 0, k))

newBearing <- ifelse(ChangeBear > 180, ChangeBear - 180, ChangeBear + 180)
return(newBearing)

}
