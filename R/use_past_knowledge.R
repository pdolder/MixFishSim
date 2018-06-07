#' @title Use past knowledge

#' @description \code{use_past_knowledge} is a helper function to make a
#' random draw whether to do exploratory fishing or go to known fishing grounds

#' @param p is the probability of using past knowledge, drawn from
#' \link{logistic}

#' @examples None

#' @export

use_past_knowledge <- function (p = NULL) {
	return(ifelse(runif(1, 0, 1) < p, TRUE, FALSE))
}


