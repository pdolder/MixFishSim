#' @title Normal distribution

#' @description Helper function used for returning the PDF of a normal
#' distribution from the supplied temperature tolerances in \link{init_moveCov}

#' @param move_init is the output from \link{init_moveCov}
	
#' @examples sapply(seq(2,20,0.1), mu = 10, va = 6)

#' @export

norm_fun <- function (x, mu, va) {
		(1/(sqrt(2*pi*va)))*(exp(-(((x - mu)^2) / (2 * va))))
}

