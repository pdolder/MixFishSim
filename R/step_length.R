#' @title Step length function 

#' @description \code{step_length} is a function to calculate the step length a
#' vessel takes based on the step parameters provided for a gamma function and
#' the revenue from the most recent fishing activity.

#' @param step_params is a list of parameters which determine the relationship between
#' revenue gained from the recent fishing activity and the next move step
#' length, based on a gamma function. The list contains the following:
#' \itemize{
#' \item \strong{rate} Determines the rate ....
#' \item \strong{B1} Determines...
#' \item \strong{B2} Determines ...
#' \item \strong{B3} Determines ..
#' }
#' @param revenue is the last observed fishing revenue for the vessel

#' @return step - the size of the next step

#' @examples
#' step_length(step_params = list(B1 = 1, B2 = 50, B3 = 2000, rate = 1),
#' revenue = 300)

#' @export

step_length <- function (step_params = params[["step_params"]], revenue = revenue) {

	rate <- step_params[["rate"]]
	B1   <- step_params[["B1"]] 
	B2   <- step_params[["B2"]] 
	B3   <- step_params[["B3"]] 
	
	if(any(is.null(rate) | is.null(B1) | is.null(B2) | is.null(B3))) stop("Must set rate, B1, B2 and B3 parameters")

	step <- rgamma(1, shape = exp(log(B2) + ((log(B1) - log(B2))/B3) * revenue) * rate, rate = rate)
	return(step)

}
