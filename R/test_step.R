#' @title test step length function

#' @description \code{test_step} is a function to test and review parameters
#' for the step_length function. This is primarily to help with identifying the
#' right parameters for the desired relationship between revenue and step
#' length.

#' @param step_params is a list of parameters which determine the relationship between
#' revenue gained from the recent fishing activity and the next move step
#' length, based on a gamma function. The list contains the following:
#' \itemize{
#' \item \strong{rate} Determines the rate ....
#' \item \strong{B1} Determines...
#' \item \strong{B2} Determines ...
#' \item \strong{B3} Determines ..
#' }

#' @param rev.max is the maximum revenue at which to test the step length
#' function.

#' @return is a plot of the relationship between revenue and step length 

#' @examples 
#' test_step(step_params = list(B1 = 1, B2 = 50, B3 = 2000, rate = 1), rev.max
#' = 2000)

#' @export

test_step <- function(step_params = step_params, rev.max = 2000) {

	# Extract the values for the plot
	rate <- step_params[["rate"]]
	B1   <- step_params[["B1"]] 
	B2   <- step_params[["B2"]] 
	B3   <- step_params[["B3"]] 

# Determine the range of values for revenue in increments of 1
rev <- seq(0,rev.max,1)

# apply the step_length function
stepD <- sapply(rev, FUN = step_length, step_params = step_params)

plot(stepD ~ rev, col = "grey") 
curve(exp(log(B2) + ((log(B1) - log(B2))/B3) * x), add = TRUE)

}

