#' @title Combine logs

#' @description \code{combine_logs} is a helper function to convert the
#' list of fleet and vessels catch logs into a single dataframe.

#' @param fleets_catches is the list output of fleets_catches from
#' \link{run_sim}

#' @return is a dataframe of the fleet and vessel catches in logbook format

#' @examples
#' logs <- combine_logs(fleets_catches)
#' ## Not run

#' @export

combine_logs <- function (fleets_catches) {

	# index
	no_fleets     <- length(fleets_catches)
	ves_per_fleet <- length(fleets_catches[[1]][[1]])
	rec_per_vess  <- nrow(fleets_catches[[1]][[1]][[1]])


	all_logs <- lapply(seq_len(no_fleets), function(x1) {
	
	fleets_logs <- lapply(seq_len(ves_per_fleet), function(x2) {
		
	fleet_level <- cbind("vessel" = x2, fleets_catches[[x1]][[1]][[x2]]) 
	return(fleet_level)
})
	fleets_logs <- do.call(rbind, fleets_logs)
	
	
	all_level <- cbind("fleet" = x1, fleets_logs)
})

	all_logs <- do.call(rbind, all_logs)
          
	return(as.data.frame(all_logs))

}


