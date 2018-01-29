#' @title Survey settings
#'
#' @description \code{survey_settings} is a function to mimic a
#' fisheries-independent survey to sample catches from the populations.
#'
#' @param sim_init is the general simualtion settings from \link{sim_init}
#' @param design is the survey design used, at the moment only
#' \emph{fixed_station}
#' @param n_stations is a Numeric for the number of stations to be fished each.
#' Note: If using 'fixed_station' design this will be rounded down to maintain
#' a grid shape if not divisble.  
#' @param start_day is a Numeric for the first day of the survey each year
#' @param stations_per_day is a Numeric for the number of stations surveyed per
#' day
#' @params Qs is a named Numeric Vector containing any survey catchabilities, assumed to be time invariant.
#'
#' @return is a list consisting of the survey setting and a a matrix for
#' storing the log of catches from the survey, to be used as an input to
#' \link{run_sim}.

#' @examples
#' survey_settings(design = 'fixed_station', n_stations = 50, start_day = 90, stations_per_days = 5, Qs = c("spp1" = 0.1, "spp2" = 0.2)

#' @export

survey_settings <- function (sim_init = NULL, design = 'fixed_station', n_stations = 50, 
			     start_day = 90, stations_per_day = 5, Qs = NULL) {

	# useful indexes
	idx       <- sim_init[["idx"]]
	n_fleets  <- idx[["nf"]]
	n_vessels <- idx[["nv"]]
	brk.idx   <- sim_init[["brk.idx"]]

	### Checks
	if(!start_day %in% sim_init[["brk.idx"]][["day.seq"]]) stop("We don't fish this day, choose another start_day!")

	## determine design

	if(design == 'fixed_station') {

	# Create a grid of stations
	x <- round(seq(1,idx[["ncols"]], length.out = round(sqrt(n_stations),0)),0) # sim_init[["idx"]][["n_rows"]], 1)
	y <- round(seq(1, idx[["nrows"]], length.out = round(sqrt(n_stations),0)),0) # sim_init[["idx"]][["n_cols"]], 1)
	grid <- cbind(x = x, y = rep(y, each = length(x)))

	# update no stations 
	n_stations <- nrow(grid)
	
	}

	if(design != "fixed_station") {stop("Sorry, only fixed_station design available at the moment") }

	##########################
	## set up survey log matrix
	##########################

	#create the repeating pattern for the station days
	station_days <- rep(sim_init[["brk.idx"]][["day.seq"]][which(sim_init[["brk.idx"]][["day.seq"]] == 92, arr.ind = T):c(which(sim_init[["brk.idx"]][["day.seq"]] == 92, arr.ind = T) + round(n_stations / stations_per_day, 0) - 1)], each = stations_per_day)[seq_len(n_stations)]


	log.mat 	       <- matrix(NA, nrow = n_stations * sim_init[["idx"]][["ny"]], 
				       ncol = 6 + idx[["n.spp"]])
	colnames(log.mat)      <- c("station_no", "x","y","day","tow","year",
				  paste0("spp",seq(idx[["n.spp"]])))
	log.mat[,'station_no'] <- rep(seq_len(n_stations), sim_init[["idx"]][["ny"]])
	log.mat[,'x']          <- rep(grid[,"x"], times = sim_init[["idx"]][["ny"]])
	log.mat[,'y']          <- rep(grid[,"y"], times = sim_init[["idx"]][["ny"]])
	log.mat[,'day']        <- rep(station_days, times = sim_init[["idx"]][["ny"]])
	log.mat[,'tow']        <- rep(seq_len(n_stations), sim_init[["idx"]][["ny"]]) 
      log.mat[,'year']       <- rep(seq_len(sim_init[["idx"]][["ny"]]), each = n_stations)


	return(list(survey_settings = c("design" = design, "n_stations" =
					n_stations, "days_fished" = round(n_stations / stations_per_day, 0),
				"Qs" = Qs), log.mat = log.mat))



}
