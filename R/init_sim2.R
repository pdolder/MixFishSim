#' @title Initialise simulation
#'
#' @description \code{init_sim2} sets up the general simulation parameters such
#' as number of tows in a day, number of days fished in a week, how often
#' species movement occurs and number of years fo`r the simulation. It also
#' creates some vector and matrix structures which are used in the
#' \code{init_pop} and \code{init_fleet} functions.
#' 
#' It also allows for variable number of tows per day, to increase or decrease 
#' effort over the simulations.

#' @param n_years is an integar defining the number of years for the
#' simulation
#' @param n_tow_day is an vector of length n_years defining the number of tows 
#' in a days fishing each year
#' @param n_days_wk_fished is an integar defining the number of days in a
#' calendar week that are fished (e.g. 5 (out of 7))
#' @param n_fleets is an integar defining the number of fleets in the simulation
#' @param n_vessels is an integar defining the number of vessels in each fleet
#' @param n_species is an integar defining the number of species in the
#' simulation
#' @param nrows Numeric integer with the y dimension of the field in
#' \emph{nrow * ncol}
#' @param ncols Numeric integer with the x dimension of the field in
#' \emph{nrow * ncol}
#' @param move_freq is an integar defining the duration (in weeks) between
#' spatial movements for the populations

#' @return is a list of lists, detailing the indexs and data formats necessary
#' for the simulation.

#' @examples init_sim(n_years = 1, n_tows_day = 4, n_days_wk_fished = 5,
#' n_fleets = 1, n_vessels = 1, n_species = 1, move_freq = 2)

#' @export

init_sim2 <- function(n_years = 1, n_tows_day = 4, n_days_wk_fished = 5,
		     n_fleets = 1, n_vessels = 1, n_species = 1, nrows = nrows,
		     ncols = ncols, move_freq = 2) {

## Create an index for the simulations
	# nrows   = number of cells in y direction
	# ncols   = number of cells in x direction
	# ntd     = number tows per day
	# ndf     = number days fished
	# nw      = number of weeks per year
	# nwm     = number of weeks per month
	# nt      = number of times pop moves per year
	# nm      = number months in year
	# ny      = number of years in sim
	# ntow    = total number of tows in simulation
	# ntow.py = number of tows per year
  
  if(length(n_tows_day) != n_years) n_tows_day <- rep(n_tows_day, n_years)
  
	idx <- list(ntd = n_tows_day, ndf = n_days_wk_fished, nw = 52, nwm = 52/12,
		 nt = 52 / move_freq , nm = 12, ny = n_years, 
		 ntow = sum(n_tows_day * n_days_wk_fished * 52 * n_years),
		 ntow.py = (n_tows_day * n_days_wk_fished * 52 * n_years)/n_years,
		 n.spp = n_species, ncols = ncols, nrows = nrows, nf = n_fleets, 
		 nv = n_vessels)

	# breaks index

	# These are the specific breaks for each period, based on the tow index
	# (our 'master' unit of time in the simulation)

	# tow.breaks = the 
	# day.seq = days of the week for each year of the simulation
	# day.breaks = 
	# trip.breaks = 
	# month.breaks = 
	# year.breaks = 

	day.seq <- seq(364) # temp

	brk.idx <- list(
    #    tow.breaks = rep(sort(rep(seq(idx[["nt"]]), length.out = idx[["ntow.py"]])), idx[["ny"]]),
	day.seq = day.seq[!day.seq %in% c(seq(7,364,7),seq(6,364,7))],
	day.breaks = unlist(lapply(idx[["ntd"]], function(x) rep(day.seq[!day.seq %in% c(seq(7,364,7),seq(6,364,7))],each = x))),
	trip.breaks = unlist(lapply(seq_len(idx[["ny"]]), function(y) {
	  rep(lapply(unlist(lapply(idx[["ny"]], function(x) idx[["ntow.py"]][[y]]/(idx[["ntd"]]*idx[["ndf"]]))),seq)[[y]],
	      each = idx[["ndf"]]*idx[["ntd"]][[y]])
	})), 
	week.breaks = unlist(lapply(idx[["ntd"]] * idx[["ndf"]], function(x) rep(seq_len(idx[["nw"]]), each = x))),
	month.breaks  = unlist(lapply(idx[["ntow.py"]], function(x) sort(rep(seq(idx[["nm"]]), length.out = x)))), 
	year.breaks   = unlist(lapply(seq_len(idx[["ny"]]), function(x) rep(x, times = idx[["ntow.py"]][[x]])))
	)


# Return a list for the sims

	index = list(idx = idx, brk.idx = brk.idx)
	return(index)
}
