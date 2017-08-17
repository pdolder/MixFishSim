#' @title Initialise simulation
#'
#' @description \code{init_sim} sets up the general simulation parameters such
#' as number of tows in a day, number of days fished in a week, how often
#' species movement occurs and number of years for the simulation. It also
#' creates some vector and matrix structures which are used in the
#' \code{init_pop} and \code{init_fleet} functions.

#' @param n_years is an integar defining the number of years for the
#' simulation
#' @param n_tow_day is an integar defining the number of tows in a days fishing
#' @param n_days_wk_fished is an integar defining the number of days in a
#' calendar week that are fished (e.g. 5 (out of 7))
#' @param n_fleets is an integar defining the number of fleets in the simulation
#' @param n_vessels is an integar defining the number of vessels in each fleet
#' @param n_species is an integar defining the number of species in the
#' simulation
#' @param move_length is an integar defining the duration (in weeks) between
#' spatial movements for the populations

#' @return is a list of lists, detailing the indexs and data formats necessary
#' for the simulation.

#' @examples init_sim(n_years = 1, n_tows_day = 4, n_days_wk_fished = 5,
#' n_fleets = 1, n_vessels = 1, n_species = 1, move_length = 2)

#' @export

init_sim <- function(n_years = 1, n_tows_day = 4, n_days_wk_fished = 5,
		     n_fleets = 1, n_vessels = 1, n_species = 1, move_length = 2) {

## Create an index for the simulations
	# ntd     = number tows per day
	# ndf     = number days fished
	# nw      = number of weeks per year
	# nwm     = number of weeks per month
	# nt      = number of times pop moves per year
	# nm      = number months in year
	# ny      = number of years in sim
	# ntow    = total number of tows in simulation
	# ntow.py = number of tows per year
	
	idx <- c(ntd = n_tows_day, ndf = n_days_wk_fished, nw = 52, nwm = 52/12,
		 nt = 52 / move_length, nm = 12, ny = n_years, 
		 ntow = n_tows_day * n_days_wk_fished * 52 * 12 * n_years + 1,
		 ntow.py = ((n_tows_day * n_days_wk_fished * 52 * 12 * n_years + 1) - 1)/n_years,
		 n.spp = n_species)

	# breaks index

	# These are the specific breaks for each period, based on the tow index
	# (our 'master' unit of time in the simulation)

	# tow.breaks = the 
	# day.seq = days of the week for each year of the simulation
	# day.breaks = 
	# trip.breaks = 
	# month.breaks = 
	# year.breaks = 

	day.seq <- 1:364 # temp

	brk.idx <- list(
        tow.breaks = c(1, rep(sort(rep(1:idx["nt"], length.out = idx["ntow.py"])), idx["ny"])),
	day.seq = day.seq[!day.seq %in% c(seq(7,364,7),seq(6,364,7))],
	day.breaks = c(1,rep(rep(day.seq[!day.seq %in%
				 c(seq(7,364,7),seq(6,364,7))],each =
				 idx["ntd"]),idx["ny"])),
	trip.breaks = c(1,rep(rep(1:(idx["ntow.py"]/(idx["ntd"]*idx["ndf"])),each=idx["ndf"]*idx["ntd"]),idx["ny"])), 
	month.breaks  = c(1,rep(sort(rep(1:idx["nm"],length.out = idx["ntow.py"])),idx["ny"])), 
	year.breaks   = c(1,rep(1:idx["ny"],each = idx["ntow.py"]))
	)


## Set up matrix to record vessel and population level results
	################
	# vessel level #
	################
	catch.mat <- matrix(NA, nrow = idx["ntow"], ncol = 9 + idx["n.spp"])
	colnames(catch.mat)  <- c("x","y","day","tow","trip","month","year",
				  paste0("spp",1:idx["n.spp"]),"allspp","val")

	# Step distances
	stepDs <- vector(mode = "numeric", length = idx["ntow"])

	# Turning angles
	angles <- vector(mode = "numeric", length = idx["ntow"])

	# Mean catch value
	MeanVal <- vector(mode = "numeric", length = idx["ntow"])

	# sd catch value
	sdVal   <- vector(mode = "numeric", length = idx["ntow"])

	###############
	# Pop level   #
	###############

	# Pop level biomass
	Bio.mat <- matrix(NA, nrow = idx["ny"], ncol = idx["nw"], dimnames =
			  list(1:idx["ny"], 1:idx["nw"]))
	# Pop level Fs
	F.mat <- matrix(NA, nrow = idx["ny"], ncol = idx["nw"], dimnames =
			list(1:idx["ny"], 1:idx["nw"]))

# Pop level catches
	Catch.mat <- matrix(NA, nrow = idx["ny"], ncol = idx["nw"], dimnames =
			    list(1:idx["ny"], 1:idx["nw"]))
	
# Pop level recruitment
Rec.mat <- matrix(NA,nrow= idx["n.spp"],ncol = idx["ny"]+1,dimnames=list(paste0("spp",1:idx["n.spp"]),0:idx["ny"]))

# Return a list for the sims

	return(list(index = list(idx = idx, brk.idx = brk.idx),
		    vessel = list(catch.mat = catch.mat, stepDs = stepDs,
				  angles = angles, MeanVal = MeanVal, sdVal =
					  sdVal),
		    pop = list(Bio.mat = Bio.mat, F.mat = F.mat, Catch.mat = Catch.mat, Rec.mat = Rec.mat)

		    ))

}
