#' @title Go fish all fleets

#' @description \code{go_fish_all_fleets} applies the function
#' \code{go_fish_fleet} to each of the fleets with a parLapply using the
#' \code{parallel} package

#' @param n_cores is the to use for the parallel processing
#' @param sim_init is the initialised sim object from \code{init_sim}
#' @param fleets is the initialised fleet object from \code{init_fleet}
#' @param fleets_log is the log of catches for fleets, containing a list of
#' fleets, each with a list of vessels, containing the vessel dataframe catches
#' and spatial catches
#' @param Pop is the current spatial populations biomass
#' @param t is the tow number

#' @return is a list the same as fleets_log

#' @examples None as yet

#' @export

go_fish_all_fleets <- function(n_cores = 1, sim_init = sim, fleets = NULL, fleets_log = NULL,Pop = NULL, t = NULL) {

cl <- makeCluster(no_cores, outfile = "")
clusterExport(cl, c("sim", "go_fish_fleet", "fleets", "Pop", "t"))

	      applied_to_fleets <- parLapply(cl, seq(length(fleets$fleet_params)), function(fl) {

				go_fish_fleet(FUN = go_fish, sim_init = sim_init, fleets_params = fleets[["fleet_params"]][[fl]],
					      fleets_catches = fleets_log[[fl]][["fleets_catches"]],
					      sp_fleets_catches = fleets_log[[fl]][["sp_fleets_catches"]], pops = Pop, t = t)



})
	      return(applied_to_fleets)

}
