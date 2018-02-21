#' @title Close areas
#'
#' @description The \code{close_areas} function implements the closures
#' according to the settings from \link{init_closure} and passes the areas to
#' \link{go_fish}. Its an internal function, requiring no user input.
#'
#' @param closure_init is the output from \link{init_clousre}.
#' @param commercial_logs is the commercial landings data, the output from
#' \link{combine_logs}. Only needed if closure 'basis' = 'commercial'.
#' @param survey_logs is the survey data, the survey[["log.mat"]]. Only needed
#' if closure 'basis' is 'survey'.
#' @param real_pop is the popualtions as recorded. Only needed if closure
#' 'basis' is 'real_pop'.

#' @return is a list of closed cells, to pass to \link{go_fish}

#' @examples None

#' @export

close_areas <- function (sim_init = sim_init, closure_init = NULL, commercial_logs = NULL, survey_logs = NULL, real_pop = NULL, t = t) {

	suppressMessages(require(dplyr))
	suppressMessages(require(akima))

	## Settings
	coords    <- closure_init[["input_coords"]]
	timescale <- closure_init[["temp_dyn"]]
	basis     <- closure_init[["basis"]]
	rationale <- closure_init[["rationale"]]
	thresh    <- closure_init[["closure_thresh"]]
	spp1 	  <- closure_init[["spp1"]]
	spp2 	  <- closure_init[["spp2"]]
	sc        <- closure_init[["sc"]]
	yr_base   <- closure_init[["year_basis"]]

	nx <- sim_init[["idx"]][["nrows"]]
	ny <- sim_init[["idx"]][["ncols"]]

	nx_i <- nx/sc
	nx_y <- ny/sc


	yr <- sim_init[["brk.idx"]][["year.breaks"]][[t]]
	mn <- sim_init[["brk.idx"]][["month.breaks"]][[t]]
	wk <- sim_init[["brk.idx"]][["week.breaks"]][[t]]

	## Annual timestep
	if(timescale == 'annual') {

		#### Pre-defined
		if(!is.null(coords)) {
			close_coords <- coords
		}

		#### Commercial logs
		if(basis == 'commercial') {

		if(rationale == 'high_pop') {

		## Get data
		logs <- as.data.frame(combine_logs(commercial_logs))
		spp <- spp1
		
		## Filter as appropriate:- here based on last years catch
		## summarise with catch/no_tows
		logs2 <- filter(logs, year == yr - 1) %>% group_by(x, y) %>% summarise(spp = sum(get(spp))/n())

	# Do interpolation
	akima.li <- interp(logs2[["x"]], logs2[["y"]], logs2[["spp"]], nx = nx_i, ny = ny_i, duplicate = "mean")

	# Make as DF - need to expand back to the correct scale of 1 x 1 cells
	akimaDF <- data.frame(x = rep(seq_len(nx), times = ny), 
		      y = rep(seq_len(ny), each = nx),
		      spp = as.numeric(apply(t(apply(akima.li$z, 1, rep, each = sc)), 2, rep, each = sc)))
	akimaDF$spp[is.na(akimaDF$spp)] <- 0

	## Get coords for cells above threshold catch
		q_close <- quantile(akimaDF$spp[akimaDF$spp > 0], prob = thresh)
		akimaDF$closure <- ifelse(akimaDF$spp >= q_close, "Closed" , "Open")
		closed_areas <- akimaDF[akimaDF$closure == 'Closed',]
		print(paste("Closing", nrow(closed_areas), "areas = ", 100 * nrow(closed_areas)/c(nx*ny), "% of area"))
			}
		}

		#### Survey logs 
		if(basis == 'survey') {
		
		}

		#### real pop
		if(basis == 'real_pop') {
		
		
		}

	}

	## Monthly timestep

		#### Pre-defined
	
		#### Commercial logs

		#### Survey logs 

		#### real pop 


	## Weekly timestep

		#### Commercial logs

		#### Survey logs 

		#### real pop 

return(closed_areas)
}
