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

close_areas <- function (sim_init = sim_init, closure_init = NULL, commercial_logs = NULL, survey_logs = NULL, real_pop = NULL) {

	require(dplyr); require(akima)

	## Settings
	coords    <- closure_init[["input_coords"]]
	timescale <- closure_init[["temp_dyn"]]
	basis     <- closure_init[["basis"]]
	rationale <- closure_init[["rationale"]]
	thresh    <- closure_init[["closure_thresh"]]
	spp1 	  <- closure_init[["spp1"]]
	spp2 	  <- closure_init[["spp2"]]

	nx <- sim_init[["idx"]][["nrows"]]
	ny <- sim_init[["idx"]][["ncols"]]

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
		logs2 <- filter(logs, year == max(logs$year) -
				1) %>% group_by(x, y) %>% summarise(spp =
			sum(spp)/n())

	# Do interpolation
	akima.li <- interp(logs2$x, logs2$y, logs2$spp, nx = nx, ny = ny)

	# Make as DF
	akimaDF <- data.frame(x = rep(seq_len(nx), times = ny), 
		      y = rep(seq_len(ny), each = nx),
		      spp = as.numeric(akima.li$z))
	akimaDF$spp[is.na(akimaDF$spp)] <- 0

	## Get coords for cells above threshold catch
		q_close <- quantile(akimaDF$spp, prob = thresh)
		akimaDF$closure <- ifelse(akimaDF$spp >= q_close, "Closed" , "Open")
		closed_areas <- akimaDF[akimaDF$closure == 'Closed',]
	
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
