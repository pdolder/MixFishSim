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
	spp_1 	  <- closure_init[["spp1"]]
	spp_2 	  <- closure_init[["spp2"]]
	sc        <- closure_init[["sc"]]
	yr_base   <- closure_init[["year_basis"]]

	nx <- sim_init[["idx"]][["nrows"]]
	ny <- sim_init[["idx"]][["ncols"]]

	nx_i <- nx/sc
	nx_y <- ny/sc

	## Time references

	## Case of dynamic closures
	if(is.null(yr_base)) {
	
	if(timescale == "annual") {
	yr <- sim_init[["brk.idx"]][["year.breaks"]][[t]] - 1
	mn <- 1:12
	wk <- 1:52
	}

	if(timescale == "monthly") {
	wk <- 1:52
	# If same year, month is previous month. If different year, month is 12
	mn <- ifelse(sim_init[["brk.idx"]][["year.breaks"]][[t]] == sim_init[["brk.idx"]][["year.breaks"]][[t-1]], 
	sim_init[["brk.idx"]][["month.breaks"]][[t-1]], 12)
	yr <- ifelse(sim_init[["brk.idx"]][["year.breaks"]][[t]] == sim_init[["brk.idx"]][["year.breaks"]][[t-1]], 
	sim_init[["brk.idx"]][["year.breaks"]][[t]], sim_init[["brk.idx"]][["year.breaks"]][[t-1]])
	}

	if(timescale == "weekly") {
	# If same year, week is previous week. If different year, week is 52
	wk <- ifelse(sim_init[["brk.idx"]][["year.breaks"]][[t]] == sim_init[["brk.idx"]][["year.breaks"]][[t-1]], 
	sim_init[["brk.idx"]][["week.breaks"]][[t-1]], 52)
	mn <- 1:12
	yr <- ifelse(sim_init[["brk.idx"]][["year.breaks"]][[t]] == sim_init[["brk.idx"]][["year.breaks"]][[t-1]], 
	sim_init[["brk.idx"]][["year.breaks"]][[t]], sim_init[["brk.idx"]][["year.breaks"]][[t-1]])
	}
			   }

	## Case of defined year basis
	if(!is.null(yr_base)) {
	yr <- yr_base
	
	if(timescale == "annual") {
	mn <- 1:12
	wk <- 1:52
	}

	if(timescale == "monthly") {
	mn <- sim_init[["brk.idx"]][["month.breaks"]][[t]]
	wk <- 1:52
	}

	if(timescale == "weekly") {
	mn <- 1:12
	wk <- sim_init[["brk.idx"]][["week.breaks"]][[t]]
	}
	}


########################
### The calculations ###
########################

##############
## The data ##
##############

#### Pre-defined closures
if(!is.null(coords)) {
if(timestep == 'annual') {closed_areas  <- coords}
if(timestep == 'monthly') {closed_areas <- coords[[mn]]}
if(timestep == 'weekly') {closed_areas <- coords[[wk]]}
		}
		
#### Calculated closures

		#### Commercial logs
		if(basis == 'commercial') {

		## Get data
		logs <- as.data.frame(combine_logs(commercial_logs))
	
		if(rationale == 'high_pop') {
		## Filter as appropriate:- here based on either yr_base or
		## dynamically on last years tow 
		## summarise with catch/no_tows - not actually needed as
		## duplicate = mean ?
		interpdat <- filter(logs, year %in% yr, month %in% mn, week %in% wk) %>% group_by(x, y) %>% summarise(dat= sum(get(spp_1))/n())
		}

		if(rationale == 'high_ratio') {
		## Filter as appropriate:- here based on either yr_base or
		## dynamically on last years tow 
		## summarise with catch/no_tows - not actually needed as
		## duplicate = mean ?
		interpdat <- filter(logs, year %in% yr, month %in% mn, week %in% wk) %>% group_by(x, y) %>% summarise(dat1 = sum(get(spp_1))/n(), dat2 =sum(get(spp_2))/n())
		## Calculate the ratios
		interpdat$dat <- interpdat$dat2 / interpdat$dat1
		## Deal with any NaN and Infs - calculate the Infs as large
		## numbers and Nans as 0s
		interpdat$dat[is.nan(interpdat$dat)]      <- 0 
		interpdat$dat[is.infinite(interpdat$dat)] <- 10 * interpdat$dat2[is.infinite(interpdat$dat)] ## very favourable areas as only targ spp found
		## scale the values
		interpdat$dat <- 1 - (interpdat$dat / max(interpdat$dat)) ## want to close areas where lots of spp1 to spp2
		}
			} ## end commercial basis
			
		#### Survey logs 
		if(basis == 'survey') {

		logs <- survey_logs

		if(rationale == 'high_pop') {
		## Filter as appropriate:- here based on either yr_base or
		interpdat <- filter(logs, year %in% yr) %>% group_by(x, y) %>% summarise(dat= sum(get(spp_1))/n())
		}

		if(rationale == 'high_ratio') {
		## Filter as appropriate:- here based on either yr_base or
		interpdat <- filter(logs, year %in% yr) %>% group_by(x, y) %>% summarise(dat1 = sum(get(spp_1))/n(), dat2 =sum(get(spp_2))/n())
		## Calculate the ratios
		interpdat$dat <- interpdat$dat2 / interpdat$dat1
		## Deal with any NaN and Infs - calculate the Infs as large
		## numbers and Nans as 0s
		interpdat$dat[is.nan(interpdat$dat)]      <- 0 
		interpdat$dat[is.infinite(interpdat$dat)] <- 10 * interpdat$dat2[is.infinite(interpdat$dat)] ## very favourable areas as only targ spp found
		## scale the values
		interpdat$dat <- 1 - (interpdat$dat / max(interpdat$dat)) ## want to close areas where lots of spp1 to spp2
		}

		} ## End survey basis 

		#### real pop
		if(basis == 'real_pop') {

		## Stored as matrices within a nested list, so we need to
			## unravel
		logs_yr_list <- lapply(yr, function(y) {
			logs_wk_list <- lapply(wk, function(w) {

		## Number species
		res2 <- lapply(seq_len(n_spp), function(x) {
		res <- data.frame(spp = as.numeric(logs[[y,w]][[x]]))
		colnames(res) <- paste("spp",x,sep="")
		 return(res) 
			   })

		res3 <- cbind(data.frame(x = rep(seq_len(100), times = 100), 
			   y = rep(seq_len(100), each = 100),
			   year = y, week = w,  do.call(cbind,res2)))

		return(res3)
		})

		logs_yr_list <- do.call(rbind, logs_wk_list)
		return(logs_yr_list)
					})

		logs <- do.call(rbind, logs_yr_list)

		if(rationale == 'high_pop') {
		## Filter as appropriate:- here based on either yr_base or
		interpdat <- filter(logs, year %in% yr) %>% group_by(x, y) %>% summarise(dat= sum(get(spp_1))/n())
		}

		if(rationale == 'high_ratio') {
		## Filter as appropriate:- here based on either yr_base or
		interpdat <- filter(logs, year %in% yr) %>% group_by(x, y) %>% summarise(dat1 = sum(get(spp_1))/n(), dat2 =sum(get(spp_2))/n())
		## Calculate the ratios
		interpdat$dat <- interpdat$dat2 / interpdat$dat1
		## Deal with any NaN and Infs - calculate the Infs as large
		## numbers and Nans as 0s
		interpdat$dat[is.nan(interpdat$dat)]      <- 0 
		interpdat$dat[is.infinite(interpdat$dat)] <- 10 * interpdat$dat2[is.infinite(interpdat$dat)] ## very favourable areas as only targ spp found
		## scale the values
		interpdat$dat <- 1 - (interpdat$dat / max(interpdat$dat)) ## want to close areas where lots of spp1 to spp2
		}

		} ## End real pop basis

		
################################
### Do interpolation on data ###
################################

akima.li <- interp(interpdat[["x"]], interpdat[["y"]], interpdat[["dat"]], nx = nx_i, ny = ny_i, duplicate = "mean")

# Make as DF - need to expand back to the correct scale of 1 x 1 cells
akimaDF <- data.frame(x = rep(seq_len(nx), times = ny), 
		      y = rep(seq_len(ny), each = nx),
		      dat = as.numeric(apply(t(apply(akima.li$z, 1, rep, each = sc)), 2, rep, each = sc)))
akimaDF$dat[is.na(akimaDF$dat)] <- 0

## Get coords for cells above threshold catch
q_close <- quantile(akimaDF$dat[akimaDF$dat > 0], prob = thresh)
akimaDF$closure <- ifelse(akimaDF$dat >= q_close, "Closed" , "Open")
closed_areas <- akimaDF[akimaDF$closure == 'Closed',]


print(paste("Closing", nrow(closed_areas), "areas = ", 100 * nrow(closed_areas)/c(nx*ny), "% of area"))

return(closed_areas)
}
