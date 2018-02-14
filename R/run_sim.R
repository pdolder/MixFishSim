#' @title Run sim

#' @description \code{run_sim} is the overarching simulation function, taking
#' all the parameterised inputs and returning the results.

#' @param sim_init is the parameterised simulation settings from
#' \code{init_sim}
#' @param pop_init is the parameterised populations from \code{init_pop}
#' @param move_cov is a parameterised movement covariate object, from
#' \code{init_moveCov}
#' @param fleets_init is the parameterised fleets from \code{init_fleets}
#' @param hab_init is the parameterised habitat maps from \code{create_hab}
#' @param InParallel is a BOLEEN indicating whether calculations should be done
#' using parallel processing from \code{parallel}, default is TRUE
#' @param save_pop_bio is a logical flag to indicate if you want to record #' true spatial population at each time step (day)
#' @param survey is the survey settings from \link{init_survey}, else NULL if no survey is due to be simulated
#' @param closure is the spatial closure settings from \link{init_closure}m
#' else NULL if no closures are to be implemented

#' @return is the results...

#' @examples Not yet
#'
#' @export

run_sim <- function (sim_init = NULL, pop_init = NULL, move_cov = NULL, fleets_init = NULL, hab_init = NULL, InParallel = TRUE, cores = 1, save_pop_bio = FALSE, survey = NULL, closure = NULL,...) {
# Overarching function for running the simulations

start.time <- Sys.time() # for printing runtime

suppressMessages(require(doParallel))
registerDoParallel(cores = cores)

#######################
####### Indices #######
#######################

# Extract these to make indexing easier 

ntow         <- sim_init[["idx"]][["ntow"]] # length of t loop
n_fleets     <- sim_init[["idx"]][["nf"]]  
n_vess       <- sim_init[["idx"]][["nv"]]  
n_spp        <- sim_init[["idx"]][["n.spp"]]  
ncols        <- sim_init[["idx"]][["ncols"]]  
nrows        <- sim_init[["idx"]][["nrows"]]  
n_weeks        <- sim_init[["idx"]][["nw"]]  

day.breaks     <- sim_init[["brk.idx"]][["day.breaks"]]  
week.breaks    <- sim_init[["brk.idx"]][["week.breaks"]]  
trip.breaks    <- sim_init[["brk.idx"]][["trip.breaks"]]  
month.breaks   <- sim_init[["brk.idx"]][["month.breaks"]]  
year.breaks    <- sim_init[["brk.idx"]][["year.breaks"]]  

###################################
##### Temporary containers ########
###################################

Rec  <- list()                   # For storing spatial recruitment, is overwritten
for(s in paste0("spp",seq_len(n_spp))) { Rec[[s]] <- 0 }
B    <- pop_init[["Start_pop"]]  # For storing current biomass, is overwritten
Bm1  <- pop_init[["Start_pop"]]  # For storing last time-step biomass, is overwritten

AreaClosures <- data.frame(x = -1, y = -1) # Dummy closures 

###################################
###### Move probabilities #########
###################################
print("Calculating movement probabilities")

MoveProb  <- foreach(s = paste0("spp", seq_len(n_spp)))  %dopar% move_prob_Lst(lambda = 0.3, hab = hab_init[["hab"]][[s]])
MoveProb_spwn <- foreach(s = paste0("spp", seq_len(n_spp)))  %dopar% move_prob_Lst(lambda = 0.3, hab = hab_init[["spwn_hab"]][[s]])
	  
names(MoveProb)      <- paste0("spp", seq_len(n_spp))
names(MoveProb_spwn) <- paste0("spp", seq_len(n_spp))

## Avoid printing every tow
print.seq <- seq(1, ntow, 20)


## Closure?
if(!is.null(closure )) {
print("You are implementing spatial closures....")
closeArea <- TRUE
}

##################
### loop control #
for (t in seq_len(ntow)) {
##################

## Loop messages

	## Print when new year
if(t == 1){
	print(paste("----------year", year.breaks[t], "-----------"))
}

if(t > 1){
	if(year.breaks[t] != year.breaks[t-1]) {
	print(paste("----------year", year.breaks[t], "-----------")) }
}

## Print some tow info
if(t %in% print.seq) {
print(paste("tow ==", t, "----",round(t/ntow * 100,0), "%"))
	}


###################################
## Switches for various processes #
###################################

## first tow in a week where any of the stocks recruit
Recruit  <- ifelse(t > 1, ifelse(week.breaks[t] != week.breaks[t-1] &
		  week.breaks[t] %in% 
		  unlist(sapply(pop_init$dem_params, function(x) x[["spwn_wk"]])),
	  TRUE, FALSE), FALSE) 

if(t != ntow) {
Pop_dyn  <- ifelse(day.breaks[t] != day.breaks[t+1], TRUE, FALSE) ## weekly delay diff
Pop_move <- ifelse(week.breaks[t] != week.breaks[t+1], TRUE, FALSE) ## weekly pop movement
Update   <- ifelse(day.breaks[t] != day.breaks[t+1], TRUE, FALSE) ## weekly pop records 

## Closure switch, when to recalculate the closed areas

if(t==1 & !closeArea) {CalcClosures  <-  FALSE }
if(t==1 & closeArea & is.null(closure[["input_coords"]])) {CalcClosures <- FALSE}
if(t==1 & closeArea & !is.null(closure[["input_coords"]]) & closure[["year_start"]] == 1) {CalcClosures <- TRUE}
if(t==1 & closeArea & !is.null(closure[["input_coords"]]) & closure[["year_start"]] > 1) {CalcClosures <- FALSE}

if( t > 1 & closeArea ) 		{
if(closure[["temp_dyn"]] == 'annual') {
CalcClosures <- ifelse(year.breaks[t] != year.breaks[t-1], TRUE, FALSE)
}
if(closure[["temp_dyn"]] == 'monthly') {
CalcClosures <- ifelse(month.breaks[t] != month.breaks[t-1], TRUE, FALSE)
}
if(closure[["temp_dyn"]] == 'weekly') {
CalcClosures <- ifelse(week.breaks[t] != week.breaks[t-1], TRUE, FALSE)
}

	      		}

	      }

#######################
##### Recruitment #####
#######################

# Specified weeks
# As defined by:
# init_pop$rec_params$rec_wk
# Recruitment only occurs in specified spwn.hab
# Recruitment based on pop in spawning grounds in first week of spawning,
# but occurs throughout the spawning period
# Different spawning periods for different pops, so we need to handle this...

if(Recruit) { # Check for new week

print("Recruiting")

    ## Check if its a recruitment week for the population
 Rec <- foreach(s = paste0("spp", seq_len(n_spp))) %dopar% {

	    if(week.breaks[t] %in% pop_init[["dem_params"]][[s]][["rec_wk"]]) {

    rec <- Recr_mat(model = pop_init[["dem_params"]][[s]][["rec_params"]][["model"]],
     params = c("a" = as.numeric(pop_init[["dem_params"]][[s]][["rec_params"]][["a"]]),
		"b" = as.numeric(pop_init[["dem_params"]][[s]][["rec_params"]][["b"]])),
     B = B[[s]], 
     cv = as.numeric(pop_init[["dem_params"]][[s]][["rec_params"]][["cv"]]))

	    }

	if(!week.breaks[t] %in% pop_init[["dem_params"]][[s]][["rec_wk"]]) {
		rec <- matrix(0, ncol = ncols, nrow = nrows)
	
	}

	rec

    }
names(Rec) <- paste0("spp", seq_len(n_spp))

print(sapply(names(Rec), function(x) { sum(Rec[[x]]) / sum(B[[x]])}))

}

##########################
#### Spatial closures ####
##########################

## Calculate where to place the closures
## Can't close areas in the first year, unless manually defined

## Dynamic closures
if(closeArea & CalcClosures & year.breaks[t] >= closure[["year_start"]] & is.null(closure[["input_coords"]])) {
print("Calculating where to place closures dynamically...")
print(paste("Based on", closure[["basis"]], "on a", closure[["temp_dyn"]], "basis using", closure[["rationale"]]))

AreaClosures <- close_areas(sim_init = sim_init, closure_init = closure, commercial_logs = catches, survey_logs = survey, real_pop = pop_bios, t = t)

}


## Fixed closures
if(closeArea & CalcClosures & !is.null(closure[["input_coords"]]) & year.breaks[t] >= closure[["year_start"]]) {
print("Setting manually defined closures")
print(paste("Closures are", closure[["temp_dyn"]]))

if(closure[["temp_dyn"]] == 'annual') {
AreaClosures <- closure[["input_coords"]]
}

if(closure[["temp_dyn"]] == "monthly") {
AreaClosures <- closure[["input_coords"]][[month.breaks[[t]]]]
}

if(closure[["temp_dyn"]] == "weekly") {
AreaClosures <- closure[["input_coords"]][[week.breaks[[t]]]]
}

}

#######################
###### Fishing ########
#######################

# every t
# uses go_fish_fleet function, either
# 1. in a loop over fleets (InParallel = F)
# 2. Using a parLapply (InParallel = T)

if(InParallel) {

if(t==1) {

catches <- foreach(fl=seq_len(n_fleets)) %dopar% 

     go_fish_fleet(FUN = go_fish, 
			sim_init = sim_init, 
			fleets_params = fleets_init[["fleet_params"]][[fl]],
		   fleets_catches =     fleets_init[["fleet_catches"]][[fl]], 
		   sp_fleets_catches =  fleets_init[["sp_fleet_catches"]][[fl]], closed_areas = AreaClosures, pops = B, t = t)


} # end t==1 run

if(t > 1) {

# if its the same day 
if(day.breaks[t] == day.breaks[t-1]) {
catches <- foreach(fl=seq_len(n_fleets)) %dopar% 

	go_fish_fleet(FUN = go_fish, 	sim_init = sim_init, 
			fleets_params = fleets_init[["fleet_params"]][[fl]],
		   fleets_catches =     catches[[fl]][["fleets_catches"]], 
		   sp_fleets_catches =  catches[[fl]][["sp_fleets_catches"]],
		   pops = B, t = t, closed_areas = AreaClosures)

	} # end same week run

# if its a new day - reset the spatial catches counter
if(day.breaks[t] != day.breaks[t-1]) {

catches <- foreach(fl=seq_len(n_fleets)) %dopar% 

  go_fish_fleet(FUN = go_fish, 
		sim_init = sim_init, 
			fleets_params = fleets_init[["fleet_params"]][[fl]],
		   fleets_catches =     catches[[fl]][["fleets_catches"]], 
		   sp_fleets_catches =  fleets_init[["sp_fleet_catches"]][[fl]],
		   pops = B, t = t, closed_areas = AreaClosures)

	       


	} # end new week run

} # end if t>1

} # end InParallel if statement

#if(!InParallel) {

#}

#######################
##### Pop dynamics ####
#######################

# every week (end week?)
# calc the spatial Fs, find_spat_f_pops()
# Apply the delay_diff()
# Need B-1 and B to calc B+1

if(Pop_dyn) {

print("Delay-difference model")

## Calculate the fishing mortalities
# 1. Sum all fleet catches - DONE
# 2. find spatial Fs - DONE
# 3. reset the catch matrices - DONE (above)

# Sum the catches of each population over the fleets and vessels
spp_catches <- sum_fleets_catches(FUN = sum_fleet_catches, fleets_log =
				  catches, sim_init = sim_init)

## Fs for all populations


# The proportion of stock removed
#print(sapply(1:2, function(x) { max(spp_catches[[x]] / B[[x]], na.rm = T) }))

spat_fs <- find_spat_f_pops(sim_init = sim_init, C = spp_catches, B = B, 
                            dem_params = pop_init[["dem_params"]])

#print(sapply(spp_catches, sum))
#print(sapply(spat_fs, mean))

## Fishing mortality rates
print(sapply(names(spat_fs), function(x) weighted.mean(spat_fs[[x]], B[[x]])))

# Apply the delay difference model
Bp1 <- foreach(x = paste0("spp", seq_len(n_spp))) %dopar% {

al   <- ifelse(week.breaks[t] %in% pop_init[["dem_params"]][[x]][["rec_wk"]],
	     1/length(pop_init[["dem_params"]][[x]][["rec_wk"]]), 0)
alm1 <- ifelse(c(week.breaks[t]-1) %in% pop_init[["dem_params"]][[x]][["rec_wk"]],
	     1/length(pop_init[["dem_params"]][[x]][["rec_wk"]]), 0)

res <- delay_diff(K = pop_init[["dem_params"]][[x]][["K"]], F = spat_fs[[x]], 
	   M = pop_init[["dem_params"]][[x]][["M"]]/365, 
	   wt = 1, wtm1 = 0.1, R = Rec[[x]], B = B[[x]],
          Bm1 = Bm1[[x]], al = al,  alm1 = alm1)

}
names(Bp1) <- paste0("spp", seq_len(n_spp))

Bm1 <- B  #record at location
B <- Bp1

####################
## scientific survey
####################

if(sim_init[["brk.idx"]][["day.breaks"]][t] %in% survey[["log.mat"]][,"day"] & !is.null(survey)) {

print("undertaking scientific survey") 

# doy and y
  doy <- sim_init[["brk.idx"]][["day.breaks"]][t]
  y   <- sim_init[["brk.idx"]][["year.breaks"]][t]

	# survey log
	log.mat <- survey[["log.mat"]]

	# survey locations
	x_loc <- log.mat[log.mat[,"day"] == doy & log.mat[,"year"] == y, "x"]
	y_loc <- log.mat[log.mat[,"day"] == doy & log.mat[,"year"] == y, "y"]

	# For each set of locations
	for(i in seq_len(length(x_loc))) {
		
# For each species
for(s in seq_len(n_spp)) {
log.mat[log.mat[,"day"]==doy & log.mat[,"year"]==y,paste0("spp",s)][i]  <-  	B[[s]][x_loc[i],y_loc[i]] * as.numeric(survey[["survey_settings"]][[paste0("Qs.spp",s)]])
}
	}

	# return log.mat to the list
	survey[["log.mat"]] <- log.mat

}


} # end if statement

#######################
##### Pop movement ####
#######################
# occurs every week, 
# uses move_population: inputs, move_Prop (movement probabilities) and Start_Pop (current population)
# uses hab or hab_spwn to calc the move probabilities, move_prob_Lst()
# while in spwn_wk, use hab.spwn for the move_prob_Lst(), else use hab 


if(Pop_move) {
	print("Moving populations")

	### With covariates ###

	## If we've spatiotemporal movement covariate, include here:
	if(!is.null(move_cov)) {
	
	## The temperature covariates for the week
	move_cov_wk <- move_cov[["cov.matrix"]][[week.breaks[t]]]

		
	B <- foreach(s = paste0("spp", seq_len(n_spp))) %dopar% {

	if(move_cov[["spp_assoc"]][[s]] == -1) { move_cov_wk <- abs(1 - move_cov_wk)}
	
	## If in a non-spawning week or spawning week
	if(!week.breaks[t] %in% pop_init[["dem_params"]][[s]][["spwn_wk"]]) {
	newPop <- move_population(moveProp = lapply(lapply(MoveProb[[s]], function(x) x * move_cov_wk), function(x1) x1/sum(x1)),
				  StartPop = Bp1[[s]]) 
	}
	
	if(week.breaks[t] %in% pop_init[["dem_params"]][[s]][["spwn_wk"]]) {
	newPop <- move_population(moveProp = lapply(lapply(MoveProb_spwn[[s]], function(x) x * move_cov_wk), function(x1) x1/sum(x1)),
				  StartPop = Bp1[[s]])
	}
	
	Reduce("+", newPop)
		
	}
	
	
	## Also need to move the previous month biomass, so the f calcs match
	## as an input to the delay diff
	Bm1 <- foreach(s = paste0("spp", seq_len(n_spp))) %dopar% {
		
	if(move_cov[["spp_assoc"]][[s]] == -1) { move_cov_wk <- abs(1 - move_cov_wk)}


	## If in a non-spawning week or spawning week
	if(!week.breaks[t] %in% pop_init[["dem_params"]][[s]][["spwn_wk"]]) {
	newPop <- move_population(lapply(lapply(MoveProb[[s]], function(x) x * move_cov_wk), function(x1) x1/sum(x1)),
			          StartPop = Bm1[[s]])
	}

	if(week.breaks[t] %in% pop_init[["dem_params"]][[s]][["spwn_wk"]]) {
	newPop <- move_population(lapply(lapply(MoveProb_spwn[[s]], function(x) x * move_cov_wk), function(x1) x1/sum(x1)),
				   StartPop = Bm1[[s]])
	}

	Reduce("+", newPop)
	
	}

	}


	### No covariates ###

	if(is.null(move_cov)) {

	B <- foreach(s = paste0("spp", seq_len(n_spp))) %dopar% {
	
	## If in a non-spawning week or spawning week
	if(!week.breaks[t] %in% pop_init[["dem_params"]][[s]][["spwn_wk"]]) {
	newPop <- move_population(moveProp = MoveProb[[s]], StartPop = Bp1[[s]]) 
	}
	
	if(week.breaks[t] %in% pop_init[["dem_params"]][[s]][["spwn_wk"]]) {
	newPop <- move_population(moveProp = MoveProb_spwn[[s]], StartPop = Bp1[[s]])
	}
	
	Reduce("+", newPop)
		
	}
	
	
	## Also need to move the previous month biomass, so the f calcs match
	## as an input to the delay diff
	Bm1 <- foreach(s = paste0("spp", seq_len(n_spp))) %dopar% {

	## If in a non-spawning week or spawning week
	if(!week.breaks[t] %in% pop_init[["dem_params"]][[s]][["spwn_wk"]]) {
	newPop <- move_population(moveProp = MoveProb[[s]], StartPop = Bm1[[s]])
	}

	if(week.breaks[t] %in% pop_init[["dem_params"]][[s]][["spwn_wk"]]) {
	newPop <- move_population(moveProp = MoveProb_spwn[[s]], StartPop = Bm1[[s]])
	}

	Reduce("+", newPop)
	
	}

	}

	names(B) <- paste0("spp", seq_len(n_spp))
	names(Bm1) <- paste0("spp", seq_len(n_spp))

## If we're saving population biomass, do so here
if(save_pop_bio == TRUE) {
if(!"pop_bios" %in% ls()) {
# Create a list structure for storing pop bios
pop_bios <- vector("list", sim_init[["idx"]][["ny"]] * sim_init[["idx"]][["nw"]]) 
dim(pop_bios) <- c(sim_init[["idx"]][["ny"]] , sim_init[["idx"]][["nw"]])
pop_bios[[year.breaks[t], week.breaks[t]]] <- B
}

if("pop_bios" %in% ls()) {
pop_bios[[year.breaks[t], week.breaks[t]]] <- B
	  }

}

if(save_pop_bio == FALSE) {
pop_bios <- NULL
}



	} # end if statement



######################
##### Update #########
######################

# Update weekly / annual records at pop level

if(Update) {
print("Recording metrics")

for(s in paste0("spp", seq_len(n_spp))) {


pop_init[["Pop_record"]][[s]][["F.mat"]][year.breaks[t], day.breaks[t]] <- weighted.mean(spat_fs[[s]], B[[s]])

pop_init[["Pop_record"]][[s]][["Catch.mat"]][year.breaks[t], day.breaks[t]] <- sum(spp_catches[[s]])

pop_init[["Pop_record"]][[s]][["Bio.mat"]][year.breaks[t], day.breaks[t]] <- sum(Bp1[[s]])

pop_init[["Pop_record"]][[s]][["Rec.mat"]][1, year.breaks[t]] <- sum(Rec[[s]], pop_init[["Pop_record"]][[s]][["Rec.mat"]][1, year.breaks[t]], na.rm = T)


}


}


} # end loop control

end.time <- Sys.time()
time.taken <- end.time - start.time
print(paste("time taken is :", format(time.taken, units = "auto"), sep = " "))

return(list(fleets_catches = catches, pop_summary = pop_init[["Pop_record"]], pop_bios = pop_bios, survey = survey))

} # end func





