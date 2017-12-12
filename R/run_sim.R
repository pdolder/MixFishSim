#' @title Run sim

#' @description \code{run_sim} is the overarching simulation function, taking
#' all the parameterised inputs and returning the results.

#' @param sim_init is the parameterised simulation settings from
#' \code{init_sim}
#' @param pop_init is the parameterised populations from \code{init_pop}
#' @param fleets_init is the parameterised fleets from \code{init_fleets}
#' @param hab_init is the parameterised habitat maps from \code{create_hab}
#' @param InParallel is a BOLEEN indicating whether calculations should be done
#' using parallel processing from \code{parallel}, default is TRUE

#' @return is the results...

#' @examples Not yet
#'
#' @export

run_sim <- function (sim_init = NULL, pop_init = NULL, fleets_init = NULL, hab_init = NULL, InParallel = TRUE, cores = 3, ...) {
# Overarching function for running the simulations

require(doParallel)
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


###################################
###### Move probabilities #########
###################################
print("Calculating movement probabilities")

MoveProb  <- foreach(s = paste0("spp", seq_len(n_spp)))  %dopar% move_prob_Lst(lambda = 0.3, hab = hab_init[["hab"]][[s]])
MoveProb_spwn <- foreach(s = paste0("spp", seq_len(n_spp)))  %dopar% move_prob_Lst(lambda = 0.3, hab = hab_init[["spwn_hab"]][[s]])
	  
names(MoveProb)      <- paste0("spp", seq_len(n_spp))
names(MoveProb_spwn) <- paste0("spp", seq_len(n_spp))

##################
### loop control #
for (t in seq_len(ntow)) {
##################
print(paste("tow ==", t))
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
		   sp_fleets_catches =  fleets_init[["sp_fleet_catches"]][[fl]],
		   pops = B, t = t)


} # end t==1 run

if(t > 1) {

# if its the same day 
if(day.breaks[t] == day.breaks[t-1]) {
catches <- foreach(fl=seq_len(n_fleets)) %dopar% 

	go_fish_fleet(FUN = go_fish, 	sim_init = sim_init, 
			fleets_params = fleets_init[["fleet_params"]][[fl]],
		   fleets_catches =     catches[[fl]][["fleets_catches"]], 
		   sp_fleets_catches =  catches[[fl]][["sp_fleets_catches"]],
		   pops = B, t = t
		   )

	} # end same week run

# if its a new day - reset the spatial catches counter
if(day.breaks[t] != day.breaks[t-1]) {

catches <- foreach(fl=seq_len(n_fleets)) %dopar% 

  go_fish_fleet(FUN = go_fish, 
		sim_init = sim_init, 
			fleets_params = fleets_init[["fleet_params"]][[fl]],
		   fleets_catches =     catches[[fl]][["fleets_catches"]], 
		   sp_fleets_catches =  fleets_init[["sp_fleet_catches"]][[fl]],
		   pops = B, t = t)

	       


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


print(sapply(1:2, function(x) { max(spp_catches[[x]] / B[[x]], na.rm = T) }))

spat_fs <- find_spat_f_pops(sim_init = sim_init, C = spp_catches, B = B, 
                            dem_params = pop_init[["dem_params"]])

#print(sapply(spp_catches, sum))
#print(sapply(spat_fs, mean))

# Apply the delay difference model
Bp1 <- foreach(x = paste0("spp", seq_len(n_spp))) %dopar% {

al   <- ifelse(week.breaks[t] %in% pop_init[["dem_params"]][[x]][["rec_wk"]],
	     1/length(pop_init[["dem_params"]][[x]][["rec_wk"]]), 0)
alm1 <- ifelse(c(week.breaks[t]-1) %in% pop_init[["dem_params"]][[x]][["rec_wk"]],
	     1/length(pop_init[["dem_params"]][[x]][["rec_wk"]]), 0)

res <- delay_diff(K = 0.3, F = spat_fs[[x]], 
	   M = pop_init[["dem_params"]][[x]][["M"]]/365, 
	   wt = 1, wtm1 = 0.1, R = Rec[[x]], B = B[[x]],
          Bm1 = Bm1[[x]], al = al,  alm1 = alm1)

}
names(Bp1) <- paste0("spp", seq_len(n_spp))

Bm1 <- B  #record at location
B <- Bp1

print("done")

} # end if statement

#######################
##### Pop movement ####
#######################
# occurs every week, 
# uses move_population: inputs, move_Prop (movement probabilities) and Start_Pop (current population)
# uses hab or hab_spwn to calc the move probabilities, move_prob_Lst()
# while in spwn_wk, use hab.spwn for the move_prob_Lst(), else use hab 


if(Pop_move) {
	print("Moving")

	B <- foreach(s = paste0("spp", seq_len(n_spp))) %dopar% {
	
	## If in a non-spawning week or spawning week
	if(!week.breaks[t] %in% pop_init[[s]][["spwn_wk"]]) {
	newPop <- move_population(moveProp = MoveProb[[s]], StartPop = Bp1[[s]]) 
	}
	
	if(week.breaks[t] %in% pop_init[[s]][["spwn_wk"]]) {
	newPop <- move_population(moveProp = MoveProb_spwn[[s]], StartPop = Bp1[[s]])
	}
	
	Reduce("+", newPop)
		
	}
	
	
	## Also need to move the previous month biomass, so the f calcs match
	## as an input to the delay diff
	Bm1 <- foreach(s = paste0("spp", seq_len(n_spp))) %dopar% {

	## If in a non-spawning week or spawning week
	if(!week.breaks[t] %in% pop_init[[s]][["spwn_wk"]]) {
	newPop <- move_population(moveProp = MoveProb[[s]], StartPop = Bm1[[s]])
	}

	if(week.breaks[t] %in% pop_init[[s]][["spwn_wk"]]) {
	newPop <- move_population(moveProp = MoveProb_spwn[[s]], StartPop = Bm1[[s]])
	}

	Reduce("+", newPop)
	
	}

	names(B) <- paste0("spp", seq_len(n_spp))
	names(Bm1) <- paste0("spp", seq_len(n_spp))


	print("done")

	} # end if statement



######################
##### Update #########
######################

# Update weekly / annual records at pop level

if(Update) {
print("Updating")

for(s in paste0("spp", seq_len(n_spp))) {

#print(sum(spat_fs[[s]]))

print(paste("year break  = ", year.breaks[t]))
print(paste("day break = ", day.breaks[t]))
print("F.mat")
pop_init[["Pop_record"]][[s]][["F.mat"]][year.breaks[t], day.breaks[t]] <- mean(spat_fs[[s]])

print("Catch.mat")
pop_init[["Pop_record"]][[s]][["Catch.mat"]][year.breaks[t], day.breaks[t]] <- sum(spp_catches[[s]])

print("Bio.mat")
pop_init[["Pop_record"]][[s]][["Bio.mat"]][year.breaks[t], day.breaks[t]] <- sum(Bp1[[s]])

print("Rec.mat")
pop_init[["Pop_record"]][[s]][["Rec.mat"]][1, year.breaks[t]] <- sum(Rec[[s]], pop_init[["Pop_record"]][[s]][["Rec.mat"]][1, year.breaks[t]], na.rm = T)


}


}


} # end loop control


return(list(fleets_catches = catches, pop_summary = pop_init[["Pop_record"]]))

} # end func





