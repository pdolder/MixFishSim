#' @title Go fish

#' @description \code{go_fish} is a function used to apply the fishing
#' simulation model

#' @param sim_init is the initialised object from \code{init_sim}.
#' @param fleet_params is the parameter settings initialised from \code{_init_fleets}
#' @param fleet_catches is the DF initialised from \code{_init_fleets}
#' @param sp_fleet_catches is a list of spatial catches (as a Numeric matrix)
#' for the fleet of each population @param closed_areas is a dataframe with the
#' x,y coordinates are any closed areas, provided internally by
#' \link{close_areas}
#' 

#' @return is a list containing i) the fleet catch dataframes , ii) the spatial
#' catches of each population

#' @examples
#'

#' @export

go_fishOLD <- function(sim_init = NULL, fleet_params = NULL, fleet_catches = NULL, 
		    sp_fleet_catches = NULL, pops = NULL, closed_areas = NULL, t = t) {

##### extract the relevant components ###########
params <- fleet_params      # fleet parameter list
catch  <- fleet_catches     # fleet catches list
catch_matrix <- sp_fleet_catches
VPT <- params[["VPT"]] 			# Value per tonne
Q <- params[["Qs"]]			# Catchability for vessel
if(length(VPT) != length(Q)) stop("VPT and Q must be the same length")
fuelC <- params[["fuelC"]]

######## indexes ##############
idx <- sim_init$idx
brk.idx <- sim_init$brk.idx
###############################
##### past knowledge decisions ####
PastKnowledge <- params[["past_knowledge"]]  # overall flag if past knowledge used
UseKnowledge  <- use_past_knowledge(p = logistic(Q = 200, B = 0.02/idx[["ny"]], t = t))    # specific flag for this timestep whether past knowledge being used in transition
##################################

## If its the first location fished, need to choose a random location
if(t == 1) {
coords <- c(round(runif(1,1,idx[["nrows"]])),round(runif(1,1,idx[["ncols"]]))) # Choose a random lat and lon
}
	

# For t > 1
if(t > 1) {

coords <- c(catch[t-1, "x"], catch[t-1,"y"]) # Previous coordinates

# If incorporating past knowledge, and its a new trip...and not in the first year
	if(!is.null(PastKnowledge) & catch[t,"trip"] != catch[t-1,"trip"] & brk.idx[["year.breaks"]][t]>1)  {

## print("USING PAST KNOWLEDGE!!!")
	
	## Need to determine start location by including the distance to
	## fishing grounds, calculate the expected profit by including fuel costs
	loc_choice <- as.data.frame(catch)
	loc_choice$loc_dist <- mapply(x1 = 0, y1 = 0, x2 = loc_choice$x, y2 = loc_choice$y, FUN = distance_calc)
	loc_choice$expec_prof <- loc_choice$val - (loc_choice$loc_dist * fuelC)

	# 3 options, choose from good hauls i) same month last year, ii) past
		# trip, or iii) combination of same month last year and past
		# trip
	
	if(PastKnowledge & any(is.null(params[["past_year_month"]]) | is.null(params[["past_trip"]]) | is.null(params[["threshold"]]))) stop("Must
	    specify whether the past knowledge of fishing grounds is based
	    on last year, last trip or both, and a threshold for 'good fishin'")
	
	#####################################################################
	# Option 1 - same month (current and) previous years
	if(params[["past_year_month"]] == TRUE & params[["past_trip"]] != TRUE) {

	q <- quantile(dplyr::filter(loc_choice,month==brk.idx[["month.breaks"]][t])$expec_prof,prob=c(params[["threshold"]]),na.rm=T) # Threshold for good hauls
	goodhauls  <- dplyr::filter(loc_choice,month==brk.idx[["month.breaks"]][t] & expec_prof>=q)
	goodhauls  <- goodhauls[complete.cases(goodhauls),] # Remove NAs

	## Exclude any closed areas from the good haul list
	goodhauls <- goodhauls[!paste(goodhauls$x, goodhauls$y) %in% paste(closed_areas$x, closed_areas$y),]

	if(dim(goodhauls)[1]==0) {
	
	q <- quantile(dplyr::filter(loc_choice,month==brk.idx[["month.breaks"]][t])$expec_prof,prob=c(0.05),na.rm=T) # Threshold for good hauls

	goodhauls  <- dplyr::filter(loc_choice,month==brk.idx[["month.breaks"]][t] & expec_prof>=q)
	goodhauls  <- goodhauls[complete.cases(goodhauls),] # Remove NAs

	## Exclude any closed areas from the good haul list
	goodhauls <- goodhauls[!paste(goodhauls$x, goodhauls$y) %in% paste(closed_areas$x, closed_areas$y),]
	
	}

	# Find area and make sure its not closed
	Closure <- TRUE; count <- 1 
	while(Closure == TRUE) {
		
		if(dim(goodhauls)[1] == 0) {   ## If we still can't find any good hauls despite lowering the threshold, choose at random
	new.point <- c(round(runif(1, 1, idx[["nrows"]])), round(runif(1, 1, idx[["ncols"]])))
			    }

		if(dim(goodhauls)[1] != 0) {
	new.point   <- sample(paste(goodhauls$x,goodhauls$y,sep=","),1)  	# Randomly select from the good hauls
	new.point   <- c(as.numeric(sapply(strsplit(new.point,","),"[",1)),as.numeric(sapply(strsplit(new.point,","),"[",2)))
		}
	## Check for closed areas
	cl <- apply(closed_areas, 1, function(x) {
			    x["x"] == new.point[1] & 
			    x["y"] == new.point[2]})

	# If new.point is in closure areas, repick, else break
	Closure <- ifelse(any(cl), TRUE, FALSE) 
	if(Closure == TRUE) {## print(paste("Stuck on option 1", count))
	count <- count+1 }
	if(Closure == FALSE) break
	}
		}

        #######################################################################
	# Option 2 - last trip
	if(params[["past_year_month"]] != TRUE & params[["past_trip"]] == TRUE) {
	
	# Case where its within the same year
	if(brk.idx[["year.breaks"]][t] == brk.idx[["year.breaks"]][t-1]) {
		
	q <- quantile(dplyr::filter(loc_choice,trip==brk.idx[["trip.breaks"]][t-1] & year==brk.idx[["year.breaks"]][t])$expec_prof,prob=c(params[["threshold"]]),na.rm=T) # Threshold for good hauls
	goodhauls  <- dplyr::filter(loc_choice,trip==brk.idx[["trip.breaks"]][t-1] & year==brk.idx[["year.breaks"]][t] & expec_prof>=q)
	goodhauls  <- goodhauls[complete.cases(goodhauls),] # Remove NAs

	}
	
	# Case of different year
	if(brk.idx[["year.breaks"]][t] != brk.idx[["year.breaks"]][t-1]) {
	q <- quantile(dplyr::filter(loc_choice,trip == max(brk.idx[["trip.breaks"]]) & year == brk.idx[["year.breaks"]][t-1])$expec_prof,prob=c(params[["threshold"]]),na.rm=T) # Threshold for good hauls
	goodhauls  <- dplyr::filter(loc_choice,trip == max(brk.idx[["trip.breaks"]]) & year == brk.idx[["year.breaks"]][t-1] & expec_prof>=q)
	goodhauls  <- goodhauls[complete.cases(goodhauls),] # Remove NAs
	}

	## Exclude any closed areas from the good haul list
	goodhauls <- goodhauls[!paste(goodhauls$x, goodhauls$y) %in% paste(closed_areas$x, closed_areas$y),]

	if(dim(goodhauls)[1] == 0) {
	
	# Case where its within the same year
	if(brk.idx[["year.breaks"]][t] == brk.idx[["year.breaks"]][t-1]) {
	q <- quantile(dplyr::filter(loc_choice,trip==brk.idx[["trip.breaks"]][t-1] & year==brk.idx[["year.breaks"]][t])$expec_prof,prob=c(0.05),na.rm=T) # Threshold for good hauls
	goodhauls  <- dplyr::filter(loc_choice,trip==brk.idx[["trip.breaks"]][t-1] & year==brk.idx[["year.breaks"]][t] & expec_prof>=q)
	goodhauls  <- goodhauls[complete.cases(goodhauls),] # Remove NAs

	}
	
	# Case of different year
	if(brk.idx[["year.breaks"]][t] != brk.idx[["year.breaks"]][t-1]) {
	q <- quantile(dplyr::filter(loc_choice,trip == max(brk.idx[["trip.breaks"]]) & year == brk.idx[["year.breaks"]][t-1])$expec_prof,prob=c(0.05),na.rm=T) # Threshold for good hauls
	goodhauls  <- dplyr::filter(loc_choice,trip == max(brk.idx[["trip.breaks"]]) & year == brk.idx[["year.breaks"]][t-1] & expec_prof>=q)
	goodhauls  <- goodhauls[complete.cases(goodhauls),] # Remove NAs
	}
	
	}

	# Find area and make sure its not closed
	Closure <- TRUE; count <- 1
	while(Closure == TRUE ) {
	
	if(dim(goodhauls)[1] == 0) {   ## If we still can't find any good hauls despite lowering the threshold, choose at random
	new.point <- c(round(runif(1, 1, idx[["nrows"]])), round(runif(1, 1, idx[["ncols"]])))
			    }
	if(dim(goodhauls)[1] != 0) {  

	new.point   <- sample(paste(goodhauls$x,goodhauls$y,sep=","),1)
	new.point   <- c(as.numeric(sapply(strsplit(new.point,","),"[",1)),as.numeric(sapply(strsplit(new.point,","),"[",2)))

	}

	## Check for closed areas
	cl <- apply(closed_areas, 1, function(x) {
			    x["x"] == new.point[1] & 
			    x["y"] == new.point[2]})

	# If new.point is in closure areas, repick, else break
	Closure <- ifelse(any(cl), TRUE, FALSE) 
	if(Closure == TRUE) { 
	count <- count+1 }

	if(Closure == FALSE) break

	}

	}
  	
	##################################################################
	# Option 3 - combination same month previous years and past trip #
	##################################################################
	if(params[["past_year_month"]] == TRUE & params[["past_trip"]] == TRUE) {
	
	# Case where its within the same year
	if(brk.idx[["year.breaks"]][t] == brk.idx[["year.breaks"]][t-1]) {
	q <- quantile(dplyr::filter(loc_choice,month==brk.idx[["month.breaks"]][t] | trip==brk.idx[["trip.breaks"]][t-1] & year==brk.idx[["year.breaks"]][t])$expec_prof,prob=c(params[["threshold"]]),na.rm=T) # Threshold for good hauls
	goodhauls  <- dplyr::filter(loc_choice,month==brk.idx[["month.breaks"]][t] & expec_prof >=q | trip==brk.idx[["trip.breaks"]][t-1] & year==brk.idx[["year.breaks"]][t] & expec_prof>=q)
	goodhauls  <- goodhauls[complete.cases(goodhauls),] # Remove NAs
	}
	
	# Case of different year
	if(brk.idx[["year.breaks"]][t] != brk.idx[["year.breaks"]][t-1]) {
	q <- quantile(dplyr::filter(loc_choice,month==brk.idx[["month.breaks"]][t] | trip == max(brk.idx[["trip.breaks"]]) & year == brk.idx[["year.breaks"]][t-1])$expec_prof,prob=c(params[["threshold"]]),na.rm=T) # Threshold for good hauls
	goodhauls  <- dplyr::filter(loc_choice,month==brk.idx$month.breaks[t] & expec_prof >=q | trip == max(brk.idx[["trip.breaks"]]) & year == brk.idx[["year.breaks"]][t-1] & expec_prof>=q)
	goodhauls  <- goodhauls[complete.cases(goodhauls),] # Remove NAs

	}
	
	## Exclude any closed areas from the good haul list
	goodhauls <- goodhauls[!paste(goodhauls$x, goodhauls$y) %in% paste(closed_areas$x, closed_areas$y),]

	## But if we're left with nowhere to fish, we need to lower the
	## threshold

	if(dim(goodhauls)[1] == 0) {
	
	# Case where its within the same year
	if(brk.idx[["year.breaks"]][t] == brk.idx[["year.breaks"]][t-1]) {
	q <- quantile(dplyr::filter(loc_choice,month==brk.idx[["month.breaks"]][t] | trip==brk.idx[["trip.breaks"]][t-1] & year==brk.idx[["year.breaks"]][t])$expec_prof,prob=c(0.05),na.rm=T) # Threshold for good hauls
	goodhauls  <- dplyr::filter(loc_choice,month==brk.idx[["month.breaks"]][t] & expec_prof >=q | trip==brk.idx[["trip.breaks"]][t-1] & year==brk.idx[["year.breaks"]][t] & expec_prof >= q)
	goodhauls  <- goodhauls[complete.cases(goodhauls),] # Remove NAs
	}
	
	# Case of different year
	if(brk.idx[["year.breaks"]][t] != brk.idx[["year.breaks"]][t-1]) {
	q <- quantile(dplyr::filter(loc_choice,month==brk.idx[["month.breaks"]][t] | trip == max(brk.idx[["trip.breaks"]]) & year == brk.idx[["year.breaks"]][t-1])$expec_prof,prob=c(0.05),na.rm=T) # Threshold for good hauls
	goodhauls  <- dplyr::filter(loc_choice,month==brk.idx$month.breaks[t] & expec_prof >=q | trip == max(brk.idx[["trip.breaks"]]) & year == brk.idx[["year.breaks"]][t-1] & expec_prof >= q)
	goodhauls  <- goodhauls[complete.cases(goodhauls),] # Remove NAs

	}
	
	## Exclude any closed areas from the good haul list
	goodhauls <- goodhauls[!paste(goodhauls$x, goodhauls$y) %in% paste(closed_areas$x, closed_areas$y),]

	}

	# Find area and make sure its not closed
	Closure <- TRUE; count <- 1
	while(Closure == TRUE ) {
	
	if(dim(goodhauls)[1] == 0) {   ## If we still can't find any good hauls, choose at random
	new.point <- c(round(runif(1, 1, idx[["nrows"]])), round(runif(1, 1, idx[["ncols"]])))
			    }

	if(dim(goodhauls)[1] != 0) {
	new.point   <- sample(paste(goodhauls$x,goodhauls$y,sep=","),1)
	new.point   <- c(as.numeric(sapply(strsplit(new.point,","),"[",1)),as.numeric(sapply(strsplit(new.point,","),"[",2)))
}

	## Check for closed areas
	cl <- apply(closed_areas, 1, function(x) {
			    x["x"] == new.point[1] & 
			    x["y"] == new.point[2]})

	# If new.point is in closure areas, repick, else break
	Closure <- ifelse(any(cl), TRUE, FALSE) 
	if(Closure == TRUE) {## print(paste("Stuck on option 3", count))
	count <- count+1 }

	if(Closure == FALSE) break

	}

	}

	}

	# CRW when no past knowledge, or within same month/trip (depending on
	# choice)
	if(!PastKnowledge | catch[t,"trip"] == catch[t-1,"trip"] | brk.idx[["year.breaks"]][t]==1) {


	## Here we need to update the max value in the step param, for the
		## current population size / value field


		ValMat <- 		lapply(names(pops), function(x) {
					  val_mat <- Q[[x]] * pops[[x]] * VPT[[x]]
					})
		ValMat <- Reduce("+", ValMat)
		B3_rev <- quantile(ValMat, prob = 0.9)
		params[["step_params"]][["B3"]] <- B3_rev 
	
	Closure <- TRUE; count <- 1
	while(Closure == TRUE ) {

		## Condition to deal with being trapped in closed area
		if(count <101) {

	stepD     <- step_length(revenue = catch[t-1,"val"],step_params = params[["step_params"]])  # Calculate step distance based on last tow value
        catch[t,"stepD"] <- stepD    # record the step distance	

	## Choosing new bearing
	# with boundary conditions, when out of bounds wrap on a taurus 
		# correlated random step and bearing
	b <- ifelse(t = 1, 0, catch[t-1,"angles"]) # base on most recent bearing
	max_k <- 20
	k = ((catch[t-1,"val"]+1) / params[["step_params"]][["B3"]]) * max_k
	# concentration parameter, a proportion of the maximum haul + 1 (to avoid infinity)
	Bear <- get_bearing(b = b, k = k)
	catch[t, "angles"] <- Bear
	new.point <- round(make_step(stepD = stepD, Bear = Bear, start.x = coords[1], start.y = coords[2])) # returns c(x2,y2)

		}
		
		## Condition to deal with being trapped in closed area
		if(count > 100) {
		new.point <- c(round(runif(1, 1, idx[["nrows"]])), round(runif(1, 1, idx[["ncols"]])))
		}

	## Boundary condition
	if(new.point[2] > idx[["ncols"]]) { new.point[2]  <-  new.point[2] - idx[["ncols"]]}
	if(new.point[1] > idx[["nrows"]]) { new.point[1]  <-  new.point[1] - idx[["nrows"]]}
	if(new.point[2] < 1) { new.point[2]  <-  new.point[2] + idx[["ncols"]]}
	if(new.point[1] < 1) { new.point[1]  <-  new.point[1] + idx[["nrows"]]}

	## Check for closed areas
	cl <- apply(closed_areas, 1, function(x) {
			    x["x"] == new.point[1] & 
			    x["y"] == new.point[2]})

	# If new.point is in closure areas, repick, else break
	Closure <- ifelse(any(cl), TRUE, FALSE) 
	if(Closure == TRUE) {## print(paste("Stuck on CRW", count))
	count <- count+1 }

	if(Closure == FALSE) break

	}

	}


coords <- new.point # assign new fishing position

	}

# save coords fished
	catch[t,"x"] <- coords[1]
	catch[t,"y"] <- coords[2]
		
	# sample from catch of species
	for (i in seq(idx[["n.spp"]])) {

	# store for the fleets record
	catch[t,paste("spp",i,sep="")] <- pops[[paste("spp",i,sep="")]][coords[1],coords[2]] * 
		Q[[paste("spp",i,sep="")]] 

	# store for the delay-diff record - add to the passed catch matrix
	catch_matrix[[paste("spp", i, sep ="")]][coords[1], coords[2]]  <-
		
		catch_matrix[[paste("spp", i, sep ="")]][coords[1], coords[2]] +
		pops[[paste("spp",i,sep="")]][coords[1],coords[2]] *
		Q[[paste("spp",i,sep="")]] 

	}
	
	catch[t,"allspp"] <- sum(catch[t,(paste("spp",seq(idx[["n.spp"]]),sep=""))])

	#calculate value of catch
	catch_val <- sapply(seq(idx[["n.spp"]]),function(n) {
	             catch[t,paste("spp",n,sep = "")] * VPT[[paste("spp",n,sep = "")]]
	    })

	catch[t,"val"] <- sum(catch_val)

	# and the costs and profit
	# costs from the distance travelled * fuel cost

	## If its the first tow
	if(t==1) {
	catch[t,"costs"] <- (distance_calc(x1 = 0, y1 = 0,
	x2 = catch[t, "x"], y2 = catch[t, "y"]) * fuelC)
	}
	
	# If not the first tow
	if(t>1) {
	
	# if its a new trip
	if(catch[t,"trip"] != catch[t-1,"trip"]) {
	catch[t,"costs"] <- (distance_calc(x1 = 0, y1 = 0,
	x2 = catch[t, "x"], y2 = catch[t, "y"]) * fuelC)
	   }

	# if its the same trip
		if(catch[t,"trip"] == catch[t-1,"trip"]) {
	catch[t,"costs"] <- (distance_calc(x1 = catch[t-1, "x"], y1 = catch[t-1, "y"],
	x2 = catch[t, "x"], y2 = catch[t, "y"]) * fuelC)
		}

		}
	
	catch[t,"profit"] <- catch[t,"val"] - catch[t,"costs"] 
	catch[t, "meanval"] <- mean(catch[seq(t),"val"]) # Update mean value
	catch[t, "sdval"]   <- ifelse(is.na(sd(catch[seq(t),"val"])),1, sd(catch[seq(t),"val"]))  # Update the SD of the catch

res <- list(catch = catch, catch_matrices = catch_matrix)
return(res)

} # End go fish function
