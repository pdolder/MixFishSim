#' @title Go fish

#' @description \code{go_fish} is a function used to apply the fishing
#' simulation model

#' @param sim_init is the initialised object from \code{init_sim}.
#' @param fleet_params is the parameter settings initialised from \code{_init_fleets}
#' @param fleet_catches is the DF initialised from \code{_init_fleets}
#' @param sp_fleet_catches is a list of spatial catches (as a Numeric matrix) for the fleet of each
#' population
#' 

#' @return is a list containing i) the fleet catch dataframes , ii) the spatial
#' catches of each population

#' @examples
#'

#' @export

go_fish <- function(sim_init = NULL, fleet_params = NULL, fleet_catches = NULL, 
		    sp_fleet_catches = NULL, pops = NULL, t = t) {

##### extract the relevant components ###########
params <- fleet_params      # fleet parameter list
catch  <- fleet_catches     # fleet catches list
catch_matrix <- sp_fleet_catches

VPT <- params[["VPT"]] 			# Value per tonne
Q <- params[["Qs"]]			# Catchability for vessel
if(length(VPT) != length(Q)) stop("VPT and Q must be the same length")

# 
PastKnowledge <- params[["past_knowledge"]]

######## indexes ##############
idx <- sim_init$idx
brk.idx <- sim_init$brk.idx
###############################

##### past knowledge decisions ####

## If its the first location fished, need to choose a random location
if(t == 1) {
coords <- c(round(runif(1,1,idx[["nrows"]])),round(runif(1,1,idx[["ncols"]]))) # Choose a random lat and lon
}

# For t > 1
if(t > 1) {

coords <- c(catch[t-1, "x"], catch[t-1,"y"]) # Previous coordinates

# If incorporating past knowledge, and its a new trip...and not in the
	# first year
	if(!is.null(PastKnowledge) & catch[t,"trip"] != catch[t-1,"trip"] & brk.idx[["year.breaks"]][t]>1)  {

## print("USING PAST KNOWLEDGE!!!")
	
		catch.df  <- as.data.frame(catch) # Needed for correct sub-setting

	# 3 options, choose from good hauls i) same month last year, ii) past
		# trip, or iii) combination of same month last year and past
		# trip
	
	if(PastKnowledge & any(is.null(params[["past_year_month"]]) | is.null(params[["past_trip"]]) | is.null(params[["threshold"]]))) stop("Must
	    specify whether the past knowledge of fishing grounds is based
	    on last year, last trip or both, and a threshold for 'good fishin'")
	
	#####################################################################
	# Option 1 - same month (current and) previous years
	if(params[["past_year_month"]] == TRUE & params[["past_trip"]] != TRUE) {
	
	q <- quantile(dplyr::filter(catch.df,month==brk.idx[["month.breaks"]][t])$val,prob=c(params[["threshold"]]),na.rm=T) # Threshold for good hauls
	goodhauls  <- dplyr::filter(catch.df,month==brk.idx[["month.breaks"]][t] & val>=q)
	goodhauls  <- goodhauls[complete.cases(goodhauls),] # Remove NAs
	new.point   <- sample(paste(goodhauls$x,goodhauls$y,sep=","),1)  	# Randomly select from the good hauls
	new.point   <- c(as.numeric(sapply(strsplit(new.point,","),"[",1)),as.numeric(sapply(strsplit(new.point,","),"[",2)))
		}

        #######################################################################
	# Option 2 - last trip
	if(params[["past_year_month"]] != TRUE & params[["past_trip"]] == TRUE) {
	
	# Case where its within the same year
	if(brk.idx[["year.breaks"]][t] == brk.idx[["year.breaks"]][t-1]) {
	q <- quantile(dplyr::filter(catch.df,trip==brk.idx[["trip.breaks"]][t-1] & year==brk.idx[["year.breaks"]][t])$val,prob=c(params[["threshold"]]),na.rm=T) # Threshold for good hauls
	goodhauls  <- dplyr::filter(catch.df,trip==brk.idx[["trip.breaks"]][t-1] & year==brk.idx[["year.breaks"]][t] & val>=q)
	goodhauls  <- goodhauls[complete.cases(goodhauls),] # Remove NAs

	}
	
	# Case of different year
	if(brk.idx[["year.breaks"]][t] != brk.idx[["year.breaks"]][t-1]) {
	q <- quantile(dplyr::filter(catch.df,trip == max(brk.idx[["trip.breaks"]]) & year == brk.idx[["year.breaks"]][t-1])$val,prob=c(params[["threshold"]]),na.rm=T) # Threshold for good hauls
	goodhauls  <- dplyr::filter(catch.df,trip == max(brk.idx[["trip.breaks"]]) & year == brk.idx[["year.breaks"]][t-1] & val>=q)
	goodhauls  <- goodhauls[complete.cases(goodhauls),] # Remove NAs
	}

	new.point   <- sample(paste(goodhauls$x,goodhauls$y,sep=","),1)
	new.point   <- c(as.numeric(sapply(strsplit(new.point,","),"[",1)),as.numeric(sapply(strsplit(new.point,","),"[",2)))

	}
  	
	################################################################
	# Option 3 - combination same month previous years and past trip
	if(params[["past_year_month"]] == TRUE & params[["past_trip"]] == TRUE) {
	
	# Case where its within the same year
	if(brk.idx[["year.breaks"]][t] == brk.idx[["year.breaks"]][t-1]) {
	q <- quantile(dplyr::filter(catch.df,month==brk.idx[["month.breaks"]][t] | trip==brk.idx[["trip.breaks"]][t-1] & year==brk.idx[["year.breaks"]][t])$val,prob=c(params[["threshold"]]),na.rm=T) # Threshold for good hauls
	goodhauls  <- dplyr::filter(catch.df,month==brk.idx[["month.breaks"]][t] & val >=q | trip==brk.idx[["trip.breaks"]][t-1] & year==brk.idx[["year.breaks"]][t] & val>=q)
	goodhauls  <- goodhauls[complete.cases(goodhauls),] # Remove NAs
	}
	
	# Case of different year
	if(brk.idx[["year.breaks"]][t] != brk.idx[["year.breaks"]][t-1]) {
	q <- quantile(dplyr::filter(catch.df,month==brk.idx[["month.breaks"]][t] | trip == max(brk.idx[["trip.breaks"]]) & year == brk.idx[["year.breaks"]][t-1])$val,prob=c(params[["threshold"]]),na.rm=T) # Threshold for good hauls
	goodhauls  <- dplyr::filter(catch.df,month==brk.idx$month.breaks[t] & val >=q | trip == max(brk.idx[["trip.breaks"]]) & year == brk.idx[["year.breaks"]][t-1] & val>=q)
	goodhauls  <- goodhauls[complete.cases(goodhauls),] # Remove NAs

	}

	new.point   <- sample(paste(goodhauls$x,goodhauls$y,sep=","),1)
	new.point   <- c(as.numeric(sapply(strsplit(new.point,","),"[",1)),as.numeric(sapply(strsplit(new.point,","),"[",2)))

	}

	}

	# CRW when no past knowledge, or within same month/trip (depending on
	# choice)
	if(!PastKnowledge | catch[t,"trip"] == catch[t-1,"trip"] | brk.idx[["year.breaks"]][t]==1) {
	
	stepD     <- step_length(revenue = catch[t-1,"val"],step_params = params[["step_params"]])  # Calculate step distance based on last tow value
        catch[t,"stepD"] <- stepD    # record the step distance	

	## Choosing new bearing
	# with boundary conditions, when out of bounds wrap on a taurus 
	
	#Bear = runif(1,0,360) # bearing - to be replaced with a correlated von mises dist
	b <- ifelse(t = 1, 0, catch[t-1,"angles"]) # base on most recent bearing
	k = 50/params[["step_params"]][["B3"]] + 1 # concentration parameter, a proportion of the maximum haul + 1 (to avoid infinity)
	Bear <- get_bearing(b = b, k = k)
	catch[t, "angles"] <- Bear
	new.point <- round(make_step(stepD = stepD, Bear = Bear, start.x = coords[1], start.y = coords[2])) # returns c(x2,y2)

	## Boundary condition
	if(new.point[2] > idx[["ncols"]]) { new.point[2]  <-  new.point[2] - idx[["ncols"]]}
	if(new.point[1] > idx[["nrows"]]) { new.point[1]  <-  new.point[1] - idx[["nrows"]]}
	if(new.point[2] < 1) { new.point[2]  <-  new.point[2] + idx[["ncols"]]}
	if(new.point[1] < 1) { new.point[1]  <-  new.point[1] + idx[["nrows"]]}

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

	catch[t, "meanval"] <- mean(catch[seq(t),"val"]) # Update mean value
	catch[t, "sdval"]   <- ifelse(is.na(sd(catch[seq(t),"val"])),1, sd(catch[seq(t),"val"]))  # Update the SD of the catch
#	print(paste("tow",t,"=",round(catch[t,"val"],0),", mean = ",round(catch[t, "meanval"],0),"euros",",  ",round((t/idx[["ntow"]])*100,0),"% complete"))


res <- list(catch = catch, catch_matrices = catch_matrix)
return(res)

} # End go fish function

