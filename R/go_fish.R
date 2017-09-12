#' @title Go fish

#' @description \code{go_fish} is a function used to apply the fishing
#' simulation model

#' @param sim_init is the initialised object from \code{init_sim}.
#' @param fleet_params is the parameter settings initialised from \code{_init_fleets}
#' @param fleet_catches is the DF initialised from \code{_init_fleets}
#' 

#' @return is ...

#' @examples
#'

#' @export

go_fish <- function(sim_init = sim, fleet_params = NULL, fleet_catches = NULL, pops = Pop, t = t) {

##### extract the relevant components ###########
params <- fleet_params      # fleet parameter list
catch  <- fleet_catches     # fleet catches list

VPT <- params[["VPT"]] 			# Value per tonne
Q <- params[["Qs"]]			# Catchability for vessel
if(length(VPT) != length(Q)) stop("VPT and Q must be the same length")

# 
PastKnowledge <- params[["past_knowledge"]]

######## indexes ##############
idx <- sim$idx
brk.idx <- sim$brk.idx
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
	if(is.null(PastKnowledge) & catch[t,"trip"] != catch[t-1,"trip"] & brk.idx[["year.breaks"]][t]>1)  {

print("USING PAST KNOWLEDGE!!!")
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
	
	q <- quantile(filter(catch.df,month==brk.idx[["month.breaks"]][t])$val,prob=c(params[["Threshold"]]),na.rm=T) # Threshold for good hauls
	goodhauls  <- filter(catch.df,month==brk.idx[["month.breaks"]][t] & val>=q)
	goodhauls  <- goodhauls[complete.cases(goodhauls),] # Remove NAs
	new.point   <- sample(paste(goodhauls$x,goodhauls$y,sep=","),1)  	# Randomly select from the good hauls
	new.point   <- c(as.numeric(sapply(strsplit(new.point,","),"[",1)),as.numeric(sapply(strsplit(new.point,","),"[",2)))
	}

        #######################################################################
	# Option 2 - last trip
	if(params[["past_year_month"]] != TRUE & params[["past_trip"]] == TRUE) {
	
	# Case where its within the same year
	if(brk.idx[["year.breaks"]][t] == brk.idx[["year.breaks"]][t-1]) {
	q <- quantile(filter(catch.df,trip==brk.idx[["trip.breaks"]][t-1] & year==brk.idx[["year.breaks"]][t])$val,prob=c(params[["threshold"]]),na.rm=T) # Threshold for good hauls
	goodhauls  <- filter(catch.df,trip==brk.idx[["trip.breaks"]][t-1] & year==brk.idx[["year.breaks"]][t] & val>=q)
	goodhauls  <- goodhauls[complete.cases(goodhauls),] # Remove NAs

	}
	
	# Case of different year
	if(brk.idx[["year.breaks"]][t] != brk.idx[["year.breaks"]][t-1]) {
	q <- quantile(filter(catch.df,trip == max(brk.idx[["trip.breaks"]]) & year == brk.idx[["year.breaks"]][t-1])$val,prob=c(params[["threshold"]]),na.rm=T) # Threshold for good hauls
	goodhauls  <- filter(catch.df,trip == max(brk.idx[["trip.breaks"]]) & year == brk.idx[["year.breaks"]][t-1] & val>=q)
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
	q <- quantile(filter(catch.df,month==brk.idx[["month.breaks"]][t] | trip==brk.idx[["trip.breaks"]][t-1] & year==brk.idx[["year.breaks"]][t])$val,prob=c(params[["threshold"]]),na.rm=T) # Threshold for good hauls
	goodhauls  <- filter(catch.df,month==brk.idx[["month.breaks"]][t] & val >=q | trip==brk.idx[["trip.breaks"]][t-1] & year==brk.idx[["year.breaks"]][t] & val>=q)
	goodhauls  <- goodhauls[complete.cases(goodhauls),] # Remove NAs
	}
	
	# Case of different year
	if(brk.idx[["year.breaks"]][t] != brk.idx[["year.breaks"]][t-1]) {
	q <- quantile(filter(catch.df,month==brk.idx[["month.breaks"]][t] | trip == max(brk.idx[["trip.breaks"]]) & year == brk.idx[["year.breaks"]][t-1])$val,prob=c(params[["threshold"]]),na.rm=T) # Threshold for good hauls
	goodhauls  <- filter(catch.df,month==brk.idx$month.breaks[t] & val >=q | trip == max(brk.idx[["trip.breaks"]]) & year == brk.idx[["year.breaks"]][t-1] & val>=q)
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
	# boundary conditions, when out of bounds redraw the direction and points 
	out.bound <-TRUE   
	while(out.bound) {
	Bear = runif(1,0,360) # bearing - to be replaced with a correlated von mises dist
	catch[t, "angles"] <- Bear
	new.point <- round(make_step(stepD = stepD, Bear = Bear, start.x = coords[1], start.y = coords[2])) # returns c(x2,y2)
	out.bound <- any(new.point < 1) | any(new.point > idx[["ncols"]])
		}

	}


coords <- new.point # assign new fishing position

	}

# save coords fished
	catch[t,"x"] <- coords[1]
	catch[t,"y"] <- coords[2]

	## matrix of the catches, to be stored for the delay-difference
	## projections

	catch_matrix <- lapply(1:idx[["n.spp"]], function(.) {
			       matrix(0, ncol = idx[["ncols"]], nrow = idx[["nrows"]])
	    })

	names(catch_matrix) <- paste("spp", 1:idx[["n.spp"]], sep = "")
	
	
	# sample from catch of species
	for (i in 1:idx[["n.spp"]]) {

	# store for the fleets record
	catch[t,paste("spp",i,sep="")] <- pops[[paste("spp",i,sep="")]][coords[1],coords[2]] * 
		Q[[paste("spp",i,sep="")]] 

	# store for the delay-diff record
	catch_matrix[[paste("spp", i, sep ="")]][coords[1], coords[2]]  <-
		pops[[paste("spp",i,sep="")]][coords[1],coords[2]] *
		Q[[paste("spp",i,sep="")]] 

	}
	
	catch[t,"allspp"] <- sum(catch[t,(paste("spp",1:idx[["n.spp"]],sep=""))])

	#calculate value of catch
	catch_val <- sapply(1:idx[["n.spp"]],function(n) {
	             catch[t,paste("spp",n,sep = "")] * VPT[[paste("spp",n,sep = "")]]
	    })

	catch[t,"val"] <- sum(catch_val)

	catch[t, "meanval"] <- mean(catch[1:t,"val"]) # Update mean value
	catch[t, "sdval"]   <- ifelse(is.na(sd(catch[1:t,"val"])),1, sd(catch[1:t,"val"]))  # Update the SD of the catch
	print(paste("tow",t,"=",round(catch[t,"val"],0),", mean = ",round(catch[t, "meanval"],0),"euros",",  ",round((t/idx[["ntow"]])*100,0),"% complete"))


res <- list(catch = catch, catch_matrix = catch_matrix)
return(res)

} # End go fish function

