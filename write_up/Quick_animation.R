##########################################
##
## Quick little animation of the random 
## walk process
#
#########################################

library(MixFishSim)
library(gganimate) 
library(gifski)
library(tidyverse)

## Load in sim
load(file.path("..", "analysis", "Scenario_runs_Nov18", "Scenario_1.RData"))
logbooks <- combine_logs(res[["fleets_catches"]])

## Choose 1 vessel in first year

wks <- 1:24

log <- filter(logbooks, fleet == 3, year == 1, week %in% wks) 

log$trip <- as.factor(log$trip)

## We need to identify trips that are jumps across the taurus
log$move_type <- ifelse(!is.na(log$stepD), "CRW", "Experience")
log$move_type <- ifelse(log$move_type == "CRW" & log$stepD > 10, "OutsideMove", log$move_type)

## lag where vessel has come from
log$x_[2:nrow(log)] <- log$x[1:(nrow(log)-1)]
log$y_[2:nrow(log)] <- log$y[1:(nrow(log)-1)]

## Every 20 is a new trip, so should be NAs
log$x_[seq(1,nrow(log), 20)] <- NA
log$y_[seq(1,nrow(log), 20)] <- NA

## The value field of fish

value_field <- lapply(wks, function(x)  {
Bs <- res[["pop_bios"]][[1, x]]

## Create value field for fleet
VPT <- c("spp1" = 100, 
	 "spp2" = 200, 
	 "spp3" = 350, 
	 "spp4" = 600)

Value <- lapply(names(Bs), function(x) {
		     VPT[[x]] * 
			     (0.01*2) * ## catchability
			     Bs[[x]]
		     })

TotVal <- Reduce("+", Value)

TotValDF <- tidyr::gather(as.data.frame(TotVal), factor_key = TRUE)
TotValDF$x <- rep(seq_len(100), times = 100)
TotValDF$y <- rep(seq_len(100), each = 100)

return(cbind(data.frame("week" = x), TotValDF))
})

value_field <- bind_rows(value_field)

## Repeat this with the number of tows per week 
value_field_tow <- value_field[rep(seq_len(nrow(value_field)), 20),] ## 20 is no tows in week

value_field_tow$tow <- rep(min(log$tow):max(log$tow), each = 10000)

rm(res); rm(logbooks)
gc()

theme_set(theme_minimal())
p1 <- ggplot(value_field_tow, aes(x = x, y = y)) + 
	geom_tile(aes(fill = value)) +
	scale_fill_gradient2(low = "blue", high = "darkblue") + 
	geom_point(data = log, 
		   aes(x = x, y = y, colour = move_type)) + 
	geom_segment(data = log,
		     aes(x = x, xend = x_, y = y, yend = y_)) +
	xlab("x distance") + ylab("y distance")
p1

	## test
#p1 <- ggplot(logs, aes(x = x, y = y)) + 
#	geom_point() # + geom_path()


anim <- p1 + transition_states(tow, transition_length = 3,
			       state_length = 1) + 
ggtitle('{closest_state}') 

an <- animate(anim, width = 1200, height = 1200,
	      renderer = gifski_renderer())


#an <- animate(anim, duration = nrow(filter(log, tow %in% 1041:1100)), fps = 100, width = 600, height = 600,
#	      renderer = gifski_renderer(), nframes = 100)

anim_save("MixFishSimGif2.gif", an)
