###########################################################################
## "killer" figure to demonstrate scaling issues with the MixFishSim data
###########################################################################

library(MixFishSim)
library(dplyr)
library(mapplots)
library(reshape2)
##########################################################################

### Some funs

## Repurpose the aggregation logs bit of close_areas

get_data <- function(basis = NULL, yr = NULL, mn = NULL, wk = NULL, dataIn = NULL, sim_init = NULL) {

## For commercial data
if(basis == 'commercial') {

		## Get data
		logs <- as.data.frame(combine_logs(dataIn))
	
		interpdat <- filter(logs, year %in% yr, month %in% mn, week %in% wk) %>% 
			group_by(x, y) %>% 
			summarise(spp_1 = sum(spp1), spp_2 = sum(spp2), spp_3 = sum(spp3), spp_4 = sum(spp4))

		} ## end commercial basis
			
		#### Survey logs 
if(basis == 'survey') {
		logs <- as.data.frame(dataIn)
		## Filter as appropriate:- here based on either yr_base or
		interpdat <- filter(logs, year %in% yr) %>% group_by(x, y) %>% 
	   	summarise(spp_1 = sum(spp1), spp_2 = sum(spp2), spp_3 = sum(spp3), spp_4 = sum(spp4))

		} ## End survey basis 

		#### real pop
if(basis == 'real_pop') {
		## Stored as matrices within a nested list, so we need to
			## unravel
		logs_yr_list <- lapply(yr, function(y) {
			logs_wk_list <- lapply(wk, function(w) {

		## Number species
		res2 <- lapply(seq_len(length(dataIn[[1,1]])), function(x) {
		res <- data.frame(spp = as.numeric(dataIn[[y,w]][[x]]))
		colnames(res) <- paste("spp",x,sep="")
		 return(res) 
			   })

		res3 <- cbind(data.frame(x = rep(seq_len(100), times = 100), 
			   y = rep(seq_len(100), each = 100),
			   year = y, month = unique(sim_init[["brk.idx"]][["month.breaks"]][sim_init[["brk.idx"]][["week.breaks"]]==w]),
			   week = w,  do.call(cbind,res2)))

		return(res3)
		})

		logs_yr_list <- do.call(rbind, logs_wk_list)
		return(logs_yr_list)
					})

		logs <- do.call(rbind, logs_yr_list)

		## Filter as appropriate:- here based on either yr_base or
		interpdat <- filter(logs, year %in% yr) %>% group_by(x, y) %>% 
		summarise(spp_1 = sum(spp1), spp_2 = sum(spp2), spp_3 = sum(spp3), spp_4 = sum(spp4))

} ## End real pop basis

return(interpdat) ## This is the data we need

} ## End function


### Plot catch comp with the right data and scale...

plot_comp <- function (gran = NULL, dataIn = NULL) {
	
## First, aggregate the catch data to the appropriate scales
breaks<-seq(0.001,100.001,by=gran)
mid_break <- breaks + (gran/2)
mid_break <- mid_break[-length(mid_break)] 

# Func to bin the data
bin.func <- function (x) {
	mid_break[which(abs(mid_break - x) == min(abs(mid_break - x)))]
}

# Then aggregate the data to the bins
catch_grid <- dataIn 
catch_grid$grid_x <- sapply(catch_grid$x,bin.func)
catch_grid$grid_y <- sapply(catch_grid$y,bin.func)

catch_grid <- group_by(catch_grid,grid_x,grid_y) %>% 
	summarise(spp1 = sum(spp_1), spp2 = sum(spp_2), spp3 = sum(spp_3), spp4 = sum(spp_4))

## Now plot the gridded data to compare against the individual points
catch_hans <- melt(catch_grid,id=c("grid_x","grid_y"))
catch_hans <- catch_hans[catch_hans$variable %in% c("spp1","spp2","spp3","spp4"),]
catch_hans <- make.xyz(x = catch_hans$grid_x,y = catch_hans$grid_y,z = catch_hans$value, group = catch_hans$variable) 

catch_comp <- list(catch_hans = catch_hans, catch_grid = catch_grid)

cols <- c("red", "blue", "purple", "green")

#par(mar = c(0,0,0,0), pty = 'm')
basemap(xlim = c(0,100), ylim = c(0,100), xlab = "", ylab = "", bg = "white", xaxt = 'n', yaxt = "n")
draw.barplot2D(catch_comp[[1]]$x,catch_comp[[1]]$y,z = catch_comp[[1]]$z, width = gran,
	       height = gran, xlab = "", ylab = "", col = cols, lwd.frame = 0.3, col.frame = "white")


}


###########################################################################

## Test

yr <- 2:4 
wk <- 1:52 
mn <- 1:12

## Load the results
load(file.path('.', 'Scenario_runs', paste0('Scenario_', 1, '_.RData')))
load(file.path('.', 'Common_Params.RData'))

## Commercial
dataIn <- get_data(basis = 'commercial', yr = yr, mn = mn, wk = wk, dataIn = res[["fleets_catches"]]) 
plot_comp(gran = 1, dataIn = dataIn)

## Survey
dataIn <- get_data(basis = 'survey', yr = yr, mn = mn, wk = wk, dataIn = res[["survey"]][["log.mat"]]) 
plot_comp(gran = 1, dataIn = dataIn)

## real pop 
dataIn <- get_data(basis = 'real_pop', yr = yr, mn = mn, wk = wk, dataIn = res[["pop_bios"]], sim_init = sim) 
plot_comp(gran = 1, dataIn = dataIn)


##############################################
## Setting out the plot

png("Data_Aggregation.png", width = 600 * 2, height = 600 * 3)
par(oma = c(2,2,12,12), mar = c(0,0,0,0), mfrow = c(4,3))

## real pop 
dataIn <- get_data(basis = 'real_pop', yr = yr, mn = mn, wk = wk, dataIn = res[["pop_bios"]], sim_init = sim)
plot_comp(gran = 1, dataIn = dataIn)
dataIn <- get_data(basis = 'commercial', yr = yr, mn = mn, wk = wk, dataIn = res[["fleets_catches"]]) 
plot_comp(gran = 1, dataIn = dataIn)
dataIn <- get_data(basis = 'survey', yr = yr, mn = mn, wk = wk, dataIn = res[["survey"]][["log.mat"]]) 
plot_comp(gran = 1, dataIn = dataIn)

dataIn <- get_data(basis = 'real_pop', yr = yr, mn = mn, wk = wk, dataIn = res[["pop_bios"]], sim_init = sim)
plot_comp(gran = 5, dataIn = dataIn)
dataIn <- get_data(basis = 'commercial', yr = yr, mn = mn, wk = wk, dataIn = res[["fleets_catches"]]) 
plot_comp(gran = 5, dataIn = dataIn)
dataIn <- get_data(basis = 'survey', yr = yr, mn = mn, wk = wk, dataIn = res[["survey"]][["log.mat"]]) 
plot_comp(gran = 5, dataIn = dataIn)

dataIn <- get_data(basis = 'real_pop', yr = yr, mn = mn, wk = wk, dataIn = res[["pop_bios"]], sim_init = sim)
plot_comp(gran = 10, dataIn = dataIn)
dataIn <- get_data(basis = 'commercial', yr = yr, mn = mn, wk = wk, dataIn = res[["fleets_catches"]]) 
plot_comp(gran = 10, dataIn = dataIn)
dataIn <- get_data(basis = 'survey', yr = yr, mn = mn, wk = wk, dataIn = res[["survey"]][["log.mat"]]) 
plot_comp(gran = 10, dataIn = dataIn)

dataIn <- get_data(basis = 'real_pop', yr = yr, mn = mn, wk = wk, dataIn = res[["pop_bios"]], sim_init = sim)
plot_comp(gran = 20, dataIn = dataIn)
dataIn <- get_data(basis = 'commercial', yr = yr, mn = mn, wk = wk, dataIn = res[["fleets_catches"]]) 
plot_comp(gran = 20, dataIn = dataIn)
dataIn <- get_data(basis = 'survey', yr = yr, mn = mn, wk = wk, dataIn = res[["survey"]][["log.mat"]]) 
plot_comp(gran = 20, dataIn = dataIn)

mtext(text = "Real Population        Commercial Data     Survey Data", side = 3, line = 2, outer = T, font = 2,   cex = 3 ) ## top
mtext(text = "20 x 20 pt                        10 x 10  pt                         5 x 5 pt                     1 x 1 pt", side = 4, line = 2, outer = T, font = 2, cex  = 3) ## right side

dev.off()


## Temporal changes

## Some measure of temporal change in distribtions



catch_comp <- matrix(NA, nc = 52, nr = 4)

for(i in 1:4) { ## spp
	for(j in 1:52) {  ##wk
		
    catch_comp[i,j] <- B / allB 	}
}

png(file.path("..","tests" , "plots", "Proportion_in_cell.png"), width = 800, height = 800)
matplot(t(catch_comp), type = "l", ylab = "Proportion of population in cell")
legend(45, 0.3, c("Pop 1", "Pop 2", "Pop 3", "Pop 4"),
            pch = "-", col = 1:4)
dev.off()
 
