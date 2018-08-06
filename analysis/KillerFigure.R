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

get_data <- function(basis = NULL, yr = NULL, mn = NULL, wk = NULL, aggBasis = 'None', dataIn = NULL, sim_init = NULL) {

## For commercial data
if(basis == 'commercial') {

		## Get data
		logs <- as.data.frame(combine_logs(dataIn))
		
		if(aggBasis == 'None') {
		interpdat <- filter(logs, year %in% yr, month %in% mn, week %in% wk) %>% 
			group_by(x, y) %>% 
			summarise(spp_1 = sum(spp1), spp_2 = sum(spp2), spp_3 = sum(spp3), spp_4 = sum(spp4))
		}

		if(aggBasis == 'year') {
		interpdat <- filter(logs, year %in% yr, month %in% mn, week %in% wk) %>% 
			group_by(x, y, year) %>% 
			summarise(spp_1 = sum(spp1), spp_2 = sum(spp2), spp_3 = sum(spp3), spp_4 = sum(spp4))
		}

		if(aggBasis == 'month') {
		interpdat <- filter(logs, year %in% yr, month %in% mn, week %in% wk) %>% 
			group_by(x, y, year, month) %>% 
			summarise(spp_1 = sum(spp1), spp_2 = sum(spp2), spp_3 = sum(spp3), spp_4 = sum(spp4))
		}
		
		if(aggBasis == 'week') {
		interpdat <- filter(logs, year %in% yr, month %in% mn, week %in% wk) %>% 
			group_by(x, y, year, month, week) %>% 
			summarise(spp_1 = sum(spp1), spp_2 = sum(spp2), spp_3 = sum(spp3), spp_4 = sum(spp4))
		}

		} ## end commercial basis
			
		#### Survey logs 
if(basis == 'survey') {
		logs <- as.data.frame(dataIn)
		## Filter as appropriate:- here based on either yr_base or

		if(aggBasis == 'None') {
		interpdat <- filter(logs, year %in% yr) %>% group_by(x, y) %>% 
	   	summarise(spp_1 = sum(spp1), spp_2 = sum(spp2), spp_3 = sum(spp3), spp_4 = sum(spp4))
		}

		if(aggBasis == 'year') {
		interpdat <- filter(logs, year %in% yr) %>% group_by(x, y, year) %>% 
	   	summarise(spp_1 = sum(spp1), spp_2 = sum(spp2), spp_3 = sum(spp3), spp_4 = sum(spp4))
		}

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

		if(aggBasis == 'None') {
		interpdat <- filter(logs, year %in% yr) %>% group_by(x, y) %>% 
		summarise(spp_1 = sum(spp1), spp_2 = sum(spp2), spp_3 = sum(spp3), spp_4 = sum(spp4))

		}
	
		if(aggBasis == 'year') {
		interpdat <- filter(logs, year %in% yr) %>% group_by(x, y, year) %>% 
		summarise(spp_1 = sum(spp1), spp_2 = sum(spp2), spp_3 = sum(spp3), spp_4 = sum(spp4))

		}

		if(aggBasis == 'month') {
		interpdat <- filter(logs, year %in% yr) %>% group_by(x, y, year, month) %>% 
		summarise(spp_1 = sum(spp1), spp_2 = sum(spp2), spp_3 = sum(spp3), spp_4 = sum(spp4))
		}

		if(aggBasis == 'week') {
		interpdat <- filter(logs, year %in% yr) %>% group_by(x, y, year, month, week) %>% 
		summarise(spp_1 = sum(spp1), spp_2 = sum(spp2), spp_3 = sum(spp3), spp_4 = sum(spp4))
		}


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
	       height = gran, xlab = "", ylab = "", col = cols, lwd.frame = 0, col.frame = "white")


}


###########################################################################

## Test

yr <- 2:10 
wk <- 1:52 
mn <- 1:12

## Load the results
load(file.path('.', 'Scenario_runs3', paste0('Scenario_', 0, '.RData')))
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
##setEPS()
##postscript('Data_Aggregation_space.eps', width = 8 * 2, height = 8 * 3, bg = "white")
pdf('Data_Aggregation_space_Rev.pdf', width = 8 * 2, height = 8 * 3, bg = "white")

par(oma = c(12,2,12,12), mar = c(0,0,0,0), mfrow = c(4,3))

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

legend(x = -250, y = -10, legend = c("Pop 1", "Pop 2", "Pop 3", "Pop 4"),
            fill = c("red", "blue", "purple", "green"), ncol = 4, xpd = NA, bty = "n", cex = 4)
 
dev.off()

######################################################################################################
###### Clustering
require(cluster)
cluster_k = 4
plot_cluster <- function(gran = 1,dataIn = dataIn, cluster_k = 4) {

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

## Convert to a % value for clustering
catch_grid[,c("spp1", "spp2", "spp3", "spp4")] <- 
	catch_grid[,c("spp1", "spp2", "spp3", "spp4")]/
apply(catch_grid[,c("spp1", "spp2", "spp3", "spp4")],1,sum)
catch_grid <- as.data.frame(catch_grid)
catch_grid[is.na(catch_grid)] <- 0

catch_grid <- catch_grid[order(catch_grid[,"grid_x"], catch_grid[,"grid_y"]),]

pam.mod <- pam(dist(catch_grid[,c("spp1", "spp2","spp3","spp4")]), k = cluster_k, trace.lev = 1)
catch_grid$cluster <- pam.mod$clustering

## Fix to ensure clustering assignment is consistent from one plot to next
cl <- catch_grid %>% group_by(cluster) %>% 
	summarise(spp1 = mean(spp1),
		  spp2 = mean(spp2),
		  spp3 = mean(spp3),
		  spp4 = mean(spp4)) %>% as.data.frame()
print(cl)

catch_grid[catch_grid[,"cluster"] == filter(cl, spp1 == max(cl[,"spp1"]))$cluster,"cluster"] <- 6
catch_grid[catch_grid[,"cluster"] == filter(cl, spp2 == max(cl[,"spp2"]))$cluster,"cluster"] <- 7
catch_grid[catch_grid[,"cluster"] == filter(cl, spp3 == max(cl[,"spp3"]))$cluster,"cluster"] <- 8
catch_grid[catch_grid[,"cluster"] == filter(cl, spp4 == max(cl[,"spp4"]))$cluster,"cluster"] <- 9

# Make xyz data
xyz <-      make.xyz(x = catch_grid$grid_x, y = catch_grid$grid_y, z = rep(1, l = nrow(catch_grid)), group = catch_grid$cluster) 
cols <- c("darkred", "darkblue", "orange", "darkgreen", "pink")[1:cluster_k]

basemap(xlim = c(0,100), ylim = c(0,100), xlab = "", ylab = "", bg = "white")
draw.barplot2D(xyz[["x"]],xyz[["y"]],z = xyz[["z"]], width = gran, height = gran, xlab = "", ylab = "", col = cols)

}



#######################################################################################################
pdf('Data_cluster_space_Rev.pdf', width = 8 * 2, height = 8 * 3, bg = "white")

par(oma = c(12,2,12,12), mar = c(0,0,0,0), mfrow = c(4,3))

## real pop 
dataIn <- get_data(basis = 'real_pop', yr = yr, mn = mn, wk = wk, dataIn = res[["pop_bios"]], sim_init = sim)
plot_cluster(gran = 1, dataIn = dataIn)
dataIn <- get_data(basis = 'commercial', yr = yr, mn = mn, wk = wk, dataIn = res[["fleets_catches"]]) 
plot_cluster(gran = 1, dataIn = dataIn)
dataIn <- get_data(basis = 'survey', yr = yr, mn = mn, wk = wk, dataIn = res[["survey"]][["log.mat"]]) 
plot_cluster(gran = 1, dataIn = dataIn)

dataIn <- get_data(basis = 'real_pop', yr = yr, mn = mn, wk = wk, dataIn = res[["pop_bios"]], sim_init = sim)
plot_cluster(gran = 5, dataIn = dataIn)
dataIn <- get_data(basis = 'commercial', yr = yr, mn = mn, wk = wk, dataIn = res[["fleets_catches"]]) 
plot_cluster(gran = 5, dataIn = dataIn)
dataIn <- get_data(basis = 'survey', yr = yr, mn = mn, wk = wk, dataIn = res[["survey"]][["log.mat"]]) 
plot_cluster(gran = 5, dataIn = dataIn)

dataIn <- get_data(basis = 'real_pop', yr = yr, mn = mn, wk = wk, dataIn = res[["pop_bios"]], sim_init = sim)
plot_cluster(gran = 10, dataIn = dataIn)
dataIn <- get_data(basis = 'commercial', yr = yr, mn = mn, wk = wk, dataIn = res[["fleets_catches"]]) 
plot_cluster(gran = 10, dataIn = dataIn)
dataIn <- get_data(basis = 'survey', yr = yr, mn = mn, wk = wk, dataIn = res[["survey"]][["log.mat"]]) 
plot_cluster(gran = 10, dataIn = dataIn)

dataIn <- get_data(basis = 'real_pop', yr = yr, mn = mn, wk = wk, dataIn = res[["pop_bios"]], sim_init = sim)
plot_cluster(gran = 20, dataIn = dataIn)
dataIn <- get_data(basis = 'commercial', yr = yr, mn = mn, wk = wk, dataIn = res[["fleets_catches"]]) 
plot_cluster(gran = 20, dataIn = dataIn)
dataIn <- get_data(basis = 'survey', yr = yr, mn = mn, wk = wk, dataIn = res[["survey"]][["log.mat"]]) 
plot_cluster(gran = 20, dataIn = dataIn)

mtext(text = "Real Population        Commercial Data     Survey Data", side = 3, line = 2, outer = T, font = 2,   cex = 3 ) ## top
mtext(text = "20 x 20 pt                        10 x 10  pt                         5 x 5 pt                     1 x 1 pt", side = 4, line = 2, outer = T, font = 2, cex  = 3) ## right side

legend(x = -250, y = -10, legend = paste("cluster", seq_len(cluster_k)),
            fill = c("darkred", "darkblue", "orange", "darkgreen", "pink")[1:cluster_k],
	    ncol = 4, xpd = NA, bty = "n", cex = 4)
 
dev.off()


#####################
## Temporal changes
#####################

## all year, month and week combinations

logs <- as.data.frame(combine_logs(res[["fleets_catches"]]))

logs_wk_mn <- unique(paste(logs$month, logs$week, sep = "_"))

## Some measure of temporal change in distribtions
plot_temp <- function(timestep = NULL, basis = NULL) {

xs <- 60:80
ys <- 20:40

## weekly 
if(timestep == 'week') {

if(basis == 'commercial') {
dataIn <- get_data(basis = 'commercial', yr = yr, mn = mn, wk = wk, aggBasis = 'week', dataIn = res[["fleets_catches"]]) %>%
	filter(x %in% xs, y %in% ys) %>% group_by(year, month, week) %>% summarise(spp_1 = sum(spp_1), spp_2 = sum(spp_2), spp_3 = sum(spp_3), spp_4 = sum(spp_4))
}

if(basis == 'real_pop') {
dataIn <- get_data(basis = 'real_pop', yr = yr, mn = mn, wk = wk, aggBasis = 'week', dataIn = res[["pop_bios"]], sim_init = sim) %>%
	filter(x %in% xs, y %in% ys) %>% group_by(year, month, week) %>% summarise(spp_1 = sum(spp_1), spp_2 = sum(spp_2), spp_3 = sum(spp_3), spp_4 = sum(spp_4))
}

dataIn <- dataIn[order(dataIn$year, dataIn$month, dataIn$week),]

dataIn[,4:7] <- dataIn[,4:7] / rowSums(dataIn[,4:7])

dataPlot <- data.frame(year = rep(2:4, each = length(logs_wk_mn)), 
		       month = sapply(strsplit(logs_wk_mn, "_"), "[", 1),
		       week = sapply(strsplit(logs_wk_mn, "_"), "[", 2), 
		       spp_1 = NA, spp_2 = NA, spp_3 = NA, spp_4 = NA)

dataPlot$spp_1 <- dataIn$spp_1[match(paste(dataPlot$year, dataPlot$month, dataPlot$week),
				     paste(dataIn$year, dataIn$month, dataIn$week))]
dataPlot$spp_2 <- dataIn$spp_2[match(paste(dataPlot$year, dataPlot$month, dataPlot$week),
				     paste(dataIn$year, dataIn$month, dataIn$week))]
dataPlot$spp_3 <- dataIn$spp_3[match(paste(dataPlot$year, dataPlot$month, dataPlot$week),
				     paste(dataIn$year, dataIn$month, dataIn$week))]
dataPlot$spp_4 <- dataIn$spp_4[match(paste(dataPlot$year, dataPlot$month, dataPlot$week),
				     paste(dataIn$year, dataIn$month, dataIn$week))]

dataPlot[,4:7][is.na(dataPlot[,4:7])] <- 0
dataPlot <- dataPlot[order(dataPlot$year, dataPlot$month, dataPlot$week),]

cols <- c("red", "blue", "purple", "green")
barplot(t(dataPlot[,4:7]), names.arg = rep(paste(""), nrow(dataPlot)), col = cols, border = NA)

}

## Monthly
if(timestep == 'month') {

if(basis == 'commercial') {
dataIn <- get_data(basis = 'commercial', yr = yr, mn = mn, wk = wk, aggBasis = 'month', dataIn = res[["fleets_catches"]]) %>%
	filter(x %in% xs, y %in% ys) %>% group_by(year, month) %>% summarise(spp_1 = sum(spp_1), spp_2 = sum(spp_2), spp_3 = sum(spp_3), spp_4 = sum(spp_4))
}

if(basis == 'real_pop') {
dataIn <- get_data(basis = 'real_pop', yr = yr, mn = mn, wk = wk, aggBasis = 'month', dataIn = res[["pop_bios"]], sim_init = sim) %>%
	filter(x %in% xs, y %in% ys) %>% group_by(year, month) %>% summarise(spp_1 = sum(spp_1), spp_2 = sum(spp_2), spp_3 = sum(spp_3), spp_4 = sum(spp_4))
}

dataIn <- dataIn[order(dataIn$year, dataIn$month),]

dataIn[,3:6] <- dataIn[,3:6] / rowSums(dataIn[,3:6])

dataPlot <- expand.grid(year = 2:4, month = 1:12, spp_1 = NA, spp_2 = NA, spp_3 = NA, spp_4 = NA)

dataPlot$spp_1 <- dataIn$spp_1[match(paste(dataPlot$year, dataPlot$month),
				     paste(dataIn$year, dataIn$month))]
dataPlot$spp_2 <- dataIn$spp_2[match(paste(dataPlot$year, dataPlot$month),
				     paste(dataIn$year, dataIn$month))]
dataPlot$spp_3 <- dataIn$spp_3[match(paste(dataPlot$year, dataPlot$month),
				     paste(dataIn$year, dataIn$month))]
dataPlot$spp_4 <- dataIn$spp_4[match(paste(dataPlot$year, dataPlot$month),
				     paste(dataIn$year, dataIn$month))]

dataPlot[,3:6][is.na(dataPlot[,3:6])] <- 0
dataPlot <- dataPlot[order(dataPlot$year, dataPlot$month),]

cols <- c("red", "blue", "purple", "green")
barplot(t(dataPlot[,3:6]), names.arg = rep(paste(""), 36), col = cols, border = NA)

}

## yearly 
if(timestep == 'year') {

if(basis == 'commercial') {
dataIn <- get_data(basis = 'commercial', yr = yr, mn = mn, wk = wk, aggBasis = 'year', dataIn = res[["fleets_catches"]]) %>%
	filter(x %in% xs, y %in% ys) %>% group_by(year) %>% summarise(spp_1 = sum(spp_1), spp_2 = sum(spp_2), spp_3 = sum(spp_3), spp_4 = sum(spp_4))
}

if(basis == 'survey') {
dataIn <- get_data(basis = 'survey', yr = yr, mn = mn, wk = wk, aggBasis = 'year', dataIn = res[["survey"]][["log.mat"]]) %>%
	filter(x %in% xs, y %in% ys) %>% group_by(year) %>% summarise(spp_1 = sum(spp_1), spp_2 = sum(spp_2), spp_3 = sum(spp_3), spp_4 = sum(spp_4))
}

if(basis == 'real_pop') {
dataIn <- get_data(basis = 'real_pop', yr = yr, mn = mn, wk = wk, aggBasis = 'year', dataIn = res[["pop_bios"]], sim_init = sim) %>%
	filter(x %in% xs, y %in% ys) %>% group_by(year) %>% summarise(spp_1 = sum(spp_1), spp_2 = sum(spp_2), spp_3 = sum(spp_3), spp_4 = sum(spp_4))
}

dataIn <- dataIn[order(dataIn$year),]

dataIn[,2:5] <- dataIn[,2:5] / rowSums(dataIn[,2:5])

dataPlot <- expand.grid(year = 2:4, spp_1 = NA, spp_2 = NA, spp_3 = NA, spp_4 = NA)

dataPlot$spp_1 <- dataIn$spp_1[match(dataPlot$year, dataIn$year)]
dataPlot$spp_2 <- dataIn$spp_2[match(dataPlot$year, dataIn$year)]
dataPlot$spp_3 <- dataIn$spp_3[match(dataPlot$year, dataIn$year)]
dataPlot$spp_4 <- dataIn$spp_4[match(dataPlot$year, dataIn$year)]

dataPlot[,2:5][is.na(dataPlot[,2:5])] <- 0
dataPlot <- dataPlot[order(dataPlot$year),]

cols <- c("red", "blue", "purple", "green")
barplot(t(dataPlot[,2:5]), names.arg = rep(paste(""), 3), col = cols, border = NA)

}

}


##########################
## Set out the plot 
#########################

##setEPS()
##postscript('Data_Aggregation_time.eps', width = 8 * 2, height = 8 * 3, bg = "white")
pdf('Data_Aggregation_time_Rev.pdf', width = 8 * 2, height = 8 * 3, bg = "white")
par(oma = c(12,2,12,12), mar = c(0,0,0,0), mfrow = c(3,3))

plot_temp(timestep = 'week', basis = 'real_pop')
plot_temp(timestep = 'week', basis = 'commercial')
plot(x = 1:100, y = 1:100, type = "n", xaxt = 'n', yaxt = 'n', xlab = "", ylab = "", bty = "n")

## plot_temp(timestep = 'week', basis = 'survey') #- only yearly

plot_temp(timestep = 'month', basis = 'real_pop')
plot_temp(timestep = 'month', basis = 'commercial')
## plot_temp(timestep = 'month', basis = 'survey') #- only yearly
plot(x = 1:100, y = 1:100, type = "n", xaxt = 'n', yaxt = 'n', xlab = "", ylab = "", bty = "n")

plot_temp(timestep = 'year', basis = 'real_pop')
plot_temp(timestep = 'year', basis = 'commercial')
plot_temp(timestep = 'year', basis = 'survey')

mtext(text = "Real Population        Commercial Data     Survey Data", side = 3, line = 2, outer = T, font = 2,   cex = 3 ) ## top
mtext(text = "yearly                                     monthly                                       weekly", side = 4, line = 2, outer = T, font = 2, cex  = 3) ## right side

legend(x = -5, y = 0, legend = c("Pop 1", "Pop 2", "Pop 3", "Pop 4"),
            fill = c("red", "blue", "purple", "green"), ncol = 4, xpd = NA, bty = "n", cex = 4)
 
dev.off()



