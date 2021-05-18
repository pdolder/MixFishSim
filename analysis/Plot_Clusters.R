
library(MixFishSim)

load('Common_Params.RData')

Run <- 2 
load(file.path('Scenario_runs2', paste0("Scenario_", Run, "_.RData")))

plot_pop_summary(res, timestep = 'annual')

logs <- as.data.frame(combine_logs(res$fleets_catches))

plot_catch_comp(gran = c(20, 10, 5, 2), logs = logs, fleets = 1:5,
       vessels = 1:20, trips = 1:60, years = 26:30, cluster_plot = TRUE, 
       scale_data = TRUE , cluster_k = 5)

## Check function with the old results
load(file.path('..', '..', 'Geostatistics', 'Sim_data','results','FullResults.RData'))


plot_catch_comp(gran = c(20, 10, 5, 2), logs = catch_results, fleets = 1:5,
       vessels = 1:20, trips = 1:60, years = 26:30, cluster_plot = TRUE, 
       scale_data = FALSE, cluster_k = 5)

###################################
## Let's look at function itself ##
## See if we can work out whats  ##
## going on....                  ##
###################################

gran = c(20, 10, 5, 2)
fleets = 1:5
vessels = 1:20
trips = 1:60
years = 26:30
cluster_plot = TRUE 
scale_data = FALSE 
cluster_k = 5

require(dplyr)
require(reshape2) 
require(mapplots)
require(cluster)

## Filter catch logs as needed
logs <- combine_logs(res$fleets_catches)
logs <- as.data.frame(logs)
logs <- filter(catch_results, fleet %in% fleets, vessel %in% vessels, trip %in% trips, year %in% years)

## First, aggregate the catch data to the appropriate scales
catch_comp <- lapply(gran, function(x) {

breaks<-seq(0.001,1000.001,by=x)
mid_break <- breaks + (x/2)
mid_break <- mid_break[-length(mid_break)] 

# Func to bin the data
bin.func <- function (x) {
	mid_break[which(abs(mid_break - x) == min(abs(mid_break - x)))]
}

# Then aggregate the data to the bins
catch_grid <- logs 
catch_grid$grid_x <- sapply(catch_grid$x,bin.func)
catch_grid$grid_y <- sapply(catch_grid$y,bin.func)

catch_grid <- group_by(catch_grid,grid_x,grid_y) %>% summarise(spp1 = sum(spp1), spp2 = sum(spp2), spp3 = sum(spp3), spp4 = sum(spp4), allspp = sum(allspp), val = sum(val))

## Now plot the gridded data to compare against the individual points
catch_hans <- melt(catch_grid,id=c("grid_x","grid_y"))
catch_hans <- catch_hans[catch_hans$variable %in% c("spp1","spp2","spp3","spp4"),]
catch_hans <- make.xyz(x = catch_hans$grid_x,y = catch_hans$grid_y,z = catch_hans$value, group = catch_hans$variable) 

return(list(catch_hans = catch_hans, catch_grid = catch_grid))

})


n_plots <- length(gran) 

## Run PAM clustering
pam_mods <- lapply(seq_len(n_plots), function(x) {
			   pam(dist(catch_comp[[x]][[2]][,c("spp1", "spp2","spp3","spp4")]), k = cluster_k, trace.lev = 1)

	       })

## Assign clustering
for(i in seq_len(n_plots)) {
catch_comp[[i]][[2]]$cluster <- pam_mods[[i]]$clustering
}

# Make xyz data
clusters <- lapply(seq_len(n_plots), function(x) {
	       make.xyz(x = catch_comp[[x]][[2]]$grid_x, y = catch_comp[[x]][[2]]$grid_y, z = rep(1, l = nrow(catch_comp[[x]][[2]])), group = catch_comp[[x]][[2]]$cluster) 
	       })



# Plot both catch compositions and clusters
par(mfrow=c(2,n_plots), mar=c(2,2,2,2))

cols <- c("red", "blue", "purple", "green")

for(i in seq_len(n_plots)) {
basemap(xlim = c(0,100), ylim = c(0,100), xlab = "", ylab = "", bg = "white")
draw.barplot2D(catch_comp[[i]][[1]]$x,catch_comp[[i]][[1]]$y,z = catch_comp[[i]][[1]]$z, width = gran[i],
	       height = gran[i], xlab = "", ylab = "", col = cols)

}

cols <- c("red", "blue", "purple", "green", "orange", "brown", "yellow")[1:cluster_k]

for(i in seq_len(n_plots)) {

basemap(xlim = c(0,100), ylim = c(0,100), xlab = "", ylab = "", bg = "white")
draw.barplot2D(clusters[[i]][["x"]],clusters[[i]][["y"]],z = clusters[[i]][["z"]], width = gran[i],       height = gran[i], xlab = "", ylab = "", col = cols)

}






