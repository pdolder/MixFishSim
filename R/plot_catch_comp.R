#' @title Plot the spatial catch composition from the commercial catches as
#' 'square pie charts' using \link{mapplots}.

#' @description Plotting of spatial catch compositions at different levels of
#' aggregation

#' @param gran is a Numeric Vector of granularities required
#' @param logs is the fleet logs from \link{combine_logs}
#' @param fleets is a Numeric Vector of the fleets to include in the catch
#' composition plot
#' @param vessels is a Numeric Vector of the vessels to include in the plot
#' @param trips is a Numeric Vector of the trips to include
#' @param years is a Numeric Vector of the years
#' @param cluster_plot is a logical, determines whether also to run PAM cluserting
#' on  the catch compositions and plot the clusters spatially
#' @param clusters_k is the number of clusters to search for in the PAM
#' clustering algorithm
#' @param scale_data is a logical, whether to normalise the data before the
#' clustering

#' @examples
#' plot_catch_comp(gran = c(20,10,5,2), logs = logs, fleets = 1:2, vessels =
#' 1:5. trips = 1:20, years = 18:20, cluster_plot = FALSE, cluster_k = 5,
#' scale_data = TRUE)

#' @export

plot_catch_comp <- function (gran = c(20,10,5), logs = logs , fleets = 1:2, vessels = 1:5, trips = 1:20, years = 18:20, cluster_plot = FALSE, cluster_k = 5, scale_data = NULL) {
require(dplyr)
require(reshape2) 
require(mapplots)
require(cluster)

## Filter catch logs as needed
logs <- as.data.frame(logs)
logs <- filter(logs, fleet %in% fleets, vessel %in% vessels, trip %in% trips, year %in% years)


## First, aggregate the catch data to the appropriate scales
catch_comp <- lapply(gran, function(x) {

breaks<-seq(0.001,100.001,by=x)
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


if(cluster_plot == FALSE) {

n_plots <- length(gran)
par(mfrow=c(1,n_plots), mar=c(2,2,2,2))

cols <- c("red", "blue", "purple", "green")

for(i in seq_len(n_plots)) {
basemap(xlim = c(0,100), ylim = c(0,100), xlab = "", ylab = "", bg = "white")
draw.barplot2D(catch_comp[[i]][[1]]$x,catch_comp[[i]][[1]]$y,z = catch_comp[[i]][[1]]$z, width = gran[i],
	       height = gran[i], xlab = "", ylab = "", col = cols)

}

}


if(cluster_plot == TRUE) {
n_plots <- length(gran) 

## Run PAM clustering
if(is.null(scale_data) | scale_data == FALSE) {
pam_mods <- lapply(seq_len(n_plots), function(x) {
			   pam(dist(catch_comp[[x]][[2]][,c("spp1", "spp2","spp3","spp4")]), k = cluster_k, trace.lev = 1)
    
	       })
}

if (scale_data == TRUE) {
pam_mods <- lapply(seq_len(n_plots), function(x) {
			   pam(dist(scale(catch_comp[[x]][[2]][,c("spp1", "spp2","spp3","spp4")])), k = cluster_k, trace.lev = 1)
    
	       })
}

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

}

}
