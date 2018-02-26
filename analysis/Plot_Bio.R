
load('Scenario_1_.RData')

pop <- res[["pop_bios"]]


## Simple plot of distribution changes over time

png(file = file.path("plots", "pop_dist.png"), width = 1600, height = 1600)
par(mfrow=c(4,4), mar = c(1,1,1,1))

for(j in round(seq.int(1,52,length.out = 4),0)) {
for(i in 1:4) {
image(seq_len(100), seq_len(100), pop[[10,j]][[i]], col = terrain.colors(100))
if(i==1) {text(x = 9, y = 94, labels = paste("t =",j), cex= 2, font = 2)}
if(j==1) {text(x = 92, y = 94, labels = paste("Pop ",i), cex = 2, font = 2)}
}

}
dev.off()


## Some measure of temporal change in distribtions

catch_comp <- matrix(NA, nc = 52, nr = 4)

for(i in 1:4) {
	for(j in 1:52) {
		
		B <- pop[[10,j]][[i]][55,42] 
		allB <- lapply(pop[[10,j]], function(x) x[55,42])
		allB <- Reduce("+", allB)

    catch_comp[i,j] <- B / allB 	}
}

png(file.path("plots", "Proportion_in_cell.png"), width = 800, height = 800)
matplot(t(catch_comp), type = "l", ylab = "Proportion of population in cell")
legend(45, 0.3, c("Pop 1", "Pop 2", "Pop 3", "Pop 4"),
            pch = "-", col = 1:4)
dev.off()
     
catch_compDF <- reshape2::melt(as.data.frame(catch_comp))

catch_compDF$Pop   <- rep(1:4)
catch_compDF$Week  <- rep(1:52, each = 4)

library(ggplot2)
ggplot(catch_compDF, aes(x = Week, y = value)) + geom_bar(stat = "identity", aes(fill = factor(Pop)))
ggsave(file.path("plots", "Proportion_in_cell.png"))

