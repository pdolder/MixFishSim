#######################################################
### Plot of Scenarios together
#######################################################

library(MixFishSim)

## Load the scenarios
load('scenarios.RData')
load('Common_Params.RData')

runs <- 1:56

for(i in runs) {
load(file.path('Scenario_runs', paste('Scenario', i, '.RData', sep = "_")))
assign(paste0("sc",i),res)
}

## Combine the population metrics

combined_pop <- lapply(runs, function(r) {
	n_spp <- length(get(paste("sc",r,sep=""))[["pop_summary"]]) 
		res_df <- lapply(seq_len(n_spp), function(x) {
	 		res_spp <- lapply(names(get(paste("sc",r,sep=""))[["pop_summary"]][[x]]), function(x1) {
			      x1_res <- tidyr::gather(as.data.frame(t(get(paste("sc",r,sep=""))[["pop_summary"]][[x]][[x1]])), key = "year", factor_key = T)
		      	      if(x1 !="Rec.mat") {	res_out <- data.frame("pop" = rep(paste("spp",x, sep = "_"), length.out = nrow(x1_res)), 
						              "metric" = rep(sapply(strsplit(x1,".",fixed = T),"[",1), length.out = nrow(x1_res)), 
							      "year" = x1_res$year, 
							      "day" = rep(1:362, length.out = nrow(x1_res)),
							      "julien_day" = seq_len(nrow(x1_res)),
							      "data" = x1_res$value) }
			      if(x1 == "Rec.mat") { res_out <- data.frame("pop" = rep(paste("spp",x, sep = "_"), length.out = nrow(x1_res)), 
						              "metric" = rep(sapply(strsplit(x1,".",fixed = T),"[",1), length.out = nrow(x1_res)), 
							      "year" = seq_len(nrow(x1_res)), 
							      "day" = rep(1, length.out = nrow(x1_res)),
							      "julien_day" = rep(1, length.out = nrow(x1_res)),
							      "data" = x1_res$value) 
			      }
			      return(res_out)
			      })
			return(do.call(rbind, res_spp))
	   })
	results_df <- do.call(rbind, res_df)

})

rm(list = ls(pattern = "sc"))
gc()

rows <- nrow(combined_pop[[1]])

combined_pop <- do.call(rbind, combined_pop)

combined_pop$scenario <- rep(runs, each = rows)


library(ggplot2)
library(dplyr)

## Make results annual

results_df_an1 <- combined_pop %>% filter(metric == "Bio", day == 1) %>% 
	group_by(scenario, pop, metric, year) %>% summarise(data = sum(data))
results_df_an2 <- combined_pop %>% filter(metric != "Bio") %>% 
	group_by(scenario, pop, metric, year) %>% summarise(data = sum(data, na.rm = T))

combined_pop_an <- rbind(results_df_an1, results_df_an2) 

pl <- c(2,5)

ggplot(filter(combined_pop_an, scenario %in% pl, metric ==  "F"), aes(x = year, y = data)) +
	geom_point(aes(colour = factor(scenario)))  + 
	facet_wrap(~pop) + expand_limits(y= 0)

## Averages years 1 - 4 and 5-10, then the difference per scenario

avg2_4  <- combined_pop_an %>% filter(year %in% 2:4) %>% 
	group_by(scenario, pop, metric) %>% summarise(value = mean(data, na.rm = T))

avg8_10 <- combined_pop_an %>% filter(year %in% 8:10) %>% 
	group_by(scenario, pop, metric) %>% summarise(value = mean(data, na.rm = T))

combined <- merge(avg2_4, avg8_10, by = c("scenario","metric","pop"))

colnames(combined)[4:5] <- c("before", "after")

combined$diff <- ((combined$after - combined$before) / combined$before)  * 100

ggplot(combined, aes(x = scenario, y = diff)) + geom_point() + facet_wrap(pop ~ metric)

val_ref <- range(combined$diff[combined$metric == "F" & combined$pop=='spp_1']) 

combined[combined$diff %in% val_ref,]

## Label up scenarios
load('scenarios.RData')
sc <- sc[sc$scenario %in% runs,]

combined$timescale  <- sc$timescale[match(combined$scenario, sc$scenario)]
combined$basis      <- sc$basis[match(combined$scenario, sc$scenario)]
combined$data_type  <- sc$data_type[match(combined$scenario, sc$scenario)]
combined$resolution <- sc$resolution[match(combined$scenario, sc$scenario)]

library(ggrepel)

ggplot(filter(combined, basis == 'high_pop', metric == "F"), aes(x = data_type, y = diff)) + geom_point(aes(colour = resolution, shape = timescale), size = 4) + 
	facet_wrap(pop~.) + #geom_text_repel(aes(data_type, diff, label = paste(timescale,"," ,resolution, sep = "")), direction = "both") +
	coord_flip() + ylab("Difference before and after closure in % F") + theme_bw() + facet_grid(pop~.) + geom_hline(yintercept = 0, linetype = "dashed") + ggtitle("Effectiveness of closure in reducing Fishing mortality") + scale_colour_gradient2(high = "red", mid = "orange", low = "yellow") +
	scale_shape_discrete(solid = F)

ggsave('Overview_plot_highPop.png', width = 12, height = 4)
