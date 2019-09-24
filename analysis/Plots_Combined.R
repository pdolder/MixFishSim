#######################################################
### Plot of Scenarios together
#######################################################

library(MixFishSim)
library(tidyverse)

## Load the scenarios
load('scenarios.RData')
load('Common_Params.RData')

runs <- 0:56

for(r in runs){
print(r)
load(file.path('Scenario_runs_Nov18', paste0('Scenario_', r, '.RData')))
#assign(paste0("sc",r),res)
#}

## Combine the population metrics

#combined_pop <- lapply(runs, function(r) {
	n_spp <- length(res[["pop_summary"]]) 
		res_df <- lapply(seq_len(n_spp), function(x) {
	 		res_spp <- lapply(names(res[["pop_summary"]][[x]]), function(x1) {
			      x1_res <- tidyr::gather(as.data.frame(t(res[["pop_summary"]][[x]][[x1]])), key = "year", factor_key = T)
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
	results_df$scenario <- r

	if(r == 1) { combined_pop <- results_df}
	if(r > 1) {combined_pop <- rbind(combined_pop, results_df) }

rm(res)
gc()
}
#})

#rm(list = ls(pattern = "sc"))
#gc()

#rows <- nrow(combined_pop[[1]])

#combined_pop <- do.call(rbind, combined_pop)

#combined_pop$scenario <- rep(runs, each = rows)

## Make results annual

results_df_an1 <- combined_pop %>% filter(metric == "Bio", day == 1) %>% 
	group_by(scenario, pop, metric, year) %>% summarise(data = sum(data))
results_df_an2 <- combined_pop %>% filter(metric != "Bio") %>% 
	group_by(scenario, pop, metric, year) %>% summarise(data = sum(data, na.rm = T))

combined_pop_an <- rbind(results_df_an1, results_df_an2) 

#####################################################################
## Averages years 1 - 4 and 5-10, then the difference per scenario ##
#####################################################################

avg2_4  <- combined_pop_an %>% filter(year %in% 29:29) %>% 
	group_by(scenario, pop, metric) %>% summarise(value = mean(data, na.rm = T))

avg8_10 <- combined_pop_an %>% filter(year %in% 50:50) %>% 
	group_by(scenario, pop, metric) %>% summarise(value = mean(data, na.rm = T))

combined <- merge(avg2_4, avg8_10, by = c("scenario","metric","pop"))

colnames(combined)[4:5] <- c("before", "after")

combined$diff <- ((combined$after - combined$before) / combined$before)  * 100

######################
## Label up scenarios
######################

load('scenarios.RData')
sc <- sc[sc$scenario %in% runs,]

sc <- rbind(data.frame("scenario" = 0, timescale = "-", basis = "-", "data_type" = "-", "resolution" = 1),
      sc)

combined$timescale  <- sc$timescale[match(combined$scenario, sc$scenario)]
combined$basis      <- sc$basis[match(combined$scenario, sc$scenario)]
combined$data_type  <- sc$data_type[match(combined$scenario, sc$scenario)]
combined$resolution <- sc$resolution[match(combined$scenario, sc$scenario)]

library(ggrepel)

ggplot(filter(combined, basis == 'high_pop', metric == "F"), aes(x = data_type, y = diff)) + geom_point(aes(colour = resolution, shape = timescale), size = 4) + 
	facet_wrap(pop~.) + #geom_text_repel(aes(data_type, diff, label = paste(timescale,"," ,resolution, sep = "")), direction = "both") +
	coord_flip() + ylab("Difference before and after closure in % F") + xlab("Data source") +
	theme_bw() + facet_grid(pop~.) + geom_hline(yintercept = 0, linetype = "dashed") + ggtitle("Effectiveness of closure in reducing Fishing mortality") +
	scale_colour_gradient2(high = "red", mid = "orange", low = "yellow") +
	scale_shape_discrete(solid = F)

ggsave('Overview_plot_highPopRev.png', width = 12, height = 4)

combined_bi <- filter(combined, basis == "high_pop", metric %in% c("F", "Catch")) 

combined_bi <- reshape2::dcast(combined_bi, scenario + pop + timescale + basis + data_type + resolution ~ metric, value.var = "diff")

ggplot(combined_bi, aes(x = F, y = Catch)) + 
	geom_point(aes(colour = resolution, shape = timescale), size = 4) + 
	facet_wrap(pop~.) + ylab("Catch") + theme_bw() + facet_grid(pop~.) + 
	geom_hline(yintercept = 0, linetype = "dashed") + 
	geom_vline(xintercept = 0, linetype = "dashed") +
	ggtitle("Effectiveness of closure in reducing Fishing mortality") +
	scale_colour_gradient2(high = "red", mid = "orange", low = "yellow") +
	scale_shape_discrete(solid = F)

## Print out table of results

write.table(combined[order(combined$diff),], file = "Closure_Analysis_Results.csv", sep =",", row.names = F)


## Latex tables
library(xtable)

Fs <- filter(combined, metric == "F", basis == "high_pop", pop == "spp_3")
Fs[,c(4:6)] <- round(Fs[,c(4:6)], 2)
Fs <- Fs[order(Fs$pop, Fs$diff),]

colnames(Fs)[4:6] <- c("F before", "F after", "F change")

Fs$pop[Fs$pop == "spp_1"] <- "Population 1"
Fs$pop[Fs$pop == "spp_2"] <- "Population 2"
Fs$pop[Fs$pop == "spp_3"] <- "Population 3"
Fs$pop[Fs$pop == "spp_4"] <- "Population 4"

Fs$data_type[Fs$data_type == "real_pop"] <- "true Population"

Fs <- Fs[c("scenario", "F after", "F change", "data_type", "timescale", "resolution")]

colnames(Fs) <- c("Scenario No", "F after closure", "% F change", "data type", "timescale", "resolution")

print(xtable(Fs, digits = 2, caption = "Fishing mortality effects of the
	     closure scenarios on population 3 (ordered by most effective
						first). The fishing mortality
	     rate before the closure was 1.08.", label =
	     "tab:6"), caption.placement = "top", file =
file.path("..","write_up", "F_changes.tex"),include.rownames = FALSE,
tabular.environment= "longtable", floating = FALSE)

Fs <- filter(combined, metric == "F", basis == "high_ratio", pop == "spp_3")
Fs[,c(4:6)] <- round(Fs[,c(4:6)], 2)
Fs <- Fs[order(Fs$pop, Fs$diff),]

print(xtable(Fs, digits = 2, caption = "Fishing mortality effects of the
	     closure scenarios (based on highest ratio, ordered by most
				effective first)", label =
	     "tab:7"), caption.placement = "top", file =
file.path("..","write_up", "F_changes_ratio.tex"),include.rownames = FALSE,
tabular.environment= "longtable", floating = FALSE)


Cs <- filter(combined, metric == "Catch", basis == "high_pop", pop == "spp_3")
Cs[,c(4:6)] <- round(Cs[,c(4:6)], 2)
Cs <- Cs[order(Cs$pop, Cs$diff),]

print(xtable(Cs, digits = 2, caption = "Catch effects of the closure scenarios", label = "tab:7"), caption.placement = "top", file =
	     file.path("..","write_up", "C_changes.tex"),include.rownames =
	     FALSE, tabular.environment= "longtable", floating = FALSE)




###########################
### 
### Plot of all scenarios
############################

## Add scenario labels

combined_pop_an <- as.data.frame(combined_pop_an)

sc$combined 		   <- paste(sc$basis, sc$data_type, sc$timescale, sc$resolution, sep = "_")
combined_pop_an$combined   <- sc$combined[match(combined_pop_an$scenario, sc$scenario)]
combined_pop_an$basis      <- sc$basis[match(combined_pop_an$scenario, sc$scenario)]
combined_pop_an$timescale  <- sc$timescale[match(combined_pop_an$scenario, sc$scenario)]
combined_pop_an$res        <- sc$resolution[match(combined_pop_an$scenario, sc$scenario)]
combined_pop_an$data_type  <- sc$data_type[match(combined_pop_an$scenario, sc$scenario)]

combined_pop_an <-  combined_pop_an[!is.na(combined_pop_an$year),] ## Remove R for last year

combined_pop_an$year <- as.numeric(combined_pop_an$year) # to fix x-axis breaks

combined_pop_an$res <- as.factor(combined_pop_an$res)


combined_pop_an$pop[combined_pop_an$pop == "spp_1"] <- "Population 1"
combined_pop_an$pop[combined_pop_an$pop == "spp_2"] <- "Population 2"
combined_pop_an$pop[combined_pop_an$pop == "spp_3"] <- "Population 3"
combined_pop_an$pop[combined_pop_an$pop == "spp_4"] <- "Population 4"

combined_pop_an$data_type[combined_pop_an$data_type == "real_pop"]  <- "True Population"


png('F_trendsREV.png', units = "in", height = 8, width = 9, res = 600)

ggplot(filter(combined_pop_an,basis == 'high_pop', metric == 'F'), 
       aes(x = year, y = data, group = combined)) + 
geom_line(aes(colour = timescale, linetype = res), size = 1) +
scale_linetype_manual(values = rev(1:4)) + 
facet_grid(data_type ~pop) + expand_limits(y = 0) +theme_bw() +
geom_vline(xintercept = 30, linetype = 2, colour = "grey") + ylab("Fishing mortality") +
theme(axis.text.x = element_text(hjust = 0),
      panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
	  axis.text = element_text(size = 12, face = "bold"),
	  axis.title = element_text(size = 12, face = "bold")) + 
		  scale_x_continuous(breaks = seq(0,50,10)) + 
		  xlab("Year") + ylab(expression(Fishing~mortality~year^{-1}))
dev.off()

ggplot(filter(combined_pop_an,basis == 'high_pop', metric == 'F', pop == "spp_3"), 
       aes(x = year, y = data, group = combined)) + 
geom_line(aes(colour = timescale, linetype = factor(res))) + 
facet_wrap(data_type ~ pop, scale = 'free', ncol = 1) + expand_limits(y = 0) +theme_bw() +
geom_vline(xintercept = 30, linetype = 2, colour = "grey") + ylab("Fishing mortality")+
theme(axis.text.x = element_text(angle = -90, hjust = 0)) + scale_x_continuous(breaks = seq(0,50,5))

ggsave('F_trends_spp3REV.png', width = 8, height = 12)


ggplot(filter(combined_pop_an,basis == 'high_pop', metric == 'Bio'), 
       aes(x = year, y = data, group = combined)) + 
geom_line(aes(colour = timescale, linetype = factor(res))) + 
facet_wrap(data_type ~ pop, scale = 'free') + expand_limits(y = 0) +theme_bw() +
geom_vline(xintercept = 30, linetype = 2, colour = "grey")+
theme(axis.text.x = element_text(angle = -90, hjust = 0)) + scale_x_continuous(breaks = seq(0,50,5))

ggsave('B_trendsREV.png', width = 10, height = 8)

ggplot(filter(combined_pop_an,basis == 'high_pop', metric == 'Rec'), 
       aes(x = year, y = data, group = combined)) + 
geom_line(aes(colour = timescale, linetype = factor(res))) + 
facet_wrap(data_type ~ pop, scale = 'free') + expand_limits(y = 0) +theme_bw() +
geom_vline(xintercept = 30, linetype = 2, colour = "grey")+
theme(axis.text.x = element_text(angle = -90, hjust = 0)) + scale_x_continuous(breaks = seq(0,50,5))

ggsave('R_trendsREV.png', width = 10, height = 8)

ggplot(filter(combined_pop_an,basis == 'high_pop', metric == 'Catch'), 
       aes(x = year, y = data, group = combined)) + 
geom_line(aes(colour = timescale, linetype = factor(res))) + 
facet_wrap(data_type ~ pop, scale = 'free') + expand_limits(y = 0) +theme_bw() +
geom_vline(xintercept = 30, linetype = 2, colour = "grey")+
theme(axis.text.x = element_text(angle = -90, hjust = 0)) + scale_x_continuous(breaks = seq(0,50,5))

ggsave('C_trendsREV.png', width = 10, height = 8)




