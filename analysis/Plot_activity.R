
library(MixFishSim)
library(dplyr)
library(ggplot2)


load('Common_Params.RData')

#Run <- 9
Run <- 3 
load(file.path('Scenario_runs_Nov18', paste0("Scenario_", Run, ".RData")))


logs <- combine_logs(res[["fleets_catches"]])

logs_b <- filter(logs, year %in% 25:29)
#logs_b <- filter(logs, year %in% 2:4, week == 4)
logs_b$time <- 'before'
#logs_a <- filter(logs, year %in% 8:10, week == 4)
logs_a <- filter(logs, year %in% 46:50)
logs_a$time <- 'after'

logs <- rbind(logs_a, logs_b)
rm(logs_a, logs_b)

## Summarise number of pings per square

logs <- logs %>% group_by(time, x, y, month) %>% summarise(n = n())

## Now to plot with geom_tile

ggplot(logs, aes(x = x, y = y)) + geom_tile(aes(fill = log(n))) + facet_wrap(month ~time) + 
	scale_fill_gradient2(low = "yellow", mid = "orange", high = "darkred")


logs_diff <- reshape2::dcast(logs, x + y + month ~ time, value.var = "n")
logs_diff[is.na(logs_diff)] <- 0

logs_diff$difference <- logs_diff$after - logs_diff$before  

ggplot(logs_diff, aes(x = x, y = y)) + geom_tile(aes(fill = difference)) + 
	scale_fill_gradient2(low = "darkgreen", midpoint = 0, high = "red") +
	facet_wrap(~month)


