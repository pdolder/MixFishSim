

## Reminder for what closure / data scenarios to run...

sc <- expand.grid(timescale = c("weekly", "monthly", "yearly"),
	    basis = c("high_pop", "high_ratio"),
	    data_type = c("commercial", "survey", "real_pop"),
	    resolution = c(1, 5, 10, 20))

sc <- cbind(data.frame(scenario = 1:nrow(sc)), sc)

sc <- sc[!(sc$timescale %in% c("weekly","monthly") & sc$data_type == 'survey'),]

nrow(sc)

write.csv(sc, file = "scenarios.csv", row.names = F)
