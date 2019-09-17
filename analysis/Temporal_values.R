
week_real_pop    <- read.csv("weekreal_pop.csv")
week_commercial  <- read.csv("weekcommercial.csv")
month_real_pop   <- read.csv("monthreal_pop.csv")
month_commercial <- read.csv("monthcommercial.csv") 
year_real_pop    <- read.csv("yearreal_pop.csv")
year_commercial  <- read.csv("yearcommercial.csv")
year_survey      <- read.csv("yearsurvey.csv")

df <- data.frame(
	rbind(	 cbind("data_type" = "real_pop", "timescale" = "weekly", week_real_pop),
	rbind(   cbind("data_type" = "commercial", "timescale" = "weekly", week_commercial),
	rbind(	 cbind("data_type" = "real_pop", "timescale" = "monthly", "week"=NA, month_real_pop),
	rbind(	 cbind("data_type" = "commercial", "timescale" = "monthly","week"=NA, month_commercial),
	rbind(	 cbind("data_type" = "real_pop", "timescale" = "yearly", "week"=NA, "month" = NA, year_real_pop),
	rbind(	 cbind("data_type" = "commercial", "timescale" = "yearly", "week"=NA, "month" = NA, year_commercial),
	rbind(	 cbind("data_type" = "survey", "timescale" = "yearly", "week"=NA, "month" = NA, year_survey)
		 )))))))
	)

library(tidyverse)

df2 <- df %>% group_by(data_type, timescale) %>%
	summarise(spp1 = 100 * mean(spp_1), sd_spp1 = 100 * sd(spp_1),
		  spp2 = 100 * mean(spp_2), sd_spp2 = 100 * sd(spp_2),
		  spp3 = 100 * mean(spp_3), sd_spp3 = 100 * sd(spp_3),
		  spp4 = 100 * mean(spp_4), sd_spp4 = 100 * sd(spp_4)
		  )

df2 <- df2 %>% as.data.frame()

df2$data_type[df2$data_type == "real_pop"]  <- "True Population"

colnames(df2) <- c("Data type", "Timescale", "Pop 1", "SD Pop 1",
 "Pop 2", "SD Pop 2",
 "Pop 3", "SD Pop 3",
 "Pop 4", "SD Pop 4"
)

df2$"Population 1" <- paste0(round(df2$"Pop 1",3), "(",round(df2$"SD Pop 1",3),")")
df2$"Population 2" <- paste0(round(df2$"Pop 2",3), "(",round(df2$"SD Pop 2",3),")")
df2$"Population 3" <- paste0(round(df2$"Pop 3",3), "(",round(df2$"SD Pop 3",3),")")
df2$"Population 4" <- paste0(round(df2$"Pop 4",3), "(",round(df2$"SD Pop 4",3),")")

df2 <- df2 %>% select("Data type", "Timescale", 
		      "Population 1", "Population 2", 
		      "Population 3", "Population 4")


library(xtable)
print(xtable(df2, digits = 3, caption = "Mean and standard deviation of proportions of each species at different levels of temporal aggregation",
	     label = "tab:7"), caption.placement = "top", file =
file.path("..","write_up", "Temp_proportions.tex"),include.rownames = FALSE,
tabular.environment= "longtable", floating = FALSE)

