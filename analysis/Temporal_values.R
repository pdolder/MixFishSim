
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


library(xtable)
print(xtable(df2, digits = 3, caption = "Proportions of each species at different levels of temporal aggregation",
	     label = "tab:7"), caption.placement = "top", file =
file.path("..","write_up", "Temp_proportions.tex"),include.rownames = FALSE,
tabular.environment= "longtable", floating = FALSE)

