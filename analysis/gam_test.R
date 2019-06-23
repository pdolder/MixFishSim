
library(tidyverse)
library(mgcv)

load('Combined_Results.RData')

combined <- filter(combined, year %in% 30:50, metric == "F")

## This is a glm, as no smoothers
m1 <- gam(data ~ resolution + timescale +  data_type, data = combined)
summary(m1)

m2 <- glm(data ~ resolution + timescale +  data_type, data = combined)
summary(m2)

AIC(m1, m2) ##  same!

test <- expand.grid(resolution = 1:100, 
		    timescale = c("weekly", "monthly", "yearly"), 
		    data_type = c("real_pop", "commercial", "survey"))

test$prediction <- predict(m1, se.fit = T, newdata = test)$fit
test$up         <- predict(m1, se.fit = T, newdata = test)$fit + 
 		   1.96 * predict(m1, se.fit = T, newdata = test)$se.fit
test$lo         <- predict(m1, se.fit = T, newdata = test)$fit - 
 		   1.96 * predict(m1, se.fit = T, newdata = test)$se.fit


ggplot(filter(test, timescale == "weekly"), aes(x = resolution, y = prediction)) +
	geom_line(aes(colour = data_type), size = 2) +
	geom_ribbon(aes(ymin = lo, ymax = up, colour = data_type, 
			fill = data_type), linetype = 2, 
		    alpha = 0.2) + ylab("F prediction") + 
	theme_bw() + ggtitle("Effect of spatial resolution")
ggsave(file.path("..", "write_up","Plots", "Spatial_effect_F.png"))


ggplot(test, aes(x = timescale, y = prediction)) +
	geom_boxplot(aes(fill = data_type), alpha = 0.4) +
	ylab("F Prediction") + 
	theme_bw() + ggtitle("Effect of temporal resolution")
ggsave(file.path("..", "write_up","Plots", "Temporal_effect_F.png"))


