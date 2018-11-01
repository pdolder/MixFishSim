
library(MixFishSim)

x <- move_prob_Lst(lambda = 0.3, hab = matrix(nc = 2, runif(4)))
y <- move_population(moveProp = x, StartPop = matrix(nc = 2, runif(4)))

		# test logistic prob
#tmax  <-  1e6
#logistic(B = 0.001, t = 1)
#logistic(B = 0.001, t = 1e6)
#
#use_past_knowledge(p = logistic(B = 0.001, t = 1))
#use_past_knowledge(p = logistic(B = 0.001, t = 1e5))


