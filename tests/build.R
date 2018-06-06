

library(devtools)

build()
reload()

#load_all()
document()

install()

## Note: document with 
## R CMD Rd2pdf '../MixFishSim' ~~ in terminal using :!

#library(MixFishSim)

#distance_calc(x1 = 2, x2 = 4, y1 = 5, y2 = 6)

#move_prob(start = c(2,2), lambda = 0.3, hab = matrix(nc = 3, runif(9)))

move_prob_Lst(lambda = 0.3, hab = matrix(nc = 2, runif(4)))
