

#source('build.R')

library(MixFishSim)

set.seed(123, kind = "L'Ecuyer-CMRG")

## initialise the simulation

sim <- init_sim(nrows = 100, ncols = 100, n_years = 2, n_tows_day = 4, n_days_wk_fished = 5,
     n_fleets = 2, n_vessels = 10, n_species = 2, move_freq = 2)


# Here's what is produced...
#names(sim)

## create the suitable habitat for each species

hab <- create_hab(sim_init = sim,
		  spp.ctrl = list(
           'spp.1' = list('nu' = 1/0.15, var = 1, scale = 10, Aniso =
           matrix(nc=2, c(1.5, 3, -3, 4))),
           'spp.2' = list('nu' = 1/0.05, var = 2, scale = 20, Aniso =
           matrix(nc=2, c(1, 2, -1, 2)))),
		  spawn_areas = list("spp1" = list(area1 = c(40, 50, 40, 50), area2 =
				   c(80, 90, 60, 70)), 
		 "spp2" = list(area1 = c(50, 60, 30, 40), area2 = c(80, 90, 90, 90))),
		spwn_mult = 10, plot.dist = TRUE, plot.file = ".")


n_spp <- 2

# Simple comparison
suppressMessages(require(doParallel))
registerDoParallel(cores = 1)


## Simple application
MoveProb  <- foreach(s = paste0("spp", seq_len(n_spp)))  %do% move_prob_Lst(lambda = 0.3, hab = hab[["hab"]][[s]])
MoveProb_para  <- foreach(s = paste0("spp", seq_len(n_spp)))  %do% move_prob_Lst_para(lambda = 0.3, hab = hab[["hab"]][[s]])

## Proper test
library(microbenchmark)

mbm <- microbenchmark("moveProb" = {  MoveProb  <- foreach(s = paste0("spp", seq_len(n_spp)))  %do% move_prob_Lst(lambda = 0.3, hab = hab[["hab"]][[s]])
},
#"moveProb_para" = {
#	MoveProb_para  <- foreach(s = paste0("spp", seq_len(n_spp)))  %do% move_prob_Lst_para(lambda = 0.3, hab = hab[["hab"]][[s]])
#},
times = 10)

mbm

library(ggplot2)
autoplot(mbm)


