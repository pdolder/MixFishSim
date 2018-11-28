
timescale <- "yearly"

if(timescale == "yearly")  { Run  <- 3 }
if(timescale == "monthly") { Run  <- 2 }
if(timescale == "weekly")  { Run  <- 1 }

load(file.path('Scenario_runs_Nov18', paste0("Scenario_", Run, ".RData")))

yrs <- c(29, 46) ## years before and after the closures

closure_yrs <- 31:50

closed_areas     <- res$closures[which(closure_yrs == yrs[2])]
closed_areas     <- as.data.frame(closed_areas)
closed_areas$x   <- as.numeric(closed_areas$x)
closed_areas$y   <- as.numeric(closed_areas$y)
closed_areas$dat <- as.numeric(closed_areas$dat)
closed_areas$year <- yrs[2]


#################################################### 
## Now reduce to the two closed areas of interest ##
####################################################
## These lie between x 50, 57; y 25, 37
#                    x 62, 85, y 87, 100 

library(dplyr)

plot(closed_areas$x, closed_areas$y)
cl1 <- filter(closed_areas, x %in% c(50:57), y %in% c(25:37))
cl2 <- filter(closed_areas, x %in% c(62:85), y %in% c(87:99))

cl <- rbind(cl1, cl2)

print(cl)

plot(1:100, 1:100, type = "n")
points(cl$x, cl$y)

## Missing point is 71, 93

cl <- rbind(cl, data.frame(x = 71, y = 93, dat = NA,closure = "Missing point", year = 46))

cl$col <- ifelse(cl$closure == "Closed", "black", "red")

plot(1:100, 1:100, type = "n")
points(cl$x, cl$y, col = cl$col)

save(cl, file = "Fixed_Closure_Locations.RData")

